import pandas as pd
import geopandas as gpd

import os
from dotenv import load_dotenv
from pathlib import Path
import numpy as np

if not Path('py_helpers/net_friction').exists():
    !git clone https://github.com/GISRedeDev/PyNetworkFriction py_helpers/net_friction

import py_helpers.net_friction.net_friction.data_preparation as prep


if __name__ == "__main__":

    # Load the .env file
    load_dotenv()
    wd = Path(os.getenv("wd"))


    raw_dir = wd / "in" / "acled"
    out_dir = wd / "out" / "gaza" / "covariates"

    os.makedirs(raw_dir, exist_ok=True)
    os.makedirs(out_dir, exist_ok=True)

    # data
    end_date = '2025-03-20'
    admin_gis = gpd.read_file(
        wd
        / "in"
        / "baseline_population"
        / "cod_ab"
        / "pse_admbnda_adm2_pamop_20210806.gpkg"

    )
    admin_gis = admin_gis[admin_gis["ADM1_EN"]=='Gaza Strip']

    time_index = admin_gis.drop(columns=['geometry'])  # st_drop_geometry()
    # group by ADM2_PCODE and ADM2_EN, and create a time index for each group
    time_index_list = []
    for _, group in time_index.groupby(['ADM2_PCODE', 'ADM2_EN']):
        time_index_group = pd.DataFrame({
            'collection_date': pd.date_range('2023-10-07', end_date, freq='D')
        })
        time_index_group['t'] = range(1, len(time_index_group) + 1)
        time_index_group['ADM2_PCODE'] = group['ADM2_PCODE'].iloc[0]
        time_index_group['ADM2_EN'] = group['ADM2_EN'].iloc[0]
        time_index_list.append(time_index_group)
    # combine the time indexes into a single DataFrame
    time_index = pd.concat(time_index_list)

    # credentials
    key = os.getenv("ACLED_KEY")
    email = os.getenv("ACLED_EMAIL")
    country_name = "Palestine"
    start_date = min(time_index["collection_date"])
    end_date = max(time_index["collection_date"])
    crs = 4326
    accept_acled_terms = True

    # get acled data
    incident_gdf = prep.get_acled_data_from_api(
        api_key=key,
        email=email,
        country=country_name,
        start_date=start_date,
        end_date=end_date,
        crs=crs,
        accept_acleddata_terms=accept_acled_terms,
    )

    # save acled data to disk
    incident_gdf.to_csv(raw_dir / ( "acled.csv"), index=False)
    incident_gdf.to_file(
        filename=raw_dir / ("acled.gpkg"),
        driver="GPKG",
        append=False,
    )

    # aggregate events
    acled = gpd.read_file(raw_dir / ( "acled.gpkg")).rename(
        columns={"event_date": "collection_date"}
    )

    acled = gpd.sjoin(
        left_df=acled,
        right_df=admin_gis[["ADM2_PCODE", "geometry"]],
        how="inner",
        predicate="within",
    )

    # summarise events by type
    acled_df = pd.DataFrame(acled)
    acled_df["collection_date"] = pd.to_datetime(acled_df["collection_date"])

    acled_df = acled_df[
        acled_df["collection_date"] <= max(time_index["collection_date"])
    ].set_index(["collection_date", "ADM2_PCODE"]).join(time_index.set_index(["collection_date", "ADM2_PCODE"]), on=["collection_date", "ADM2_PCODE"], how="left")

    conditions = {
        "acled_withfatalities": acled_df["fatalities"] != "0",
        "acled_eventAgainstCivilians": acled_df["event_type"]
        == "Violence against civilians",
        "acled_actor1Israel": acled_df['actor1']
    }

    # Calculate counts for each condition
    acled_i = (
        acled_df.groupby(["ADM2_PCODE", "t"])["event_id_cnty"]
        .count()
        .reset_index(name="acled_all")
    )

    for key, condition in conditions.items():
        filtered_acled = acled_df[condition]
        acled_i = acled_i.merge(
            filtered_acled.groupby(["ADM2_PCODE", "t"])["event_id_cnty"]
            .count()
            .reset_index(name=key),
            how="left",
            on=["ADM2_PCODE", "t"],
        )

    acled_i = acled_i[
        acled_i["ADM2_PCODE"].isin(time_index["ADM2_PCODE"])
    ].merge(
        time_index.set_index(["ADM2_PCODE", "t"]), how="right", on=["ADM2_PCODE", "t"]
    ).fillna(0)

    acled_i_lg = pd.melt(
        acled_i,
        id_vars=["ADM2_PCODE", "t", "collection_date", "ADM2_EN"],
        value_vars=["acled_all", "acled_withfatalities", "acled_eventAgainstCivilians"],
        var_name="variable",
        value_name="value",
    )

    acled_i_lg.to_csv(out_dir / ("acled_i_lg.csv"), index=False)
    acled_i_lg.to_file(
        filename=out_dir / ("acled_i_lg.gpkg"),
        driver="GPKG",
        append=False,
    )


    acled_i_lg.to_csv(out_dir / ("acled.csv"), index=False)
