
import os
import rasterio
from rasterio.mask import mask
import geopandas as gpd
from datetime import datetime
import pandas as pd
import re
from dotenv import load_dotenv

env_path = Path(".") / ".env"
load_dotenv(env_path)
in_dir = Path(os.getenv("in_dir"))
wd = Path(os.getenv("wd"))

IN_FOLDER = 'K:/DemSci/data/Conflict-Ecology/Gaza/gaza_damage_retrospective_20May2024/'

outdir = wd / 'out' / 'gaza'/ "covariates"

governorate_gis = gpd.read_file(wd / 'in' / 'baseline_population' / 'cod_ab' / 'pse_admbnda_adm2_pamop_20210806.gpkg')
governorate_gis = governorate_gis[governorate_gis['ADM1_EN'] == 'Gaza Strip']
governorate_gis = governorate_gis.to_crs(32636)

time_index = governorate_gis.drop(columns=['geometry'])  # st_drop_geometry()
# group by ADM2_PCODE and ADM2_EN, and create a time index for each group
time_index_list = []
for _, group in time_index.groupby(['ADM2_PCODE', 'ADM2_EN']):
    time_index_group = pd.DataFrame({
        'collection_date': pd.date_range('2023-10-07', '2024-05-31', freq='D')
    })
    time_index_group['t'] = range(1, len(time_index_group) + 1)
    time_index_group['ADM2_PCODE'] = group['ADM2_PCODE'].iloc[0]
    time_index_group['ADM2_EN'] = group['ADM2_EN'].iloc[0]
    time_index_list.append(time_index_group)
# combine the time indexes into a single DataFrame
time_index = pd.concat(time_index_list)

# Function to read TIFF files
def read_tiff_files(folder):
    tiff_files = []
    for file in os.listdir(folder):
        if file.endswith(".tif"):
            tiff_files.append(os.path.join(folder, file))
    return tiff_files

# Function to calculate the number of pixels in each polygon
def count_pixels_in_polygons(tiff_files, polygons):
    # polygons = governorate_gis
    data = []  # List to store results
    # Read the polygon file

    # Iterate over each TIFF file
    for tiff_file in tiff_files:
        # tiff_file = tiff_files[1]
        # Open the TIFF file

        with rasterio.open(tiff_file) as src:
            # Read the raster data
            raster_data = src.read(1)
            # Iterate over each polygon
            for index, polygon in polygons.iterrows():
                # polygon = polygons.loc[1]
                # Mask the raster data with the polygon
                masked_data, _ = mask(src, [polygon.geometry], crop=True)

                # Calculate the number of pixels within the polygon
                num_pixels_damaged = (masked_data == 1).sum()
                num_pixels_undamaged = (masked_data == 0).sum()

                # Extract base name of the TIFF file
                date_pattern = r'\d{8}'
                date = re.findall(date_pattern, os.path.splitext(os.path.basename(tiff_file))[0])[-1]


                adm2_en = polygon['ADM2_EN']

                # Append data to the list
                data.append({'collection_date': datetime.strptime(date, '%Y%m%d').date(), 'ADM2_EN': adm2_en, 'damaged': num_pixels_damaged, 'undamaged': num_pixels_undamaged})

    # Create DataFrame from the data
    df = pd.DataFrame(data)
    return df

# Get the list of TIFF files
tiff_files = read_tiff_files(IN_FOLDER)

# Path to the polygon file

# Calculate the number of pixels in each polygon
damages_gaza = count_pixels_in_polygons(tiff_files, governorate_gis)
damages_gaza.collection_date = pd.to_datetime(damages_gaza.collection_date)
damages_gaza['prop_damaged'] = damages_gaza['damaged'] / (damages_gaza['damaged'] + damages_gaza['undamaged'])

# Standardise format

damages_gaza = damages_gaza[
    damages_gaza["ADM2_EN"].isin(time_index["ADM2_EN"])
].merge(
    time_index.set_index(["ADM2_EN", "collection_date"]), how="right", on=["ADM2_EN", "collection_date"]
)

damages_gaza = pd.melt(
    damages_gaza,
    id_vars=["ADM2_PCODE", "t", "collection_date", "ADM2_EN"],
    value_vars=["damaged", "prop_damaged"],
    var_name="variable",
    value_name="value"
)
damages_gaza['value'] = damages_gaza.sort_values('t').groupby(['ADM2_PCODE', 'variable'])['value'].transform(lambda x: x.interpolate(method='linear'))

# Write output
damages_gaza.to_csv(outdir /  'ecology.csv', index=False)

# Visualise an example

plt.figure(figsize=(12, 6))

test = damages_gaza[damages_gaza['variable'] == 'prop_damaged']
for i in damages_gaza['ADM2_EN'].unique():
    i_data = test[damages_gaza['ADM2_EN'] == i]
    plt.scatter(i_data['t'], i_data['value'], label=f'i={i} (t)')

plt.legend()
plt.show()