import py_helpers
import os
import pandas as pd
import geopandas as gpd
from datetime import datetime
from datetime import timedelta


if __name__ == '__main__':

    # create directories
    country = 'gaza'
    wd = os.path.join(os.getenv('wd'))
    outdir = os.path.join(wd, 'out', country, 'governorate_municipality', 'daily_audience')
    os.makedirs(outdir, exist_ok=True)


    #---- input data ----#

    # Gaza admin
    gaza_admin = gpd.read_file(os.path.join(wd, 'out', country, 'governorate_municipality', 'baseline_population', 'gaza_admin.gpkg'))
    gaza_admin['meta_key'] = gaza_admin['meta_key'].apply(str)

    # agesex demographic groups
    agesex = \
        ['F_13Plus', 'F_18Plus', 'F_20Plus', 'F_13_19', 'F_15_49', 'F_15_64', 'F_18_34', 'F_20_29', 'F_30_39', 'F_40_49', 'F_50_59', 'F_60Plus', 'F_65Plus',
         'M_13Plus', 'M_18Plus', 'M_20Plus', 'M_13_19', 'M_15_49', 'M_15_64', 'M_18_34', 'M_20_29', 'M_30_39', 'M_40_49', 'M_50_59', 'M_60Plus', 'M_65Plus',
         'T_13Plus', 'T_18Plus', 'T_20Plus', 'T_13_19', 'T_15_49', 'T_15_64', 'T_18_34', 'T_20_29', 'T_30_39', 'T_40_49', 'T_50_59', 'T_60Plus', 'T_65Plus']

    #---- daily audience ----#
    for platform in ['facebook']:
        # platform = 'facebook'

        outfile = os.path.join(outdir, 'gaza_'+platform+'_governorate_audience.csv')

        # check for existing data
        if os.path.isfile(outfile):
            audience_existing = pd.read_csv(outfile)
            date_start = (
                    datetime.strptime(max(audience_existing.collection_date), '%Y-%m-%d') +
                    timedelta(days=1)
            ).strftime('%Y-%m-%d')
        else:
            date_start = '2023-10-13'
            audience_existing = None

        # end date
        date_end = datetime.today().strftime('%Y-%m-%d')

        # daily audience
        if date_start > date_end:
            audience = audience_existing
        else:
            audience = py_helpers.daily_audience(
                date_start=date_start,
                date_end=date_end,
                platform=platform,
                country='PS',
                geo_level='cities',
                geo_keys=list(gaza_admin['meta_key'].apply(str)),
                agesex= agesex
            )
            audience = audience.rename(columns={'geo_key': 'meta_key'})
            audience = audience.merge(gaza_admin[['meta_key', 'ADM2_PCODE', 'ADM2_EN']], how='left', on=['meta_key'])

            # append new data to old data
            if os.path.isfile(outfile):
                audience = pd.concat([audience_existing, audience])

        # remove duplicate rows (if duplicates, keep row with greatest mau_lower)
        audience = (audience.
                    sort_values('mau_lower', ascending=False).
                    drop_duplicates(subset=['ADM2_EN', 'collection_date', 'agesex'], keep='first'))

        # sort
        audience = audience.sort_values(['collection_date', 'ADM2_EN', 'agesex'])

        # save
        audience.to_csv(outfile, index=False)



