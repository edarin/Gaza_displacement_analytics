# download git code
from pathlib import Path
import shutil
import ee
import matplotlib.pyplot as plt
import rasterstats
import geopandas as gpd
from dotenv import load_dotenv
import os
import pandas as pd
import time

if not Path('py_helpers/PWTT').exists():
    !git clone https://github.com/oballinger/PWTT py_helpers/PWTT
from py_helpers.PWTT.code import pwtt

# parameters
ggdrive = Path('H:/My Drive/')
folder = 'pwtt_gaza'
country = 'gaza'

env_path = Path(".") / ".env"
load_dotenv(env_path)
in_dir = Path(os.getenv("in_dir"))
wd = Path(os.getenv("wd"))

admin_gis = gpd.read_file(in_dir / 'cod_ab/pse_admbnda_adm2_pamop_20210806.gpkg')
admin_gis = admin_gis[admin_gis['ADM1_EN'] == 'Gaza Strip']

adminName_col = 'ADM2_EN'

time_index = admin_gis.drop(columns=['geometry'])  # st_drop_geometry()
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


# 1. run pwtt
project_name = 'ee-nowpoplcds'
ee.Authenticate()
ee.Initialize(project=project_name)
gaza = ee.Geometry.Rectangle([34.205933,31.205754,34.593201,31.601931])

war_date = pd.to_datetime('2023-10-13')
end_date = pd.to_datetime('2024-06-30')
dates = pd.date_range(war_date+pd.Timedelta(weeks=4), end_date, freq='ME')
dates = [date.replace(day=13).strftime('%Y-%m-%d') for date in dates if date <= end_date]

for month in dates:
    # month = dates[7]
    gaza_damge = pwtt.filter_s1(aoi=gaza,
                   war_start=war_date,
                   inference_start=month,
                   pre_interval=12,
                   post_interval=1,
                   export=True,
                   export_dir=folder)         
    task = ee.batch.Export.image.toDrive(
                image=gaza_damge,
                description=month,
                folder=folder,
                scale=1000,
                fileFormat='GeoTIFF',
                maxPixels=8e10
            )
    task.start()

ee.batch.Task.list()
# wait until all Earth Engine tasks are completed
tasks_remaining = sum([task.status()['state'] not in ['COMPLETED', 'CANCELLED'] for task in ee.batch.Task.list()])
while tasks_remaining > 0:
    wait_time = 60 * tasks_remaining
    print(f'Waiting {round(wait_time/60, 1)} minute(s) to re-check status of {tasks_remaining} Earth Engine task(s)...')
    time.sleep(wait_time)
    tasks_remaining = sum([task.status()['state'] not in ['COMPLETED', 'CANCELLED'] for task in ee.batch.Task.list()])

# 2. transfer the images from google drive to output folder
shutil.copytree( ggdrive / folder,  wd / 'in' /  folder, dirs_exist_ok=True)

# 3. Extract building damages per admin unit

tiff_list = os.listdir( wd / 'in' /  folder)
tiff_list = [s for s in tiff_list if s.endswith('23296.tif')]

admin_sum = {}

# Iterate over the raster files
for raster_file in tiff_list:
    # raster_file = tiff_list[0]
    raster_path = wd / 'in' /  folder / raster_file
    zonal = rasterstats.zonal_stats(admin_gis, raster_path, stats=['sum'])
    admin_sum[os.path.splitext(raster_file)[0].split('-0000000000-')[0]] = [d['sum'] for d in zonal]

admin_sum = pd.DataFrame(admin_sum)
admin_sum[adminName_col] = admin_gis[adminName_col].reset_index(drop=True)

# Transform to long format
admin_sum_lg = pd.melt(admin_sum, id_vars=adminName_col, var_name='collection_date', value_name='pwtt')

admin_sum_lg['collection_date'] = pd.to_datetime(admin_sum_lg['collection_date'], format='%Y-%m-%d')	
admin_sum_lg = admin_sum_lg.merge(time_index, how='right', on=[adminName_col, 'collection_date'])

admin_sum_lg['pwtt'] = admin_sum_lg.sort_values('t').groupby(['ADM2_PCODE'])['pwtt'].transform(lambda x: x.interpolate(method='linear'))

admin_sum_lg = pd.melt(admin_sum_lg, id_vars=['ADM2_PCODE', 't', 'collection_date', 'ADM2_EN'], var_name='variable', value_name='value')

# Write output

admin_sum_lg.to_csv(wd / 'out' / 'gaza' / 'covariates' / 'pwtt.csv', index=False) 

# Visualise an example

plt.figure(figsize=(12, 6))

for i in admin_sum_lg[adminName_col].unique():
    i_data = admin_sum_lg[admin_sum_lg[adminName_col] == i]
    plt.scatter(i_data['collection_date'], i_data['value'], label=f'i={i} (t)')

plt.ylabel('pwtt')
plt.legend()
plt.show()
