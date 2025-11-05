import time
import pandas as pd
from bs4 import BeautifulSoup
import requests
import pickle
import re
import geopandas as gpd
from shapely.geometry import Point


with open(OUT_FOLDER + '/airwars_raw.pkl', 'rb') as file:
    df = pickle.load(file)

governorate_gis = gpd.read_file(IN_FOLDER + '/cod_ab/pse_admbnda_adm2_pamop_20210806.gpkg')
governorate_gis = governorate_gis[governorate_gis['ADM1_EN'] == 'Gaza Strip']

# 767 = Gaza strip

df_gaza =  df[df['country'].apply(lambda x: 767 in x)]
df_gaza.index = range(0, len(df_gaza))

links =  df_gaza['link']

for index,row in df_gaza.iterrows():
    location_text, geolocation_text, killed_text = extract_attributes(row)

def extract_point(row):
    print(row)
    if row:
        if pd.notna(row) and ', ' in row:
            lat, lon = row.split(', ')
            return Point(float(lon), float(lat))  # Point expects (x, y) which corresponds to (longitude, latitude)
        else:
            return None
    else:
        return None

def find_containing_polygon(point, polygons):
    if point:
        for index, row in polygons.iterrows():
            if row['geometry'].contains(point):
                return row['ADM2_EN']
    return None

points = df_gaza['geolocation_text'].apply(extract_point)

governorates = []

# Iterate over each point and find the containing polygon
for idx, point in points.items():
    governorate = find_containing_polygon(point, governorate_gis)
    if governorate is None:
        governorate = df_gaza.loc[idx, 'location_text'].split(',')[-2].strip()
        if df_gaza.loc[idx, 'slug'] == 'ispt0418-october-21-2023':
            governorate = 'Khan Younis'
        elif df_gaza.loc[idx, 'slug'] == 'ispt0378-october-19-2023':
            governorate = 'Rafah'
        elif df_gaza.loc[idx, 'slug'] == 'ispt0310-october-17-2023':
            governorate = 'Khan Younis'
        elif df_gaza.loc[idx, 'slug'] == 'ispt0303-october-17-2023':
            governorate = 'Khan Younis'
        elif df_gaza.loc[idx, 'slug'] == 'ispt0291-october-17-2023':
            governorate = 'Rafah'
        elif df_gaza.loc[idx, 'slug'] == 'ispt0239-october-15-2023':
            governorate = 'Khan Younis'
        elif df_gaza.loc[idx, 'slug'] == 'ispt0178a-october-13-2023':
            governorate = 'Rafah'
        elif df_gaza.loc[idx, 'slug'] == 'ispt0145a-october-12-2023':
            governorate = 'Rafah'
        elif df_gaza.loc[idx, 'slug'] == 'ispt0053-october-9-2023':
            governorate = 'Khan Younis'
    governorates.append(governorate)

replacement_rules = [
    ("central", "Deir Al-Balah"),
    ("balah", "Deir Al-Balah"),
    ("jabalia", "North Gaza"),
    ("north", "North Gaza"),
    ("rafah", "Rafah"),
    ("nuseirat", "Deir Al-Balah"),
    ("khan", "Khan Younis"),
    ("beit hanoun", "North Gaza"),
    ("lahiya", "North Gaza"),
    ("hamouda", "North Gaza"),
    ("bureij", "Deir Al-Balah"),
    ("karni", "Gaza"),
    ("shawa", "Gaza"),
    ("city", "Gaza")
]

# Function to replace the city name based on the presence of any key
def replace_city(city, rules):
    for key, value in rules:
        if key in city.lower():
            return value
    return city

# Apply the replacement function to the DataFrame
governorates = [replace_city(city, replacement_rules) for city in governorates]

valid_governorates = governorate_gis['ADM2_EN'].unique()
governorates = [None if city not in valid_governorates else city for city in governorates]

df_gaza['ADM2_EN'] = governorates


df_gaza.to_csv(OUT_FOLDER + '/airwars_gaza.csv')

