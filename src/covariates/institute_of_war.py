import requests
import json

services = {}
services_endpoint = "https://services6.arcgis.com/7otqighcHwehAR0n/arcgis/rest/services/"
req = requests.get(f"{services_endpoint}?f=json", verify=False)
svcs_root = json.loads(req.text)

OUT_FOLDER = 'K:/DemSci/projects/2023_Gaza_NowPop/data/in/isw_ct/'
raw_folder = OUT_FOLDER + '/raw/'
country_folder = OUT_FOLDER + '/country_boundaries/'

for fld in svcs_root['folders']:
    req = requests.get(f"{services_endpoint}/{fld}?f=json", verify=False)
    svcs_fld = json.loads(req.text)
    for svc in svcs_fld['services']:
        if svc['type'] not in ('MapServer'): continue
        req = requests.get(f"{services_endpoint}/{svc['name']}/{svc['type']}?f=json", verify=False)
        svc_def = json.loads(req.text)
        services.update({svc['name']:{'type':svc['type'], 'layers':svc_def['layers']}})

for svc in svcs_root['services']:
    if svc['type'] not in ('MapServer'): continue
    req = requests.get(f"{services_endpoint}/{svc['name']}/{svc['type']}?f=json", verify=False)
    svc_def = svc = json.loads(req.text)
    services.update({svc['name']:{'type':svc['type'], 'layers':svc_def['layers']}})


# List of file paths to GeoJSON files containing your data
isw_files_downloaded = [raw_folder + item for item in os.listdir(raw_folder)]

isw_files_processed = [
    os.path.join(root, file)
    for root, dirs, files in os.walk(OUT_FOLDER)
    for file in files
    if file.endswith(".geojson")
]
isw_files_processed = [file for file in isw_files_processed if "raw" not in file]

isw_files_downloaded = [
    file
    for file in isw_files_downloaded
    if file not in isw_files_processed
]
# List of file paths to shapefiles containing country boundaries
country_paths = [country_folder + item for item in os.listdir(country_folder)]

# Iterate over each GeoJSON file containing your data
for data_file_path in isw_files_downloaded:
    # Flag to indicate if any overlap is found with any country

    # data_file_path = isw_files_downloaded[2]
    # Load GeoDataFrame from the GeoJSON file
    print(data_file_path)
    try:
        gdf = gpd.read_file(data_file_path)
        gdf = gdf.to_crs(epsg=4326)

        overlap_found = False

        # Iterate over each GeoJSON file containing country boundaries
        for country_boundary_path in country_paths:
            country_found = False
            # country_boundary_path = country_paths[1]
            # Load GeoDataFrame containing country boundaries
            country_boundary = gpd.read_file(country_boundary_path)
            # Check for overlap with each feature in GeoDataFrame
            for idx, feature in gdf.iterrows():
                if feature.geometry != None:
                    if any(feature.geometry.intersects(country_boundary.geometry)):
                        # If overlap is found, set the flag and break the loop
                        country_found = True
                        overlap_found = True
                # If overlap is found, write entire GeoJSON data to the country folder
                if country_found:
                    # Extract country name from the GeoJSON file path
                    country_name = country_boundary['COUNTRY'].iloc[0]

                    # Create folder for the country if it doesn't exist
                    output_folder = OUT_FOLDER + country_name
                    os.makedirs(output_folder, exist_ok=True)

                    # Write entire GeoJSON data to the country folder
                    gdf.to_file(os.path.join(output_folder, f"{os.path.basename(data_file_path)}"),
                                driver="GeoJSON")

                # Break the loop after writing to the country folder
                break

        # If no overlap is found with any country, store the GeoJSON data in the misc folder
        if not overlap_found:
            # Create misc folder if it doesn't exist
            misc_folder = OUT_FOLDER + "/misc"
            os.makedirs(misc_folder, exist_ok=True)

            # Write entire GeoJSON data to the misc folder
            gdf.to_file(os.path.join(misc_folder, f"{os.path.basename(data_file_path)}"), driver="GeoJSON")

    except Exception as e:
        print(f"Skipping file '{data_file_path}' due to error: {e}")

