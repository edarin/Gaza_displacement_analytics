import py_helpers
import os
from datetime import datetime
from datetime import timedelta
from dotenv import load_dotenv
from pathlib import Path


# create directories
# Load the .env file
env_path = Path('.') / '.env'
load_dotenv(env_path)

# Access the environment variables
out_dir = Path(os.getenv('wd'))
out_dir = os.path.join(out_dir,'out','egypt', 'daily_audience')
os.makedirs(out_dir, exist_ok=True)


#---- input data ----#

# agesex demographic groups
agesex = ['T_20Plus']

# date
date_end = '2024-05-14'
date_start = '2023-10-10'

# country
country = 'EG'

#---- daily audience ----#
for platform in ['facebook']:
    # platform = 'facebook'

    outfile = os.path.join(out_dir, country + '_'+platform+'_adm1_audience.csv')

    date_start_next = datetime.strptime(date_start, '%Y-%m-%d')
    date_start_next += timedelta(days=2)
    date_start_next = date_start_next.strftime('%Y-%m-%d')

    meta_key = py_helpers.query_api(
        endpoint='query_clean',            
        args = {'date_start': date_start,
                    'date_end': date_start_next,
                    'platform': 'facebook',
                    'country': country,
                    'geo_level': 'regions',
                    'location_types':'["recent"]',
                    'language_name': 'all',
                    'age_max': '999'})

    # daily audience
    audience = py_helpers.daily_audience(
            date_start=date_start,
            date_end= date_end,
            platform=platform,
            country=country,
            geo_level='regions',
            geo_keys=meta_key['geo_key'].unique().tolist(),
            agesex= agesex,
            location_types='["recent"]',
            language_name='all'
        )

    audience = audience.rename(columns={'geo_key': 'meta_key'})


    # remove duplicate rows (if duplicates, keep row with greatest mau_lower)
    audience = (audience.
                sort_values('mau_lower', ascending=False).
                drop_duplicates(subset=['meta_key', 'collection_date', 'agesex'], keep='first'))

    # sort
    audience = audience.sort_values(['collection_date', 'meta_key', 'agesex'])

    # save
    audience.to_csv(outfile, index=False)



