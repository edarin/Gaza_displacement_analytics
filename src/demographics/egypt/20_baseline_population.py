import pandas as pd
import os
import re

wd = os.getenv('wd')

df_population_2023 = pd.read_csv(os.path.join(wd, 'in', 'baseline_population', 'egy_nso', 'egy_pop_governorate_2023.csv'))
df_age_distribution = pd.read_csv(os.path.join(wd, 'in', 'baseline_population', 'egy_nso', 'egy_pop_governorate_age_2017.csv'))


# Calculate the total population per governorate in 2017
age_totals = df_age_distribution.groupby('governorate')['total'].sum().reset_index()

# Merge the 2017 age distribution totals with 2023 total population
merged_df = pd.merge(df_population_2023, age_totals, on ='governorate', suffixes=('_2023', '_2017'))

# Calculate the population per age group for 2023 based on 2017 proportions
for age_group in df_age_distribution['Age groups'].unique():
    age_group_total = df_age_distribution[df_age_distribution['Age groups'] == age_group].groupby('governorate')['total'].sum().reset_index()
    merged_df[age_group] = (age_group_total['total'] / merged_df['total_2017']) * merged_df['total_2023']

# Display the result
merged_df = merged_df.drop(columns=['total_2017', 'total_2023'])
merged_df =  pd.melt(merged_df, id_vars=['governorate'],
                  var_name='age_group', value_name='population')

def transform_age_group_to_min(age_group):
    # Define patterns and corresponding new labels
    if '90 and more' in age_group:
        return  90
    match = re.match(r'From (\d+) to less than(\d+)', age_group)
    if match:
        age_min = int(match.group(1))
        return age_min

merged_df['age_min']= merged_df['age_group'].apply(lambda x: pd.Series(transform_age_group_to_min(x)))

final_population = merged_df[merged_df['age_min']>=20].groupby('governorate')['population'].sum().reset_index()
final_population['agesex'] = 'T_20Plus'	
print(final_population)

final_population.to_csv(os.path.join(wd, 'out', 'egypt', 'baseline_population', 'EG_governorate_sex_age_2023.csv'), index=False)
