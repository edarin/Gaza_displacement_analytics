import pandas as pd
import os

wd = os.getenv('wd')

data_governorate = pd.read_csv(os.path.join(wd, 'in', 'baseline_population', 'jordan_nso', 'Jordan_governorate_sex_2023.csv'))
data_age_group = pd.read_csv(os.path.join(wd, 'in', 'baseline_population', 'jordan_nso', 'Jordan_nat_sex_age_2023.csv'))


# Total population by gender for all governorates (used to calculate proportions)
nat_total = data_governorate['Total'].sum()

# Add columns to store the results for each governorate and age group
results = []

# Loop through each governorate
for i, row in data_governorate.iterrows():
    governorate = row['Governorate']
    total_population = row['Total']
    
    # Calculate the proportion of the population in this governorate
    gov_proportion = total_population / nat_total
    
    # Allocate the population for each age group to the governorate
    for j, age_row in data_age_group.iterrows():
        age_group = age_row['Age Group']
        
        # Distribute the population of this age group based on proportions
        pop_in_age_group = gov_proportion * age_row['Total']
        
        # Store the result
        results.append({
            'Governorate': governorate,
            'Age Group': age_group,
            'Total': round(pop_in_age_group)
        })

# Convert results to DataFrame for better readability
df_results = pd.DataFrame(results)

# Save the results
df_results.to_csv(os.path.join(wd, 'out', 'jordan', 'baseline_population', country + '_governorate_sex_age_2023.csv'), index=False)

# check results
df_results['Total'].sum()
nat_total

df_results.groupby('Governorate').sum(['Total'])
data_governorate