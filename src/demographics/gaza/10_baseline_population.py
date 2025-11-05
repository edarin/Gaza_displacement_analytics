import py_helpers
import os
import pandas as pd
import geopandas as gpd


if __name__ == '__main__':

    # make directories
    country = 'gaza'
    wd = os.path.join(os.getenv('wd'))
    outdir = os.path.join(wd, 'out', country, 'baseline_population')
    os.makedirs(outdir, exist_ok=True)

    #### input data ####

    #-- Gaza admin boundaries --#

    # admin boundaries (COD-AB)
    cod_ab = gpd.read_file(filename=os.path.join(wd, 'in', 'baseline_population', 'cod_ab',
                                                 'pse_admbnda_adm2_pamop_20210806.gpkg'))
    cod_ab = cod_ab[cod_ab.ADM1_PCODE == 'PS02']


    #-- baseline age-sex proportions --#

    # baseline population admin-1 (with age breakdown)
    cod_pop = pd.read_csv(os.path.join(wd, 'in', 'baseline_population', 'cod_ps', 'pse', 'pse_admpop_adm1_2023.csv'))
    cod_pop = cod_pop[cod_pop['ADM1_EM'] == 'Gaza Strip']

    # UNPD single year pop
    single_pop = pd.read_csv(os.path.join(wd, 'in', 'baseline_population', 'unpd', 'pse_admpop_adm0_1year_2023.csv'))


    #-- baseline population --#

    # baseline population admin-2 (no age breakdown)
    base_pop = pd.read_csv(os.path.join(wd, 'in', 'baseline_population', 'ocha', 'pse_admpop_adm2_2024.csv'))
    base_pop = base_pop[base_pop['ADM1_EN'].eq('Gaza Strip') & base_pop['year'].eq(2023)]

    ####################


    #---- Gaza admin boundaries ----#

    # get points for Facebook's Gaza cities
    gaza_cities = py_helpers.query_sql(con=py_helpers.db_engine(),
                                  sql="select * from cities where country='PS';",
                                  geom_col='geometry')

    # add lat/lon
    gaza_cities['latitude'] = gaza_cities['geometry'].y
    gaza_cities['longitude'] = gaza_cities['geometry'].x

    # spatial join admin attributes to cities
    adm_cols = ['ADM0_PCODE', 'ADM0_EN', 'ADM1_PCODE', 'ADM1_EN', 'ADM2_PCODE', 'ADM2_EN', 'geometry']
    city_cols = ['meta_key', 'name', 'latitude', 'longitude', 'geometry']
    gaza_admin = cod_ab[adm_cols].sjoin(gaza_cities[city_cols], how='left', predicate='intersects')
    gaza_admin['index_right'] = None

    # write to disk
    gaza_admin.to_file(os.path.join(outdir, country + '_admin.gpkg'))

    # cleanup
    del city_cols; del adm_cols; del gaza_cities; del cod_ab


    #---- baseline agesex proportions ----#

    # agesex column names
    agesex_all = py_helpers.agesex_cols(gender=[2, 1], age_min=0, age_max=80)
    agesex_fem = py_helpers.agesex_cols(gender=[2], age_min=0, age_max=80)
    agesex_mal = py_helpers.agesex_cols(gender=[1], age_min=0, age_max=80)
    agesex_single = (['_'.join(['T', str(x), str(x)]) for x in range(1, 25)])

    # Gaza age-sex proportions
    agesex_prop = pd.DataFrame(columns=agesex_all, index=['total', 'bysex'])
    agesex_prop.loc['total', agesex_all] = [x / cod_pop.at[0, 'T_TL'] for x in list(cod_pop.loc[0, agesex_all])]
    agesex_prop.loc['bysex', agesex_fem] = [x / cod_pop.at[0, 'F_TL'] for x in list(cod_pop.loc[0, agesex_fem])]
    agesex_prop.loc['bysex', agesex_mal] = [x / cod_pop.at[0, 'M_TL'] for x in list(cod_pop.loc[0, agesex_mal])]

    # single-year proportions
    for a in agesex_single:
        agesex_prop.loc['total', a] = single_pop.loc[0, a] / single_pop.loc[0, 'T_TL']

    # save to disk
    agesex_prop.to_csv(os.path.join(outdir, country + '_baseline_agesex_proportions.csv'))

    # cleanup
    del agesex_fem; del agesex_mal; del agesex_single; del single_pop; del cod_pop; del a


    # ---- baseline population ----#

    # rescale M_TL and F_TL to sum to T_TL
    correction_factor = base_pop['T_TL'] / base_pop[['M_TL', 'F_TL']].sum('columns')
    base_pop['F_TL'] = base_pop['F_TL'] * correction_factor
    base_pop['M_TL'] = base_pop['M_TL'] * correction_factor

    # Gaze age-sex populations for admin-2
    agesex_prop_type = 'bysex'
    for a in agesex_all:
        if agesex_prop_type == 'total':
            base_pop[a] = base_pop['T_TL'] * agesex_prop.at['total', a]
        elif agesex_prop_type == 'bysex':
            base_pop[a] = base_pop[a[0] + '_TL'] * agesex_prop.at['bysex', a]

    # Ratio T_13_14 : T_10_14
    ratio_13_14_to_10_14 = (sum(agesex_prop.loc['total', ['T_13_13', 'T_14_14']]) /
                            sum(agesex_prop.loc['total', ['T_10_10', 'T_11_11', 'T_12_12', 'T_13_13', 'T_14_14']]))

    # Ratio T_18_19 : T_15_19
    ratio_18_19_to_15_19 = (sum(agesex_prop.loc['total', ['T_18_18', 'T_19_19']]) /
                            sum(agesex_prop.loc['total', ['T_15_15', 'T_16_16', 'T_17_17', 'T_18_18', 'T_19_19']]))

    # aggregate baseline population to age-sex demographic groups of-interest
    base_pop['F_13_14'] = base_pop['F_10_14'] * ratio_13_14_to_10_14
    base_pop['M_13_14'] = base_pop['M_10_14'] * ratio_13_14_to_10_14
    base_pop['T_13_14'] = base_pop[['F_13_14', 'M_13_14']].sum('columns')

    base_pop['F_18_19'] = base_pop['F_15_19'] * ratio_18_19_to_15_19
    base_pop['M_18_19'] = base_pop['M_15_19'] * ratio_18_19_to_15_19
    base_pop['T_18_19'] = base_pop[['F_18_19', 'M_18_19']].sum('columns')

    base_pop['F_00_17'] = base_pop[['F_00_04', 'F_05_09', 'F_10_14']].sum('columns') + (base_pop['F_15_19'] * (1-ratio_18_19_to_15_19))
    base_pop['M_00_17'] = base_pop[['M_00_04', 'M_05_09', 'M_10_14']].sum('columns') + (base_pop['M_15_19'] * (1-ratio_18_19_to_15_19))
    base_pop['T_00_17'] = (base_pop[['F_00_04', 'F_05_09', 'F_10_14', 'M_00_04', 'M_05_09', 'M_10_14']].sum('columns') +
                           (base_pop[['F_15_19', 'M_15_19']].sum('columns') * (1-ratio_18_19_to_15_19)))

    base_pop['F_13Plus'] = base_pop[['F_13_14'] + py_helpers.agesex_cols(gender=[2], age_min=15, age_max=80)].sum('columns')
    base_pop['F_18Plus'] = base_pop[['F_18_19'] + py_helpers.agesex_cols(gender=[2], age_min=20, age_max=80)].sum('columns')
    base_pop['F_20Plus'] = base_pop[py_helpers.agesex_cols(gender=[2], age_min=20, age_max=80)].sum('columns')
    base_pop['F_13_19'] = base_pop[['F_13_14', 'F_15_19']].sum('columns')
    base_pop['F_15_49'] = base_pop[py_helpers.agesex_cols(gender=[2], age_min=15, age_max=50)[:-1]].sum('columns')
    base_pop['F_15_64'] = base_pop[py_helpers.agesex_cols(gender=[2], age_min=15, age_max=65)[:-1]].sum('columns')
    base_pop['F_18_34'] = base_pop[['F_18_19', 'F_20_24', 'F_25_29', 'F_30_34']].sum('columns')
    base_pop['F_20_29'] = base_pop[['F_20_24', 'F_25_29']].sum('columns')
    base_pop['F_30_39'] = base_pop[['F_30_34', 'F_35_39']].sum('columns')
    base_pop['F_40_49'] = base_pop[['F_40_44', 'F_45_49']].sum('columns')
    base_pop['F_50_59'] = base_pop[['F_50_54', 'F_55_59']].sum('columns')
    base_pop['F_60Plus'] = base_pop[py_helpers.agesex_cols(gender=[2], age_min=60, age_max=80)].sum('columns')
    base_pop['F_65Plus'] = base_pop[py_helpers.agesex_cols(gender=[2], age_min=65, age_max=80)].sum('columns')

    base_pop['M_13Plus'] = base_pop[['M_13_14'] + py_helpers.agesex_cols(gender=[1], age_min=15, age_max=80)].sum('columns')
    base_pop['M_18Plus'] = base_pop[['M_18_19'] + py_helpers.agesex_cols(gender=[1], age_min=20, age_max=80)].sum('columns')
    base_pop['M_20Plus'] = base_pop[py_helpers.agesex_cols(gender=[1], age_min=20, age_max=80)].sum('columns')
    base_pop['M_13_19'] = base_pop[['M_13_14', 'M_15_19']].sum('columns')
    base_pop['M_15_49'] = base_pop[py_helpers.agesex_cols(gender=[1], age_min=15, age_max=50)[:-1]].sum('columns')
    base_pop['M_15_64'] = base_pop[py_helpers.agesex_cols(gender=[1], age_min=15, age_max=65)[:-1]].sum('columns')
    base_pop['M_18_34'] = base_pop[['M_18_19', 'M_20_24', 'M_25_29', 'M_30_34']].sum('columns')
    base_pop['M_20_29'] = base_pop[['M_20_24', 'M_25_29']].sum('columns')
    base_pop['M_30_39'] = base_pop[['M_30_34', 'M_35_39']].sum('columns')
    base_pop['M_40_49'] = base_pop[['M_40_44', 'M_45_49']].sum('columns')
    base_pop['M_50_59'] = base_pop[['M_50_54', 'M_55_59']].sum('columns')
    base_pop['M_60Plus'] = base_pop[py_helpers.agesex_cols(gender=[1], age_min=60, age_max=80)].sum('columns')
    base_pop['M_65Plus'] = base_pop[py_helpers.agesex_cols(gender=[1], age_min=65, age_max=80)].sum('columns')

    base_pop['T_13Plus'] = base_pop[['T_13_14'] + py_helpers.agesex_cols(gender=[2, 1], age_min=15, age_max=80)].sum('columns')
    base_pop['T_18Plus'] = base_pop[['T_18_19'] + py_helpers.agesex_cols(gender=[2, 1], age_min=20, age_max=80)].sum('columns')
    base_pop['T_20Plus'] = base_pop[py_helpers.agesex_cols(gender=[2, 1], age_min=20, age_max=80)].sum('columns')
    base_pop['T_13_19'] = base_pop[['T_13_14', 'F_15_19', 'M_15_19']].sum('columns')
    base_pop['T_15_49'] = base_pop[py_helpers.agesex_cols(gender=[2], age_min=15, age_max=50)[:-1] +
                                   py_helpers.agesex_cols(gender=[1], age_min=15, age_max=50)[:-1]].sum('columns')
    base_pop['T_15_64'] = base_pop[py_helpers.agesex_cols(gender=[2], age_min=15, age_max=65)[:-1] +
                                   py_helpers.agesex_cols(gender=[1], age_min=15, age_max=65)[:-1]].sum('columns')
    base_pop['T_18_34'] = base_pop[['T_18_19', 'F_20_24', 'F_25_29', 'F_30_34', 'M_20_24', 'M_25_29', 'M_30_34']].sum('columns')
    base_pop['T_20_29'] = base_pop[['F_20_24', 'F_25_29', 'M_20_24', 'M_25_29']].sum('columns')
    base_pop['T_30_39'] = base_pop[['F_30_34', 'F_35_39', 'M_30_34', 'M_35_39']].sum('columns')
    base_pop['T_40_49'] = base_pop[['F_40_44', 'F_45_49', 'M_40_44', 'M_45_49']].sum('columns')
    base_pop['T_50_59'] = base_pop[['F_50_54', 'F_55_59', 'M_50_54', 'M_55_59']].sum('columns')
    base_pop['T_20_24'] = base_pop[['F_20_24', 'M_20_24']].sum('columns')
    base_pop['T_25_29'] = base_pop[['F_25_29', 'M_25_29']].sum('columns')
    base_pop['T_30_34'] = base_pop[['F_30_34', 'M_30_34']].sum('columns')
    base_pop['T_35_39'] = base_pop[['F_35_39', 'M_35_39']].sum('columns')
    base_pop['T_40_44'] = base_pop[['F_40_44', 'M_40_44']].sum('columns')
    base_pop['T_45_49'] = base_pop[['F_45_49', 'M_45_49']].sum('columns')
    base_pop['T_50_54'] = base_pop[['F_50_54', 'M_50_54']].sum('columns')
    base_pop['T_55_59'] = base_pop[['F_55_59', 'M_55_59']].sum('columns')
    base_pop['T_60_64'] = base_pop[['F_60_64', 'M_60_64']].sum('columns')
    base_pop['T_60Plus'] = base_pop[py_helpers.agesex_cols(gender=[1, 2], age_min=60, age_max=80)].sum('columns')
    base_pop['T_65Plus'] = base_pop[py_helpers.agesex_cols(gender=[1, 2], age_min=65, age_max=80)].sum('columns')

    # melt baseline pop to long format
    base_pop_long = base_pop.melt(id_vars='ADM2_PCODE',
                                  value_vars=list(base_pop.columns[list(base_pop.columns).index('F_TL'):]),
                                  var_name='agesex',
                                  value_name='pop')
    base_pop_long = base_pop_long.sort_values(by=['ADM2_PCODE', 'agesex'])

    # save
    base_pop.to_csv(os.path.join(outdir, country + '_base_pop.csv'), index=False)
    base_pop_long.to_csv(os.path.join(outdir, country + '_base_pop_long.csv'), index=False)

    # cleanup
    del a; del agesex_all
