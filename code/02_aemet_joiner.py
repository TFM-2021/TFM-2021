import json
import pandas as pd 
from os import listdir
from os.path import isfile, join


onlyfiles = [f for f in listdir(r"data/data_raw/AEMET/") if isfile(join(r"data/data_raw/AEMET/", f))]


columnas = ['latitud', 'provincia', 'altitud', 'indicativo', 'nombre', 'indsinop',
           'longitud', 'fecha', 'p_max', 'n_cub', 'hr', 'n_fog', 'inso', 'q_max',
           'nw_55', 'q_mar', 'q_med', 'tm_min', 'ta_max', 'ts_min', 'nt_30',
           'nv_0050', 'n_des', 'w_racha', 'np_100', 'n_nub', 'p_sol', 'nw_91',
           'np_001', 'ta_min', 'e', 'np_300', 'nv_1000', 'evap', 'p_mes', 'n_tor',
           'w_med', 'nt_00', 'ti_max', 'tm_mes', 'tm_max', 'nv_0100', 'q_min',
           'np_010', 'n_gra', 'n_llu', 'n_nie']

TMP_data_merged = pd.DataFrame(columns=columnas)

    
for file in onlyfiles:

    with open(r'data/data_raw/AEMET/'+ file) as f:
        data = json.load(f)
        
    
        print(file)


        for estacion in (range(0,len(data))):
            try:
                
                TMP_data = pd.merge(pd.json_normalize(data[estacion]["estacion"]),
                            pd.json_normalize(data[estacion]["valores_climatologicos"]))

                TMP_data_merged = TMP_data_merged.append(TMP_data)

            except: 
                pass
TMP_data_merged = TMP_data_merged.reset_index(drop=True)


columnas_con_parentesis = list([8, 13, 18, 24, 29, 42])



mes_13 = []
for j, row in TMP_data_merged.iterrows():

    if (row.fecha[-2:]==str(13)) == True:

        mes_13.extend([TMP_data_merged.index[j]])

TMP_data_merged.drop(mes_13, inplace=True)


TMP_data_merged.to_csv(r"data/data_clean/AEMET/data_join_aemet.csv", index= False)
