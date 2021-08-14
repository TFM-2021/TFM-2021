import json
import pandas as pd 
from os import listdir
from os.path import isfile, join


onlyfiles = [f for f in listdir(r"data/data_raw/AEMET/datos_diario/") if isfile(join(r"data/data_raw/AEMET/datos_diario/", f))]

onlyfiles_year = [k for k in onlyfiles if 'year' in k]

print(onlyfiles_year)


columnas = ['fecha', 'indicativo', 'nombre', 'provincia', 'altitud', 'tmed', 'prec',
            'tmin', 'horatmin', 'tmax', 'horatmax', 'dir', 'velmedia', 'racha',     
            'horaracha', 'sol', 'presMax', 'horaPresMax', 'presMin', 'horaPresMin']

TMP_data_merged = pd.DataFrame(columns=columnas)

for file in onlyfiles_year:

    with open(r'data/data_raw/AEMET/datos_diario/'+ file) as f:

        print(str(file))

        data = json.load(f)

        TMP_data = pd.json_normalize(data)

        TMP_data_merged = TMP_data_merged.append(TMP_data)

TMP_data_merged = TMP_data_merged.reset_index(drop=True)


TMP_aemet_diarios_join = pd.read_csv(r"data/data_raw/AEMET/datos_diario/aemet_diarios_join.csv")

TMP_data_merged = TMP_data_merged.append(TMP_aemet_diarios_join)

TMP_data_merged.to_csv(r"data/data_raw/AEMET/datos_diario/aemet_diarios_join_completo.csv", index= False)

