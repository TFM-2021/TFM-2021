import json
import pandas as pd 
from os import listdir
from os.path import isfile, join


onlyfiles = [f for f in listdir(r"data/data_raw/AEMET/datos_diario/") if isfile(join(r"data/data_raw/AEMET/datos_diario/", f))]

columnas = ['fecha', 'indicativo', 'nombre', 'provincia', 'altitud', 'tmed', 'prec',
            'tmin', 'horatmin', 'tmax', 'horatmax', 'dir', 'velmedia', 'racha',     
            'horaracha', 'sol', 'presMax', 'horaPresMax', 'presMin', 'horaPresMin']

TMP_data_merged = pd.DataFrame(columns=columnas)

for file in onlyfiles:

    with open(r'data/data_raw/AEMET/datos_diario/'+ file) as f:

        print(str(file))

        data = json.load(f)

        TMP_data = pd.json_normalize(data)

        TMP_data_merged = TMP_data_merged.append(TMP_data)

TMP_data_merged = TMP_data_merged.reset_index(drop=True)


TMP_data_merged.to_csv(r"data/data_clean/AEMET/aemet_diarios_join.csv", index= False)




