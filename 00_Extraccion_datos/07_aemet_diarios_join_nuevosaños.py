import json
import pandas as pd 
from os import listdir
from os.path import isfile, join


onlyfiles = [f for f in listdir(r"data/data_raw/AEMET/datos_diario/") if isfile(join(r"data/data_raw/AEMET/datos_diario/", f))]

onlyfiles_year = [k for k in onlyfiles if 'year' in k]

print(onlyfiles_year)