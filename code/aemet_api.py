from aemet import Aemet, Estacion, Municipio
import json

key ="eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJzZXJjYWxhOTdAZ21haWwuY29tIiwianRpIjoiYmZkYzY2MGEtNTc3Yy00OTAzLWJjYTYtMmQ0ZDVlYjczYzUzIiwiaXNzIjoiQUVNRVQiLCJpYXQiOjE2MjMyNDY5NTIsInVzZXJJZCI6ImJmZGM2NjBhLTU3N2MtNDkwMy1iY2E2LTJkNGQ1ZWI3M2M1MyIsInJvbGUiOiIifQ.-opcHy-1SSiPo3-BgKJoNuA1sP_cl0vChlR8exs2_A4"

aemet = Aemet(api_key= key)
estaciones = Estacion.get_estaciones(api_key=key)
datos = []
anyo =2021

for estacion in estaciones:
    vcm = aemet.get_valores_climatologicos_mensuales(anyo, estacion['indicativo'])
    resultado = {
            'estacion': estacion,
            'valores_climatologicos': vcm,
            'anyo': anyo
        }
    datos.append(resultado)


with open('data_aemet_2021.json', 'w') as f:
    json.dump(datos, f)
