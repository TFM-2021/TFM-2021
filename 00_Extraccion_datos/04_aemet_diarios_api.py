from aemet import Aemet, Estacion
import json
import time

key = "******"
aemet = Aemet(api_key= key)
estaciones = Estacion.get_estaciones(api_key=key)



for estacion in range(0, 291):
    start_time = time.time()
    datos = []
    
    print(estaciones[estacion]["nombre"])
   
    for ano in range(1971,2020,5):
        
        text = r"data/data_raw/AEMET/datos_diario/datos_diarios_inicio_"+str(estaciones[estacion]["indicativo"])+"_aemet.json"
        datos.extend(aemet.get_valores_climatologicos_diarios(f"{ano}-01-01T00:00:00UTC",
                                                            f"{ano+4}-12-31T00:00:00UTC",
                                                            estaciones[estacion]["indicativo"]))
        with open(text, 'w') as f:
            json.dump(datos, f)

    print("--- %s seconds ---" % (time.time() - start_time))
        
        