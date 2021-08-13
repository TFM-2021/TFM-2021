


from aemet import Aemet, Estacion
import json
import time



key = "*****"
aemet = Aemet(api_key= key)
estaciones = Estacion.get_estaciones(api_key=key)


anyos = tuple(range(1976,1981)) + tuple(range(1996,2001))


for anyo in anyos:
    start_time = time.time()
    datos = []
    print(anyo)
    
    text="../data/data_raw/AEMET/datos_diario/datos_diarios_year_"+str(anyo)+"_aemet.json"
    print("se guardará como: "+ text)
    
    n = 16
    m = 0
    
    while n <= 291:
        
        for estacion in estaciones[m:n]:
            
            print(estacion["nombre"])
            vcm =aemet.get_valores_climatologicos_diarios(f"{anyo}-01-01T00:00:00UTC",
                                                            f"{anyo}-12-31T00:00:00UTC", estacion['indicativo'])
            datos.extend(vcm)
            
        with open(text, 'w') as f:
            json.dump(datos, f)
            
        print("contando...")
        time.sleep(61)
        print("FIN", m, n)

        m = n
        n = n + 25
        
    
    print("--- %s seconds ---" % (time.time() - start_time))