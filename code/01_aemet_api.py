# PASOS INSTALAR PAQUETE: https://github.com/pablo-moreno/python-aemet


from aemet import Aemet, Estacion, Municipio
import json
import time

#la API KEY se pide en https://opendata.aemet.es/centrodedescargas/obtencionAPIKey y te llegará al corre :)

key = "*********"
aemet = Aemet(api_key= key)
estaciones = Estacion.get_estaciones(api_key=key)


anyos = tuple(range(1971,2021))


for anyo in anyos:
    start_time = time.time()
    datos = []
    print(anyo)
    
    text="../data/data_raw/AEMET/data_"+str(anyo)+"_aemet.json"
    print("se guardará como: "+text)
    
    n = 16
    m = 0
    
    while n <= 291:
        
        for estacion in estaciones[m:n]:
            
            print(estacion["nombre"])
            vcm = aemet.get_valores_climatologicos_mensuales(anyo, estacion['indicativo'])
            resultado = {
            'estacion': estacion,
            'valores_climatologicos': vcm,
            'anyo': anyo}
            datos.append(resultado)
            
        with open(text, 'w') as f:
            json.dump(datos, f)
            
        print("contando...")
        time.sleep(61)
        print("FIN", m, n)

        m = n
        n = n + 25
        
    
    print("--- %s seconds ---" % (time.time() - start_time))
