# TFM-2021

>El presente trabajo nace con el objetivo de recopilar y analizar, en la medida de
lo posible, la bibliografía existente en torno a las pruebas de resistencia o test de estrés
climáticos realizados en el sector financiero sobre las entidades financieras como bancos
y compañías aseguradoras. Así mismo, el propósito del equipo es poder poner en
práctica el conocimiento adquirido durante todo el año académico, en la medida de lo
posible.

>Además de hacer una revisión de la bibliografía, hemos implementado modelos
de machine learning de clasificación y de la teoría de eventos extremos para medir la
frecuencia y estimar el coste de terremotos en la Península Ibérica diferenciando por
sistema montañoso.

>Del mismo modo, se ha realizado el mismo análisis para los incendios forestales
en España. En este caso como cambio se buscará ajustar los valores extremos de las
superficies afectadas (en hectáreas) y se aplicarán modelos de regresión para la parte
de pricing.

## Version R
R version 4.1.0 (2021-05-18)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 19043)

Matrix products: default

locale:
[1] LC_COLLATE=Spanish_Spain.1252 
[2] LC_CTYPE=Spanish_Spain.1252   
[3] LC_MONETARY=Spanish_Spain.1252
[4] LC_NUMERIC=C                  
[5] LC_TIME=Spanish_Spain.1252    

attached base packages:
[1] stats     graphics  grDevices
[4] utils     datasets  methods  
[7] base     

loaded via a namespace (and not attached):
[1] compiler_4.1.0 tools_4.1.0

## Estructura de las carpetas

Listado de rutas de carpetas
C:.
|   .gitignore
|   README.md
|   requirements.txt
|   TFM-2021.Rproj
|   tree.txt
|   
+---00_Extraccion_datos
|       01_aemet_api.py
|       02_aemet_joiner.py
|       03_aemet_clean.R
|       04_aemet_diarios_api.py
|       05_aemet_diarios_joiner.py
|       06_aemet_diarios_api_nuevosa�os.py
|       07_aemet_diarios_join_nuevosa�os.py
|       
+---01_Data_understanding
|   +---00_Describe_Explore_data
|   |   |   00_EDA_aemet.R
|   |   |   00_EDA_incendios.R
|   |   |   01_Describe_aemet_diarios_join.R
|   |   |   02_Explore_aemet_diarios_join.R
|   |   |   03_Describe_terremotos.R
|   |   |   04_Explore_terremotos.R
|   |   |   05_Describe_CCSS.R
|   |   |   07_Explore_CCSS.R
|   |   |   README.txt
|   |   |   
|   |   \---corrplots
|   |       \---AEMET
|   |               corrplot.png
|   |               
|   \---01_Verify_data_quality
|           00_Quality_aemet_darios.R
|           README.txt
|           
+---02_Data_preparation
|   |   README.txt
|   |   
|   +---00_Select_data
|   |       00_aemet_variables.csv
|   |       00_aemet_variables.R
|   |       00_incendios_variables.csv
|   |       00_incendios_variables.R
|   |       01_terremotos_intensidad.R
|   |       02_inundaciones_TCA.R
|   |       03_aemet_diario.R
|   |       04_terremotos_EVT.R
|   |       inundaciones_TCA_selected.rds
|   |       README.txt
|   |       terremotos_EVT_selected.rds
|   |       terremotos_selected_modelo_intensidad.rds
|   |       
|   +---01_Clean_data
|   |       01_clean_data_aemet.R
|   |       01_clean_data_incendios.R
|   |       02_clean_terremotos_modelo_intensidad.R
|   |       03_clean_inundaciones_TCA.R
|   |       04_clean_aemet_diarios.R
|   |       05_clean_terremotos_EVT.R
|   |       aemet_clean.csv
|   |       incendios_clean.csv
|   |       inundaciones_TCA_clean.rds
|   |       README.txt
|   |       terremotos_clean_modelo_intensidad.rds
|   |       
|   +---02_Construct_data
|   |       01_Construct_terremotos_modelo_intensidad.R
|   |       02_Construct_aemet.R
|   |       02_Construct_incendios.R
|   |       03_Construct_inundaciones_TCA.R
|   |       incendios_clean_final.csv
|   |       README.txt
|   |       
|   \---03_Integrate_Format_data
|           README.txt
|           
+---03_Modeling
|   |   01_clustering_incendios.R
|   |   01_clustering_terremotos.R
|   |   02_EVT_incendios.R
|   |   02_EVT_terremotos.R
|   |   03_pricing_incendios.R
|   |   04_pricing_terremotos.R
|   |   cluter_terremoto.rds
|   |   README.txt
|   |   regresion_incendios.R
|   |   
|   +---hash
|   |       bagged_decision_tree_hash.rds
|   |       boost_tree_model_hash.rds
|   |       c5_rules_hash.rds
|   |       LDA_hash.rds
|   |       multinom_reg_hash.rds
|   |       naive_Bayes_hash.rds
|   |       nearest_neighbor_hash.rds
|   |       rand_forest_hash.rds
|   |       rdm_hash.rds
|   |       svm_poly_hash.rds
|   |       svm_rbf_hash.rds
|   |       
|   \---hash_incendios
|       |   bag_mars_hash.rds
|       |   boost_tree_model_hash.rds
|       |   cubis_hash.rds
|       |   nearest_neighbor_hash.rds
|       |   rand_forest_hash.rds
|       |   
|       \---backup
|               bag_mars_hash.rds
|               boost_tree_model_hash.rds
|               cubis_hash.rds
|               nearest_neighbor_hash.rds
|               rand_forest_hash.rds
|               
+---04_Evaluacion
|   |   02_pricing_incendios.R
|   |   03_pricing_terremotos.R
|   |   04_evaluacion_EVT.R
|   |   04_evaluacion_EVT_incendios.R
|   |   README.txt
|   |   
|   +---trained_models_incendios
|   |       bag_mars_trained.rds
|   |       boost_tree_trained.rds
|   |       cubist_rules_trained.rds
|   |       nearest_neighbor_trained.rds
|   |       rand_forest_trained.rds
|   |       
|   \---trained_models_terremotos
|           bag_mars_trained.rds
|           bag_tree_trained.rds
|           boost_tree_model_hash_trained.rds
|           C5_rules_trained.rds
|           discrim_linear_trained.rds
|           discrim_regularized_trained.rds
|           multinom_reg_trained.rds
|           naive_Bayes_trained.rds
|           nearest_neighbor_trained.rds
|           svm_poly_trained.rds
|           svm_rbf_trained.rds
|           
+---05_Deployment
|   +---app_tfm_costes
|   |   |   app.R
|   |   |   arbol_intensidad_terremotos.rds
|   |   |   bag_mars_trained.rds
|   |   |   bag_tree_trained.rds
|   |   |   boost_tree_model_hash_trained.rds
|   |   |   boost_tree_trained.rds
|   |   |   C5_rules_trained.rds
|   |   |   CCAA.rds
|   |   |   cubist_rules_trained.rds
|   |   |   incendios_evt.rds
|   |   |   incendios_pricing.rds
|   |   |   matriz_costes.rds
|   |   |   multinom_reg_trained.rds
|   |   |   naive_Bayes_trained.rds
|   |   |   nearest_neighbor_trained.rds
|   |   |   rand_forest_trained.rds
|   |   |   VAL_terremotos_EVT_clusters_clara.rds
|   |   |   
|   |   \---rsconnect
|   |       \---shinyapps.io
|   |           \---sercala97
|   |                   app_tfm_costes.dcf
|   |                   
|   +---DOCKER-APP
|   |   |   Dockerfile
|   |   |   
|   |   \---shiny-app
|   |           .Rhistory
|   |           app.R
|   |           bag_mars_trained.rds
|   |           bag_tree_trained.rds
|   |           boost_tree_model_hash_trained.rds
|   |           boost_tree_trained.rds
|   |           C5_rules_trained.rds
|   |           CCAA.rds
|   |           cubist_rules_trained.rds
|   |           incendios_evt.rds
|   |           incendios_pricing.rds
|   |           matriz_costes.rds
|   |           multinom_reg_trained.rds
|   |           naive_Bayes_trained.rds
|   |           nearest_neighbor_trained.rds
|   |           rand_forest_trained.rds
|   |           VAL_terremotos_EVT_clusters_clara.rds
|   |           
|   +---REST_API
|   |       arbol_intensidad_terremotos.rda
|   |       arbol_intensidad_terremotos.rds
|   |       plumber.R
|   |       prueba.R
|   |       REST_API.R
|   |       
|   +---rsconnect
|   |   \---shinyapps.io
|   |       \---sercala97
|   |               05_Deployment.dcf
|   |               terremotos_coste.dcf
|   |               TFM_app.dcf
|   |               
|   +---Spark
|   |   |   pasos_spark.txt
|   |   |   
|   |   +---.metadata
|   |   |   |   .lock
|   |   |   |   .log
|   |   |   |   version.ini
|   |   |   |   
|   |   |   \---.plugins
|   |   |       \---org.eclipse.core.resources
|   |   |           \---.root
|   |   +---apache-maven-3.8.2-bin
|   |   |   \---apache-maven-3.8.2
|   |   +---spark-3.1.2-bin-hadoop3.2
|   |   |   \---spark-3.1.2-bin-hadoop3.2
|   |   \---winutils
|   |       \---bin
|   |               winutils.exe
|   |               
|   \---spark-java
|       |   .classpath
|       |   .project
|       |   pom.xml
|       |   
|       +---.settings
|       |       org.eclipse.jdt.core.prefs
|       |       org.eclipse.m2e.core.prefs
|       |       
|       +---1
|       +---src
|       |   +---main
|       |   |   +---java
|       |   |   |       Example.java
|       |   |   |       MYSQL_CLASS.java
|       |   |   |       TFM_query.java
|       |   |   |       
|       |   |   \---resources
|       |   \---test
|       |       +---java
|       |       \---resources
|       \---target
|           +---classes
|           |       Example.class
|           |       MYSQL_CLASS.class
|           |       TFM_query.class
|           |       
|           \---test-classes
+---99_Funciones
|       conversor_coordenadas.R
|       mapa_ESP.R
|       
+---data
|   |   mapa_ESP.rds
|   |   
|   +---data_raw
|   |   |   causas_incendios.csv
|   |   |   causas_incendios.xlsx
|   |   |   comunidades incendios.csv
|   |   |   incendios.csv
|   |   |   provincias incendios.csv
|   |   |   terremotos-ign.csv
|   |   |   
|   |   +---AEMET
|   |   |   |   data_1971_aemet.json
|   |   |   |   data_1972_aemet.json
|   |   |   |   data_1973_aemet.json
|   |   |   |   data_1974_aemet.json
|   |   |   |   data_1975_aemet.json
|   |   |   |   data_1976_aemet.json
|   |   |   |   data_1977_aemet.json
|   |   |   |   data_1978_aemet.json
|   |   |   |   data_1979_aemet.json
|   |   |   |   data_1980_aemet.json
|   |   |   |   data_1981_aemet.json
|   |   |   |   data_1982_aemet.json
|   |   |   |   data_1983_aemet.json
|   |   |   |   data_1984_aemet.json
|   |   |   |   data_1985_aemet.json
|   |   |   |   data_1986_aemet.json
|   |   |   |   data_1987_aemet.json
|   |   |   |   data_1988_aemet.json
|   |   |   |   data_1989_aemet.json
|   |   |   |   data_1990_aemet.json
|   |   |   |   data_1991_aemet.json
|   |   |   |   data_1992_aemet.json
|   |   |   |   data_1993_aemet.json
|   |   |   |   data_1994_aemet.json
|   |   |   |   data_1995_aemet.json
|   |   |   |   data_1996_aemet.json
|   |   |   |   data_1997_aemet.json
|   |   |   |   data_1998_aemet.json
|   |   |   |   data_1999_aemet.json
|   |   |   |   data_2000_aemet.json
|   |   |   |   data_2001_aemet.json
|   |   |   |   data_2002_aemet.json
|   |   |   |   data_2003_aemet.json
|   |   |   |   data_2004_aemet.json
|   |   |   |   data_2005_aemet.json
|   |   |   |   data_2006_aemet.json
|   |   |   |   data_2007_aemet.json
|   |   |   |   data_2008_aemet.json
|   |   |   |   data_2009_aemet.json
|   |   |   |   data_2010_aemet.json
|   |   |   |   data_2011_aemet.json
|   |   |   |   data_2012_aemet.json
|   |   |   |   data_2013_aemet.json
|   |   |   |   data_2014_aemet.json
|   |   |   |   data_2015_aemet.json
|   |   |   |   data_2016_aemet.json
|   |   |   |   data_2017_aemet.json
|   |   |   |   data_2018_aemet.json
|   |   |   |   data_2019_aemet.json
|   |   |   |   data_2020_aemet.json
|   |   |   |   data_join_aemet.csv
|   |   |   |   
|   |   |   \---datos_diario
|   |   |           datos_diarios_inicio_0002I_aemet.json
|   |   |           datos_diarios_inicio_0016A_aemet.json
|   |   |           datos_diarios_inicio_0076_aemet.json
|   |   |           datos_diarios_inicio_0149X_aemet.json
|   |   |           datos_diarios_inicio_0200E_aemet.json
|   |   |           datos_diarios_inicio_0201D_aemet.json
|   |   |           datos_diarios_inicio_0229I_aemet.json
|   |   |           datos_diarios_inicio_0252D_aemet.json
|   |   |           datos_diarios_inicio_0255B_aemet.json
|   |   |           datos_diarios_inicio_0324A_aemet.json
|   |   |           datos_diarios_inicio_0367_aemet.json
|   |   |           datos_diarios_inicio_0370B_aemet.json
|   |   |           datos_diarios_inicio_0372C_aemet.json
|   |   |           datos_diarios_inicio_1002Y_aemet.json
|   |   |           datos_diarios_inicio_1014A_aemet.json
|   |   |           datos_diarios_inicio_1014_aemet.json
|   |   |           datos_diarios_inicio_1024E_aemet.json
|   |   |           datos_diarios_inicio_1037Y_aemet.json
|   |   |           datos_diarios_inicio_1041A_aemet.json
|   |   |           datos_diarios_inicio_1050J_aemet.json
|   |   |           datos_diarios_inicio_1055B_aemet.json
|   |   |           datos_diarios_inicio_1057B_aemet.json
|   |   |           datos_diarios_inicio_1059X_aemet.json
|   |   |           datos_diarios_inicio_1078I_aemet.json
|   |   |           datos_diarios_inicio_1082_aemet.json
|   |   |           datos_diarios_inicio_1083L_aemet.json
|   |   |           datos_diarios_inicio_1109_aemet.json
|   |   |           datos_diarios_inicio_1110_aemet.json
|   |   |           datos_diarios_inicio_1111X_aemet.json
|   |   |           datos_diarios_inicio_1111_aemet.json
|   |   |           datos_diarios_inicio_1159_aemet.json
|   |   |           datos_diarios_inicio_1183X_aemet.json
|   |   |           datos_diarios_inicio_1207U_aemet.json
|   |   |           datos_diarios_inicio_1208A_aemet.json
|   |   |           datos_diarios_inicio_1208H_aemet.json
|   |   |           datos_diarios_inicio_1208_aemet.json
|   |   |           datos_diarios_inicio_1210X_aemet.json
|   |   |           datos_diarios_inicio_1212E_aemet.json
|   |   |           datos_diarios_inicio_1221D_aemet.json
|   |   |           datos_diarios_inicio_1249I_aemet.json
|   |   |           datos_diarios_inicio_1249X_aemet.json
|   |   |           datos_diarios_inicio_1283U_aemet.json
|   |   |           datos_diarios_inicio_1331A_aemet.json
|   |   |           datos_diarios_inicio_1351_aemet.json
|   |   |           datos_diarios_inicio_1387E_aemet.json
|   |   |           datos_diarios_inicio_1387_aemet.json
|   |   |           datos_diarios_inicio_1393_aemet.json
|   |   |           datos_diarios_inicio_1400_aemet.json
|   |   |           datos_diarios_inicio_1428_aemet.json
|   |   |           datos_diarios_inicio_1437O_aemet.json
|   |   |           datos_diarios_inicio_1473A_aemet.json
|   |   |           datos_diarios_inicio_1475X_aemet.json
|   |   |           datos_diarios_inicio_1484C_aemet.json
|   |   |           datos_diarios_inicio_1484_aemet.json
|   |   |           datos_diarios_inicio_1495_aemet.json
|   |   |           datos_diarios_inicio_1505_aemet.json
|   |   |           datos_diarios_inicio_1542_aemet.json
|   |   |           datos_diarios_inicio_1549_aemet.json
|   |   |           datos_diarios_inicio_1631E_aemet.json
|   |   |           datos_diarios_inicio_1690A_aemet.json
|   |   |           datos_diarios_inicio_1690B_aemet.json
|   |   |           datos_diarios_inicio_1700X_aemet.json
|   |   |           datos_diarios_inicio_1735X_aemet.json
|   |   |           datos_diarios_inicio_2030_aemet.json
|   |   |           datos_diarios_inicio_2117D_aemet.json
|   |   |           datos_diarios_inicio_2150H_aemet.json
|   |   |           datos_diarios_inicio_2235U_aemet.json
|   |   |           datos_diarios_inicio_2298_aemet.json
|   |   |           datos_diarios_inicio_2331_aemet.json
|   |   |           datos_diarios_inicio_2374X_aemet.json
|   |   |           datos_diarios_inicio_2400E_aemet.json
|   |   |           datos_diarios_inicio_2401_aemet.json
|   |   |           datos_diarios_inicio_2422_aemet.json
|   |   |           datos_diarios_inicio_2444C_aemet.json
|   |   |           datos_diarios_inicio_2444_aemet.json
|   |   |           datos_diarios_inicio_2462_aemet.json
|   |   |           datos_diarios_inicio_2465A_aemet.json
|   |   |           datos_diarios_inicio_2465_aemet.json
|   |   |           datos_diarios_inicio_2491C_aemet.json
|   |   |           datos_diarios_inicio_2503X_aemet.json
|   |   |           datos_diarios_inicio_2539_aemet.json
|   |   |           datos_diarios_inicio_2614_aemet.json
|   |   |           datos_diarios_inicio_2630X_aemet.json
|   |   |           datos_diarios_inicio_2661B_aemet.json
|   |   |           datos_diarios_inicio_2661_aemet.json
|   |   |           datos_diarios_inicio_2737E_aemet.json
|   |   |           datos_diarios_inicio_2755X_aemet.json
|   |   |           datos_diarios_inicio_2775X_aemet.json
|   |   |           datos_diarios_inicio_2811A_aemet.json
|   |   |           datos_diarios_inicio_2867_aemet.json
|   |   |           datos_diarios_inicio_2870_aemet.json
|   |   |           datos_diarios_inicio_2916A_aemet.json
|   |   |           datos_diarios_inicio_2946X_aemet.json
|   |   |           datos_diarios_inicio_3013_aemet.json
|   |   |           datos_diarios_inicio_3044X_aemet.json
|   |   |           datos_diarios_inicio_3094B_aemet.json
|   |   |           datos_diarios_inicio_3100B_aemet.json
|   |   |           datos_diarios_inicio_3110C_aemet.json
|   |   |           datos_diarios_inicio_3111D_aemet.json
|   |   |           datos_diarios_inicio_3129_aemet.json
|   |   |           datos_diarios_inicio_3130C_aemet.json
|   |   |           datos_diarios_inicio_3168A_aemet.json
|   |   |           datos_diarios_inicio_3168C_aemet.json
|   |   |           datos_diarios_inicio_3168D_aemet.json
|   |   |           datos_diarios_inicio_3175_aemet.json
|   |   |           datos_diarios_inicio_3191E_aemet.json
|   |   |           datos_diarios_inicio_3194U_aemet.json
|   |   |           datos_diarios_inicio_3195_aemet.json
|   |   |           datos_diarios_inicio_3196_aemet.json
|   |   |           datos_diarios_inicio_3200_aemet.json
|   |   |           datos_diarios_inicio_3259_aemet.json
|   |   |           datos_diarios_inicio_3260B_aemet.json
|   |   |           datos_diarios_inicio_3266A_aemet.json
|   |   |           datos_diarios_inicio_3298X_aemet.json
|   |   |           datos_diarios_inicio_3338_aemet.json
|   |   |           datos_diarios_inicio_3365A_aemet.json
|   |   |           datos_diarios_inicio_3391_aemet.json
|   |   |           datos_diarios_inicio_3434X_aemet.json
|   |   |           datos_diarios_inicio_3463X_aemet.json
|   |   |           datos_diarios_inicio_3469A_aemet.json
|   |   |           datos_diarios_inicio_3469_aemet.json
|   |   |           datos_diarios_inicio_3519X_aemet.json
|   |   |           datos_diarios_inicio_3526X_aemet.json
|   |   |           datos_diarios_inicio_3576X_aemet.json
|   |   |           datos_diarios_inicio_4061X_aemet.json
|   |   |           datos_diarios_inicio_4067_aemet.json
|   |   |           datos_diarios_inicio_4090Y_aemet.json
|   |   |           datos_diarios_inicio_4103X_aemet.json
|   |   |           datos_diarios_inicio_4121C_aemet.json
|   |   |           datos_diarios_inicio_4121_aemet.json
|   |   |           datos_diarios_inicio_4147X_aemet.json
|   |   |           datos_diarios_inicio_4148_aemet.json
|   |   |           datos_diarios_inicio_4220X_aemet.json
|   |   |           datos_diarios_inicio_4244X_aemet.json
|   |   |           datos_diarios_inicio_4267X_aemet.json
|   |   |           datos_diarios_inicio_4358X_aemet.json
|   |   |           datos_diarios_inicio_4386B_aemet.json
|   |   |           datos_diarios_inicio_4410X_aemet.json
|   |   |           datos_diarios_inicio_4452_aemet.json
|   |   |           datos_diarios_inicio_4511C_aemet.json
|   |   |           datos_diarios_inicio_4549Y_aemet.json
|   |   |           datos_diarios_inicio_4560Y_aemet.json
|   |   |           datos_diarios_inicio_4605_aemet.json
|   |   |           datos_diarios_inicio_4642E_aemet.json
|   |   |           datos_diarios_inicio_5000A_aemet.json
|   |   |           datos_diarios_inicio_5000C_aemet.json
|   |   |           datos_diarios_inicio_5038X_aemet.json
|   |   |           datos_diarios_inicio_5047E_aemet.json
|   |   |           datos_diarios_inicio_5051X_aemet.json
|   |   |           datos_diarios_inicio_5181D_aemet.json
|   |   |           datos_diarios_inicio_5192_aemet.json
|   |   |           datos_diarios_inicio_5246_aemet.json
|   |   |           datos_diarios_inicio_5270B_aemet.json
|   |   |           datos_diarios_inicio_5270_aemet.json
|   |   |           datos_diarios_inicio_5298X_aemet.json
|   |   |           datos_diarios_inicio_5390Y_aemet.json
|   |   |           datos_diarios_inicio_5402_aemet.json
|   |   |           datos_diarios_inicio_5427X_aemet.json
|   |   |           datos_diarios_inicio_5514_aemet.json
|   |   |           datos_diarios_inicio_5530E_aemet.json
|   |   |           datos_diarios_inicio_5582A_aemet.json
|   |   |           datos_diarios_inicio_5612B_aemet.json
|   |   |           datos_diarios_inicio_5641X_aemet.json
|   |   |           datos_diarios_inicio_5704B_aemet.json
|   |   |           datos_diarios_inicio_5783_aemet.json
|   |   |           datos_diarios_inicio_5796_aemet.json
|   |   |           datos_diarios_inicio_5860E_aemet.json
|   |   |           datos_diarios_inicio_5910_aemet.json
|   |   |           datos_diarios_inicio_5911A_aemet.json
|   |   |           datos_diarios_inicio_5960_aemet.json
|   |   |           datos_diarios_inicio_5972X_aemet.json
|   |   |           datos_diarios_inicio_5973_aemet.json
|   |   |           datos_diarios_inicio_5995B_aemet.json
|   |   |           datos_diarios_inicio_6000A_aemet.json
|   |   |           datos_diarios_inicio_6001_aemet.json
|   |   |           datos_diarios_inicio_6032B_aemet.json
|   |   |           datos_diarios_inicio_6058I_aemet.json
|   |   |           datos_diarios_inicio_6084X_aemet.json
|   |   |           datos_diarios_inicio_6106X_aemet.json
|   |   |           datos_diarios_inicio_6155A_aemet.json
|   |   |           datos_diarios_inicio_6156X_aemet.json
|   |   |           datos_diarios_inicio_6172O_aemet.json
|   |   |           datos_diarios_inicio_6205X_aemet.json
|   |   |           datos_diarios_inicio_6268X_aemet.json
|   |   |           datos_diarios_inicio_6277B_aemet.json
|   |   |           datos_diarios_inicio_6293X_aemet.json
|   |   |           datos_diarios_inicio_6297_aemet.json
|   |   |           datos_diarios_inicio_6302A_aemet.json
|   |   |           datos_diarios_inicio_6325O_aemet.json
|   |   |           datos_diarios_inicio_6332X_aemet.json
|   |   |           datos_diarios_inicio_6367B_aemet.json
|   |   |           datos_diarios_inicio_6381_aemet.json
|   |   |           datos_diarios_inicio_7002Y_aemet.json
|   |   |           datos_diarios_inicio_7012C_aemet.json
|   |   |           datos_diarios_inicio_7031X_aemet.json
|   |   |           datos_diarios_inicio_7031_aemet.json
|   |   |           datos_diarios_inicio_7096B_aemet.json
|   |   |           datos_diarios_inicio_7119B_aemet.json
|   |   |           datos_diarios_inicio_7145D_aemet.json
|   |   |           datos_diarios_inicio_7178I_aemet.json
|   |   |           datos_diarios_inicio_7209_aemet.json
|   |   |           datos_diarios_inicio_7228_aemet.json
|   |   |           datos_diarios_inicio_7247X_aemet.json
|   |   |           datos_diarios_inicio_7275C_aemet.json
|   |   |           datos_diarios_inicio_8019_aemet.json
|   |   |           datos_diarios_inicio_8025_aemet.json
|   |   |           datos_diarios_inicio_8050X_aemet.json
|   |   |           datos_diarios_inicio_8058X_aemet.json
|   |   |           datos_diarios_inicio_8096_aemet.json
|   |   |           datos_diarios_inicio_8175_aemet.json
|   |   |           datos_diarios_inicio_8177A_aemet.json
|   |   |           datos_diarios_inicio_8178D_aemet.json
|   |   |           datos_diarios_inicio_8293X_aemet.json
|   |   |           datos_diarios_inicio_8309X_aemet.json
|   |   |           datos_diarios_inicio_8325X_aemet.json
|   |   |           datos_diarios_inicio_8368U_aemet.json
|   |   |           datos_diarios_inicio_8414A_aemet.json
|   |   |           datos_diarios_inicio_8416Y_aemet.json
|   |   |           datos_diarios_inicio_8416_aemet.json
|   |   |           datos_diarios_inicio_8489X_aemet.json
|   |   |           datos_diarios_inicio_8500A_aemet.json
|   |   |           datos_diarios_inicio_8501_aemet.json
|   |   |           datos_diarios_inicio_8523X_aemet.json
|   |   |           datos_diarios_inicio_9001D_aemet.json
|   |   |           datos_diarios_inicio_9019B_aemet.json
|   |   |           datos_diarios_inicio_9051_aemet.json
|   |   |           datos_diarios_inicio_9087_aemet.json
|   |   |           datos_diarios_inicio_9091O_aemet.json
|   |   |           datos_diarios_inicio_9091R_aemet.json
|   |   |           datos_diarios_inicio_9111_aemet.json
|   |   |           datos_diarios_inicio_9170_aemet.json
|   |   |           datos_diarios_inicio_9201K_aemet.json
|   |   |           datos_diarios_inicio_9208E_aemet.json
|   |   |           datos_diarios_inicio_9244X_aemet.json
|   |   |           datos_diarios_inicio_9262_aemet.json
|   |   |           datos_diarios_inicio_9263D_aemet.json
|   |   |           datos_diarios_inicio_9263X_aemet.json
|   |   |           datos_diarios_inicio_9283X_aemet.json
|   |   |           datos_diarios_inicio_9294E_aemet.json
|   |   |           datos_diarios_inicio_9381I_aemet.json
|   |   |           datos_diarios_inicio_9381_aemet.json
|   |   |           datos_diarios_inicio_9390_aemet.json
|   |   |           datos_diarios_inicio_9394X_aemet.json
|   |   |           datos_diarios_inicio_9434P_aemet.json
|   |   |           datos_diarios_inicio_9434_aemet.json
|   |   |           datos_diarios_inicio_9563X_aemet.json
|   |   |           datos_diarios_inicio_9569A_aemet.json
|   |   |           datos_diarios_inicio_9573X_aemet.json
|   |   |           datos_diarios_inicio_9576C_aemet.json
|   |   |           datos_diarios_inicio_9585_aemet.json
|   |   |           datos_diarios_inicio_9619_aemet.json
|   |   |           datos_diarios_inicio_9698U_aemet.json
|   |   |           datos_diarios_inicio_9720X_aemet.json
|   |   |           datos_diarios_inicio_9771C_aemet.json
|   |   |           datos_diarios_inicio_9771_aemet.json
|   |   |           datos_diarios_inicio_9784P_aemet.json
|   |   |           datos_diarios_inicio_9814A_aemet.json
|   |   |           datos_diarios_inicio_9898_aemet.json
|   |   |           datos_diarios_inicio_9981A_aemet.json
|   |   |           datos_diarios_inicio_9987P_aemet.json
|   |   |           datos_diarios_inicio_9990X_aemet.json
|   |   |           datos_diarios_inicio_B013X_aemet.json
|   |   |           datos_diarios_inicio_B228_aemet.json
|   |   |           datos_diarios_inicio_B248_aemet.json
|   |   |           datos_diarios_inicio_B278_aemet.json
|   |   |           datos_diarios_inicio_B346X_aemet.json
|   |   |           datos_diarios_inicio_B434X_aemet.json
|   |   |           datos_diarios_inicio_B569X_aemet.json
|   |   |           datos_diarios_inicio_B691Y_aemet.json
|   |   |           datos_diarios_inicio_B893_aemet.json
|   |   |           datos_diarios_inicio_B954_aemet.json
|   |   |           datos_diarios_inicio_C029O_aemet.json
|   |   |           datos_diarios_inicio_C129Z_aemet.json
|   |   |           datos_diarios_inicio_C139E_aemet.json
|   |   |           datos_diarios_inicio_C229J_aemet.json
|   |   |           datos_diarios_inicio_C249I_aemet.json
|   |   |           datos_diarios_inicio_C329Z_aemet.json
|   |   |           datos_diarios_inicio_C429I_aemet.json
|   |   |           datos_diarios_inicio_C430E_aemet.json
|   |   |           datos_diarios_inicio_C439J_aemet.json
|   |   |           datos_diarios_inicio_C447A_aemet.json
|   |   |           datos_diarios_inicio_C449C_aemet.json
|   |   |           datos_diarios_inicio_C459Z_aemet.json
|   |   |           datos_diarios_inicio_C619I_aemet.json
|   |   |           datos_diarios_inicio_C629X_aemet.json
|   |   |           datos_diarios_inicio_C649I_aemet.json
|   |   |           datos_diarios_inicio_C659H_aemet.json
|   |   |           datos_diarios_inicio_C659M_aemet.json
|   |   |           datos_diarios_inicio_C689E_aemet.json
|   |   |           datos_diarios_inicio_C839I_aemet.json
|   |   |           datos_diarios_inicio_C929I_aemet.json
|   |   |           datos_diarios_year_1976_aemet.json
|   |   |           datos_diarios_year_1977_aemet.json
|   |   |           datos_diarios_year_1978_aemet.json
|   |   |           datos_diarios_year_1979_aemet.json
|   |   |           datos_diarios_year_1980_aemet.json
|   |   |           datos_diarios_year_1996_aemet.json
|   |   |           datos_diarios_year_1997_aemet.json
|   |   |           datos_diarios_year_1998_aemet.json
|   |   |           datos_diarios_year_1999_aemet.json
|   |   |           datos_diarios_year_2000_aemet.json
|   |   |           
|   |   \---CCSS
|   |           expedientes.csv
|   |           expedientes_inundaciones_provicia_a�o.csv
|   |           expedientes_tempestades_provincia_a�o.csv
|   |           expedientes_terremotos_provicia_a�o.csv
|   |           grandes_eventos.csv
|   |           grandes_eventos_desglosados.csv
|   |           indemnizaciones.csv
|   |           indemnizaciones_inundacion_provincia_a�o.csv
|   |           indemnizaciones_tempestades_provincia_a�o.csv
|   |           indemnizaciones_terremotos_provincia_a�o.csv
|   |           
|   \---data_VAL
|       |   da�os_edificios_terremotos.csv
|       |   incendios_clean_final.csv
|       |   VAL_incendios_EVT_clusters_clara.csv
|       |   VAL_terremotos_EVT.rds
|       |   VAL_terremotos_EVT_clusters_clara.rds
|       |   VAL_terremotos_modelo_intensidad.rds
|       |   VAL_terremotos_mundo_MMI.csv
|       |   
|       \---AEMET
+---diccionarios_tablas
|       diccionario_aemet.txt
|       diccionario_aemet_diario.txt
|       
\---POWER BI
        aemet_1.csv
        aemet_2.csv
        aemet_3.csv
        aemet_diarios_join.R
        


