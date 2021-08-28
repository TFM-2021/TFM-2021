
terremotos_selected_modelo_intensidad <- readRDS("02_Data_preparation/00_Select_data/terremotos_selected_modelo_intensidad.rds")


terremotos_selected_modelo_intensidad <- terremotos_selected_modelo_intensidad %>%
  drop_na()



unique(terremotos_selected_modelo_intensidad$`Prof. (Km)`)
unique(terremotos_selected_modelo_intensidad$Inten.)
unique(terremotos_selected_modelo_intensidad$Mag.)

terremotos_selected_modelo_intensidad <- terremotos_selected_modelo_intensidad %>%
  dplyr::filter(Inten. != "-1.0",
                Inten. != "-1")


terremotos_selected_modelo_intensidad %>%
  dplyr::group_by(Mag.)%>%
  dplyr::summarise(suma = n()) %>%
  ggplot()+
  geom_col(aes(Mag., suma))

terremotos_selected_modelo_intensidad %>%
  dplyr::group_by(Inten.)%>%
  dplyr::summarise(suma = n()) %>%
  ggplot()+
  geom_col(aes(Inten., suma))


terremotos_selected_modelo_intensidad <- janitor::clean_names(terremotos_selected_modelo_intensidad)

saveRDS(terremotos_selected_modelo_intensidad, file = "02_Data_preparation/01_Clean_data/terremotos_clean_modelo_intensidad.rds")




