
##Código para calcular el rezago habitacional en México

##Borrar datos del entorno
rm(list=ls())


#Directorio en mac
setwd("Desktop")
##Crear folders de almacenamiento
dir.create("microdatos", showWarnings = F)

#Paquetería
if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse,srvyr, gt)

#url enigh
url_enigh <- "https://www.inegi.org.mx/contenidos/programas/enigh/nc/2022/microdatos/enigh2022_ns_viviendas_csv.zip"

#Descargar y descomprimir
download.file(url_enigh, destfile = "microdatos/enigh2022_ns_viviendas_csv.zip")
unzip("microdatos/enigh2022_ns_viviendas_csv.zip", exdir = "microdatos")

#Cargar datos
enigh <- read_csv("microdatos/viviendas.csv")%>%
  janitor::clean_names()%>%
  #Convertir a numeric
  mutate(mat_pared=as.numeric(mat_pared),
         mat_pisos=as.numeric(mat_pisos),
         mat_techos=as.numeric(mat_techos),
         #Crear variable de rezago habitacional
         rezago=case_when((tot_resid/num_cuarto)>2.5 | 
                            mat_pared %in% c(1,2,3,4,5,6) | 
                            mat_pisos %in% c(1) |
                            mat_techos %in% c(1,2,3,4,6,7,9) |
                            excusado==2
                          ~ "En rezago", 
                          
                          TRUE ~ "Fuera de rezago"),
         #extraer primero dos caracteres de folioviv si el largo es de 10, si no, extraer el primero y añadir un cero al final  
          cve_ent=case_when(nchar(folioviv)==10 ~ substr(folioviv,1,2),
                              nchar(folioviv)==9 ~ paste0(substr(folioviv,1,1),"0"),
                              TRUE ~ folioviv),
        #Crear nom_ent a partir de cve_ent
        nom_ent=case_when(cve_ent=="01" ~ "Aguascalientes",
                          cve_ent=="02" ~ "Baja California",
                          cve_ent=="03" ~ "Baja California Sur",
                          cve_ent=="04" ~ "Campeche",
                          cve_ent=="05" ~ "Coahuila de Zaragoza",
                          cve_ent=="06" ~ "Colima",
                          cve_ent=="07" ~ "Chiapas",
                          cve_ent=="08" ~ "Chihuahua",
                          cve_ent=="09" ~ "Ciudad de México",
                          cve_ent=="10" ~ "Durango",
                          cve_ent=="11" ~ "Guanajuato",
                          cve_ent=="12" ~ "Guerrero",
                          cve_ent=="13" ~ "Hidalgo",
                          cve_ent=="14" ~ "Jalisco",
                          cve_ent=="15" ~ "México",
                          cve_ent=="16" ~ "Michoacán de Ocampo",
                          cve_ent=="17" ~ "Morelos",
                          cve_ent=="18" ~ "Nayarit",
                          cve_ent=="19" ~ "Nuevo León",
                          cve_ent=="20" ~ "Oaxaca",
                          cve_ent=="21" ~ "Puebla",
                          cve_ent=="22" ~ "Querétaro",
                          cve_ent=="23" ~ "Quintana Roo",
                          cve_ent=="24" ~ "San Luis Potosí",
                          cve_ent=="25" ~ "Sinaloa",
                          cve_ent=="26" ~ "Sonora",
                          cve_ent=="27" ~ "Tabasco",
                          cve_ent=="28" ~ "Tamaulipas",
                          cve_ent=="29" ~ "Tlaxcala",
                          cve_ent=="30" ~ "Veracruz de Ignacio de la Llave",
                          cve_ent=="31" ~ "Yucatán",
                          cve_ent=="32" ~ "Zacatecas",
                          TRUE ~ "No identificado"))



enigh%>%
  #Diseño muestral
  as_survey(weights=factor, strata=est_dis, ids=upm)%>%
  #Estimación puntual
  group_by(rezago)%>%
  summarise(viviendas=survey_total(),
            pct=round(survey_prop()*100,1))%>%
  ungroup()%>%
  #Eliminar columnas innecesarias viviendas_se, prop_se
    select(-ends_with("se"))%>%
    #Viviendas con big.mark
    mutate(viviendas=scales::comma(viviendas, big.mark = ","))%>%
  #Tabla
    gt()%>%
    tab_header(title=md("**Rezago habitacional en México, 2022**"),
                 subtitle=md("*(Número de viviendas y porcentaje)*"),
                 )%>%
    cols_label(rezago=md("**Condición**"),
                viviendas=md("**Viviendas**"),
                pct=md("**Porcentaje**"))%>%
    cols_align(align="center")%>%
    tab_source_note(
      md("Fuente: @claudiodanielpc con datos de la ENIGH 2022."))%>%
      #Eliminar bordes arriba y abajo
      tab_options(table.border.top.width = px(0),
                  table.border.bottom.width = px(0))%>%
  tab_options(column_labels.background.color = "#F039B1")%>%
  #Salvar tabla
  gtsave("rezago2022.png")



  #Rezago habitacional por entidad federativa

  enigh%>%
  #Diseño muestral
  as_survey(weights=factor, strata=est_dis, ids=upm)%>%
  #Estimación puntual
  group_by(nom_ent,rezago)%>%
  summarise(viviendas=survey_total(),
            pct=round(survey_prop()*100,1))%>%
              ungroup()%>%
  #Eliminar columnas innecesarias viviendas_se, prop_se
    select(-ends_with("se"))%>%
    filter(rezago=="En rezago")%>%
    #Gráfica de barras horizontal con porcentaje
    ggplot(aes(x=reorder(nom_ent,pct),y=pct))+
    geom_col(fill="#a0c6e0")+
    geom_text(aes(label=paste0(pct,"%")),hjust=-.1, size=5)+
    labs(x="",y="",title="Rezago habitacional por entidad federativa, 2022",
         subtitle="(Porcentaje de viviendas en rezago habitacional respecto al total en cada entidad federativa)",
         caption="Fuente: @claudiodanielpc con datos de la ENIGH 2022.")+
    theme_minimal()+
    theme(plot.title = element_text(size=20, face="bold",hjust=0),
          plot.subtitle = element_text(size=16, face="italic",hjust=0),
          plot.caption = element_text(size=11,hjust=0),
          axis.text.y = element_text(size=13),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(color="#EAEAEA"),
          panel.grid.minor.y = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          plot.margin = margin(10,10,10,10, "pt"))+
    coord_flip()

    #GUARDAR GRÁFICA
    ggsave("rezago_entidad2022.png", width = 20, height = 10)
