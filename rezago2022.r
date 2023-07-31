
##Código para calcular el rezago habitacional en México

##Borrar datos del entorno
rm(list=ls())


#Directorio
setwd("Desktop")
##Crear folders de almacenamiento
dir.create("microdatos", showWarnings = F)

#Paquetería
if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse,srvyr, gt, showtext)
font_add_google("Poppins", "pop")
showtext_auto()

download_and_unzip <- function(url, dir) {
  # Crear directorio si no existe
  if (!dir.exists(dir)) {
    dir.create(dir, showWarnings = FALSE)
  }
  
  # Crear el archivo destino
  destfile <- paste0(dir, "/", basename(url))
  
  # Descarga
  download.file(url, destfile)
  
  # Unzip
  unzip(destfile, exdir = dir)
}

# Use the function to download and unzip data
download_and_unzip("https://www.inegi.org.mx/contenidos/programas/enigh/nc/2022/microdatos/enigh2022_ns_viviendas_csv.zip", "microdatos")
download_and_unzip("https://www.inegi.org.mx/contenidos/programas/enigh/nc/2020/microdatos/enigh2020_ns_viviendas_csv.zip", "data_2020")

# Define function
load_data <- function(file_path) {
  df <- read_csv(file_path) %>%
    janitor::clean_names() %>%
    # Convert to numeric
    mutate(mat_pared = as.numeric(mat_pared),
           mat_pisos = as.numeric(mat_pisos),
           mat_techos = as.numeric(mat_techos),
           # Create variable for housing backlog
           rezago = case_when((tot_resid / num_cuarto) > 2.5 | 
                                mat_pared %in% c(1, 2, 3, 4, 5, 6) | 
                                mat_pisos %in% c(1) |
                                mat_techos %in% c(1, 2, 3, 4, 6, 7, 9) |
                                excusado == 2 ~ "En rezago", 
                                TRUE ~ "Fuera de rezago"),
           # Build key for federal entity
           cve_ent = case_when(nchar(folioviv) == 10 ~ substr(folioviv, 1, 2),
                               nchar(folioviv) == 9 ~ paste0(substr(folioviv, 1, 1), "0"),
                               TRUE ~ folioviv),
           # Create nom_ent from cve_ent
           nom_ent = case_when(cve_ent == "01" ~ "Aguascalientes",
                               cve_ent == "02" ~ "Baja California",
                               cve_ent == "03" ~ "Baja California Sur",
                               cve_ent == "04" ~ "Campeche",
                               cve_ent == "05" ~ "Coahuila de Zaragoza",
                               cve_ent == "06" ~ "Colima",
                               cve_ent == "07" ~ "Chiapas",
                               cve_ent == "08" ~ "Chihuahua",
                               cve_ent == "09" ~ "Ciudad de México",
                               cve_ent == "10" ~ "Durango",
                               cve_ent == "11" ~ "Guanajuato",
                               cve_ent == "12" ~ "Guerrero",
                               cve_ent == "13" ~ "Hidalgo",
                               cve_ent == "14" ~ "Jalisco",
                               cve_ent == "15" ~ "México",
                               cve_ent == "16" ~ "Michoacán de Ocampo",
                               cve_ent == "17" ~ "Morelos",
                               cve_ent == "18" ~ "Nayarit",
                               cve_ent == "19" ~ "Nuevo León",
                               cve_ent == "20" ~ "Oaxaca",
                               cve_ent == "21" ~ "Puebla",
                               cve_ent == "22" ~ "Querétaro",
                               cve_ent == "23" ~ "Quintana Roo",
                               cve_ent == "24" ~ "San Luis Potosí",
                               cve_ent == "25" ~ "Sinaloa",
                               cve_ent == "26" ~ "Sonora",
                               cve_ent == "27" ~ "Tabasco",
                               cve_ent == "28" ~ "Tamaulipas",
                               cve_ent == "29" ~ "Tlaxcala",
                               cve_ent == "30" ~ "Veracruz de Ignacio de la Llave",
                               cve_ent == "31" ~ "Yucatán",
                               cve_ent == "32" ~ "Zacatecas",
                               TRUE ~ "No identificado"))
  return(df)
}

# Use the function
enigh <- load_data("microdatos/viviendas.csv")
enigh_2020 <- load_data("data_2020/viviendas.csv")

#Definir diseño muestral
dm<-enigh%>%
  #Diseño muestral
  as_survey(weights=factor, strata=est_dis, ids=upm)
dm20<-enigh_2020%>%
  #Diseño muestral
  as_survey(weights=factor, strata=est_dis, ids=upm)


#Rezago total
dm%>%
  #Estimación puntual
  group_by(rezago)%>%
  summarise(viviendas=survey_total(),
            pct=round(survey_prop()*100,1))%>%
  ungroup()%>%
  #Eliminar columnas innecesarias viviendas_se, prop_se
    select(-ends_with("se"))%>%
    janitor::adorn_totals("row")%>%
    #Viviendas con big.mark
    mutate(viviendas=scales::comma(viviendas, big.mark = ","))%>%
  #Tabla
    gt()%>%
tab_options(table.font.names = 'Poppins',
source_notes.font.size = 8
)%>%
 tab_header(title=md("**Rezago habitacional en México, 2022**"),
               subtitle=md("*(Número de viviendas y porcentaje)*"))%>%


    cols_label(rezago=md("**Condición**"),
                viviendas=md("**Viviendas**"),
                pct=md("**Porcentaje**"))%>%
    cols_align(align="center")%>%
    tab_source_note(
      md("Fuente: @claudiodanielpc con datos de la ENIGH 2022."))%>%
      #Eliminar bordes arriba y abajo
      tab_options(table.border.top.width = px(0),
                  table.border.bottom.width = px(0))%>%
  tab_options(column_labels.background.color = "#F0449C")%>%
  #Salvar tabla
  gtsave("rezago2022.png")



#Rezago por estrato
dm%>%
filter(rezago=="En rezago")%>%
  #Estimación puntual
  group_by(est_socio,rezago)%>%
  summarise(viviendas=survey_total(vartype="cv"))%>%
  ungroup()%>%
  #Seleccionar columnas est_socio y viviendas
  select(est_socio,viviendas)%>%    
    #Viviendas con big.mark
    #Calcula porcentaje
    mutate(pct=round(viviendas/sum(viviendas)*100,1),
    est_socio=case_when(est_socio==1 ~ "Bajo",
                        est_socio==2 ~ "Medio bajo",
                        est_socio==3 ~ "Medio alto",
                        est_socio==4 ~ "Alto")
    
    )%>%
    janitor::adorn_totals("row")%>%
    mutate(viviendas=scales::comma(viviendas, big.mark = ","),
    #Estratos
    )%>%
      #Tabla
    gt()%>%
tab_options(table.font.names = 'Poppins',
source_notes.font.size = 8
)%>%
 tab_header(title=md("**Rezago habitacional en México por estrato socioeconómico, 2022**"),
               subtitle=md("*(Número de viviendas y porcentaje)*"))%>%
    cols_label(est_socio=md("**Estrato**"),
                viviendas=md("**Viviendas**"),
                pct=md("**Porcentaje**"))%>%
    cols_align(align="center")%>%
    tab_source_note(
  md("Nota: La variable de estrato socioeconómico fue construida por el INEGI con base en la información de la ENIGH 2022.")
) %>%
tab_source_note(
  md("Fuente: @claudiodanielpc con datos de la ENIGH 2022.")
)%>%
    # tab_source_note(
    #   md("Nota: La variable de estrato socioeconómica fue construida por el INEGI con base en la información de la ENIGH 2022.\nFuente: @claudiodanielpc con datos de la ENIGH 2022."))%>%
      #Eliminar bordes arriba y abajo
      tab_options(table.border.top.width = px(0),
                  table.border.bottom.width = px(0))%>%
  tab_options(column_labels.background.color = "#F0449C")%>%
  #Salvar tabla
  gtsave("rezago2022_estrato.png")


  #Rezago habitacional por entidad federativa
  dm%>%
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
    geom_col(fill="#F0449C")+
    geom_text(aes(label=paste0(pct,"%")),hjust=-.1, size=5)+
    labs(x="",y="",title="Rezago habitacional por entidad federativa, 2022",
         subtitle="(Porcentaje de viviendas en rezago habitacional respecto al total en cada entidad federativa)",
         caption="Fuente: @claudiodanielpc con datos de la ENIGH 2022.")+
    theme_minimal()+
    theme(
        text = element_text(family = "pop"),

      plot.title = element_text(size=20, face="bold",hjust=0),
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
    ggsave("rezago_entidad2022.png", width = 20, height = 10, bg="white",dpi=100)

#Rezago 2020 y 2022 por entidad federativa
rez2020<-dm20%>%
  #Estimación puntual
  group_by(nom_ent,rezago)%>%
  summarise(rez20=survey_total(),
            pct20=round(survey_prop()*100,1))%>%
              ungroup()%>%
  #Eliminar columnas innecesarias viviendas_se, prop_se
    select(-ends_with("se"))%>%
    filter(rezago=="En rezago")
  
rez2022<-dm%>%
  #Estimación puntual
  group_by(nom_ent,rezago)%>%
  summarise(rez22=survey_total(),
            pct22=round(survey_prop()*100,1))%>%
              ungroup()%>%
  #Eliminar columnas innecesarias viviendas_se, prop_se
    select(-ends_with("se"))%>%
    filter(rezago=="En rezago")

rezagos<-left_join(rez2020,rez2022,by="nom_ent")

rezagos%>%
#Hacer scatterplot
ggplot(aes(x=pct20,y=pct22,label=nom_ent))+
  geom_point(color="#F0449C",size=4)+
  #Colorear puntos que estén por debajo de la diagonal
  geom_point(data=rezagos%>%filter(pct20<pct22),color="#4ca3ea",size=4)+
  #Agregar diagonal
  geom_abline(intercept = 0, slope = 1, color="black",linetype="dashed")+
  ggrepel::geom_text_repel(hjust = -.1, size = 3)+
  labs(x="Porcentaje de viviendas en rezago habitacional, 2020",
       y="Porcentaje de viviendas en rezago habitacional, 2022",
       title="Rezago habitacional por entidad federativa, 2020 y 2022",
       subtitle="(Porcentaje de viviendas en rezago habitacional respecto al total en cada entidad federativa)",
       caption="
       Nota: Los puntos azules corresponden a las entidades federativas cuyo porcentaje de rezago habitacional en 2022 es mayor al de 2020.
       Fuente: @claudiodanielpc con datos de la ENIGH 2020 y 2022.")+
  theme_minimal()+
  theme(
        text = element_text(family = "pop"),
      plot.title = element_text(size=20, face="bold",hjust=0),
          plot.subtitle = element_text(size=16, face="italic",hjust=0),
          plot.caption = element_text(size=11,hjust=0),
          axis.text.y = element_text(size=13),
          axis.text.x = element_text(size=13),
          axis.ticks.x = element_blank(),

          panel.grid.major.x = element_line(color="#EAEAEA"),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(color="#EAEAEA"),
          panel.grid.minor.y = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          plot.margin = margin(10,10,10,10, "pt"))

#GUARDAR GRÁFICA
ggsave("rezago_entidad2020_2022.png", width = 20, height = 10, bg="white",dpi=100)

rezagos%>%
mutate(diferencia=pct22-pct20)%>%
  #Ordenar de mayor a menor
  arrange(diferencia)


#Antigüedad de las viviendas en rezago habitacional
dm%>%
filter(rezago=="En rezago")%>%
#Crear variables de antigüedad
  mutate(catego=case_when(antiguedad==0 & antiguedad<5 ~ "0 y menos de 5 años",
                          antiguedad>=5 & antiguedad<10 ~ "5 y menos de 10 años",
                          antiguedad>=10 & antiguedad<20 ~ "10 y menos de 20 años",
                          antiguedad>=20 & antiguedad<30 ~ "20 y menos de 30 años",
                          antiguedad>=30 & antiguedad<40 ~ "30 y menos de 40 años",
                          antiguedad>=40 & antiguedad<50 ~ "40 y menos de 50 años",
                          antiguedad>=50 & antiguedad<60 ~ "50 y menos de 60 años",
                          antiguedad>=60 & antiguedad<70 ~ "60 y menos de 70 años",
                          antiguedad>=70 & antiguedad<80 ~ "70 y menos de 80 años",
                          antiguedad>=80 & antiguedad<90 ~ "80 y menos de 90 años",
                          antiguedad>=90 ~ "Más de 90 años",
                          TRUE ~ "No especificado"))%>%
      
  group_by(catego,rezago)%>%
  summarise(viviendas=survey_total(vartype = "cv"))%>%
  ungroup()%>%
  filter(catego!="No especificado")%>%
  #Calcular porcentaje
  mutate(pct=round(viviendas/sum(viviendas)*100,1),
  #Ordenar categorías
  catego=fct_relevel(catego,"0 y menos de 5 años",
                     "5 y menos de 10 años",
                     "10 y menos de 20 años",
                     "20 y menos de 30 años",
                     "30 y menos de 40 años",
                     "40 y menos de 50 años",
                     "50 y menos de 60 años",
                     "60 y menos de 70 años",
                     "70 y menos de 80 años",
                     "80 y menos de 90 años",
                     "Más de 90 años"))%>%
  #Gráfica de barras
  ggplot(aes(x=catego,y=pct))+
  geom_col(fill="#F0449C")+
  geom_text(aes(label=paste0(pct,"%")),hjust=.5, vjust=-.5, size=5)+
  labs(x="",y="",title="Antigüedad de las viviendas en rezago habitacional, 2022",
       subtitle="(Porcentaje de viviendas en rezago habitacional que cuentan con información sobre su antigüedad)",
       caption="
       Nota: De las 9 millones de viviendas que existen en rezago habitacional, 20.7% no cuentan con información sobre su antigüedad.
       Fuente: @claudiodanielpc con datos de la ENIGH 2022.")+
  theme_minimal()+
   theme(plot.title = element_text(hjust = 0, size=25,face="bold"),
        plot.subtitle = element_text(hjust = 0, size=15, face="italic"),
        plot.caption = element_text(hjust = 0,size=12),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        text=element_text("pop",size=20))

#Salvar
ggsave("antiguedad2022.png", width = 20, height = 10, bg="white",dpi=100)

#Viviendas propias y propias en pago
dm%>%
  filter(rezago=="En rezago")%>%
  mutate(tenencia=case_when(tenencia==1 ~ "Rentada",
                            tenencia==2 ~ "Prestada",
                            tenencia==3 ~ "Propia y la está pagando",
                            tenencia==4 ~ "Propia",
                            tenencia==5 ~ "Intestada o en litigio",
                            tenencia==6 ~ "Otra situación"))%>%

  #Estimación puntual
  group_by(rezago,tenencia)%>%
  summarise(
    viviendas=survey_total(vartype = "cv"),
    pct=round(survey_prop(
    vartype = "cv")*100,1))%>%
  ungroup()%>%
  #Ordenar de mayor a menor
  arrange(desc(pct))


dm%>%
  filter(rezago=="En rezago" & !is.na(tipo_adqui))%>%
  #Estimación puntual
  group_by(tipo_adqui,rezago)%>%
  summarise(viviendas=survey_total(vartype = "cv"))%>%
              ungroup()%>%
              #Calcular porcentaje
              mutate(pct=round(viviendas/sum(viviendas)*100,1),
              #Categorías
              tipo_adqui=case_when(tipo_adqui==1 ~ "La compró hecha",
                                   tipo_adqui==2 ~ "La mandó construir",
                                   tipo_adqui==3 ~ "La construyó él mismo",
                                   tipo_adqui==4 ~ "La obtuvo de otra manera"))%>%

  #Gráfica de barras horizontal con porcentaje
    ggplot(aes(x=reorder(tipo_adqui,pct),y=pct))+
    geom_col(fill="#F0449C")+
    geom_text(aes(label=paste0(pct,"%")),hjust=-.1, size=5)+
    labs(x="",y="",title="Forma de adquisición de las viviendas en rezago habitacional, 2022",
         subtitle="(Porcentaje de viviendas propias y propias en pago en rezago habitacional)",
         caption="Fuente: @claudiodanielpc con datos de la ENIGH 2022.")+
    theme_minimal()+
    theme(
        text = element_text(family = "pop"),

      plot.title = element_text(size=20, face="bold",hjust=0),
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
    ggsave("adquisicion2022.png", width = 20, height = 10, bg="white",dpi=100)
