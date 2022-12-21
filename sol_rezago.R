##Estimación de soluciones habitacionales para el rezago habitacional

#Se borra todo lo que se encuentra en el entorno

rm(list=ls())

# Librerías ====
if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, showtext, srvyr)


#Directorio de trabajo
setwd("D:/")

#Descarga de archivos
url<-"https://www.inegi.org.mx/contenidos/programas/enigh/nc/2018/microdatos/enigh2018_ns_viviendas_csv.zip"

##Creación de directorio temporal

td<- tempdir()

# Descarga del archivo temporal


tf = tempfile(tmpdir=td, fileext=".zip")
download.file(url, tf)

# unzip

unzip(tf, files="viviendas.csv", exdir=td, 
      overwrite=TRUE)
fpath=file.path(td,"viviendas.csv")
unlink(td)

#Leer el archivo

enigh<-read.csv(fpath)%>%
  rename(folioviv=1)


#Calcular rezago habitacional====

enigh<-enigh%>%
  mutate(
    across(starts_with("mat") & where(is.character),
           ~ replace_na(parse_number(.x, na = c('', 'NA', '&')), 0)),
    rezago=if_else(
    ((tot_resid / num_cuarto) > 2.5) |
      (mat_pared %in% 1:6) |
      (mat_techos %in% c(1:4, 6, 7, 9)) |
      (mat_pisos == 1) |
      (excusado == 2),
    "En rezago",
    "Fuera de rezago"
  ))



#Cálculo de soluciones al rezago: Metodología 1====


#Generación de indicador de soluciones

enigh<-enigh%>%
  mutate(sol=0,
         sol=case_when(rezago=="En rezago" & (mat_pared==1  | mat_pared==2
                       | mat_pared==4 | mat_pared==5) ~ "Autoproducción",
                       rezago=="En rezago" & sol!=1 & 
                         (mat_techos==1 | mat_techos==2 | mat_techos==6) ~ "Mejoramiento",
         rezago=="En rezago" & sol!=1 & sol!=2 & ( mat_pared==3 | mat_pared==6) ~"Autoproducción",
         rezago=="En rezago" & sol!=1 & sol!=2 & sol!=3 & (mat_techos==3 | mat_techos==4 | 
                                                    mat_techos==7 | mat_techos==9) ~ "Mejoramiento",
         rezago=="En rezago" & sol!=1 & sol!=2 & sol!=3 & sol!=4 & (mat_pisos==1) ~ "Mejoramiento",
         rezago=="En rezago" & sol!=1 & sol!=2 & sol!=3 & sol!=4 & sol!=5 & 
           (tot_resid/num_cuarto)>2.5 ~ "Ampliación o reemplazo",
         rezago=="En rezago" & sol!=1 & sol!=2 & sol!=3 & sol!=4 & sol!=5 & sol!=6 &  
           ( excusado==2) ~"Ampliación o reemplazo")
  )
         


#Generación de indicador de número optimo de num_cuartoos
#enigh<-enigh%>%
 # mutate(optimo=0,
  #       optimo=if_else((tot_resid / num_num_cuartoo)> 2.5,
   #                  ceiling((tot_resid / 2.5)-num_num_cuartoo),optimo),
    #     optimo=if_else(excusado==2,optimo+1,
     #                optimo))





#Definir diseño muestral

mydesign <-enigh%>%
  as_survey_design(ids=upm,
                   strata=est_dis,
                   weights=factor)

#Prueba de significancia estadística
mydesign %>%
  filter(rezago=="En rezago")%>%
  group_by(sol)%>%  
  summarize(total=survey_total(
                               vartype = c("cv", "ci")))

#Segunda descarga====

#Descarga de archivos
url<-"https://www.inegi.org.mx/contenidos/programas/enigh/nc/2016/microdatos/enigh2016_ns_viviendas_csv.zip"

##Creación de directorio temporal

td<- tempdir()

# Descarga del archivo temporal


tf = tempfile(tmpdir=td, fileext=".zip")
download.file(url, tf)

# unzip

unzip(tf, files="viviendas.csv", exdir=td, 
      overwrite=TRUE)
fpath=file.path(td,"viviendas.csv")
unlink(td)

#Leer el archivo

enigh16<-read.csv(fpath)%>%
  rename(folioviv=1)



#Calcular rezago habitacional====

enigh16<-enigh16%>%
  mutate(
    across(starts_with("mat") & where(is.character),
           ~ replace_na(parse_number(.x, na = c('', 'NA', '&')), 0)),
    rezago=if_else(
      ((tot_resid / num_cuarto) > 2.5) |
        (mat_pared %in% 1:6) |
        (mat_techos %in% c(1:4, 6, 7, 9)) |
        (mat_pisos == 1) |
        (excusado == 2),
      "En rezago",
      "Fuera de rezago"
    ))


#Cálculo de soluciones al rezago: Metodología 2====

enigh16<-enigh16%>%
  #Cálculo del número de cuartos adicionales. Se incluyen aquellas vivienda que no tienen
  #habitantes en hacinamiento pero que declararon no contar con baño
    mutate(optimo=0,
         optimo=if_else((tot_resid / num_cuarto)> 2.5,
                                          ceiling((tot_resid / 2.5)-num_cuarto),
                        optimo),
                             optimo=if_else(excusado==2,optimo+1,
                                        optimo),
noviv = if_else(tipo_viv == "5", 1, 0), #Locales no construidos para vivienda
noconst = if_else((
  optimo == 1 | optimo == 2 | 
    (mat_pared %in% 1:6)) & (tipo_viv == "3" |
                               tipo_viv == "2" | tipo_viv == "4"), 1, 0), #Si tienen que ampliarse con uno o dos cuartos o si tienen que autoproducirse pero el tipo de vivienda no lo permite
autoprod = if_else( (mat_pared %in% 1:6) == 1, 1, 0), #Aquellas que deben autoproducirse por tener paredes endebles
mejora = ifelse((mat_techos %in% c(1:4, 6, 7, 9)) | mat_pisos == 1, 1, 0), #Necesitan un mejoramiento en su techo o piso
ampli = ifelse(optimo == 1 | optimo == 2, 1, 0), #Necesitan ampliarse uno o dos cuartos

sol = case_when( #Se calcula la solución habitacional para eliminar el rezago
  (rezago == "En rezago") & 
    (noviv == 1 | noconst == 1 | (optimo >= 3  & autoprod != 1)) ~ 1, #Se necesita vivienda nueva
  (rezago == "En rezago") 
  & (noviv == 0 & noconst == 0) & (autoprod == 1) ~ 2, #Las noviv no se captan los materiales, por lo que no están en la categoría de autoprod. 
  (rezago == "En rezago") 
  & (mejora == 1 & ampli == 1) & noviv == 0 &
    noconst == 0 & autoprod == 0 ~ 3, #Necesitan mejoramiento y ampliación al mismo tiempo, pero no van a ser vivienda nueva ni autoproducción
  (rezago == "En rezago") 
  & (mejora == 1 & ampli == 0) & noviv == 0 & 
  noconst == 0 & autoprod == 0 ~ 4, #Necesita solo mejoramiento, pero no ampliación y no vivienda nueva y no autoproducción
  (rezago == "En rezago") 
  & (mejora == 0 & ampli == 1) & noviv == 0 & noconst == 0 & autoprod == 0 ~ 5 #Necesita solo ampliación, pero no mejoramiento y no vivienda nueva y no autoproducción
)
  )


#Definir diseño muestral

mydesign16 <-enigh16%>%
  as_survey_design(ids=upm,
                   strata=est_dis,
                   weights=factor)



#Prueba de significancia estadística
mydesign16 %>%
  filter(rezago=="En rezago")%>%
  group_by(sol)%>%  
  summarize(total=survey_prop(
    vartype = c("cv", "ci")))
