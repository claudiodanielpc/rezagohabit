##Código para calcular el hacinamiento con los datos del Censo 2020

##Borrar datos del entorno
rm(list=ls())


#Paquetería
if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse,srvyr)



#Datos Censo. Lectura y limpieza====
#Se asume que ya se descargaron los datos
#https://www.inegi.org.mx/programas/ccpv/2020/#Microdatos
censo<-read.csv("D:/Viviendas00.csv")%>%
  janitor::clean_names()%>%
  #Seleccionar variables
  select(ent,mun,cobertura,estrato,upm,clavivp,paredes,pisos, techos,
         factor, totcuart,numpers,sersan)%>%
  #Omitir viviendas móviles, locales y refugios
  filter(clavivp<7 | clavivp==99)%>%
  #Condición de rezago
  mutate(
          rezago=case_when(( 
                            (numpers/totcuart)>2.5 |
                            totcuart==99 ) | 
                            paredes %in% c(1,2,3,4,5,6,9) | 
                            pisos %in% c(1,9) |
                            techos %in% c(1,2,3,4,6,7,9,99) |
                            #El documento trae un detalle en esta condición
                            sersan %in% c(3,9)
                          ~ "En rezago", 
                          
                          TRUE ~ "Fuera de rezago"))

#Cuantificación
censo %>%
  #Diseño muestral
  as_survey_design( weights=factor)%>%
  group_by(rezago)%>%
  summarise(
   viviendas=survey_total(vartype = "cv"))%>%
   ungroup()
