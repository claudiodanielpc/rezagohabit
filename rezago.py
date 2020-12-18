##Script para calcular el rezago habitacional en México
##Fuente: ENIGH 2018
import os
import pandas as pd 
import numpy as np
import seaborn as sns
from urllib.request import urlopen
import matplotlib.pyplot as plt
from zipfile import ZipFile

##URL tabla viviendas de ENIGH
zipurl="https://www.inegi.org.mx/contenidos/programas/enigh/nc/2018/microdatos/enigh2018_ns_viviendas_csv.zip"

##Cambiar directorio de trabajo

dir=os.chdir("C:/Users/ALIENWARE/Documents/enigh")

##Descomprimir
zipresp = urlopen(zipurl)
tempzip = open("tempfile.zip", "wb")
tempzip.write(zipresp.read())
tempzip.close()
zf = ZipFile("tempfile.zip")
zf.extractall(dir)
zf.close()

##Leer archivo csv
vivi=pd.read_csv("viviendas.csv")


#Crear variable de clave de entidad federativa

##Obtener el largo del folioviv
vivi['largo'] = vivi['folioviv'].astype(str).map(len)

##Extraer la clave de entidad
vivi['cve_ent'] = np.where((vivi['largo']== 9), 
vivi.folioviv.astype(str).str[:1],vivi.folioviv.astype(str).str[:2] )

##Agregar cero a la clave de la entidad
vivi['cve_ent'] = vivi['cve_ent'].str.zfill(2)


##Llamar al catálogo de entidades
cata=pd.read_excel("C:/Users/ALIENWARE/Documents/bolet/estatal/claves.xlsx", engine="openpyxl")
##transformar a texto la clave de entidad
cata['cve_ent'] = cata['cve_ent'].astype(str)
cata['cve_ent'] = cata['cve_ent'].str.zfill(2)
##Dado que es una tabla a nivel municipal, hacer el subset para dejar únicamente los datos de las 32 entidades
cata=cata[["cve_ent","nom_ent"]]
cata= cata.drop_duplicates(subset=['cve_ent'])

##pegar nombres
vivi= pd.merge(vivi,cata,how="left",left_on="cve_ent",right_on="cve_ent")


##Calcular rezago habitacional
##Mutar material de pisos a numérico
vivi['mat_pisos'] = pd.to_numeric(vivi['mat_pisos'],errors='coerce')

##Condiciones de rezago

condiciones=[(vivi["tot_resid"]/vivi["num_cuarto"]>2.5) | 
((vivi["mat_pared"]>=1) & (vivi["mat_pared"]<=6)) |
((vivi["mat_techos"]>=1) & (vivi["mat_techos"]<=4)) |
((vivi["mat_techos"]>=6) & (vivi["mat_techos"]<=7)) |
(vivi["mat_techos"]==9) |
(vivi["mat_pisos"]==1) |
(vivi["excusado"]==2)]

opciones=[1]

vivi["rezago"]=np.select(condiciones,opciones)

vivi["rezago"]=np.where(vivi["rezago"]==1,"En rezago","Fuera de rezago")

##Calcular rezago habitacional total
rez=pd.crosstab(index=vivi.rezago, 
                           columns="count",
                           values=vivi.factor,
                           aggfunc=np.sum, margins=False, 
                           margins_name="Total")


# Colores de gráfico
colors = ["#E13F29", "#D69A80"]

##Gráfica del rezago habitacional total
rez.plot.pie(subplots=True,figsize=(12, 5),
autopct='%1.1f%%',explode=(0, 0.15),colors=colors)
#Título
plt.title('México. Parque habitacional por condición de rezago, 2018 \n (%)',
fontsize=14,fontweight="bold", loc="left")
plt.legend("",frameon=False)
plt.axis('equal')
plt.ylabel('')
plt.xlabel('\nFuente: @claudiodanielpc con información de INEGI. Encuesta Nacional de Ingresos y Gastos de los Hogares (ENIGH) 2018')
##Guardar y mostrar la gráfica
plt.savefig("rezago.png",format="png",dpi=600,transparent=False)
plt.show()


################################
##Calcular rezago habitacional por entidad federativa
##Filtrar rezago
entidad = vivi[vivi['rezago']=="En rezago"]

##Calcular rezago habitacional por entidad
rezent=pd.crosstab(index=entidad.nom_ent, 
                           columns=entidad.rezago,
                           values=entidad.factor,
                           aggfunc=np.sum, margins=False, 
                           margins_name="Total")

##Ordenar de mayor a menor
rezent.sort_values('En rezago',inplace=True)
##Calcular porcentaje del total
rezent["pct"]=rezent["En rezago"]/sum(rezent["En rezago"])*100
##Dejar solo la variable que se graficará
rezent=rezent[["pct"]]

##Gráfica
rezent.plot.barh(figsize=(19, 7),color="#2ca25f")
#Título
plt.title('México. Parque habitacional en rezago habitacional por entidad federativa, 2018 \n (%)',
fontsize=14,fontweight="bold", loc="left")
plt.legend("",frameon=False)
plt.xlabel('Porcentaje del total del rezago habitacional\nFuente: @claudiodanielpc con información de INEGI. Encuesta Nacional de Ingresos y Gastos de los Hogares (ENIGH) 2018')
##Guardar y mostrar la gráfica
plt.savefig("rezagoent.png",format="png",dpi=600,transparent=False)
plt.show()


##Calcular rezago habitacional en cada entidad


rezent2=pd.crosstab(index=vivi.nom_ent, 
                           columns=vivi.rezago,
                           values=vivi.factor,
                           aggfunc=np.sum, margins=True, 
                           margins_name="Nacional")


##Calcular porcentaje del total
rezent2["pct"]=rezent2["En rezago"]/rezent2["Nacional"]*100
##Ordenar de mayor a menor
rezent2.sort_values('pct',inplace=True)

##Dejar solo la variable que se graficará
rezent2=rezent2[["pct"]]


##Gráfica
rezent2.plot.barh(figsize=(19, 7),color="green")
#Título
plt.title('México. Parque habitacional en rezago habitacional por entidad federativa, 2018 \n (% del total de cada entidad)',
fontsize=14,fontweight="bold", loc="left")
plt.legend("",frameon=False)
plt.xlabel('Porcentaje de rezago habitacional\nFuente: @claudiodanielpc con información de INEGI. Encuesta Nacional de Ingresos y Gastos de los Hogares (ENIGH) 2018')
##Guardar y mostrar la gráfica
plt.savefig("rezagoent2.png",format="png",dpi=600,transparent=False)
plt.show()