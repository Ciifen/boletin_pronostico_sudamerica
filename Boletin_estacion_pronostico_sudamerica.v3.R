#install.packages("officer")
library(xlsx)
library(readxl)
library(tidyverse)
library(officer)

directorio<-'D:/CIIFEN/Boletines_Sudamerica/2024/SON'
dir.create(paste0(directorio,'/Union'))


folder_peru<-''
setwd(directorio)

categorizacion<-function(tabla_datos){
  total_data_prec<-data.frame(nombre=tabla_datos[,1],latitud=as.numeric(tabla_datos[,2]),longitud=as.numeric(tabla_datos[,3]),bajo=as.numeric(tabla_datos[,4]),medio=as.numeric(tabla_datos[,5]),alto=as.numeric(tabla_datos[,6]),codigo=tabla_datos[,7])
  #pos_na<-which(is.na(total_data_prec[,1]))
  #clasificando y rellenando codigo
  for(k in 1:nrow(total_data_prec)){
    aux_max<-max(total_data_prec[k,4:6],na.rm = T)
    TEST_50<-length(which(total_data_prec[k,4:6]==aux_max))
    TEST<-max(c(NA,NA,NA),na.rm = T)
    if(TEST_50==2){
      total_data_prec[k,7]<-0
    }else if(aux_max==TEST){
      total_data_prec[k,7]<-NA
    }else if(aux_max==33){
      total_data_prec[k,7]<-0
    }else{
      pos_aux<-which(total_data_prec[k,4:6]==aux_max)
      total_data_prec[k,7]<-pos_aux[1]
    }
  }
  
  return(total_data_prec)
  #print('terminó la ejecución, muchas gracias vuelva pronto')
}

NOMBRE_DATE<-'SON'
anio<-2024
archivos_xlsx<-list.files(pattern = '.xlsx')
archivos<-list.files(pattern = '.xls')
archivos_csv <- list.files(pattern = '.csv')
#COLOMBIA
nombre_colombia1<-grep(pattern = 'PREC',x =archivos_csv )
nombre_colombia2<-grep(pattern = 'TMX',x =archivos_csv )
nombre_colombia3<-grep(pattern = 'TMN',x =archivos_csv )
# PRECIP_COL<-xlsx::read.xlsx(archivos_xlsx[nombre_colombia],sheetName = 'PRECIPITACION')
# MAX_COL<-xlsx::read.xlsx(archivos_xlsx[nombre_colombia],sheetName = 'MAXIMA')
# MIN_COL<-xlsx::read.xlsx(archivos_xlsx[nombre_colombia],sheetName = 'MINIMA',endRow = 216)
PRECIP_COL<-read.csv(archivos_csv[nombre_colombia1],sep = ';',header = T,dec = '.')
MAX_COL<-read.csv(archivos_csv[nombre_colombia2],sep = ';',header = T,dec = '.')
MIN_COL<-read.csv(archivos_csv[nombre_colombia3],sep = ';',header = T,dec = '.')
#BOLIVIA
nombre_BOL<-grep(pattern = 'BOL',x =archivos_xlsx )
PRECIP_BOL<-xlsx::read.xlsx(archivos_xlsx[nombre_BOL],sheetName = paste('PCPN'))
MAX_BOL<-xlsx::read.xlsx(archivos_xlsx[nombre_BOL],sheetName = paste('TX'))
MIN_BOL<-xlsx::read.xlsx(archivos_xlsx[nombre_BOL],sheetName = paste('TN'))
#ECUADOR
nombre_ECU<-grep(pattern = 'ECU',x =archivos_xlsx )
PRECIP_ECU<-xlsx::read.xlsx(archivos_xlsx[nombre_ECU],sheetName = 'RR')
#PRECIP_ECU<-xlsx::read.xlsx(archivos_xlsx[nombre_ECU],sheetName = 'RR')
MAX_ECU<-xlsx::read.xlsx(archivos_xlsx[nombre_ECU],sheetName = 'TMAX',colIndex = c(1:6))
MIN_ECU<-xlsx::read.xlsx(archivos_xlsx[nombre_ECU],sheetName = 'TMIN',colIndex = c(1:6))
#VENEZUELA
nombre_VEN<-grep(pattern = 'VEN',x =archivos)
nombre_VEN <- as.character(archivos[nombre_VEN])
PRECIP_VEN<-as.data.frame(readxl::read_excel(nombre_VEN,sheet= paste0('TERCILES_PRECIPITACION_',NOMBRE_DATE,anio)))
precip_ven<-PRECIP_VEN[1:109,1:6]
MAX_VEN<-as.data.frame(readxl::read_excel(nombre_VEN,sheet= paste0('TERCILES_TMAX_',NOMBRE_DATE,anio)))
max_ven<-MAX_VEN[1:34,1:6]
MIN_VEN<-as.data.frame(readxl::read_excel(nombre_VEN,sheet= paste0('TERCILES_TMIN_',NOMBRE_DATE,anio)))
min_ven<-MIN_VEN[1:34,1:6]
# #PARAGUAY
# nombre_PAR<-grep(pattern = 'PAR',x =archivos)
# 
# PRECIP_PAR<-as.data.frame(read_excel(archivos[nombre_PAR],sheet= 1,skip = 2,n_max = 12,range = cell_cols("A:F")))
# cod=as.numeric(PRECIP_PAR[4:14,1])
# longitud=as.numeric(PRECIP_PAR[4:14,2])
# latitud=as.numeric(PRECIP_PAR[4:14,3])
# below=as.numeric(PRECIP_PAR[4:14,4])
# normal=as.numeric(PRECIP_PAR[4:14,5])
# above=as.numeric(PRECIP_PAR[4:14,6])
# precip_par<-data.frame(codigo=cod,latitud,longitud,below,normal,above)
# 
# MAX_PAR<-as.data.frame(read_excel(archivos[nombre_PAR],sheet= 1,range = cell_cols("H:M")))
# cod=as.numeric(MAX_PAR[2:10,1])
# longitud=as.numeric(MAX_PAR[2:10,2])
# latitud=as.numeric(MAX_PAR[2:10,3])
# below=as.numeric(MAX_PAR[2:10,4])
# normal=as.numeric(MAX_PAR[2:10,5])
# above=as.numeric(MAX_PAR[2:10,6])
# max_par<-data.frame(codigo=cod,latitud,longitud,below,normal,above)
# 
# MIN_PAR<-as.data.frame(read_excel(archivos[nombre_PAR],sheet= 1,range = cell_cols("H:M")))
# cod=as.numeric(MIN_PAR[15:24,1])
# longitud=as.numeric(MIN_PAR[15:24,2])
# latitud=as.numeric(MIN_PAR[15:24,3])
# below=as.numeric(MIN_PAR[15:24,4])
# normal=as.numeric(MIN_PAR[15:24,5])
# above=as.numeric(MIN_PAR[15:24,6])
# min_par<-data.frame(codigo=cod,longitud,latitud,below,normal,above)
#CHILE
nombre_CHI<-grep(pattern = 'CHI',x =archivos)
PRECIP_CH<-xlsx::read.xlsx(archivos[nombre_CHI],sheetName= 'Precipitación')

precip_ch<-PRECIP_CH[3:52,3:8]
colnames(precip_ch)<-c('estacion','latitud','longitud','bajo','normal','sobre')
MAX_CH<-xlsx::read.xlsx(archivos[nombre_CHI],sheetName= 'Temperatura')
max_ch<-MAX_CH[4:22,3:8]
colnames(max_ch)<-c('estacion','latitud','longitud','bajo','normal','sobre')
min_ch<-MAX_CH[4:22,c(3,4,5,10,11,12)]
colnames(min_ch)<-c('estacion','latitud','longitud','bajo','normal','sobre')
#PERU
archivos_per<-grep(pattern = 'PER',x =archivos)
precip_per<-xlsx::read.xlsx(archivos[archivos_per],sheetName = 'PP',colIndex = c(1,9,10,11,12,13))
MAX_per<-xlsx::read.xlsx(archivos[archivos_per],sheetName = 'TMAX',colIndex = c(1,9,10,11,12,13))
MIN_per<-xlsx::read.xlsx(archivos[archivos_per],sheetName = 'TMIN',colIndex = c(1,9,10,11,12,13))
#nombres
nom_col<-c('Nombre de Estacion','Latitud','Longitud','Bajo','Medio','Alto','Codigo')
#precipitaciones ordenadas
precip_col<-data.frame(PRECIP_COL[,1],PRECIP_COL[1:5],codigo=rep(NA,dim(PRECIP_COL)[1]))
colnames(precip_col)<-c('nombre','latitud','longitud','bajo','medio','alto','codigo')

precip_bol<-data.frame(PRECIP_BOL,codigo=rep(NA,dim(PRECIP_BOL)[1]))
colnames(precip_bol)<-c('nombre','latitud','longitud','bajo','medio','alto','codigo')

precip_ecu<-data.frame(PRECIP_ECU,codigo=rep(NA,dim(PRECIP_ECU)[1]))
colnames(precip_ecu)<-c('nombre','latitud','longitud','bajo','medio','alto','codigo')

precip_ven<-data.frame(precip_ven,codigo=rep(NA,dim(precip_ven)[1]))
precip_ven<-precip_ven[,c(1,2,3,4,5,6,7)]
colnames(precip_ven)<-c('nombre','latitud','longitud','bajo','medio','alto','codigo')

#precip_par<-data.frame(precip_par,codigo=rep(NA,dim(precip_par)[1]))
#colnames(precip_par)<-c('nombre','latitud','longitud','bajo','medio','alto','codigo')

precip_ch<-data.frame(precip_ch[,c(1:6)],rep(NA,dim(precip_ch)[1]))
colnames(precip_ch)<-c('nombre','latitud','longitud','bajo','medio','alto','codigo')

precip_per<-data.frame(precip_per[,c(1:6)],rep(NA,dim(precip_per)[1]))
colnames(precip_per)<-c('nombre','latitud','longitud','bajo','medio','alto','codigo')

#total_data<-rbind(precip_col,precip_ecu,precip_ven,precip_ch)

total_data<-rbind(precip_col,precip_bol,precip_ecu,precip_ven,precip_ch,precip_per)
#total_data<-rbind(precip_col,precip_bol,precip_ecu,precip_ven,precip_par,precip_ch,precip_per)

total_data=categorizacion(tabla_datos = total_data)
xlsx::write.xlsx(x = total_data,col.names = T,row.names = F,file = paste0(directorio,'/Union/','terciles_',NOMBRE_DATE,'_prec.xlsx'))



#TEMPERATURA minima
min_col<-data.frame(MIN_COL[,1],MIN_COL[1:5],codigo=rep(NA,dim(MIN_COL)[1]))
colnames(min_col)<-c('nombre','latitud','longitud','bajo','medio','alto','codigo')

min_bol<-data.frame(MIN_BOL,codigo=rep(NA,dim(MIN_BOL)[1]))
colnames(min_bol)<-c('nombre','latitud','longitud','bajo','medio','alto','codigo')

min_ecu<-data.frame(MIN_ECU,rep(NA,dim(MIN_ECU)[1]))
colnames(min_ecu)<-c('nombre','latitud','longitud','bajo','medio','alto','codigo')

min_ven<-data.frame(min_ven[,c(1,3,2,4,5,6)],rep(NA,dim(min_ven)[1]))
colnames(min_ven)<-c('nombre','latitud','longitud','bajo','medio','alto','codigo')

# min_par<-data.frame(min_par[,c(1,3,2,4,5,6)],rep(NA,dim(min_par)[1]))
# colnames(min_par)<-c('nombre','latitud','longitud','bajo','medio','alto','codigo')

min_ch<-data.frame(min_ch[,c(1:6)],rep(NA,dim(min_ch)[1]))
colnames(min_ch)<-c('nombre','latitud','longitud','bajo','medio','alto','codigo')

min_per<-data.frame(MIN_per[,c(1:6)],rep(NA,dim(MIN_per)[1]))
colnames(min_per)<-c('nombre','latitud','longitud','bajo','medio','alto','codigo')

#total_data_min<-rbind(min_col,min_bol,min_ecu,min_ven,min_par,min_ch,min_per)
total_data_min<-rbind(min_col,min_bol,min_ecu,min_ven,min_ch,min_per)
#total_data_min<-rbind(min_col,min_ecu,min_ven,min_ch)

total_data_min=categorizacion(tabla_datos = total_data_min)

pos_na<-which(is.na(total_data_min[,1]))

pos_na<-which(is.na(total_data_min[,1]))
if(length(pos_na)==0){
  total_data_min=total_data_min
  xlsx::write.xlsx(x = total_data_min,col.names = T,row.names = F,file = paste0(directorio,'/Union/','terciles_',NOMBRE_DATE,'_tmin.xlsx'))
}else{
  total_data_min<-total_data_min[-pos_na,]  
  xlsx::write.xlsx(x = total_data_min,col.names = T,row.names = F,file = paste0(directorio,'/Union/','terciles_',NOMBRE_DATE,'_tmin.xlsx'))
}

#temperatura maxima

max_col<-data.frame(MAX_COL[,1],MAX_COL[,1:5],codigo=rep(NA,dim(MAX_COL)[1]))
colnames(max_col)<-c('nombre','latitud','longitud','bajo','medio','alto','codigo')

max_bol<-data.frame(MAX_BOL,codigo=rep(NA,dim(MAX_BOL)[1]))
colnames(max_bol)<-c('nombre','latitud','longitud','bajo','medio','alto','codigo')

max_ecu<-data.frame(MAX_ECU,rep(NA,dim(MAX_ECU)[1]))
colnames(max_ecu)<-c('nombre','latitud','longitud','bajo','medio','alto','codigo')

max_ven<-data.frame(max_ven[,c(1,3,2,4,5,6)],nombre=rep(NA,dim(max_ven)[1]))
colnames(max_ven)<-c('nombre','latitud','longitud','bajo','medio','alto','codigo')

#max_par<-data.frame(max_par,nombre=rep(NA,dim(max_par)[1]))
#colnames(max_par)<-c('nombre','latitud','longitud','bajo','medio','alto','codigo')

max_ch<-data.frame(max_ch[,c(1:6)],rep(NA,dim(max_ch)[1]))
colnames(max_ch)<-c('nombre','latitud','longitud','bajo','medio','alto','codigo')

max_per<-data.frame(MAX_per[,c(1:6)],nombre=rep(NA,dim(MAX_per)[1]))
colnames(max_per)<-c('nombre','latitud','longitud','bajo','medio','alto','codigo')

#total_data_max<-rbind(max_col,max_bol,max_ecu,max_ven,max_par,max_ch,max_per)
total_data_max<-rbind(max_col,max_bol,max_ecu,max_ven,max_ch,max_per)
#total_data_max<-rbind(max_col,max_ecu,max_ven,max_ch)
total_data_max=total_data_max[!is.na(total_data_max$latitud),]
total_data_max=categorizacion(tabla_datos = total_data_max)

pos_na<-which(is.na(total_data_max[,1]))
if(length(pos_na)==0){
  total_data_max=total_data_max#pronostico_DEF_2022
  xlsx::write.xlsx(x = total_data_max,col.names = T,row.names = F,file = paste0(directorio,'/Union/','terciles_',NOMBRE_DATE,'_tmax.xlsx'))
}else{
  total_data_max<-total_data_max[-pos_na,]  
  xlsx::write.xlsx(x = total_data_max,col.names = T,row.names = F,file = paste0(directorio,'/Union/','terciles_',NOMBRE_DATE,'_tmax.xlsx'))
}

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#graficacion library(ggplot2)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(sf)
library(dplyr)
library(magrittr)
clasificador<-function(tabla,parametro){
  #parametro=c(Lluvia,Tmáx,Tmin')
  pos_up<-which(tabla$codigo==3)
  pos_mid<-which(tabla$codigo==2)
  pos_down<-which(tabla$codigo==1)
  #pos_no_signal<-which(tabla$codigo==0)
  pos_NO_DATA<-which(is.na(tabla$codigo)==T)
  tabla$Leyenda=NA
  tabla$Leyenda[pos_up]<-paste(parametro,'sobre lo normal')
  tabla$Leyenda[pos_mid]<-paste(parametro,'cerca de lo normal')
  tabla$Leyenda[pos_down]<-paste(parametro,'bajo lo normal')
  #tabla$Leyenda[pos_no_signal]<-"Pronóstico Incierto"
  tabla$Leyenda[pos_NO_DATA]<-"Estación seca"
  tabla = tabla %>% na.omit()
  #print('Tabla clasificada!!!!!')
  return(tabla)
  
}

world <- ne_countries(scale = "medium", returnclass = "sf")
colnames(total_data)<-c('nombre','lat','long','bajo','medio','alto','codigo')
total_data<-clasificador(tabla = total_data,parametro = 'Lluvia')
parametro = 'Lluvia'
VAL_NAMES<-c(paste(parametro,'sobre lo normal'),paste(parametro,'cerca de lo normal'),paste(parametro,'bajo lo normal'))

colnames(total_data_max)<-c('nombre','lat','long','bajo','medio','alto','codigo')
total_data_max<-clasificador(tabla = total_data_max,parametro = 'Tmáx')
parametro = 'Tmáx'
VAL_NAMES_tmax<-c(paste(parametro,'sobre lo normal'),paste(parametro,'cerca de lo normal'),paste(parametro,'bajo lo normal'))

colnames(total_data_min)<-c('nombre','lat','long','bajo','medio','alto','codigo')
total_data_min<-clasificador(tabla = total_data_min,parametro = 'Tmin')
parametro = 'Tmin'
VAL_NAMES_tmin<-c(paste(parametro,'sobre lo normal'),paste(parametro,'cerca de lo normal'),paste(parametro,'bajo lo normal'))



tabla_precip=total_data
tabla_tmax=total_data_max
tabla_tmin=total_data_min


# Cargar las librerías necesarias
library(ggplot2)
library(sf)
library(raster)
library(marmap)
#install.packages('marmap',dependencies = T)
# Directorio del raster
DIR_RAST <- 'D:/CIIFEN/Boletines_Sudamerica/DEM Sudamerica'

# Cargar el raster
sud_rast <- raster::raster(paste0(DIR_RAST, '/Dem_Sudamerica.tif'))
DATA_WORLD<-world[,4]
#TEST<-world%>%select(sovereignt)
#str(world)
#which(TEST$sovereignt=='Venezuela')
# which(TEST$sovereignt=='Colombia')
# which(TEST$sovereignt=='Ecuador')
# which(TEST$sovereignt=='Peru')
# which(TEST$sovereignt=='Chile')
# which(TEST$sovereignt=='Paraguay')
# which(TEST$sovereignt=='Bolivia')
america_selection<-c(231,48,64,171,41,32)

# which(TEST$sovereignt=='Brazil')
# which(TEST$sovereignt=='Argentina')
# which(TEST$sovereignt=='Uruguay')
# which(TEST$sovereignt=='Guyana')
# which(TEST$sovereignt=='France')
# which(TEST$sovereignt=='Suriname')
#28#134
america_selection_2<-c(33,9,226,90,203,73,179)

# Crear un data frame para la capa raster
raster_df <- rasterToPoints(sud_rast)
raster_df <- as.data.frame(raster_df)
colnames(raster_df) <- c("long", "lat", "elevation")

#graficacion library(ggplot2)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(sf)
library(dplyr)
library(magrittr)
clasificador<-function(tabla,parametro){
  #parametro=c(Lluvia,Tmáx,Tmin')
  pos_up<-which(tabla$codigo==3)
  pos_mid<-which(tabla$codigo==2)
  pos_down<-which(tabla$codigo==1)
  #pos_no_signal<-which(tabla$codigo==0)
  pos_NO_DATA<-which(is.na(tabla$codigo)==T)
  tabla$Leyenda=NA
  tabla$Leyenda[pos_up]<-paste(parametro,'sobre lo normal')
  tabla$Leyenda[pos_mid]<-paste(parametro,'cerca de lo normal')
  tabla$Leyenda[pos_down]<-paste(parametro,'bajo lo normal')
  #tabla$Leyenda[pos_no_signal]<-"Pronóstico Incierto"
  tabla$Leyenda[pos_NO_DATA]<-"Estación seca"
  tabla = tabla %>% na.omit()
  #print('Tabla clasificada!!!!!')
  return(tabla)
  
}


sud_rast[sud_rast<0] <- 0

elevation_colors <- c('grey','black')
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#Mapa 1 Pronóstico de precipitación
#View(total_data)
DIRECTORIO_PNG<-paste0(directorio,'/Union/','PNG')
dir.create(DIRECTORIO_PNG)
png(filename = paste0(DIRECTORIO_PNG,'/','Pronostico_SUD_precip2.png'),width = 828,height = 1028,res = 110)
plot_map<-ggplot(data = DATA_WORLD) +
  geom_raster(data = raster_df, aes(x = long, y = lat, fill = elevation))+
  geom_sf(data = DATA_WORLD[america_selection,], fill = 'transparent', color = 'black') +
  scale_fill_gradientn(colors = elevation_colors,guide = "none") +  # Usar la paleta de colores definida
  annotation_scale(location = "tr", width_hint = 0.4) +
  ggspatial::annotation_north_arrow(location = "tr", which_north = "true",
                                    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
                                    style = ggspatial::north_arrow_nautical(fill = c("grey40", "white"),
                                                                            line_col = "grey20",
                                                                            text_family = "ArcherPro Book")) +
  coord_sf(xlim = c(-95, -40), ylim = c(-60, 15), expand = FALSE)+geom_point(data = total_data,aes(x=long,y=lat,color = Leyenda),size=1.7)+
  scale_color_manual(values =c("Lluvia sobre lo normal"='mediumblue',"Lluvia cerca de lo normal"='forestgreen',"Lluvia bajo lo normal"='darkgoldenrod2',"Estación seca"='hotpink3'))
#c("lluvia por sobre lo normal " = "blue", "lluvia cerca de lo normal  " = "green","lluvia bajo lo normal" = "yellow",)
plot_map+labs(y="",x="")+
  theme(legend.position=c(.74,.74),legend.background = element_rect(size=0.5, linetype="solid", alpha("grey", 0.7)),
        legend.key=element_rect(fill = alpha("white", 1)),legend.title=element_text(size=12,face="bold"),
        axis.title.x=element_text(colour="black",size=14,face="bold"),
        axis.title.y = element_text(colour="black",size=14,angle=90,face="bold"),
        legend.text= element_text(colour="black", size=10, face="bold.italic"),
        axis.text.x = element_text(colour="black",size=10,face="bold"),
        axis.text.y = element_text(colour="black",size=10,face="bold"))

dev.off()
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
DIRECTORIO_PNG<-paste0(directorio,'/Union/','PNG')
dir.create(DIRECTORIO_PNG)
total_data_max[,8]
png(filename = paste0(DIRECTORIO_PNG,'/','Pronostico_SUD_tmax2.png'),width = 828,height = 1028,res = 110)
plot_map<-ggplot(data = DATA_WORLD) +
  geom_raster(data = raster_df, aes(x = long, y = lat, fill = elevation))+
  geom_sf(data = DATA_WORLD[america_selection,], fill = 'transparent', color = 'black') +
  scale_fill_gradientn(colors = elevation_colors,guide = "none") +  # Usar la paleta de colores definida
  annotation_scale(location = "tr", width_hint = 0.4) +
  ggspatial::annotation_north_arrow(location = "tr", which_north = "true",
                                    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
                                    style = ggspatial::north_arrow_nautical(fill = c("grey40", "white"),
                                                                            line_col = "grey20",
                                                                            text_family = "ArcherPro Book"))+
  coord_sf(xlim = c(-95, -40), ylim = c(-60, 15), expand = FALSE)+geom_point(data = total_data_max,aes(x=long,y=lat,color = Leyenda),size=1.7)+
  scale_color_manual(values =c("Tmáx sobre lo normal"='brown1',"Tmáx cerca de lo normal"='green3',"Tmáx bajo lo normal"='blue2'))
#scale_color_manual(values =c("Tmáx sobre lo normal"='brown1',"Tmáx cerca de lo normal"='green3',"Tmáx bajo lo normal"='blue2'))
#c("lluvia por sobre lo normal " = "blue", "lluvia cerca de lo normal  " = "green","lluvia bajo lo normal" = "yellow",)
plot_map+labs(y="",x="")+
  theme(legend.position=c(.74,.74),legend.background = element_rect(size=0.5, linetype="solid", alpha("grey", 0.7)),
        legend.key=element_rect(fill = alpha("white", 1)),legend.title=element_text(size=12,face="bold"),
        axis.title.x=element_text(colour="black",size=14,face="bold"),
        axis.title.y = element_text(colour="black",size=14,angle=90,face="bold"),
        legend.text= element_text(colour="black", size=10, face="bold.italic"),
        axis.text.x = element_text(colour="black",size=10,face="bold"),
        axis.text.y = element_text(colour="black",size=10,face="bold"))

dev.off()

print("tmin se va a crear")

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
DIRECTORIO_PNG<-paste0(directorio,'/Union/','PNG')
dir.create(DIRECTORIO_PNG)
total_data_max[,8]
png(filename = paste0(DIRECTORIO_PNG,'/','Pronostico_SUD_tmin2.png'),width = 828,height = 1028,res = 110)
plot_map<-ggplot(data = DATA_WORLD) +
  geom_raster(data = raster_df, aes(x = long, y = lat, fill = elevation))+
  geom_sf(data = DATA_WORLD[america_selection,], fill = 'transparent', color = 'black') +
  scale_fill_gradientn(colors = elevation_colors,guide = "none") +  # Usar la paleta de colores definida
  annotation_scale(location = "tr", width_hint = 0.4) +
  ggspatial::annotation_north_arrow(location = "tr", which_north = "true",
                                    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
                                    style = ggspatial::north_arrow_nautical(fill = c("grey40", "white"),
                                                                            line_col = "grey20",
                                                                            text_family = "ArcherPro Book"))+
  coord_sf(xlim = c(-95, -40), ylim = c(-60, 15), expand = FALSE)+geom_point(data = total_data_min,aes(x=long,y=lat,color = Leyenda),size=1.7)+
  scale_color_manual(values =c("Tmin sobre lo normal"='brown1',"Tmin cerca de lo normal"='green3',"Tmin bajo lo normal"='blue2'))
#c("lluvia por sobre lo normal " = "blue", "lluvia cerca de lo normal  " = "green","lluvia bajo lo normal" = "yellow",)
plot_map+labs(y="",x="")+
  theme(legend.position=c(.74,.74),legend.background = element_rect(size=0.5, linetype="solid", alpha("grey", 0.7)),
        legend.key=element_rect(fill = alpha("white", 1)),legend.title=element_text(size=12,face="bold"),
        axis.title.x=element_text(colour="black",size=14,face="bold"),
        axis.title.y = element_text(colour="black",size=14,angle=90,face="bold"),
        legend.text= element_text(colour="black", size=10, face="bold.italic"),
        axis.text.x = element_text(colour="black",size=10,face="bold"),
        axis.text.y = element_text(colour="black",size=10,face="bold"))

dev.off()





#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#Mapa 2 Venezuela, Colombia, Ecuador Precipitacion

png(filename = paste0(DIRECTORIO_PNG,'/','Pronostico_VEN_COL_EC_PREC.png'),width = 1128,height = 928,res = 120)
plot_map<-ggplot(data = DATA_WORLD) +
  geom_raster(data = raster_df, aes(x = long, y = lat, fill = elevation))+
  geom_sf(data = DATA_WORLD[america_selection,], fill = 'transparent', color = 'black') +
  scale_fill_gradientn(colors = elevation_colors,guide = "none") +  # Usar la paleta de colores definida
  annotation_scale(location = "tr", width_hint = 0.4) +
  ggspatial::annotation_north_arrow(location = "tr", which_north = "true",
                                    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
                                    style = ggspatial::north_arrow_nautical(fill = c("grey40", "white"),
                                                                            line_col = "grey20",
                                                                            text_family = "ArcherPro Book"))+
  coord_sf(xlim = c(-95, -60), ylim = c(-6, 15), expand = FALSE)+geom_point(data = tabla_precip,aes(x=long,y=lat,color = Leyenda),size=2.5)+
  scale_color_manual(values =c("Lluvia sobre lo normal"='mediumblue',"Lluvia cerca de lo normal"='forestgreen',"Lluvia bajo lo normal"='darkgoldenrod2',"Estación seca"='hotpink3'))
#c("lluvia por sobre lo normal " = "blue", "lluvia cerca de lo normal  " = "green","lluvia bajo lo normal" = "yellow",)
plot_map+labs(y="",x="")+
  theme(legend.position=c(.2,.7),legend.background = element_rect(size=0.5, linetype="solid", alpha("grey", 0.7)),
        legend.key=element_rect(fill = alpha("white", 1)),legend.title=element_text(size=12,face="bold"),
        axis.title.x=element_text(colour="black",size=14,face="bold"),
        axis.title.y = element_text(colour="black",size=14,angle=90,face="bold"),
        legend.text= element_text(colour="black", size=10, face="bold.italic"),
        axis.text.x = element_text(colour="black",size=10,face="bold"),
        axis.text.y = element_text(colour="black",size=10,face="bold"))

dev.off()


#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#Mapa 3 Perú Bolivia CHILE Precipitacion

png(filename = paste0(DIRECTORIO_PNG,'/','Pronostico_PER_BOL_CH_PREC.png'),width = 1128,height = 928,res = 120)
plot_map<-ggplot(data = DATA_WORLD) +
  geom_raster(data = raster_df, aes(x = long, y = lat, fill = elevation))+
  geom_sf(data = DATA_WORLD[america_selection,], fill = 'transparent', color = 'black') +
  scale_fill_gradientn(colors = elevation_colors,guide = "none") +  # Usar la paleta de colores definida
  annotation_scale(location = "tr", width_hint = 0.4) +
  ggspatial::annotation_north_arrow(location = "tr", which_north = "true",
                                    pad_x = unit(1, "in"), pad_y = unit(0.4, "in"),
                                    style = ggspatial::north_arrow_nautical(fill = c("grey40", "white"),
                                                                            line_col = "grey20",
                                                                            text_family = "ArcherPro Book"))+
  coord_sf(xlim = c(-85, -53), ylim = c(-30, 2), expand = FALSE)+geom_point(data = tabla_precip,aes(x=long,y=lat,color = Leyenda),size=2.5)+
  scale_color_manual(values =c("Lluvia sobre lo normal"='mediumblue',"Lluvia cerca de lo normal"='forestgreen',"Lluvia bajo lo normal"='darkgoldenrod2',"Estación seca"='hotpink3'))
#c("lluvia por sobre lo normal " = "blue", "lluvia cerca de lo normal  " = "green","lluvia bajo lo normal" = "yellow",)
plot_map+labs(y="",x="")+
  theme(legend.position=c(.2,.2),legend.background = element_rect(size=0.5, linetype="solid", alpha("grey", 0.7)),
        legend.key=element_rect(fill = alpha("white", 1)),legend.title=element_text(size=12,face="bold"),
        axis.title.x=element_text(colour="black",size=14,face="bold"),
        axis.title.y = element_text(colour="black",size=14,angle=90,face="bold"),
        legend.text= element_text(colour="black", size=10, face="bold.italic"),
        axis.text.x = element_text(colour="black",size=10,face="bold"),
        axis.text.y = element_text(colour="black",size=10,face="bold"))

dev.off()

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#Mapa 4 CHILE Precipitacion

png(filename = paste0(DIRECTORIO_PNG,'/','Pronostico_CH_PREC.png'),width = 700,height = 1124,res = 120)
plot_map<-ggplot(data = DATA_WORLD) +
  geom_raster(data = raster_df, aes(x = long, y = lat, fill = elevation))+
  geom_sf(data = DATA_WORLD[america_selection,], fill = 'transparent', color = 'black') +
  scale_fill_gradientn(colors = elevation_colors,guide = "none") +  # Usar la paleta de colores definida
  annotation_scale(location = "bl", width_hint = 0.4) +
  ggspatial::annotation_north_arrow(location = "tl", which_north = "true",
                                    pad_x = unit(0.35, "in"), pad_y = unit(0.4, "in"),
                                    style = ggspatial::north_arrow_nautical(fill = c("grey40", "white"),
                                                                            line_col = "grey20",
                                                                            text_family = "ArcherPro Book"))+
  coord_sf(xlim = c(-78, -65), ylim = c(-60, -15), expand = FALSE)+geom_point(data = tabla_precip,aes(x=long,y=lat,color = Leyenda),size=2.5)+
  scale_color_manual(values =c("Lluvia sobre lo normal"='mediumblue',"Lluvia cerca de lo normal"='forestgreen',"Lluvia bajo lo normal"='darkgoldenrod2',"Estación seca"='hotpink3'))+
  scale_x_continuous(breaks = seq(-75, -65, 4))
#c("lluvia por sobre lo normal " = "blue", "lluvia cerca de lo normal  " = "green","lluvia bajo lo normal" = "yellow",)
plot_map+labs(y="",x="")+
  theme(legend.position=c(.8,.75),legend.background = element_rect(size=0.5, linetype="solid", alpha("grey", 0.7)),
        legend.key=element_rect(fill = alpha("white", 1)),legend.title=element_text(size=12,face="bold"),
        axis.title.x=element_text(colour="black",size=14,face="bold"),
        axis.title.y = element_text(colour="black",size=14,angle=90,face="bold"),
        legend.text= element_text(colour="black", size=10, face="bold.italic"),
        axis.text.x = element_text(colour="black",size=10,face="bold"),
        axis.text.y = element_text(colour="black",size=10,face="bold"))+ guides(fill=FALSE, color=FALSE)

dev.off()

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#Mapa 5 Venezuela, Colombia, Ecuador , Bolivia, Perú Temperatura Máxima

# png(filename = paste0(DIRECTORIO_PNG,'/','Pronostico_VEN_COL_EC_BOL_PER_temp_max2.png'),width = 1028,height = 1028,res = 130)
# plot_map<-ggplot(data = DATA_WORLD) +
#   geom_raster(data = raster_df, aes(x = long, y = lat, fill = elevation))+
#   geom_sf(data = DATA_WORLD[america_selection,], fill = 'transparent', color = 'black') +
#   scale_fill_gradientn(colors = elevation_colors,guide = "none") +  # Usar la paleta de colores definida
#   annotation_scale(location = "tr", width_hint = 0.4) +
#   ggspatial::annotation_north_arrow(location = "tr", which_north = "true",
#                                     pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
#                                     style = ggspatial::north_arrow_nautical(fill = c("grey40", "white"),
#                                                                             line_col = "grey20",
#                                                                             text_family = "ArcherPro Book"))+
#   coord_sf(xlim = c(-95, -50), ylim = c(-30, 15), expand = FALSE)+geom_point(data = tabla_tmax,aes(x=long,y=lat,color = Leyenda),size=2.5)+
#   scale_color_manual(values =c("Tmáx sobre lo normal"='brown1',"Tmáx cerca de lo normal"='green3',"Tmáx bajo lo normal"='blue2'))
# #c("lluvia por sobre lo normal " = "blue", "lluvia cerca de lo normal  " = "green","lluvia bajo lo normal" = "yellow",)
# plot_map+labs(y="",x="")+
#   theme(legend.position=c(.76,.53),legend.background = element_rect(size=0.5, linetype="solid", alpha("grey", 0.7)),
#         legend.key=element_rect(fill = alpha("white", 1)),legend.title=element_text(size=12,face="bold"),
#         axis.title.x=element_text(colour="black",size=14,face="bold"),
#         axis.title.y = element_text(colour="black",size=14,angle=90,face="bold"),
#         legend.text= element_text(colour="black", size=10, face="bold.italic"),
#         axis.text.x = element_text(colour="black",size=10,face="bold"),
#         axis.text.y = element_text(colour="black",size=10,face="bold"))
# 
# dev.off()

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#Mapa 6 CHILE TEMPERAURA MÁXIMA

# png(filename = paste0(DIRECTORIO_PNG,'/','Pronostico_CH_temp_max2.png'),width = 900,height = 1124,res = 120)
# plot_map<-ggplot(data = DATA_WORLD) +
#   geom_raster(data = raster_df, aes(x = long, y = lat, fill = elevation))+
#   geom_sf(data = DATA_WORLD[america_selection,], fill = 'transparent', color = 'black') +
#   scale_fill_gradientn(colors = elevation_colors,guide = "none") +  # Usar la paleta de colores definida
#   annotation_scale(location = "bl", width_hint = 0.4) +
#   ggspatial::annotation_north_arrow(location = "tl", which_north = "true",
#                                     pad_x = unit(0.35, "in"), pad_y = unit(0.4, "in"),
#                                     style = ggspatial::north_arrow_nautical(fill = c("grey40", "white"),
#                                                                             line_col = "grey20",
#                                                                             text_family = "ArcherPro Book"))+
#   coord_sf(xlim = c(-78, -65), ylim = c(-60, -15), expand = FALSE)+geom_point(data = tabla_tmax,aes(x=long,y=lat,color = Leyenda),size=2.5)+
#   scale_color_manual(values =c("Tmáx sobre lo normal"='brown1',"Tmáx cerca de lo normal"='green3',"Tmáx bajo lo normal"='blue2'))+
#   scale_x_continuous(breaks = seq(-75, -65, 4))
# #c("lluvia por sobre lo normal " = "blue", "lluvia cerca de lo normal  " = "green","lluvia bajo lo normal" = "yellow",)
# plot_map+labs(y="",x="")+
#   theme(legend.position=c(.8,.75),legend.background = element_rect(size=0.5, linetype="solid", alpha("grey", 0.7)),
#         legend.key=element_rect(fill = alpha("white", 1)),legend.title=element_text(size=12,face="bold"),
#         axis.title.x=element_text(colour="black",size=14,face="bold"),
#         axis.title.y = element_text(colour="black",size=14,angle=90,face="bold"),
#         legend.text= element_text(colour="black", size=10, face="bold.italic"),
#         axis.text.x = element_text(colour="black",size=10,face="bold"),
#         axis.text.y = element_text(colour="black",size=10,face="bold"))+ guides(fill=FALSE, color=FALSE)
# 
# dev.off()

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#Mapa 7 Venezuela, Colombia, Ecuador , Bolivia, Perú Temperatura Mínima
# 
# png(filename = paste0(DIRECTORIO_PNG,'/','Pronostico_VEN_COL_EC_BOL_PER_temp_min2.png'),width = 1228,height = 1028,res = 130)
# plot_map<-ggplot(data = DATA_WORLD) +
#   geom_raster(data = raster_df, aes(x = long, y = lat, fill = elevation))+
#   geom_sf(data = DATA_WORLD[america_selection,], fill = 'transparent', color = 'black') +
#   scale_fill_gradientn(colors = elevation_colors,guide = "none") +  # Usar la paleta de colores definida
#   annotation_scale(location = "tr", width_hint = 0.4) +
#   ggspatial::annotation_north_arrow(location = "tr", which_north = "true",
#                                     pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
#                                     style = ggspatial::north_arrow_nautical(fill = c("grey40", "white"),
#                                                                             line_col = "grey20",
#                                                                             text_family = "ArcherPro Book"))+
#   coord_sf(xlim = c(-95, -50), ylim = c(-30, 15), expand = FALSE)+geom_point(data = tabla_tmin,aes(x=long,y=lat,color = Leyenda),size=2.5)+
#   scale_color_manual(values =c("Tmin sobre lo normal"='brown1',"Tmin cerca de lo normal"='green3',"Tmin bajo lo normal"='blue2'))
# #c("lluvia por sobre lo normal " = "blue", "lluvia cerca de lo normal  " = "green","lluvia bajo lo normal" = "yellow",)
# plot_map+labs(y="",x="")+
#   theme(legend.position=c(.76,.53),legend.background = element_rect(size=0.5, linetype="solid", alpha("grey", 0.7)),
#         legend.key=element_rect(fill = alpha("white", 1)),legend.title=element_text(size=12,face="bold"),
#         axis.title.x=element_text(colour="black",size=14,face="bold"),
#         axis.title.y = element_text(colour="black",size=14,angle=90,face="bold"),
#         legend.text= element_text(colour="black", size=10, face="bold.italic"),
#         axis.text.x = element_text(colour="black",size=10,face="bold"),
#         axis.text.y = element_text(colour="black",size=10,face="bold"))
# 
# dev.off()

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#Mapa 7 CHILE TEMPERAURA Mínima

# png(filename = paste0(DIRECTORIO_PNG,'/','Pronostico_CH_temp_min2.png'),width = 900,height = 1124,res = 120)
# plot_map<-ggplot(data = DATA_WORLD) +
#   geom_raster(data = raster_df, aes(x = long, y = lat, fill = elevation))+
#   geom_sf(data = DATA_WORLD[america_selection,], fill = 'transparent', color = 'black') +
#   scale_fill_gradientn(colors = elevation_colors,guide = "none") +  # Usar la paleta de colores definida
#   annotation_scale(location = "bl", width_hint = 0.4) +
#   ggspatial::annotation_north_arrow(location = "tl", which_north = "true",
#                                     pad_x = unit(0.35, "in"), pad_y = unit(0.4, "in"),
#                                     style = ggspatial::north_arrow_nautical(fill = c("grey40", "white"),
#                                                                             line_col = "grey20",
#                                                                             text_family = "ArcherPro Book"))+
#   coord_sf(xlim = c(-78, -65), ylim = c(-60, -15), expand = FALSE)+geom_point(data = tabla_tmin,aes(x=long,y=lat,color = Leyenda),size=2.5)+
#   scale_color_manual(values =c("Tmin sobre lo normal"='brown1',"Tmin cerca de lo normal"='green3',"Tmin bajo lo normal"='blue2'))+
#   scale_x_continuous(breaks = seq(-75, -65, 4))
# #c("lluvia por sobre lo normal " = "blue", "lluvia cerca de lo normal  " = "green","lluvia bajo lo normal" = "yellow",)
# plot_map+labs(y="",x="")+
#   theme(legend.position=c(.8,.75),legend.background = element_rect(size=0.5, linetype="solid", alpha("grey", 0.7)),
#         legend.key=element_rect(fill = alpha("white", 1)),legend.title=element_text(size=12,face="bold"),
#         axis.title.x=element_text(colour="black",size=14,face="bold"),
#         axis.title.y = element_text(colour="black",size=14,angle=90,face="bold"),
#         legend.text= element_text(colour="black", size=10, face="bold.italic"),
#         axis.text.x = element_text(colour="black",size=10,face="bold"),
#         axis.text.y = element_text(colour="black",size=10,face="bold"))+ guides(fill=FALSE, color=FALSE)
# 
# dev.off()
#### editar diapositiva

