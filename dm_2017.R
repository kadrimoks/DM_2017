#install.packages("maps")
#install.packages("mapdata")
#install.packages("ggmap")
#install.packages("ggplot2")
#install.packages("rgeos")
#install.packages("gplots")
library(gplots) 
library(rgeos)
library(tidyverse)
library(stringr)
library(janitor)
library(rvest)
library(sf)
library(sp)
library(maps)       
library(mapdata)
library(ggmap)
library(ggplot2)
library(plyr)
library(readxl)
library(RColorBrewer)
library(data.table)

#data1=read.table("C:/Users/kadrmo/Desktop/data mining/project/maj.df.txt")
#head(data)


#from http://www.gadm.org/country
gadm <- readRDS("EST_adm1.rds")
gadm$maakond[1] <- "Harju maakond"
gadm$maakond[2] <-"Hiiu maakond" 
gadm$maakond[3] <- "Ida-Viru maakond"
gadm$maakond[5] <- "Jõgeva maakond"
gadm$maakond[4] <- "Järva maakond"
gadm$maakond[7] <- "Lääne maakond"
gadm$maakond[6] <- "Lääne-Viru maakond"
gadm$maakond[9] <- "Põlva maakond"
gadm$maakond[8] <- "Pärnu maakond"
gadm$maakond[11] <- "Rapla maakond"
gadm$maakond[12] <- "Saare maakond"
gadm$maakond[13] <- "Tartu maakond"
gadm$maakond[15] <- "Valga maakond"
gadm$maakond[16] <- "Viljandi maakond"
gadm$maakond[14] <- "Võru maakond"



#miskipärast on peipsi maakonnaks tehtud
gadm=gadm[-c(10), ]



#t1<-read_excel("C:/Users/kadrmo/Desktop/data mining/project/inf.xlsx")
#t2<-read_excel("C:/Users/kadrmo/Desktop/data mining/project/aadr.xlsx")
#map.df<-merge(x = t1, y =  t2,  by = "Registrikood" , all.x = T)
map.df=read.table("map.df.txt")

map.df2<-data.table(map.df)


df2015sumkasum<-map.df2[map.df2$Aruandlusaasta == 2015, sum(Aruandlusaastakasum, na.rm = T), by = puhas_mk]

df2015sumkaive<-map.df2[map.df2$Aruandlusaasta == 2015, sum(Aruandlusaastakäive, na.rm = T), by = puhas_mk]
df2015sumtootajad<-map.df2[map.df2$Aruandlusaasta == 2015, sum(Aruandlusaasta.töötajate.arv, na.rm = T), by = puhas_mk]

df2015<-df2015sumkasum
df2015$normkasum<-df2015sumkasum$V1/df2015sumkaive$V1*100
df2015$normkasumTootajad<-df2015$normkasum/df2015sumtootajad$V1*10000

df2015<-df2015[!is.na(df2015$puhas_mk),]
df2015<-df2015[order(df2015$puhas_mk),]




gadm <- gadm[order(gadm$maakond),]
gadm$normkas15=round(df2015$normkasum,1)
gadm$normkas215=round(df2015$normkasumTootajad,1)
gadm$kasum15=round(as.numeric(df2015$V1))




#gadmile saab maakondade järgi sidudes andmetega tulpasid jurde teha.
#seejärel saab panna kaadrile punkte ja maakondi vastavalt värvida
#plot(gadm)

###_________PLOT 1_______________

legend0= seq(-3,10, by = 1)


#levels are chosen with values that are not represented in purpoce: to give more distinct colors 
levels0= seq(-3,10, by = 1)
colorlevels1 = heat.colors(length(levels0))

round(df2015$normkasum)+3
colorlevels1[round(df2015$normkasum)+3]

normkas0=levels0+5
cols0=colorlevels1[round(df2015$normkasum)+3]
legend0=legend0[c(normkas0)]

p0cols=unique(colorlevels1)
p0txt=unique(levels0)
textColours <- gray(0)

p=plot(gadm, col = cols0, border = 'grey',main="2015 profit margin")+
  text(coordinates(gadm),
       col = textColours, labels=gadm$maakond, cex= 0.8, pos=3)+
  text(coordinates(gadm),
       col = textColours, labels=gadm$normkas15, cex= 0.8, pos=2)
legend('bottomleft', legend=p0txt,col=p0cols,fill=p0cols, bty="n",cex=0.7, 
       inset = 0.10,ncol=3)




###_________PLOT 2_______________

legend0= seq(-2,47, by = 5)


 
levels0= seq(-2,47, by = 5)
colorlevels1 = heat.colors(length(levels0))



normkas0=levels0+2
cols0=colorlevels1[round(df2015$normkasumTootajad/5)+1]
legend0=legend0[c(normkas0)]

p0cols=unique(colorlevels1)
p0txt=unique(levels0)
textColours <- gray(0)

p=plot(gadm, col = cols0, border = 'grey',main="2015 profit margin per employee")+
  text(coordinates(gadm),
       col = textColours, labels=gadm$maakond, cex= 0.8, pos=3)+
  text(coordinates(gadm),
       col = textColours, labels=gadm$normkas215, cex= 0.8, pos=2)
legend('bottomleft', legend=p0txt,col=p0cols,fill=p0cols, bty="n",cex=0.7, 
       inset = 0.10,ncol=3)


###___________PLOT3___________

legend0= c(-40,0,40,80,120,250, 500, 750, 1000,1500,2000,2500,3000)



levels0= legend0
colorlevels1 = heat.colors(length(levels0))


round(df2015$V1/1000000)

normkas0=levels0+2
cols0=colorlevels1[c(12,2,1,2,2,4,2,2,4,2,2,7,2,3,2)]
legend0=legend0[c(normkas0)]

p0cols=unique(colorlevels1)
p0txt=unique(levels0)
textColours <- gray(0)

p=plot(gadm, col = cols0, border = 'grey',main="2015 total profit")+
  text(coordinates(gadm),
       col = textColours, labels=gadm$maakond, cex= 0.8, pos=3)+
  text(coordinates(gadm),
       col = textColours, labels=paste(round(gadm$kasum15/1000000),"M ???"), cex= 0.8, pos=2)
legend('bottomleft', legend=p0txt,col=p0cols,fill=p0cols, bty="n",cex=0.7, 
       inset = 0.10,ncol=3)









