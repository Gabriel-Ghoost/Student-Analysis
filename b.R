library(car)
library(foreign)

setwd("C:/Users/GHOOST/Desktop/Work/Dami/R")

data<-read.spss("C:/Users/GHOOST/Desktop/Work/Dami/R/r7_merged_data_34ctry.release.sav", 
                to.data.frame=T, use.value.labels = T)

names(data)

naija<-data[which(data$COUNTRY=="Nigeria"),]


attach(naija)

table(Q44A)
table(as.numeric(Q44A))

table(Q44B)

table(Q44C)

table(Q44D)

naija$polcor <- ((as.numeric(Q44A))+(as.numeric(Q44B))+(as.numeric(Q44C))+(as.numeric(Q44D)))/4

table(polcor)

naija$polcorruption <- recode(as.numeric(polcor), "2:2.25=1; 2.5:3.25=2; 3.5:4.25=3; 4.5:5.25=4; 5.5:7=0")

table(polcorruption)

table(Q56G)
table(as.numeric(Q56G))

table(Q56H)
table(as.numeric(Q56H))

table(Q56I)
table(as.numeric(Q56I))

table(Q56L)
table(as.numeric(Q56L))

table(Q56M)
table(as.numeric(Q56M))

cor.test()
table(Q101)

a<-data.frame(as.numeric(Q56G), as.numeric(Q56H),as.numeric(Q56I), as.numeric(Q56L), as.numeric(Q56M) )

cor(a)
cor.test(as.numeric(Q56G), as.numeric(Q56H))

model0<-lm(as.numeric(polcorruption)~1, data = naija)
summary(model0)

model3<-lm(as.numeric(polcor)~as.numeric(Q56G)+as.numeric(Q56H)
           +as.numeric(Q56I)+as.numeric(Q56L)+as.numeric(Q56M), data = naija)
summary(model3)


