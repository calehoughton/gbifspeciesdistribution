install.packages("geodata",dependencies=TRUE,repos="https://cloud.r-project.org")
install.packages("predicts",dependencies=TRUE,repos="https://cloud.r-project.org")
install.packages("terra",dependencies=TRUE,repos="https://cloud.r-project.org")
library(geodata)
library(predicts)
library(terra)

occdata <- geodata::sp_occurrence("Triturus", "cristatus", geo=FALSE,removeZeros=TRUE,start=1,end=10000)
occdata[1:10,]


wrld <- world(path=".")
plot(wrld, xlim=c(-180,180), ylim=c(-80,80), col="light yellow", border="light gray")
# add the points
points(occdata$lon, occdata$lat, col='blue', pch=20)
plot(occdata$lon, occdata$lat, col='blue', pch=20)


occdata<-subset(occdata,lat>0)
dups <- duplicated(occdata[, c('lon', 'lat')])
sum(dups)
occ <- occdata[!dups, ]

output_dir<-"~/Downloads/"
bio_glob<-worldclim_global(var="bio", res=10,path=output_dir, version="2.1")
dim(bio_glob)

summary(occ$lon)
summary(occ$lat)
e <- ext(-179, 174, 33, 80)
predictors <- crop(bio_glob, e)
names(predictors)<-substring(names(predictors),11,16)
plot(predictors,1:9)

plot(predictors,1)
points(occ$lon,occ$lat, col='blue',pch=16)


bg<-spatSample(predictors,5000,"random", na.rm=TRUE, as.points=TRUE,ext=e)
plot(predictors, 1)
points(bg, cex=0.1)



occlatlon<-cbind(occ$lon,occ$lat)
presvals <- extract(predictors, occlatlon)
#presvals is the climate data for where the species is present
backvals <- values(bg)
#backvals is the climate data for the background data
pb <- c(rep(1, nrow(presvals)), rep(0, nrow(backvals)))
#The first column of the dataset is a vector of 1s for presences and 0s for background data.
sdmdata <- data.frame(cbind(pb, rbind(presvals, backvals)))

pairs(sdmdata[,2:5], cex=0.1)

model<-MaxEnt(sdmdata[,-1],sdmdata[,1],removeDuplicates=TRUE)

plot(model)


predictedocc <- predict(model, predictors, args=c("outputformat=raw")) 
par(mfrow=c(2,1))
plot(predictedocc)
plot(predictedocc)
points(occlatlon,pch=".")




plot(predictors,2)

plot(fut_predictors,2)

names(fut_predictors)<-names(predictors)


