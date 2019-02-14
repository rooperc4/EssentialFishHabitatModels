library(mgcv)
library(dismo)
library(rJava)
library(PresenceAbsence)
library(raster)
library(rgdal)
library(sp)
library(maptools)
library(rgeos)
library(maps)
library(xtable)
library(gridGraphics)
library(grid)
library(gridExtra)



#GOAData<-read.csv("D://EFH Descriptions 2015/Variables/Variables_raw_data/GOA_Trawldata.csv",header=TRUE,stringsAsFactors=FALSE)
#data1<-read.csv("D://Data1.csv",header=TRUE)
#GOAData<-merge(GOAData,data1,by.x="hauljoin",by.y="hauljoin",all.x=TRUE)
#indexes = sample(1:nrow(GOAData), size=0.2*nrow(GOAData))
#test.data = GOAData[indexes,]
#training.data = GOAData[-indexes,]

#write.csv(test.data,"D://EFH Descriptions 2015/Variables/Variables_raw_data/GOA_Trawl_testdata.csv", row.names=FALSE)
#write.csv(training.data,"D://EFH Descriptions 2015/Variables/Variables_raw_data/GOA_Trawl_trainingdata.csv", row.names=FALSE)

training.data<-read.csv("D://EFH Descriptions 2015/Variables/Variables_raw_data/GOA_Trawl_trainingdata.csv",header=TRUE,stringsAsFactors=FALSE)
for(i in 1:length(training.data$btemp)){if(is.na(training.data[i,"btemp"])==TRUE){training.data[i,"btemp"]<-training.data[i,"rtemp"]}}
training.data<-subset(training.data,training.data$slope>=0)
training.data$canrf<-training.data$a_canrf+training.data$j_canrf
training.data$qurf<-training.data$a_qurf+training.data$j_qurf
training.data$blackrf<-training.data$a_blackrf+training.data$j_blackrf
training.data$dbrf<-training.data$a_dbrf+training.data$jdbrf
training.data$darkrf<-training.data$a_darkrf+training.data$j_darkrf
training.data$atka<-training.data$a_atka+training.data$j_atka
training.data$gstrf<-training.data$a_gstrf+training.data$j_gstrf


test.data<-read.csv("D://EFH Descriptions 2015/Variables/Variables_raw_data/GOA_Trawl_testdata.csv",header=TRUE,stringsAsFactors=FALSE)
for(i in 1:length(test.data$btemp)){if(is.na(test.data[i,"btemp"])==TRUE){test.data[i,"btemp"]<-test.data[i,"rtemp"]}}
test.data<-subset(test.data,test.data$slope>=0)
test.data<-subset(test.data,test.data$slope>=0)
test.data$canrf<-test.data$a_canrf+test.data$j_canrf
test.data$qurf<-test.data$a_qurf+test.data$j_qurf
test.data$blackrf<-test.data$a_blackrf+test.data$j_blackrf
test.data$dbrf<-test.data$a_dbrf+test.data$jdbrf
test.data$darkrf<-test.data$a_darkrf+test.data$j_darkrf
test.data$atka<-test.data$a_atka+test.data$j_atka
test.data$gstrf<-test.data$a_gstrf+test.data$j_gstrf

###MAKE THE 1 KM BASE RASTERS#####
ak.coast<-readShapeSpatial("D://EFH Descriptions 2015/Variables/Shapefiles/namerica_dcw")
#bathy<-raster("D://EFH Descriptions 2015/Variables/Variables_GOA_1ha/bathy_all")
#bathy<-aggregate(bathy, fact=10, fun=mean, expand=TRUE, na.rm=TRUE, filename="D://EFH Descriptions 2015/Variables/Variables_GOA_1km/Bathy")
bathy<-raster("D://EFH Descriptions 2015/Variables/Variables_GOA_1km/Bathy")
#slope<-raster("D://EFH Descriptions 2015/Variables/Variables_GOA_1ha/slope")
#slope<-aggregate(slope, fact=10, fun=mean, expand=TRUE, na.rm=TRUE, filename="D://EFH Descriptions 2015/Variables/Variables_GOA_1km/Slope")
slope<-raster("D://EFH Descriptions 2015/Variables/Variables_GOA_1km/Slope")
#color<-raster("D://EFH Descriptions 2015/Variables/Variables_GOA_1ha/color")
#color<-aggregate(color, fact=10, fun=mean, expand=TRUE, na.rm=TRUE, filename="D://EFH Descriptions 2015/Variables/Variables_GOA_1km/Color")
color<-raster("D://EFH Descriptions 2015/Variables/Variables_GOA_1km/Color")
#tmax<-raster("D://EFH Descriptions 2015/Variables/Variables_GOA_1ha/tmax")
#tmax<-aggregate(color, fact=10, fun=mean, expand=TRUE, na.rm=TRUE, filename="D://EFH Descriptions 2015/Variables/Variables_GOA_1km/Tmax")
tmax<-raster("D://EFH Descriptions 2015/Variables/Variables_GOA_1km/Tmax")
#bcurrent<-raster("D://EFH Descriptions 2015/Variables/Variables_GOA_1ha/speed")
#bcurrent<-aggregate(bcurrent, fact=10, fun=mean, expand=TRUE, na.rm=TRUE, filename="D://EFH Descriptions 2015/Variables/Variables_GOA_1km/Bcurrent")
bcurrent<-raster("D://EFH Descriptions 2015/Variables/Variables_GOA_1km/Bcurrent")
#btemp<-raster("D://EFH Descriptions 2015/Variables/Variables_GOA_1ha/btemp")
#btemp<-aggregate(btemp, fact=10, fun=mean, expand=TRUE, na.rm=TRUE,overwrite=TRUE,filename="D://EFH Descriptions 2015/Variables/Variables_GOA_1km/Btemp")
btemp<-raster("D://EFH Descriptions 2015/Variables/Variables_GOA_1km/Btemp")
lat<- init(bathy, v='y')
lat<-mask(lat,bathy,filename="D://EFH Descriptions 2015/Variables/Variables_GOA_1km/Lat",overwrite=TRUE)
lon<- init(bathy, v='x')
lon<-mask(lon,bathy,filename="D://EFH Descriptions 2015/Variables/Variables_GOA_1km/Lon",overwrite=TRUE)
#coral<-raster("D://EFH Descriptions 2015/Variables/Variables_GOA_1ha/coralfactor_all")
#coral<-aggregate(coral, fact=10, fun=max, expand=TRUE, na.rm=TRUE,overwrite=TRUE,filename="D://EFH Descriptions 2015/Variables/Variables_GOA_1km/Coralfactor")
coral<-raster("D://EFH Descriptions 2015/Variables/Variables_GOA_1km/Coralfactor")
#sponge<-raster("D://EFH Descriptions 2015/Variables/Variables_GOA_1ha/spongefactor_all")
#sponge<-aggregate(sponge, fact=10, fun=max, expand=TRUE, na.rm=TRUE,overwrite=TRUE,filename="D://EFH Descriptions 2015/Variables/Variables_GOA_1km/Spongefactor")
sponge<-raster("D://EFH Descriptions 2015/Variables/Variables_GOA_1km/Spongefactor")
#whips<-raster("D://EFH Descriptions 2015/Variables/Variables_GOA_1ha/whipsfactor_all")
#whips<-aggregate(whips, fact=10, fun=max, expand=TRUE, na.rm=TRUE,overwrite=TRUE,filename="D://EFH Descriptions 2015/Variables/Variables_GOA_1km/Whipsfactor")
whips<-raster("D://EFH Descriptions 2015/Variables/Variables_GOA_1km/Whipsfactor")
GOAstrata<-readShapeSpatial("D://EFH Descriptions 2015/Variables/Shapefiles/GOAstrata")



raster.stack<-stack(lon,lat,bathy,slope,btemp,color,bcurrent,tmax, coral, sponge, whips)
names(raster.stack)<-c("lon","lat","bdepth","slope","btemp","color","speed", "tmax", "sponge", "coral", "pen")
raster.stack<-mask(raster.stack,GOAstrata)

ModelPlan<-read.csv("D://EFH Descriptions 2015/ModelPlan.csv",header=TRUE, stringsAsFactors=FALSE)
ModelPlan<-subset(ModelPlan,ModelPlan$Region=="GOA")
species<-c(ModelPlan$Trawl_name_juvenile,ModelPlan$Trawl_name_adult)
models<-c(ModelPlan$Trawl_model_juvenile,ModelPlan$Trawl_model_adult)
start_year<-rep(ModelPlan$Start_year,2)
species_name<-c(paste("Juvenile_",ModelPlan$Species,sep=""),paste("Adult_",ModelPlan$Species,sep=""))
species_name<-species_name[species!=""]
models<-models[species!=""]
start_year<-start_year[species!=""]
species<-species[species!=""]

#j<-1
#k<-1

species_pct<-species
for(j in 1:length(species)){
if(start_year[j]>1990){
training.dat<-subset(training.data,training.data$year>=start_year[j])
test.dat<-subset(test.data,test.data$year>=start_year[j])}

if(start_year[j]==0){
training.dat<-training.data
test.dat<-test.data}

t1<-(length(subset(training.dat[,species[j]],training.dat[,species[j]]>0))+length(subset(test.dat[,species[j]],test.dat[,species[j]]>0)))/(length(training.dat[,1])+length(test.dat[,1]))
species_pct[j]<-t1
print(paste(species_name[j],round(t1,2)))
dir.create(paste("D://EFH Descriptions 2015/Trawl_models/GOA/",species_name[j],sep=""))
results.path <- paste("D://EFH Descriptions 2015/Trawl_models/GOA/",species_name[j],sep="")


####################################################################################################################################
####################################################################################################################################
##################################################CPUE MODEL######################################################################
####################################################################################################################################

if(models[j]=="gam"){
yvar<-training.dat[species[j]]^1
xvars<-c("s(lon,lat,k=10)","s(slope,k=4)","s(tmax,k=4)","s(color,k=4)","s(speed,k=4)","s(btemp,k=4)","s(bdepth,k=4)","as.factor(sponge)","as.factor(coral)","as.factor(pen)")
gam.form <- as.formula(paste("yvar ~", paste(xvars,collapse="+")))

for(i in 1:length(xvars)){
cpue.gam <- gam(gam.form, family = gaussian, data = training.dat)
gcv_gam<-cpue.gam$gcv.ubre
	pvals<-summary(cpue.gam)$s.pv
	pvals<-c(pvals,summary(cpue.gam)$p.pv[-1])
	least_sig<-which.max(pvals)

xvars1<-xvars[-least_sig]
gam.form1 <- as.formula(paste("yvar ~", paste(xvars1,collapse="+")))
cpue.gam1 <- gam(gam.form1, family = gaussian, data = training.dat)
gcv_gam1<-cpue.gam1$gcv.ubre


if(gcv_gam>gcv_gam1){
	gam.form<-gam.form1
	gcv_gam<-gcv_gam1
	xvars1<-xvars[-least_sig]
	print(summary(cpue.gam1))}
if(gcv_gam<gcv_gam1)break}

######## Training Data

pred2 <- predict(cpue.gam, training.dat, fun = predict, type = "response")
cpue.obs.pred <- data.frame(cbind(x = yvar, y = pred2))
colnames(cpue.obs.pred)[1]<-"x"

######### Test Data
pred4 <- predict(cpue.gam, test.dat, fun = predict, type = "response")
t.yvar<-test.dat[species[j]]

t.cpue.obs.pred <- data.frame(cbind(x = t.yvar, y = pred4))
colnames(t.cpue.obs.pred)[1]<-"x"

######## Prediction Map
predict.CPUE.raster1 <- predict(raster.stack, cpue.gam, fun = predict, na.rm = TRUE, overwrite = TRUE, progress = "text", type = "response", newdata.guaranteed = TRUE)
predict.CPUE.raster<-mask(predict.CPUE.raster1,ak.coast,inverse=TRUE,overwrite=TRUE,progress="text",filename=paste(results.path,"/CPUEpredict",sep=""))

sample1<-sampleRandom(predict.CPUE.raster,600000,na.rm=TRUE)
sample1[sample1<=0]<-NA
breaks<-quantile(sample1, probs =c(0,0.05,0.25,0.5,0.75,1), na.rm = TRUE,names = FALSE)

EFH.raster<-cut(predict.CPUE.raster,breaks=breaks,overwrite=TRUE, filename = paste(results.path,"/EFHmap", sep = ""),progress="text")
save.image(paste(results.path,"/",species_name[j],".RData",sep=""))

####FIGURE 1 - GAM DIAGNOSTICS PLOTS
dir.create(paste(results.path,"/Figures",sep=""))
png(filename=paste(results.path, "/Figures/CPUEGAMdiagnostics.png", sep = ""),width=8.5,height=11,res=300,units="in")
par(mfcol=c(3,2),family="sans",mar=c(4,4,3,1))
qqnorm((cpue.obs.pred$x-cpue.obs.pred$y),main="Training data")
qqline((cpue.obs.pred$x-cpue.obs.pred$y))
hist((cpue.obs.pred$x-cpue.obs.pred$y),xlab="Residuals",main="")
regr <- lm(y~x, data = cpue.obs.pred)
rsqr <- summary(lm(y~x, data = cpue.obs.pred))$r.squared
adj.rsqr <- summary(lm(y~x, data = cpue.obs.pred))$adj.r.squared
pred.max <- max(cpue.obs.pred$y)
obs.max <- max(cpue.obs.pred$x)
if(pred.max > obs.max){
	plot.max = pred.max + (0.04*pred.max)
}else{
	plot.max = obs.max + (0.04*obs.max)
	}
par(xaxs = "i", yaxs = "i")
plot(cpue.obs.pred$x, cpue.obs.pred$y, ylim = c(0,plot.max), xlim = c(0,plot.max), ylab = "Predicted", xlab = "Observed",
	main = "", pch = 20)
abline(coef = c(0,1), lty = 2)
lines(cpue.obs.pred$x, regr$fitted.values)
text(1, plot.max-1, paste("R-squared = ", round(rsqr,2)), pos = 4)

#Plots for test data
qqnorm((t.cpue.obs.pred$x-t.cpue.obs.pred$y),main="Test data")
qqline((t.cpue.obs.pred$x-t.cpue.obs.pred$y))
hist((t.cpue.obs.pred$x-t.cpue.obs.pred$y),xlab="Residuals",main="")
regr <- lm(y~x, data = t.cpue.obs.pred)
rsqr <- summary(lm(y~x, data = t.cpue.obs.pred))$r.squared
adj.rsqr <- summary(lm(y~x, data = t.cpue.obs.pred))$adj.r.squared
pred.max <- max(t.cpue.obs.pred$y)
obs.max <- max(t.cpue.obs.pred$x)
if(pred.max > obs.max){
	plot.max = pred.max + (0.04*pred.max)
}else{
	plot.max = obs.max + (0.04*obs.max)
	}
par(xaxs = "i", yaxs = "i")
plot(t.cpue.obs.pred$x, t.cpue.obs.pred$y, ylim = c(0,plot.max), xlim = c(0,plot.max), ylab = "Predicted", xlab = "Observed",
	main = "", pch = 20)
abline(coef = c(0,1), lty = 2)
lines(t.cpue.obs.pred$x, regr$fitted.values)
text(1, plot.max-1, paste("R-squared = ", round(rsqr,2)), pos = 4)
dev.off()

####FIGURE 2 - GAM EFFECTS PLOT################################
png(filename=paste(results.path, "/Figures/CPUEGAMplots.png", sep = ""),width=8.5,height=11,res=300,units="in")
n1<-summary(cpue.gam)$m
n2<-length(summary(cpue.gam)$p.t)
n2<-ifelse(n2>0,n2-1,0)+n1
v1<-all.vars(formula(cpue.gam))
v1<-v1[-1]
if(v1[1]=="lon"){v1[1]<-"lonlat"
	v1<-v1[-2]}

par(mfrow=c(ceiling(n2/3),3),mar=c(4,4,2,.01))
for(i in 1:n2){
	if(v1[i]=="lonlat"){plot(cpue.gam, scale=0, shade=TRUE,select=i,all.terms=TRUE,xlab="Longitude (northings)",ylab="Latitude (eastings)") }
	if(v1[i]=="slope"){plot(cpue.gam, scale=0, shade=TRUE,select=i,all.terms=TRUE,ylab="Variable effect",xlab="Slope (%)")  }
	if(v1[i]=="tmax"){plot(cpue.gam, scale=0, shade=TRUE,select=i,all.terms=TRUE,ylab="Variable effect",xlab="Tidal current speed (cm/s)")}
	if(v1[i]=="color"){plot(cpue.gam, scale=0, shade=TRUE,select=i,all.terms=TRUE,ylab="Variable effect",xlab="Ocean color")  }
	if(v1[i]=="speed"){plot(cpue.gam, scale=0, shade=TRUE,select=i,all.terms=TRUE,ylab="Variable effect",xlab="Current speed (m/s)")}
	if(v1[i]=="btemp"){plot(cpue.gam, scale=0, shade=TRUE,select=i,all.terms=TRUE,ylab="Variable effect",xlab="Temperature (C)")}
	if(v1[i]=="bdepth"){plot(cpue.gam, scale=0, shade=TRUE,select=i,all.terms=TRUE,ylab="Variable effect",xlab="Depth (m)")}
	if(v1[i]=="sponge"){plot(cpue.gam,select=i,all.terms=TRUE,ylabs="Variable effect",xlabs="Sponge presence")}
	if(v1[i]=="coral"){plot(cpue.gam,select=i,all.terms=TRUE,ylabs="Variable effect",xlabs="Coral presence")}
	if(v1[i]=="pen"){plot(cpue.gam,select=i,all.terms=TRUE,ylabs="Variable effect",xlabs="Pennatulacean presence")}}
dev.off()


######FIGURE 3 - PREDICTED ABUNDANCE MAP ##################################################

png(filename=paste(results.path, "/Figures/CPUEGAMmap.png", sep = ""),width=7.1,height=5,res=300,units="in")
par(mfrow=c(1,1),mar=c(5,4,1,1),family="sans")
plot(predict.CPUE.raster, main = "",xaxt="n",yaxt="n", box=F,col = jet.colors(255),ext=GOA.ext.pol,legend.shrink=0.5,axis.args=list(cex.axis=0.65),legend.args=list(text="Abundance",cex=0.65,cex.lab=0.65,side=1,line=2),horiz=TRUE,ylab="Latitude",xlab="Longitude", zlim = c(0,maxValue(predict.CPUE.raster)))
plot(ak.coast, col = "grey80", add = TRUE)
plot(wrld_grd_proj2,lty=3,col="lightgrey",add=T)
axis(1, at=GOA.xaxis.ticks,labels=GOA.xaxis)
axis(2,at=GOA.yaxis.ticks,labels=GOA.yaxis)
dev.off()

######FIGURE 4 - EFH MAP ##################################################
png(filename=paste(results.path, "/Figures/EFHmap.png", sep = ""),width=7.1,height=5,res=300,units="in")
par(mfrow=c(1,1),mar=c(5,4,1,1),family="sans")
plot(EFH.raster, main = "",xaxt="n",yaxt="n", box=F,col = colorRampPalette(c("grey","coral3"))(4),ext=GOA.ext.pol,legend=FALSE,ylab="Latitude",xlab="Longitude",horiz=TRUE, zlim = c(2,maxValue(EFH.raster)))
plot(ak.coast, col = "black", add = TRUE)
plot(wrld_grd_proj2,lty=3,col="lightgrey",add=T)
axis(1, at=GOA.xaxis.ticks,labels=GOA.xaxis)
axis(2,at=GOA.yaxis.ticks,labels=GOA.yaxis)
legVals <- c("95%","75%","50%","25%")
legend("bottomleft", legend = legVals, pch = 15,col=colorRampPalette(c("grey","coral3"))(4),bty="n", pt.cex = 2 , title = "Percentiles",cex=1)
depth.contour<-contour(raster.stack[["bdepth"]],levels=c(100,300,500),col="black",lwd=0.05,drawlabels=FALSE,add=TRUE)
dev.off()


##########################WRITE RASTERS TO ArcGIS FORMAT GeoTIFF's############################################
writeRaster(predict.CPUE.raster,filename=paste("D://EFH Descriptions 2015/GeoTiffs/GeoTiffs_GOA/",species_name[j], "_CPUE_map.tif", sep = ""),overwrite=TRUE,format="GTiff",options=c("PROFILE=GeoTIFF","TFW=YES"),NAflag=-999)
writeRaster(EFH.raster,filename=paste("D://EFH Descriptions 2015/GeoTiffs/GeoTiffs_GOA/",species_name[j],"_EFHmap.tif", sep = ""),overwrite=TRUE,format="GTiff",options=c("PROFILE=GeoTIFF","TFW=YES"),NAflag=-999)

########TABLE 1 - SUMMARY TABLE FOR CPUE GAM#######################

n1<-summary(cpue.gam)$m
n2<-length(summary(cpue.gam)$p.t)
n2<-ifelse(n2>0,n2-1,0)+n1
v1<-all.vars(formula(cpue.gam))
v1<-v1[-1]
if(v1[1]=="lon"){v1[1]<-"lonlat"
	v1<-v1[-2]}
table1<-array("",dim=c(length(v1)+2,6))
table1[1,]<-c(" "," "," "," ","Training data","Test data")
table1[2,]<-c("Model term","EDF","p-value","Deviance explained","rsq","rsq")
table1[3,4]<-round(summary(cpue.gam)$dev.expl,3)
table1[3,5]<-rsqr <- round(summary(lm(y~x, data = cpue.obs.pred))$r.squared,2)
table1[3,6]<-rsqr <- round(summary(lm(y~x, data = t.cpue.obs.pred))$r.squared,2)

table1a<-v1
table1a[table1a=="lonlat"]<-"Longitude*latitude"
table1a[table1a=="slope"]<-"slope"
table1a[table1a=="pen"]<-"Pennatulacean present"
table1a[table1a=="coral"]<-"Coral present"
table1a[table1a=="sponge"]<-"Sponge present"
table1a[table1a=="color"]<-"Ocean color"
table1a[table1a=="speed"]<-"Current speed"
table1a[table1a=="bdepth"]<-"Depth"
table1a[table1a=="tmax"]<-"Tidal current"
table1a[table1a=="btemp"]<-"Temperature"
table1b<-c(summary(cpue.gam)$edf,summary(cpue.gam)$pTerms.df)
table1c<-c(summary(cpue.gam)$s.pv,summary(cpue.gam)$pTerms.pv)
table1[3:(length(v1)+2),1]<-table1a[order(table1c)] 
table1[3:(length(v1)+2),2]<-signif(table1b[order(table1c)],digits=2)
table1[3:(length(v1)+2),3]<-signif(table1c[order(table1c)],digits=3) 
cpue.table<-xtable(table1)
print.xtable(cpue.table,type="html",file=paste(results.path, "/Figures/cpuegamtable.html", sep = ""),include.rownames = getOption("xtable.include.rownames", FALSE),html.table.attributes=2,
  include.colnames = getOption("xtable.include.colnames", FALSE),  hline.after = getOption("xtable.hline.after", c(-1,1,nrow(table1))))
print(paste(species[j],"Catch model complete"))}

####################################################################################################################################
####################################################################################################################################
##################################################HURDLE MODEL######################################################################
####################################################################################################################################


if(models[j]=="hgam"){

######PRESENCE ABSENCE PART#########
yvar<-training.dat[species[j]]^1
yvar[yvar>0]<-1
xvars<-c("s(lon,lat,k=10)","s(slope,k=4)","s(tmax,k=4)","s(color,k=4)","s(speed,k=4)","s(btemp,k=4)","s(bdepth,k=4)","as.factor(sponge)","as.factor(coral)","as.factor(pen)")
gam.form <- as.formula(paste("yvar ~", paste(xvars,collapse="+")))

for(i in 1:length(xvars)){
pa.gam <- gam(gam.form, family = binomial, data = training.dat)
gcv_gam<-pa.gam$gcv.ubre
	pvals<-summary(pa.gam)$s.pv
	pvals<-c(pvals,summary(pa.gam)$p.pv[-1])
	least_sig<-which.max(pvals)

xvars1<-xvars[-least_sig]
gam.form1 <- as.formula(paste("yvar ~", paste(xvars1,collapse="+")))
pa.gam1 <- gam(gam.form1, family = binomial, data = training.dat)
gcv_gam1<-pa.gam1$gcv.ubre

if(gcv_gam>gcv_gam1){
	gam.form<-gam.form1
	gcv_gam<-gcv_gam1
	xvars<-xvars[-least_sig]
	print(summary(pa.gam1))}
if(gcv_gam<gcv_gam1)break}

######## Training Data

pred2 <- predict(pa.gam, training.dat, fun = predict, na.rm = TRUE, overwrite = TRUE, progress = "text", type = "response", newdata.guaranteed = TRUE)
pa.obs.pred <- data.frame(cbind(x = yvar, y = pred2))
colnames(pa.obs.pred)[1]<-"x"
auc.dat <- data.frame(cbind(seq(1,length(pred2)), pa.obs.pred))
print(auc(auc.dat, na.rm = TRUE))
print(optimal.thresholds(auc.dat, opt.methods = c(seq(1:9))))
thresh <- optimal.thresholds(auc.dat, opt.methods = 2)
thresh <- thresh[,2]
print(cmx(auc.dat, threshold = thresh))

######### Test Data
pred4 <- predict(pa.gam, test.dat, fun = predict, na.rm = TRUE, overwrite = TRUE, progress = "text", type = "response", newdata.guaranteed = TRUE)
t.yvar<-test.dat[species[j]]
t.yvar[t.yvar>0]<-1

t.pa.obs.pred <- data.frame(cbind(x = t.yvar, y = pred4))
colnames(t.pa.obs.pred)[1]<-"x"
t.auc.dat <- data.frame(cbind(seq(1,length(pred4)), t.pa.obs.pred))
print(auc(t.auc.dat, na.rm = TRUE))
print(cmx(t.auc.dat, threshold = thresh))

######## Prediction Map
predict.pa.raster1 <- predict(raster.stack, pa.gam, fun = predict, na.rm = TRUE, overwrite = TRUE, progress = "text", type = "response", newdata.guaranteed = TRUE)
predict.pa.raster<-mask(predict.pa.raster1,ak.coast,inverse=TRUE,overwrite=TRUE,progress="text",filename=paste(results.path,"/PApredict",sep=""))
breaks = c(thresh,1)
pa.raster.cut <- cut(predict.pa.raster, breaks = breaks)

######CPUE PART##########
training.dat.catch<-subset(training.dat,training.dat[species[j]]>0)
test.dat.catch<-subset(test.dat,test.dat[species[j]]>0)
yvar<-training.dat.catch[species[j]]^1
xvars<-c("s(lon,lat,k=10)","s(slope,k=4)","s(tmax,k=4)","s(color,k=4)","s(speed,k=4)","s(btemp,k=4)","s(bdepth,k=4)","as.factor(sponge)","as.factor(coral)","as.factor(pen)")
gam.form <- as.formula(paste("yvar ~", paste(xvars,collapse="+")))

for(i in 1:length(xvars)){
cpue.gam <- gam(gam.form, family = gaussian, data = training.dat.catch)
gcv_gam<-cpue.gam$gcv.ubre
	pvals<-summary(cpue.gam)$s.pv
	pvals<-c(pvals,summary(cpue.gam)$p.pv[-1])
	least_sig<-which.max(pvals)

xvars1<-xvars[-least_sig]
gam.form1 <- as.formula(paste("yvar ~", paste(xvars1,collapse="+")))
cpue.gam1 <- gam(gam.form1, family = gaussian, data = training.dat.catch)
gcv_gam1<-cpue.gam1$gcv.ubre


if(gcv_gam>gcv_gam1){
	gam.form<-gam.form1
	gcv_gam<-gcv_gam1
	xvars<-xvars[-least_sig]
	print(summary(cpue.gam1))}
if(gcv_gam<gcv_gam1) break}

######## Training Data

pred2 <- predict(cpue.gam, training.dat.catch, fun = predict, na.rm = TRUE, overwrite = TRUE, progress = "text", type = "response", newdata.guaranteed = TRUE)
cpue.obs.pred <- data.frame(cbind(x = yvar, y = pred2))
colnames(cpue.obs.pred)[1]<-"x"

######### Test Data
pred4 <- predict(cpue.gam, test.dat.catch, fun = predict, na.rm = TRUE, overwrite = TRUE, progress = "text", type = "response", newdata.guaranteed = TRUE)
t.yvar<-test.dat.catch[species[j]]

t.cpue.obs.pred <- data.frame(cbind(x = t.yvar, y = pred4))
colnames(t.cpue.obs.pred)[1]<-"x"

######## Prediction Map
predict.CPUE.raster1 <- predict(raster.stack, cpue.gam, fun = predict, na.rm = TRUE, overwrite = TRUE, progress = "text", type = "response", newdata.guaranteed = TRUE)
predict.CPUE.raster2<-mask(predict.CPUE.raster1,ak.coast,inverse=TRUE,overwrite=TRUE,progress="text")
predict.CPUE.raster<-mask(predict.CPUE.raster2,pa.raster.cut,overwrite=TRUE,progress="text",filename=paste(results.path,"/CPUEpredict",sep=""))

sample1<-sampleRandom(predict.CPUE.raster,600000,na.rm=TRUE)
sample1[sample1<=0]<-NA
breaks<-quantile(sample1, probs =c(0,0.05,0.25,0.5,0.75,1), na.rm = TRUE,names = FALSE)

EFH.raster<-cut(predict.CPUE.raster,breaks=breaks,overwrite=TRUE, filename = paste(results.path,"/EFHmap", sep = ""),progress="text")
save.image(paste(results.path,"/",species_name[j],".RData",sep=""))

####FIGURE 1 - PA GAM EFFECTS PLOT################################
dir.create(paste(results.path,"/Figures",sep=""))
png(filename=paste(results.path, "/Figures/PAGAMplots.png", sep = ""),width=8.5,height=11,res=300,units="in")
n1<-summary(pa.gam)$m
n2<-length(summary(pa.gam)$p.t)
n2<-ifelse(n2>0,n2-1,0)+n1
v1<-all.vars(formula(pa.gam))
v1<-v1[-1]
if(v1[1]=="lon"){v1[1]<-"lonlat"
	v1<-v1[-2]}

par(mfrow=c(ceiling(n2/3),3),mar=c(4,4,2,.01))
for(i in 1:n2){
	if(v1[i]=="lonlat"){plot(pa.gam, scale=0, shade=TRUE,select=i,all.terms=TRUE,xlab="Longitude (northings)",ylab="Latitude (eastings)") }
	if(v1[i]=="slope"){plot(pa.gam, scale=0, shade=TRUE,select=i,all.terms=TRUE,ylab="Variable effect",xlab="Slope (%)")  }
	if(v1[i]=="tmax"){plot(pa.gam, scale=0, shade=TRUE,select=i,all.terms=TRUE,ylab="Variable effect",xlab="Tidal current speed (cm/s)")}
	if(v1[i]=="color"){plot(pa.gam, scale=0, shade=TRUE,select=i,all.terms=TRUE,ylab="Variable effect",xlab="Ocean color")  }
	if(v1[i]=="speed"){plot(pa.gam, scale=0, shade=TRUE,select=i,all.terms=TRUE,ylab="Variable effect",xlab="Current speed (m/s)")}
	if(v1[i]=="btemp"){plot(pa.gam, scale=0, shade=TRUE,select=i,all.terms=TRUE,ylab="Variable effect",xlab="Temperature (C)")}
	if(v1[i]=="bdepth"){plot(pa.gam, scale=0, shade=TRUE,select=i,all.terms=TRUE,ylab="Variable effect",xlab="Depth (m)")}
	if(v1[i]=="sponge"){plot(pa.gam,select=i,all.terms=TRUE,ylabs="Variable effect",xlabs="Sponge presence")}
	if(v1[i]=="coral"){plot(pa.gam,select=i,all.terms=TRUE,ylabs="Variable effect",xlabs="Coral presence")}
	if(v1[i]=="pen"){plot(pa.gam,select=i,all.terms=TRUE,ylabs="Variable effect",xlabs="Pennatulacean presence")}}
dev.off()

####FIGURE 2 - PA GAM DIAGNOSTICS PLOTS
#Plots for training data
png(filename=paste(results.path, "/Figures/PAGAMdiagnostics.png", sep = ""),width=8.5,height=11,res=300,units="in")
par(mfcol=c(3,2),family="sans",mar=c(4,4,1,.01))
auc.roc.plot(auc.dat,opt.methods=2,main="",add.legend=F,xlab="Specificity",ylab="Sensitivity",add.opt.legend=F)
text(0,.95,"Training data",pos=4,cex=1.25)
calibration.plot(auc.dat,N.bins=10,xlab="Predicted occurence",ylab="Proportion of observed occurence",main="")
presence.absence.hist(auc.dat,,truncate.tallest=TRUE,main="",ylab="Number of observations",xlab="Predicted probability")

#Plots for test data
auc.roc.plot(t.auc.dat,main="",add.legend=F,xlab="Specificity",ylab="Sensitivity",add.opt.legend=F)
text(0,.95,"Test data",pos=4,cex=1.25)
calibration.plot(t.auc.dat,N.bins=10,xlab="Predicted occurence",ylab="Proportion of observed occurence",main="")
presence.absence.hist(t.auc.dat,,truncate.tallest=TRUE,main="",ylab="Number of observations",xlab="Predicted probability")
dev.off()

######FIGURE 3 - PREDICTED PRESENCE MAP ##################################################

png(filename=paste(results.path, "/Figures/PAGAMmap.png", sep = ""),width=7.1,height=5,res=300,units="in")
par(mfrow=c(1,1),mar=c(5,4,1,1),family="sans")
plot(predict.pa.raster, main = "",xaxt="n",yaxt="n", box=F,col = jet.colors(255),ext=GOA.ext.pol,legend.shrink=0.5,axis.args=list(cex.axis=0.65),legend.args=list(text="Probability of presence",cex=0.65,cex.lab=0.65,side=1,line=2),horiz=TRUE,ylab="Latitude",xlab="Longitude", zlim = c(0,1))
plot(ak.coast, col = "grey80", add = TRUE)
plot(wrld_grd_proj2,lty=3,col="lightgrey",add=T)
axis(1, at=GOA.xaxis.ticks,labels=GOA.xaxis)
axis(2,at=GOA.yaxis.ticks,labels=GOA.yaxis)
dev.off()

####FIGURE 4 - CPUE GAM EFFECTS PLOT################################
png(filename=paste(results.path, "/Figures/CPUEGAMplots.png", sep = ""),width=8.5,height=11,res=300,units="in")
n1<-summary(cpue.gam)$m
n2<-length(summary(cpue.gam)$p.t)
n2<-ifelse(n2>0,n2-1,0)+n1
v1<-all.vars(formula(cpue.gam))
v1<-v1[-1]
if(v1[1]=="lon"){v1[1]<-"lonlat"
	v1<-v1[-2]}

par(mfrow=c(ceiling(n2/3),3),mar=c(4,4,2,.01))
for(i in 1:n2){
	if(v1[i]=="lonlat"){plot(cpue.gam, scale=0, shade=TRUE,select=i,all.terms=TRUE,xlab="Longitude (northings)",ylab="Latitude (eastings)") }
	if(v1[i]=="slope"){plot(cpue.gam, scale=0, shade=TRUE,select=i,all.terms=TRUE,ylab="Variable effect",xlab="Slope (%)")  }
	if(v1[i]=="tmax"){plot(cpue.gam, scale=0, shade=TRUE,select=i,all.terms=TRUE,ylab="Variable effect",xlab="Tidal current speed (cm/s)")}
	if(v1[i]=="color"){plot(cpue.gam, scale=0, shade=TRUE,select=i,all.terms=TRUE,ylab="Variable effect",xlab="Ocean color")  }
	if(v1[i]=="speed"){plot(cpue.gam, scale=0, shade=TRUE,select=i,all.terms=TRUE,ylab="Variable effect",xlab="Current speed (m/s)")}
	if(v1[i]=="btemp"){plot(cpue.gam, scale=0, shade=TRUE,select=i,all.terms=TRUE,ylab="Variable effect",xlab="Temperature (C)")}
	if(v1[i]=="bdepth"){plot(cpue.gam, scale=0, shade=TRUE,select=i,all.terms=TRUE,ylab="Variable effect",xlab="Depth (m)")}
	if(v1[i]=="sponge"){plot(cpue.gam,select=i,all.terms=TRUE,ylabs="Variable effect",xlabs="Sponge presence")}
	if(v1[i]=="coral"){plot(cpue.gam,select=i,all.terms=TRUE,ylabs="Variable effect",xlabs="Coral presence")}
	if(v1[i]=="pen"){plot(cpue.gam,select=i,all.terms=TRUE,ylabs="Variable effect",xlabs="Pennatulacean presence")}}
dev.off()

####FIGURE 5 - CPUE GAM DIAGNOSTICS PLOTS
#Plots for training data
png(filename=paste(results.path, "/Figures/CPUEGAMdiagnostics.png", sep = ""),width=8.5,height=11,res=300,units="in")
par(mfcol=c(3,2),family="sans",mar=c(4,4,3,1))
qqnorm((cpue.obs.pred$x-cpue.obs.pred$y),main="Training data")
qqline((cpue.obs.pred$x-cpue.obs.pred$y))
hist((cpue.obs.pred$x-cpue.obs.pred$y),xlab="Residuals",main="")
regr <- lm(y~x, data = cpue.obs.pred)
rsqr <- summary(lm(y~x, data = cpue.obs.pred))$r.squared
adj.rsqr <- summary(lm(y~x, data = cpue.obs.pred))$adj.r.squared
pred.max <- max(cpue.obs.pred$y)
obs.max <- max(cpue.obs.pred$x)
if(pred.max > obs.max){
	plot.max = pred.max + (0.04*pred.max)
}else{
	plot.max = obs.max + (0.04*obs.max)
	}
par(xaxs = "i", yaxs = "i")
plot(cpue.obs.pred$x, cpue.obs.pred$y, ylim = c(0,plot.max), xlim = c(0,plot.max), ylab = "Predicted", xlab = "Observed",
	main = "", pch = 20)
abline(coef = c(0,1), lty = 2)
lines(cpue.obs.pred$x, regr$fitted.values)
text(1, plot.max-1, paste("R-squared = ", round(rsqr,2)), pos = 4)

#Plots for test data
qqnorm((t.cpue.obs.pred$x-t.cpue.obs.pred$y),main="Test data")
qqline((t.cpue.obs.pred$x-t.cpue.obs.pred$y))
hist((t.cpue.obs.pred$x-t.cpue.obs.pred$y),xlab="Residuals",main="")
regr <- lm(y~x, data = t.cpue.obs.pred)
rsqr <- summary(lm(y~x, data = t.cpue.obs.pred))$r.squared
adj.rsqr <- summary(lm(y~x, data = t.cpue.obs.pred))$adj.r.squared
pred.max <- max(t.cpue.obs.pred$y)
obs.max <- max(t.cpue.obs.pred$x)
if(pred.max > obs.max){
	plot.max = pred.max + (0.04*pred.max)
}else{
	plot.max = obs.max + (0.04*obs.max)
	}
par(xaxs = "i", yaxs = "i")
plot(t.cpue.obs.pred$x, t.cpue.obs.pred$y, ylim = c(0,plot.max), xlim = c(0,plot.max), ylab = "Predicted", xlab = "Observed",
	main = "", pch = 20)
abline(coef = c(0,1), lty = 2)
lines(t.cpue.obs.pred$x, regr$fitted.values)
text(1, plot.max-1, paste("R-squared = ", round(rsqr,2)), pos = 4)
dev.off()

######FIGURE 6 - PREDICTED ABUNDANCE MAP ##################################################
png(filename=paste(results.path, "/Figures/CPUEGAMmap.png", sep = ""),width=7.1,height=5,res=300,units="in")
par(mfrow=c(1,1),mar=c(5,4,1,1),family="sans")
plot(predict.CPUE.raster, main = "",xaxt="n",yaxt="n", box=F,col = jet.colors(255),ext=GOA.ext.pol,legend.shrink=0.5,axis.args=list(cex.axis=0.65),legend.args=list(text="Abundance",cex=0.65,cex.lab=0.65,side=1,line=2),horiz=TRUE,ylab="Latitude",xlab="Longitude", zlim = c(0,maxValue(predict.CPUE.raster)))
plot(ak.coast, col = "grey80", add = TRUE)
plot(wrld_grd_proj2,lty=3,col="lightgrey",add=T)
axis(1, at=GOA.xaxis.ticks,labels=GOA.xaxis)
axis(2,at=GOA.yaxis.ticks,labels=GOA.yaxis)
dev.off()

######FIGURE 7 - EFH MAP ##################################################
png(filename=paste(results.path, "/Figures/EFHmap.png", sep = ""),width=7.1,height=5,res=300,units="in")
par(mfrow=c(1,1),mar=c(5,4,1,1),family="sans")
plot(EFH.raster, main = "",xaxt="n",yaxt="n", box=F,col = colorRampPalette(c("grey","coral3"))(4),ext=GOA.ext.pol,legend=FALSE,ylab="Latitude",xlab="Longitude",horiz=TRUE, zlim = c(2,maxValue(EFH.raster)))
plot(ak.coast, col = "black", add = TRUE)
plot(wrld_grd_proj2,lty=3,col="lightgrey",add=T)
axis(1, at=GOA.xaxis.ticks,labels=GOA.xaxis)
axis(2,at=GOA.yaxis.ticks,labels=GOA.yaxis)
legVals <- c("95%","75%","50%","25%")
legend("bottomleft", legend = legVals, pch = 15,col=colorRampPalette(c("grey","coral3"))(4),bty="n", pt.cex = 2 , title = "Percentiles",cex=1)
depth.contour<-contour(raster.stack[["bdepth"]],levels=c(100,300,500),col="black",lwd=0.05,drawlabels=FALSE,add=TRUE)
dev.off()


##########################WRITE RASTERS TO ArcGIS FORMAT GeoTIFF's############################################
dir.create(paste(results.path,"/GeoTiffs",sep=""))
writeRaster(predict.CPUE.raster,filename=paste("D://EFH Descriptions 2015/GeoTiffs/GeoTiffs_GOA/",species_name[j], "_CPUE_map.tif", sep = ""),overwrite=TRUE,format="GTiff",options=c("PROFILE=GeoTIFF","TFW=YES"),NAflag=-999)
writeRaster(predict.pa.raster,filename=paste("D://EFH Descriptions 2015/GeoTiffs/GeoTiffs_GOA/",species_name[j],"_PA_map.tif", sep = ""),overwrite=TRUE,format="GTiff",options=c("PROFILE=GeoTIFF","TFW=YES"),NAflag=-999)
writeRaster(EFH.raster,filename=paste("D://EFH Descriptions 2015/GeoTiffs/GeoTiffs_GOA/",species_name[j],"_EFH_map.tif", sep = ""),overwrite=TRUE,format="GTiff",options=c("PROFILE=GeoTIFF","TFW=YES"),NAflag=-999)

########TABLE 1 - SUMMARY TABLE FOR PRESENCE-ABSENCE GAM#######################
n1<-summary(pa.gam)$m
n2<-length(summary(pa.gam)$p.t)
n2<-ifelse(n2>0,n2-1,0)+n1
v1<-all.vars(formula(pa.gam))
v1<-v1[-1]
if(v1[1]=="lon"){v1[1]<-"lonlat"
	v1<-v1[-2]}
table1<-array("",dim=c(length(v1)+2,9))
table1[1,]<-c(" "," "," "," ","Training data"," "," ","Test data"," ")
table1[2,]<-c("Model term","EDF","p-value","Deviance explained","AUC","Optimum threshold","Percent correctly classified","AUC","Percent correctly classified")
table1[3,4]<-round(summary(pa.gam)$dev.expl,3)
table1[3,5]<-round(auc(auc.dat)$AUC,2)
table1[3,6]<-thresh
table1[3,7]<-round(pcc(cmx(auc.dat,threshold=thresh))$PCC,2)
table1[3,8]<-round(auc(t.auc.dat)$AUC,2)
table1[3,9]<-round(pcc(cmx(t.auc.dat,threshold=thresh))$PCC,2)

table1a<-v1
table1a[table1a=="lonlat"]<-"Longitude*latitude"
table1a[table1a=="slope"]<-"Slope"
table1a[table1a=="pen"]<-"Pennatulacean present"
table1a[table1a=="coral"]<-"Coral present"
table1a[table1a=="sponge"]<-"Sponge present"
table1a[table1a=="color"]<-"Ocean color"
table1a[table1a=="speed"]<-"Current speed"
table1a[table1a=="bdepth"]<-"Depth"
table1a[table1a=="tmax"]<-"Tidal current"
table1a[table1a=="btemp"]<-"Temperature"
table1b<-c(summary(pa.gam)$edf,summary(pa.gam)$pTerms.df)
table1c<-c(summary(pa.gam)$s.pv,summary(pa.gam)$pTerms.pv)
table1[3:(length(v1)+2),1]<-table1a[order(table1c)] 
table1[3:(length(v1)+2),2]<-signif(table1b[order(table1c)],digits=2)
table1[3:(length(v1)+2),3]<-signif(table1c[order(table1c)],digits=3) 
pa.table<-xtable(table1)
print.xtable(pa.table,type="html",file=paste(results.path, "/Figures/pagamtable.html", sep = ""),include.rownames = getOption("xtable.include.rownames", FALSE),html.table.attributes=2,
  include.colnames = getOption("xtable.include.colnames", FALSE),  hline.after = getOption("xtable.hline.after", c(-1,1,nrow(table1))))

########TABLE 2 - SUMMARY TABLE FOR CPUE GAM#######################
n1<-summary(cpue.gam)$m
n2<-length(summary(cpue.gam)$p.t)
n2<-ifelse(n2>0,n2-1,0)+n1
v1<-all.vars(formula(cpue.gam))
v1<-v1[-1]
if(v1[1]=="lon"){v1[1]<-"lonlat"
	v1<-v1[-2]}
table1<-array("",dim=c(length(v1)+2,6))
table1[1,]<-c(" "," "," "," ","Training data","Test data")
table1[2,]<-c("Model term","EDF","p-value","Deviance explained","rsq","rsq")
table1[3,4]<-round(summary(cpue.gam)$dev.expl,3)
table1[3,5]<-rsqr <- round(summary(lm(y~x, data = cpue.obs.pred))$r.squared,2)
table1[3,6]<-rsqr <- round(summary(lm(y~x, data = t.cpue.obs.pred))$r.squared,2)

table1a<-v1
table1a[table1a=="lonlat"]<-"Longitude*latitude"
table1a[table1a=="slope"]<-"Slope"
table1a[table1a=="pen"]<-"Pennatulacean present"
table1a[table1a=="coral"]<-"Coral present"
table1a[table1a=="sponge"]<-"Sponge present"
table1a[table1a=="color"]<-"Ocean color"
table1a[table1a=="speed"]<-"Current speed"
table1a[table1a=="bdepth"]<-"Depth"
table1a[table1a=="tmax"]<-"Tidal current"
table1a[table1a=="btemp"]<-"Temperature"
table1b<-c(summary(cpue.gam)$edf,summary(cpue.gam)$pTerms.df)
table1c<-c(summary(cpue.gam)$s.pv,summary(cpue.gam)$pTerms.pv)
table1[3:(length(v1)+2),1]<-table1a[order(table1c)] 
table1[3:(length(v1)+2),2]<-signif(table1b[order(table1c)],digits=2)
table1[3:(length(v1)+2),3]<-signif(table1c[order(table1c)],digits=3) 
cpue.table<-xtable(table1)
print.xtable(cpue.table,type="html",file=paste(results.path, "/Figures/cpuegamtable.html", sep = ""),include.rownames = getOption("xtable.include.rownames", FALSE),html.table.attributes=2,
  include.colnames = getOption("xtable.include.colnames", FALSE),  hline.after = getOption("xtable.hline.after", c(-1,1,nrow(table1))))
print(paste(species[j],"hurdle model complete"))}


####################################################################################################################################
####################################################################################################################################
##################################################MAXENT MODEL######################################################################
####################################################################################################################################
if(models[j]=="maxent"){

training.pos<-subset(training.dat,training.dat[species[j]]>0)
training.pos<-cbind(training.pos$lon,training.pos$lat)

test.pos<-subset(test.dat,test.dat[species[j]]>0)
test.pos<-cbind(test.pos$lon,test.pos$lat)

maxent.stack<-stack(bathy,slope,btemp,color,bcurrent,tmax)
names(maxent.stack)<-c("Depth","Slope","Temperature","Color","Current_speed","Tidal_current")
maxent.stack<-mask(maxent.stack,GOAstrata)

maxent.model<-maxent(maxent.stack,training.pos, args=c("-P","-J"),path=results.path)

habitat.prediction1<-predict(maxent.model,maxent.stack,overwrite=TRUE,progress="text")
habitat.prediction<-mask(habitat.prediction1,ak.coast,inverse=TRUE,overwrite=TRUE,progress="text",filename=paste(results.path,"/SuitableHabitat",sep=""))

######STEP 5 - TEST THE PREDICTIONS AGGOANST THE TRGOANING DATA############################
#Extract the predictions at the training points
train.predicted<-extract(habitat.prediction,training.pos)

#Create a vector of 1's (presence points for training data)
train.observed<-rep(1,length(train.predicted))

#Choose a random vector of absence points
train.background<-randomPoints(habitat.prediction,length(training.pos[,1]),training.pos,tryf=50)

#Extract the predictions at these points
train.background.predicted<-extract(habitat.prediction,train.background)

#Create a vector of 0's (absence points for training data)
train.background.observed<-rep(0,length(train.background.predicted))

#Bind the observations and predictions together and create a dataframe
train.predicted<-c(train.predicted,train.background.predicted)
train.observed<-c(train.observed,train.background.observed)
train.auc_data<-data.frame(cbind(seq(1,length(train.predicted),1),train.observed,train.predicted))
train.auc_data<-subset(train.auc_data,train.auc_data$train.predicted>=0)

#Calculate the AUC
auc(train.auc_data,na.rm=TRUE)

#Estimate the thresholds and calculate diagnostics
optimal.thresholds(train.auc_data,opt.methods=c(seq(1:9)))
train.threshold<-optimal.thresholds(train.auc_data,opt.methods=2)
train.threshold<-train.threshold[,2]
cmx(train.auc_data,threshold=train.threshold)

#######STEP 6 - TEST THE PREDICTIONS AGGOANST THE TEST DATA FOR FALL############################
#Extract the predictions at the testing points
test.predicted<-extract(habitat.prediction,test.pos)

#Create a vector of 1's (presence points for testing data)
test.observed<-rep(1,length(test.predicted))

#Choose a random vector of absence points
test.background<-randomPoints(habitat.prediction,length(test.pos[,1]),test.pos,tryf=50)

#Extract the predictions at these points
test.background.predicted<-extract(habitat.prediction,test.background)

#Create a vector of 0's (absence points for testing data)
test.background.observed<-rep(0,length(test.background.predicted))

#Bind the observations and predictions together and create a dataframe
test.predicted<-c(test.predicted,test.background.predicted)
test.observed<-c(test.observed,test.background.observed)
test.auc_data<-data.frame(cbind(seq(1,length(test.predicted),1),test.observed,test.predicted))
test.auc_data<-subset(test.auc_data,test.auc_data$test.predicted>=0)

#Calculate the AUC
auc(test.auc_data,na.rm=TRUE)

cmx(test.auc_data,threshold=train.threshold)

####FIGURE 2 - MAXENT EFFECTS PLOT################################
dir.create(paste(results.path,"/Figures",sep=""))
png(filename=paste(results.path, "/Figures/VariableEffectplots.png", sep = ""),width=8.5,height=11,res=300,units="in")

par(mfrow=c(2,3),mar=c(4,4,2,.01))
response(maxent.model,var="Depth",col=1,rug=FALSE)
response(maxent.model,var="Tidal_current",col=1,rug=FALSE)
response(maxent.model,var="Slope",col=1,rug=FALSE)
response(maxent.model,var="Color",col=1,rug=FALSE)
response(maxent.model,var="Temperature",col=1,rug=FALSE)
response(maxent.model,var="Current_speed",col=1,rug=FALSE)
dev.off()

####FIGURE 3 - MAXENT DIAGNOSTICS PLOTS
#Plots for training data
png(filename=paste(results.path, "/Figures/maxentdiagnostics.png", sep = ""),width=8.5,height=11,res=300,units="in")
par(mfcol=c(3,2),family="sans",mar=c(4,4,1,.01))
auc.roc.plot(train.auc_data,opt.methods=2,main="",add.legend=F,xlab="Specificity",ylab="Sensitivity",add.opt.legend=F)
text(0,.95,"Training data",pos=4,cex=1.25)
calibration.plot(train.auc_data,N.bins=10,xlab="Predicted occurence",ylab="Proportion of observed occurence",main="")
presence.absence.hist(train.auc_data,,truncate.tallest=TRUE,main="",ylab="Number of observations",xlab="Predicted probability")

#Plots for test data
auc.roc.plot(test.auc_data,main="",add.legend=F,xlab="Specificity",ylab="Sensitivity",add.opt.legend=F)
text(0,.95,"Test data",pos=4,cex=1.25)
calibration.plot(test.auc_data,N.bins=10,xlab="Predicted occurence",ylab="Proportion of observed occurence",main="")
presence.absence.hist(test.auc_data,,truncate.tallest=TRUE,main="",ylab="Number of observations",xlab="Predicted probability")
dev.off()

######FIGURE 4 - PREDICTED PRESENCE MAP ##################################################

png(filename=paste(results.path, "/Figures/SuitableHabitatmap.png", sep = ""),width=7.1,height=5,res=300,units="in")
par(mfrow=c(1,1),mar=c(5,4,1,1),family="sans")
plot(habitat.prediction, main = "",xaxt="n",yaxt="n", box=F,col = jet.colors(255),ext=GOA.ext.pol,legend.shrink=0.5,axis.args=list(cex.axis=0.65),legend.args=list(text="Probability of suitable habitat",cex=0.65,cex.lab=0.65,side=1,line=2),horiz=TRUE,ylab="Latitude",xlab="Longitude", zlim = c(0,1))
plot(ak.coast, col = "grey80", add = TRUE)
plot(wrld_grd_proj2,lty=3,col="lightgrey",add=T)
axis(1, at=GOA.xaxis.ticks,labels=GOA.xaxis)
axis(2,at=GOA.yaxis.ticks,labels=GOA.yaxis)
dev.off()


######FIGURE 5 - EFH MAP ##################################################

sample1<-sampleRandom(habitat.prediction,300000,na.rm=TRUE)
sample1[sample1<=0]<-NA
breaks<-quantile(sample1, probs =c(0,0.05,0.25,0.5,0.75,1), na.rm = TRUE,names = FALSE)

EFH.raster<-cut(habitat.prediction,breaks=breaks,overwrite=TRUE, filename = paste(results.path, "/EFHmap", sep = ""))
save.image(paste(results.path,"/",species_name[j],".RData",sep=""))
png(filename=paste(results.path, "/Figures/EFHmap.png", sep = ""),width=7.1,height=5,res=300,units="in")
par(mfrow=c(1,1),mar=c(5,4,1,1),family="sans")
plot(EFH.raster, main = "",xaxt="n",yaxt="n", box=F,col = colorRampPalette(c("grey","coral3"))(4),ext=GOA.ext.pol,legend=FALSE,ylab="Latitude",xlab="Longitude",horiz=TRUE, zlim = c(2,maxValue(EFH.raster)))
plot(ak.coast, col = "black", add = TRUE)
plot(wrld_grd_proj2,lty=3,col="lightgrey",add=T)
axis(1, at=GOA.xaxis.ticks,labels=GOA.xaxis)
axis(2,at=GOA.yaxis.ticks,labels=GOA.yaxis)
legVals <- c("95%","75%","50%","25%")
legend("bottomleft", legend = legVals, pch = 15,col=colorRampPalette(c("grey","coral3"))(4),bty="n", pt.cex = 2 , title = "Percentiles",cex=1)
depth.contour<-contour(maxent.stack[["Depth"]],levels=c(100,300,500),col="black",lwd=0.05,drawlabels=FALSE,add=TRUE)
dev.off()

#######FIGURE 1 - POINTS ON DEPTH MAP#################################################
png(filename=paste(results.path, "/Figures/Mappts.png", sep = ""),width=7.1,height=5,res=300,units="in")
par(mfrow=c(1,1),mar=c(5,4,1,1),family="sans")
plot(maxent.stack[["Depth"]], main = "",xaxt="n",yaxt="n", box=F,ext=GOA.ext.pol,legend.shrink=0.5,axis.args=list(cex.axis=0.65),legend.args=list(text="Depth",cex=0.65,cex.lab=0.65,side=1,line=2),horiz=TRUE,ylab="Latitude",xlab="Longitude")
plot(ak.coast, col = "grey80", add = TRUE)
points(training.pos,col="blue",pch=20)
points(test.pos,col="purple",pch=20)
plot(wrld_grd_proj2,lty=3,col="lightgrey",add=T)
axis(1, at=GOA.xaxis.ticks,labels=GOA.xaxis)
axis(2,at=GOA.yaxis.ticks,labels=GOA.yaxis)
legVals <- c("Training data","Testing data")
legend("bottomleft", legend = legVals, pch = 20,col=c("blue","purple"),bty="n", pt.cex = 2 , title = "",cex=1)
dev.off()


##########################WRITE RASTERS TO ArcGIS FORMAT GeoTIFF's############################################
dir.create(paste(results.path,"/GeoTiffs",sep=""))
writeRaster(habitat.prediction,filename=paste("D://EFH Descriptions 2015/GeoTiffs/GeoTiffs_GOA/",species_name[j],"_SuitableHabitat_map.tif", sep = ""),overwrite=TRUE,format="GTiff",options=c("PROFILE=GeoTIFF","TFW=YES"),NAflag=-999)
writeRaster(EFH.raster,filename=paste("D://EFH Descriptions 2015/GeoTiffs/GeoTiffs_GOA/",species_name[j],"_EFH_map.tif", sep = ""),overwrite=TRUE,format="GTiff",options=c("PROFILE=GeoTIFF","TFW=YES"),NAflag=-999)

########TABLE 1 - SUMMARY TABLE FOR PRESENCE-ABSENCE GAM#######################
table1<-array("",dim=c(12,9))
table1[,1]<-c(" "," "," ","Training data"," "," "," "," ","Test data"," "," "," ")
table1[,2]<-c("Model term","Relative importance","Permutation importance","AUC","Correlation","Optimum threshold","Percent correctly classified","Kappa","AUC","Correlation","Percent correctly classified","Kappa")
table1[4,3]<-round(auc(train.auc_data)$AUC,2)
table1[5,3]<-round(cor.test(train.auc_data[,2],train.auc_data[,3])$estimate,3)
table1[6,3]<-train.threshold
table1[7,3]<-round(pcc(cmx(train.auc_data,threshold=train.threshold))$PCC,2)
table1[8,3]<-round(Kappa(cmx(train.auc_data,threshold=train.threshold))$Kappa,2)
table1[9,3]<-round(pcc(cmx(test.auc_data,threshold=train.threshold))$PCC,2)
table1[10,3]<-round(cor.test(test.auc_data[,2],test.auc_data[,3])$estimate,3)
table1[11,3]<-round(pcc(cmx(test.auc_data,threshold=train.threshold))$PCC,2)
table1[12,3]<-round(Kappa(cmx(test.auc_data,threshold=train.threshold))$Kappa,2)

maxentResults<-read.csv(paste(results.path,"/maxentResults.csv",sep=""),header=TRUE)
maxentResults<-as.vector(maxentResults[8:21])
col1<-unlist(strsplit(colnames(maxentResults),split=".",fixed=TRUE))[1]
col2<-unlist(strsplit(colnames(maxentResults),split=".",fixed=TRUE))[3]
col3<-unlist(strsplit(colnames(maxentResults),split=".",fixed=TRUE))[5]
col4<-unlist(strsplit(colnames(maxentResults),split=".",fixed=TRUE))[7]
col5<-unlist(strsplit(colnames(maxentResults),split=".",fixed=TRUE))[9]
col6<-unlist(strsplit(colnames(maxentResults),split=".",fixed=TRUE))[11]
col7<-unlist(strsplit(colnames(maxentResults),split=".",fixed=TRUE))[13]
names1<-c(col1,col2,col3,col4,col5,col6,col7)

cont.order<-maxentResults[1,(1:7)]
perm.order<-maxentResults[1,(8:14)]
perm.order<-perm.order[order(cont.order[1,])]
names1<-names1[order(cont.order[1,])]
cont.order<-cont.order[order(cont.order[1,])]

table1[1,(3:9)]<-rev(names1)
table1[2,(3:9)]<-round(rev(unlist(cont.order)),1)
table1[3,(3:9)]<-round(rev(unlist(perm.order)),1)

pa.table<-xtable(table1)
print.xtable(pa.table,type="html",file=paste(results.path, "/Figures/MaxentTable.html", sep = ""),include.rownames = getOption("xtable.include.rownames", FALSE),html.table.attributes=2,
  include.colnames = getOption("xtable.include.colnames", FALSE),  hline.after = getOption("xtable.hline.after", c(-1,1,nrow(table1))))
print(paste(species[j],"maxent model complete"))}

####################################Make Figures################
if(models[j]=="gam"){
myplot<-list()
myplot[]
dev.new(width=7.1,height=5,res=300,units="in")
par(mfrow=c(1,1),mar=c(5,4,1,1),family="sans")
plot(raster.stack[["bdepth"]], main = "",xaxt="n",yaxt="n", box=F,ext=GOA.ext.pol,zlim=c(0,1000),legend.shrink=0.5,axis.args=list(cex.axis=0.65),legend.args=list(text="Depth",cex=0.65,cex.lab=0.65,side=1,line=2),horiz=TRUE,ylab="Latitude",xlab="Longitude")
plot(ak.coast, col = "grey80", add = TRUE)
training.pos<-subset(training.dat,training.dat[species[j]]>0)
test.pos<-subset(test.dat,test.dat[species[j]]>0)
present<-rbind(cbind(training.pos$lon,training.pos$lat),cbind(test.pos$lon,test.pos$lat))
training.pos<-subset(training.dat,training.dat[species[j]]==0)
test.pos<-subset(test.dat,test.dat[species[j]]==0)
absent<-rbind(cbind(training.pos$lon,training.pos$lat),cbind(test.pos$lon,test.pos$lat))
points(absent,col="black",pch=4,cex=.5)
points(present,col="blue",pch=20,cex=1.5)
plot(wrld_grd_proj2,lty=3,col="lightgrey",add=T)
axis(1, at=GOA.xaxis.ticks,labels=GOA.xaxis)
axis(2,at=GOA.yaxis.ticks,labels=GOA.yaxis)
legVals <- c("Present","Absent")
legend("bottom", legend = legVals, pch = c(20,4),col=c("blue","black"),bty="n", pt.cex = c(2,1) , title = paste("n = ",(length(training.pos[,1])+length(test.pos[,1])),sep=""),cex=1)
text(-250000,1350000,species_name[j],font=2,cex=1)
grid.echo()
a <- grid.grab()
myplot[["a"]]<-a
dev.off()

dev.new(width=7.1,height=5,res=300,units="in")
n1<-summary(cpue.gam)$m
n2<-length(summary(cpue.gam)$p.t)
n2<-ifelse(n2>0,n2-1,0)+n1
v1<-all.vars(formula(cpue.gam))
v1<-v1[-1]
if(v1[1]=="lon"){v1[1]<-"lonlat"
	v1<-v1[-2]}
par(mfrow=c(ceiling(n2/3),3),mar=c(4,4,2,.01))
for(i in 1:n2){
	if(v1[i]=="lonlat"){plot(cpue.gam, scale=0, shade=TRUE,select=i,all.terms=TRUE,xlab="Longitude (northings)",ylab="Latitude (eastings)") }
	if(v1[i]=="slope"){plot(cpue.gam, scale=0, shade=TRUE,select=i,all.terms=TRUE,ylab="Variable effect",xlab="Slope (%)")  }
	if(v1[i]=="tmax"){plot(cpue.gam, scale=0, shade=TRUE,select=i,all.terms=TRUE,ylab="Variable effect",xlab="Tidal current speed (cm/s)")}
	if(v1[i]=="color"){plot(cpue.gam, scale=0, shade=TRUE,select=i,all.terms=TRUE,ylab="Variable effect",xlab="Ocean color")  }
	if(v1[i]=="speed"){plot(cpue.gam, scale=0, shade=TRUE,select=i,all.terms=TRUE,ylab="Variable effect",xlab="Current speed (m/s)")}
	if(v1[i]=="btemp"){plot(cpue.gam, scale=0, shade=TRUE,select=i,all.terms=TRUE,ylab="Variable effect",xlab="Temperature (C)")}
	if(v1[i]=="bdepth"){plot(cpue.gam, scale=0, shade=TRUE,select=i,all.terms=TRUE,ylab="Variable effect",xlab="Depth (m)")}
	if(v1[i]=="sponge"){plot(cpue.gam,select=i,all.terms=TRUE,ylabs="Variable effect",xlabs="Sponge presence")}
	if(v1[i]=="coral"){plot(cpue.gam,select=i,all.terms=TRUE,ylabs="Variable effect",xlabs="Coral presence")}
	if(v1[i]=="pen"){plot(cpue.gam,select=i,all.terms=TRUE,ylabs="Variable effect",xlabs="Pennatulacean presence")}}
grid.echo()
b<- grid.grab()
myplot[["b"]]<-b
dev.off()

dev.new(width=7.1,height=5,res=300,units="in")
par(mfrow=c(1,1),mar=c(5,4,1,1),family="sans")
plot(predict.CPUE.raster, main = "",xaxt="n",yaxt="n", box=F,col = jet.colors(255),ext=GOA.ext.pol,legend.shrink=0.5,axis.args=list(cex.axis=0.65),legend.args=list(text="Abundance",cex=0.65,cex.lab=0.65,side=1,line=2),horiz=TRUE,ylab="Latitude",xlab="Longitude",zlim=c(0,maxValue(predict.CPUE.raster)))
plot(ak.coast, col = "grey80", add = TRUE)
plot(wrld_grd_proj2,lty=3,col="lightgrey",add=T)
axis(1, at=GOA.xaxis.ticks,labels=GOA.xaxis)
axis(2,at=GOA.yaxis.ticks,labels=GOA.yaxis)
grid.echo()
c<- grid.grab()
myplot[["c"]]<-c
dev.off()

png(filename=paste("D:/EFH Descriptions 2015/Figures/Figures_GOA/", species_name[j],"_ModelFig.png", sep = ""),width=8.1,height=16,res=300,units="in")
grid.arrange(grobs=myplot, ncol=1, heights = unit(c(5,5,5),c("in")))
dev.off()
}

if(models[j]=="hgam"){
myplot<-list()
myplot[]
dev.new(width=7.1,height=5,res=300,units="in")
par(mfrow=c(1,1),mar=c(5,4,1,1),family="sans")
plot(raster.stack[["bdepth"]], main = "",xaxt="n",yaxt="n", box=F,ext=GOA.ext.pol,zlim=c(0,1000),legend.shrink=0.5,axis.args=list(cex.axis=0.65),legend.args=list(text="Depth",cex=0.65,cex.lab=0.65,side=1,line=2),horiz=TRUE,ylab="Latitude",xlab="Longitude")
plot(ak.coast, col = "grey80", add = TRUE)
training.pos<-subset(training.dat,training.dat[species[j]]>0)
test.pos<-subset(test.dat,test.dat[species[j]]>0)
present<-rbind(cbind(training.pos$lon,training.pos$lat),cbind(test.pos$lon,test.pos$lat))
training.pos<-subset(training.dat,training.dat[species[j]]==0)
test.pos<-subset(test.dat,test.dat[species[j]]==0)
absent<-rbind(cbind(training.pos$lon,training.pos$lat),cbind(test.pos$lon,test.pos$lat))
points(absent,col="black",pch=4,cex=.5)
points(present,col="blue",pch=20,cex=1.5)
plot(wrld_grd_proj2,lty=3,col="lightgrey",add=T)
axis(1, at=GOA.xaxis.ticks,labels=GOA.xaxis)
axis(2,at=GOA.yaxis.ticks,labels=GOA.yaxis)
legVals <- c("Present","Absent")
legend("bottom", legend = legVals, pch = c(20,4),col=c("blue","black"),bty="n", pt.cex = c(2,1) , title = paste("n = ",(length(training.pos[,1])+length(test.pos[,1])),sep=""),cex=1)
text(-250000,1350000,species_name[j],font=2,cex=1)
grid.echo()
a <- grid.grab()
myplot[["a"]]<-a
dev.off()

dev.new(width=7.1,height=5,res=300,units="in")
n1<-summary(pa.gam)$m
n2<-length(summary(pa.gam)$p.t)
n2<-ifelse(n2>0,n2-1,0)+n1
v1<-all.vars(formula(pa.gam))
v1<-v1[-1]
if(v1[1]=="lon"){v1[1]<-"lonlat"
	v1<-v1[-2]}
par(mfrow=c(ceiling(n2/3),3),mar=c(4,4,2,.01))
for(i in 1:n2){
	if(v1[i]=="lonlat"){plot(pa.gam, scale=0, shade=TRUE,select=i,all.terms=TRUE,xlab="Longitude (northings)",ylab="Latitude (eastings)") }
	if(v1[i]=="slope"){plot(pa.gam, scale=0, shade=TRUE,select=i,all.terms=TRUE,ylab="Variable effect",xlab="Slope (%)")  }
	if(v1[i]=="tmax"){plot(pa.gam, scale=0, shade=TRUE,select=i,all.terms=TRUE,ylab="Variable effect",xlab="Tidal current speed (cm/s)")}
	if(v1[i]=="color"){plot(pa.gam, scale=0, shade=TRUE,select=i,all.terms=TRUE,ylab="Variable effect",xlab="Ocean color")  }
	if(v1[i]=="speed"){plot(pa.gam, scale=0, shade=TRUE,select=i,all.terms=TRUE,ylab="Variable effect",xlab="Current speed (m/s)")}
	if(v1[i]=="btemp"){plot(pa.gam, scale=0, shade=TRUE,select=i,all.terms=TRUE,ylab="Variable effect",xlab="Temperature (C)")}
	if(v1[i]=="bdepth"){plot(pa.gam, scale=0, shade=TRUE,select=i,all.terms=TRUE,ylab="Variable effect",xlab="Depth (m)")}
	if(v1[i]=="sponge"){plot(pa.gam,select=i,all.terms=TRUE,ylabs="Variable effect",xlabs="Sponge presence")}
	if(v1[i]=="coral"){plot(pa.gam,select=i,all.terms=TRUE,ylabs="Variable effect",xlabs="Coral presence")}
	if(v1[i]=="pen"){plot(pa.gam,select=i,all.terms=TRUE,ylabs="Variable effect",xlabs="Pennatulacean presence")}}
grid.echo()
b<- grid.grab()
myplot[["b"]]<-b
dev.off()

dev.new(width=7.1,height=5,res=300,units="in")
par(mfrow=c(1,1),mar=c(5,4,1,1),family="sans")
plot(predict.pa.raster, main = "",xaxt="n",yaxt="n", box=F,col = jet.colors(255),ext=GOA.ext.pol,legend.shrink=0.5,axis.args=list(cex.axis=0.65),legend.args=list(text="Probability of presence",cex=0.65,cex.lab=0.65,side=1,line=2),horiz=TRUE,ylab="Latitude",xlab="Longitude",zlim=c(0,1))
plot(ak.coast, col = "grey80", add = TRUE)
plot(wrld_grd_proj2,lty=3,col="lightgrey",add=T)
axis(1, at=GOA.xaxis.ticks,labels=GOA.xaxis)
axis(2,at=GOA.yaxis.ticks,labels=GOA.yaxis)
grid.echo()
c<- grid.grab()
myplot[["c"]]<-c
dev.off()

dev.new(width=7.1,height=5,res=300,units="in")
n1<-summary(cpue.gam)$m
n2<-length(summary(cpue.gam)$p.t)
n2<-ifelse(n2>0,n2-1,0)+n1
v1<-all.vars(formula(cpue.gam))
v1<-v1[-1]
if(v1[1]=="lon"){v1[1]<-"lonlat"
	v1<-v1[-2]}
par(mfrow=c(ceiling(n2/3),3),mar=c(4,4,2,.01))
for(i in 1:n2){
	if(v1[i]=="lonlat"){plot(cpue.gam, scale=0, shade=TRUE,select=i,all.terms=TRUE,xlab="Longitude (northings)",ylab="Latitude (eastings)") }
	if(v1[i]=="slope"){plot(cpue.gam, scale=0, shade=TRUE,select=i,all.terms=TRUE,ylab="Variable effect",xlab="Slope (%)")  }
	if(v1[i]=="tmax"){plot(cpue.gam, scale=0, shade=TRUE,select=i,all.terms=TRUE,ylab="Variable effect",xlab="Tidal current speed (cm/s)")}
	if(v1[i]=="color"){plot(cpue.gam, scale=0, shade=TRUE,select=i,all.terms=TRUE,ylab="Variable effect",xlab="Ocean color")  }
	if(v1[i]=="speed"){plot(cpue.gam, scale=0, shade=TRUE,select=i,all.terms=TRUE,ylab="Variable effect",xlab="Current speed (m/s)")}
	if(v1[i]=="btemp"){plot(cpue.gam, scale=0, shade=TRUE,select=i,all.terms=TRUE,ylab="Variable effect",xlab="Temperature (C)")}
	if(v1[i]=="bdepth"){plot(cpue.gam, scale=0, shade=TRUE,select=i,all.terms=TRUE,ylab="Variable effect",xlab="Depth (m)")}
	if(v1[i]=="sponge"){plot(cpue.gam,select=i,all.terms=TRUE,ylabs="Variable effect",xlabs="Sponge presence")}
	if(v1[i]=="coral"){plot(cpue.gam,select=i,all.terms=TRUE,ylabs="Variable effect",xlabs="Coral presence")}
	if(v1[i]=="pen"){plot(cpue.gam,select=i,all.terms=TRUE,ylabs="Variable effect",xlabs="Pennatulacean presence")}}
grid.echo()
d<- grid.grab()
myplot[["d"]]<-d
dev.off()

dev.new(width=7.1,height=5,res=300,units="in")
par(mfrow=c(1,1),mar=c(5,4,1,1),family="sans")
plot(predict.CPUE.raster, main = "",xaxt="n",yaxt="n", box=F,col = jet.colors(255),ext=GOA.ext.pol,legend.shrink=0.5,axis.args=list(cex.axis=0.65),legend.args=list(text="Abundance",cex=0.65,cex.lab=0.65,side=1,line=2),horiz=TRUE,ylab="Latitude",xlab="Longitude")
plot(ak.coast, col = "grey80", add = TRUE)
plot(wrld_grd_proj2,lty=3,col="lightgrey",add=T)
axis(1, at=GOA.xaxis.ticks,labels=GOA.xaxis)
axis(2,at=GOA.yaxis.ticks,labels=GOA.yaxis)
grid.echo()
e<- grid.grab()
myplot[["e"]]<-e
dev.off()

png(filename=paste("D:/EFH Descriptions 2015/Figures/Figures_GOA/", species_name[j],"_ModelFig.png", sep = ""),width=16.2,height=16,res=300,units="in")
grid.arrange(grobs=myplot, ncol=2, layout_matrix=rbind(c(NA,1,1,NA),c(2,2,4,4),c(3,3,5,5)),heights = unit(c(5,5,5),c("in")))
dev.off()
}


if(models[j]=="maxent"){
myplot<-list()
myplot[]
dev.new(width=7.1,height=5,res=300,units="in")
par(mfrow=c(1,1),mar=c(5,4,1,1),family="sans")
plot(maxent.stack[["Depth"]], main = "",xaxt="n",yaxt="n", box=F,ext=GOA.ext.pol,zlim=c(0,1000),legend.shrink=0.5,axis.args=list(cex.axis=0.65),legend.args=list(text="Depth",cex=0.65,cex.lab=0.65,side=1,line=2),horiz=TRUE,ylab="Latitude",xlab="Longitude")
plot(ak.coast, col = "grey80", add = TRUE)
points(training.pos,col="blue",pch=20)
points(test.pos,col="purple",pch=20)
plot(wrld_grd_proj2,lty=3,col="lightgrey",add=T)
axis(1, at=GOA.xaxis.ticks,labels=GOA.xaxis)
axis(2,at=GOA.yaxis.ticks,labels=GOA.yaxis)
legVals <- c("Training data","Testing data")
legend("bottom", legend = legVals, pch = 20,col=c("blue","purple"),bty="n", pt.cex = 2 , title = paste("n = ",(length(training.pos[,1])+length(test.pos[,1])),sep=""),cex=1)
text(-250000,1350000,species_name[j],font=2,cex=1)
grid.echo()
a <- grid.grab()
myplot[["a"]]<-a
dev.off()

dev.new(width=7.1,height=5,res=300,units="in")
par(mfrow=c(2,3),mar=c(4,2,2,.01),oma=c(2,4,2,4))
response(maxent.model,var="Depth",col=1,rug=FALSE)
response(maxent.model,var="Tidal_current",col=1,rug=FALSE)
response(maxent.model,var="Slope",col=1,rug=FALSE)
response(maxent.model,var="Color",col=1,rug=FALSE)
response(maxent.model,var="Temperature",col=1,rug=FALSE)
response(maxent.model,var="Current_speed",col=1,rug=FALSE)
mtext("Predicted value",side=2,outer=TRUE,line=1)
grid.echo()
b <- grid.grab()
myplot[["b"]]<-b
dev.off()

dev.new(width=7.1,height=5,res=300,units="in")
par(mfrow=c(1,1),mar=c(5,4,1,1),family="sans")
plot(habitat.prediction, main = "",xaxt="n",yaxt="n", box=F,col = jet.colors(255),ext=GOA.ext.pol,legend.shrink=0.5,axis.args=list(cex.axis=0.65),legend.args=list(text="Probability of suitable habitat",cex=0.65,cex.lab=0.65,side=1,line=2),horiz=TRUE,ylab="Latitude",xlab="Longitude", zlim = c(0,1))
plot(ak.coast, col = "grey80", add = TRUE)
plot(wrld_grd_proj2,lty=3,col="lightgrey",add=T)
axis(1, at=GOA.xaxis.ticks,labels=GOA.xaxis)
axis(2,at=GOA.yaxis.ticks,labels=GOA.yaxis)
grid.echo()
c<- grid.grab()
myplot[["c"]]<-c
dev.off()

png(filename=paste("D:/EFH Descriptions 2015/Figures/Figures_GOA/", species_name[j],"_ModelFig.png", sep = ""),width=8.1,height=16,res=300,units="in")
#dev.new(width=18,height=7,units="in",dpi=300)
grid.arrange(grobs=myplot, ncol=1, heights = unit(c(5,5,5),c("in")))
dev.off()
}
}
removeTmpFiles(h=.5)




########################MAPPING STUFF#####################################
#############MAP ODDS AND ENDS#################################
GOA.xaxis<-c(-162,-158,-154,-150,-146,-142,-138,-134)
GOA.yaxis<-c(53,55,57,59,61)
GOA.yaxis.ticks<-cbind(c(-165,-165,-165.75,-166.5,-167.75),GOA.yaxis)
GOA.xaxis.ticks<-cbind(GOA.xaxis,c(53,53.5,53.75,53,53.15,52.5,52,51.55))
GOA.yaxis.ticks<-project(GOA.yaxis.ticks,"+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
GOA.xaxis.ticks<-project(GOA.xaxis.ticks,"+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
GOA.yaxis.ticks<-GOA.yaxis.ticks[,2]
GOA.xaxis.ticks<-GOA.xaxis.ticks[,1]
GOA.ext.x<-c(-720000,1380000,1380000,-720000,-720000)
GOA.ext.y<-c(450000,450000,1330000,1330000,450000)
GOA.ext.pol<-Polygon(cbind(GOA.ext.x,GOA.ext.y))
GOA.ext.pol<-Polygons(list(GOA.ext.pol),"GOA")
GOA.ext.pol<-SpatialPolygons(list(GOA.ext.pol), proj4string=CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
wrld_p <- map("world", interior = FALSE,plot = FALSE)
llCRS <- CRS("+proj=longlat +ellps=WGS84")
wrld_sp <- map2SpatialLines(wrld_p, proj4string = llCRS)
prj_new <- CRS("+proj=moll")
wrld_proj <- spTransform(wrld_sp, prj_new)
wrld_grd <- gridlines(wrld_sp, easts = c(-130, seq(-166,-134, 2), -170), norths = seq(-75, 75, 2), ndiscr = 100)
wrld_grd_proj <- spTransform(wrld_grd, prj_new)
wrld_grd_proj2<-spTransform(wrld_grd,CRS("+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

#######STEP 7 - PLOT THE RESULTS ##########################################################
jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                     "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))


