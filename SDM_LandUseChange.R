##########################################################################
# SHEFS Species distribution model
##########################################################################

library(dismo)
library(rgdal)  
library(raster) 
library(sp)
library(randomForest)
library(kernlab)
library(rJava)
library(doParallel)
library(maptools)

options(java.parameters = "-Xmx8000m")
memory.limit(size=500000000)
#-----------------------------------------------------------------------------
# user section to set directories and chose input data
#-----------------------------------------------------------------------------

# data source
path<-"C:/Users/Vivienne Groner/Desktop/Vivienne/2020/SDM/Clivia_miniata_2020/"
#future scenario selection
models<-c('CanESM2',"HadGEM2-ES","MPI-ESM-MR" , "MRI-CGCM3")
years<-2015:2055
scenarios<-c("rcp26",'rcp85')
LU_scen<-"scen002"

# Input/output directories
predictors_dir <- paste(path,"predictors/",sep='')
predictors_LU_dir <- paste(path,"predictors/MODIS/",LU_scen,'/',sep='')
occurrence_dir <- paste(path,"occurrence/",sep='')
background_dir <- paste(path,"occurrence/",sep='')
model_dir <- paste(path,"output/scen000/output_SDM/",sep='')
CMIP5_dir <- paste("D:/Vivi/UCL/CMIP5/CMIP5_bioclim/",sep='')

if(!dir.exists(paste(path,"output/",LU_scen,"/output_SDM/",sep=''))){
  dir.create(paste(path,"output/",LU_scen,"/output_SDM/",sep=''), recursive = T)
}
outdir_SDM <- paste(path,"output/",LU_scen,"/output_SDM/",sep='')

if(!dir.exists(paste(path,"output/",LU_scen,"/output_LU/",sep=''))){
  dir.create(paste(path,"output/",LU_scen,"/output_LU/",sep=''), recursive = T)
}
outdir_LU <- paste(path,"output/",LU_scen,"/output_LU/",sep='')

if(!dir.exists(paste(path,"output/",LU_scen,"/output_projected/", sep=""))){
  dir.create(paste(path,"output/",LU_scen,"/output_projected/", sep=""), recursive = T)
}
proj_folder_out<-paste(path,"output/",LU_scen,"/output_projected/", sep="")   


#-----------------------------------------------------------------------------
# data preparation
#-----------------------------------------------------------------------------
# load predictors and occurrence records
predictors<- list.files(predictors_dir, pattern = 'grd')
predictors_in<-paste(predictors_dir, predictors, sep="/")
predictors_r <- lapply(predictors_in, raster)
predictors <- stack(predictors_r)
print(paste('predictors loaded'))

species_in <- as.character(list.files(occurrence_dir, pattern = 'occurrence'))
species_in
species <-species_in[[4]]

occurrence_in <- read.csv(paste(occurrence_dir, species, sep=''))
species_name <- substr(species,12,nchar(species)-4)   #this could be more reproducible although need to think of a way
species_name
coordinates(occurrence_in) <- ~lon+lat
crs(occurrence_in) <- "+proj=longlat +datum=WGS84 +no_defs"
#ext <- extent(occurrence_in)*1.5
ext<-c(25,33,-34,-24)
ext

# create data frame with presence/background data and predictor values
# occurence training and test set
occurrence_lonlat=cbind(data.frame(occurrence_in$lon,occurrence_in$lat))
colnames(occurrence_lonlat) = c('lon', 'lat')
envpres <- data.frame(raster::extract(predictors, occurrence_lonlat))
envpres
#removing points which have missing environmental data
occurrence_lonlat<-occurrence_lonlat[-which(!complete.cases(envpres)),]
envpres<-envpres[complete.cases(envpres),]
nrow(envpres) == nrow(occurrence_lonlat)
print(nrow(envpres))
nrow(unique(envpres))

set.seed(0)
k<-5
group_pres <- kfold(occurrence_lonlat, k=k)

# Background training and test set (bias correction)
background_in <- read.csv(paste(background_dir,'background_Clivia.csv',sep=''),header = TRUE) 
coordinates(background_in) <- ~lon+lat
crs(background_in) <- "+proj=longlat +datum=WGS84 +no_defs"
background_raster <- rasterize(background_in, predictors[[1]], 'X', fun = min)
background_raster_sp<-crop(background_raster, ext) ##added this so all background points are within the same extent as the models
background_raster_sp[!is.na(background_raster_sp)]<-1

#ensuring the background data are at locations for which there is environmental data available in all the layers
a<-calc(predictors, is.na)
a<-calc(a, sum)
values(a)<-ifelse(values(a) ==0,1,NA)
background_raster_sp<-a+background_raster_sp

set.seed(0)
backgr <- randomPoints(background_raster_sp, 1000,occurrence_lonlat) #removed !is.na as the function automatically excludes NA points and '!is.na' was cancelling it out somehow!
colnames(backgr) = c('lon', 'lat')

set.seed(0)
group_bg <- kfold(backgr, k=k)

# extract predictors 
envbackg <- data.frame(raster::extract(predictors, backgr))
envpres_pa<-cbind(1, envpres)
envbackg_pa<-cbind(0, envbackg)
colnames(envpres_pa)[1]<-"pa"
colnames(envbackg_pa)[1]<-"pa"
env_all<-rbind(envpres_pa, envbackg_pa)

# plot occurrence and background for check

data(wrld_simpl)
plot(wrld_simpl,xlim=c(25,33),ylim=c(-34,-24))
box()
points(backgr,col='red')
points(occurrence_lonlat$lon,occurrence_lonlat$lat, col='black',pch=16)

#-----------------------------------------------------------------------------
# SDM Model ensemble
#-----------------------------------------------------------------------------

# GENERALIZED LINEAR MODEL
print(paste('+++ GLM +++'))

fla<- paste("pa ~", paste(names(predictors), collapse="+"))
model_glm <- glm(as.formula(fla),family=binomial(link = "logit"), data=env_all) 

evl_glm<- list()

cl <- makeCluster((detectCores()-1), type='PSOCK')
registerDoParallel(cl)
clusterExport(cl,c('evaluate', 'threshold', 'predict', 'envpres_pa', 
                   'envbackg_pa', 'group_pres', 'group_bg', 'model_glm', 'evl_glm', 
                   'outdir_SDM', 'species_name', 'predictors', 'ext'))

eval_glm<-parLapply(cl, 1:k, function(kf){
  pres_train<-envpres_pa[group_pres!=kf ,]
  pres_test<-envpres_pa[(group_pres==kf) ,]
  back_test<-envbackg_pa[(group_bg==kf),]
  back_train<-envbackg_pa[(group_bg!=kf),]
  envtrain<-rbind(pres_train, back_train)
  
  yy<-names(coef(model_glm))[-1]
  fla<-paste("pa ~", paste(yy, collapse="+"))
  model_glm_out<-glm(as.formula(fla),family=binomial(link = "logit"), data=envtrain)
  evl_glm[[kf]] <- evaluate(pres_test, back_test,model= model_glm_out,type="response")
  saveRDS(evl_glm[[kf]], file = paste(outdir_SDM,species_name,"_eval_glm_",kf,".ascii",sep=''),ascii=TRUE)
  print(evl_glm[[kf]])
}
)
stopCluster(cl)

#If you've run the model once already and just need the evaluation info: NOTE: model comes from 'no land cover change'
eval_glm<-list()
for(kf in 1:k){
 eval_glm[[kf]]<-readRDS(paste(model_dir,species_name,"_eval_glm_",kf,".ascii",sep=''))
}

auc_glm <- sapply(eval_glm, function(x){slot(x, "auc")} )
print(auc_glm)
glm_auc<-mean(auc_glm)

print(paste('GLM done'))


# MAXENT
print(paste('+++ MAXENT +++'))

jar <- paste(system.file(package="dismo"), "/java/maxent.jar", sep='')
evl_ma<- list()

cl <- makeCluster((detectCores()-1), type='PSOCK')
registerDoParallel(cl)
clusterExport(cl,c('maxent', 'evaluate', 'threshold', 'predict', 'occurrence_lonlat', 
                   'backgr', 'group_pres', 'group_bg','outdir_SDM', 'species_name', 
                   'predictors', 'ext', 'evl_ma'))

eval_ma<-parLapply(cl, 1:k, function(kf){
  pres_train<-occurrence_lonlat[group_pres!=kf ,]
  pres_test<-occurrence_lonlat[(group_pres==kf) ,]
  back_test<-backgr[(group_bg==kf),]
  back_train<-backgr[(group_bg!=kf),]
  model_ma <- maxent(predictors, pres_train)#,l1_regularizer=0.7)
  
  evl_ma[[kf]] <- evaluate(pres_test, back_test,model= model_ma,x = predictors)
  
  saveRDS(evl_ma[[kf]], file = paste(outdir_SDM,species_name,"_eval_ma_",kf,".ascii",sep=''),ascii=TRUE)
  print(evl_ma[[kf]])
}
)#
stopCluster(cl)

#If you've run the model once already and just need the evaluation info: NOTE: model comes from 'no land cover change'
eval_ma<-list()
for(kf in 1:k){
  eval_ma[[kf]]<-readRDS(paste(model_dir,species_name,"_eval_ma_",kf,".ascii",sep=''))
}

auc_ma <- sapply( eval_ma, function(x){slot(x, "auc")} )
print(auc_ma)
ma_auc<-mean(auc_ma)
print(paste('MAXENT done'))


# RANDOM FOREST
print(paste('+++ Random forest +++'))

evl_rf<- list()

cl <- makeCluster((detectCores()-1), type='PSOCK')
registerDoParallel(cl)
clusterExport(cl,c('randomForest', 'evaluate', 'threshold', 'predict', 'envpres_pa', 
                   'envbackg_pa', 'group_pres', 'group_bg','outdir_SDM', 'species_name', 
                   'predictors', 'ext', 'evl_rf'))

eval_rf<-parLapply(cl, 1:k, function(kf){
  pres_train<-envpres_pa[group_pres!=kf ,]
  pres_test<-envpres_pa[(group_pres==kf) ,]
  back_test<-envbackg_pa[(group_bg==kf),]
  back_train<-envbackg_pa[(group_bg!=kf),]
  envtrain<-rbind(pres_train, back_train)
  model_rf1 <- paste("pa ~", paste(names(predictors), collapse=" + "))
  model_rf <- randomForest:::randomForest(as.formula(model_rf1), data=envtrain) 
  #saveRDS(model_rf, file = paste(outdir_SDM,species_name,"_model_rf.ascii",sep=''),ascii=TRUE)
  
  evl_rf[[kf]] <- evaluate(pres_test, back_test,model= model_rf,type="response")
  saveRDS(evl_rf[[kf]], file = paste(outdir_SDM,species_name,"_eval_rf_",kf,".ascii",sep=''),ascii=TRUE)
  print(evl_rf[[kf]])
}
)
stopCluster(cl)

#If you've run the model once already and just need the evaluation info: NOTE: model comes from 'no land cover change'
eval_rf<-list()
for(kf in 1:k){
  eval_rf[[kf]]<-readRDS(paste(model_dir,species_name,"_eval_rf_",kf,".ascii",sep=''))  
}

auc_rf <- sapply(eval_rf, function(x){slot(x, "auc")} )
print(auc_rf)
rf_auc<-mean(auc_rf)
print(paste('Random Forest done'))


####################################################################################################
# MODEL ENSEMBLE
####################################################################################################
print(paste('+++ Model ensemble +++'))

##need to run the models again on the whole data set - rather than just the training 
fla<-paste("pa ~", paste(names(predictors), collapse="+"))
model_glm_all<-glm(as.formula(fla),family=binomial(link = "logit"), data=env_all)
model_ma_all <- maxent(predictors, occurrence_lonlat)
model_rf1 <- paste("pa ~", paste(names(predictors), collapse=" + "))
model_rf_all <- randomForest(as.formula(model_rf1), data=env_all, na.action=na.roughfix) 

predict_glm_all<- predict(predictors, model_glm_all, ext = ext)
predict_glm_all <-raster:::calc(predict_glm_all, fun=function(x){ exp(x)/(1+exp(x))})
predict_maxent_all<-predict(model_ma_all, predictors,ext=ext)  
predict_rf_all<-predict(predictors,model_rf_all, ext=ext)  

all_models <- stack( predict_glm_all, predict_maxent_all, predict_rf_all)  
names(all_models) <- c( "GLM", "MAXENT","RANDOM FOREST")
auc<-c( glm_auc,ma_auc ,rf_auc)
w <- (auc-0.5)^2
saveRDS(auc, file = paste(outdir_SDM,species_name,"_all_models_auc.asc",sep=''),ascii=TRUE)
ensemble_raster <- weighted.mean( all_models, w)  
saveRDS(ensemble_raster, file = paste(outdir_SDM,species_name,"_ensemble_raster_raw.asc",sep=''),ascii=TRUE)

tiff(paste(outdir_SDM,species_name,'_ensemble_raw.tiff', sep=''))
plot(ensemble_raster,legend = FALSE, main=paste('Weighted ensemble mean - ', species_name),xlab="longitude", ylab="latitude")
plot(wrld_simpl, add=TRUE)
box()
points(occurrence_in$lon,occurrence_in$lat,pch=16)  
dev.off()

# save input HS map for RAMAS
writeRaster(ensemble_raster,file=paste(outdir_SDM,species_name,"_HSmap.asc",sep=''),formate='ascii',overwrite=TRUE)
#ensemble_raster<-raster(paste(outdir_SDM,species_name,"_HSmap.asc",sep=''))
#ensemble_raster
#crs(ensemble_raster) <- "+proj=longlat +datum=WGS84 +no_defs"
#plot(ensemble_raster)
print(paste('Ensemble mean done'))


#evaluate ensemble
#source("ensemble_evaluate.R")
ensemble_evaluate<-function (p, a, ensemble_raster, tr) 
{
  p<-extract(ensemble_raster, p)
  a<-extract(ensemble_raster, a)
  
  p <- stats::na.omit(p)
  a <- stats::na.omit(a)
  np <- length(p)
  na <- length(a)
  if (na == 0 | np == 0) {
    stop("cannot evaluate a model without absence and presence data that are not NA")
  }
  if (missing(tr)) {
    if (length(p) > 1000) {
      tr <- as.vector(quantile(p, 0:1000/1000))
    }
    else {
      tr <- p
    }
    if (length(a) > 1000) {
      tr <- c(tr, as.vector(quantile(a, 0:1000/1000)))
    }
    else {
      tr <- c(tr, a)
    }
    tr <- sort(unique(round(tr, 8)))
    tr <- c(tr - 1e-04, tr[length(tr)] + c(0, 1e-04))
  }
  else {
    tr <- sort(as.vector(tr))
  }
  N <- na + np
  xc <- new("ModelEvaluation")
  xc@presence = p
  xc@absence = a
  R <- sum(rank(c(p, a))[1:np]) - (np * (np + 1)/2)
  xc@auc <- R/(as.numeric(na) * as.numeric(np))
  cr <- try(cor.test(c(p, a), c(rep(1, length(p)), rep(0, length(a)))), 
            silent = TRUE)
  if (class(cr) != "try-error") {
    xc@cor <- cr$estimate
    xc@pcor <- cr$p.value
  }
  res <- matrix(ncol = 4, nrow = length(tr))
  colnames(res) <- c("tp", "fp", "fn", "tn")
  xc@t <- tr
  for (i in 1:length(tr)) {
    res[i, 1] <- length(p[p >= tr[i]])
    res[i, 2] <- length(a[a >= tr[i]])
    res[i, 3] <- length(p[p < tr[i]])
    res[i, 4] <- length(a[a < tr[i]])
  }
  xc@confusion = res
  a = res[, 1]
  b = res[, 2]
  c = res[, 3]
  d = res[, 4]
  xc@np <- as.integer(np)
  xc@na <- as.integer(na)
  xc@prevalence = (a + c)/N
  xc@ODP = (b + d)/N
  xc@CCR = (a + d)/N
  xc@TPR = a/(a + c)
  xc@TNR = d/(b + d)
  xc@FPR = b/(b + d)
  xc@FNR = c/(a + c)
  xc@PPP = a/(a + b)
  xc@NPP = d/(c + d)
  xc@MCR = (b + c)/N
  xc@OR = (a * d)/(c * b)
  prA = (a + d)/N
  prY = (a + b)/N * (a + c)/N
  prN = (c + d)/N * (b + d)/N
  prE = prY + prN
  xc@kappa = (prA - prE)/(1 - prE)
  return(xc)
}

ens<-ensemble_evaluate(occurrence_lonlat,backgr, ensemble_raster)

# find thresholds for different methods
thres_list <- c('kappa','spec_sens','no_omission','prevalence','equal_sens_spec','sensitivity')
tr_ensemble <- list()
ensemble_raster_pa <- list()

for (i in 1:length(thres_list)){
  tr_ensemble[[i]]<-threshold(ens, stat=paste(thres_list[[i]]))
  ensemble_raster_pa[[i]] <- ensemble_raster > tr_ensemble[[i]]
  print(paste('threshold',thres_list[[i]],tr_ensemble[[i]], sep=' '))
}

ens<-ensemble_evaluate(occurrence_lonlat,backgr, ensemble_raster, tr=as.character(tr_ensemble))
ens@t
ens@FNR

ensemble_raster_pa_stack<-stack(ensemble_raster_pa)
plot(ensemble_raster_pa_stack,main=paste(thres_list),zlim=c(0,1))

tiff(paste(outdir_SDM,species_name,"_pa.tiff", sep=''))
plot(ensemble_raster_pa_stack[[3]],legend = FALSE, col = rev(terrain.colors(2)), main=paste(species_name),xlab="longitude", ylab="latitude")
box()
points(occurrence_in$lon,occurrence_in$lat,pch=3)  
dev.off()


for (i in 1:length(thres_list)){
  saveRDS(ensemble_raster_pa[[i]], file = paste(outdir_SDM,species_name,"_pa_",thres_list[[i]],".asc",sep=''),ascii=TRUE)
}

#save models

save(model_glm_all,file = paste(outdir_SDM,species_name,"glm_model.rda",sep=''))
load(paste(outdir_SDM,species_name,"glm_model.rda",sep=''))

save(model_ma_all,file = paste(outdir_SDM,species_name,"maxent_model.rda",sep=''))
load(paste(outdir_SDM,species_name,"maxent_model.rda",sep=''))

save(model_rf_all,file = paste(outdir_SDM,species_name,"randomforest_model.rda",sep=''))
load(paste(outdir_SDM,species_name,"randomforest_model.rda",sep=''))

print(paste(species_name, 'SDM DONE', sep=' '))


####################################################################################
####################################################################################
#  
# Land use model
#  
####################################################################################
####################################################################################
# load predictor MODIS file
f <- list.files('C:/Users/Vivienne Groner/Desktop/Vivienne/2020/SDM/Clivia_miniata_2020/predictors/MODIS/scen002/',pattern='grd')
fp<-paste('C:/Users/Vivienne Groner/Desktop/Vivienne/2020/SDM/Clivia_miniata_2020/predictors/MODIS/scen002/', f, sep="")
fp

predictors_LU_in <- lapply(fp, raster)
predictors_LU_in1<-stack(predictors_LU_in[1:5])
predictors_LU_in2<-stack(predictors_LU_in[6:41])
predictors_LU_x1 <- crop(predictors_LU_in1,extent(ensemble_raster))
predictors_LU_x2 <- crop(predictors_LU_in2,extent(ensemble_raster))
predictors_LU_s <- stack(predictors_LU_x1,predictors_LU_x2)

predictors_LU<-resample(predictors_LU_s,ensemble_raster)
predictors_LU

# load species land cover preference mask
species_mask_in<-raster(paste('C:/Users/Vivienne Groner/Desktop/Vivienne/2020/SDM/Clivia_miniata_2020/predictors/MODIS/Clivia_all_LU_mask.grd',sep=''))
species_mask<-crop(species_mask_in,extent(ensemble_raster))

predictors_LU_species<-predictors_LU*species_mask
predictors_LU_species[is.na(predictors_LU_species)]<-0
predictors_LU_species[predictors_LU_species>0]<-1

lct_mask<-ensemble_raster*predictors_LU_species

for (i in 1:length(years)){
  writeRaster(lct_mask[[i]],file=paste(outdir_LU,species_name,'_',LU_scen,'_LUonly_HS',years[[i]],'.asc',sep=''),overwrite=TRUE)
}

# mask land use with occurrence based on climate  
ensemble_raster_mask<- ensemble_raster
ensemble_raster_mask[ensemble_raster_mask <tr_ensemble[[3]]] <- NA
predictor_LU_masked <- predictors_LU_species*ensemble_raster_mask
plot(predictor_LU_masked[[2]],legend = FALSE, col = rev(terrain.colors(2)), main=paste(thres_list[[3]]),xlab="longitude", ylab="latitude")

print(paste(species_name, 'LU DONE', sep=' '))


##########################################################
# predict in future
######################################################

predictors_static <-subset(predictors,7:nlayers(predictors))
predictors_static 

fp_f<-list.files(CMIP5_dir, pattern = "*grd")
all_proj<-expand.grid(models, years, scenarios)
colnames(all_proj)<-c("models", "years", "scenarios")

proj_out<- list()

cl <- makeCluster((detectCores()-1), type='PSOCK')
registerDoParallel(cl)
clusterExport(cl,c('randomForest', 'predict','all_proj', 'fp_f', 'CMIP5_dir','raster', 'stack', 'weighted.mean', 
                   'writeRaster', 'model_glm_all', 'model_ma_all', 'model_rf_all', 'ext', 
                   'glm_auc','ma_auc' ,'rf_auc', 'proj_folder_out', 'species_name', 'proj_out',
                   'predictors_LU_species','predictors_static','tr_ensemble','LU_scen'))

clusterApply(cl, 1:nrow(all_proj), function(pn){
  model = all_proj$models[pn]
  year = all_proj$years[pn]
  scenario = all_proj$scenarios[pn]
  
  model_sel<-fp_f[grepl(model, fp_f)]
  year_sel<-model_sel[grepl(year, model_sel)]
  all_sel<-year_sel[grepl(scenario, year_sel)]
  
  pred_proj<-stack(paste(CMIP5_dir, all_sel, sep = "/"))
  names(pred_proj)<-all_sel
  names(pred_proj)[grepl("MAP", names(pred_proj))]<-"new_CHELSA_annualprecip"
  names(pred_proj)[grepl("MAT", names(pred_proj))]<-"new_CHELSA_meantemp"
  names(pred_proj)[grepl("PCV", names(pred_proj))]<- "new_CHELSA_precipseasonality"
  names(pred_proj)[grepl("TSMAX", names(pred_proj))]<- "new_CHELSA_tempmaxmax"
  names(pred_proj)[grepl("TSMIN", names(pred_proj))]<- "new_CHELSA_tempminmin"
  names(pred_proj)[grepl("TSD", names(pred_proj))]<- "new_CHELSA_tempseasonality"
  
  pred_proj<-stack(pred_proj,predictors_static)
  
  predict_glm_proj<- predict(pred_proj, model_glm_all, ext = ext)
  predict_glm_proj <-raster:::calc(predict_glm_proj, fun=function(x){ exp(x)/(1+exp(x))})
  predict_maxent_proj <-predict(model_ma_all, pred_proj,ext=ext)
  predict_rf_proj <-predict(pred_proj,model_rf_all, ext=ext)
  
  all_models_proj <- stack( predict_glm_proj, predict_maxent_proj, predict_rf_proj)
  names(all_models_proj) <- c( "GLM", "MAXENT","RANDOM FOREST")
  auc<-c( glm_auc,ma_auc ,rf_auc)
  w <- (auc-0.5)^2
  
  proj_out[[pn]] <- weighted.mean(all_models_proj, w)
  #writeRaster(proj_out[[pn]], paste(proj_folder_out,species_name,"_", model,"_" ,year,"_",scenario,"_",LU_scen,"SDM_HS.asc", sep = ""), overwrite = TRUE)
  
  proj_out[[pn]]<-proj_out[[pn]]*predictors_LU_species[[year-2014]]
  proj_out[[pn]][is.na(proj_out[[pn]])]<-0
  writeRaster(proj_out[[pn]], paste(proj_folder_out,species_name,"_", model,"_" ,year,"_",scenario,"_",LU_scen,"_HS.asc", sep = ""), overwrite = TRUE)
  
}

)

stopCluster(cl)

print(paste(species_name, 'prediction DONE', sep=' '))



##################################################################################################
# END
##################################################################################################


