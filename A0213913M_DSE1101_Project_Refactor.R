rm(list = ls())
library(xlsx)
library(ISLR)
library(ROCR)
library(kknn)
library(rpart)
library(rpart.plot)
library(ks)
library(stringr)
library(pls)
library(BBmisc)
library(dplyr)

#NOTE: This code is for the purposes of grading, hence we have only plotted essential outputs and used more limited
#predictors for some of the models in the 1st step as described in the report.
#The original code with full output is in "DSE1101_Project.R".

#set directory
dir="C:/work/nus work after 8-8-22/A0213913M DSE1101 Project/"
setwd(dir)

#not sure if it is a localized problem, but I am not able to read in the xlsx file correctly, so have converted to csv manually
data=read.csv("HDB_data_2021_sample.csv",header=TRUE) #6000 obs of 230 variables
#below is the read function for .xlsx file, though it does not work on my machine and I have commented it out
#data=read.xlsx("HDB_data_2021_sample.xlsx",1,header=TRUE)

#we notice many of the columns are all 0s, so we remove them as they do not contribute
cols <- colnames(data)
cols.to.keep <- cols[colSums(data[cols]) != 0]
data_clean=data[cols.to.keep] #6000 obs of 213 variables
#removing irrelevant variables for the scope of this project
data_clean=select(data_clean,-starts_with("postal_2digits_"))
data_clean=select(data_clean,-starts_with("storey_range_"))
data_clean=select(data_clean,-starts_with("year"))
data_clean=select(data_clean,-starts_with("flat_model_"))
data_clean=select(data_clean,-starts_with("ADF_"))
data_clean=select(data_clean,-starts_with("Dist_nearest_ADF"))
data_clean=select(data_clean,-starts_with("Dist_nearest_A_hospital"))
data_clean=select(data_clean,-starts_with("Dist_nearest_G_"))
data_clean=select(data_clean,-starts_with("Dist_nearest_GAI_"))
data_clean=select(data_clean,-starts_with("no_G_"))
data_clean=select(data_clean,-starts_with("no_GAI_"))
data_clean=select(data_clean,-starts_with("Nearest_",ignore.case=FALSE)) #6000 obs of 88 variables

#target variables
TARGET='resale_price'
contvars=c('Remaining_lease','floor_area_sqm','max_floor_lvl','total_dwelling_units','Dist_nearest_GHawker',
           'Dist_nearest_mall','Dist_nearest_beach','Dist_nearest_CC','Dist_nearest_station','Dist_nearest_primary_school',
           'Dist_nearest_secondary_school','Dist_nearest_jc','Dist_nearest_polytechnic','Dist_nearest_university',
           'Dist_nearest_hospital','Dist_CBD','unique_no_mrt_0.5km','unique_no_mrt_1km','mature')
noroomsold=c('X1room_sold','X2room_sold','X3room_sold','X4room_sold','X5room_sold','exec_sold','multigen_sold','studio_apartment_sold')
noroomrent=c('X1room_rental','X2room_rental','X3room_rental','other_room_rental')
flattype=c('flat_type_1.ROOM','flat_type_2.ROOM','flat_type_3.ROOM','flat_type_4.ROOM','flat_type_5.ROOM','flat_type_EXECUTIVE',
           'flat_type_MULTI.GENERATION')
amenities=c('Dist_nearest_GHawker','nearest_ghawker_no_of_cooked_food_stalls','nearest_ghawker_no_of_mkt_produce_stalls',
            'nearest_ghawker_no_of_stalls','Dist_nearest_mall','no_malls_0.5km','no_malls_1km','no_malls_2km',
            'Dist_nearest_beach','beach_within_2km','Dist_nearest_waterbody','waterbody_within_2km','Dist_nearest_CC',
            'Dist_nearest_station')
mrt=c('NSL','EWL','NEL','CCL','DTL','TEL','LRT')
month=c('month_Jan','month_Feb','month_Mar','month_Apr','month_May')
town=colnames(select(data_clean,starts_with("town_")))

#handle collinearity
data_clean=cbind(data_clean,month=apply(data_clean[2:6],1,function(x) names(x)[as.logical(as.numeric(as.character(x)))]))
for (i in 1:length(month)){
  data_clean$month[data_clean$month==month[i]]<-i
}
data_clean$month<-as.numeric(data_clean$month)
data_clean=cbind(data_clean,town=apply(data_clean[7:32],1,function(x) names(x)[as.logical(as.numeric(as.character(x)))]))
for (i in 1:length(town)){
  data_clean$town[data_clean$town==town[i]]<-i
}
data_clean$town<-as.numeric(data_clean$town)
data_clean=cbind(data_clean,flattype=apply(data_clean[50:56],1,function(x) names(x)[as.logical(as.numeric(as.character(x)))]))
for (i in 1:length(flattype)){
  data_clean$flattype[data_clean$flattype==flattype[i]]<-i
}
data_clean$flattype<-as.numeric(data_clean$flattype)

#standardization/normalization of resale_price
data_std=data_clean
data_std[,1]<-scale(data_clean[,1])
data_norm=data_clean
data_norm[,1]<-normalize(data_clean[,1],method='range',range=c(0,1))
summary(data_clean)
summary(data_std)
summary(data_norm)

set.seed(1)
ntrain=5000
tr=sample(1:nrow(data_clean),ntrain)
train_OG=data_clean[tr,]
test_OG=data_clean[-tr,]
train_STD=data_std[tr,]
test_STD=data_std[-tr,]
train_NORM=data_norm[tr,]
test_NORM=data_norm[-tr,]

#our initial linear regression and correlation matrix on all variables, just to see if there is anything useful
c1=cor(data_clean)
any(c1>=0.8 & c1<1)
which(c1>=0.8 & c1<1)
any(c1<=-0.8 & c1>-1)
which(c1<=-0.8 & c1>-1)
lm1=lm(resale_price~.,train_OG)
lm2=lm(resale_price~.,train_STD)
lm3=lm(resale_price~.,train_NORM)
summary(lm1)
summary(lm2)
summary(lm3)

# cleaning whitespace (some methods of importing dataset may result in ints/numerics being chr with whitespace)
# do not uncomment, they do not work here, for reference only
# data_clean[,var_to_use]<-apply(data_clean[,var_to_use],2,function(x) as.numeric(str_trim(as.character(x))))
# train_OG[,var_to_use]<-apply(train_OG[,var_to_use],2,function(x) as.numeric(str_trim(as.character(x))))
# train_STD[,var_to_use]<-apply(train_STD[,var_to_use],2,function(x) as.numeric(str_trim(as.character(x))))
# train_NORM[,var_to_use]<-apply(train_NORM[,var_to_use],2,function(x) as.numeric(str_trim(as.character(x))))
# test_OG[,var_to_use]<-apply(test_OG[,var_to_use],2,function(x) as.numeric(str_trim(as.character(x))))
# test_STD[,var_to_use]<-apply(test_STD[,var_to_use],2,function(x) as.numeric(str_trim(as.character(x))))
# test_NORM[,var_to_use]<-apply(test_NORM[,var_to_use],2,function(x) as.numeric(str_trim(as.character(x))))

numvars=c(contvars,noroomsold,noroomrent,flattype,amenities,mrt)
numvarss=c("contvars","noroomsold","noroomrent","flattype","amenities","mrt")
catvars=c(month,town,flattype)
catvarss=c('month','town','flattype')
sets=c('OG','STD','NORM')

temp_kde=c("floor_area_sqm","Dist_CBD","max_floor_lvl","Remaining_lease","flattype")
temp_pca=c("flattype","noroomsold","mrt","contvars")
temp_kmeans=c("total_dwelling_units")

#KDE
for (x in numvars){
  if (x %in% temp_kde){
    plot(kde(eval(parse(text=paste('data_clean$',x))), h = hlscv(eval(parse(text=paste('data_clean$',x))))), main = paste(x,' KDE - min MISE'), xlab = paste(x))
    fname=paste(paste("KDE",x,sep="-"),".pdf",sep="")
    pdf(fname)
    plot(kde(eval(parse(text=paste('data_clean$',x))), h = hlscv(eval(parse(text=paste('data_clean$',x))))), main = paste(x,' KDE - min MISE'), xlab = paste(x))
    dev.off()
  }
}
for (x in catvarss){
  if (x %in% temp_kde){
    plot(kde(eval(parse(text=paste('data_clean$',x))), h = hlscv(eval(parse(text=paste('data_clean$',x))))), main = paste(x,' KDE - min MISE'), xlab = paste(x))
    fname=paste(paste("KDE",x,sep="-"),".pdf",sep="")
    pdf(fname)
    plot(kde(eval(parse(text=paste('data_clean$',x))), h = hlscv(eval(parse(text=paste('data_clean$',x))))), main = paste(x,' KDE - min MISE'), xlab = paste(x))
    dev.off()
  }
}

#decision tree
#trees are not plotted in R GUI as they are quite big and difficult to see, refer to pdf output
for(x in 1:length(catvarss)){
  for(z in 1:length(sets)){
    print(paste("tree",catvarss[x],z,sep="-"))
    d=eval(parse(text=catvarss[x]))
    big.tree = rpart(paste(paste(TARGET,'~.'),paste(d,collapse="-"),catvarss[x],sep="-"),method="anova",eval(parse(text=paste0("train_",sets[z],collapse=''))), minsplit=5,cp=.0005)
    length(unique(big.tree$where))
    plotcp(big.tree) #CV plot
    bestcp=big.tree$cptable[which.min(big.tree$cptable[,"xerror"]),"CP"]
    best.tree = prune(big.tree,cp=bestcp)
    fname=paste(paste("tree",catvarss[x],z,sep="-"),".pdf",sep="")
    pdf(fname)
    rpart.plot(best.tree,uniform=TRUE)
    dev.off()
    treefit=predict(best.tree,eval(parse(text=paste0("test_",sets[z],collapse=''))),type="vector")
    print(mean((eval(parse(text=paste0("test_",sets[z],'$',TARGET,collapse='')))-treefit)^2))
  }
}

#PCA&PCR
for(x in 1:length(numvarss)){
  for(z in 1:length(sets)){
    pr = prcomp(eval(parse(text=paste0("train_",sets[z],collapse='')))[eval(parse(text=numvarss[x]))], scale = TRUE)
    if (numvarss[x] %in% temp_pca){
      biplot(pr)
      fname=paste(paste("biplot",numvarss[x],z,sep="-"),".pdf",sep="")
      pdf(fname)
      biplot(pr)
      dev.off()
    }
    pr.s = summary(pr)
    print(pr.s$importance)
    scree = pr.s$importance[2,]
    if(numvarss[x] %in% temp_pca){
      plot(scree, main = paste("Scree Plot",numvarss[x],z,sep=" "), xlab = "Principal Component",
           ylab = "Proportion of Variance Explained", ylim = c(0,1), type = 'b', cex = .8)
      fname=paste(paste("scree",numvarss[x],z,sep="-"),".pdf",sep="")
      pdf(fname)
      plot(scree, main = paste("Scree Plot",numvarss[x],z,sep=" "), xlab = "Principal Component",
           ylab = "Proportion of Variance Explained", ylim = c(0,1), type = 'b', cex = .8)
      dev.off()
    }

    #LOOCV takes too long to run
    a=eval(parse(text=numvarss[x]))
    threshold=0.8
    set.seed(2)
    pcr.fit=NULL
    res<-tryCatch(
      {
        pcr.fit<<-pcr(formula(paste(paste(TARGET,'~'),paste(a,collapse='+'))),data=eval(parse(text=paste0("train_",sets[z],collapse=''))), scale=TRUE, validation="CV")
      },error=function(err){
        pcr.fit<<-pcr(formula(paste(paste(TARGET,'~'),paste(a,collapse='+'))),data=eval(parse(text=paste0("train_",sets[z],collapse=''))), scale=FALSE, validation="CV")
      }
    )
    #stopifnot(!is.null(pcr.fit))
    if (numvarss[x] %in% temp_pca){
      plot(pcr.fit, "loadings", comps = 1:as.numeric(which(data.frame(pr.s$importance)[3,]>threshold)[1]), legendpos = "topleft")
      abline(h = 0)
      fname=paste(paste("loadings",numvarss[x],z,threshold,sep="-"),".pdf",sep="")
      pdf(fname)
      plot(pcr.fit, "loadings", comps = 1:as.numeric(which(data.frame(pr.s$importance)[3,]>threshold)[1]), legendpos = "topleft")
      abline(h = 0)
      dev.off()
      validationplot(pcr.fit, val.type="MSEP", main="CV",legendpos = "topright")
      m=MSEP(pcr.fit)
      abline(v=as.numeric(which(data.frame(pr.s$importance)[3,]>threshold)[1]))
      abline(h=min(m$val[1,1,]),lty=2)
      abline(h=min(m$val[2,1,]),lty=2)
      fname=paste(paste("validationplot",numvarss[x],z,threshold,sep="-"),".pdf",sep="")
      pdf(fname)
      validationplot(pcr.fit, val.type="MSEP", main="CV",legendpos = "topright")
      abline(v=as.numeric(which(data.frame(pr.s$importance)[3,]>threshold)[1]))
      abline(h=min(m$val[1,1,]),lty=2)
      abline(h=min(m$val[2,1,]),lty=2)
      dev.off()
    }

    errors=rep(0,length(eval(parse(text=numvarss[x]))))
    for (i in 1:length(eval(parse(text=numvarss[x])))){
      pcr.pred=predict(pcr.fit, newdata=eval(parse(text=paste0("test_",sets[z],collapse='')))[c(TARGET,eval(parse(text=numvarss[x])))], ncomp=i)
      print(mean((aperm(array(eval(parse(text=paste0("test_",sets[z],"$",TARGET,collapse=''))),c(1,1,length(eval(parse(text=paste0("test_",sets[z],"$",TARGET,collapse='')))))))-pcr.pred)^2))
      errors[i]=mean((aperm(array(eval(parse(text=paste0("test_",sets[z],"$",TARGET,collapse=''))),c(1,1,length(eval(parse(text=paste0("test_",sets[z],"$",TARGET,collapse='')))))))-pcr.pred)^2)
    }
    print(errors[as.numeric(which(data.frame(pr.s$importance)[3,]>threshold)[1])])
  }
}

#kmeans
kt=1:5
bic=rep(0,5)
aicc=rep(0,5)
var_to_use=numvars #catvars not useful
d=length(colnames(data_clean[,var_to_use]))
for (z in 1:length(sets)){
  for (ii in kt){
    k=ii
    grp = kmeans(scale(eval(parse(text=paste0('train_',sets[z],collapse='')))[,var_to_use]), centers = k, nstart = 25)
    grp$cluster
    n=nrow(eval(parse(text=paste0('train_',sets[z],collapse='')))[,var_to_use])
    df=d*ii
    bic[ii] = grp$tot.withinss + log(n)*df
    aicc[ii] = grp$tot.withinss + 2*df*n/(n-df-1)
  }

  bicmin=which.min(bic)
  aiccmin=which.min(aicc)

  grpbic = kmeans(scale(eval(parse(text=paste0('train_',sets[z],collapse='')))[,var_to_use]), centers = bicmin, nstart = 25)
  grpbic$cluster
  #hierarchical clustering doesnt produce any meaningful result
  for (x in var_to_use){
    if (x %in% temp_kmeans){
      plot(eval(parse(text=paste0('train_',sets[z],'$',x,collapse=''))), eval(parse(text=paste0('train_',sets[z],'$',TARGET,collapse=''))), main = paste("bic K = ",bicmin), xlab=paste(x), ylab=paste(TARGET), type="p", col=rainbow(k)[grpbic$cluster])
      fname=paste(paste("kmeans-bic",x,z,sep="-"),".pdf",sep="")
      pdf(fname)
      plot(eval(parse(text=paste0('train_',sets[z],'$',x,collapse=''))), eval(parse(text=paste0('train_',sets[z],'$',TARGET,collapse=''))), main = paste("bic K = ",bicmin), xlab=paste(x), ylab=paste(TARGET), type="p", col=rainbow(k)[grpbic$cluster])
      dev.off()
    }
  }

  grpaicc = kmeans(scale(eval(parse(text=paste0('train_',sets[z],collapse='')))[,var_to_use]), centers = aiccmin, nstart = 25)
  grpaicc$cluster
  #hierarchical clustering doesnt produce any meaningful result
  for (x in var_to_use){
    if (x %in% temp_kmeans){
      plot(eval(parse(text=paste0('train_',sets[z],'$',x,collapse=''))), eval(parse(text=paste0('train_',sets[z],'$',TARGET,collapse=''))), main = paste("aicc K = ",aiccmin), xlab=paste(x), ylab=paste(TARGET), type="p", col=rainbow(k)[grpaicc$cluster])
      fname=paste(paste("kmeans-aicc",x,z,sep="-"),".pdf",sep="")
      pdf(fname)
      plot(eval(parse(text=paste0('train_',sets[z],'$',x,collapse=''))), eval(parse(text=paste0('train_',sets[z],'$',TARGET,collapse=''))), main = paste("aicc K = ",aiccmin), xlab=paste(x), ylab=paste(TARGET), type="p", col=rainbow(k)[grpaicc$cluster])
      dev.off()
    }
  }
}

#final models for evaluation
#the variables we use are: flattype, mature, exec_sold, LRT, NSL, EWL, TEL, DTL, floor_area_sqm, Dist_CBD,
#max_floor_lvl, total_dwelling_units, Remaining_lease

vars=c('flattype','mature','exec_sold','LRT','NSL','EWL','TEL','DTL','floor_area_sqm','Dist_CBD','max_floor_lvl',
       'total_dwelling_units','Remaining_lease')

glmfinal=glm(resale_price~flattype+mature+exec_sold+LRT+NSL+EWL+TEL+DTL+floor_area_sqm+Dist_CBD+max_floor_lvl+total_dwelling_units+Remaining_lease,train_STD,family='gaussian')
summary(glmfinal)
glmfinal.pred=predict(glmfinal,test_STD, type="response")
mean((test_STD$resale_price-glmfinal.pred)^2)
par(mfrow=c(2,2))
plot(glmfinal)
par(mfrow=c(1,1))
fname="GLM.pdf"
pdf(fname)
par(mfrow=c(2,2))
plot(glmfinal)
par(mfrow=c(1,1))
dev.off()

kernels=c('rectangular','gaussian','optimal','triangular','epanechnikov','inv','cos')
scaling=c(TRUE,FALSE)
for(x in kernels){
  for(y in scaling){
    set.seed(3)
    knnfin=train.kknn(resale_price~flattype+mature+exec_sold+LRT+NSL+EWL+TEL+DTL+floor_area_sqm+Dist_CBD+max_floor_lvl+total_dwelling_units+Remaining_lease,train_STD,ks=c(1:100), kernel = x,scale=y)
    plot(c(1:100),knnfin$MEAN.SQU, type="l", col = "blue", main="CV MSE", xlab="Complexity: K", ylab="MSE")
    fname=paste(paste("CV MSE","kernel",x,"scale",y,sep="-"),".pdf",sep="")
    pdf(fname)
    plot(c(1:100),knnfin$MEAN.SQU, type="l", col = "blue", main="CV MSE", xlab="Complexity: K", ylab="MSE")
    dev.off()
    kbest=knnfin$best.parameters$k
    print(kbest)
    knnpred=kknn(resale_price~flattype+mature+exec_sold+LRT+NSL+EWL+TEL+DTL+floor_area_sqm+Dist_CBD+max_floor_lvl+total_dwelling_units+Remaining_lease,train_STD,test_STD,k=kbest,kernel = x,scale=y)
    print(mean((test_STD$resale_price-knnpred$fitted.values)^2))
  }
}

#cannot set dists too large as it is very time consuming
dists=seq(1,2,by=0.05)
mses1=rep(0,21)
mses2=rep(0,21)
for(x in 1:length(dists)){
  knnpred1=kknn(resale_price~flattype+mature+exec_sold+LRT+NSL+EWL+TEL+DTL+floor_area_sqm+Dist_CBD+max_floor_lvl+total_dwelling_units+Remaining_lease,train_STD,test_STD,k=7,distance=as.numeric(dists[x]),kernel = "triangular",scale=TRUE)
  knnpred2=kknn(resale_price~flattype+mature+exec_sold+LRT+NSL+EWL+TEL+DTL+floor_area_sqm+Dist_CBD+max_floor_lvl+total_dwelling_units+Remaining_lease,train_STD,test_STD,k=7,distance=as.numeric(dists[x]),kernel = "inv",scale=TRUE)
  mses1[x]<-mean((test_STD$resale_price-knnpred1$fitted.values)^2)
  mses2[x]<-mean((test_STD$resale_price-knnpred2$fitted.values)^2)
}
plot(dists,mses1,type='l',col='red', main="MSEs vs Distance",lty=1,ylab='mses', cex.axis=2,cex.lab=2,cex.main=2)
par(new=TRUE)
plot(dists,mses2,type='l',col='blue', xaxt="n",yaxt="n",ylab="",xlab="",lty=2)
legend("topleft", c("Triangular","Inv"),col=c("red","blue"),lty=c(1,2),cex=2)
par(new=FALSE)
fname="MSE vs Distance.pdf"
pdf(fname)
plot(dists,mses1,type='l',col='red', main="MSEs vs Distance",lty=1,ylab='mses',cex.axis=2,cex.lab=2,cex.main=2)
par(new=TRUE)
plot(dists,mses2,type='l',col='blue', xaxt="n",yaxt="n",ylab="",xlab="",lty=2)
legend("topleft", c("Triangular","Inv"),col=c("red","blue"),lty=c(1,2),cex=2)
par(new=FALSE)
dev.off()
mses1[1]
mses2[1]
#surprisingly distance=1 works better than the default distance=2

means=rep(0,length(town))
sds=rep(0,length(town))
for(x in 1:length(town)){
  means[x]<-mean(data_clean[data_clean$town==x,]$resale_price)
  sds[x]<-sd(data_clean[data_clean$town==x,]$resale_price)
}
par(mar = c(5, 5, 3, 5))
plot(c(1:length(town)),means,type='l',col='red',main='mean and sd of resale price by town',xlab='towns',ylab='mean',lty=1,cex.axis=1.5,cex.lab=1.5,cex.main=1.5)
par(new=TRUE)
plot(c(1:length(town)),sds,type='l',col='blue',xaxt='n',yaxt='n',ylab='',xlab='',lty=2,cex.axis=1.5,cex.lab=1.5,cex.main=1.5)
axis(side=4)
text('')
mtext("sd",side=4,line=3,cex=1.5)
legend("topleft", c("Mean","SD"),col=c("red","blue"),lty=c(1,2),cex=1.5)
par(new=FALSE)
fname="Mean and SD.pdf"
pdf(fname)
par(mar = c(5, 5, 3, 5))
plot(c(1:length(town)),means,type='l',col='red',main='mean and sd of resale price by town',xlab='towns',ylab='mean',lty=1,cex.axis=1,cex.lab=1,cex.main=1)
par(new=TRUE)
plot(c(1:length(town)),sds,type='l',col='blue',xaxt='n',yaxt='n',ylab='',xlab='',lty=2,cex.axis=1,cex.lab=1,cex.main=1)
axis(side=4)
text('')
mtext("sd",side=4,line=3,cex=1)
legend("topleft", c("Mean","SD"),col=c("red","blue"),lty=c(1,2),cex=1)
par(new=FALSE)
dev.off()

#housing CPI increased by 5.1% from 2021 to 2022, according to singstat. we account for this in our estimate.
#we use the example of a 5 room flat at Blk 101 Bishan St 12 S570101 taken from https://services2.hdb.gov.sg/web/fi10/emap.html
#in particular, we look at one that was sold in Oct 2022.
#flattype=5, mature=1, exec_sold=8, LRT=0, NSL=1, EWL=0, TEL=0, DTL=0, floor_area_sqm=121.00, Dist_CBD=7,
#max_floor_lvl=24, total_dwelling_units=92, Remaining_lease=64
#some variables are estimated, such as Dist_CBD, which we used Google Maps for, and defined distance as from the HDB to City Hall MRT.
#max_floor_lvl is estimated based on the highest storey number we observe in the data from the site

instance=data.frame(flattype=c(5),mature=c(1),exec_sold=c(8),LRT=c(0),NSL=c(1),EWL=c(0),TEL=c(0),DTL=c(0),
                    floor_area_sqm=c(121),Dist_CBD=c(7),max_floor_lvl=c(24),total_dwelling_units=c(92),Remaining_lease=c(64))

knnpredfinal=kknn(resale_price~flattype+mature+exec_sold+LRT+NSL+EWL+TEL+DTL+floor_area_sqm+Dist_CBD+max_floor_lvl+total_dwelling_units+Remaining_lease,train_STD,instance,k=7,distance=1,kernel = "triangular",scale=TRUE)
knnprediction=predict(knnpredfinal)
knnprediction
knnpredictionraw=knnprediction*attr(data_std$resale_price,'scaled:scale')+attr(data_std$resale_price,'scaled:center')
knnpredictionraw
knnpredictionrawcorrected=knnpredictionraw*1.051
knnpredictionrawcorrected
#after correction for inflation, we get a final price predicted of 855396.1, which is close to the actual value of 880000

#can consider splitting resale_price by various income levels as well, e.g. compare with median monthly salary?
#may be hard to implement and interpret, but worth a try for future work.
