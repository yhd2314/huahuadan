######COX
getwd()
library("survival")
library("survminer")
library(rms)



# #多因素cox完成
# coxstomach=coxph(Surv(Survival.months, status_code1111) ~ Age+Grade+Nstage+Radiotherapy+
#                    RNE,data=stomach)
# summary(coxstomach)
# exp(confint(coxstomach,level = 0.9))
# sum.surv <- summary(coxstomach)
# c_index <- sum.surv$concordance
# c_index
# #####接下里应用逐步回归方案筛选最佳模型
# cox_step=step(coxstomach,direction = "both")
# summary(cox_step)
# exp(confint(cox_step,level = 0.9))

# ####然后看一下逐步回归过后的模型的c-index
# sum.surv <- summary(cox_step)
# c_index <- sum.surv$concordance
# c_index
# ####看一下逐步回归后的模型的95%的置信区间。
# exp(confint(cox_step,level = 0.9))
####Cox nomogram图,只做多因素COX分析中统计学意义的变量
table(stomach$`Derived.AJCC.M,.7th.ed.(2010-2015)`)
table(stomach$Sex)
# stomach$Sex[stomach$Sex=="Female"]="0"
# stomach$Sex[stomach$Sex=="Male"]="1"
# stomach$Sex=as.numeric(stomach$Sex)
table(stomach$Age)
# 
# stomach$Age[stomach$Age=="<50"]="0"
# stomach$Age[stomach$Age=="50-60"]="1"
# stomach$Age[stomach$Age==">=60"]="2"
# stomach$Age=as.numeric(stomach$Age)

table(stomach$RNE)
# stomach$RNE[stomach$RNE=="<15"]="0"
# stomach$RNE[stomach$RNE=="≥15"]="1"
# stomach$RNE=as.numeric(stomach$RNE)
# table(stomach$Tumor.size)
# stomach$Tumor.size[stomach$Tumor.size=="≥989mm"]="0"
# stomach$Tumor.size[stomach$Tumor.size=="2-988mm"]="1"
# stomach$Tumor.size=as.numeric(stomach$Tumor.size)
table(stomach$Grade)
# stomach$Grade[stomach$Grade=="G1"]="1"
# stomach$Grade[stomach$Grade=="G2"]="2"
# stomach$Grade[stomach$Grade=="G3"]="3"
# stomach$Grade[stomach$Grade=="G4"]="4"
# stomach$Grade=as.numeric(stomach$Grade)
# table(stomach$Chemotherapy)
table(stomach$Radiotherapy)
table(stomach$Rad.saq)
# stomach$Radiotherapy[stomach$Radiotherapy=="no"]="0"
# stomach$Radiotherapy[stomach$Radiotherapy=="Yes"]="1"
# stomach$Radiotherapy=as.numeric(stomach$Radiotherapy)
# table(stomach$Rad.saq)
# stomach=subset(stomach,stomach$Rad.saq!="Surgery both before and after radiation")
# stomach=subset(stomach,stomach$Rad.saq!="Radiation before and after surgery")
# stomach$Rad.saq[stomach$Rad.saq=="No radiation and/or cancer-directed surgery"]="NoRT"
# stomach$Rad.saq[stomach$Rad.saq=="Radiation after surgery"]="SRT"
# stomach$Rad.saq[stomach$Rad.saq=="Radiation prior to surgery"]="RST"
# stomach$Rad.saq[stomach$Rad.saq=="NoRT"]="0"
# stomach$Rad.saq[stomach$Rad.saq=="RST"]="2"
# stomach$Rad.saq[stomach$Rad.saq=="SRT"]="1"
# stomach$Rad.saq=as.numeric(stomach$Rad.saq)
# table(stomach$Rad.saq)
# table(stomach$MLR)
table(stomach$Survival.months)



################开始筛选剔除T1-2N0的变量
stomach=subset(stomach,stomach$Tstage!="T1")
stomach=subset(stomach,stomach$Tstage!="T2")
stomach=subset(stomach,stomach$Tstage!="TX")
# stomach$Tstage[stomach$Tstage=="T3"]="0"
# stomach$Tstage[stomach$Tstage=="T4"]="1"
# stomach$Tstage=as.numeric(stomach$Tstage)
table(stomach$Tstage)

table(stomach$Nstage)
stomach=subset(stomach,stomach$Nstage!="N0")
# # stomach$Nstage[stomach$Nstage=="N0"]="0"
# stomach$Nstage[stomach$Nstage=="N1"]="1"
# stomach$Nstage[stomach$Nstage=="N2"]="2"
# stomach$Nstage[stomach$Nstage=="N3"]="3"
# stomach$Nstage=as.numeric(stomach$Nstage)
table(stomach$Nstage)
# stomach$Tstage[stomach$Tstage=="T1"]="1"
# stomach$Tstage[stomach$Tstage=="T2"]="2"



#####手术信息
# index30=which(stomach$Surgery.method<30)
# index50=which(30<=stomach$Surgery.method & stomach$Surgery.method<50)
# index60=which(50<=stomach$Surgery.method & stomach$Surgery.method<80)
# index80=which(stomach$Surgery.method>=80)
# stomach$Surgery.method[index30]='Local tumor destruction'
# stomach$Surgery.method[index50]='Gastrectomy1'
# stomach$Surgery.method[index60]='Gastrectomy2'
# stomach$Surgery.method[index80]='aSurgery_NOS'
# stomach=subset(stomach,stomach$Surgery.method!="aSurgery_NOS")
# stomach=subset(stomach,stomach$Surgery.method!="Local tumor destruction")
# stomach$Surgery.method[stomach$Surgery.method=="Gastrectomy1"]="1"
# stomach$Surgery.method[stomach$Surgery.method=="Gastrectomy2"]="2"
# stomach$Surgery.method=as.numeric(stomach$Surgery.method)
# table(stomach$Surgery.method)

dd <- datadist(stomach)
options(datadist="dd")
cph <- cph(Surv(Survival.months, status_code1111) ~ Age+Grade+Nstage+Radiotherapy+
             RNE,data=stomach,x = TRUE,
           y = TRUE, surv = TRUE)

####作图
survival <- Survival(cph)
survival1 <- function(x) survival(12, x)
survival2 <- function(x) survival(3*12, x)
survival3 <- function(x) survival(5*12, x)
nom1 <- nomogram(cph, fun = list(survival1, survival2,survival3), lp=F,
                 fun.at = c(0.05, seq(0.1,0.9, by = 0.1), 0.95), funlabel = c("1 year survival", "3 year survival","5 year survival"))
plot(nom1)
par(mfrow = c(1,3))#####前面是行，后边是列
####COX的Clibration plot校正曲线
fstomach=cph(formula = Surv(Survival.months, status_code1111) ~Age+Grade+Nstage+Radiotherapy+
               RNE,
      data=stomach,y=T,x=T,surv = T,na.action=na.delete,time.inc = 1*12) 

cal=calibrate(fstomach, cmethod="KM", method="boot",u=1*12,m=50,B=1000) 
#参数m=1600表示每组1600个样本进行重复计算，作图，点数跟m的取值有关
par(mar=c(8,5,3,2),cex = 1.0)
plot(cal,lwd=2,lty=1,
     errbar.col=c(rgb(0,118,192,maxColorValue=255)),
     xlim=c(0,1),ylim=c(0,1),
     xlab="Nomogram-Predicted Probability of 1-year overall survival",
     ylab="Actual 1-year overall survival",
     col=c(rgb(192,98,83,maxColorValue=255)))

##########################################3年的
fstomach=cph(formula = Surv(Survival.months, status_code1111) ~Age+Grade+Nstage+Radiotherapy+
               RNE,
             data=stomach,y=T,x=T,surv = T,na.action=na.delete,time.inc = 3*12) 
cal=calibrate(fstomach, cmethod="KM", method="boot",u=3*12,m=50,B=1000) 
par(mar=c(8,5,3,2),cex = 1.0)
plot(cal,lwd=2,lty=1,
     errbar.col=c(rgb(0,118,192,maxColorValue=255)),
     xlim=c(0,1),ylim=c(0,1),
     xlab="Nomogram-Predicted Probability of 3-year overall survival",
     ylab="Actual 3-year overall survival",
     col=c(rgb(192,98,83,maxColorValue=255)))


################################################5年的
fstomach=cph(formula = Surv(Survival.months, status_code1111) ~Age+Grade+Nstage+Radiotherapy+
               RNE,
             data=stomach,y=T,x=T,surv = T,na.action=na.delete,time.inc = 5*12) 
cal=calibrate(fstomach, cmethod="KM", method="boot",u=5*12,m=50,B=1000) 
par(mar=c(8,5,3,2),cex = 1.0)
plot(cal,lwd=2,lty=1,
     errbar.col=c(rgb(0,118,192,maxColorValue=255)),
     xlim=c(0,1),ylim=c(0,1),
     xlab="Nomogram-Predicted Probability of 5-year overall survival",
     ylab="Actual 5-year overall survival",
     col=c(rgb(192,98,83,maxColorValue=255)))
