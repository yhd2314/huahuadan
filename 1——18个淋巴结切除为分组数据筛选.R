#
getwd()
library(readxl)
#setwd("/Users/tianchenrui/Desktop/放疗联合化疗vs化疗seer数据结果/原始数据/")
## 安装openxlsx包
# install.packages("openxlsx")

# 加载openxlsx包
library(openxlsx)

# 读取Excel文件
seer <- read.xlsx("/Users/tianchenrui/Desktop/放疗联合化疗vs化疗seer数据结果/放化疗/数据库结果/seer.xlsx")


##种族
table(seer$`Race.recode.(White,.Black,.Other)`)
seer$Race=seer$`Race.recode.(White,.Black,.Other)`
seer=subset(seer,seer$`Race.recode.(White,.Black,.Other)`!='Unknown')
table(seer$Race)
seer$Race[seer$Race=="Other (American Indian/AK Native, Asian/Pacific Islander)"]="Other"
##年龄
table(seer$`Age.recode.with.<1.year.olds`)
seer$Age=seer$`Age.recode.with.<1.year.olds`
index50=which(seer$Age<=50)
index60=which(50<seer$Age & seer$Age<60)
index70=which(seer$Age>=60)
seer$Age[index50]='<50'
seer$Age[index60]='50-60'
seer$Age[index70]='>=60'
table(seer$Age)
##性别
table(seer$Sex)
####病理类型
table(seer$`Histology.recode.-.broad.groupings`)
seer$Histology=seer$`Histology.recode.-.broad.groupings`
seer$Histology[seer$Histology=="8050-8089: squamous cell neoplasms"]="SECC"
seer$Histology[seer$Histology=="8140-8389: adenomas and adenocarcinomas"]="AEC"
table(seer$Histology)
##T分期
table(seer$`Derived.AJCC.T,.7th.ed.(2010-2015)`)
seer=subset(seer,seer$`Derived.AJCC.T,.7th.ed.(2010-2015)`!='Blank(s)')
seer=subset(seer,seer$`Derived.AJCC.T,.7th.ed.(2010-2015)`!='T0')
seer=subset(seer,seer$`Derived.AJCC.T,.7th.ed.(2010-2015)`!='Tis')
seer=subset(seer,seer$`Derived.AJCC.T,.7th.ed.(2010-2015)`!='TX')

seer$Tstage=seer$`Derived.AJCC.T,.7th.ed.(2010-2015)`
seer$Tstage[seer$Tstage=="T1"]="T1"
seer$Tstage[seer$Tstage=="T1a"]="T1"
seer$Tstage[seer$Tstage=="T1b"]="T1"
seer$Tstage[seer$Tstage=="T1NOS"]="T1"
seer$Tstage[seer$Tstage=="T4"]="T4"
seer$Tstage[seer$Tstage=="T4a"]="T4"
seer$Tstage[seer$Tstage=="T4b"]="T4"
seer$Tstage[seer$Tstage=="T4NOS"]="T4"
table(seer$Tstage)
##N分期
table(seer$`Derived.AJCC.N,.7th.ed.(2010-2015)`)
seer=subset(seer,seer$`Derived.AJCC.N,.7th.ed.(2010-2015)`!='NX')
seer$Nstage=seer$`Derived.AJCC.N,.7th.ed.(2010-2015)`
seer$Nstage[seer$Nstage=="N3a"]="N3"
seer$Nstage[seer$Nstage=="N3b"]="N3"
seer$Nstage[seer$Nstage=="N3NOS"]="N3"
table(seer$Nstage)
##M分期
table(seer$`Derived.AJCC.M,.7th.ed.(2010-2015)`)
seer=subset(seer,seer$`Derived.AJCC.M,.7th.ed.(2010-2015)`!='M1')

##生存时间
table(seer$Survival.months)
seer$time=seer$Survival.months
seer=subset(seer,seer$Survival.months!="0")
#####化疗
seer$Chemotherapy=seer$`Chemotherapy.recode.(yes,.no/unk)`

####放疗
####rm(p)这个可以剔除中间某一种不想要的列明
table(seer$Radiation.recode)
##seer2<- seer###备份一下
k<- seer$Radiation.recode##赋予一个中间变量尝试修改
seer$Radiotherapy<- ifelse(k == 'None/Unknown'|k=='Recommended, unknown if administered'|k=='Refused (1988+)'
                           ,'no','Yes' )
table(seer$Radiotherapy)
######放疗顺序
seer$Rad.saq=seer$`RX.Summ--Surg/Rad.Seq`
table(seer$Rad.saq)

seer=subset(seer,seer$Rad.saq!="Sequence unknown, but both were given")

#####部位
table(seer$`Primary.Site.-.labeled`)
seer$Primary.site=seer$`Primary.Site.-.labeled`
seer$Primary.site[seer$Primary.site=="C15.5-Lower third of esophagus"]="Esophagus"
seer$Primary.site[seer$Primary.site=="C15.9-Esophagus, NOS"]="Esophagus"
seer$Primary.site[seer$Primary.site=="C16.0-Cardia, NOS"]="Cardia"
seer$Primary.site[seer$Primary.site=="C16.1-Fundus of stomach"]="Stomach"
seer$Primary.site[seer$Primary.site=="C16.2-Body of stomach"]="Stomach"
seer$Primary.site[seer$Primary.site=="C16.9-Stomach, NOS"]="Stomach"
table(seer$Primary.site)

####手术:0表示没做手术，90表示做了手术，但是不知道具体信息
##00是没有做手术，99是不知道做手术了没有
table(seer$`RX.Summ--Surg.Prim.Site.(1998+)`)
seer$Surgery=seer$`RX.Summ--Surg.Prim.Site.(1998+)`
seer$Surgery.method=seer$Surgery

###分级
seer$Grade=seer$`Grade.(thru.2017)`
table(seer$Grade)
seer$Grade[seer$Grade=="Well differentiated; Grade I"]="G1"
seer$Grade[seer$Grade=="Moderately differentiated; Grade II"]="G2"
seer$Grade[seer$Grade=="Poorly differentiated; Grade III"]="G3"
seer$Grade[seer$Grade=="Undifferentiated; anaplastic; Grade IV"]="G4"
table(seer$Grade)

####肿瘤大小
table(seer$`CS.tumor.size.(2004-2015)`)
seer=subset(seer,seer$`CS.tumor.size.(2004-2015)`!='999')
seer=subset(seer,seer$`CS.tumor.size.(2004-2015)`!='990')
seer=subset(seer,seer$`CS.tumor.size.(2004-2015)`!='998')
seer$Tumor.size=seer$`CS.tumor.size.(2004-2015)`
index1=which(seer$Tumor.size<=1)
index2=which(2<=seer$Tumor.size & seer$Tumor.size<=988)
index989=which(seer$Tumor.size>=989)
seer$Tumor.size[index1]='≤1mm'
seer$Tumor.size[index2]='2-988mm'
seer$Tumor.size[index989]='≥989mm'
table(seer$Tumor.size)

#####切除淋巴结数
table(seer$`Regional.nodes.examined.(1988+)`)
seer$RNE=seer$`Regional.nodes.examined.(1988+)`
index18=which(seer$RNE<18)
index19=which(18<=seer$RNE)
seer$RNE[index18]='<18'
seer$RNE[index19]='≥18'
table(seer$RNE)

#####MLR淋巴结阳性转移率
#install.packages("scales")
library(scales)

seer$MLR <- percent(seer$MLR)
print(seer$MLR)
table(seer$MLR)
##导出清洗后的数据

# write.table (stomach, file ="/Users/tianchenrui/Desktop/放疗联合化疗vs化疗seer数据结果/原始数据/stomach.csv", sep =",", row.names =FALSE)
##############################################seer是清洗以及筛选后的
table(seer$Primary.site)


stomach=seer#####胃区
stomach=subset(stomach,stomach$Primary.site!="Cardia")
stomach=subset(stomach,stomach$Primary.site!="Esophagus")

stomach$N_stage=stomach$Nstage
stomach$N_stage[stomach$N_stage=="N3a"]="N3"
stomach$N_stage[stomach$N_stage=="N3b"]="N3"
stomach$N_stage[stomach$N_stage=="N3NOS"]="N3"
stomach=subset(stomach,stomach$N_stage!="N3NOS")
stomach=subset(stomach,stomach$N_stage!="NX")
table(stomach$Histology)
stomach=subset(stomach,stomach$Histology!="SECC")
stomach$status_code1111=ifelse(stomach$`Vital.status.recode.(study.cutoff.used)`=='Alive'
                               ,0,1)
table(stomach$RNE)
