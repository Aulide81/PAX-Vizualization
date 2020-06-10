setwd("F://MDS/Visualizacion de datos/pac3/")
df<-read.csv("pax_data_1832_agreements_14-04-20.csv",stringsAsFactors=F)
names(df)[1]<-"Con"


Con<-strsplit(df$Con,"/")
Con<-lapply(Con,function(k){
  gsub("[()]","",k)
})

paises<-unique(unlist(Con))
matriz<-sapply(Con,function(k){
  (paises%in%k)*1
})

matriz<-t(matriz)
colnames(matriz)<-paises
matriz<-matriz[,!colnames(matriz)%in%c("RPA","ABB")]
df2<-data.frame(df,matriz,stringsAsFactors = F)
names(df2)[(ncol(df)+1):(ncol(df)+ncol(matriz))]<-colnames(matriz)

referencias<-list(
  
  "Humans Rights"=c("HrGen","EqGen","HrDem","Prot","HrFra","HrCp",
                    "HrSec","HrNi","HrIi","HrMob","HrDet","Med","HrCit"),
  
  "Security Sector"=c("SsrGua","Ce","SsrPol","SsrArm",
                      "SsrDdr","SsrInt","SsrPsf","SsrFf","Cor",
                      "SsrCrOcr","SsrDrugs","Terr"),
  
  "Governance"=c("Pol","ConRen","Cons","Ele","ElecComm",
                 "PolPar","Civso","Tral","Pubad"),
  
  "Socio Economic"=c("Dev","NEC","NatRes","IntFu","Bus","Tax","Ban"),
  
  "Land"=c("LaRef","LaNom","LaCH","LaEn","Wat"),
  
  "Transitional Justice"=c("TjGen","TjAm","TjCou","TjMech",
                           "TjPrire","TjVet","TjVic","TjMis",
                           "TjRep","TjNR"),
  
  "Groups"=c("GCh","GDis","GAge","GMig","GRa",
             "GRe","GInd","GOth","GRef","GSoc"),
  
  "Gender"=c("GeWom","GeMe","GeLgbti","GeFa"),
  
  "State"=c("StDef"),
  
  "Powersharing"=c("Polps","Terps","Eps","Mps"),
  
  "Justice Sector Reform"=c("JusCr","JusEm","JusJu","JusPri","JusTra"))

matriz_referencias<-sapply(referencias,function(k){
  rowSums(df[,k,drop=F])
})

matriz_referencias[matriz_referencias>1]<-1

df2<-cbind(df2,matriz_referencias)

var_int<-c('Con','PPName','Reg','AgtId','Dat','Contp','Status','Agtp',
           'Stage','N_characters','Lgt',colnames(matriz),colnames(matriz_referencias))

df2<-df2[,var_int]

df3<-reshape2::melt(df2,id.vars=var_int[c(1:11,158:168)])
df3<-df3[df3$value>0,]
df3$value<-NULL
names(df3)[names(df3)=="variable"]<-"country"

df3<-reshape2::melt(df3,id.vars=names(df3)[c(1:11,23)])
df3<-df3[df3$value>0,]
df3$value<-NULL
names(df3)[names(df3)=="variable"]<-"topic"

df3$unicos<-!duplicated(df3$AgtId)

openxlsx::write.xlsx(df3,"datos.xlsx")




df$year<-lubridate::ymd(df$Dat)
df$year<-lubridate::year(df$Dat)
df$Total<-1
df[,c("PPName","Lgt","N_characters","year")]

df[df$PPName=="Afghanistan: 2000s Post-intervention process",c("Lgt","N_characters","year")]
df<-df[order(df$year),]


library(dplyr)
progreso<-df%>%group_by(PPName)%>%select(Lgt,N_characters,Total)%>%mutaste_all(cumsum)
year<-df%>%group_by(PPName)%>%select(year)
progreso$year<-unlist(year[,2])

progreso<-progreso%>%group_by(PPName,year)%>%select(Lgt,N_characters,Total)%>%summarise_all(max)

procesos<-unique(progreso$PPName)
inicio<-data.frame(procesos,1989,0,0,0)
names(inicio)<-names(progreso)
progreso<-bind_rows(progreso,inicio)
progreso<-progreso[order(progreso$PPName,progreso$year),]
rm(inicio)

time_line<-data.frame(PPName="inicio",year=NA,Lgt=NA,N_characters=NA,Total=NA,stringsAsFactors = F)
procesos<-unique(progreso$PPName)
anos<-c(1989:2019)

#i<-procesos[1]
#j<-anos[2]
for (i in procesos){
  for (j in anos){
    test<-progreso[progreso$PPName==i & progreso$year==j,]
    if(nrow(test)==0){
      adjuntar<-time_line[time_line$PPName==i & time_line$year==j-1,]
      adjuntar$year<-j
      time_line<-bind_rows(time_line,adjuntar)
    }else{
      time_line<-bind_rows(time_line,test)
    }
    
  }
}

ztime_line<-time_line[-1,]

a<-df$Reg
names(a)<-df$PPName
a<-a[time_line$PPName]

time_line$Reg<-a
openxlsx::write.xlsx(time_line,"progreso.xlsx")