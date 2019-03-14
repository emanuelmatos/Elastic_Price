

setwd("~/Price_Elast/Segmento_2_125K/F7_BASE/Base")

library(readxl)
estados <- read_excel("F_estados.xlsx")

#install.packages("dplyr")
library(dplyr)
library(readxl)
library(openxlsx)
library(readr)



####################################################################################################
#Tabela de Minimo Cliente

library(readr)
setwd("~/Price_Elast/Segmento_2_125K/F7_BASE/y.output")
res1<- read_csv(paste0("res_part_0_1.csv"))
res2<- read_csv(paste0("res_part_0_2.csv"))
res3<- read_csv(paste0("res_part_0_3.csv"))
res4<- read_csv(paste0("res_part_0_4.csv"))
res5<- read_csv(paste0("res_part_0_5.csv"))

results<-rbind(res1,res2,res3,res4,res5)
results<-results[,]

#####################################

library(readr)
setwd("~/Price_Elast/Segmento_2_125K/F7_BASE")
tabela<- read_csv("tabela_0p.csv")
tabela<-tabela[,-c(2)]
###################################################################

id_tarifa<-as.numeric(as.factor(tabela$tarifa))
id_f_peso<-as.numeric(as.factor(tabela$f_peso))

tabela<-cbind(id_tarifa,id_f_peso,tabela)

max(id_tarifa)
max(id_f_peso)

id_res<-paste0(tabela$tarifa,tabela$f_peso)
tabela<-cbind(id_res,tabela)

########################################################

results$taxa_desc<-as.numeric(as.character(results$taxa_desc))
results$Preco_Cliente<-as.numeric(as.character(results$Preco_Cliente))
results$Preco_Base<-as.numeric(as.character(results$Preco_Base))
results$Qdt_extra<-as.numeric(as.character(results$Qdt_extra))
results$Net<-as.numeric(as.character(results$Net))
calc1<-results$Preco_Base*(1-results$taxa_desc/100)

id_res<-paste0(results$tarifa,results$faixa_peso)
#id_res<-as.numeric(as.factor(id_res))


results<-cbind(id_res,results,calc1)

#results_net<-results[results$Net>0&results$faixa_peso=='0.0 - 0.25'&results$tarifa=='SP_C1',]



#################################################################################

results1<-results[results$Net>0&results$Qdt_extra>0&results$calc1<=results$Preco_Cliente,]


tab_f<-tab_f2<-NULL


id_res<-as.array(unique(as.character(results1$id_res)))
nid_res2<-as.numeric(as.factor(id_res))
nid_res3<-seq(1:max(nid_res2))
nid_res4<-as.data.frame(cbind(id_res,nid_res2))

results1<- merge(results1,nid_res4,'id_res',all.x=TRUE)


for (i in 1:max(nid_res2))
{ 
  res<-results1[results1$nid_res2==i,]
  
  net_max<-max(res[,'Net'])
  prec_tab<-res[res$Net==net_max,'calc1']
  tab<-res[res$Net==net_max,]
  
  tab_f<-cbind(tab,prec_tab)
  tab_f2<-rbind(tab_f,tab_f2)
  
}


########################################################################

tabela_valida<-as.data.frame(unique(tab_f2$id_res))
colnames(tabela_valida)<-c('id_res')
qtd_valida<-1

tabela_valida<-cbind(tabela_valida,qtd_valida)
tabela_valida<-merge(tabela_valida,tab_f2[,c(1,13)],'id_res',all.x=TRUE)

tab_res<-merge(tabela,tabela_valida,'id_res',all.x=TRUE)

tab_res[is.na(tab_res)] <- 0
tab_res1<-tab_res[tab_res$qtd_valida==1,]
tab_res2<-tab_res[tab_res$qtd_valida==0,]


####################### Tabela Preço part2
tab_res2$prec_tab<-tab_res2$custo*1.30


tabela_final<-rbind(tab_res1,tab_res2)


tab_prec_final<-tabela_final[,c('id_res','tarifa','f_peso','qtd','prec_tab')]

tab_prec_final<-unique(tab_prec_final)

id_res<-as.array(unique(as.character(tab_prec_final$id_res)))
nid_res2<-as.numeric(as.factor(id_res))
nid_res3<-seq(1:max(nid_res2))
nid_res4<-as.data.frame(cbind(id_res,nid_res2))

tab_prec_final<- merge(tab_prec_final,nid_res4,'id_res',all.x=TRUE)

tab_f<-tab_f2<-NULL

for (i in 1:max(nid_res2))
{ 
  
  res<-tab_prec_final[tab_prec_final$nid_res2==i,]
  
  
  prec_tab<-sum(res[,'prec_tab']*res[,'qtd'])/sum(res[,'qtd'])
  tab<-unique(res[,c('id_res','tarifa','f_peso')])
  
  tab_f<-cbind(tab,prec_tab)
  tab_f2<-rbind(tab_f,tab_f2)
  
}

tabela_mandae<-as.data.frame(tab_f2)

library(tidyr)
tabela_mandae<-separate(tabela_mandae,tarifa,into=c( "UF" , "Local" ),sep="_")

tarifa<-paste0(tabela_mandae$UF,"_",tabela_mandae$Local)

tabela_mandae<-cbind(tarifa,tabela_mandae)

tabela_mandae<-merge(tabela_mandae,estados[,c(1,5)],'UF',all.x=TRUE)

setwd("~/Price_Elast/Segmento_2_125K/F7_BASE/z.tabelas")
write.xlsx(tabela_mandae,"tabela_mandae_min.xlsx",sheetName="Sheet1",col.names=TRUE, row.names=FALSE)


################################################
###############################################
################################################################################
################## Cliente 1



setwd("~/Price_Elast/Segmento_2_125K/F7_BASE/Cliente_1")
tabela3<- read_csv(paste0("tabela_CL1_0p.csv"))
tabela<-tabela3
tabela<-tabela[,-c(1)]

setwd("~/Price_Elast/Segmento_2_125K/F7_BASE/z.tabelas")
library(readxl)
tab_final <- read_excel("tabela_mandae_min.xlsx")
id_res<-paste0(tab_final$tarifa,tab_final$f_peso)
tab_final<-cbind(id_res,tab_final)


id_tarifa<-as.numeric(as.factor(tabela$tarifa))
id_f_peso<-as.numeric(as.factor(tabela$f_peso))

tabela<-cbind(id_tarifa,id_f_peso,tabela)

max(id_tarifa)
max(id_f_peso)

id_res<-paste0(tabela$tarifa,tabela$f_peso)
tabela<-cbind(id_res,tabela)


tab_prec<-merge(tabela,tab_final[,c('id_res','prec_tab')],'id_res',all.x=TRUE)

tab_prec[is.na(tab_prec)] <- 0

tab_prec$minimo<-tab_prec$minimo/1


#############################################################################
TIPO<-PEDIDOS<-GSV<-CUSTO<-NET<-PERC_NET<-DIF<-PERC_T<-dat<-datf<-0


PEDIDOS<-sum(tab_prec$qtd_original)
GSV_ORI_MAND<-sum(tab_prec$qtd_original*tab_prec$preco)
GSV<-sum(tab_prec$qtd_original*tab_prec$preco)
CUSTO<-sum(tab_prec$qtd_original*tab_prec$custo)
NET<-sum(tab_prec$qtd_original*tab_prec$preco)-sum(tab_prec$qtd_original*tab_prec$custo)
PERC_NET<-(NET/sum(tab_prec$qtd_original*tab_prec$preco))*100
TIPO<-c("Tabela Mandae Original")

dat<-as.data.frame(cbind(TIPO,PEDIDOS,GSV,CUSTO,NET,PERC_NET,DIF,PERC_T))



TIPO<-PEDIDOS<-GSV<-CUSTO<-NET<-PERC_NET<-DIF<-PERC_T<-0

PEDIDOS<-sum(tab_prec$qtd_original)
GSV_MIN_TRANSP<-sum(tab_prec$qtd_original*tab_prec$minimo)
GSV<-sum(tab_prec$qtd_original*tab_prec$minimo)
DIF<-GSV_ORI_MAND-GSV_MIN_TRANSP
PERC_T<-round(DIF/GSV_MIN_TRANSP*100,2)
TIPO<-c("Tabela Minima Transportadoras")

dat2<-as.data.frame(cbind(TIPO,PEDIDOS,GSV,CUSTO,NET,PERC_NET,DIF,PERC_T))


#######################################################################

tab0<-tab_prec[tab_prec$preco<=tab_prec$minimo&tab_prec$qtd_original>0,]

TIPO<-PEDIDOS<-GSV<-CUSTO<-NET<-PERC_NET<-DIF<-PERC_T<-0

PEDIDOS<-sum(tab0$qtd_original)
GSV_ORI_MAND<-sum(tab0$qtd_original*tab0$preco)
GSV<-sum(tab_prec$qtd_original*tab_prec$preco)
CUSTO<-sum(tab0$qtd_original*tab0$custo)
NET<-sum(tab0$qtd_original*tab0$preco)-sum(tab0$qtd_original*tab0$custo)
PERC_NET<-(NET/sum(tab0$qtd_original*tab0$preco))*100
TIPO<-c("Tabela Mandae Original Winner")

dat3<-as.data.frame(cbind(TIPO,PEDIDOS,GSV,CUSTO,NET,PERC_NET,DIF,PERC_T))



#----------------------------
TIPO<-PEDIDOS<-GSV<-CUSTO<-NET<-PERC_NET<-DIF<-PERC_T<-0

PEDIDOS<-sum(tab0$qtd_original)
GSV_MIN_TRANSP<-sum(tab0$qtd_original*tab0$minimo)
GSV<-sum(tab0$qtd_original*tab0$minimo)
DIF<-GSV_ORI_MAND-GSV_MIN_TRANSP
PERC_T<-round(DIF/GSV_MIN_TRANSP*100,2)
TIPO<-c("Tabela Minima Transportadoras Winner")

dat4<-as.data.frame(cbind(TIPO,PEDIDOS,GSV,CUSTO,NET,PERC_NET,DIF,PERC_T))


###############################################################################


tab1<-tab_prec[tab_prec$prec_tab<=tab_prec$minimo&tab_prec$qtd_original>0,]

TIPO<-PEDIDOS<-GSV<-CUSTO<-NET<-PERC_NET<-DIF<-PERC_T<-0


PEDIDOS<-sum(tab1$qtd_original)
GSV_ORI_MAND<-sum(tab1$qtd_original*tab1$prec_tab)
GSV<-sum(tab1$qtd_original*tab1$prec_tab)
CUSTO<-sum(tab1$qtd_original*tab1$custo)
NET<-sum(tab1$qtd_original*tab1$prec_tab)-sum(tab1$qtd_original*tab1$custo)
PERC_NET<-(NET/sum(tab1$qtd_original*tab1$prec_tab))*100
TIPO<-c("Tabela Otimizada Mandae")

dat5<-as.data.frame(cbind(TIPO,PEDIDOS,GSV,CUSTO,NET,PERC_NET,DIF,PERC_T))

#---------------------------
TIPO<-PEDIDOS<-GSV<-CUSTO<-NET<-PERC_NET<-DIF<-PERC_T<-0

PEDIDOS<-sum(tab1$qtd_original)
GSV_MIN_TRANSP<-sum(tab1$qtd_original*tab1$minimo)
GSV<-sum(tab1$qtd_original*tab1$minimo)
DIF<-GSV_ORI_MAND-GSV_MIN_TRANSP
PERC_T<-round(DIF/GSV_MIN_TRANSP*100,2)
TIPO<-c("Tabela Minima Transportadoras vs Otimizada")

dat6<-as.data.frame(cbind(TIPO,PEDIDOS,GSV,CUSTO,NET,PERC_NET,DIF,PERC_T))

dat_p1<-NULL
dat_p1<-rbind(dat,dat2,dat3,dat4,dat5,dat6)



PEDIDOS_TOTAL<-sum(tab_prec$qtd_original)
GSV_TOTAL_OTIMIZADO<-sum(tab_prec$qtd_original*tab_prec$prec_tab)
GSV_MINIMO<-sum(tab_prec$qtd_original*tab_prec$minimo)
dif<-GSV_TOTAL_OTIMIZADO-GSV_MINIMO
perc<-(dif/GSV_MINIMO)*100



cliente<-c("Cl1")

datf<-NULL

dat_<-cbind(cliente,dat_p1,PEDIDOS_TOTAL,GSV_TOTAL_OTIMIZADO,GSV_MINIMO,dif,perc)
datf<-rbind(dat_,datf)




setwd("~/Price_Elast/Segmento_2_125K/F7_BASE/y.output")
write.xlsx(datf,"tabela_resultados_CL1_segmento2_150_400_M.xlsx",sheetName="Sheet1",col.names=TRUE, row.names=FALSE)






