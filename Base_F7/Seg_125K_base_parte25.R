setwd("~/Price_Elast/Segmento_2_125K/F7_BASE/Base")

library(readxl)
F_estados <- read_excel("F_estados.xlsx")
#install.packages("dplyr")
library(dplyr)
library(readxl)
library(openxlsx)
library(readr)

desconto<-0.99

####################################################################################################
#Tabela de Minimo Cliente

library(readr)
setwd("~/Price_Elast/Segmento_2_125K/F7_BASE/y.output")
res1<- read_csv(paste0("res_part_5_1.csv"))
res2<- read_csv(paste0("res_part_5_2.csv"))
res3<- read_csv(paste0("res_part_5_3.csv"))
res4<- read_csv(paste0("res_part_5_4.csv"))
res5<- read_csv(paste0("res_part_5_5.csv"))

results<-rbind(res1,res2,res3,res4,res5)
results<-results[,]
##########################################

library(readr)
setwd("~/Price_Elast/Segmento_2_125K/F7_BASE")
tabela<- read_csv("tabela_5p.csv")
tabela<-tabela[,-c(2)]


id_tarifa<-as.numeric(as.factor(tabela$tarifa))
id_f_peso<-as.numeric(as.factor(tabela$f_peso))

tabela<-cbind(id_tarifa,id_f_peso,tabela)

max(id_tarifa)
max(id_f_peso)

id_res<-paste0(tabela$tarifa,tabela$f_peso)
tabela<-cbind(id_res,tabela)



#############################################################


###############################################################################
results$taxa_desc<-as.numeric(as.character(results$taxa_desc))
results$Preco_Cliente<-as.numeric(as.character(results$Preco_Cliente))
results$Preco_Base<-as.numeric(as.character(results$Preco_Base))
results$Qdt_extra<-as.numeric(as.character(results$Qdt_extra))
results$Net<-as.numeric(as.character(results$Net))
calc1<-results$Preco_Base*(1-results$taxa_desc/100)

id_res<-paste0(results$tarifa,results$faixa_peso)
#id_res<-as.numeric(as.factor(id_res))


results<-cbind(id_res,results,calc1)

#################################################################################

results1<-results[results$Qdt_extra>0,]

results2<-results1[results1$Net>0,]

####################################################################

results3<-results2[results2$taxa_desc<0&results2$calc1<=results2$Preco_Cliente,]

nid_res<-as.numeric(as.factor(results3$id_res))
results3<-cbind(nid_res,results3)

results3<-arrange(results3,results3$nid_res)

nid_res<-as.array(unique(as.numeric(results3$nid_res)))
nid_res2<-seq(1:nrow(nid_res))
nid_res3<-as.data.frame(cbind(nid_res2,nid_res))
results3<- merge(results3,nid_res3,'nid_res',all.x=TRUE)

tab_f<-tab_f2<-NULL

for (i in 1:max(nid_res2))
{ 
  tab0<-results3[results3$nid_res2==i,]
  max_net<-max(tab0[,'Net'])
  prec_tab<-tab0[tab0$Net==max_net,'calc1']
  faixa_peso<-as.character(tab0[tab0$Net==max_net,'faixa_peso'])
  tarifa<-as.character(tab0[tab0$Net==max_net,'tarifa'])
  id_res<-as.character(tab0[tab0$Net==max_net,'id_res'])
  prec_mand<-as.character(tab0[tab0$Net==max_net,'Preco_Base'])
  prec_cliente<-as.character(tab0[tab0$Net==max_net,'Preco_Cliente'])
  
  tab_f<-as.data.frame(cbind(i,id_res,max_net,faixa_peso,tarifa,prec_tab,prec_mand,prec_cliente))
  tab_f2<-rbind(tab_f,tab_f2)
  
}

###############################################################################


results4<-results2[results2$taxa_desc>=0&results2$calc1<=results2$Preco_Cliente,]
nid_res<-as.numeric(as.factor(results4$id_res))
results4<-cbind(nid_res,results4)

results4<-arrange(results4,results4$nid_res)




nid_res<-as.array(unique(as.numeric(results4$nid_res)))
nid_res2<-seq(1:nrow(nid_res))
nid_res3<-as.data.frame(cbind(nid_res2,nid_res))
results4<- merge(results4,nid_res3,'nid_res',all.x=TRUE)

tab_f<-tab_f3<-NULL

for (i in 1:max(nid_res2))
{ 
  tab0<-results4[results4$nid_res2==i,]
  max_net<-max(tab0[,'Net'])
  prec_tab<-tab0[tab0$Net==max_net,'calc1']
  faixa_peso<-as.character(tab0[tab0$Net==max_net,'faixa_peso'])
  tarifa<-as.character(tab0[tab0$Net==max_net,'tarifa'])
  id_res<-as.character(tab0[tab0$Net==max_net,'id_res'])
  prec_mand<-as.character(tab0[tab0$Net==max_net,'Preco_Base'])
  prec_cliente<-as.character(tab0[tab0$Net==max_net,'Preco_Cliente'])
  
  tab_f<-as.data.frame(cbind(i,id_res,max_net,faixa_peso,tarifa,prec_tab,prec_mand,prec_cliente))
  tab_f3<-rbind(tab_f,tab_f3)
  
}


##################################################################################

tab_A<-rbind(tab_f2,tab_f3)

nid_res<-as.numeric(as.factor(tab_A$id_res))
tab_A<-cbind(nid_res,tab_A)



tab_A<-arrange(tab_A,tab_A$nid_res)




nid_res<-as.array(unique(as.numeric(tab_A$nid_res)))
nid_res2<-seq(1:nrow(nid_res))
nid_res3<-as.data.frame(cbind(nid_res2,nid_res))
tab_A<- merge(tab_A,nid_res3,'nid_res',all.x=TRUE)
tab_A$max_net<-as.numeric(as.character(tab_A$max_net))


tab_f<-tab_f4<-NULL

for (i in 1:max(nid_res2))
{ 
  tab0<-tab_A[tab_A$nid_res2==i,]
  max_net<-max(tab0[,'max_net'])
  prec_tab<-as.character(tab0[tab0$max_net==max_net,'prec_tab'])
  faixa_peso<-as.character(tab0[tab0$max_net==max_net,'faixa_peso'])
  tarifa<-as.character(tab0[tab0$max_net==max_net,'tarifa'])
  id_res<-as.character(tab0[tab0$max_net==max_net,'id_res'])
  prec_mand<-as.character(tab0[tab0$max_net==max_net,'prec_mand'])
  prec_cliente<-as.character(tab0[tab0$max_net==max_net,'prec_cliente'])
  
  tab_f<-as.data.frame(cbind(i,id_res,max_net,faixa_peso,tarifa,prec_tab,prec_mand,prec_cliente))
  tab_f4<-rbind(tab_f,tab_f4)
  
}

tab_result1<-tab_f4 # resultado da otimizaçao

#################################################################

nid_res<-as.numeric(as.factor(tabela$id_res))

tabela<-cbind(nid_res,tabela)

tabela<-arrange(tabela,tabela$nid_res)

tab_result2<-unique(tabela[,c('id_res','nid_res','tarifa','f_cep','f_peso','preco','custo')])

tab_results3<-merge(tab_result2,tab_result1[,c('id_res','prec_tab','prec_cliente')],'id_res',all.x=TRUE)
tab_results3$prec_tab<-as.numeric(as.character(tab_results3$prec_tab))
tab_results3$prec_cliente<-as.numeric(as.character(tab_results3$prec_cliente))

tab_results3$prec_tab[is.na(tab_results3$prec_tab)] <- tab_results3$preco[is.na(tab_results3$prec_tab)]

tab_final<-unique(tab_results3[,c('tarifa','f_peso','prec_tab')])

out<-strsplit(as.character(tab_final$tarifa),'_')
out<-do.call(rbind, out)
colnames(out)<-c('UF','local')

out<-merge(out,F_estados[,c(1,5)],'UF',all.x=TRUE)


tab_final<-cbind(tab_final,out)




setwd("~/Price_Elast/Segmento_2_125K/F7_BASE/z.tabelas")

write.xlsx(tab_final,"tabela_preco_segmento2_150_400_5.xlsx",sheetName="Sheet1",col.names=TRUE, row.names=FALSE)



################################################################################
################## CLiente 1



setwd("~/Price_Elast/Segmento_2_125K/F7_BASE/Cliente_1")
tabela3<- read_csv(paste0("tabela_CL1_5p.csv"))
tabela<-tabela3
tabela<-tabela[,-c(1)]

setwd("~/Price_Elast/Segmento_2_125K/F7_BASE/z.tabelas")
library(readxl)
tab_final <- read_excel("tabela_preco_segmento2_150_400_5.xlsx")
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

tab_prec$minimo<-tab_prec$minimo/desconto


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



cliente<-c("CL1")

datf<-NULL

dat_<-cbind(cliente,dat_p1,PEDIDOS_TOTAL,GSV_TOTAL_OTIMIZADO,GSV_MINIMO,dif,perc)
datf<-rbind(dat_,datf)




setwd("~/Price_Elast/Segmento_2_125K/F7_BASE/y.output")
write.xlsx(datf,"tabela_resultados_CL1_segmento2_150_400_5.xlsx",sheetName="Sheet1",col.names=TRUE, row.names=FALSE)


#########################################################
