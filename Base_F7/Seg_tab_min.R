
#setwd("G:/Team Drives/Opera??es/4. Pricing/Tabela builder v2/Parice elasticity/Simulation/Toctech")
setwd("~/Price_Elast/Segmento_2_125K")

library(readxl)
F_estados <- read_excel("F_estados.xlsx")
#install.packages("dplyr")
library(dplyr)
library(readxl)
library(openxlsx)
library(readr)



####################################################################################################
#Tabela de Minimo Cliente

library(readr)
setwd("~/Price_Elast/Segmento_2_125K/y.output")
res1<- read_csv(paste0("res_part_0_1.csv"))
res2<- read_csv(paste0("res_part_0_2.csv"))
res3<- read_csv(paste0("res_part_0_3.csv"))
res4<- read_csv(paste0("res_part_0_4.csv"))
res5<- read_csv(paste0("res_part_0_5.csv"))

results<-rbind(res1,res2,res3,res4,res5)

#####################################

library(readr)
setwd("~/Price_Elast/Segmento_2_125K")
tabela<- read_csv("tabela_0p.csv")

###################################################################

id_tarifa<-as.numeric(as.factor(tabela$tarifa))
id_f_peso<-as.numeric(as.factor(tabela$f_peso))

tabela<-cbind(id_tarifa,id_f_peso,tabela)

max(id_tarifa)
max(id_f_peso)

id_res<-paste0(tabela$tarifa,tabela$f_peso)
tabela<-cbind(id_res,tabela)

########################################################
#write.table(results,paste0("results_seg1_8p.csv"),sep=",",row.names=FALSE)


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

#write.table(results1,"resultado_SP_C1.csv",sep=",",row.names=FALSE)

tab_f<-tab_f2<-NULL


id_res<-as.array(unique(as.character(results1$id_res)))
nid_res2<-as.numeric(as.factor(id_res))
nid_res3<-seq(1:max(nid_res2))
nid_res4<-as.data.frame(cbind(id_res,nid_res2))

results1<- merge(results1,nid_res4,'id_res',all.x=TRUE)



#tab_res<-merge(tabela,tab_f[,c('id_res','prec_tab')],'id_res',all.x=TRUE)


#tab_res<- merge(tab_res,nid_res4,'id_res',all.x=TRUE)


#tab_res_2<-tab_res[tab_res$qtd]


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
tab_res2$prec_tab<-tab_res2$custo*1.05


tabela_final<-rbind(tab_res1,tab_res2)


tab_prec_final<-tabela_final[,c('id_res','tarifa','f_peso','qtd','prec_tab')]

tab_prec_final<-unique(tabela_final)

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
#########################################
setwd("~/Price_Elast/Segmento_2_125K")
library(readxl)
estados <- read_excel("F_estados.xlsx")
#########################################

tabela_mandae<-merge(tabela_mandae,estados[,c(1,5)],'UF',all.x=TRUE)

setwd("~/Price_Elast/Segmento_2_125K/z.tabelas")
write.xlsx(tabela_mandae,"tabela_mandae_min.xlsx",sheetName="Sheet1",col.names=TRUE, row.names=FALSE)

