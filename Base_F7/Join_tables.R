setwd("~/Price_Elast/Segmento_2_125K/F7_BASE/z.tabelas")
library(readxl)
min <- read_excel("tabela_mandae_min.xlsx")
p0 <- read_excel("tabela_preco_segmento2_150_400_0.xlsx")
id_res<-paste0(p0$tarifa,p0$f_peso)
p0<-cbind(id_res,p0)
p5 <- read_excel("tabela_preco_segmento2_150_400_5.xlsx")
id_res<-paste0(p5$tarifa,p5$f_peso)
p5<-cbind(id_res,p5)

tab_final<-merge(min,p0[,c('id_res','prec_tab')],'id_res',all.x=TRUE)

colnames(tab_final)[6] <- "prec_tab_m"
colnames(tab_final)[8] <- "prec_tab_0"

tab_final<-merge(tab_final,p5[,c('id_res','prec_tab')],'id_res',all.x=TRUE)
colnames(tab_final)[9] <- "prec_tab_5"


tab_final$prec_tab <- with(tab_final, pmin(prec_tab_m,prec_tab_0,prec_tab_5))

setwd("~/Price_Elast/Segmento_2_125K/F7_BASE/z.tabelas")

write.xlsx(tab_final,"tabela_preco_final.xlsx",sheetName="Sheet1",col.names=TRUE, row.names=FALSE)

########################################################################
################################################################################
################## CLiente 1



setwd("~/Price_Elast/Segmento_2_125K/F7_BASE/Cliente_1")
tabela3<- read_csv(paste0("tabela_CL1_0p.csv"))
tabela<-tabela3
tabela<-tabela[,-c(1)]

setwd("~/Price_Elast/Segmento_2_125K/F7_BASE/z.tabelas")
library(readxl)
tab_final <- read_excel("tabela_preco_final.xlsx")



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



cliente<-c("CL1")

datf<-NULL

dat_<-cbind(cliente,dat_p1,PEDIDOS_TOTAL,GSV_TOTAL_OTIMIZADO,GSV_MINIMO,dif,perc)
datf<-rbind(dat_,datf)




setwd("~/Price_Elast/Segmento_2_125K/F7_BASE/y.output")
write.xlsx(datf,"tabela_resultados_tab_final.xlsx",sheetName="Sheet1",col.names=TRUE, row.names=FALSE)


#########################################################

