
setwd("~/Price_Elast/Segmento_2_125K/F7_BASE")

desconto<-0.99

#install.packages("dplyr")
library(dplyr)
library(readxl)
library(openxlsx)
library(readr)


############################## ABrangencia Economico
setwd("~/Price_Elast/Segmento_2_125K/F7_BASE/Base")
library(readxl)
abrang <- read_excel("E_abrangencia_nova_2019.xlsx")
abrang<-abrang[,c(1,6,7)]
colnames(abrang)<-c('F_CEP','PRAZO','TARIFA')
######################################## FAIXA PESO
library(readr)
FAIXA_PESO <- read_delim("G_FAIXA_PESO.csv", 
                         ";", escape_double = FALSE, trim_ws = TRUE)
#################################################################################

#################CUSTO com desconto de % - Margem 30%
custo_mandae <- read_excel("A_costs_mandae_NA.xlsx")
custo_mandae<-arrange(custo_mandae,custo_mandae$F_CEP)
custo_tar<-custo_mandae[,]
#write.table(economico,"custo_economico.csv",sep=",",row.names=FALSE)

###########################################################################  
##   TABELA de PRECO MANDAE - T_MAX
################################################################################################
#preco_mandae <- read_excel("tab_starter_rapida.xlsx")
preco_mandae <- read_excel("B_tab_prec_TMAX_NA.xlsx")
preco_mandae<-arrange(preco_mandae, (preco_mandae$F_CEP))           ## dplyr
tab_economico_tar<-preco_mandae[,]
#write.table(tab_economico,"tabela_economico.csv",sep=",",row.names=FALSE)
#################################################################################


data<-custo_tar##Tabela de custo
{
  data5<-datal<-datar<-NULL
  datar<-as.data.frame(data[,c(2,1)])
  for (i in 3:19)
  {
    j<-(i-2)
    data5<-cbind(datar,data[,i],paste0(c('FAIXA_'),j))
    colnames(data5)<-c('tarifa','F_CEP','VALOR','FAIXA')
    datal<-rbind(datal,data5)
  }
  datal<-merge(datal,FAIXA_PESO,by.x='FAIXA')
  tab_custo_mandae<-datal[,c(2,3,4,7)]
}

#############################################
data<-tab_economico_tar##Tabela de preco Mandae
{
  data5<-datal<-datar<-NULL
  datar<-as.data.frame(data[,c(2,1)])
  for (i in 3:19)
  {
    j<-(i-2)
    data5<-cbind(datar,data[,i],paste0(c('FAIXA_'),j))
    colnames(data5)<-c('tarifa','F_CEP','VALOR','FAIXA')
    datal<-rbind(datal,data5)
  }
  datal<-merge(datal,FAIXA_PESO,by.x='FAIXA')
  tab_prec_mandae<-datal[,c(2,3,4,7)]
}

####################################################################################################
setwd("~/Price_Elast/Segmento_2_125K/F7_BASE/Base")

library(readxl)
qtd_gen<- read_excel("generica_125K_8869.xlsx")
qtd_gen<-qtd_gen[,-c(19,20)]
qtd_gen<-merge(abrang[,-(2)],qtd_gen,'F_CEP',all.x=TRUE)
qtd_gen[is.na(qtd_gen)]<-0

data<-qtd_gen##Tabela de quntidade
{
  data5<-datal<-datar<-NULL
  datar<-as.data.frame(data[,c(2,1)])
  for (i in 3:19)
  {
    j<-(i-2)
    data5<-cbind(datar,data[,i],paste0(c('FAIXA_'),j))
    colnames(data5)<-c('tarifa','F_CEP','VALOR','FAIXA')
    datal<-rbind(datal,data5)
  }
  datal<-merge(datal,FAIXA_PESO,by.x='FAIXA')
  tab_qtd_generico_mandae<-datal[,c(2,3,4,7)]
}







#########################################################################################################################
##CLIENTE_1
######################################################################
#Quantidade - utiliar a generica mandae
##############################################################################################
#setwd("~/Price_Elast/Segmento_2_125K/F7_BASE/Cliente_1")
#library(readxl)
#qtd <- read_excel("C_hist_cliente.xlsx")
#qtd<-merge(abrang[,c('F_CEP')],qtd,'F_CEP',all.x=TRUE)
#qtd[is.na(qtd)]<-0


######################################################################
#Tabela Transportadoras
##############################################################################################
setwd("~/Price_Elast/Segmento_2_125K/F7_BASE/Cliente_1")
library(readxl)
transp <- read_excel("tab_min_transp_correios.xlsx")
#transp3 <- read_excel("min_f7_v3.xlsx")
#colnames(transp)[1] <-"F_CEP"
#colnames(transp)[2] <-"tarifa"
#transp<-merge(abrang[,c('F_CEP')],transp,'F_CEP',all.x=TRUE)
transp[is.na(transp)]<-0
#########################################################################
########################################################################
###  TABELAS CLIENTES

####################################################################################################
#Tabela de Preco transportadora Cliente

data<-transp##Tabela de custo
{
  data5<-datal<-datar<-NULL
  datar<-as.data.frame(data[,c(2,1)])
  for (i in 3:19)
  {
    j<-(i-2)
    data5<-cbind(datar,data[,i],paste0(c('FAIXA_'),j))
    colnames(data5)<-c('tarifa','F_CEP','VALOR','FAIXA')
    datal<-rbind(datal,data5)
  }
  datal<-merge(datal,FAIXA_PESO,by.x='FAIXA')
  tab_prec_cliente<-datal[,c(2,3,4,7)]
}



#
################################################################
# Tabela Unica




tabela<-cbind(tab_prec_mandae[,c(1,2,4,3)])
id_res<-paste0(tabela$F_CEP,tabela$FAIXA_PESO)
colnames(tabela)[4] <- "preco"
tabela<-cbind(id_res,tabela)


id_res<-paste0(tab_custo_mandae$F_CEP,tab_custo_mandae$FAIXA_PESO)
tab_custo_mandae<-cbind(id_res,tab_custo_mandae)


id_res<-paste0(tab_prec_cliente$F_CEP,tab_prec_cliente$FAIXA_PESO)
tab_prec_cliente<-cbind(id_res,tab_prec_cliente)

id_res<-paste0(tab_qtd_generico_mandae$F_CEP,tab_qtd_generico_mandae$FAIXA_PESO)
tab_qtd_generico_mandae<-cbind(id_res,tab_qtd_generico_mandae)


tabela1<-merge(tabela,tab_custo_mandae[,c('id_res','VALOR')],'id_res',all.x=TRUE)
colnames(tabela1)[6] <- "custo"

tabela1<-merge(tabela1,tab_prec_cliente[,c('id_res','VALOR')],'id_res',all.x=TRUE)
colnames(tabela1)[7] <- "minimo"

tabela1<-merge(tabela1,tab_qtd_generico_mandae[,c('id_res','VALOR')],'id_res',all.x=TRUE)
colnames(tabela1)[8] <- "qtd"


tabela<-tabela1


colnames(tabela)<-c('id_res','tarifa','f_cep','f_peso','preco','custo','minimo','qtd')

tabela[is.na(tabela)]<-0


qtd_original<-tabela$qtd
tabela$qtd<-tabela$qtd*10
tabela$qtd<-tabela$qtd+1
tabela<-cbind(tabela,qtd_original)

#Custo Simulação = 15,71 Custo KH = 11.12 --> 1.4127

custo_original<-tabela$custo
tabela$custo<-tabela$custo
tabela<-cbind(tabela,custo_original)


preco_original<-tabela$preco
tabela$preco<-tabela$preco
tabela<-cbind(tabela,preco_original)

# PAC(32.40)/MIN(19.58) = 1.65


#tabela$minimo[tabela$tarifa!='SP_C1'] <- tabela$minimo[tabela$tarifa!='SP_C1']*1






#
tabelacl1<-tabela



setwd("~/Price_Elast/Segmento_2_125K/F7_BASE/Cliente_1")
write.table(tabela,paste0("tabela_CL1_0p.csv"),sep=",",row.names=FALSE)

setwd("~/Price_Elast/Segmento_2_125K/F7_BASE/Cliente_1")
tabela$minimo<-tabela$minimo*desconto #5% de desconto
write.table(tabela,paste0("tabela_CL1_5p.csv"),sep=",",row.names=FALSE)
######################################################################

library(readr)
setwd("~/Price_Elast/Segmento_2_125K/F7_BASE/Cliente_1")
tabela<- read_csv("tabela_CL1_0p.csv")
cliente<-c('Cliente1')
tabela1<-cbind(cliente,tabela)

tabela<-tabela1

setwd("~/Price_Elast/Segmento_2_125K/F7_BASE")
write.table(tabela,"tabela_0p.csv",sep=",",row.names=FALSE)

#############

library(readr)
setwd("~/Price_Elast/Segmento_2_125K/F7_BASE/Cliente_1")
tabela<- read_csv("tabela_CL1_5p.csv")
cliente<-c('Cliente1')
tabela1<-cbind(cliente,tabela)

tabela<-tabela1

setwd("~/Price_Elast/Segmento_2_125K/F7_BASE")
write.table(tabela,"tabela_5p.csv",sep=",",row.names=FALSE)

#########################################################################
