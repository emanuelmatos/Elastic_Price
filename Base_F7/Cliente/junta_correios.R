library(readxl)
SEDEX_ECOMM6F7 <- read_excel("SEDEX_ECOMM6F7.xlsx")
SEDEX<-SEDEX_ECOMM6F7[,c(2,7:23)]
colnames(SEDEX)[1] <- "CEP"


library(readxl)
PAC_ECOMM6F7 <- read_excel("PAC_ECOMM6F7.xlsx")
PAC<-PAC_ECOMM6F7[,c(2,7:23)]
colnames(PAC)[1] <- "CEP"


library(readxl)
abrang_prz <- read_excel("abrang_prz.xlsx")
colnames(abrang_prz)[2] <- "CEP"
abrang_tab<-abrang_prz[,c(1,2,6,7,9)]

SEDEX<-merge(abrang_tab,SEDEX,'CEP',all.x=TRUE)
PAC<-merge(abrang_tab,PAC,'CEP',all.x=TRUE)



data<-SEDEX##Tabela de custo
{
  data5<-datal<-datar<-NULL
  datar<-as.data.frame(data[,c(4,2,5)])
  for (i in 6:22)
  {
    j<-(i-5)
    data5<-cbind(datar,data[,i],paste0(c('FAIXA_'),j))
    colnames(data5)<-c('tarifa','F_CEP','PRAZO','VALOR','FAIXA')
    datal<-rbind(datal,data5)
  }
  datal<-merge(datal,FAIXA_PESO,by.x='FAIXA')
  tab_sedex_cliente<-datal[,c(2,3,4,5,8)]
}




data<-PAC##Tabela de custo
{
  data5<-datal<-datar<-NULL
  datar<-as.data.frame(data[,c(4,2,5)])
  for (i in 6:22)
  {
    j<-(i-5)
    data5<-cbind(datar,data[,i],paste0(c('FAIXA_'),j))
    colnames(data5)<-c('tarifa','F_CEP','PRAZO','VALOR','FAIXA')
    datal<-rbind(datal,data5)
  }
  datal<-merge(datal,FAIXA_PESO,by.x='FAIXA')
  tab_pac_cliente<-na.omit(datal[,c(2,3,4,5,8)])
}

tab_pac_cliente$VALOR<-gsub("x","0",tab_pac_cliente$VALOR)

tab_correios<-cbind(tab_pac_cliente,tab_sedex_cliente[,'VALOR'])
colnames(tab_correios)[4] <- "PAC"
colnames(tab_correios)[6] <- "SEDEX"

zpac<-(tab_correios$PAC>0)*1
zsedex<-(tab_correios$SEDEX>0)*1



tab_results3$prec_tab[is.na(tab_results3$prec_tab)] <- tab_results3$preco[is.na(tab_results3$prec_tab)]

tab_correios$Min <- with(tab_correios, pmin(tab_correios$PAC, tab_correios$SEDEX))


