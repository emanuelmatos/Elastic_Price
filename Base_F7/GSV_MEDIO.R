library(readr)
tabela <- read_csv("~/Price_Elast/Segmento_2_125K/F7_BASE/tabela_0p.csv")

max(tabela$minimo/tabela$preco)


tabela$minimo<-tabela$minimo*1.05

sum(tabela$qtd_original)

gsv_medio_KH<-sum(tabela_0p$preco*tabela_0p$qtd_original)/sum(tabela_0p$qtd_original)
gsv_medio_MIN<-sum(tabela_0p$minv3*tabela_0p$qtd_original)/sum(tabela_0p$qtd_original)
custo_medio<-sum(tabela$custo_original*tabela$qtd_original)/sum(tabela_0p$qtd_original)


gsv_medio_KH<-sum(tabela_0p$preco*tabela_0p$qtd_original)/sum(tabela_0p$qtd_original)
gsv_medio_MIN<-sum(tabela_0p$minimo*tabela_0p$qtd_original)/sum(tabela_0p$qtd_original)
custo_medio<-sum(tabela_0p$custo_original*tabela_0p$qtd_original)/sum(tabela_0p$qtd_original)


dif<-(tabela$minimo-tabela_0p$m3)

tabela<-cbind(tabela,dif)

tab<-tabela[tabela$dif<0,]




net_med<-gsv_medio-custo_medio

perc_net_med<-net_med/gsv_medio_KH*100

pedidos<-sum(tabela_0p$qtd_original)

NET<- sum(tabela_0p$preco_original*tabela_0p$qtd_original)-sum(tabela_0p$custo_original*tabela_0p$qtd_original)

CUSTO<-sum(tabela_0p$custo_original*tabela_0p$qtd_original)

GSV<-sum(tabela_0p$preco_original*tabela_0p$qtd_original)

Perc_Net<-NET/GSV*100

###################################################

gsv_medio_KH2<-sum(tabela_0p$preco*tabela_0p$qtd_original)/sum(tabela_0p$qtd_original)
gsv_medio_MIN2<-sum(tabela_0p$minimo*tabela_0p$qtd_original)/sum(tabela_0p$qtd_original)
custo_medio2<-sum(tabela_0p$custo*tabela_0p$qtd_original)/sum(tabela_0p$qtd_original)

net_med2<-gsv_medio_KH2-custo_medio2

perc_net_med2<-net_med2/gsv_medio_KH2*100

pedidos<-sum(tabela_0p$qtd_original)

NET2<- sum(tabela_0p$preco*tabela_0p$qtd_original)-sum(tabela_0p$custo*tabela_0p$qtd_original)

CUSTO2<-sum(tabela_0p$custo*tabela_0p$qtd_original)

GSV2<-sum(tabela_0p$preco*tabela_0p$qtd_original)

Perc_Net2<-NET2/GSV2*100