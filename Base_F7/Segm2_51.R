setwd("~/Price_Elast/Segmento_2_125K/F7_BASE")

p<-1
ini<-1
fim<-20
desc1<-c(-900)
desc2<-40

nome<-paste0('part_5_',p)



#install.packages("dplyr")
library(dplyr)
library(readxl)
library(openxlsx)
library(readr)

setwd("~/Price_Elast/Segmento_2_125K/F7_BASE")
tabela<- read_csv("tabela_5p.csv")

tabela<-tabela[,-(1)]


####################### INICIALIZACAO

setwd("~/Price_Elast/Segmento_2_125K/F7_BASE")



  
  ##########################################
  
  
  id_tarifa<-as.numeric(as.factor(tabela$tarifa))
  id_f_peso<-as.numeric(as.factor(tabela$f_peso))
  
  tabela<-cbind(id_tarifa,id_f_peso,tabela)
  
  max(id_tarifa)
  max(id_f_peso)
  
  id_res<-paste0(tabela$tarifa,tabela$f_peso)
  tabela<-cbind(id_res,tabela)
  
  
  
  
########################################################################
##  Fase 2 - calculo simula?ao
########################################################################

t<-tar<-fp_tar<-a<-b<-c<-d<-NULL
results<-results0<-NULL

tmax<-max(tabela$id_tarifa)


for (t in ini:fim)#t<-1 104/103
{

  tar<-as.character(unique(tabela[tabela$id_tarifa==t,'tarifa']))  
  tab0<-tabela[tabela$id_tarifa==t,]
  print(paste0(tar," - ",t))
    for (f in 1:17)#f<-1 17
      
    {

## Tabela de Pre?o
      fp_tar<-as.character(unique(tab0[tab0$id_f_peso==f,'f_peso'])) 

  tab_mandae1<-tab0[tab0$id_f_peso==f,c(1:5)]
  tab_mandae2<-tab0[tab0$id_f_peso==f,'preco']*1
  tab_mandae_preco<-cbind(tab_mandae1,tab_mandae2)
 

## Tabela de Custo

  tab_mandae1<-tab0[tab0$id_f_peso==f,c(1:5)]
  tab_mandae2<-tab0[tab0$id_f_peso==f,'custo']
  tab_mandae_custo<-cbind(tab_mandae1,tab_mandae2)
  

  ## Tabela Minima-Cliente  ##############################################

  tab_mandae1<-tab0[tab0$id_f_peso==f,c(1:5)]
  tab_mandae2<-tab0[tab0$id_f_peso==f,'minimo']
  tab_mandae_mincliente<-cbind(tab_mandae1,tab_mandae2)
  

  #Tabela Quantidade

  tab_mandae1<-tab0[tab0$id_f_peso==f,c(1:5)]
  tab_mandae2<-tab0[tab0$id_f_peso==f,'qtd']
  tab_mandae_quantidade<-cbind(tab_mandae1,tab_mandae2)
  
  

#####################################################

  
for (t in desc1:desc2)
{

  
    
      tax<-t/100
      desc<-(1-tax)
      estrut<-tab_mandae_preco[,1:5]
      tab_mandaep<-tab_mandae_preco[,6]*desc
      tab_mandaep<-cbind(estrut,tab_mandaep)
      colnames(tab_mandaep)<-c('id_tarifa','id_f_peso','tarifa','F_CEP','F_PESO','VALOR')
      prec_m<-tab_mandae_preco[1,6]

      dif<-(tab_mandaep[,6]<tab_mandae_mincliente[,6])*1
      dif<-cbind(estrut,dif)
      colnames(dif)<-c('id_tarifa','id_f_peso','tarifa','F_CEP','F_PESO','VALOR')
      dif[is.na(dif)] <- 0
      prec_min<-tab_mandae_mincliente[1,6]

      qtd_dif<-dif[,6]*tab_mandae_quantidade[,6]
      qtd_dif<-cbind(estrut,qtd_dif)
      colnames(qtd_dif)<-c('id_tarifa','id_f_peso','tarifa','F_CEP','F_PESO','VALOR')
      qtd_cap<-sum(qtd_dif$VALOR)
      qtd_op<-tab_mandae_quantidade[1,6]

      cust_mand<-qtd_dif[,6]*tab_mandae_custo[,6]
      cust_mand[is.na(cust_mand)] <- 0
      cust_mand<-cbind(estrut,cust_mand)
      colnames(cust_mand)<-c('id_tarifa','id_f_peso','tarifa','F_CEP','F_PESO','VALOR')
      gsv_custo<-sum(cust_mand$VALOR)

  
      prec_mand<-qtd_dif[,6]*tab_mandaep[,6]
      prec_mand[is.na(prec_mand)] <- 0
      prec_mand<-cbind(estrut,prec_mand)
      colnames(prec_mand)<-c('id_tarifa','id_f_peso','tarifa','F_CEP','F_PESO','VALOR')
      gsv_prec<-sum(prec_mand$VALOR)

      profit<-prec_mand[,'VALOR']-cust_mand[,'VALOR']
      profit<-cbind(estrut,profit)
      colnames(profit)<-c('id_tarifa','id_f_peso','tarifa','F_CEP','F_PESO','VALOR')
      gsv_profit<-sum(profit$VALOR)
  
      
      
      

results0<-cbind(tar,fp_tar,tax*100,qtd_cap,prec_min,prec_m,gsv_custo,gsv_prec,gsv_profit)
results<-rbind(results0,results)

  }
  
}

}

results<-as.data.frame(results)
colnames(results)<-c('tarifa','faixa_peso','taxa_desc','Qdt_extra','Preco_Cliente','Preco_Base','Custo_qq_extra','GSV_Extra','Net')


#results_old<-results
setwd("~/Price_Elast/Segmento_2_125K/F7_BASE/y.output")
write.table(results,paste0("res_",nome,".csv"),sep=",",row.names=FALSE)
 

###############################################################################
