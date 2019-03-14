setwd("~/Price_Elast/Segmento_2_125K/F7_BASE/y.output")

BP0<-BP8<-BPM0<-BP5<-BPM<-NULL
library(readxl)
BP0 <- read_excel("tabela_resultados_CL1_segmento2_150_400_0.xlsx")
tipo<-c('0p')
BP0<-cbind(tipo,BP0)

BP5 <- read_excel("tabela_resultados_CL1_segmento2_150_400_5.xlsx")
tipo<-c('5p')
BP5<-cbind(tipo,BP5)

BPM0 <- read_excel("tabela_resultados_CL1_segmento2_150_400_M.xlsx")
tipo<-c('0pmin')
BPM0<-cbind(tipo,BPM0)

BPM <- read_excel("tabela_resultados_tab_final.xlsx")
tipo<-c('Minimo(0/8/m)')
BPM<-cbind(tipo,BPM)

CL1<-rbind(BP0,BP5,BPM0,BPM)


setwd("~/Price_Elast/Segmento_2_125K/F7_BASE/y.output")
write.xlsx(CL1,"tabela_resultados.xlsx",sheetName="Sheet1",col.names=TRUE, row.names=FALSE)


