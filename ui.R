library(dplyr)
library(shiny)
library(shinyWidgets)
library(tidyr)
library(shinythemes)
library(highcharter)
library(tidyverse)
library(stringi)
library(readr)
library(lubridate)
library(quantmod)
library(xts)
library(plyr)
library(plotly)



options(highcharter.download_map_data = TRUE)
mapdata <- get_data_from_map(download_map_data("countries/br/br-all"))



map <- read_delim("C:/Users/Neo/Desktop/map.csv", 
                  delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% as.data.frame()

map_doses<- read_delim("C:/Users/Neo/Desktop/codigos SQL ITPS/DADOS/map_doses.csv", 
                    delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% as.data.frame()

map_doses = merge(map, map_doses, by.x = "SIGLA", by.y = "NOMES") %>% select(SIGLA,UF, POPULACAO, `DOSE 1`, `DOSE 2`, `DOSE R`)

map_doses$`%DOSE 1` = round((map_doses$`DOSE 1`/map_doses$POPULACAO) * 100,3)
map_doses$`%DOSE 2` = round((map_doses$`DOSE 2`/map_doses$POPULACAO) * 100,3)
map_doses$`%DOSE R` = round((map_doses$`DOSE R`/map_doses$POPULACAO) * 100,3)

map$`%DOSE 1` = map_doses$`%DOSE 1`
map$`%DOSE 2` = map_doses$`%DOSE 2`
map$`%DOSE R` = map_doses$`%DOSE R`






top_3_1 <- read_delim("C:/Users/Neo/Desktop/top_3_1.csv", 
                     delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% as.data.frame()

top_3_2 <- read_delim("C:/Users/Neo/Desktop/top_3_2.csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% as.data.frame()

top_3_R <- read_delim("C:/Users/Neo/Desktop/top_3_R.csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% as.data.frame()






totais <- read_delim("C:/Users/Neo/Desktop/totais.csv", 
                     delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% as.data.frame()

TS_BR_UFS <- read_delim("C:/Users/Neo/Desktop/codigos SQL ITPS/DADOS/TS_BR_UFS.csv", 
                    delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% as.data.frame()
TS_BR_UFS$DATA = as.Date(TS_BR_UFS$DATA, format = "%d/%m/%Y")
#TS_BR_UFS = TS_BR_UFS %>% arrange(TS_BR_UFS$DATA)


TS_BR_UFS = merge(TS_BR_UFS,totais, by.x = "NOME", by.y = "UF") %>% select(NOME ,DATA, HOSPITALIZAÇÕES.x, ÓBITOS.x ,CURADOS.x ,UTI ,INTUBAÇÃO ,REGIÃO ,NOME.y)%>%
  `colnames<-`(c("NOME" ,"DATA", "HOSPITALIZAÇÕES", "ÓBITOS" ,"CURADOS" ,"UTI" ,"INTUBAÇÃO" ,"REGIÃO" ,"NOMES"))



cruza_all <- read_delim("C:/Users/Neo/Desktop/codigos SQL ITPS/DADOS/cruza_all.csv", 
                        delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% as.data.frame()
cruza_all$DATA = as.Date(cruza_all$DATA, format = "%d/%m/%Y")

cruza=cruza_all %>% select(DATA,UF,OBT_ZR, OBT_UMA, OBT_DAS, OBT_TRS) 



estados = read_delim("C:/Users/Neo/Desktop/codigos SQL ITPS/DADOS/estados.csv", 
           delim = ",", escape_double = FALSE, trim_ws = TRUE) %>% as.data.frame()


cruza_all = merge(cruza_all,estados, by.x = "UF", by.y = "uf") %>% select(UF,DATA, UTI_INT_OBT_ZR ,UTI_INT_OBT_UMA, UTI_INT_OBT_DAS, UTI_INT_OBT_TRS ,UTI_OBT_ZR ,UTI_OBT_UMA ,UTI_OBT_DAS, UTI_OBT_TRS, UTI_INT_ZR ,UTI_INT_UMA, UTI_INT_DAS, UTI_INT_TRS,
INT_OBT_ZR ,INT_OBT_UMA, INT_OBT_DAS, INT_OBT_TRS ,INT_ZR, INT_UMA, INT_DAS ,INT_TRS, OBT_ZR ,OBT_UMA ,OBT_DAS, OBT_TRS, ZR ,UMA ,DAS, TRS, UTI_ZR, UTI_UMA, UTI_DAS ,UTI_TRS,  regiao)

cor_doses;map

M=read.csv("C:/Users/Neo/Desktop/codigos SQL ITPS/DADOS/M.csv",sep = ";", header = T, row.names = 1)%>%as.matrix()

cor_doses = read_delim("C:/Users/Neo/Desktop/codigos SQL ITPS/DADOS/corr_h_o.csv", 
                       delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% as.data.frame()

cor_doses = cor_doses %>% arrange((UF))


###MORAN########
cor_doses$MORTALIDADE_0 = as.numeric(unlist((((cor_doses %>% arrange((UF))) %>% select(OBITO_0))/(map_doses %>% arrange((UF)) %>% select(POPULACAO)))* 100000))
cor_doses$MORTALIDADE_1 = as.numeric(unlist((((cor_doses %>% arrange((UF))) %>% select(OBITO_1))/(map_doses %>% arrange((UF)) %>% select(POPULACAO)))* 100000))
cor_doses$MORTALIDADE_2 = as.numeric(unlist((((cor_doses %>% arrange((UF))) %>% select(OBITO_2))/(map_doses %>% arrange((UF)) %>% select(POPULACAO)))* 100000))
cor_doses$MORTALIDADE_3 = as.numeric(unlist((((cor_doses %>% arrange((UF))) %>% select(OBITO_3))/(map_doses %>% arrange((UF)) %>% select(POPULACAO)))* 100000))

cor_doses$HOSPITALIZACAO_0 = as.numeric(unlist((((cor_doses %>% arrange((UF))) %>% select(HOSP_0))/(map_doses %>% arrange((UF)) %>% select(POPULACAO)))* 100000))
cor_doses$HOSPITALIZACAO_1 = as.numeric(unlist((((cor_doses %>% arrange((UF))) %>% select(HOSP_1))/(map_doses %>% arrange((UF)) %>% select(POPULACAO)))* 100000))
cor_doses$HOSPITALIZACAO_2 = as.numeric(unlist((((cor_doses %>% arrange((UF))) %>% select(HOSP_2))/(map_doses %>% arrange((UF)) %>% select(POPULACAO)))* 100000))
cor_doses$HOSPITALIZACAO_3 = as.numeric(unlist((((cor_doses %>% arrange((UF))) %>% select(HOSP_3))/(map_doses %>% arrange((UF)) %>% select(POPULACAO)))* 100000))


y=as.numeric(cor_doses%>%select(MORTALIDADE_0)%>%unlist()%>%as.numeric())
wm = M
n <- length(y)
ybar <- mean(y)
dy <- y - ybar
yi <- rep(dy, each=n)
yj <- rep(dy)
yiyj <- yi * yj
pm <- matrix(yiyj, ncol=n)#
pmw <- pm * wm
spmw <- sum(pmw)
smw <- sum(wm)
sw  <- spmw / smw
vr <- n / sum(dy^2)
MI <- vr * sw
ms <- cbind(id=rep(1:n, each=n), y=rep(y, each=n), value=as.vector(wm * y))
ms <- ms[ms[,3] > 0, ]
ams <- aggregate(ms[,2:3], list(ms[,1]), FUN=mean)
ams <- ams[,-1]
colnames(ams) <- c('y', 'Defasagem espacial')
G_MORTALIDADE_0 = plot_ly(ams, x = ~y, y = ~`Defasagem espacial`, type = "scatter", mode = "markers") %>%
  layout(showlegend = F)%>%
  add_trace(data = ams, x = y, y = c(as.numeric(unlist(lm(ams[,2] ~ ams[,1])[1]))[1]   +   as.numeric(unlist(lm(ams[,2] ~ ams[,1])[1]))[2]*y), mode = "lines")%>%
  layout(title = 'diagrama de dispersão de Moran', plot_bgcolor = "#f0f0f0", xaxis = list(title = paste("MORTALIDADE_0")), yaxis = list(title = 'Defasagem espacial'))
reg <- lm(ams[,2] ~ ams[,1])
T_MORTALIDADE_0 = data.frame(Moran_I = MI,COEF = as.numeric(coefficients(reg)[2]))


y=cor_doses%>%select(MORTALIDADE_1)%>%unlist()%>%as.numeric()
wm = M
n <- length(y)
ybar <- mean(y)
dy <- y - ybar
yi <- rep(dy, each=n)
yj <- rep(dy)
yiyj <- yi * yj
pm <- matrix(yiyj, ncol=n)
pmw <- pm * wm
spmw <- sum(pmw)
smw <- sum(wm)
sw  <- spmw / smw
vr <- n / sum(dy^2)
MI <- vr * sw
ms <- cbind(id=rep(1:n, each=n), y=rep(y, each=n), value=as.vector(wm * y))
ms <- ms[ms[,3] > 0, ]
ams <- aggregate(ms[,2:3], list(ms[,1]), FUN=mean)
ams <- ams[,-1]
colnames(ams) <- c('y', 'Defasagem espacial')
G_MORTALIDADE_1 = plot_ly(ams, x = ~y, y = ~`Defasagem espacial`, type = "scatter", mode = "markers") %>%
  layout(showlegend = F)%>%
  add_trace(data = ams, x = y, y = c(as.numeric(unlist(lm(ams[,2] ~ ams[,1])[1]))[1]   +   as.numeric(unlist(lm(ams[,2] ~ ams[,1])[1]))[2]*y), mode = "lines")%>%
  layout(title = 'diagrama de dispersão de Moran', plot_bgcolor = "#f0f0f0", xaxis = list(title = paste("MORTALIDADE_1")), yaxis = list(title = 'Defasagem espacial'))
reg <- lm(ams[,2] ~ ams[,1])
T_MORTALIDADE_1 = data.frame(Moran_I = MI,COEF = as.numeric(coefficients(reg)[2]))



y=cor_doses%>%select(MORTALIDADE_2)%>%unlist()%>%as.numeric()
wm = M
n <- length(y)
ybar <- mean(y)
dy <- y - ybar
yi <- rep(dy, each=n)
yj <- rep(dy)
yiyj <- yi * yj
pm <- matrix(yiyj, ncol=n)
pmw <- pm * wm
spmw <- sum(pmw)
smw <- sum(wm)
sw  <- spmw / smw
vr <- n / sum(dy^2)
MI <- vr * sw
ms <- cbind(id=rep(1:n, each=n), y=rep(y, each=n), value=as.vector(wm * y))
ms <- ms[ms[,3] > 0, ]
ams <- aggregate(ms[,2:3], list(ms[,1]), FUN=mean)
ams <- ams[,-1]
colnames(ams) <- c('y', 'Defasagem espacial')
G_MORTALIDADE_2 = plot_ly(ams, x = ~y, y = ~`Defasagem espacial`, type = "scatter", mode = "markers") %>%
  layout(showlegend = F)%>%
  add_trace(data = ams, x = y, y = c(as.numeric(unlist(lm(ams[,2] ~ ams[,1])[1]))[1]   +   as.numeric(unlist(lm(ams[,2] ~ ams[,1])[1]))[2]*y), mode = "lines")%>%
  layout(title = 'diagrama de dispersão de Moran', plot_bgcolor = "#f0f0f0", xaxis = list(title = paste("MORTALIDADE_2")), yaxis = list(title = 'Defasagem espacial'))
reg <- lm(ams[,2] ~ ams[,1])
T_MORTALIDADE_2 = data.frame(Moran_I = MI,COEF = as.numeric(coefficients(reg)[2]))



y=cor_doses%>%select(MORTALIDADE_3)%>%unlist()%>%as.numeric()
wm = M
n <- length(y)
ybar <- mean(y)
dy <- y - ybar
yi <- rep(dy, each=n)
yj <- rep(dy)
yiyj <- yi * yj
pm <- matrix(yiyj, ncol=n)
pmw <- pm * wm
spmw <- sum(pmw)
smw <- sum(wm)
sw  <- spmw / smw
vr <- n / sum(dy^2)
MI <- vr * sw
ms <- cbind(id=rep(1:n, each=n), y=rep(y, each=n), value=as.vector(wm * y))
ms <- ms[ms[,3] > 0, ]
ams <- aggregate(ms[,2:3], list(ms[,1]), FUN=mean)
ams <- ams[,-1]
colnames(ams) <- c('y', 'Defasagem espacial')
G_MORTALIDADE_3 = plot_ly(ams, x = ~y, y = ~`Defasagem espacial`, type = "scatter", mode = "markers") %>%
  layout(showlegend = F)%>%
  add_trace(data = ams, x = y, y = c(as.numeric(unlist(lm(ams[,2] ~ ams[,1])[1]))[1]   +   as.numeric(unlist(lm(ams[,2] ~ ams[,1])[1]))[2]*y), mode = "lines")%>%
  layout(title = 'diagrama de dispersão de Moran', plot_bgcolor = "#f0f0f0", xaxis = list(title = paste("MORTALIDADE_3")), yaxis = list(title = 'Defasagem espacial'))
reg <- lm(ams[,2] ~ ams[,1])
T_MORTALIDADE_3 = data.frame(Moran_I = MI,COEF = as.numeric(coefficients(reg)[2]))




y=cor_doses%>%select(HOSPITALIZACAO_0)%>%unlist()%>%as.numeric()
wm = M
n <- length(y)
ybar <- mean(y)
dy <- y - ybar
yi <- rep(dy, each=n)
yj <- rep(dy)
yiyj <- yi * yj
pm <- matrix(yiyj, ncol=n)
pmw <- pm * wm
spmw <- sum(pmw)
smw <- sum(wm)
sw  <- spmw / smw
vr <- n / sum(dy^2)
MI <- vr * sw
ms <- cbind(id=rep(1:n, each=n), y=rep(y, each=n), value=as.vector(wm * y))
ms <- ms[ms[,3] > 0, ]
ams <- aggregate(ms[,2:3], list(ms[,1]), FUN=mean)
ams <- ams[,-1]
colnames(ams) <- c('y', 'Defasagem espacial')
G_HOSPITALIZACAO_0 = plot_ly(ams, x = ~y, y = ~`Defasagem espacial`, type = "scatter", mode = "markers") %>%
  layout(showlegend = F)%>%
  add_trace(data = ams, x = y, y = c(as.numeric(unlist(lm(ams[,2] ~ ams[,1])[1]))[1]   +   as.numeric(unlist(lm(ams[,2] ~ ams[,1])[1]))[2]*y), mode = "lines")%>%
  layout(title = 'diagrama de dispersão de Moran', plot_bgcolor = "#f0f0f0", xaxis = list(title = paste("HOSPITALIZACAO_0")), yaxis = list(title = 'Defasagem espacial'))
reg <- lm(ams[,2] ~ ams[,1])
T_HOSPITALIZACAO_0 = data.frame(Moran_I = MI,COEF = as.numeric(coefficients(reg)[2]))



y=cor_doses%>%select(HOSPITALIZACAO_1)%>%unlist()%>%as.numeric()
wm = M
n <- length(y)
ybar <- mean(y)
dy <- y - ybar
yi <- rep(dy, each=n)
yj <- rep(dy)
yiyj <- yi * yj
pm <- matrix(yiyj, ncol=n)
pmw <- pm * wm
spmw <- sum(pmw)
smw <- sum(wm)
sw  <- spmw / smw
vr <- n / sum(dy^2)
MI <- vr * sw
ms <- cbind(id=rep(1:n, each=n), y=rep(y, each=n), value=as.vector(wm * y))
ms <- ms[ms[,3] > 0, ]
ams <- aggregate(ms[,2:3], list(ms[,1]), FUN=mean)
ams <- ams[,-1]
colnames(ams) <- c('y', 'Defasagem espacial')
G_HOSPITALIZACAO_1 = plot_ly(ams, x = ~y, y = ~`Defasagem espacial`, type = "scatter", mode = "markers") %>%
  layout(showlegend = F)%>%
  add_trace(data = ams, x = y, y = c(as.numeric(unlist(lm(ams[,2] ~ ams[,1])[1]))[1]   +   as.numeric(unlist(lm(ams[,2] ~ ams[,1])[1]))[2]*y), mode = "lines")%>%
  layout(title = 'diagrama de dispersão de Moran', plot_bgcolor = "#f0f0f0", xaxis = list(title = paste("HOSPITALIZACAO_1")), yaxis = list(title = 'Defasagem espacial'))
reg <- lm(ams[,2] ~ ams[,1])
T_HOSPITALIZACAO_1 = data.frame(Moran_I = MI,COEF = as.numeric(coefficients(reg)[2]))



y=cor_doses%>%select(HOSPITALIZACAO_2)%>%unlist()%>%as.numeric()
wm = M
n <- length(y)
ybar <- mean(y)
dy <- y - ybar
yi <- rep(dy, each=n)
yj <- rep(dy)
yiyj <- yi * yj
pm <- matrix(yiyj, ncol=n)
pmw <- pm * wm
spmw <- sum(pmw)
smw <- sum(wm)
sw  <- spmw / smw
vr <- n / sum(dy^2)
MI <- vr * sw
ms <- cbind(id=rep(1:n, each=n), y=rep(y, each=n), value=as.vector(wm * y))
ms <- ms[ms[,3] > 0, ]
ams <- aggregate(ms[,2:3], list(ms[,1]), FUN=mean)
ams <- ams[,-1]
colnames(ams) <- c('y', 'Defasagem espacial')
G_HOSPITALIZACAO_2 = plot_ly(ams, x = ~y, y = ~`Defasagem espacial`, type = "scatter", mode = "markers") %>%
  layout(showlegend = F)%>%
  add_trace(data = ams, x = y, y = c(as.numeric(unlist(lm(ams[,2] ~ ams[,1])[1]))[1]   +   as.numeric(unlist(lm(ams[,2] ~ ams[,1])[1]))[2]*y), mode = "lines")%>%
  layout(title = 'diagrama de dispersão de Moran', plot_bgcolor = "#f0f0f0", xaxis = list(title = paste("HOSPITALIZACAO_2")), yaxis = list(title = 'Defasagem espacial'))
reg <- lm(ams[,2] ~ ams[,1])
T_HOSPITALIZACAO_2 = data.frame(Moran_I = MI,COEF = as.numeric(coefficients(reg)[2]))



y=cor_doses%>%select(HOSPITALIZACAO_3)%>%unlist()%>%as.numeric()
wm = M
n <- length(y)
ybar <- mean(y)
dy <- y - ybar
yi <- rep(dy, each=n)
yj <- rep(dy)
yiyj <- yi * yj
pm <- matrix(yiyj, ncol=n)
pmw <- pm * wm
spmw <- sum(pmw)
smw <- sum(wm)
sw  <- spmw / smw
vr <- n / sum(dy^2)
MI <- vr * sw
ms <- cbind(id=rep(1:n, each=n), y=rep(y, each=n), value=as.vector(wm * y))
ms <- ms[ms[,3] > 0, ]
ams <- aggregate(ms[,2:3], list(ms[,1]), FUN=mean)
ams <- ams[,-1]
colnames(ams) <- c('y', 'Defasagem espacial')
G_HOSPITALIZACAO_3 = plot_ly(ams, x = ~y, y = ~`Defasagem espacial`, type = "scatter", mode = "markers") %>%
  layout(showlegend = F)%>%
  add_trace(data = ams, x = y, y = c(as.numeric(unlist(lm(ams[,2] ~ ams[,1])[1]))[1]   +   as.numeric(unlist(lm(ams[,2] ~ ams[,1])[1]))[2]*y), mode = "lines")%>%
  layout(title = 'diagrama de dispersão de Moran', plot_bgcolor = "#f0f0f0", xaxis = list(title = paste("HOSPITALIZACAO_3")), yaxis = list(title = 'Defasagem espacial'))
reg <- lm(ams[,2] ~ ams[,1])
T_HOSPITALIZACAO_3 = data.frame(Moran_I = MI,COEF = as.numeric(coefficients(reg)[2]))





y=as.numeric(cor_doses%>%select(OBITO_0)%>%unlist()%>%as.numeric())
wm = M
n <- length(y)
ybar <- mean(y)
dy <- y - ybar
yi <- rep(dy, each=n)
yj <- rep(dy)
yiyj <- yi * yj
pm <- matrix(yiyj, ncol=n)#
pmw <- pm * wm
spmw <- sum(pmw)
smw <- sum(wm)
sw  <- spmw / smw
vr <- n / sum(dy^2)
MI <- vr * sw
ms <- cbind(id=rep(1:n, each=n), y=rep(y, each=n), value=as.vector(wm * y))
ms <- ms[ms[,3] > 0, ]
ams <- aggregate(ms[,2:3], list(ms[,1]), FUN=mean)
ams <- ams[,-1]
colnames(ams) <- c('y', 'Defasagem espacial')
G_OBITO_0 = plot_ly(ams, x = ~y, y = ~`Defasagem espacial`, type = "scatter", mode = "markers") %>%
  layout(showlegend = F)%>%
  add_trace(data = ams, x = y, y = c(as.numeric(unlist(lm(ams[,2] ~ ams[,1])[1]))[1]   +   as.numeric(unlist(lm(ams[,2] ~ ams[,1])[1]))[2]*y), mode = "lines")%>%
  layout(title = 'diagrama de dispersão de Moran', plot_bgcolor = "#f0f0f0", xaxis = list(title = paste("OBITO_0")), yaxis = list(title = 'Defasagem espacial'))
reg <- lm(ams[,2] ~ ams[,1])
T_OBITO_0 = data.frame(Moran_I = MI,COEF = as.numeric(coefficients(reg)[2]))


y=cor_doses%>%select(OBITO_1)%>%unlist()%>%as.numeric()
wm = M
n <- length(y)
ybar <- mean(y)
dy <- y - ybar
yi <- rep(dy, each=n)
yj <- rep(dy)
yiyj <- yi * yj
pm <- matrix(yiyj, ncol=n)
pmw <- pm * wm
spmw <- sum(pmw)
smw <- sum(wm)
sw  <- spmw / smw
vr <- n / sum(dy^2)
MI <- vr * sw
ms <- cbind(id=rep(1:n, each=n), y=rep(y, each=n), value=as.vector(wm * y))
ms <- ms[ms[,3] > 0, ]
ams <- aggregate(ms[,2:3], list(ms[,1]), FUN=mean)
ams <- ams[,-1]
colnames(ams) <- c('y', 'Defasagem espacial')
G_OBITO_1 = plot_ly(ams, x = ~y, y = ~`Defasagem espacial`, type = "scatter", mode = "markers") %>%
  layout(showlegend = F)%>%
  add_trace(data = ams, x = y, y = c(as.numeric(unlist(lm(ams[,2] ~ ams[,1])[1]))[1]   +   as.numeric(unlist(lm(ams[,2] ~ ams[,1])[1]))[2]*y), mode = "lines")%>%
  layout(title = 'diagrama de dispersão de Moran', plot_bgcolor = "#f0f0f0", xaxis = list(title = paste("OBITO_1")), yaxis = list(title = 'Defasagem espacial'))
reg <- lm(ams[,2] ~ ams[,1])
T_OBITO_1 = data.frame(Moran_I = MI,COEF = as.numeric(coefficients(reg)[2]))



y=cor_doses%>%select(OBITO_2)%>%unlist()%>%as.numeric()
wm = M
n <- length(y)
ybar <- mean(y)
dy <- y - ybar
yi <- rep(dy, each=n)
yj <- rep(dy)
yiyj <- yi * yj
pm <- matrix(yiyj, ncol=n)
pmw <- pm * wm
spmw <- sum(pmw)
smw <- sum(wm)
sw  <- spmw / smw
vr <- n / sum(dy^2)
MI <- vr * sw
ms <- cbind(id=rep(1:n, each=n), y=rep(y, each=n), value=as.vector(wm * y))
ms <- ms[ms[,3] > 0, ]
ams <- aggregate(ms[,2:3], list(ms[,1]), FUN=mean)
ams <- ams[,-1]
colnames(ams) <- c('y', 'Defasagem espacial')
G_OBITO_2 = plot_ly(ams, x = ~y, y = ~`Defasagem espacial`, type = "scatter", mode = "markers") %>%
  layout(showlegend = F)%>%
  add_trace(data = ams, x = y, y = c(as.numeric(unlist(lm(ams[,2] ~ ams[,1])[1]))[1]   +   as.numeric(unlist(lm(ams[,2] ~ ams[,1])[1]))[2]*y), mode = "lines")%>%
  layout(title = 'diagrama de dispersão de Moran', plot_bgcolor = "#f0f0f0", xaxis = list(title = paste("OBITO_2")), yaxis = list(title = 'Defasagem espacial'))
reg <- lm(ams[,2] ~ ams[,1])
T_OBITO_2 = data.frame(Moran_I = MI,COEF = as.numeric(coefficients(reg)[2]))



y=cor_doses%>%select(OBITO_3)%>%unlist()%>%as.numeric()
wm = M
n <- length(y)
ybar <- mean(y)
dy <- y - ybar
yi <- rep(dy, each=n)
yj <- rep(dy)
yiyj <- yi * yj
pm <- matrix(yiyj, ncol=n)
pmw <- pm * wm
spmw <- sum(pmw)
smw <- sum(wm)
sw  <- spmw / smw
vr <- n / sum(dy^2)
MI <- vr * sw
ms <- cbind(id=rep(1:n, each=n), y=rep(y, each=n), value=as.vector(wm * y))
ms <- ms[ms[,3] > 0, ]
ams <- aggregate(ms[,2:3], list(ms[,1]), FUN=mean)
ams <- ams[,-1]
colnames(ams) <- c('y', 'Defasagem espacial')
G_OBITO_3 = plot_ly(ams, x = ~y, y = ~`Defasagem espacial`, type = "scatter", mode = "markers") %>%
  layout(showlegend = F)%>%
  add_trace(data = ams, x = y, y = c(as.numeric(unlist(lm(ams[,2] ~ ams[,1])[1]))[1]   +   as.numeric(unlist(lm(ams[,2] ~ ams[,1])[1]))[2]*y), mode = "lines")%>%
  layout(title = 'diagrama de dispersão de Moran', plot_bgcolor = "#f0f0f0", xaxis = list(title = paste("OBITO_3")), yaxis = list(title = 'Defasagem espacial'))
reg <- lm(ams[,2] ~ ams[,1])
T_OBITO_3 = data.frame(Moran_I = MI,COEF = as.numeric(coefficients(reg)[2]))




y=cor_doses%>%select(HOSP_0)%>%unlist()%>%as.numeric()
wm = M
n <- length(y)
ybar <- mean(y)
dy <- y - ybar
yi <- rep(dy, each=n)
yj <- rep(dy)
yiyj <- yi * yj
pm <- matrix(yiyj, ncol=n)
pmw <- pm * wm
spmw <- sum(pmw)
smw <- sum(wm)
sw  <- spmw / smw
vr <- n / sum(dy^2)
MI <- vr * sw
ms <- cbind(id=rep(1:n, each=n), y=rep(y, each=n), value=as.vector(wm * y))
ms <- ms[ms[,3] > 0, ]
ams <- aggregate(ms[,2:3], list(ms[,1]), FUN=mean)
ams <- ams[,-1]
colnames(ams) <- c('y', 'Defasagem espacial')
G_HOSP_0 = plot_ly(ams, x = ~y, y = ~`Defasagem espacial`, type = "scatter", mode = "markers") %>%
  layout(showlegend = F)%>%
  add_trace(data = ams, x = y, y = c(as.numeric(unlist(lm(ams[,2] ~ ams[,1])[1]))[1]   +   as.numeric(unlist(lm(ams[,2] ~ ams[,1])[1]))[2]*y), mode = "lines")%>%
  layout(title = 'diagrama de dispersão de Moran', plot_bgcolor = "#f0f0f0", xaxis = list(title = paste("HOSP_0")), yaxis = list(title = 'Defasagem espacial'))
reg <- lm(ams[,2] ~ ams[,1])
T_HOSP_0 = data.frame(Moran_I = MI,COEF = as.numeric(coefficients(reg)[2]))



y=cor_doses%>%select(HOSP_1)%>%unlist()%>%as.numeric()
wm = M
n <- length(y)
ybar <- mean(y)
dy <- y - ybar
yi <- rep(dy, each=n)
yj <- rep(dy)
yiyj <- yi * yj
pm <- matrix(yiyj, ncol=n)
pmw <- pm * wm
spmw <- sum(pmw)
smw <- sum(wm)
sw  <- spmw / smw
vr <- n / sum(dy^2)
MI <- vr * sw
ms <- cbind(id=rep(1:n, each=n), y=rep(y, each=n), value=as.vector(wm * y))
ms <- ms[ms[,3] > 0, ]
ams <- aggregate(ms[,2:3], list(ms[,1]), FUN=mean)
ams <- ams[,-1]
colnames(ams) <- c('y', 'Defasagem espacial')
G_HOSP_1 = plot_ly(ams, x = ~y, y = ~`Defasagem espacial`, type = "scatter", mode = "markers") %>%
  layout(showlegend = F)%>%
  add_trace(data = ams, x = y, y = c(as.numeric(unlist(lm(ams[,2] ~ ams[,1])[1]))[1]   +   as.numeric(unlist(lm(ams[,2] ~ ams[,1])[1]))[2]*y), mode = "lines")%>%
  layout(title = 'diagrama de dispersão de Moran', plot_bgcolor = "#f0f0f0", xaxis = list(title = paste("HOSP_1")), yaxis = list(title = 'Defasagem espacial'))
reg <- lm(ams[,2] ~ ams[,1])
T_HOSP_1 = data.frame(Moran_I = MI,COEF = as.numeric(coefficients(reg)[2]))



y=cor_doses%>%select(HOSP_2)%>%unlist()%>%as.numeric()
wm = M
n <- length(y)
ybar <- mean(y)
dy <- y - ybar
yi <- rep(dy, each=n)
yj <- rep(dy)
yiyj <- yi * yj
pm <- matrix(yiyj, ncol=n)
pmw <- pm * wm
spmw <- sum(pmw)
smw <- sum(wm)
sw  <- spmw / smw
vr <- n / sum(dy^2)
MI <- vr * sw
ms <- cbind(id=rep(1:n, each=n), y=rep(y, each=n), value=as.vector(wm * y))
ms <- ms[ms[,3] > 0, ]
ams <- aggregate(ms[,2:3], list(ms[,1]), FUN=mean)
ams <- ams[,-1]
colnames(ams) <- c('y', 'Defasagem espacial')
G_HOSP_2 = plot_ly(ams, x = ~y, y = ~`Defasagem espacial`, type = "scatter", mode = "markers") %>%
  layout(showlegend = F)%>%
  add_trace(data = ams, x = y, y = c(as.numeric(unlist(lm(ams[,2] ~ ams[,1])[1]))[1]   +   as.numeric(unlist(lm(ams[,2] ~ ams[,1])[1]))[2]*y), mode = "lines")%>%
  layout(title = 'diagrama de dispersão de Moran', plot_bgcolor = "#f0f0f0", xaxis = list(title = paste("HOSP_2")), yaxis = list(title = 'Defasagem espacial'))
reg <- lm(ams[,2] ~ ams[,1])
T_HOSP_2 = data.frame(Moran_I = MI,COEF = as.numeric(coefficients(reg)[2]))



y=cor_doses%>%select(HOSP_3)%>%unlist()%>%as.numeric()
wm = M
n <- length(y)
ybar <- mean(y)
dy <- y - ybar
yi <- rep(dy, each=n)
yj <- rep(dy)
yiyj <- yi * yj
pm <- matrix(yiyj, ncol=n)
pmw <- pm * wm
spmw <- sum(pmw)
smw <- sum(wm)
sw  <- spmw / smw
vr <- n / sum(dy^2)
MI <- vr * sw
ms <- cbind(id=rep(1:n, each=n), y=rep(y, each=n), value=as.vector(wm * y))
ms <- ms[ms[,3] > 0, ]
ams <- aggregate(ms[,2:3], list(ms[,1]), FUN=mean)
ams <- ams[,-1]
colnames(ams) <- c('y', 'Defasagem espacial')
G_HOSP_3 = plot_ly(ams, x = ~y, y = ~`Defasagem espacial`, type = "scatter", mode = "markers") %>%
  layout(showlegend = F)%>%
  add_trace(data = ams, x = y, y = c(as.numeric(unlist(lm(ams[,2] ~ ams[,1])[1]))[1]   +   as.numeric(unlist(lm(ams[,2] ~ ams[,1])[1]))[2]*y), mode = "lines")%>%
  layout(title = 'diagrama de dispersão de Moran', plot_bgcolor = "#f0f0f0", xaxis = list(title = paste("HOSP_3")), yaxis = list(title = 'Defasagem espacial'))
reg <- lm(ams[,2] ~ ams[,1])
T_HOSP_3 = data.frame(Moran_I = MI,COEF = as.numeric(coefficients(reg)[2]))
############


ui = navbarPage("SIVEP-Gripe",theme = shinytheme("flatly"), #shinytheme("cerulean"),#theme = shinytheme("slate"),

                tags$head(
                  tags$style("
             body {
    margin-left: 70px;
    margin-right: 70px;
    margin-bottom: 100px;
             }     



.navbar-default {
    background-color: #166bc1;
    margin-left: -70px;
    margin-right: -70px;
}
                  
.navbar-nav {
    float: right;
    margin: 0;

}
                  



                ")),
                
                

                
#PRIMEIRA ABA######################                          
           tabPanel("Painel Principal",
                    fluidRow(


                      h2(HTML("<b>Painel SIVEP-Gripe</b>")),
                      h4(div(HTML("Análise dos dados de Síndrome Respiratória Aguda Grave <br> do SIVEP-Gripe - incluindo dados da COVID-19 2021-2022"), style = "color:gray")),

                      selectInput("totais", h3("Unidade Federativa"),
                                  choices = list("TODOS" = 'Todas', "Acre" = 'AC',"ALAGOAS"	="AL",
                                                 "AMAZONAS"=	"AM",
                                                 "AMAPÁ"=	"AP",
                                                 "BAHIA"	="BA",
                                                 "CEARÁ"=	"CE",
                                                 "DISTRITO FEDERAL"=	"DF",
                                                 "ESPÍRITO SANTO"=	"ES",
                                                 "GOIÁS"=	"GO",
                                                 "MARANHÃO"	="MA",
                                                 "MINAS GERAIS"=	"MG",
                                                 "MATO GROSSO DO SUL"="MS",
                                                 "MATO GROSSO"=	"MT",
                                                 "PARÁ"	="PA",
                                                 "PARAÍBA"	="PB",
                                                 "PERNANBUCO"	="PE",
                                                 "PIAUÍ"=	"PI",
                                                 "PARANÁ"	="PR",
                                                 "RIO DE JANEIRO"=	"RJ",
                                                 "RIO GRANDE DO NORTE"=	"RN",
                                                 "RONDÔNIA"=	"RO",
                                                 "RORAIMA"=	"RR",
                                                 "RIO GRANDE DO SUL"=	"RS",
                                                 "SANTA CATARINA"	="SC",
                                                 "SERGIPE"=	"SE",
                                                 "SÃO PAULO"=	"SP",
                                                 "TOCANTINS"	="TO"), selected = 1),

                      
                      h3(textOutput("selected_var"), align = 'center'),
                      fluidRow(tableOutput("table"), align = 'center'),
                      
                      
                      
          
                      
                      fluidRow(
                        
                      column(width = 4,
                      h3(textOutput("selected_var1"), align = 'center'),
                      fluidRow(tableOutput("table1"), align = 'center')),
                      
                      column(width = 4,
                      h3(textOutput("selected_var2"), align = 'center'),
                      fluidRow(tableOutput("table2"), align = 'center')),
                      
                      column(width = 4,
                      h3(textOutput("selected_varR"), align = 'center'),
                      fluidRow(tableOutput("tableR"), align = 'center')),
                      ),
                      h6(div("Dados top doses de 2021", style = "color:gray")),
                      
                      fluidRow(column(width = 12,h3(checkboxInput("checkbox2", label = "Visualização por semana", value = F)))),
                      
                      
                      conditionalPanel(condition = "(input.checkbox2 == false)",fluidRow(column(width = 12,highchartOutput('obts_dose',height=600),  align = 'center'))),
                      conditionalPanel(condition = "(input.checkbox2 == true)",fluidRow(column(width = 12,highchartOutput('obts_dose_sem',height=600),  align = 'center'))),
                      
                      #conditionalPanel(condition = "(input.checkbox2 == false)",fluidRow(column(width = 1),column(width = 10,highchartOutput('uti_dose',height=400),  align = 'center')),column(width = 1)),
                      #conditionalPanel(condition = "(input.checkbox2 == true)",fluidRow(column(width = 1),column(width = 10,highchartOutput('uti_dose_sem',height=400),  align = 'center')),column(width = 1)),
                      

                      fluidRow(column(width = 12, selectInput("map", h3("Síntese de :"),
                                         choices = list("LETALIDADE %" = 'LETALIDADE', "INCIDENCIA/100mil hab." = 'INCIDENCIA',
                                                        "MORTALIDADE/100mil hab." = 'MORTALIDADE'#,
                                                        #"PRIMEIRA DOSE %" = "%DOSE 1", "SEGUNDA DOSE %" ="%DOSE 2" ,"REFORÇO DOSE %" ="%DOSE R"
                                                        ), selected = 2))),
                      
                      
                      
                      fluidRow(column(width = 12,highchartOutput('mapp', height=600),  align = 'center',h6(div("Dados mapa de 2021", style = "color:gray")))),
                      
                      
                      
                      
                      
                      fluidRow(column(width = 12,h3(checkboxInput("checkbox", label = "Visualização por semana", value = F)))),
                      
                      #fluidRow(column(width = 12,h3(checkboxInput("checkbox", label = "Habilitar visualização por semana epidemiológica", value = F))), align = 'center'),
                      
                      
                      
                      
               
                      fluidRow(
                        column(width = 3, h3(HTML("Hospitalizações:</br>"),textOutput("selected_hos"),HTML("</br>Total:</br>"),textOutput("text_hos"))),
                        column(width = 9,
                        #       h3(textOutput("selected_var1"), align = 'center'), 
                               fluidRow( conditionalPanel(condition = "(input.checkbox == true)",highchartOutput('hosp_sem',height=400)),
                        conditionalPanel(condition = "(input.checkbox == false)",highchartOutput('hosp',height=400)))
                        ),
                        
                        column(width = 3, h3(HTML("Óbitos:</br>"),textOutput("selected_obt"),HTML("</br>Total:</br>"),textOutput("text_obt"))),
                        column(width = 9,
                         #      h3(textOutput("selected_var1"), align = 'center'), 
                               fluidRow(  conditionalPanel(condition = "(input.checkbox == true)",highchartOutput('obt_sem',height=400)),
                         conditionalPanel(condition = "(input.checkbox == false)",highchartOutput('obt',height=400)))
                         ),
                        
                        
                      )
                      
                      
                      
                      
                      
                    )

           ),
#############           
#SEGUNDA ABA####

tabPanel("Experimentos",
         fluidRow(
           h2(HTML("<b>Painel SIVEP-Gripe</b>")),
           h4(div(HTML("Análise dos dados de Síndrome Respiratória Aguda Grave <br> do SIVEP-Gripe - incluindo dados da COVID-19 2021-2022"), style = "color:gray")),
           
           
           fluidRow(column(width = 4,selectInput("locais", h3("Unidade Federativa"),
                       choices = list("TODOS" = 'Todas', "Acre" = 'AC',"ALAGOAS"	="AL",
                                      "AMAZONAS"=	"AM",
                                      "AMAPÁ"=	"AP",
                                      "BAHIA"	="BA",
                                      "CEARÁ"=	"CE",
                                      "DISTRITO FEDERAL"=	"DF",
                                      "ESPÍRITO SANTO"=	"ES",
                                      "GOIÁS"=	"GO",
                                      "MARANHÃO"	="MA",
                                      "MINAS GERAIS"=	"MG",
                                      "MATO GROSSO DO SUL"="MS",
                                      "MATO GROSSO"=	"MT",
                                      "PARÁ"	="PA",
                                      "PARAÍBA"	="PB",
                                      "PERNANBUCO"	="PE",
                                      "PIAUÍ"=	"PI",
                                      "PARANÁ"	="PR",
                                      "RIO DE JANEIRO"=	"RJ",
                                      "RIO GRANDE DO NORTE"=	"RN",
                                      "RONDÔNIA"=	"RO",
                                      "RORAIMA"=	"RR",
                                      "RIO GRANDE DO SUL"=	"RS",
                                      "SANTA CATARINA"	="SC",
                                      "SERGIPE"=	"SE",
                                      "SÃO PAULO"=	"SP",
                                      "TOCANTINS"	="TO"), selected = 1)),
                    
                    #column(width = 6, selectInput("doses", h3("Doses aplicadas :"),
                    #                               choices = list("Zero" = 'ZR', "Uma" = 'UMA',
                    #                                              "Duas" = 'DAS', "Três" = 'TRS'
                    #                               ), selected = 1))
                    
                    
              
                    
                
           
 
           column(width = 4,
           selectInput("doses",h3("Nº de doses tomadas"),
                       
                       c(Vazio = "vazio", Zero_doses = "ZR", Uma_dose = "UMA", Duas_doses = "DAS", Três_doses = "TRS"))),

           
           
           column(width = 4,
                           selectInput("tempo",h3("Frequência dos dados"),
                                       
                                       c(Vazio = "vazio", Dia = "dia", Semana = "semana", Ano = "ano"))),
                    
           
           
           ),
           

 
           h3("Estado do internado"),
           fluidRow(
             #column(width = 1),
                    column(width = 4,h3(checkboxInput("uti", label = "UTI", value = F))),
                    column(width = 4,h3(checkboxInput("intubado", label = "Intubado", value = F))),
                    column(width = 4,h3(checkboxInput("obito", label = "Óbito", value = F))),
            #column(width = 1)
            ),
           fluidRow(#column(width = 1),
                    column(width = 12,h6(div("Selecione de nenhuma a todas as opções acima", style = "color:gray")))),
           
           fluidRow(
             column(width = 4,h3(checkboxInput("regibox", label = "Habilitar região", value = F))),
             column(width = 4,selectInput("regioes", h3("Regiões"),
                                                 choices = list("TODOS" = 'Todas', 
                                                                "Sul" = 'Sul',
                                                                "Sudeste"=	"Sudeste",
                                                                "Centro-Oeste"=	"Centro-Oeste",
                                                                "Nordeste"	="Nordeste",
                                                                "Norte"=	"Norte"), selected = 1))),
           
           

######tabelas#####
conditionalPanel(condition = "(input.tempo == 'vazio' && input.doses == 'ZR' && input.uti == false && input.intubado == false  && input.obito == false && input.regibox == false)",
                column(width = 12,tableOutput('T_ZR'),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'vazio' && input.doses == 'UMA' && input.uti == false && input.intubado == false  && input.obito == false && input.regibox == false)",
                column(width = 12,tableOutput('T_UMA'),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'vazio' && input.doses == 'DAS' && input.uti == false && input.intubado == false  && input.obito == false && input.regibox == false)",
                column(width = 12,tableOutput('T_DAS'),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'vazio' && input.doses == 'TRS' && input.uti == false && input.intubado == false  && input.obito == false && input.regibox == false)",
                column(width = 12,tableOutput('T_TRS'),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'vazio' && input.doses == 'ZR' && input.uti == true && input.intubado == false  && input.obito == false && input.regibox == false)",
                column(width = 12,tableOutput('T_UTI_ZR'),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'vazio' && input.doses == 'UMA' && input.uti == true && input.intubado == false  && input.obito == false && input.regibox == false)",
                column(width = 12,tableOutput('T_UTI_UMA'),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'vazio' && input.doses == 'DAS' && input.uti == true && input.intubado == false  && input.obito == false && input.regibox == false)",
                column(width = 12,tableOutput('T_UTI_DAS'),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'vazio' && input.doses == 'TRS' && input.uti == true && input.intubado == false  && input.obito == false && input.regibox == false)",
                column(width = 12,tableOutput('T_UTI_TRS'),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'vazio' && input.doses == 'ZR' && input.uti == false && input.intubado == true  && input.obito == false && input.regibox == false)",
                column(width = 12,tableOutput('T_INT_ZR'),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'vazio' && input.doses == 'UMA' && input.uti == false && input.intubado == true  && input.obito == false && input.regibox == false)",
                column(width = 12,tableOutput('T_INT_UMA'),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'vazio' && input.doses == 'DAS' && input.uti == false && input.intubado == true  && input.obito == false && input.regibox == false)",
                column(width = 12,tableOutput('T_INT_DAS'),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'vazio' && input.doses == 'TRS' && input.uti == false && input.intubado == true  && input.obito == false && input.regibox == false)",
                column(width = 12,tableOutput('T_INT_TRS'),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'vazio' && input.doses == 'ZR' && input.uti == false && input.intubado == false  && input.obito == true && input.regibox == false)",
                column(width = 12,tableOutput('T_OBT_ZR'),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'vazio' && input.doses == 'UMA' && input.uti == false && input.intubado == false  && input.obito == true && input.regibox == false)",
                column(width = 12,tableOutput('T_OBT_UMA'),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'vazio' && input.doses == 'DAS' && input.uti == false && input.intubado == false  && input.obito == true && input.regibox == false)",
                column(width = 12,tableOutput('T_OBT_DAS'),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'vazio' && input.doses == 'TRS' && input.uti == false && input.intubado == false  && input.obito == true && input.regibox == false)",
                column(width = 12,tableOutput('T_OBT_TRS'),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'vazio' && input.doses == 'ZR' && input.uti == true && input.intubado == false  && input.obito == true && input.regibox == false)",
                column(width = 12,tableOutput('T_UTI_OBT_ZR'),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'vazio' && input.doses == 'UMA' && input.uti == true && input.intubado == false  && input.obito == true && input.regibox == false)",
                column(width = 12,tableOutput('T_UTI_OBT_UMA'),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'vazio' && input.doses == 'DAS' && input.uti == true && input.intubado == false  && input.obito == true && input.regibox == false)",
                column(width = 12,tableOutput('T_UTI_OBT_DAS'),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'vazio' && input.doses == 'TRS' && input.uti == true && input.intubado == false  && input.obito == true && input.regibox == false)",
                column(width = 12,tableOutput('T_UTI_OBT_TRS'),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'vazio' && input.doses == 'ZR' && input.uti == false && input.intubado == true  && input.obito == true && input.regibox == false)",
                column(width = 12,tableOutput('T_INT_OBT_ZR'),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'vazio' && input.doses == 'UMA' && input.uti == false && input.intubado == true  && input.obito == true && input.regibox == false)",
                column(width = 12,tableOutput('T_INT_OBT_UMA'),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'vazio' && input.doses == 'DAS' && input.uti == false && input.intubado == true  && input.obito == true && input.regibox == false)",
                column(width = 12,tableOutput('T_INT_OBT_DAS'),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'vazio' && input.doses == 'TRS' && input.uti == false && input.intubado == true  && input.obito == true && input.regibox == false)",
                column(width = 12,tableOutput('T_INT_OBT_TRS'),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'vazio' && input.doses == 'ZR' && input.uti == true && input.intubado == true  && input.obito == false && input.regibox == false)",
                column(width = 12,tableOutput('T_UTI_INT_ZR'),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'vazio' && input.doses == 'UMA' && input.uti == true && input.intubado == true  && input.obito == false && input.regibox == false)",
                column(width = 12,tableOutput('T_UTI_INT_UMA'),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'vazio' && input.doses == 'DAS' && input.uti == true && input.intubado == true  && input.obito == false && input.regibox == false)",
                column(width = 12,tableOutput('T_UTI_INT_DAS'),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'vazio' && input.doses == 'TRS' && input.uti == true && input.intubado == true  && input.obito == false && input.regibox == false)",
                column(width = 12,tableOutput('T_UTI_INT_TRS'),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'vazio' && input.doses == 'ZR' && input.uti == true && input.intubado == true  && input.obito == true && input.regibox == false)",
                column(width = 12,tableOutput('T_UTI_INT_OBT_ZR'),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'vazio' && input.doses == 'UMA' && input.uti == true && input.intubado == true  && input.obito == true && input.regibox == false)",
                column(width = 12,tableOutput('T_UTI_INT_OBT_UMA'),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'vazio' && input.doses == 'DAS' && input.uti == true && input.intubado == true  && input.obito == true && input.regibox == false)",
                column(width = 12,tableOutput('T_UTI_INT_OBT_DAS'),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'vazio' && input.doses == 'TRS' && input.uti == true && input.intubado == true  && input.obito == true && input.regibox == false)",
                column(width = 12,tableOutput('T_UTI_INT_OBT_TRS'),  align = 'center')),

######Dias#### 

conditionalPanel(condition = "(input.tempo == 'dia' && input.doses == 'ZR' && input.uti == false && input.intubado == false  && input.obito == false && input.regibox == false)",
                column(width = 12,highchartOutput('G_D_ZR',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'dia' && input.doses == 'UMA' && input.uti == false && input.intubado == false  && input.obito == false && input.regibox == false)",
                column(width = 12,highchartOutput('G_D_UMA',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'dia' && input.doses == 'DAS' && input.uti == false && input.intubado == false  && input.obito == false && input.regibox == false)",
                column(width = 12,highchartOutput('G_D_DAS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'dia' && input.doses == 'TRS' && input.uti == false && input.intubado == false  && input.obito == false && input.regibox == false)",
                column(width = 12,highchartOutput('G_D_TRS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'dia' && input.doses == 'ZR' && input.uti == true && input.intubado == false  && input.obito == false && input.regibox == false)",
                column(width = 12,highchartOutput('G_D_UTI_ZR',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'dia' && input.doses == 'UMA' && input.uti == true && input.intubado == false  && input.obito == false && input.regibox == false)",
                column(width = 12,highchartOutput('G_D_UTI_UMA',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'dia' && input.doses == 'DAS' && input.uti == true && input.intubado == false  && input.obito == false && input.regibox == false)",
                column(width = 12,highchartOutput('G_D_UTI_DAS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'dia' && input.doses == 'TRS' && input.uti == true && input.intubado == false  && input.obito == false && input.regibox == false)",
                column(width = 12,highchartOutput('G_D_UTI_TRS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'dia' && input.doses == 'ZR' && input.uti == false && input.intubado == true  && input.obito == false && input.regibox == false)",
                column(width = 12,highchartOutput('G_D_INT_ZR',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'dia' && input.doses == 'UMA' && input.uti == false && input.intubado == true  && input.obito == false && input.regibox == false)",
                column(width = 12,highchartOutput('G_D_INT_UMA',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'dia' && input.doses == 'DAS' && input.uti == false && input.intubado == true  && input.obito == false && input.regibox == false)",
                column(width = 12,highchartOutput('G_D_INT_DAS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'dia' && input.doses == 'TRS' && input.uti == false && input.intubado == true  && input.obito == false && input.regibox == false)",
                column(width = 12,highchartOutput('G_D_INT_TRS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'dia' && input.doses == 'ZR' && input.uti == false && input.intubado == false  && input.obito == true && input.regibox == false)",
                column(width = 12,highchartOutput('G_D_OBT_ZR',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'dia' && input.doses == 'UMA' && input.uti == false && input.intubado == false  && input.obito == true && input.regibox == false)",
                column(width = 12,highchartOutput('G_D_OBT_UMA',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'dia' && input.doses == 'DAS' && input.uti == false && input.intubado == false  && input.obito == true && input.regibox == false)",
                column(width = 12,highchartOutput('G_D_OBT_DAS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'dia' && input.doses == 'TRS' && input.uti == false && input.intubado == false  && input.obito == true && input.regibox == false)",
                column(width = 12,highchartOutput('G_D_OBT_TRS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'dia' && input.doses == 'ZR' && input.uti == true && input.intubado == false  && input.obito == true && input.regibox == false)",
                column(width = 12,highchartOutput('G_D_UTI_OBT_ZR',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'dia' && input.doses == 'UMA' && input.uti == true && input.intubado == false  && input.obito == true && input.regibox == false)",
                column(width = 12,highchartOutput('G_D_UTI_OBT_UMA',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'dia' && input.doses == 'DAS' && input.uti == true && input.intubado == false  && input.obito == true && input.regibox == false)",
                column(width = 12,highchartOutput('G_D_UTI_OBT_DAS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'dia' && input.doses == 'TRS' && input.uti == true && input.intubado == false  && input.obito == true && input.regibox == false)",
                column(width = 12,highchartOutput('G_D_UTI_OBT_TRS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'dia' && input.doses == 'ZR' && input.uti == false && input.intubado == true  && input.obito == true && input.regibox == false)",
                column(width = 12,highchartOutput('G_D_INT_OBT_ZR',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'dia' && input.doses == 'UMA' && input.uti == false && input.intubado == true  && input.obito == true && input.regibox == false)",
                column(width = 12,highchartOutput('G_D_INT_OBT_UMA',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'dia' && input.doses == 'DAS' && input.uti == false && input.intubado == true  && input.obito == true && input.regibox == false)",
                column(width = 12,highchartOutput('G_D_INT_OBT_DAS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'dia' && input.doses == 'TRS' && input.uti == false && input.intubado == true  && input.obito == true && input.regibox == false)",
                column(width = 12,highchartOutput('G_D_INT_OBT_TRS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'dia' && input.doses == 'ZR' && input.uti == true && input.intubado == true  && input.obito == false && input.regibox == false)",
                column(width = 12,highchartOutput('G_D_UTI_INT_ZR',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'dia' && input.doses == 'UMA' && input.uti == true && input.intubado == true  && input.obito == false && input.regibox == false)",
                column(width = 12,highchartOutput('G_D_UTI_INT_UMA',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'dia' && input.doses == 'DAS' && input.uti == true && input.intubado == true  && input.obito == false && input.regibox == false)",
                column(width = 12,highchartOutput('G_D_UTI_INT_DAS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'dia' && input.doses == 'TRS' && input.uti == true && input.intubado == true  && input.obito == false && input.regibox == false)",
                column(width = 12,highchartOutput('G_D_UTI_INT_TRS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'dia' && input.doses == 'ZR' && input.uti == true && input.intubado == true  && input.obito == true && input.regibox == false)",
                column(width = 12,highchartOutput('G_D_UTI_INT_OBT_ZR',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'dia' && input.doses == 'UMA' && input.uti == true && input.intubado == true  && input.obito == true && input.regibox == false)",
                column(width = 12,highchartOutput('G_D_UTI_INT_OBT_UMA',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'dia' && input.doses == 'DAS' && input.uti == true && input.intubado == true  && input.obito == true && input.regibox == false)",
                column(width = 12,highchartOutput('G_D_UTI_INT_OBT_DAS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'dia' && input.doses == 'TRS' && input.uti == true && input.intubado == true  && input.obito == true && input.regibox == false)",
                column(width = 12,highchartOutput('G_D_UTI_INT_OBT_TRS',height=600),  align = 'center')),





#####Semana####
conditionalPanel(condition = "(input.tempo == 'semana' && input.doses == 'ZR' && input.uti == false && input.intubado == false  && input.obito == false && input.regibox == false)",
                column(width = 12,highchartOutput('G_S_ZR',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'semana' && input.doses == 'UMA' && input.uti == false && input.intubado == false  && input.obito == false && input.regibox == false)",
                column(width = 12,highchartOutput('G_S_UMA',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'semana' && input.doses == 'DAS' && input.uti == false && input.intubado == false  && input.obito == false && input.regibox == false)",
                column(width = 12,highchartOutput('G_S_DAS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'semana' && input.doses == 'TRS' && input.uti == false && input.intubado == false  && input.obito == false && input.regibox == false)",
                column(width = 12,highchartOutput('G_S_TRS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'semana' && input.doses == 'ZR' && input.uti == true && input.intubado == false  && input.obito == false && input.regibox == false)",
                column(width = 12,highchartOutput('G_S_UTI_ZR',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'semana' && input.doses == 'UMA' && input.uti == true && input.intubado == false  && input.obito == false && input.regibox == false)",
                column(width = 12,highchartOutput('G_S_UTI_UMA',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'semana' && input.doses == 'DAS' && input.uti == true && input.intubado == false  && input.obito == false && input.regibox == false)",
                column(width = 12,highchartOutput('G_S_UTI_DAS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'semana' && input.doses == 'TRS' && input.uti == true && input.intubado == false  && input.obito == false && input.regibox == false)",
                column(width = 12,highchartOutput('G_S_UTI_TRS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'semana' && input.doses == 'ZR' && input.uti == false && input.intubado == true  && input.obito == false && input.regibox == false)",
                column(width = 12,highchartOutput('G_S_INT_ZR',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'semana' && input.doses == 'UMA' && input.uti == false && input.intubado == true  && input.obito == false && input.regibox == false)",
                column(width = 12,highchartOutput('G_S_INT_UMA',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'semana' && input.doses == 'DAS' && input.uti == false && input.intubado == true  && input.obito == false && input.regibox == false)",
                column(width = 12,highchartOutput('G_S_INT_DAS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'semana' && input.doses == 'TRS' && input.uti == false && input.intubado == true  && input.obito == false && input.regibox == false)",
                column(width = 12,highchartOutput('G_S_INT_TRS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'semana' && input.doses == 'ZR' && input.uti == false && input.intubado == false  && input.obito == true && input.regibox == false)",
                column(width = 12,highchartOutput('G_S_OBT_ZR',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'semana' && input.doses == 'UMA' && input.uti == false && input.intubado == false  && input.obito == true && input.regibox == false)",
                column(width = 12,highchartOutput('G_S_OBT_UMA',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'semana' && input.doses == 'DAS' && input.uti == false && input.intubado == false  && input.obito == true && input.regibox == false)",
                column(width = 12,highchartOutput('G_S_OBT_DAS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'semana' && input.doses == 'TRS' && input.uti == false && input.intubado == false  && input.obito == true && input.regibox == false)",
                column(width = 12,highchartOutput('G_S_OBT_TRS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'semana' && input.doses == 'ZR' && input.uti == true && input.intubado == false  && input.obito == true && input.regibox == false)",
                column(width = 12,highchartOutput('G_S_UTI_OBT_ZR',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'semana' && input.doses == 'UMA' && input.uti == true && input.intubado == false  && input.obito == true && input.regibox == false)",
                column(width = 12,highchartOutput('G_S_UTI_OBT_UMA',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'semana' && input.doses == 'DAS' && input.uti == true && input.intubado == false  && input.obito == true && input.regibox == false)",
                column(width = 12,highchartOutput('G_S_UTI_OBT_DAS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'semana' && input.doses == 'TRS' && input.uti == true && input.intubado == false  && input.obito == true && input.regibox == false)",
                column(width = 12,highchartOutput('G_S_UTI_OBT_TRS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'semana' && input.doses == 'ZR' && input.uti == false && input.intubado == true  && input.obito == true && input.regibox == false)",
                column(width = 12,highchartOutput('G_S_INT_OBT_ZR',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'semana' && input.doses == 'UMA' && input.uti == false && input.intubado == true  && input.obito == true && input.regibox == false)",
                column(width = 12,highchartOutput('G_S_INT_OBT_UMA',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'semana' && input.doses == 'DAS' && input.uti == false && input.intubado == true  && input.obito == true && input.regibox == false)",
                column(width = 12,highchartOutput('G_S_INT_OBT_DAS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'semana' && input.doses == 'TRS' && input.uti == false && input.intubado == true  && input.obito == true && input.regibox == false)",
                column(width = 12,highchartOutput('G_S_INT_OBT_TRS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'semana' && input.doses == 'ZR' && input.uti == true && input.intubado == true  && input.obito == false && input.regibox == false)",
                column(width = 12,highchartOutput('G_S_UTI_INT_ZR',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'semana' && input.doses == 'UMA' && input.uti == true && input.intubado == true  && input.obito == false && input.regibox == false)",
                column(width = 12,highchartOutput('G_S_UTI_INT_UMA',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'semana' && input.doses == 'DAS' && input.uti == true && input.intubado == true  && input.obito == false && input.regibox == false)",
                column(width = 12,highchartOutput('G_S_UTI_INT_DAS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'semana' && input.doses == 'TRS' && input.uti == true && input.intubado == true  && input.obito == false && input.regibox == false)",
                column(width = 12,highchartOutput('G_S_UTI_INT_TRS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'semana' && input.doses == 'ZR' && input.uti == true && input.intubado == true  && input.obito == true && input.regibox == false)",
                column(width = 12,highchartOutput('G_S_UTI_INT_OBT_ZR',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'semana' && input.doses == 'UMA' && input.uti == true && input.intubado == true  && input.obito == true && input.regibox == false)",
                column(width = 12,highchartOutput('G_S_UTI_INT_OBT_UMA',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'semana' && input.doses == 'DAS' && input.uti == true && input.intubado == true  && input.obito == true && input.regibox == false)",
                column(width = 12,highchartOutput('G_S_UTI_INT_OBT_DAS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'semana' && input.doses == 'TRS' && input.uti == true && input.intubado == true  && input.obito == true && input.regibox == false)",
                column(width = 12,highchartOutput('G_S_UTI_INT_OBT_TRS',height=600),  align = 'center')),


#####Ano####
conditionalPanel(condition = "(input.tempo == 'ano' && input.doses == 'ZR' && input.uti == false && input.intubado == false  && input.obito == false && input.regibox == false)",
                column(width = 12,highchartOutput('G_A_ZR',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'ano' && input.doses == 'UMA' && input.uti == false && input.intubado == false  && input.obito == false && input.regibox == false)",
                column(width = 12,highchartOutput('G_A_UMA',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'ano' && input.doses == 'DAS' && input.uti == false && input.intubado == false  && input.obito == false && input.regibox == false)",
                column(width = 12,highchartOutput('G_A_DAS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'ano' && input.doses == 'TRS' && input.uti == false && input.intubado == false  && input.obito == false && input.regibox == false)",
                column(width = 12,highchartOutput('G_A_TRS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'ano' && input.doses == 'ZR' && input.uti == true && input.intubado == false  && input.obito == false && input.regibox == false)",
                column(width = 12,highchartOutput('G_A_UTI_ZR',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'ano' && input.doses == 'UMA' && input.uti == true && input.intubado == false  && input.obito == false && input.regibox == false)",
                column(width = 12,highchartOutput('G_A_UTI_UMA',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'ano' && input.doses == 'DAS' && input.uti == true && input.intubado == false  && input.obito == false && input.regibox == false)",
                column(width = 12,highchartOutput('G_A_UTI_DAS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'ano' && input.doses == 'TRS' && input.uti == true && input.intubado == false  && input.obito == false && input.regibox == false)",
                column(width = 12,highchartOutput('G_A_UTI_TRS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'ano' && input.doses == 'ZR' && input.uti == false && input.intubado == true  && input.obito == false && input.regibox == false)",
                column(width = 12,highchartOutput('G_A_INT_ZR',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'ano' && input.doses == 'UMA' && input.uti == false && input.intubado == true  && input.obito == false && input.regibox == false)",
                column(width = 12,highchartOutput('G_A_INT_UMA',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'ano' && input.doses == 'DAS' && input.uti == false && input.intubado == true  && input.obito == false && input.regibox == false)",
                column(width = 12,highchartOutput('G_A_INT_DAS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'ano' && input.doses == 'TRS' && input.uti == false && input.intubado == true  && input.obito == false && input.regibox == false)",
                column(width = 12,highchartOutput('G_A_INT_TRS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'ano' && input.doses == 'ZR' && input.uti == false && input.intubado == false  && input.obito == true && input.regibox == false)",
                column(width = 12,highchartOutput('G_A_OBT_ZR',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'ano' && input.doses == 'UMA' && input.uti == false && input.intubado == false  && input.obito == true && input.regibox == false)",
                column(width = 12,highchartOutput('G_A_OBT_UMA',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'ano' && input.doses == 'DAS' && input.uti == false && input.intubado == false  && input.obito == true && input.regibox == false)",
                column(width = 12,highchartOutput('G_A_OBT_DAS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'ano' && input.doses == 'TRS' && input.uti == false && input.intubado == false  && input.obito == true && input.regibox == false)",
                column(width = 12,highchartOutput('G_A_OBT_TRS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'ano' && input.doses == 'ZR' && input.uti == true && input.intubado == false  && input.obito == true && input.regibox == false)",
                column(width = 12,highchartOutput('G_A_UTI_OBT_ZR',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'ano' && input.doses == 'UMA' && input.uti == true && input.intubado == false  && input.obito == true && input.regibox == false)",
                column(width = 12,highchartOutput('G_A_UTI_OBT_UMA',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'ano' && input.doses == 'DAS' && input.uti == true && input.intubado == false  && input.obito == true && input.regibox == false)",
                column(width = 12,highchartOutput('G_A_UTI_OBT_DAS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'ano' && input.doses == 'TRS' && input.uti == true && input.intubado == false  && input.obito == true && input.regibox == false)",
                column(width = 12,highchartOutput('G_A_UTI_OBT_TRS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'ano' && input.doses == 'ZR' && input.uti == false && input.intubado == true  && input.obito == true && input.regibox == false)",
                column(width = 12,highchartOutput('G_A_INT_OBT_ZR',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'ano' && input.doses == 'UMA' && input.uti == false && input.intubado == true  && input.obito == true && input.regibox == false)",
                column(width = 12,highchartOutput('G_A_INT_OBT_UMA',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'ano' && input.doses == 'DAS' && input.uti == false && input.intubado == true  && input.obito == true && input.regibox == false)",
                column(width = 12,highchartOutput('G_A_INT_OBT_DAS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'ano' && input.doses == 'TRS' && input.uti == false && input.intubado == true  && input.obito == true && input.regibox == false)",
                column(width = 12,highchartOutput('G_A_INT_OBT_TRS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'ano' && input.doses == 'ZR' && input.uti == true && input.intubado == true  && input.obito == false && input.regibox == false)",
                column(width = 12,highchartOutput('G_A_UTI_INT_ZR',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'ano' && input.doses == 'UMA' && input.uti == true && input.intubado == true  && input.obito == false && input.regibox == false)",
                column(width = 12,highchartOutput('G_A_UTI_INT_UMA',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'ano' && input.doses == 'DAS' && input.uti == true && input.intubado == true  && input.obito == false && input.regibox == false)",
                column(width = 12,highchartOutput('G_A_UTI_INT_DAS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'ano' && input.doses == 'TRS' && input.uti == true && input.intubado == true  && input.obito == false && input.regibox == false)",
                column(width = 12,highchartOutput('G_A_UTI_INT_TRS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'ano' && input.doses == 'ZR' && input.uti == true && input.intubado == true  && input.obito == true && input.regibox == false)",
                column(width = 12,highchartOutput('G_A_UTI_INT_OBT_ZR',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'ano' && input.doses == 'UMA' && input.uti == true && input.intubado == true  && input.obito == true && input.regibox == false)",
                column(width = 12,highchartOutput('G_A_UTI_INT_OBT_UMA',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'ano' && input.doses == 'DAS' && input.uti == true && input.intubado == true  && input.obito == true && input.regibox == false)",
                column(width = 12,highchartOutput('G_A_UTI_INT_OBT_DAS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'ano' && input.doses == 'TRS' && input.uti == true && input.intubado == true  && input.obito == true && input.regibox == false)",
                column(width = 12,highchartOutput('G_A_UTI_INT_OBT_TRS',height=600),  align = 'center')),



#######
######tabelas#####
conditionalPanel(condition = "(input.tempo == 'vazio' && input.doses == 'ZR' && input.uti == false && input.intubado == false  && input.obito == false && input.regibox == true)",
                column(width = 12,tableOutput('R_T_ZR'),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'vazio' && input.doses == 'UMA' && input.uti == false && input.intubado == false  && input.obito == false && input.regibox == true)",
                column(width = 12,tableOutput('R_T_UMA'),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'vazio' && input.doses == 'DAS' && input.uti == false && input.intubado == false  && input.obito == false && input.regibox == true)",
                column(width = 12,tableOutput('R_T_DAS'),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'vazio' && input.doses == 'TRS' && input.uti == false && input.intubado == false  && input.obito == false && input.regibox == true)",
                column(width = 12,tableOutput('R_T_TRS'),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'vazio' && input.doses == 'ZR' && input.uti == true && input.intubado == false  && input.obito == false && input.regibox == true)",
                column(width = 12,tableOutput('R_T_UTI_ZR'),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'vazio' && input.doses == 'UMA' && input.uti == true && input.intubado == false  && input.obito == false && input.regibox == true)",
                column(width = 12,tableOutput('R_T_UTI_UMA'),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'vazio' && input.doses == 'DAS' && input.uti == true && input.intubado == false  && input.obito == false && input.regibox == true)",
                column(width = 12,tableOutput('R_T_UTI_DAS'),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'vazio' && input.doses == 'TRS' && input.uti == true && input.intubado == false  && input.obito == false && input.regibox == true)",
                column(width = 12,tableOutput('R_T_UTI_TRS'),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'vazio' && input.doses == 'ZR' && input.uti == false && input.intubado == true  && input.obito == false && input.regibox == true)",
                column(width = 12,tableOutput('R_T_INT_ZR'),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'vazio' && input.doses == 'UMA' && input.uti == false && input.intubado == true  && input.obito == false && input.regibox == true)",
                column(width = 12,tableOutput('R_T_INT_UMA'),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'vazio' && input.doses == 'DAS' && input.uti == false && input.intubado == true  && input.obito == false && input.regibox == true)",
                column(width = 12,tableOutput('R_T_INT_DAS'),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'vazio' && input.doses == 'TRS' && input.uti == false && input.intubado == true  && input.obito == false && input.regibox == true)",
                column(width = 12,tableOutput('R_T_INT_TRS'),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'vazio' && input.doses == 'ZR' && input.uti == false && input.intubado == false  && input.obito == true && input.regibox == true)",
                column(width = 12,tableOutput('R_T_OBT_ZR'),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'vazio' && input.doses == 'UMA' && input.uti == false && input.intubado == false  && input.obito == true && input.regibox == true)",
                column(width = 12,tableOutput('R_T_OBT_UMA'),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'vazio' && input.doses == 'DAS' && input.uti == false && input.intubado == false  && input.obito == true && input.regibox == true)",
                column(width = 12,tableOutput('R_T_OBT_DAS'),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'vazio' && input.doses == 'TRS' && input.uti == false && input.intubado == false  && input.obito == true && input.regibox == true)",
                column(width = 12,tableOutput('R_T_OBT_TRS'),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'vazio' && input.doses == 'ZR' && input.uti == true && input.intubado == false  && input.obito == true && input.regibox == true)",
                column(width = 12,tableOutput('R_T_UTI_OBT_ZR'),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'vazio' && input.doses == 'UMA' && input.uti == true && input.intubado == false  && input.obito == true && input.regibox == true)",
                column(width = 12,tableOutput('R_T_UTI_OBT_UMA'),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'vazio' && input.doses == 'DAS' && input.uti == true && input.intubado == false  && input.obito == true && input.regibox == true)",
                column(width = 12,tableOutput('R_T_UTI_OBT_DAS'),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'vazio' && input.doses == 'TRS' && input.uti == true && input.intubado == false  && input.obito == true && input.regibox == true)",
                column(width = 12,tableOutput('R_T_UTI_OBT_TRS'),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'vazio' && input.doses == 'ZR' && input.uti == false && input.intubado == true  && input.obito == true && input.regibox == true)",
                column(width = 12,tableOutput('R_T_INT_OBT_ZR'),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'vazio' && input.doses == 'UMA' && input.uti == false && input.intubado == true  && input.obito == true && input.regibox == true)",
                column(width = 12,tableOutput('R_T_INT_OBT_UMA'),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'vazio' && input.doses == 'DAS' && input.uti == false && input.intubado == true  && input.obito == true && input.regibox == true)",
                column(width = 12,tableOutput('R_T_INT_OBT_DAS'),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'vazio' && input.doses == 'TRS' && input.uti == false && input.intubado == true  && input.obito == true && input.regibox == true)",
                column(width = 12,tableOutput('R_T_INT_OBT_TRS'),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'vazio' && input.doses == 'ZR' && input.uti == true && input.intubado == true  && input.obito == false && input.regibox == true)",
                column(width = 12,tableOutput('R_T_UTI_INT_ZR'),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'vazio' && input.doses == 'UMA' && input.uti == true && input.intubado == true  && input.obito == false && input.regibox == true)",
                column(width = 12,tableOutput('R_T_UTI_INT_UMA'),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'vazio' && input.doses == 'DAS' && input.uti == true && input.intubado == true  && input.obito == false && input.regibox == true)",
                column(width = 12,tableOutput('R_T_UTI_INT_DAS'),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'vazio' && input.doses == 'TRS' && input.uti == true && input.intubado == true  && input.obito == false && input.regibox == true)",
                column(width = 12,tableOutput('R_T_UTI_INT_TRS'),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'vazio' && input.doses == 'ZR' && input.uti == true && input.intubado == true  && input.obito == true && input.regibox == true)",
                column(width = 12,tableOutput('R_T_UTI_INT_OBT_ZR'),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'vazio' && input.doses == 'UMA' && input.uti == true && input.intubado == true  && input.obito == true && input.regibox == true)",
                column(width = 12,tableOutput('R_T_UTI_INT_OBT_UMA'),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'vazio' && input.doses == 'DAS' && input.uti == true && input.intubado == true  && input.obito == true && input.regibox == true)",
                column(width = 12,tableOutput('R_T_UTI_INT_OBT_DAS'),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'vazio' && input.doses == 'TRS' && input.uti == true && input.intubado == true  && input.obito == true && input.regibox == true)",
                column(width = 12,tableOutput('R_T_UTI_INT_OBT_TRS'),  align = 'center')),

######Dias#### 

conditionalPanel(condition = "(input.tempo == 'dia' && input.doses == 'ZR' && input.uti == false && input.intubado == false  && input.obito == false && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_D_ZR',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'dia' && input.doses == 'UMA' && input.uti == false && input.intubado == false  && input.obito == false && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_D_UMA',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'dia' && input.doses == 'DAS' && input.uti == false && input.intubado == false  && input.obito == false && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_D_DAS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'dia' && input.doses == 'TRS' && input.uti == false && input.intubado == false  && input.obito == false && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_D_TRS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'dia' && input.doses == 'ZR' && input.uti == true && input.intubado == false  && input.obito == false && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_D_UTI_ZR',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'dia' && input.doses == 'UMA' && input.uti == true && input.intubado == false  && input.obito == false && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_D_UTI_UMA',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'dia' && input.doses == 'DAS' && input.uti == true && input.intubado == false  && input.obito == false && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_D_UTI_DAS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'dia' && input.doses == 'TRS' && input.uti == true && input.intubado == false  && input.obito == false && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_D_UTI_TRS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'dia' && input.doses == 'ZR' && input.uti == false && input.intubado == true  && input.obito == false && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_D_INT_ZR',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'dia' && input.doses == 'UMA' && input.uti == false && input.intubado == true  && input.obito == false && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_D_INT_UMA',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'dia' && input.doses == 'DAS' && input.uti == false && input.intubado == true  && input.obito == false && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_D_INT_DAS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'dia' && input.doses == 'TRS' && input.uti == false && input.intubado == true  && input.obito == false && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_D_INT_TRS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'dia' && input.doses == 'ZR' && input.uti == false && input.intubado == false  && input.obito == true && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_D_OBT_ZR',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'dia' && input.doses == 'UMA' && input.uti == false && input.intubado == false  && input.obito == true && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_D_OBT_UMA',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'dia' && input.doses == 'DAS' && input.uti == false && input.intubado == false  && input.obito == true && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_D_OBT_DAS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'dia' && input.doses == 'TRS' && input.uti == false && input.intubado == false  && input.obito == true && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_D_OBT_TRS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'dia' && input.doses == 'ZR' && input.uti == true && input.intubado == false  && input.obito == true && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_D_UTI_OBT_ZR',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'dia' && input.doses == 'UMA' && input.uti == true && input.intubado == false  && input.obito == true && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_D_UTI_OBT_UMA',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'dia' && input.doses == 'DAS' && input.uti == true && input.intubado == false  && input.obito == true && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_D_UTI_OBT_DAS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'dia' && input.doses == 'TRS' && input.uti == true && input.intubado == false  && input.obito == true && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_D_UTI_OBT_TRS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'dia' && input.doses == 'ZR' && input.uti == false && input.intubado == true  && input.obito == true && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_D_INT_OBT_ZR',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'dia' && input.doses == 'UMA' && input.uti == false && input.intubado == true  && input.obito == true && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_D_INT_OBT_UMA',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'dia' && input.doses == 'DAS' && input.uti == false && input.intubado == true  && input.obito == true && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_D_INT_OBT_DAS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'dia' && input.doses == 'TRS' && input.uti == false && input.intubado == true  && input.obito == true && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_D_INT_OBT_TRS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'dia' && input.doses == 'ZR' && input.uti == true && input.intubado == true  && input.obito == false && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_D_UTI_INT_ZR',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'dia' && input.doses == 'UMA' && input.uti == true && input.intubado == true  && input.obito == false && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_D_UTI_INT_UMA',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'dia' && input.doses == 'DAS' && input.uti == true && input.intubado == true  && input.obito == false && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_D_UTI_INT_DAS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'dia' && input.doses == 'TRS' && input.uti == true && input.intubado == true  && input.obito == false && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_D_UTI_INT_TRS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'dia' && input.doses == 'ZR' && input.uti == true && input.intubado == true  && input.obito == true && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_D_UTI_INT_OBT_ZR',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'dia' && input.doses == 'UMA' && input.uti == true && input.intubado == true  && input.obito == true && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_D_UTI_INT_OBT_UMA',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'dia' && input.doses == 'DAS' && input.uti == true && input.intubado == true  && input.obito == true && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_D_UTI_INT_OBT_DAS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'dia' && input.doses == 'TRS' && input.uti == true && input.intubado == true  && input.obito == true && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_D_UTI_INT_OBT_TRS',height=600),  align = 'center')),





#####Semana####
conditionalPanel(condition = "(input.tempo == 'semana' && input.doses == 'ZR' && input.uti == false && input.intubado == false  && input.obito == false && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_S_ZR',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'semana' && input.doses == 'UMA' && input.uti == false && input.intubado == false  && input.obito == false && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_S_UMA',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'semana' && input.doses == 'DAS' && input.uti == false && input.intubado == false  && input.obito == false && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_S_DAS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'semana' && input.doses == 'TRS' && input.uti == false && input.intubado == false  && input.obito == false && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_S_TRS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'semana' && input.doses == 'ZR' && input.uti == true && input.intubado == false  && input.obito == false && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_S_UTI_ZR',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'semana' && input.doses == 'UMA' && input.uti == true && input.intubado == false  && input.obito == false && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_S_UTI_UMA',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'semana' && input.doses == 'DAS' && input.uti == true && input.intubado == false  && input.obito == false && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_S_UTI_DAS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'semana' && input.doses == 'TRS' && input.uti == true && input.intubado == false  && input.obito == false && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_S_UTI_TRS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'semana' && input.doses == 'ZR' && input.uti == false && input.intubado == true  && input.obito == false && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_S_INT_ZR',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'semana' && input.doses == 'UMA' && input.uti == false && input.intubado == true  && input.obito == false && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_S_INT_UMA',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'semana' && input.doses == 'DAS' && input.uti == false && input.intubado == true  && input.obito == false && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_S_INT_DAS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'semana' && input.doses == 'TRS' && input.uti == false && input.intubado == true  && input.obito == false && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_S_INT_TRS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'semana' && input.doses == 'ZR' && input.uti == false && input.intubado == false  && input.obito == true && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_S_OBT_ZR',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'semana' && input.doses == 'UMA' && input.uti == false && input.intubado == false  && input.obito == true && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_S_OBT_UMA',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'semana' && input.doses == 'DAS' && input.uti == false && input.intubado == false  && input.obito == true && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_S_OBT_DAS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'semana' && input.doses == 'TRS' && input.uti == false && input.intubado == false  && input.obito == true && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_S_OBT_TRS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'semana' && input.doses == 'ZR' && input.uti == true && input.intubado == false  && input.obito == true && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_S_UTI_OBT_ZR',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'semana' && input.doses == 'UMA' && input.uti == true && input.intubado == false  && input.obito == true && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_S_UTI_OBT_UMA',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'semana' && input.doses == 'DAS' && input.uti == true && input.intubado == false  && input.obito == true && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_S_UTI_OBT_DAS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'semana' && input.doses == 'TRS' && input.uti == true && input.intubado == false  && input.obito == true && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_S_UTI_OBT_TRS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'semana' && input.doses == 'ZR' && input.uti == false && input.intubado == true  && input.obito == true && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_S_INT_OBT_ZR',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'semana' && input.doses == 'UMA' && input.uti == false && input.intubado == true  && input.obito == true && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_S_INT_OBT_UMA',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'semana' && input.doses == 'DAS' && input.uti == false && input.intubado == true  && input.obito == true && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_S_INT_OBT_DAS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'semana' && input.doses == 'TRS' && input.uti == false && input.intubado == true  && input.obito == true && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_S_INT_OBT_TRS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'semana' && input.doses == 'ZR' && input.uti == true && input.intubado == true  && input.obito == false && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_S_UTI_INT_ZR',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'semana' && input.doses == 'UMA' && input.uti == true && input.intubado == true  && input.obito == false && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_S_UTI_INT_UMA',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'semana' && input.doses == 'DAS' && input.uti == true && input.intubado == true  && input.obito == false && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_S_UTI_INT_DAS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'semana' && input.doses == 'TRS' && input.uti == true && input.intubado == true  && input.obito == false && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_S_UTI_INT_TRS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'semana' && input.doses == 'ZR' && input.uti == true && input.intubado == true  && input.obito == true && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_S_UTI_INT_OBT_ZR',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'semana' && input.doses == 'UMA' && input.uti == true && input.intubado == true  && input.obito == true && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_S_UTI_INT_OBT_UMA',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'semana' && input.doses == 'DAS' && input.uti == true && input.intubado == true  && input.obito == true && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_S_UTI_INT_OBT_DAS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'semana' && input.doses == 'TRS' && input.uti == true && input.intubado == true  && input.obito == true && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_S_UTI_INT_OBT_TRS',height=600),  align = 'center')),


#####Ano####
conditionalPanel(condition = "(input.tempo == 'ano' && input.doses == 'ZR' && input.uti == false && input.intubado == false  && input.obito == false && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_A_ZR',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'ano' && input.doses == 'UMA' && input.uti == false && input.intubado == false  && input.obito == false && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_A_UMA',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'ano' && input.doses == 'DAS' && input.uti == false && input.intubado == false  && input.obito == false && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_A_DAS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'ano' && input.doses == 'TRS' && input.uti == false && input.intubado == false  && input.obito == false && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_A_TRS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'ano' && input.doses == 'ZR' && input.uti == true && input.intubado == false  && input.obito == false && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_A_UTI_ZR',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'ano' && input.doses == 'UMA' && input.uti == true && input.intubado == false  && input.obito == false && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_A_UTI_UMA',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'ano' && input.doses == 'DAS' && input.uti == true && input.intubado == false  && input.obito == false && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_A_UTI_DAS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'ano' && input.doses == 'TRS' && input.uti == true && input.intubado == false  && input.obito == false && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_A_UTI_TRS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'ano' && input.doses == 'ZR' && input.uti == false && input.intubado == true  && input.obito == false && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_A_INT_ZR',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'ano' && input.doses == 'UMA' && input.uti == false && input.intubado == true  && input.obito == false && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_A_INT_UMA',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'ano' && input.doses == 'DAS' && input.uti == false && input.intubado == true  && input.obito == false && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_A_INT_DAS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'ano' && input.doses == 'TRS' && input.uti == false && input.intubado == true  && input.obito == false && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_A_INT_TRS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'ano' && input.doses == 'ZR' && input.uti == false && input.intubado == false  && input.obito == true && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_A_OBT_ZR',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'ano' && input.doses == 'UMA' && input.uti == false && input.intubado == false  && input.obito == true && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_A_OBT_UMA',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'ano' && input.doses == 'DAS' && input.uti == false && input.intubado == false  && input.obito == true && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_A_OBT_DAS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'ano' && input.doses == 'TRS' && input.uti == false && input.intubado == false  && input.obito == true && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_A_OBT_TRS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'ano' && input.doses == 'ZR' && input.uti == true && input.intubado == false  && input.obito == true && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_A_UTI_OBT_ZR',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'ano' && input.doses == 'UMA' && input.uti == true && input.intubado == false  && input.obito == true && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_A_UTI_OBT_UMA',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'ano' && input.doses == 'DAS' && input.uti == true && input.intubado == false  && input.obito == true && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_A_UTI_OBT_DAS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'ano' && input.doses == 'TRS' && input.uti == true && input.intubado == false  && input.obito == true && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_A_UTI_OBT_TRS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'ano' && input.doses == 'ZR' && input.uti == false && input.intubado == true  && input.obito == true && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_A_INT_OBT_ZR',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'ano' && input.doses == 'UMA' && input.uti == false && input.intubado == true  && input.obito == true && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_A_INT_OBT_UMA',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'ano' && input.doses == 'DAS' && input.uti == false && input.intubado == true  && input.obito == true && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_A_INT_OBT_DAS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'ano' && input.doses == 'TRS' && input.uti == false && input.intubado == true  && input.obito == true && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_A_INT_OBT_TRS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'ano' && input.doses == 'ZR' && input.uti == true && input.intubado == true  && input.obito == false && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_A_UTI_INT_ZR',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'ano' && input.doses == 'UMA' && input.uti == true && input.intubado == true  && input.obito == false && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_A_UTI_INT_UMA',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'ano' && input.doses == 'DAS' && input.uti == true && input.intubado == true  && input.obito == false && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_A_UTI_INT_DAS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'ano' && input.doses == 'TRS' && input.uti == true && input.intubado == true  && input.obito == false && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_A_UTI_INT_TRS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'ano' && input.doses == 'ZR' && input.uti == true && input.intubado == true  && input.obito == true && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_A_UTI_INT_OBT_ZR',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'ano' && input.doses == 'UMA' && input.uti == true && input.intubado == true  && input.obito == true && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_A_UTI_INT_OBT_UMA',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'ano' && input.doses == 'DAS' && input.uti == true && input.intubado == true  && input.obito == true && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_A_UTI_INT_OBT_DAS',height=600),  align = 'center')),

conditionalPanel(condition = "(input.tempo == 'ano' && input.doses == 'TRS' && input.uti == true && input.intubado == true  && input.obito == true && input.regibox == true)",
                column(width = 12,highchartOutput('R_G_A_UTI_INT_OBT_TRS',height=600),  align = 'center'))



#########

         )#fluidrow
),#teste

#Correlaçao####
tabPanel("Correlação",
         fluidRow(
           h2(HTML("<b>Painel SIVEP-Gripe</b>")),
           h4(div(HTML("Análise dos dados de Síndrome Respiratória Aguda Grave <br> do SIVEP-Gripe - incluindo dados da COVID-19 2021-2022"), style = "color:gray")),
           
           h3("Selecione as variaveis desejadas para calcular o índice de Moran"),
           
           fluidRow(
           column(width = 6,
           selectInput("corr1",h3(" "),
                       
                       choices = list("Mortalidade" = "MORTALIDADE", 
                                      "Óbito" = "OBITO", 
                                      "Hospitalização" = "HOSPITALIZACAO",
                                      "Internados" = "INTERNADOS"
                       ))),
           column(width = 6,
           selectInput("corr2",h3(" "),
                       
                       choices = list("0 doses" = "D_0", 
                                      "1 dose" = "D_1", 
                                      "2 doses" = "D_2", 
                                      "3 doses" = "D_3"
                       )))),
           
           
           column(width = 12,

                  
                 #"(input.tempo == 'MORTALIDADE' && input.doses == 'D_0')" (input.tempo == 'HOSPITALIZACAO' && input.doses == 'D_0')
                 conditionalPanel(condition = "(input.corr1 == 'MORTALIDADE' && input.corr2 == 'D_0')",
                                  column(width = 12,tableOutput('T_MORTALIDADE_0'),  align = 'center')),
                 conditionalPanel(condition = "(input.corr1 == 'MORTALIDADE' && input.corr2 == 'D_1')",
                                  column(width = 12,tableOutput('T_MORTALIDADE_1'),  align = 'center')),
                 conditionalPanel(condition = "(input.corr1 == 'MORTALIDADE' && input.corr2 == 'D_2')",
                                  column(width = 12,tableOutput('T_MORTALIDADE_2'),  align = 'center')),
                 conditionalPanel(condition = "(input.corr1 == 'MORTALIDADE' && input.corr2 == 'D_3')",
                                  column(width = 12,tableOutput('T_MORTALIDADE_3'),  align = 'center')),
                 conditionalPanel(condition = "(input.corr1 == 'HOSPITALIZACAO' && input.corr2 == 'D_0')",
                                  column(width = 12,tableOutput('T_HOSPITALIZACAO_0'),  align = 'center')),
                 conditionalPanel(condition = "(input.corr1 == 'HOSPITALIZACAO' && input.corr2 == 'D_1')",
                                  column(width = 12,tableOutput('T_HOSPITALIZACAO_1'),  align = 'center')),
                 conditionalPanel(condition = "(input.corr1 == 'HOSPITALIZACAO' && input.corr2 == 'D_2')",
                                  column(width = 12,tableOutput('T_HOSPITALIZACAO_2'),  align = 'center')),
                 conditionalPanel(condition = "(input.corr1 == 'HOSPITALIZACAO' && input.corr2 == 'D_3')",
                                  column(width = 12,tableOutput('T_HOSPITALIZACAO_3'),  align = 'center')),
                 
                 conditionalPanel(condition = "(input.corr1 == 'OBITO' && input.corr2 == 'D_0')",
                                  column(width = 12,tableOutput('T_OBITO_0'),  align = 'center')),
                 conditionalPanel(condition = "(input.corr1 == 'OBITO' && input.corr2 == 'D_1')",
                                  column(width = 12,tableOutput('T_OBITO_1'),  align = 'center')),
                 conditionalPanel(condition = "(input.corr1 == 'OBITO' && input.corr2 == 'D_2')",
                                  column(width = 12,tableOutput('T_OBITO_2'),  align = 'center')),
                 conditionalPanel(condition = "(input.corr1 == 'OBITO' && input.corr2 == 'D_3')",
                                  column(width = 12,tableOutput('T_OBITO_3'),  align = 'center')),
                 conditionalPanel(condition = "(input.corr1 == 'INTERNADOS' && input.corr2 == 'D_0')",
                                  column(width = 12,tableOutput('T_HOSP_0'),  align = 'center')),
                 conditionalPanel(condition = "(input.corr1 == 'INTERNADOS' && input.corr2 == 'D_1')",
                                  column(width = 12,tableOutput('T_HOSP_1'),  align = 'center')),
                 conditionalPanel(condition = "(input.corr1 == 'INTERNADOS' && input.corr2 == 'D_2')",
                                  column(width = 12,tableOutput('T_HOSP_2'),  align = 'center')),
                 conditionalPanel(condition = "(input.corr1 == 'INTERNADOS' && input.corr2 == 'D_3')",
                                  column(width = 12,tableOutput('T_HOSP_3'),  align = 'center'))
                  
                  
                  
                  ),
           
          # fluidRow(column(width = 12, selectInput("map", h3("Síntese de :"),
          #                                         choices = list("LETALIDADE %" = 'LETALIDADE', "INCIDENCIA/100mil hab." = 'INCIDENCIA',
          #                                                        "MORTALIDADE/100mil hab." = 'MORTALIDADE'#,
          #                                                        #"PRIMEIRA DOSE %" = "%DOSE 1", "SEGUNDA DOSE %" ="%DOSE 2" ,"REFORÇO DOSE %" ="%DOSE R"
          #                                         ), selected = 2))),


          
          column(width = 6,highchartOutput('mapp2', height=600)),
           conditionalPanel(condition = "(input.corr1 == 'MORTALIDADE' && input.corr2 == 'D_0')",
                            column(width = 6,plotlyOutput('G_MORTALIDADE_0',height=600))),
           conditionalPanel(condition = "(input.corr1 == 'MORTALIDADE' && input.corr2 == 'D_1')",
                            column(width = 6,plotlyOutput('G_MORTALIDADE_1',height=600))),
           conditionalPanel(condition = "(input.corr1 == 'MORTALIDADE' && input.corr2 == 'D_2')",
                            column(width = 6,plotlyOutput('G_MORTALIDADE_2',height=600))),
           conditionalPanel(condition = "(input.corr1 == 'MORTALIDADE' && input.corr2 == 'D_3')",
                            column(width = 6,plotlyOutput('G_MORTALIDADE_3',height=600))),
           conditionalPanel(condition = "(input.corr1 == 'HOSPITALIZACAO' && input.corr2 == 'D_0')",
                            column(width = 6,plotlyOutput('G_HOSPITALIZACAO_0',height=600))),
           conditionalPanel(condition = "(input.corr1 == 'HOSPITALIZACAO' && input.corr2 == 'D_1')",
                            column(width = 6,plotlyOutput('G_HOSPITALIZACAO_1',height=600))),
           conditionalPanel(condition = "(input.corr1 == 'HOSPITALIZACAO' && input.corr2 == 'D_2')",
                            column(width = 6,plotlyOutput('G_HOSPITALIZACAO_2',height=600))),
           conditionalPanel(condition = "(input.corr1 == 'HOSPITALIZACAO' && input.corr2 == 'D_3')",
                            column(width = 6,plotlyOutput('G_HOSPITALIZACAO_3',height=600))),
          
          conditionalPanel(condition = "(input.corr1 == 'OBITO' && input.corr2 == 'D_0')",
                           column(width = 6,plotlyOutput('G_OBITO_0',height=600))),
          conditionalPanel(condition = "(input.corr1 == 'OBITO' && input.corr2 == 'D_1')",
                           column(width = 6,plotlyOutput('G_OBITO_1',height=600))),
          conditionalPanel(condition = "(input.corr1 == 'OBITO' && input.corr2 == 'D_2')",
                           column(width = 6,plotlyOutput('G_OBITO_2',height=600))),
          conditionalPanel(condition = "(input.corr1 == 'OBITO' && input.corr2 == 'D_3')",
                           column(width = 6,plotlyOutput('G_OBITO_3',height=600))),
          conditionalPanel(condition = "(input.corr1 == 'INTERNADOS' && input.corr2 == 'D_0')",
                           column(width = 6,plotlyOutput('G_HOSP_0',height=600))),
          conditionalPanel(condition = "(input.corr1 == 'INTERNADOS' && input.corr2 == 'D_1')",
                           column(width = 6,plotlyOutput('G_HOSP_1',height=600))),
          conditionalPanel(condition = "(input.corr1 == 'INTERNADOS' && input.corr2 == 'D_2')",
                           column(width = 6,plotlyOutput('G_HOSP_2',height=600))),
          conditionalPanel(condition = "(input.corr1 == 'INTERNADOS' && input.corr2 == 'D_3')",
                           column(width = 6,plotlyOutput('G_HOSP_3',height=600))),
           
          fluidRow(column(width = 12 ,h6(div("Dados mapa de 2021", style = "color:gray"))))
           
           
           #conditionalPanel(condition = "(input.corr == 'MORTALIDADE_0')",
           #                 column(width = 12,tableOutput('T_MORTALIDADE_0'),  align = 'center'))
           #plotlyOutput(
           
         )
         
         ),#Correlaçao


#####sobre #######
tabPanel("Sobre",

         
         
         fluidRow(
           tags$style(HTML("
                    img {
                      border-radius: 50%;
                    }")),
           column(width = 12,uiOutput("img"),h2("Alessandro Pereira"),p("Graduado em Estatística da Universidade Federal do Rio Grande do Norte. Possui experiência em ciência de dados, principalmente na utilização da linguagem R. Usuário avançado do framework shiny, utilizado para a construção de dashboards.") ,p("GitHub:",a("https://github.com/AlessandroPTSN/Painel-SIVEP-Gripe",   href="https://github.com/AlessandroPTSN/Painel-SIVEP-Gripe"))),
           
         ),
         h2("Informações sobre os cálculos"),
         fluidRow(column(width = 12,p("Cálculo da letalidade : (Óbitos/casos) * 100"))),
         fluidRow(column(width = 12,p("Cálculo da incidência : (Casos/População) * 100.000"))),
         fluidRow(column(width = 12,p("Cálculo da mortalidade : (Óbitos/População) * 100.000"))),
         
         h2("Indice de Moran"),
         mainPanel(
           br(),
           code("y = Mortalidade_X ou HOSPITALIZACAO_Y #Variavel"),br(),
           code("wm = M #Matrix de distâncias das UF"),br(),
           code("n = length(y) #27 UF's"),br(),
           code("ybar = mean(y)"),br(),
           code("dy = y - ybar"),br(),
           code("yi = rep(dy, each=n)"),br(),
           code("yj = rep(dy)"),br(),
           code("yiyj = yi * yj"),br(),
           code("pm = matrix(yiyj, ncol=n)"),br(),
           code("pmw = pm * wm"),br(),
           code("spmw = sum(pmw)"),br(),
           code("smw = sum(wm)"),br(),
           code("sw  = spmw / smw"),br(),
           code("vr = n / sum(dy^2)"),br(),
           code("MI = vr * sw # INDICE DE MORAN"),br(),
           code("ms = cbind(id=rep(1:n, each=n), y=rep(y, each=n), value=as.vector(wm * y))"),br(),
           code("ms = ms[ms[,3] > 0, ]"),br(),
           code("ams = aggregate(ms[,2:3], list(ms[,1]), FUN=mean)"),br(),
           code("ams = ams[,-1] #Variavel + Defasagem espacial"),br()
         )

         
         
         
)#Sobre


#####
)

