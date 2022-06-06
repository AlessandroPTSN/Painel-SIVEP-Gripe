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



options(highcharter.download_map_data = TRUE)
mapdata <- get_data_from_map(download_map_data("countries/br/br-all"))



map <- read_delim("/app/map.csv", 
                  delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% as.data.frame()

map_doses<- read_delim("/app/map_doses.csv", 
                    delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% as.data.frame()

map_doses = merge(map, map_doses, by.x = "SIGLA", by.y = "NOMES") %>% select(SIGLA,UF, POPULACAO, `DOSE 1`, `DOSE 2`, `DOSE R`)

map_doses$`%DOSE 1` = round((map_doses$`DOSE 1`/map_doses$POPULACAO) * 100,3)
map_doses$`%DOSE 2` = round((map_doses$`DOSE 2`/map_doses$POPULACAO) * 100,3)
map_doses$`%DOSE R` = round((map_doses$`DOSE R`/map_doses$POPULACAO) * 100,3)

map$`%DOSE 1` = map_doses$`%DOSE 1`
map$`%DOSE 2` = map_doses$`%DOSE 2`
map$`%DOSE R` = map_doses$`%DOSE R`






top_3_1 <- read_delim("/app/top_3_1.csv", 
                     delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% as.data.frame()

top_3_2 <- read_delim("/app/top_3_2.csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% as.data.frame()

top_3_R <- read_delim("/app/top_3_R.csv", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% as.data.frame()






totais <- read_delim("/app/totais.csv", 
                     delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% as.data.frame()

TS_BR_UFS <- read_delim("/app/TS_BR_UFS.csv", 
                    delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% as.data.frame()
TS_BR_UFS$DATA = as.Date(TS_BR_UFS$DATA, format = "%d/%m/%Y")
#TS_BR_UFS = TS_BR_UFS %>% arrange(TS_BR_UFS$DATA)


TS_BR_UFS = merge(TS_BR_UFS,totais, by.x = "NOME", by.y = "UF") %>% select(NOME ,DATA, HOSPITALIZAÇÕES.x, ÓBITOS.x ,CURADOS.x ,UTI ,INTUBAÇÃO ,REGIÃO ,NOME.y)%>%
  `colnames<-`(c("NOME" ,"DATA", "HOSPITALIZAÇÕES", "ÓBITOS" ,"CURADOS" ,"UTI" ,"INTUBAÇÃO" ,"REGIÃO" ,"NOMES"))


head(TS_BR_UFS)

cruza_all <- read_delim("/app/cruza_all.csv", 
                        delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% as.data.frame()
cruza_all$DATA = as.Date(cruza_all$DATA, format = "%d/%m/%Y")

cruza=cruza_all %>% select(DATA,UF,OBT_ZR, OBT_UMA, OBT_DAS, OBT_TRS) 



estados = read_delim("/app/estados.csv", 
           delim = ",", escape_double = FALSE, trim_ws = TRUE) %>% as.data.frame()


cruza_all = merge(cruza_all,estados, by.x = "UF", by.y = "uf") %>% select(UF,DATA, UTI_INT_OBT_ZR ,UTI_INT_OBT_UMA, UTI_INT_OBT_DAS, UTI_INT_OBT_TRS ,UTI_OBT_ZR ,UTI_OBT_UMA ,UTI_OBT_DAS, UTI_OBT_TRS, UTI_INT_ZR ,UTI_INT_UMA, UTI_INT_DAS, UTI_INT_TRS,
INT_OBT_ZR ,INT_OBT_UMA, INT_OBT_DAS, INT_OBT_TRS ,INT_ZR, INT_UMA, INT_DAS ,INT_TRS, OBT_ZR ,OBT_UMA ,OBT_DAS, OBT_TRS, ZR ,UMA ,DAS, TRS, UTI_ZR, UTI_UMA, UTI_DAS ,UTI_TRS,  regiao)

#cruza_all %>% filter(UF == "Todas")
#cruza2=cruza_all %>% select(DATA,UF,UTI_ZR, UTI_UMA, UTI_DAS, UTI_TRS) 

#unique(cruza_all$regiao)


shinyServer(function(input, output, session) {
#Seletor de Local e tabelas####################################################################################################################################

  
  output$selected_var <- renderText({ 
    paste("Dados selecionados:", tolower((totais %>% filter(UF == as.character(input$totais)))[[1]]))
  })
  output$table <- renderTable(TS_BR_UFS%>% filter(NOME == as.character(input$totais)) %>% select(`HOSPITALIZAÇÕES`, CURADOS,`ÓBITOS`)%>% summarise_all(sum))
  
  
  #TS_BR_UFS%>% filter(NOME == as.character('Todas')) %>% select(`HOSPITALIZAÇÕES`, CURADOS,`ÓBITOS`)%>% summarise_all(sum)
  

 # output$ObitosBox <- renderValueBox({
#    valueBox(
#      paste0(as.numeric(totais %>% filter(UF == as.character(input$totais))%>% select(`ÓBITOS`)) ), "Óbitos", icon = icon("glyphicon glyphicon-user", lib = "glyphicon"),
#      color = "red"
#    )
#  })
  
  
  
  output$text_hos <- renderText({ as.numeric(TS_BR_UFS %>% filter(NOME == as.character(input$totais))%>%select(`HOSPITALIZAÇÕES`)%>% summarise_all(sum)) })
  output$text_obt <- renderText({ as.numeric(TS_BR_UFS %>% filter(NOME == as.character(input$totais))%>%select(`ÓBITOS`)%>% summarise_all(sum)) })
  
  
  output$selected_hos<- renderText({ 
    paste(tolower(TS_BR_UFS %>% filter(NOME == as.character(input$totais))%>%select(NOMES)%>%unique()))
  })
  
  output$selected_obt<- renderText({ 
    paste(tolower(TS_BR_UFS %>% filter(NOME == as.character(input$totais))%>%select(NOMES)%>%unique()))
  })
  
  

  
  
  
  output$selected_var1 <- renderText({ 
    paste("Top 3 Primeira Dose:", tolower((totais %>% filter(UF == as.character(input$totais)))[[1]]))
  })
  output$table1 <- renderTable(top_3_1 %>% filter(NOME == as.character(input$totais))%>% select("TOTAL",	"PRIMEIRA DOSE"))
  
  output$selected_var2 <- renderText({ 
    paste("Top 3 Segunda Dose:", tolower((totais %>% filter(UF == as.character(input$totais)))[[1]]))
  })
  output$table2 <- renderTable(top_3_2 %>% filter(NOME == as.character(input$totais))%>% select("TOTAL",	"SEGUNDA DOSE"))
  
  output$selected_varR <- renderText({ 
    paste("Top 3 Reforço Dose:", tolower((totais %>% filter(UF == as.character(input$totais)))[[1]]))
  })
  output$tableR <- renderTable(top_3_R %>% filter(NOME == as.character(input$totais))%>% select("TOTAL",	"REFORÇO DOSE"))
  

  
  
  
#Grafico obitos por dose (dia/semana)######################################################################################## 

  
  
  output$obts_dose_sem<- renderHighchart({ 
    cruza %>% filter(UF == input$totais)%>%
      group_by(year = year(DATA), SEMANA = week(DATA)) %>%select(OBT_ZR,OBT_UMA,OBT_DAS,OBT_TRS)%>% summarise_all(sum) %>% 
      `colnames<-`(c("year" ,"SEMANA", "Zero", "Uma" ,"Duas" ,"Três"))%>%
      pivot_longer(., cols=c(Zero,Uma ,Duas ,Três), names_to = "Doses", values_to = "Obitos")%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = 'Obitos', group = 'Doses')) %>%
      hc_add_theme(hc_theme_538()) %>% hc_title(text = paste("Óbitos por doses tomadas ",paste(input$totais)," 2021-2022"))
  }) 
  
  
  
  output$obts_dose<- renderHighchart({ 
    cruza %>% filter(UF == input$totais)%>%
      group_by(year = DATA) %>%select(OBT_ZR,OBT_UMA,OBT_DAS,OBT_TRS)%>% summarise_all(sum) %>% 
      `colnames<-`(c("year" , "Zero", "Uma" ,"Duas" ,"Três"))%>%
      pivot_longer(., cols=c(Zero,Uma ,Duas ,Três), names_to = "Doses", values_to = "Obitos")%>%
      hchart('column', hcaes(x = year, y = 'Obitos', group = 'Doses')) %>%
      hc_add_theme(hc_theme_538()) %>% hc_title(text = paste("Óbitos por doses tomadas ",paste(input$totais)," 2021-2022"))
  }) 
  
#mapa#####################################################################

  
  output$mapp <- renderHighchart({
    hcmap("countries/br/br-all", data = map %>% select(SIGLA,input$map), value = paste(input$map),
          joinBy = c("hc-a2", "SIGLA"), name= paste(as.character(input$map)),
          dataLabels = list(enabled = F),
          tooltip = list(valueDecimals = 2, valuePrefix = "")) %>%
      
     # "PRIMEIRA DOSE %" = "%DOSE 1", "SEGUNDA DOSE %" ="%DOSE 2" ,"REFORÇO DOSE %" ="%DOSE R"
      hc_title(text = paste(tolower(as.character(input$map)),"no Brasil")) %>%
      hc_colorAxis(dataClasses = color_classes(c((map %>% select(paste(input$map)) %>% sapply(., summary) %>% c())[-3]),
         if(as.character(input$map) == "INCIDENCIA"){colors = c("#BDD7E7","#6BAED6","#3182BD","#08519C") }else{
           if(as.character(input$map) == "MORTALIDADE"){colors = c("#c78181","#a63636","#810303","#640202")}else{
             if(as.character(input$map) == "LETALIDADE"){colors = c("#c7c2df","#9a91c6","#8077ac","#645d86")}else{
               #if(as.character(input$map) == "%DOSE 1"){colors = c("#c7c2df","#9a91c6","#8077ac","#645d86")}else{
                 #if(as.character(input$map) == "%DOSE 2"){colors = c("#c7c2df","#9a91c6","#8077ac","#645d86")}else{
                   #if(as.character(input$map) == "%DOSE R"){colors = c("#c7c2df","#9a91c6","#8077ac","#645d86")}
                 #}
               #}
              }
             }
         } 
      )
      ) %>% 
      hc_legend(layout = "vertical", align = "right", valueDecimals = 2) %>% 
      hc_add_theme(hc_theme_538()) %>%
      hc_mapNavigation(enabled = TRUE)
  }) 




#Graficos hospitalizacao e obitos (dias e semanas)############################################################################################################   
  
  #if(input$checkbox){
    
    output$hosp_sem<- renderHighchart({ 
      #TS_BR_UFS %>% filter(NOME == input$totais)%>%
       # group_by(year = year(DATA), SEMANA = week(DATA)) %>% 
      
      TS_BR_UFS %>% filter(NOME == input$totais)%>%
        group_by(year = year(DATA), SEMANA = week(DATA)) %>%select(HOSPITALIZAÇÕES)%>% summarise_all(sum)%>%
        
        #summarise_if(is.numeric, sum)%>%
        hchart('column', hcaes(x = paste(SEMANA,"_",year), y = HOSPITALIZAÇÕES)) %>% 
        hc_xAxis(title = list(text = " ")) %>% 
        hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Hospitalizações")%>%hc_colors(c("#3182BD"))%>% 
        hc_add_theme(hc_theme_538())
    }) 
    
    
    
    output$obt_sem<- renderHighchart({ 
      #TS_BR_UFS %>% filter(NOME == input$totais)%>%
        #group_by(year = year(DATA), SEMANA = week(DATA)) %>% 
      TS_BR_UFS %>% filter(NOME == input$totais)%>%
        group_by(year = year(DATA), SEMANA = week(DATA)) %>%select(ÓBITOS)%>% summarise_all(sum)%>%
        
        hchart('column', hcaes(x = paste(SEMANA,"_",year), y = ÓBITOS)) %>% 
        hc_xAxis(title = list(text = " ")) %>% 
        hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Óbitos")%>%hc_colors(c("#a63636"))%>% 
        hc_add_theme(hc_theme_538())
    }) 
    
    
    
  #}else{ 
  output$hosp<- renderHighchart({ TS_BR_UFS %>% filter(NOME == input$totais)%>%
    hchart('column', hcaes(x = DATA, y = HOSPITALIZAÇÕES)) %>% 
    hc_xAxis(title = list(text = " ")) %>% 
    hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Hospitalizações")%>%hc_colors(c("#3182BD"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  

  
  
  output$obt<- renderHighchart({ TS_BR_UFS%>% filter(NOME == input$totais) %>%
      hchart('column', hcaes(x = DATA, y = ÓBITOS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Óbitos")%>%hc_colors(c("#a63636"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  #}
  
  
  
#  output$uti_dose_sem<- renderHighchart({ 
#    cruza2 %>% filter(UF == input$totais)%>%
#      group_by(year = year(DATA), SEMANA = week(DATA)) %>%select(UTI_ZR,UTI_UMA,UTI_DAS,UTI_TRS)%>% summarise_all(sum) %>% 
#      `colnames<-`(c("year" ,"SEMANA", "Zero", "Uma" ,"Duas" ,"Três"))%>%
#      pivot_longer(., cols=c(Zero,Uma ,Duas ,Três), names_to = "Doses", values_to = "UTI")%>%
#      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = 'UTI', group = 'Doses')) %>%
#      hc_add_theme(hc_theme_538()) %>% hc_title(text = paste("UTI por doses tomadas ",paste(input$totais)," 2021-2022"))%>%hc_size(height = 400)
#  }) 
  
  
#  output$uti_dose<- renderHighchart({ 
#    cruza2 %>% filter(UF == input$totais)%>%
#      group_by(year = DATA) %>%select(UTI_ZR,UTI_UMA,UTI_DAS,UTI_TRS)%>% summarise_all(sum) %>% 
#      `colnames<-`(c("year" , "Zero", "Uma" ,"Duas" ,"Três"))%>%
#      pivot_longer(., cols=c(Zero,Uma ,Duas ,Três), names_to = "Doses", values_to = "UTI")%>%
#      hchart('column', hcaes(x = year, y = 'UTI', group = 'Doses')) %>%
#      hc_add_theme(hc_theme_538()) %>% hc_title(text = paste("UTI por doses tomadas ",paste(input$totais)," 2021-2022"))%>%hc_size(height = 400)
#  }) 
  

  
 # output$obt_dose<- renderHighchart({ 
#  highchart() %>% 
#    hc_xAxis( categories =  as.Date(as.numeric(unlist(cruza %>% select(DATA,UF)%>% filter(UF == input$totais) %>% select(DATA))))  )   %>% 
#    hc_add_series(  name = "Zero", data = as.numeric(unlist(cruza %>% select(DATA,UF,OBT_ZR)%>% filter(UF == input$totais) %>% select(OBT_ZR))))%>%
#    hc_add_series(  name = "Uma", data = as.numeric(unlist(cruza %>% select(DATA,UF,OBT_UMA)%>% filter(UF == input$totais) %>% select(OBT_UMA))))%>%
#    hc_add_series(  name = "Duas", data = as.numeric(unlist(cruza %>% select(DATA,UF,OBT_DAS)%>% filter(UF == input$totais) %>% select(OBT_DAS))))%>%
#    hc_add_series(  name = "Três", data = as.numeric(unlist(cruza %>% select(DATA,UF,OBT_TRS)%>% filter(UF == input$totais) %>% select(OBT_TRS))))%>% 
#    hc_yAxis(title = list(text = "Contagem")) %>%
#    hc_add_theme(hc_theme_538()) %>% hc_title(text = paste("Óbitos por doses tomadas ",paste(input$totais)," 2021-2022"))%>%hc_size(height = 400)
#  }) 

  
  
  
  
  
  
  

  #
  
  #COM DATA DIA#####
  
  output$G_D_ZR<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,ZR)%>%
      group_by(year = year(DATA), SEMANA = DATA) %>%select(ZR)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = ZR)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que tomaram zero doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$G_D_UMA<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,UMA)%>%
      group_by(year = year(DATA), SEMANA = DATA) %>%select(UMA)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UMA)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que tomaram uma dose")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$G_D_DAS<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,DAS)%>%
      group_by(year = year(DATA), SEMANA = DATA) %>%select(DAS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = DAS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que tomaram duas doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  output$G_D_TRS<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,TRS)%>%
      group_by(year = year(DATA), SEMANA = DATA) %>%select(TRS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = TRS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que tomaram três doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  
  
  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###
  
  output$G_D_OBT_ZR<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,OBT_ZR)%>%
      group_by(year = year(DATA), SEMANA = DATA) %>%select(OBT_ZR)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = OBT_ZR)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram a óbito e tomaram zero doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$G_D_OBT_UMA<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,OBT_UMA)%>%
      group_by(year = year(DATA), SEMANA = DATA) %>%select(OBT_UMA)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = OBT_UMA)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas que foram a óbito e tomaram uma dose")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$G_D_OBT_DAS<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,OBT_DAS)%>%
      group_by(year = year(DATA), SEMANA = DATA) %>%select(OBT_DAS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = OBT_DAS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram a óbito e tomaram duas doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  output$G_D_OBT_TRS<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,OBT_TRS)%>%
      group_by(year = year(DATA), SEMANA = DATA) %>%select(OBT_TRS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = OBT_TRS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram a óbito e tomaram três doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  
  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###
  
  
  
  output$G_D_UTI_ZR<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,UTI_ZR)%>%
      group_by(year = year(DATA), SEMANA = DATA) %>%select(UTI_ZR)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_ZR)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram a UTI e tomaram zero doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$G_D_UTI_UMA<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,UTI_UMA)%>%
      group_by(year = year(DATA), SEMANA = DATA) %>%select(UTI_UMA)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_UMA)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram a UTI e tomaram uma dose")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$G_D_UTI_DAS<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,UTI_DAS)%>%
      group_by(year = year(DATA), SEMANA = DATA) %>%select(UTI_DAS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_DAS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram a UTI e tomaram duas doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  output$G_D_UTI_TRS<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,UTI_TRS)%>%
      group_by(year = year(DATA), SEMANA = DATA) %>%select(UTI_TRS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_TRS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram a UTI e tomaram três doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###
  
  
  
  output$G_D_INT_ZR<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,INT_ZR)%>%
      group_by(year = year(DATA), SEMANA = DATA) %>%select(INT_ZR)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = INT_ZR)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram intubadas e tomaram zero doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$G_D_INT_UMA<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,INT_UMA)%>%
      group_by(year = year(DATA), SEMANA = DATA) %>%select(INT_UMA)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = INT_UMA)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram intubadas e tomaram uma dose")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$G_D_INT_DAS<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,INT_DAS)%>%
      group_by(year = year(DATA), SEMANA = DATA) %>%select(INT_DAS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = INT_DAS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram intubadas e tomaram duas doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  output$G_D_INT_TRS<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,INT_TRS)%>%
      group_by(year = year(DATA), SEMANA = DATA) %>%select(INT_TRS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = INT_TRS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram intubadas e tomaram três doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  
  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###
  
  output$G_D_UTI_OBT_ZR<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,UTI_OBT_ZR)%>%
      group_by(year = year(DATA), SEMANA = DATA) %>%select(UTI_OBT_ZR)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_OBT_ZR)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram para UTI e foram a óbito que tomaram zero doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$G_D_UTI_OBT_UMA<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,UTI_OBT_UMA)%>%
      group_by(year = year(DATA), SEMANA = DATA) %>%select(UTI_OBT_UMA)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_OBT_UMA)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram para UTI e foram a óbito que tomaram uma dose")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$G_D_UTI_OBT_DAS<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,UTI_OBT_DAS)%>%
      group_by(year = year(DATA), SEMANA = DATA) %>%select(UTI_OBT_DAS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_OBT_DAS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram para UTI e foram a óbito que tomaram duas doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  output$G_D_UTI_OBT_TRS<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,UTI_OBT_TRS)%>%
      group_by(year = year(DATA), SEMANA = DATA) %>%select(UTI_OBT_TRS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_OBT_TRS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram para UTI e foram a óbito que tomaram três doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  
  
  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###
  
  output$G_D_INT_OBT_ZR<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,INT_OBT_ZR)%>%
      group_by(year = year(DATA), SEMANA = DATA) %>%select(INT_OBT_ZR)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = INT_OBT_ZR)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram intubadas e foram a óbito que tomaram zero doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$G_D_INT_OBT_UMA<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,INT_OBT_UMA)%>%
      group_by(year = year(DATA), SEMANA = DATA) %>%select(INT_OBT_UMA)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = INT_OBT_UMA)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas que foram a óbito e tomaram uma dose")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$G_D_INT_OBT_DAS<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,INT_OBT_DAS)%>%
      group_by(year = year(DATA), SEMANA = DATA) %>%select(INT_OBT_DAS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = INT_OBT_DAS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram a óbito e tomaram duas doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  output$G_D_INT_OBT_TRS<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,INT_OBT_TRS)%>%
      group_by(year = year(DATA), SEMANA = DATA) %>%select(INT_OBT_TRS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = INT_OBT_TRS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram a óbito e tomaram três doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  
  
  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###
  
  
  
  output$G_D_UTI_INT_ZR<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,UTI_INT_ZR)%>%
      group_by(year = year(DATA), SEMANA = DATA) %>%select(UTI_INT_ZR)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_INT_ZR)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram para UTI, foram intubadas e tomaram zero doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$G_D_UTI_INT_UMA<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,UTI_INT_UMA)%>%
      group_by(year = year(DATA), SEMANA = DATA) %>%select(UTI_INT_UMA)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_INT_UMA)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram para UTI, foram intubadas e tomaram uma dose")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$G_D_UTI_INT_DAS<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,UTI_INT_DAS)%>%
      group_by(year = year(DATA), SEMANA = DATA) %>%select(UTI_INT_DAS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_INT_DAS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram para UTI, foram intubadas e tomaram duas doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  output$G_D_UTI_INT_TRS<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,UTI_INT_TRS)%>%
      group_by(year = year(DATA), SEMANA = DATA) %>%select(UTI_INT_TRS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_INT_TRS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram para UTI, foram intubadas e tomaram três doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###
  
  
  
  output$G_D_UTI_INT_OBT_ZR<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,UTI_INT_OBT_ZR)%>%
      group_by(year = year(DATA), SEMANA = DATA) %>%select(UTI_INT_OBT_ZR)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_INT_OBT_ZR)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram para UTI, foram intubadas e tomaram zero doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$G_D_UTI_INT_OBT_UMA<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,UTI_INT_OBT_UMA)%>%
      group_by(year = year(DATA), SEMANA = DATA) %>%select(UTI_INT_OBT_UMA)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_INT_OBT_UMA)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram para UTI, foram intubadas e tomaram uma dose")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$G_D_UTI_INT_OBT_DAS<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,UTI_INT_OBT_DAS)%>%
      group_by(year = year(DATA), SEMANA = DATA) %>%select(UTI_INT_OBT_DAS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_INT_OBT_DAS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram para UTI, foram intubadas e tomaram duas doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  output$G_D_UTI_INT_OBT_TRS<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,UTI_INT_OBT_TRS)%>%
      group_by(year = year(DATA), SEMANA = DATA) %>%select(UTI_INT_OBT_TRS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_INT_OBT_TRS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram para UTI, foram intubadas e tomaram três doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 

  
  
  
  #COM DATA SEMANA####
  output$G_S_ZR<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,ZR)%>%
      group_by(year = year(DATA), SEMANA = week(DATA)) %>%select(ZR)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = ZR)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que tomaram zero doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$G_S_UMA<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,UMA)%>%
      group_by(year = year(DATA), SEMANA = week(DATA)) %>%select(UMA)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UMA)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que tomaram uma dose")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$G_S_DAS<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,DAS)%>%
      group_by(year = year(DATA), SEMANA = week(DATA)) %>%select(DAS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = DAS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que tomaram duas doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  output$G_S_TRS<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,TRS)%>%
      group_by(year = year(DATA), SEMANA = week(DATA)) %>%select(TRS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = TRS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que tomaram três doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  
  
  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###
  
  output$G_S_OBT_ZR<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,OBT_ZR)%>%
      group_by(year = year(DATA), SEMANA = week(DATA)) %>%select(OBT_ZR)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = OBT_ZR)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram a óbito e tomaram zero doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$G_S_OBT_UMA<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,OBT_UMA)%>%
      group_by(year = year(DATA), SEMANA = week(DATA)) %>%select(OBT_UMA)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = OBT_UMA)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas que foram a óbito e tomaram uma dose")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$G_S_OBT_DAS<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,OBT_DAS)%>%
      group_by(year = year(DATA), SEMANA = week(DATA)) %>%select(OBT_DAS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = OBT_DAS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram a óbito e tomaram duas doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  output$G_S_OBT_TRS<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,OBT_TRS)%>%
      group_by(year = year(DATA), SEMANA = week(DATA)) %>%select(OBT_TRS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = OBT_TRS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram a óbito e tomaram três doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  
  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###
  
  
  
  output$G_S_UTI_ZR<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,UTI_ZR)%>%
      group_by(year = year(DATA), SEMANA = week(DATA)) %>%select(UTI_ZR)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_ZR)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram a UTI e tomaram zero doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$G_S_UTI_UMA<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,UTI_UMA)%>%
      group_by(year = year(DATA), SEMANA = week(DATA)) %>%select(UTI_UMA)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_UMA)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram a UTI e tomaram uma dose")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$G_S_UTI_DAS<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,UTI_DAS)%>%
      group_by(year = year(DATA), SEMANA = week(DATA)) %>%select(UTI_DAS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_DAS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram a UTI e tomaram duas doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  output$G_S_UTI_TRS<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,UTI_TRS)%>%
      group_by(year = year(DATA), SEMANA = week(DATA)) %>%select(UTI_TRS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_TRS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram a UTI e tomaram três doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###
  
  
  
  output$G_S_INT_ZR<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,INT_ZR)%>%
      group_by(year = year(DATA), SEMANA = week(DATA)) %>%select(INT_ZR)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = INT_ZR)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram intubadas e tomaram zero doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$G_S_INT_UMA<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,INT_UMA)%>%
      group_by(year = year(DATA), SEMANA = week(DATA)) %>%select(INT_UMA)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = INT_UMA)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram intubadas e tomaram uma dose")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$G_S_INT_DAS<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,INT_DAS)%>%
      group_by(year = year(DATA), SEMANA = week(DATA)) %>%select(INT_DAS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = INT_DAS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram intubadas e tomaram duas doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  output$G_S_INT_TRS<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,INT_TRS)%>%
      group_by(year = year(DATA), SEMANA = week(DATA)) %>%select(INT_TRS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = INT_TRS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram intubadas e tomaram três doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  
  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###
  
  output$G_S_UTI_OBT_ZR<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,UTI_OBT_ZR)%>%
      group_by(year = year(DATA), SEMANA = week(DATA)) %>%select(UTI_OBT_ZR)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_OBT_ZR)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram para UTI e foram a óbito que tomaram zero doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$G_S_UTI_OBT_UMA<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,UTI_OBT_UMA)%>%
      group_by(year = year(DATA), SEMANA = week(DATA)) %>%select(UTI_OBT_UMA)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_OBT_UMA)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram para UTI e foram a óbito que tomaram uma dose")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$G_S_UTI_OBT_DAS<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,UTI_OBT_DAS)%>%
      group_by(year = year(DATA), SEMANA = week(DATA)) %>%select(UTI_OBT_DAS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_OBT_DAS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram para UTI e foram a óbito que tomaram duas doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  output$G_S_UTI_OBT_TRS<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,UTI_OBT_TRS)%>%
      group_by(year = year(DATA), SEMANA = week(DATA)) %>%select(UTI_OBT_TRS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_OBT_TRS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram para UTI e foram a óbito que tomaram três doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  
  
  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###
  
  output$G_S_INT_OBT_ZR<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,INT_OBT_ZR)%>%
      group_by(year = year(DATA), SEMANA = week(DATA)) %>%select(INT_OBT_ZR)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = INT_OBT_ZR)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram intubadas e foram a óbito que tomaram zero doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$G_S_INT_OBT_UMA<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,INT_OBT_UMA)%>%
      group_by(year = year(DATA), SEMANA = week(DATA)) %>%select(INT_OBT_UMA)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = INT_OBT_UMA)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas que foram a óbito e tomaram uma dose")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$G_S_INT_OBT_DAS<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,INT_OBT_DAS)%>%
      group_by(year = year(DATA), SEMANA = week(DATA)) %>%select(INT_OBT_DAS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = INT_OBT_DAS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram a óbito e tomaram duas doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  output$G_S_INT_OBT_TRS<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,INT_OBT_TRS)%>%
      group_by(year = year(DATA), SEMANA = week(DATA)) %>%select(INT_OBT_TRS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = INT_OBT_TRS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram a óbito e tomaram três doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  
  
  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###
  
  
  
  output$G_S_UTI_INT_ZR<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,UTI_INT_ZR)%>%
      group_by(year = year(DATA), SEMANA = week(DATA)) %>%select(UTI_INT_ZR)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_INT_ZR)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram para UTI, foram intubadas e tomaram zero doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$G_S_UTI_INT_UMA<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,UTI_INT_UMA)%>%
      group_by(year = year(DATA), SEMANA = week(DATA)) %>%select(UTI_INT_UMA)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_INT_UMA)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram para UTI, foram intubadas e tomaram uma dose")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$G_S_UTI_INT_DAS<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,UTI_INT_DAS)%>%
      group_by(year = year(DATA), SEMANA = week(DATA)) %>%select(UTI_INT_DAS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_INT_DAS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram para UTI, foram intubadas e tomaram duas doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  output$G_S_UTI_INT_TRS<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,UTI_INT_TRS)%>%
      group_by(year = year(DATA), SEMANA = week(DATA)) %>%select(UTI_INT_TRS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_INT_TRS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram para UTI, foram intubadas e tomaram três doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###
  
  
  
  output$G_S_UTI_INT_OBT_ZR<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,UTI_INT_OBT_ZR)%>%
      group_by(year = year(DATA), SEMANA = week(DATA)) %>%select(UTI_INT_OBT_ZR)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_INT_OBT_ZR)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram para UTI, foram intubadas e tomaram zero doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$G_S_UTI_INT_OBT_UMA<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,UTI_INT_OBT_UMA)%>%
      group_by(year = year(DATA), SEMANA = week(DATA)) %>%select(UTI_INT_OBT_UMA)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_INT_OBT_UMA)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram para UTI, foram intubadas e tomaram uma dose")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$G_S_UTI_INT_OBT_DAS<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,UTI_INT_OBT_DAS)%>%
      group_by(year = year(DATA), SEMANA = week(DATA)) %>%select(UTI_INT_OBT_DAS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_INT_OBT_DAS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram para UTI, foram intubadas e tomaram duas doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  output$G_S_UTI_INT_OBT_TRS<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,UTI_INT_OBT_TRS)%>%
      group_by(year = year(DATA), SEMANA = week(DATA)) %>%select(UTI_INT_OBT_TRS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_INT_OBT_TRS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram para UTI, foram intubadas e tomaram três doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  #COM DATA ANO####
  output$G_A_ZR<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,ZR)%>%
      group_by(year = year(DATA), SEMANA = year(DATA)) %>%select(ZR)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = ZR)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que tomaram zero doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$G_A_UMA<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,UMA)%>%
      group_by(year = year(DATA), SEMANA = year(DATA)) %>%select(UMA)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UMA)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que tomaram uma dose")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$G_A_DAS<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,DAS)%>%
      group_by(year = year(DATA), SEMANA = year(DATA)) %>%select(DAS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = DAS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que tomaram duas doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  output$G_A_TRS<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,TRS)%>%
      group_by(year = year(DATA), SEMANA = year(DATA)) %>%select(TRS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = TRS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que tomaram três doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  
  
  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###
  
  output$G_A_OBT_ZR<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,OBT_ZR)%>%
      group_by(year = year(DATA), SEMANA = year(DATA)) %>%select(OBT_ZR)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = OBT_ZR)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram a óbito e tomaram zero doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$G_A_OBT_UMA<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,OBT_UMA)%>%
      group_by(year = year(DATA), SEMANA = year(DATA)) %>%select(OBT_UMA)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = OBT_UMA)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas que foram a óbito e tomaram uma dose")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$G_A_OBT_DAS<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,OBT_DAS)%>%
      group_by(year = year(DATA), SEMANA = year(DATA)) %>%select(OBT_DAS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = OBT_DAS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram a óbito e tomaram duas doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  output$G_A_OBT_TRS<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,OBT_TRS)%>%
      group_by(year = year(DATA), SEMANA = year(DATA)) %>%select(OBT_TRS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = OBT_TRS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram a óbito e tomaram três doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  
  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###
  
  
  
  output$G_A_UTI_ZR<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,UTI_ZR)%>%
      group_by(year = year(DATA), SEMANA = year(DATA)) %>%select(UTI_ZR)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_ZR)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram a UTI e tomaram zero doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$G_A_UTI_UMA<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,UTI_UMA)%>%
      group_by(year = year(DATA), SEMANA = year(DATA)) %>%select(UTI_UMA)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_UMA)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram a UTI e tomaram uma dose")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$G_A_UTI_DAS<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,UTI_DAS)%>%
      group_by(year = year(DATA), SEMANA = year(DATA)) %>%select(UTI_DAS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_DAS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram a UTI e tomaram duas doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  output$G_A_UTI_TRS<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,UTI_TRS)%>%
      group_by(year = year(DATA), SEMANA = year(DATA)) %>%select(UTI_TRS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_TRS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram a UTI e tomaram três doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###
  
  
  
  output$G_A_INT_ZR<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,INT_ZR)%>%
      group_by(year = year(DATA), SEMANA = year(DATA)) %>%select(INT_ZR)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = INT_ZR)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram intubadas e tomaram zero doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$G_A_INT_UMA<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,INT_UMA)%>%
      group_by(year = year(DATA), SEMANA = year(DATA)) %>%select(INT_UMA)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = INT_UMA)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram intubadas e tomaram uma dose")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$G_A_INT_DAS<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,INT_DAS)%>%
      group_by(year = year(DATA), SEMANA = year(DATA)) %>%select(INT_DAS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = INT_DAS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram intubadas e tomaram duas doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  output$G_A_INT_TRS<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,INT_TRS)%>%
      group_by(year = year(DATA), SEMANA = year(DATA)) %>%select(INT_TRS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = INT_TRS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram intubadas e tomaram três doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  
  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###
  
  output$G_A_UTI_OBT_ZR<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,UTI_OBT_ZR)%>%
      group_by(year = year(DATA), SEMANA = year(DATA)) %>%select(UTI_OBT_ZR)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_OBT_ZR)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram para UTI e foram a óbito que tomaram zero doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$G_A_UTI_OBT_UMA<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,UTI_OBT_UMA)%>%
      group_by(year = year(DATA), SEMANA = year(DATA)) %>%select(UTI_OBT_UMA)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_OBT_UMA)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram para UTI e foram a óbito que tomaram uma dose")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$G_A_UTI_OBT_DAS<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,UTI_OBT_DAS)%>%
      group_by(year = year(DATA), SEMANA = year(DATA)) %>%select(UTI_OBT_DAS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_OBT_DAS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram para UTI e foram a óbito que tomaram duas doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  output$G_A_UTI_OBT_TRS<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,UTI_OBT_TRS)%>%
      group_by(year = year(DATA), SEMANA = year(DATA)) %>%select(UTI_OBT_TRS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_OBT_TRS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram para UTI e foram a óbito que tomaram três doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  
  
  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###
  
  output$G_A_INT_OBT_ZR<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,INT_OBT_ZR)%>%
      group_by(year = year(DATA), SEMANA = year(DATA)) %>%select(INT_OBT_ZR)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = INT_OBT_ZR)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram intubadas e foram a óbito que tomaram zero doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$G_A_INT_OBT_UMA<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,INT_OBT_UMA)%>%
      group_by(year = year(DATA), SEMANA = year(DATA)) %>%select(INT_OBT_UMA)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = INT_OBT_UMA)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas que foram a óbito e tomaram uma dose")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$G_A_INT_OBT_DAS<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,INT_OBT_DAS)%>%
      group_by(year = year(DATA), SEMANA = year(DATA)) %>%select(INT_OBT_DAS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = INT_OBT_DAS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram a óbito e tomaram duas doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  output$G_A_INT_OBT_TRS<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,INT_OBT_TRS)%>%
      group_by(year = year(DATA), SEMANA = year(DATA)) %>%select(INT_OBT_TRS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = INT_OBT_TRS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram a óbito e tomaram três doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  
  
  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###
  
  
  
  output$G_A_UTI_INT_ZR<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,UTI_INT_ZR)%>%
      group_by(year = year(DATA), SEMANA = year(DATA)) %>%select(UTI_INT_ZR)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_INT_ZR)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram para UTI, foram intubadas e tomaram zero doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$G_A_UTI_INT_UMA<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,UTI_INT_UMA)%>%
      group_by(year = year(DATA), SEMANA = year(DATA)) %>%select(UTI_INT_UMA)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_INT_UMA)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram para UTI, foram intubadas e tomaram uma dose")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$G_A_UTI_INT_DAS<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,UTI_INT_DAS)%>%
      group_by(year = year(DATA), SEMANA = year(DATA)) %>%select(UTI_INT_DAS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_INT_DAS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram para UTI, foram intubadas e tomaram duas doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  output$G_A_UTI_INT_TRS<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,UTI_INT_TRS)%>%
      group_by(year = year(DATA), SEMANA = year(DATA)) %>%select(UTI_INT_TRS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_INT_TRS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram para UTI, foram intubadas e tomaram três doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###
  
  
  
  output$G_A_UTI_INT_OBT_ZR<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,UTI_INT_OBT_ZR)%>%
      group_by(year = year(DATA), SEMANA = year(DATA)) %>%select(UTI_INT_OBT_ZR)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_INT_OBT_ZR)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram para UTI, foram intubadas e tomaram zero doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$G_A_UTI_INT_OBT_UMA<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,UTI_INT_OBT_UMA)%>%
      group_by(year = year(DATA), SEMANA = year(DATA)) %>%select(UTI_INT_OBT_UMA)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_INT_OBT_UMA)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram para UTI, foram intubadas e tomaram uma dose")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$G_A_UTI_INT_OBT_DAS<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,UTI_INT_OBT_DAS)%>%
      group_by(year = year(DATA), SEMANA = year(DATA)) %>%select(UTI_INT_OBT_DAS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_INT_OBT_DAS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram para UTI, foram intubadas e tomaram duas doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  output$G_A_UTI_INT_OBT_TRS<- renderHighchart({ 
    cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DATA,UTI_INT_OBT_TRS)%>%
      group_by(year = year(DATA), SEMANA = year(DATA)) %>%select(UTI_INT_OBT_TRS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_INT_OBT_TRS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram para UTI, foram intubadas e tomaram três doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  #sem data#################################
  #Apenas doses #####
  output$T_ZR <- renderTable(cruza_all %>%filter(UF == as.character(input$locais)) %>% select(ZR)%>% summarise_all(c(sum,mean,var))%>%`colnames<-`(c("Total","Média","Variância")))
  
  output$T_UMA<- renderTable(cruza_all %>%filter(UF == as.character(input$locais)) %>% select(UMA)%>% summarise_all(c(sum,mean,var))%>%`colnames<-`(c("Total","Média","Variância")))
  
  output$T_DAS <- renderTable(cruza_all %>%filter(UF == as.character(input$locais)) %>% select(DAS)%>% summarise_all(c(sum,mean,var))%>%`colnames<-`(c("Total","Média","Variância")))
  
  output$T_TRS<- renderTable(cruza_all %>%filter(UF == as.character(input$locais)) %>% select(TRS)%>% summarise_all(c(sum,mean,var))%>%`colnames<-`(c("Total","Média","Variância")))
  
  
  #Apenas doses e obitos#####
  output$T_OBT_ZR <- renderTable(cruza_all %>% filter(UF == as.character(input$locais))  %>% select(OBT_ZR)%>% summarise_all(c(sum,mean,var))%>%`colnames<-`(c("Total","Média","Variância")))
  
  output$T_OBT_UMA <- renderTable(cruza_all %>% filter(UF == as.character(input$locais))  %>% select(OBT_UMA)%>% summarise_all(c(sum,mean,var))%>%`colnames<-`(c("Total","Média","Variância")))
  
  output$T_OBT_DAS <- renderTable(cruza_all %>% filter(UF == as.character(input$locais))  %>% select(OBT_DAS)%>% summarise_all(c(sum,mean,var))%>%`colnames<-`(c("Total","Média","Variância")))
  
  output$T_OBT_TRS <- renderTable(cruza_all %>% filter(UF == as.character(input$locais))  %>% select(OBT_TRS)%>% summarise_all(c(sum,mean,var))%>%`colnames<-`(c("Total","Média","Variância")))
  
  #Apenas doses e UTI#####
  output$T_UTI_ZR <- renderTable(cruza_all %>% filter(UF == as.character(input$locais))  %>% select(UTI_ZR)%>% summarise_all(c(sum,mean,var))%>%`colnames<-`(c("Total","Média","Variância")))
  
  output$T_UTI_UMA <- renderTable(cruza_all %>% filter(UF == as.character(input$locais))  %>% select(UTI_UMA)%>% summarise_all(c(sum,mean,var))%>%`colnames<-`(c("Total","Média","Variância")))
  
  output$T_UTI_DAS<- renderTable(cruza_all %>% filter(UF == as.character(input$locais))  %>% select(UTI_DAS)%>% summarise_all(c(sum,mean,var))%>%`colnames<-`(c("Total","Média","Variância")))
  
  output$T_UTI_TRS <- renderTable(cruza_all %>% filter(UF == as.character(input$locais))  %>% select(UTI_TRS)%>% summarise_all(c(sum,mean,var))%>%`colnames<-`(c("Total","Média","Variância")))
  
  #Apenas doses e intubadas#####
  output$T_INT_ZR <- renderTable(cruza_all %>% filter(UF == as.character(input$locais))  %>% select(INT_ZR)%>% summarise_all(c(sum,mean,var))%>%`colnames<-`(c("Total","Média","Variância")))
  
  output$T_INT_UMA <- renderTable(cruza_all %>% filter(UF == as.character(input$locais))  %>% select(INT_UMA)%>% summarise_all(c(sum,mean,var))%>%`colnames<-`(c("Total","Média","Variância")))
  
  output$T_INT_DAS <- renderTable(cruza_all %>% filter(UF == as.character(input$locais))  %>% select(INT_DAS)%>% summarise_all(c(sum,mean,var))%>%`colnames<-`(c("Total","Média","Variância")))
  
  output$T_INT_TRS <- renderTable(cruza_all %>% filter(UF == as.character(input$locais))  %>% select(INT_TRS)%>% summarise_all(c(sum,mean,var))%>%`colnames<-`(c("Total","Média","Variância")))
  
  
  #Apenas doses e obitos UTI#####
  output$T_UTI_OBT_ZR <- renderTable(cruza_all %>% filter(UF == as.character(input$locais))  %>% select(UTI_OBT_ZR)%>% summarise_all(c(sum,mean,var))%>%`colnames<-`(c("Total","Média","Variância")))
  
  output$T_UTI_OBT_UMA <- renderTable(cruza_all %>% filter(UF == as.character(input$locais))  %>% select(UTI_OBT_UMA)%>% summarise_all(c(sum,mean,var))%>%`colnames<-`(c("Total","Média","Variância")))
  
  output$T_UTI_OBT_DAS <- renderTable(cruza_all %>% filter(UF == as.character(input$locais))  %>% select(UTI_OBT_DAS)%>% summarise_all(c(sum,mean,var))%>%`colnames<-`(c("Total","Média","Variância")))
  
  output$T_UTI_OBT_TRS <- renderTable(cruza_all %>% filter(UF == as.character(input$locais))  %>% select(UTI_OBT_TRS)%>% summarise_all(c(sum,mean,var))%>%`colnames<-`(c("Total","Média","Variância")))
  
  
  #Apenas doses e obitos intubadas#####
  output$T_INT_OBT_ZR <- renderTable(cruza_all %>% filter(UF == as.character(input$locais))  %>% select(INT_OBT_ZR)%>% summarise_all(c(sum,mean,var))%>%`colnames<-`(c("Total","Média","Variância")))
  
  output$T_INT_OBT_UMA <- renderTable(cruza_all %>% filter(UF == as.character(input$locais))  %>% select(INT_OBT_UMA)%>% summarise_all(c(sum,mean,var))%>%`colnames<-`(c("Total","Média","Variância")))
  
  output$T_INT_OBT_DAS <- renderTable(cruza_all %>% filter(UF == as.character(input$locais))  %>% select(INT_OBT_DAS)%>% summarise_all(c(sum,mean,var))%>%`colnames<-`(c("Total","Média","Variância")))
  
  output$T_INT_OBT_TRS <- renderTable(cruza_all %>% filter(UF == as.character(input$locais))  %>% select(INT_OBT_TRS)%>% summarise_all(c(sum,mean,var))%>%`colnames<-`(c("Total","Média","Variância")))
  
  
  #Apenas doses e  UTI intubadas#####
  output$T_UTI_INT_ZR <- renderTable(cruza_all %>% filter(UF == as.character(input$locais))  %>% select(UTI_INT_ZR)%>% summarise_all(c(sum,mean,var))%>%`colnames<-`(c("Total","Média","Variância")))
  
  output$T_UTI_INT_UMA <- renderTable(cruza_all %>% filter(UF == as.character(input$locais))  %>% select(UTI_INT_UMA)%>% summarise_all(c(sum,mean,var))%>%`colnames<-`(c("Total","Média","Variância")))
  
  output$T_UTI_INT_DAS <- renderTable(cruza_all %>% filter(UF == as.character(input$locais))  %>% select(UTI_INT_DAS)%>% summarise_all(c(sum,mean,var))%>%`colnames<-`(c("Total","Média","Variância")))
  
  output$T_UTI_INT_TRS <- renderTable(cruza_all %>% filter(UF == as.character(input$locais))  %>% select(UTI_INT_TRS)%>% summarise_all(c(sum,mean,var))%>%`colnames<-`(c("Total","Média","Variância")))
  
  #Apenas doses e  UTI e intubadas e obitos#####
  output$T_UTI_INT_OBT_ZR <- renderTable(cruza_all %>% filter(UF == as.character(input$locais))  %>% select(UTI_INT_OBT_ZR)%>% summarise_all(c(sum,mean,var))%>%`colnames<-`(c("Total","Média","Variância")))
  
  output$T_UTI_INT_OBT_UMA <- renderTable(cruza_all %>% filter(UF == as.character(input$locais))  %>% select(UTI_INT_OBT_UMA)%>% summarise_all(c(sum,mean,var))%>%`colnames<-`(c("Total","Média","Variância")))
  
  output$T_UTI_INT_OBT_DAS <- renderTable(cruza_all %>% filter(UF == as.character(input$locais))  %>% select(UTI_INT_OBT_DAS)%>% summarise_all(c(sum,mean,var))%>%`colnames<-`(c("Total","Média","Variância")))
  
  output$T_UTI_INT_OBT_TRS <- renderTable(cruza_all %>% filter(UF == as.character(input$locais))  %>% select(UTI_INT_OBT_TRS)%>% summarise_all(c(sum,mean,var))%>%`colnames<-`(c("Total","Média","Variância")))
  
  
  
##########
  
  #COM DATA DIA#####
  
  output$R_G_D_ZR<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,ZR)%>%
      group_by(year = year(DATA), SEMANA = DATA) %>%select(ZR)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = ZR)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que tomaram zero doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$R_G_D_UMA<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,UMA)%>%
      group_by(year = year(DATA), SEMANA = DATA) %>%select(UMA)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UMA)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que tomaram uma dose")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$R_G_D_DAS<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,DAS)%>%
      group_by(year = year(DATA), SEMANA = DATA) %>%select(DAS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = DAS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que tomaram duas doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  output$R_G_D_TRS<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,TRS)%>%
      group_by(year = year(DATA), SEMANA = DATA) %>%select(TRS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = TRS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que tomaram três doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  
  
  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###
  
  output$R_G_D_OBT_ZR<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,OBT_ZR)%>%
      group_by(year = year(DATA), SEMANA = DATA) %>%select(OBT_ZR)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = OBT_ZR)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram a óbito e tomaram zero doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$R_G_D_OBT_UMA<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,OBT_UMA)%>%
      group_by(year = year(DATA), SEMANA = DATA) %>%select(OBT_UMA)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = OBT_UMA)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas que foram a óbito e tomaram uma dose")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$R_G_D_OBT_DAS<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,OBT_DAS)%>%
      group_by(year = year(DATA), SEMANA = DATA) %>%select(OBT_DAS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = OBT_DAS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram a óbito e tomaram duas doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  output$R_G_D_OBT_TRS<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,OBT_TRS)%>%
      group_by(year = year(DATA), SEMANA = DATA) %>%select(OBT_TRS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = OBT_TRS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram a óbito e tomaram três doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  
  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###
  
  
  
  output$R_G_D_UTI_ZR<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,UTI_ZR)%>%
      group_by(year = year(DATA), SEMANA = DATA) %>%select(UTI_ZR)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_ZR)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram a UTI e tomaram zero doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$R_G_D_UTI_UMA<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,UTI_UMA)%>%
      group_by(year = year(DATA), SEMANA = DATA) %>%select(UTI_UMA)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_UMA)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram a UTI e tomaram uma dose")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$R_G_D_UTI_DAS<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,UTI_DAS)%>%
      group_by(year = year(DATA), SEMANA = DATA) %>%select(UTI_DAS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_DAS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram a UTI e tomaram duas doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  output$R_G_D_UTI_TRS<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,UTI_TRS)%>%
      group_by(year = year(DATA), SEMANA = DATA) %>%select(UTI_TRS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_TRS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram a UTI e tomaram três doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###
  
  
  
  output$R_G_D_INT_ZR<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,INT_ZR)%>%
      group_by(year = year(DATA), SEMANA = DATA) %>%select(INT_ZR)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = INT_ZR)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram intubadas e tomaram zero doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$R_G_D_INT_UMA<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,INT_UMA)%>%
      group_by(year = year(DATA), SEMANA = DATA) %>%select(INT_UMA)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = INT_UMA)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram intubadas e tomaram uma dose")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$R_G_D_INT_DAS<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,INT_DAS)%>%
      group_by(year = year(DATA), SEMANA = DATA) %>%select(INT_DAS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = INT_DAS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram intubadas e tomaram duas doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  output$R_G_D_INT_TRS<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,INT_TRS)%>%
      group_by(year = year(DATA), SEMANA = DATA) %>%select(INT_TRS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = INT_TRS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram intubadas e tomaram três doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  
  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###
  
  output$R_G_D_UTI_OBT_ZR<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,UTI_OBT_ZR)%>%
      group_by(year = year(DATA), SEMANA = DATA) %>%select(UTI_OBT_ZR)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_OBT_ZR)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram para UTI e foram a óbito que tomaram zero doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$R_G_D_UTI_OBT_UMA<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,UTI_OBT_UMA)%>%
      group_by(year = year(DATA), SEMANA = DATA) %>%select(UTI_OBT_UMA)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_OBT_UMA)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram para UTI e foram a óbito que tomaram uma dose")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$R_G_D_UTI_OBT_DAS<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,UTI_OBT_DAS)%>%
      group_by(year = year(DATA), SEMANA = DATA) %>%select(UTI_OBT_DAS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_OBT_DAS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram para UTI e foram a óbito que tomaram duas doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  output$R_G_D_UTI_OBT_TRS<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,UTI_OBT_TRS)%>%
      group_by(year = year(DATA), SEMANA = DATA) %>%select(UTI_OBT_TRS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_OBT_TRS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram para UTI e foram a óbito que tomaram três doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  
  
  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###
  
  output$R_G_D_INT_OBT_ZR<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,INT_OBT_ZR)%>%
      group_by(year = year(DATA), SEMANA = DATA) %>%select(INT_OBT_ZR)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = INT_OBT_ZR)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram intubadas e foram a óbito que tomaram zero doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$R_G_D_INT_OBT_UMA<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,INT_OBT_UMA)%>%
      group_by(year = year(DATA), SEMANA = DATA) %>%select(INT_OBT_UMA)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = INT_OBT_UMA)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas que foram a óbito e tomaram uma dose")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$R_G_D_INT_OBT_DAS<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,INT_OBT_DAS)%>%
      group_by(year = year(DATA), SEMANA = DATA) %>%select(INT_OBT_DAS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = INT_OBT_DAS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram a óbito e tomaram duas doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  output$R_G_D_INT_OBT_TRS<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,INT_OBT_TRS)%>%
      group_by(year = year(DATA), SEMANA = DATA) %>%select(INT_OBT_TRS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = INT_OBT_TRS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram a óbito e tomaram três doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  
  
  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###
  
  
  
  output$R_G_D_UTI_INT_ZR<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,UTI_INT_ZR)%>%
      group_by(year = year(DATA), SEMANA = DATA) %>%select(UTI_INT_ZR)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_INT_ZR)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram para UTI, foram intubadas e tomaram zero doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$R_G_D_UTI_INT_UMA<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,UTI_INT_UMA)%>%
      group_by(year = year(DATA), SEMANA = DATA) %>%select(UTI_INT_UMA)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_INT_UMA)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram para UTI, foram intubadas e tomaram uma dose")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$R_G_D_UTI_INT_DAS<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,UTI_INT_DAS)%>%
      group_by(year = year(DATA), SEMANA = DATA) %>%select(UTI_INT_DAS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_INT_DAS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram para UTI, foram intubadas e tomaram duas doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  output$R_G_D_UTI_INT_TRS<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,UTI_INT_TRS)%>%
      group_by(year = year(DATA), SEMANA = DATA) %>%select(UTI_INT_TRS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_INT_TRS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram para UTI, foram intubadas e tomaram três doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###
  
  
  
  output$R_G_D_UTI_INT_OBT_ZR<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,UTI_INT_OBT_ZR)%>%
      group_by(year = year(DATA), SEMANA = DATA) %>%select(UTI_INT_OBT_ZR)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_INT_OBT_ZR)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram para UTI, foram intubadas e tomaram zero doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$R_G_D_UTI_INT_OBT_UMA<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,UTI_INT_OBT_UMA)%>%
      group_by(year = year(DATA), SEMANA = DATA) %>%select(UTI_INT_OBT_UMA)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_INT_OBT_UMA)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram para UTI, foram intubadas e tomaram uma dose")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$R_G_D_UTI_INT_OBT_DAS<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,UTI_INT_OBT_DAS)%>%
      group_by(year = year(DATA), SEMANA = DATA) %>%select(UTI_INT_OBT_DAS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_INT_OBT_DAS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram para UTI, foram intubadas e tomaram duas doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  output$R_G_D_UTI_INT_OBT_TRS<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,UTI_INT_OBT_TRS)%>%
      group_by(year = year(DATA), SEMANA = DATA) %>%select(UTI_INT_OBT_TRS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_INT_OBT_TRS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram para UTI, foram intubadas e tomaram três doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  
  
  #COM DATA SEMANA####
  output$R_G_S_ZR<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,ZR)%>%
      group_by(year = year(DATA), SEMANA = week(DATA)) %>%select(ZR)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = ZR)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que tomaram zero doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$R_G_S_UMA<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,UMA)%>%
      group_by(year = year(DATA), SEMANA = week(DATA)) %>%select(UMA)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UMA)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que tomaram uma dose")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$R_G_S_DAS<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,DAS)%>%
      group_by(year = year(DATA), SEMANA = week(DATA)) %>%select(DAS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = DAS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que tomaram duas doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  output$R_G_S_TRS<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,TRS)%>%
      group_by(year = year(DATA), SEMANA = week(DATA)) %>%select(TRS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = TRS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que tomaram três doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  
  
  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###
  
  output$R_G_S_OBT_ZR<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,OBT_ZR)%>%
      group_by(year = year(DATA), SEMANA = week(DATA)) %>%select(OBT_ZR)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = OBT_ZR)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram a óbito e tomaram zero doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$R_G_S_OBT_UMA<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,OBT_UMA)%>%
      group_by(year = year(DATA), SEMANA = week(DATA)) %>%select(OBT_UMA)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = OBT_UMA)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas que foram a óbito e tomaram uma dose")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$R_G_S_OBT_DAS<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,OBT_DAS)%>%
      group_by(year = year(DATA), SEMANA = week(DATA)) %>%select(OBT_DAS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = OBT_DAS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram a óbito e tomaram duas doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  output$R_G_S_OBT_TRS<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,OBT_TRS)%>%
      group_by(year = year(DATA), SEMANA = week(DATA)) %>%select(OBT_TRS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = OBT_TRS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram a óbito e tomaram três doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  
  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###
  
  
  
  output$R_G_S_UTI_ZR<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,UTI_ZR)%>%
      group_by(year = year(DATA), SEMANA = week(DATA)) %>%select(UTI_ZR)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_ZR)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram a UTI e tomaram zero doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$R_G_S_UTI_UMA<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,UTI_UMA)%>%
      group_by(year = year(DATA), SEMANA = week(DATA)) %>%select(UTI_UMA)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_UMA)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram a UTI e tomaram uma dose")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$R_G_S_UTI_DAS<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,UTI_DAS)%>%
      group_by(year = year(DATA), SEMANA = week(DATA)) %>%select(UTI_DAS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_DAS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram a UTI e tomaram duas doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  output$R_G_S_UTI_TRS<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,UTI_TRS)%>%
      group_by(year = year(DATA), SEMANA = week(DATA)) %>%select(UTI_TRS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_TRS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram a UTI e tomaram três doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###
  
  
  
  output$R_G_S_INT_ZR<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,INT_ZR)%>%
      group_by(year = year(DATA), SEMANA = week(DATA)) %>%select(INT_ZR)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = INT_ZR)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram intubadas e tomaram zero doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$R_G_S_INT_UMA<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,INT_UMA)%>%
      group_by(year = year(DATA), SEMANA = week(DATA)) %>%select(INT_UMA)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = INT_UMA)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram intubadas e tomaram uma dose")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$R_G_S_INT_DAS<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,INT_DAS)%>%
      group_by(year = year(DATA), SEMANA = week(DATA)) %>%select(INT_DAS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = INT_DAS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram intubadas e tomaram duas doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  output$R_G_S_INT_TRS<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,INT_TRS)%>%
      group_by(year = year(DATA), SEMANA = week(DATA)) %>%select(INT_TRS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = INT_TRS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram intubadas e tomaram três doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  
  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###
  
  output$R_G_S_UTI_OBT_ZR<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,UTI_OBT_ZR)%>%
      group_by(year = year(DATA), SEMANA = week(DATA)) %>%select(UTI_OBT_ZR)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_OBT_ZR)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram para UTI e foram a óbito que tomaram zero doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$R_G_S_UTI_OBT_UMA<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,UTI_OBT_UMA)%>%
      group_by(year = year(DATA), SEMANA = week(DATA)) %>%select(UTI_OBT_UMA)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_OBT_UMA)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram para UTI e foram a óbito que tomaram uma dose")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$R_G_S_UTI_OBT_DAS<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,UTI_OBT_DAS)%>%
      group_by(year = year(DATA), SEMANA = week(DATA)) %>%select(UTI_OBT_DAS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_OBT_DAS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram para UTI e foram a óbito que tomaram duas doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  output$R_G_S_UTI_OBT_TRS<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,UTI_OBT_TRS)%>%
      group_by(year = year(DATA), SEMANA = week(DATA)) %>%select(UTI_OBT_TRS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_OBT_TRS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram para UTI e foram a óbito que tomaram três doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  
  
  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###
  
  output$R_G_S_INT_OBT_ZR<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,INT_OBT_ZR)%>%
      group_by(year = year(DATA), SEMANA = week(DATA)) %>%select(INT_OBT_ZR)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = INT_OBT_ZR)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram intubadas e foram a óbito que tomaram zero doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$R_G_S_INT_OBT_UMA<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,INT_OBT_UMA)%>%
      group_by(year = year(DATA), SEMANA = week(DATA)) %>%select(INT_OBT_UMA)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = INT_OBT_UMA)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas que foram a óbito e tomaram uma dose")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$R_G_S_INT_OBT_DAS<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,INT_OBT_DAS)%>%
      group_by(year = year(DATA), SEMANA = week(DATA)) %>%select(INT_OBT_DAS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = INT_OBT_DAS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram a óbito e tomaram duas doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  output$R_G_S_INT_OBT_TRS<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,INT_OBT_TRS)%>%
      group_by(year = year(DATA), SEMANA = week(DATA)) %>%select(INT_OBT_TRS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = INT_OBT_TRS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram a óbito e tomaram três doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  
  
  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###
  
  
  
  output$R_G_S_UTI_INT_ZR<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,UTI_INT_ZR)%>%
      group_by(year = year(DATA), SEMANA = week(DATA)) %>%select(UTI_INT_ZR)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_INT_ZR)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram para UTI, foram intubadas e tomaram zero doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$R_G_S_UTI_INT_UMA<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,UTI_INT_UMA)%>%
      group_by(year = year(DATA), SEMANA = week(DATA)) %>%select(UTI_INT_UMA)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_INT_UMA)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram para UTI, foram intubadas e tomaram uma dose")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$R_G_S_UTI_INT_DAS<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,UTI_INT_DAS)%>%
      group_by(year = year(DATA), SEMANA = week(DATA)) %>%select(UTI_INT_DAS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_INT_DAS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram para UTI, foram intubadas e tomaram duas doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  output$R_G_S_UTI_INT_TRS<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,UTI_INT_TRS)%>%
      group_by(year = year(DATA), SEMANA = week(DATA)) %>%select(UTI_INT_TRS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_INT_TRS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram para UTI, foram intubadas e tomaram três doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###
  
  
  
  output$R_G_S_UTI_INT_OBT_ZR<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,UTI_INT_OBT_ZR)%>%
      group_by(year = year(DATA), SEMANA = week(DATA)) %>%select(UTI_INT_OBT_ZR)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_INT_OBT_ZR)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram para UTI, foram intubadas e tomaram zero doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$R_G_S_UTI_INT_OBT_UMA<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,UTI_INT_OBT_UMA)%>%
      group_by(year = year(DATA), SEMANA = week(DATA)) %>%select(UTI_INT_OBT_UMA)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_INT_OBT_UMA)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram para UTI, foram intubadas e tomaram uma dose")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$R_G_S_UTI_INT_OBT_DAS<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,UTI_INT_OBT_DAS)%>%
      group_by(year = year(DATA), SEMANA = week(DATA)) %>%select(UTI_INT_OBT_DAS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_INT_OBT_DAS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram para UTI, foram intubadas e tomaram duas doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  output$R_G_S_UTI_INT_OBT_TRS<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,UTI_INT_OBT_TRS)%>%
      group_by(year = year(DATA), SEMANA = week(DATA)) %>%select(UTI_INT_OBT_TRS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_INT_OBT_TRS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram para UTI, foram intubadas e tomaram três doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  #COM DATA ANO####
  output$R_G_A_ZR<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,ZR)%>%
      group_by(year = year(DATA), SEMANA = year(DATA)) %>%select(ZR)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = ZR)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que tomaram zero doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$R_G_A_UMA<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,UMA)%>%
      group_by(year = year(DATA), SEMANA = year(DATA)) %>%select(UMA)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UMA)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que tomaram uma dose")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$R_G_A_DAS<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,DAS)%>%
      group_by(year = year(DATA), SEMANA = year(DATA)) %>%select(DAS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = DAS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que tomaram duas doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  output$R_G_A_TRS<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,TRS)%>%
      group_by(year = year(DATA), SEMANA = year(DATA)) %>%select(TRS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = TRS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que tomaram três doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  
  
  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###
  
  output$R_G_A_OBT_ZR<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,OBT_ZR)%>%
      group_by(year = year(DATA), SEMANA = year(DATA)) %>%select(OBT_ZR)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = OBT_ZR)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram a óbito e tomaram zero doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$R_G_A_OBT_UMA<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,OBT_UMA)%>%
      group_by(year = year(DATA), SEMANA = year(DATA)) %>%select(OBT_UMA)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = OBT_UMA)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas que foram a óbito e tomaram uma dose")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$R_G_A_OBT_DAS<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,OBT_DAS)%>%
      group_by(year = year(DATA), SEMANA = year(DATA)) %>%select(OBT_DAS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = OBT_DAS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram a óbito e tomaram duas doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  output$R_G_A_OBT_TRS<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,OBT_TRS)%>%
      group_by(year = year(DATA), SEMANA = year(DATA)) %>%select(OBT_TRS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = OBT_TRS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram a óbito e tomaram três doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  
  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###
  
  
  
  output$R_G_A_UTI_ZR<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,UTI_ZR)%>%
      group_by(year = year(DATA), SEMANA = year(DATA)) %>%select(UTI_ZR)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_ZR)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram a UTI e tomaram zero doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$R_G_A_UTI_UMA<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,UTI_UMA)%>%
      group_by(year = year(DATA), SEMANA = year(DATA)) %>%select(UTI_UMA)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_UMA)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram a UTI e tomaram uma dose")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$R_G_A_UTI_DAS<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,UTI_DAS)%>%
      group_by(year = year(DATA), SEMANA = year(DATA)) %>%select(UTI_DAS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_DAS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram a UTI e tomaram duas doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  output$R_G_A_UTI_TRS<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,UTI_TRS)%>%
      group_by(year = year(DATA), SEMANA = year(DATA)) %>%select(UTI_TRS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_TRS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram a UTI e tomaram três doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###
  
  
  
  output$R_G_A_INT_ZR<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,INT_ZR)%>%
      group_by(year = year(DATA), SEMANA = year(DATA)) %>%select(INT_ZR)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = INT_ZR)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram intubadas e tomaram zero doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$R_G_A_INT_UMA<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,INT_UMA)%>%
      group_by(year = year(DATA), SEMANA = year(DATA)) %>%select(INT_UMA)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = INT_UMA)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram intubadas e tomaram uma dose")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$R_G_A_INT_DAS<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,INT_DAS)%>%
      group_by(year = year(DATA), SEMANA = year(DATA)) %>%select(INT_DAS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = INT_DAS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram intubadas e tomaram duas doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  output$R_G_A_INT_TRS<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,INT_TRS)%>%
      group_by(year = year(DATA), SEMANA = year(DATA)) %>%select(INT_TRS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = INT_TRS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram intubadas e tomaram três doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  
  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###
  
  output$R_G_A_UTI_OBT_ZR<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,UTI_OBT_ZR)%>%
      group_by(year = year(DATA), SEMANA = year(DATA)) %>%select(UTI_OBT_ZR)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_OBT_ZR)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram para UTI e foram a óbito que tomaram zero doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$R_G_A_UTI_OBT_UMA<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,UTI_OBT_UMA)%>%
      group_by(year = year(DATA), SEMANA = year(DATA)) %>%select(UTI_OBT_UMA)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_OBT_UMA)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram para UTI e foram a óbito que tomaram uma dose")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$R_G_A_UTI_OBT_DAS<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,UTI_OBT_DAS)%>%
      group_by(year = year(DATA), SEMANA = year(DATA)) %>%select(UTI_OBT_DAS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_OBT_DAS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram para UTI e foram a óbito que tomaram duas doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  output$R_G_A_UTI_OBT_TRS<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,UTI_OBT_TRS)%>%
      group_by(year = year(DATA), SEMANA = year(DATA)) %>%select(UTI_OBT_TRS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_OBT_TRS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram para UTI e foram a óbito que tomaram três doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  
  
  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###
  
  output$R_G_A_INT_OBT_ZR<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,INT_OBT_ZR)%>%
      group_by(year = year(DATA), SEMANA = year(DATA)) %>%select(INT_OBT_ZR)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = INT_OBT_ZR)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram intubadas e foram a óbito que tomaram zero doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$R_G_A_INT_OBT_UMA<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,INT_OBT_UMA)%>%
      group_by(year = year(DATA), SEMANA = year(DATA)) %>%select(INT_OBT_UMA)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = INT_OBT_UMA)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas que foram a óbito e tomaram uma dose")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$R_G_A_INT_OBT_DAS<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,INT_OBT_DAS)%>%
      group_by(year = year(DATA), SEMANA = year(DATA)) %>%select(INT_OBT_DAS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = INT_OBT_DAS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram a óbito e tomaram duas doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  output$R_G_A_INT_OBT_TRS<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,INT_OBT_TRS)%>%
      group_by(year = year(DATA), SEMANA = year(DATA)) %>%select(INT_OBT_TRS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = INT_OBT_TRS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram a óbito e tomaram três doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  
  
  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###
  
  
  
  output$R_G_A_UTI_INT_ZR<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,UTI_INT_ZR)%>%
      group_by(year = year(DATA), SEMANA = year(DATA)) %>%select(UTI_INT_ZR)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_INT_ZR)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram para UTI, foram intubadas e tomaram zero doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$R_G_A_UTI_INT_UMA<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,UTI_INT_UMA)%>%
      group_by(year = year(DATA), SEMANA = year(DATA)) %>%select(UTI_INT_UMA)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_INT_UMA)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram para UTI, foram intubadas e tomaram uma dose")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$R_G_A_UTI_INT_DAS<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,UTI_INT_DAS)%>%
      group_by(year = year(DATA), SEMANA = year(DATA)) %>%select(UTI_INT_DAS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_INT_DAS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram para UTI, foram intubadas e tomaram duas doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  output$R_G_A_UTI_INT_TRS<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,UTI_INT_TRS)%>%
      group_by(year = year(DATA), SEMANA = year(DATA)) %>%select(UTI_INT_TRS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_INT_TRS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram para UTI, foram intubadas e tomaram três doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###
  
  
  
  output$R_G_A_UTI_INT_OBT_ZR<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,UTI_INT_OBT_ZR)%>%
      group_by(year = year(DATA), SEMANA = year(DATA)) %>%select(UTI_INT_OBT_ZR)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_INT_OBT_ZR)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram para UTI, foram intubadas e tomaram zero doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$R_G_A_UTI_INT_OBT_UMA<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,UTI_INT_OBT_UMA)%>%
      group_by(year = year(DATA), SEMANA = year(DATA)) %>%select(UTI_INT_OBT_UMA)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_INT_OBT_UMA)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram para UTI, foram intubadas e tomaram uma dose")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  
  output$R_G_A_UTI_INT_OBT_DAS<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,UTI_INT_OBT_DAS)%>%
      group_by(year = year(DATA), SEMANA = year(DATA)) %>%select(UTI_INT_OBT_DAS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_INT_OBT_DAS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas e foram para UTI, foram intubadas e tomaram duas doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  output$R_G_A_UTI_INT_OBT_TRS<- renderHighchart({ 
    cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DATA,UTI_INT_OBT_TRS)%>%
      group_by(year = year(DATA), SEMANA = year(DATA)) %>%select(UTI_INT_OBT_TRS)%>% summarise_all(sum)%>%
      hchart('column', hcaes(x = paste(SEMANA,"_",year), y = UTI_INT_OBT_TRS)) %>% 
      hc_xAxis(title = list(text = " ")) %>% 
      hc_yAxis(title = list(text = "Contagem")) %>% hc_title(text = "Pessoas internadas que foram para UTI, foram intubadas e tomaram três doses")%>%hc_colors(c("green"))%>% 
      hc_add_theme(hc_theme_538())
  }) 
  
  #sem data#################################
  #Apenas doses #####
  output$R_T_ZR <- renderTable(cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(ZR)%>% summarise_all(c(sum,mean,var))%>%`colnames<-`(c("Total","Média","Variância")))
  
  output$R_T_UMA<- renderTable(cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(UMA)%>% summarise_all(c(sum,mean,var))%>%`colnames<-`(c("Total","Média","Variância")))
  
  output$R_T_DAS <- renderTable(cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(DAS)%>% summarise_all(c(sum,mean,var))%>%`colnames<-`(c("Total","Média","Variância")))
  
  output$R_T_TRS<- renderTable(cruza_all %>%filter(regiao == as.character(input$regioes)) %>% select(TRS)%>% summarise_all(c(sum,mean,var))%>%`colnames<-`(c("Total","Média","Variância")))
  
  
  #Apenas doses e obitos#####
  output$R_T_OBT_ZR <- renderTable(cruza_all %>% filter(regiao == as.character(input$regioes))  %>% select(OBT_ZR)%>% summarise_all(c(sum,mean,var))%>%`colnames<-`(c("Total","Média","Variância")))
  
  output$R_T_OBT_UMA <- renderTable(cruza_all %>% filter(regiao == as.character(input$regioes))  %>% select(OBT_UMA)%>% summarise_all(c(sum,mean,var))%>%`colnames<-`(c("Total","Média","Variância")))
  
  output$R_T_OBT_DAS <- renderTable(cruza_all %>% filter(regiao == as.character(input$regioes))  %>% select(OBT_DAS)%>% summarise_all(c(sum,mean,var))%>%`colnames<-`(c("Total","Média","Variância")))
  
  output$R_T_OBT_TRS <- renderTable(cruza_all %>% filter(regiao == as.character(input$regioes))  %>% select(OBT_TRS)%>% summarise_all(c(sum,mean,var))%>%`colnames<-`(c("Total","Média","Variância")))
  
  #Apenas doses e UTI#####
  output$R_T_UTI_ZR <- renderTable(cruza_all %>% filter(regiao == as.character(input$regioes))  %>% select(UTI_ZR)%>% summarise_all(c(sum,mean,var))%>%`colnames<-`(c("Total","Média","Variância")))
  
  output$R_T_UTI_UMA <- renderTable(cruza_all %>% filter(regiao == as.character(input$regioes))  %>% select(UTI_UMA)%>% summarise_all(c(sum,mean,var))%>%`colnames<-`(c("Total","Média","Variância")))
  
  output$R_T_UTI_DAS<- renderTable(cruza_all %>% filter(regiao == as.character(input$regioes))  %>% select(UTI_DAS)%>% summarise_all(c(sum,mean,var))%>%`colnames<-`(c("Total","Média","Variância")))
  
  output$R_T_UTI_TRS <- renderTable(cruza_all %>% filter(regiao == as.character(input$regioes))  %>% select(UTI_TRS)%>% summarise_all(c(sum,mean,var))%>%`colnames<-`(c("Total","Média","Variância")))
  
  #Apenas doses e intubadas#####
  output$R_T_INT_ZR <- renderTable(cruza_all %>% filter(regiao == as.character(input$regioes))  %>% select(INT_ZR)%>% summarise_all(c(sum,mean,var))%>%`colnames<-`(c("Total","Média","Variância")))
  
  output$R_T_INT_UMA <- renderTable(cruza_all %>% filter(regiao == as.character(input$regioes))  %>% select(INT_UMA)%>% summarise_all(c(sum,mean,var))%>%`colnames<-`(c("Total","Média","Variância")))
  
  output$R_T_INT_DAS <- renderTable(cruza_all %>% filter(regiao == as.character(input$regioes))  %>% select(INT_DAS)%>% summarise_all(c(sum,mean,var))%>%`colnames<-`(c("Total","Média","Variância")))
  
  output$R_T_INT_TRS <- renderTable(cruza_all %>% filter(regiao == as.character(input$regioes))  %>% select(INT_TRS)%>% summarise_all(c(sum,mean,var))%>%`colnames<-`(c("Total","Média","Variância")))
  
  
  #Apenas doses e obitos UTI#####
  output$R_T_UTI_OBT_ZR <- renderTable(cruza_all %>% filter(regiao == as.character(input$regioes))  %>% select(UTI_OBT_ZR)%>% summarise_all(c(sum,mean,var))%>%`colnames<-`(c("Total","Média","Variância")))
  
  output$R_T_UTI_OBT_UMA <- renderTable(cruza_all %>% filter(regiao == as.character(input$regioes))  %>% select(UTI_OBT_UMA)%>% summarise_all(c(sum,mean,var))%>%`colnames<-`(c("Total","Média","Variância")))
  
  output$R_T_UTI_OBT_DAS <- renderTable(cruza_all %>% filter(regiao == as.character(input$regioes))  %>% select(UTI_OBT_DAS)%>% summarise_all(c(sum,mean,var))%>%`colnames<-`(c("Total","Média","Variância")))
  
  output$R_T_UTI_OBT_TRS <- renderTable(cruza_all %>% filter(regiao == as.character(input$regioes))  %>% select(UTI_OBT_TRS)%>% summarise_all(c(sum,mean,var))%>%`colnames<-`(c("Total","Média","Variância")))
  
  
  #Apenas doses e obitos intubadas#####
  output$R_T_INT_OBT_ZR <- renderTable(cruza_all %>% filter(regiao == as.character(input$regioes))  %>% select(INT_OBT_ZR)%>% summarise_all(c(sum,mean,var))%>%`colnames<-`(c("Total","Média","Variância")))
  
  output$R_T_INT_OBT_UMA <- renderTable(cruza_all %>% filter(regiao == as.character(input$regioes))  %>% select(INT_OBT_UMA)%>% summarise_all(c(sum,mean,var))%>%`colnames<-`(c("Total","Média","Variância")))
  
  output$R_T_INT_OBT_DAS <- renderTable(cruza_all %>% filter(regiao == as.character(input$regioes))  %>% select(INT_OBT_DAS)%>% summarise_all(c(sum,mean,var))%>%`colnames<-`(c("Total","Média","Variância")))
  
  output$R_T_INT_OBT_TRS <- renderTable(cruza_all %>% filter(regiao == as.character(input$regioes))  %>% select(INT_OBT_TRS)%>% summarise_all(c(sum,mean,var))%>%`colnames<-`(c("Total","Média","Variância")))
  
  
  #Apenas doses e  UTI intubadas#####
  output$R_T_UTI_INT_ZR <- renderTable(cruza_all %>% filter(regiao == as.character(input$regioes))  %>% select(UTI_INT_ZR)%>% summarise_all(c(sum,mean,var))%>%`colnames<-`(c("Total","Média","Variância")))
  
  output$R_T_UTI_INT_UMA <- renderTable(cruza_all %>% filter(regiao == as.character(input$regioes))  %>% select(UTI_INT_UMA)%>% summarise_all(c(sum,mean,var))%>%`colnames<-`(c("Total","Média","Variância")))
  
  output$R_T_UTI_INT_DAS <- renderTable(cruza_all %>% filter(regiao == as.character(input$regioes))  %>% select(UTI_INT_DAS)%>% summarise_all(c(sum,mean,var))%>%`colnames<-`(c("Total","Média","Variância")))
  
  output$R_T_UTI_INT_TRS <- renderTable(cruza_all %>% filter(regiao == as.character(input$regioes))  %>% select(UTI_INT_TRS)%>% summarise_all(c(sum,mean,var))%>%`colnames<-`(c("Total","Média","Variância")))
  
  #Apenas doses e  UTI e intubadas e obitos#####
  output$R_T_UTI_INT_OBT_ZR <- renderTable(cruza_all %>% filter(regiao == as.character(input$regioes))  %>% select(UTI_INT_OBT_ZR)%>% summarise_all(c(sum,mean,var))%>%`colnames<-`(c("Total","Média","Variância")))
  
  output$R_T_UTI_INT_OBT_UMA <- renderTable(cruza_all %>% filter(regiao == as.character(input$regioes))  %>% select(UTI_INT_OBT_UMA)%>% summarise_all(c(sum,mean,var))%>%`colnames<-`(c("Total","Média","Variância")))
  
  output$R_T_UTI_INT_OBT_DAS <- renderTable(cruza_all %>% filter(regiao == as.character(input$regioes))  %>% select(UTI_INT_OBT_DAS)%>% summarise_all(c(sum,mean,var))%>%`colnames<-`(c("Total","Média","Variância")))
  
  output$R_T_UTI_INT_OBT_TRS <- renderTable(cruza_all %>% filter(regiao == as.character(input$regioes))  %>% select(UTI_INT_OBT_TRS)%>% summarise_all(c(sum,mean,var))%>%`colnames<-`(c("Total","Média","Variância")))
  
  
  
##########  

})

#####Server ^ #####
