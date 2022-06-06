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

port <- Sys.getenv('PORT')

shiny::runApp(
  appDir = getwd(),
  host = '0.0.0.0',
  port = as.numeric(port)
) 
