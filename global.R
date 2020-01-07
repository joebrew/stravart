# global.R is sourced when the application is lauched

# dependencies
library(shiny)
library(rStrava)
library(tidyverse)
library(lubridate)
library(httr)
library(jsonlite)
library(glue)
library(DT)
library(leaflet)
library(highcharter)
library(shinyjs)
library(shinydashboard)
library(shinythemes)
library(googlePolylines)
library(tidyquant)
library(yaml)
require(dplyr)
require(RPostgreSQL)
require(readr)
require(DBI)
library(gridExtra)
library(revgeo)
library(sp)

# Get creds
creds <- yaml.load_file('credentials.yaml')

# Source helper scripts
source('./utils.R')
source("./dplyr_verbs.R")
source('./plot_functions.R')

# Connect to the database
creds_list <- credentials_extract( credentials_path = '.')
creds_list <- creds_list[names(creds_list) %in% c('dbname', 'host', 'port', 'user', 'password')]
creds_list$drv <- DBI::dbDriver("PostgreSQL")
# Connect to the db
con = do.call(DBI::dbConnect, creds_list)
# con = do.call(src_postgres, creds_list)
# con = DBI::dbConnect(pg, dbname="stravart")
message('-Connected to the stravart database')
for(i in 1:length(creds_list)){
  if(!names(creds_list)[i] %in% c('password', 'drv')){
    message('---', names(creds_list)[i], ': ', creds_list[[i]])
  }
}