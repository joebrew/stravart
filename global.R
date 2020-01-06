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
library(logging)
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

# initialise logging ----
# basicConfig()
# dir.create('./logs/',showWarnings = F)
# addHandler(writeToFile,file=glue('./logs/{strftime(Sys.time(),\'%Y%m%d-%H%M%S\')}.log'))
#addHandler(writeToConsole)

# file dependencies ----

# loginfo('Load file depencencies',logger='authentication')
source('./utils.R')
source("./dplyr_verbs.R")


# Application options ----

# dir.create('cache',showWarnings = F)
# cache <- F # whether to load cached data (must have authenticated before)
# 
# if (cache) {
#   loginfo('Use cached credentials and data',logger='authentication')
# }

# app misc ----
periods <- list(
  'This week' = c(floor_date(Sys.Date()-1,unit = 'week')+1,as.Date(strftime(Sys.Date(),'%Y-%m-%d'))),
  'Last 7 days' = c(Sys.Date()-6,Sys.Date()),
  'This month' = c(floor_date(Sys.Date(),unit = 'month'),as.Date(strftime(Sys.Date(),'%Y-%m-%d'))),
  'Last 30 days' = c(Sys.Date()-29,Sys.Date()),
  'This year' = c(floor_date(Sys.Date(),unit = 'year'),Sys.Date()),
  'Last week' = c(floor_date(Sys.Date()-1-7,unit = 'week')+1,floor_date(Sys.Date()-1,unit = 'week')),
  'Last month' = c(floor_date(Sys.Date(),unit = 'month') - months(1),floor_date(Sys.Date(),'month')-1),
  'All time' = c(as.Date('1900-01-01'),as.Date(Sys.Date())),
  'Custom' = c(floor_date(Sys.Date()-1,unit = 'week')+1,as.Date(strftime(Sys.Date(),'%Y-%m-%d')))
)


# Connect to the database
creds <- yaml.load_file('credentials.yaml')
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
# 
# # Already geocoded starting locations
# old_starting_locations <- dbReadTable(conn = con,name = 'starting_locations')
