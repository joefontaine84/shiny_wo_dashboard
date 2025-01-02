# The goal of this script is to connect to the workout data

library(tidyverse)
library(googlesheets4)
library(gargle)
library(jsonlite)



get_data <- function(){
  
  # service account details from google cloud platform. The service account
  # will allow read/write operations to a given google sheet after the google sheet
  # is configured to be shareable to anyone with a link.
  service_account_path <- "workout.json"
  gs4_auth(path = service_account_path)
  
  sheet <- "https://docs.google.com/spreadsheets/d/1N3VfTiKHuZOXs0Df0ya1YPU7ZQqCuWfgboJkYTOLRLM/edit?gid=0#gid=0"
  
  df <- googlesheets4::read_sheet(sheet, col_types = "Dnnnnnncc")
  
  return(df)
  
}


