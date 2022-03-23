# the necessary packages
install.packages("RPostgres")
install.packages("leaflet")
install.packages("plotly")
install.packages("shinythemes")
install.packages("shinyWidgets")
library(leaflet)
library(plotly)
library(RPostgres)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)

con <- dbConnect(
  drv = dbDriver('Postgres'),
  dbname = 'ytube',
  host = 'db-postgresql-nyc1-44203-do-user-8018943-0.b.db.ondigitalocean.com',
  port = 25061,
  user = 'ytuser1',
  password = 'H6yeTMl7dOIHCg1b',
  sslmode = 'require'
)

onStop(
  function()
  {
    dbDisconnect(con)
  }
)