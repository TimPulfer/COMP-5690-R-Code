# Author:           Timothy Pulfer
# Instructor:       Ashok Krishnamurthy
# Course:           COMP 5690 (FALL 2021)
#
#
# Purpose:          To produce a R Shiny application that displays statistical
#                   non spatial data on different epidemiological models based on
#                   user input.

library(shiny)
library(shinyjs)
library(shinyhelper)
library(shinyWidgets)
library(deSolve)
library(ggplot2)
library(tidyverse)

shinyApp(
  ui= source('ui.r',local=TRUE),
  server=source('server.r',session)
)