################################################################################

#Analisis 

#Entrega 2

#Cargamos las librerias
pacman::p_load(tidyverse, # Manipulacion de datos
               car, # Recodificar
               sjPlot, # Tablas y graficos
               sjmisc, # Descriptivos
               kableExtra, # Tablas
               psych, # Bivariados
               corrplot, # Graficos correlación
               broom, 
               gginference,
               ggplot2,
               dplyr,
               stargazer,
               lme4,
               haven) # Varios

options(scipen = 999) # para desactivar notacion cientifica
rm(list = ls()) # para limpiar el entorno de trabajo

#Cargamos la base de datos
load("inv.RData")

