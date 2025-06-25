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

load(url("https://datos.gob.cl/dataset/c6983439-49f6-4e71-85fe-e8de6e73dae0/resource/ed81f50c-1c7d-43d9-9083-dfc161e0cd66/download/20240516_enssex_data.rdata"))

#/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/

names(enssex4)

#Filtramos por edad de 18 a 29 años -> jovenes
proc_enssex <- enssex4 %>% filter(p4 >= 18 & edad <= 29) 

# I- Seleccion de variables
proc_enssex <- proc_enssex %>%
  select(p3, i_3_p28, i_5_p28, i_7_p28, p31, p32, p34, p38, 
         i_1_p41, i_2_p41, i_3_p41, i_4_p41, p63, p98, i_1_p102, 
         i_3_p102, i_8_p102, p134, p269, p270, p4)

#/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/

# II- Recodificación de variables

# 1) variable género # justificar que agrupamos a mujeres con 
# mujeres trans, y por qué trabajamos sólo con mujeres
proc_enssex <- proc_enssex %>%
  mutate (gen = ifelse(p3 %in% c(2, 4), "1", NA))

# 2) variable orientacion sexual #justificar que eliminamos a gays 
proc_enssex$p134[proc_enssex$p134 %in% c(1)] <- 9 

#cambiamos aca para agregar la categoria "otros", solo eliminamos categoria "gays"

#/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/

# III- Creación de escalas y recodificacion de variables para crear la variable dependiente

# 1) variable aborto (0-1)
proc_enssex <- proc_enssex %>%
  mutate(aborto = case_when(
    i_5_p28 %in% c(1, 2) ~ 0,
    i_5_p28 %in% c(3, 4) ~ 1))

# 2) variable aceptacion_dis (0-2)
# -recodificar p31
proc_enssex$acept_min_sex <- ifelse(proc_enssex$p31 == 1, 1,
                                    ifelse(proc_enssex$p31 == 2, 0, 
                                           ifelse(proc_enssex$p31 == 3, 0, NA)))


# -recodificar p32
proc_enssex$acept_trans <- ifelse(proc_enssex$p32 == 1, 1,
                                  ifelse(proc_enssex$p32 == 2, 0, 
                                         ifelse(proc_enssex$p32 == 3, 0, NA)))

# -crear escala sumativa (0=menor - 2=mayor)
proc_enssex$acept_dis <- proc_enssex$acept_min_sex + proc_enssex$acept_trans

# 3) variable educ_placer (0-2)
proc_enssex <- proc_enssex %>%
  mutate(educ_placer = case_when(
    i_2_p41 == 1 ~ 0,
    i_2_p41 == 2 ~ 1,
    i_2_p41 == 3 ~ 2))

# 4) variable motiv_1sex (0-2)
proc_enssex <- proc_enssex %>%
  mutate(motiv_1sex = case_when(
    p63 %in% c(1, 2) ~ 2,
    p63 %in% c(3) ~ 1,
    p63 %in% c(4, 5, 6) ~ 0))

# 5) variable exploracion_sex (0-2)
# -recodificar i_1_p102
proc_enssex$uso_juguetes <- ifelse(proc_enssex$i_1_p102 == 1, 1,
                                   ifelse(proc_enssex$i_1_p102 == 2, 0, NA))

# -recodificar i_3_p102
proc_enssex$uso_productos <- ifelse(proc_enssex$i_3_p102 == 1, 1,
                                    ifelse(proc_enssex$i_3_p102 == 2, 0, NA))

# -crear escala sumativa (0=ninguno - 1=ha usado uno - 2=ha usado ambos)
proc_enssex$exploracion_sex <- proc_enssex$uso_juguetes + proc_enssex$uso_productos

# 6) variable masturbacion a otra persona (0-1)
# -recodificar i_8_p102
proc_enssex <- proc_enssex %>%
  mutate(mast_alguien = case_when(i_8_p102 == 1 ~ 1, i_8_p102 == 2 ~ 0))

# 7) variable masturbacion propia (0-1)
# -recodificar p98
proc_enssex <- proc_enssex %>% 
  mutate(mast_propia = case_when(p98 == 1 ~ 1, p98 == 2 ~ 0))

#/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/
# IV- Tratamiento de NA 
#- Transformar valores missing a NA
proc_enssex$i_3_p28[proc_enssex$i_3_p28 %in% c(8, 9)] <- NA
proc_enssex$i_5_p28[proc_enssex$i_5_p28 %in% c(8, 9)] <- NA
proc_enssex$i_7_p28[proc_enssex$i_7_p28 %in% c(8, 9)] <- NA
proc_enssex$p34[proc_enssex$p34 %in% c(8, 9)] <- NA
proc_enssex$p38[proc_enssex$p38 %in% c(8, 9)] <- NA
proc_enssex$i_1_p41[proc_enssex$i_1_p41 %in% c(9)] <- NA
proc_enssex$i_2_p41[proc_enssex$i_2_p41 %in% c(9)] <- NA
proc_enssex$i_3_p41[proc_enssex$i_3_p41 %in% c(9)] <- NA
proc_enssex$i_4_p41[proc_enssex$i_4_p41 %in% c(9)] <- NA
proc_enssex$p98[proc_enssex$p98 %in% c(9)] <- NA
proc_enssex$p134[proc_enssex$p134 %in% c(6, 8, 9)] <- NA 
proc_enssex$p269[proc_enssex$p269 %in% c(9)] <- NA
proc_enssex$p270[proc_enssex$p270 %in% c(99)] <- NA
proc_enssex$p4[proc_enssex$p4 %in% c(999)] <- NA  # Solo si aplica

# Filtrar personas de 18 a 29 años
proc_enssex <- proc_enssex %>%
  filter(!is.na(p4) & p4 >= 18 & p4 <= 29) #aca se eliminan los que pueden estar en edad

# Tratamiento de NA (justificar por qué eliminamos los NA) (2203 obs. 32 variables)
proc_enssex <- na.omit(proc_enssex)

#/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/

# V- Creción de variable dependiente
#- crear matriz de correlacion
matriz_1 <- proc_enssex %>% 
  dplyr::select(aborto, acept_dis, educ_placer, motiv_1sex, exploracion_sex, mast_alguien, mast_propia)

#- ver matriz de correlacion (ver que no existan correlaciones negativas)
sjPlot::tab_corr(matriz_1,
                 triangle = "lower")

#- ver alpha de cronbach para consistencia interna (o.54 estandariza, 0.49 clasico)
psych::alpha(matriz_1)

#- creación de la escala sumativa (0-13)
proc_enssex$apertura_sex <- proc_enssex$aborto + proc_enssex$acept_dis + proc_enssex$educ_placer + 
  proc_enssex$motiv_1sex + proc_enssex$exploracion_sex + proc_enssex$mast_propia + proc_enssex$mast_alguien

#/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/

table(proc_enssex$p134)

