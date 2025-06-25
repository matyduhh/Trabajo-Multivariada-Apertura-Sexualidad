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
  select(p3, p4, i_5_p28, p31, p32, i_2_p41, p63, i_1_p102, i_3_p102, p98, i_8_p102, p269, 
         p270, p38, p134, i_3_p28, i_7_p28, i_1_p41, i_3_p41)

#/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/

# II- Recodificación de variables

# 1) variable género # justificar que agrupamos a mujeres con 
# mujeres trans, y por qué trabajamos sólo con mujeres
proc_enssex <- proc_enssex %>%
  mutate (gen = ifelse(p3 %in% c(2, 4), "1", NA))


#/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/

#III- Creación de escalas y recodificacion de variables para crear la variable dependiente

# 1) variable educ_placer (0-2)
proc_enssex <- proc_enssex %>%
  mutate(educ_placer = case_when(
    i_2_p41 == 1 ~ 0,
    i_2_p41 == 2 ~ 1,
    i_2_p41 == 3 ~ 2))

# 2) variable motiv_1sex (0-2)
proc_enssex <- proc_enssex %>%
  mutate(motiv_1sex = case_when(
    p63 %in% c(1, 2) ~ 2,
    p63 %in% c(3) ~ 1,
    p63 %in% c(4, 5, 6) ~ 0))

# 3) variable exploracion_sex (0-2)
# -recodificar i_1_p102
proc_enssex$uso_juguetes <- ifelse(proc_enssex$i_1_p102 == 1, 1,
                                   ifelse(proc_enssex$i_1_p102 == 2, 0, NA))

# -recodificar i_3_p102
proc_enssex$uso_productos <- ifelse(proc_enssex$i_3_p102 == 1, 1,
                                    ifelse(proc_enssex$i_3_p102 == 2, 0, NA))

# -crear escala sumativa (0=ninguno - 1=ha usado uno - 2=ha usado ambos)
proc_enssex$exploracion_sex <- proc_enssex$uso_juguetes + proc_enssex$uso_productos

# 4) variable masturbacion a otra persona (0-1)
# -recodificar i_8_p102
proc_enssex <- proc_enssex %>%
  mutate(mast_alguien = case_when(i_8_p102 == 1 ~ 1, i_8_p102 == 2 ~ 0))

# 5) variable masturbacion propia (0-1)
# -recodificar p98
proc_enssex <- proc_enssex %>% 
  mutate(mast_propia = case_when(p98 == 1 ~ 1, p98 == 2 ~ 0))

#/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/
# IV- Tratamiento de NA 
#- Transformar valores missing a NA
proc_enssex$i_3_p28[proc_enssex$i_3_p28 %in% c(8, 9)] <- NA
proc_enssex$i_5_p28[proc_enssex$i_5_p28 %in% c(8, 9)] <- NA
proc_enssex$i_7_p28[proc_enssex$i_7_p28 %in% c(8, 9)] <- NA
proc_enssex$p38[proc_enssex$p38 %in% c(8, 9)] <- NA
proc_enssex$i_1_p41[proc_enssex$i_1_p41 %in% c(9)] <- NA
proc_enssex$i_2_p41[proc_enssex$i_2_p41 %in% c(9)] <- NA
proc_enssex$i_3_p41[proc_enssex$i_3_p41 %in% c(9)] <- NA
proc_enssex$p98[proc_enssex$p98 %in% c(9)] <- NA
proc_enssex$p134[proc_enssex$p134 %in% c(1, 6, 8, 9)] <- NA 
proc_enssex$p269[proc_enssex$p269 %in% c(9)] <- NA
proc_enssex$p31[proc_enssex$p31 %in% c(9)] <- NA
proc_enssex$p32[proc_enssex$p32 %in% c(9)] <- NA


# Tratamiento de NA (justificar por qué eliminamos los NA) (1857 obs. 27 variables)
proc_enssex <- na.omit(proc_enssex)

#/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/

# V- Creción de variable dependiente
#- crear matriz de correlacion
matriz_1 <- proc_enssex %>% 
  dplyr::select (educ_placer, motiv_1sex, exploracion_sex, mast_alguien, mast_propia)

#- ver matriz de correlacion (ver que no existan correlaciones negativas)
sjPlot::tab_corr(matriz_1,
                 triangle = "lower")

#- ver alpha de cronbach para consistencia interna (o.53)
psych::alpha(matriz_1)

#- creación de la escala sumativa (0-8)
proc_enssex$apertura_sex <- proc_enssex$educ_placer + 
  proc_enssex$motiv_1sex + proc_enssex$exploracion_sex + proc_enssex$mast_propia + proc_enssex$mast_alguien

#/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/

#VI Creación variables predictoras

# 1) variable orientación sexual (+) (justificar que eliminamos a los gays porque dice textualmente que es la
# "atracción de un hombre hacia otro hombre" y estamos investigando mujeres)
proc_enssex <- proc_enssex %>%
  mutate(orient_sex = case_when(
    p134 == 5 ~ 2,        
    p134 %in% c(2, 3) ~ 1, 
    p134 == 4 ~ 0        
  ))


# 2) variable religion (-) (0 bajo, 1 medio, 2 alto)
proc_enssex <- proc_enssex %>%
  mutate(escala_religion = case_when(
    p269 %in% c(1, 2)  ~ 2,
    p269 == 3  ~ 1,
    p269 %in% c(4, 5) ~ 0
    ))

# 3) variable política (-) (0 = izq, 1 = centro, 2 = derecha, 3 = No sabe / No responde)
proc_enssex <- proc_enssex %>%
  mutate(orient_politica = case_when(
    p270 %in% c(1, 2, 3, 4) ~ 0,           
    p270 %in% c(5, 6) ~ 1,     
    p270 %in% c(7, 8, 9, 10) ~ 2,    
    p270 == 99 ~ 3                 
  ))

# 4) variable educacion sexual en colegio (+) (0=mala, 1=ni buena ni mala, 2=buena)
proc_enssex <- proc_enssex %>%
  mutate(ed_sex = case_when(
    p38 %in% c(1, 2) ~ 0,
    p38 %in% 3 ~ 1,
    p38 %in% c(4, 5) ~ 2))

# 5) conservadurismo (-)

# 1) variable aborto (0-1)
proc_enssex <- proc_enssex %>%
  mutate(aborto = case_when(
    i_5_p28 %in% c(1, 2) ~ 1,
    i_5_p28 %in% c(3, 4) ~ 0))

# 2) variable aceptacion_dis (0-2)
# -recodificar p31
proc_enssex$acept_min_sex <- proc_enssex %>%
  mutate(acept_min_sex = case_when(
    p31 %in% c(2, 3) ~ 1,
    p31 == 1 ~ 0))


# -recodificar p32
proc_enssex$acept_trans <- ifelse(proc_enssex$p32 == 1, 1,
                                  ifelse(proc_enssex$p32 == 2, 0, 
                                         ifelse(proc_enssex$p32 == 3, 0, NA)))

# -crear escala sumativa (0=menor - 2=mayor)
proc_enssex$acept_dis <- proc_enssex$acept_min_sex + proc_enssex$acept_trans
