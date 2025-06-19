
#Trabajo multivariada: Grado de Apertura de la sexualidad femenina

#/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/
# 0- Cargar librerías y base de datos
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

load(url("https://datos.gob.cl/dataset/c6983439-49f6-4e71-85fe-e8de6e73dae0/resource/ed81f50c-1c7d-43d9-9083-dfc161e0cd66/download/20240516_enssex_data.rdata"))

load("inv.RData")

#/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/

# I- Seleccion de variables
proc_enssex <- enssex4 %>%
  select(p3, i_3_p28, i_5_p28, i_7_p28, p31, p32, p34, p38, 
         i_1_p41, i_2_p41, i_3_p41, i_4_p41, p63, p98, i_1_p102, 
         i_3_p102, i_8_p102, p134, p269, p270)

#/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/

# II- Recodificación de variables

# 1) variable género # justificar que agrupamos a mujeres con 
# mujeres trans, y por qué trabajamos sólo con mujeres
proc_enssex <- proc_enssex %>%
  mutate (gen = ifelse(p3 %in% c(2, 4), "1", NA))

# 2) variable orientacion sexual #justificar que eliminamos a gays y otros
proc_enssex$p134[proc_enssex$p134 %in% c(1, 5)] <- 9

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

# Tratamiento de NA (justificar por qué eliminamos los NA) (5624 obs. 32 variables)
proc_enssex <- na.omit(proc_enssex)

#/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/

# V- Creción de variable dependiente
#- crear matriz de correlacion
matriz_1 <- proc_enssex %>% 
  dplyr::select(aborto, acept_dis, educ_placer, motiv_1sex, exploracion_sex, mast_alguien, mast_propia)

#- ver matriz de correlacion (ver que no existan correlaciones negativas)
sjPlot::tab_corr(matriz_1,
                 triangle = "lower")

#- ver alpha de cronbach para consistencia interna (o.57)
psych::alpha(matriz_1)

#- creación de la escala sumativa (0-13)
proc_enssex$apertura_sex <- proc_enssex$aborto + proc_enssex$acept_dis + proc_enssex$educ_placer + 
  proc_enssex$motiv_1sex + proc_enssex$exploracion_sex + proc_enssex$mast_propia + proc_enssex$mast_alguien

#/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/

# VI- Creación de variables predictoras

# 1) variable religion (-) (0 nada religioso, 4 muy religioso)
proc_enssex <- proc_enssex %>%
  mutate(escala_religion = case_when(
    p269 == 1 ~ 4,
    p269 == 2 ~ 3,
    p269 == 3 ~ 2,
    p269 == 4 ~ 1,
    p269 == 5 ~ 0))

# 2) variable política (-) (1 = ultra izq, 2 = izq, 3 = centro, 4 = der, 5 = ultra der)
proc_enssex <- proc_enssex %>%
  mutate(escala_politica = case_when(
    p270 %in% c(1, 2)   ~ 1,
    p270 %in% c(3, 4)   ~ 2,
    p270 %in% c(5, 6)   ~ 3,
    p270 %in% c(7, 8)   ~ 4,
    p270 %in% c(9, 10)  ~ 5))

# 3) variable educacion sexual en colegio (+) (0=muy mala, 2=ni buena ni mala, 4=muy buena)
proc_enssex <- proc_enssex %>%
  mutate(ed_sex = case_when(
    p38 == 1 ~ 0,
    p38 == 2 ~ 1,
    p38 == 3 ~ 2,
    p38 == 4 ~ 3,
    p38 == 5 ~ 4))


# 4) variable orientación sexual (+) (justificar que eliminamos a los gays porque dice textualmente que es la
# "atracción de un hombre hacia otro hombre" y estamos investigando mujeres)
proc_enssex$orient_sex <- ifelse(proc_enssex$p134 %in% c(2, 3), "1",
                                 ifelse(proc_enssex$p134 == 4, "0", NA))

# 5) variable orientaciones normativas (-)
#-recodificar i_3_p28
proc_enssex <- proc_enssex %>%
  mutate(i_3_p28 = case_when(
    i_3_p28 == 1 ~ 0,
    i_3_p28 == 2 ~ 1,
    i_3_p28 == 3 ~ 2,
    i_3_p28 == 4 ~ 3))

#-recodificar i_7_p28
proc_enssex <- proc_enssex %>%
  mutate(i_7_p28 = case_when(
    i_7_p28 == 1 ~ 0,
    i_7_p28 == 2 ~ 1,
    i_7_p28 == 3 ~ 2,
    i_7_p28 == 4 ~ 3))

#-recodificar i_1_p41
proc_enssex <- proc_enssex %>%
  mutate(i_1_p41 = case_when(
    i_1_p41 == 1 ~ 0,
    i_1_p41 == 2 ~ 1,
    i_1_p41 == 3 ~ 2))

#-recodificar i_3_p41
proc_enssex <- proc_enssex %>%
  mutate(i_3_p41 = case_when(
    i_3_p41 == 1 ~ 0,
    i_3_p41 == 2 ~ 1,
    i_3_p41 == 3 ~ 2))

#- crear matriz de correlacion
matriz_2 <- proc_enssex %>% 
  dplyr::select(i_3_p28, i_7_p28, i_1_p41, i_3_p41)

#- ver matriz de correlacion (ver que no existan correlaciones negativas)
sjPlot::tab_corr(matriz_2,
                 triangle = "lower")

#- ver alpha de cronbach para consistencia interna (o.56)
psych::alpha(matriz_2)

#-crear escala sumativa (0-10)
proc_enssex$escala_orient_norm <- proc_enssex$i_3_p28 + proc_enssex$i_7_p28 + proc_enssex$i_1_p41 + 
  proc_enssex$i_3_p41 

#/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/

#VII- Selección de variables de interés y base de datos limpia

inv_com <- proc_enssex %>%
  select(apertura_sex, escala_religion, escala_politica, escala_orient_norm, ed_sex, orient_sex)

#/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/

#VIII- Selección de 1000 casos aleatorios *seleccionamos arbitrariamente los 215 casos de disidencias

#-tomar total de casos disidentes (todos los que tienen orient_sex == 1)
sub_dis <- subset(inv_com, orient_sex == 1)

#-extraer muestra aleatoria de heterosexuales para completar 1500 casos
al_hetero <- subset(inv_com, orient_sex == 0)

set.seed(123)  # para reproducibilidad
sub_hetero <- al_hetero[sample(nrow(al_hetero), 1000 - nrow(sub_dis)), ]

#-combinar ambas muestras y obtener submuestra final
inv <- rbind(sub_dis, sub_hetero)

#para verificar que son los mismos casos 7-9-6-8-9-11
head(inv$apertura_sex)

#/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/-/

#IX- Guardar base de datos (para mi escritorio, dejé el código igualmente por si se evalua)
save(inv, file = "~/Desktop/trabajo_final_estadistica/inv.RData")

