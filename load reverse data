#full_Base

#preparacion base
#cargar base - load database
library(readxl)
library(tidyverse)
library(janitor)
library(fastDummies)
library(readxl)
library(dplyr)
library(tidyverse)
library(Hmisc)
library(janitor)
library(ggpubr)
library(gridExtra)
library(grid)

data <- read_excel("")

#######reverse variables a_ and ghq######

data %>% 
  dplyr::select(-id,
                -Digitador,
                -Período) %>%
  dplyr::mutate(a_06 = case_when(a_06 == "2_4"~"2", #2_4
                                 T ~ as.character(a_06)),
                a_06 = as.numeric(a_06),
                GHQ_05 = case_when(GHQ_05 == "1_2"~"1", #2_4
                                   T ~ as.character(GHQ_05)),
                GHQ_05 = as.numeric(GHQ_05),
                dem_01 = case_when(dem_01 == "ilegible"~"NA",
                                   T ~ as.character(dem_01)),
                dem_01 = as.numeric(dem_01)) %>% 
  dplyr::mutate(#estigma
    a_01 = 6-a_01,
    a_02 = 6-a_02,
    a_03 = 6-a_03,
    a_04 = 6-a_04,
    a_08 = 6-a_08,
    a_10 = 6-a_10,
    a_11 = 6-a_11,
    #morbilidad salud mental
    GHQ_02 = 5-GHQ_02) %>% 
  dplyr::select(-starts_with("c1_"),
                -starts_with("RSA_"),
                -starts_with("Cond_"),
                -Princ_enfer) %>% 
  dplyr::mutate(suma_stigm = rowSums(.[1:16]),
                prom_stigm = suma_stigm/16,
                suma_disc = rowSums(.[17:21]),
                suma_GHQ = rowSums(.[22:33])) %>% 
  dplyr::select(starts_with("a_"),
                starts_with("b_"),
                starts_with("GHQ_"),
                dem_01,
                dem_02,
                dem_03,
                dem_08,
                dem_09,
                dem_11,
                dem_12,
                suma_stigm,
                prom_stigm,
                suma_disc,
                suma_GHQ) %>% 
  dplyr::mutate(age = dem_01,
                gender = case_when(dem_02=="b"~0,
                                   dem_02=="a"~1),
                rel_status = case_when(
                  dem_03=="a"~0,
                  dem_03=="b"~0,
                  dem_03=="c"~0,
                  dem_03=="d"~1,
                  dem_03=="e"~1,
                  dem_03=="f"~1,
                  dem_03=="g"~1,
                  dem_03=="h"~2),
                religion = case_when(
                  dem_08=="a"~0,
                  dem_08=="b"~1,
                  dem_08=="c"~2,
                  dem_08=="d"~2,
                  dem_08=="e"~2,
                  dem_08=="f"~2,
                  dem_08=="g"~2,
                  dem_08=="h"~2,
                  dem_08=="l"~2,
                  dem_08=="i"~3,
                  dem_08=="j"~3,
                  dem_08=="k"~3),
                time_cesfam = case_when(
                  dem_09=="a"~0,
                  dem_09=="b"~0,
                  dem_09=="c"~0,
                  dem_09=="d"~0,
                  dem_09=="e"~0,
                  dem_09=="f"~1),
                substance_use = case_when(dem_11=="a"~1,
                                          dem_11=="b"~0),
                education_level = case_when(dem_12=="a"~0,
                                            dem_12=="b"~0,
                                            dem_12=="c"~1,
                                            dem_12=="d"~1,
                                            dem_12=="e"~2,
                                            dem_12=="f"~2,
                                            dem_12=="g"~3,
                                            dem_12=="h"~3,
                                            dem_12=="l"~3,
                                            dem_12=="i"~3,
                                            dem_12=="j"~3)) %>% 
  dplyr::select(-starts_with("dem_"))-> data

