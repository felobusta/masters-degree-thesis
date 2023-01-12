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

#read base

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

##some analysis

#correlation between quantitative variables
res2 <- rcorr(as.matrix(data %>% 
                          dplyr::select(prom_stigm,
                                 suma_disc,
                                 suma_GHQ,
                                 age)),type = "spearman")


res2

#correlation between ítems and the scale
corr.items.stigma <- rcorr(as.matrix(data %>% 
                          dplyr::select(starts_with("a_"))),type = "spearman")

corr.items.stigma

#check differences between groups - non-parametric test
#main variable (mean stigma, sum discrimination): 
lapply(data %>% 
         select(time_cesfam,substance_use,gender),
       function(x) wilcox.test(data$prom_stigm~factor(x)))
       
lapply(data %>% 
         select(education_level,religion,rel_status),
       function(x) wilcox.test(data$prom_stigm~factor(x)))
       
lapply(data %>% 
         select(time_cesfam,substance_use,gender),
       function(x) wilcox.test(data$suma_disc~factor(x)))
       
lapply(data %>% 
         select(education_level,religion,rel_status),
       function(x) wilcox.test(data$suma_disc~factor(x)))
       
#let's check age for some reason
lapply(data %>% 
         select(time_cesfam,substance_use,gender),
       function(x) wilcox.test(data_SEM$age~factor(x)))
       

      
#means and percentages
#if you are reading this, this code is not the best way to get multiple means and frequencies, it's actually a really bad way because you are going to use too much code
       
############## DATA MEAN perceived provider stigma and frequencies ############

#get mean
lapply(data %>%
         select(education_level,religion,
                rel_status,gender,
                substance_use,time_cesfam),
       function(x) aggregate(data_SEM$prom_stigm, by=list(x),
                             FUN=mean,na.rm=T))->promedios_percibido
#promedios_percibido is a list, so get the variable categories and mean vectors 

promedios_percibido$education_level$Group.1->vector_a
promedios_percibido$education_level$x->vector_b
promedios_percibido$gender$Group.1->vector_c
promedios_percibido$gender$x->vector_d
promedios_percibido$religion$Group.1->vector_e
promedios_percibido$religion$x->vector_f
promedios_percibido$rel_status$Group.1->vector_g
promedios_percibido$rel_status$x->vector_h
promedios_percibido$substance_use$Group.1->vector_i
promedios_percibido$substance_use$x->vector_j
promedios_percibido$time_cesfam$Group.1->vector_k
promedios_percibido$time_cesfam$x->vector_l
       
#make a table with the frequency of each category

data %>%
  select(education_level,religion,
         rel_status,gender,
         substance_use,time_cesfam) %>% 
  lapply(table)-> tablas_todos

#bind frequency of each category and means perceived provider stigma x variable
rbind(data.frame(group=vector_a,
                 mean=vector_b,
                 tablas_todos$education_level,
                 var="educacion"),
      data.frame(group=vector_c,
                 mean=vector_d,
                 tablas_todos$gender,
                 var="gender"),
      data.frame(group=vector_e,
                 mean=vector_f,
                 tablas_todos$religion,
                 var="religion"),
      data.frame(group=vector_g,
                 mean=vector_h,
                 tablas_todos$rel_status,
                 var="rel_status"),
      data.frame(group=vector_i,
                 mean=vector_j,
                 tablas_todos$substance_use,
                 var="substance_use"),
      data.frame(group=vector_k,
                 mean=vector_l,
                 tablas_todos$time_cesfam,
                 var="time_cesfam"))-> estigma_mean_freq;estigma_mean_freq

estigma_mean_freq %>% writexl::write_xlsx("estigma promedios frecuencias.xlsx")


############## we do the same for discrimination ############

lapply(data %>%
         select(education_level,religion,
                rel_status,gender,
                substance_use,time_cesfam),
       function(x) aggregate(data_SEM$suma_disc, by=list(x),
                             FUN=mean,na.rm=T))->promedios_discri

promedios_discri$education_level$Group.1->vector_a
promedios_discri$education_level$x->vector_b
promedios_discri$gender$Group.1->vector_c
promedios_discri$gender$x->vector_d
promedios_discri$religion$Group.1->vector_e
promedios_discri$religion$x->vector_f
promedios_discri$rel_status$Group.1->vector_g
promedios_discri$rel_status$x->vector_h
promedios_discri$substance_use$Group.1->vector_i
promedios_discri$substance_use$x->vector_j
promedios_discri$time_cesfam$Group.1->vector_k
promedios_discri$time_cesfam$x->vector_l

data %>%
  select(education_level,religion,
         rel_status,gender,
         substance_use,time_cesfam) %>% 
  lapply(table)-> tablas_todos


rbind(data.frame(group=vector_a,
                 mean=vector_b,
                 tablas_todos$education_level,
                 var="educacion"),
      data.frame(group=vector_c,
                 mean=vector_d,
                 tablas_todos$gender,
                 var="gender"),
      data.frame(group=vector_e,
                 mean=vector_f,
                 tablas_todos$religion,
                 var="religion"),
      data.frame(group=vector_g,
                 mean=vector_h,
                 tablas_todos$rel_status,
                 var="rel_status"),
      data.frame(group=vector_i,
                 mean=vector_j,
                 tablas_todos$substance_use,
                 var="substance_use"),
      data.frame(group=vector_k,
                 mean=vector_l,
                 tablas_todos$time_cesfam,
                 var="time_cesfam"))-> internalizado_mean_freq

internalizado_mean_freq %>% writexl::write_xlsx("internalizado promedios frecuencias.xlsx")
       
       
#######medias por item prom_stigm######

data %>% 
  summarise(mean(prom_stigm,na.rm=T),
            median(prom_stigm,na.rm=T))

data %>% 
  select(starts_with("a_")) %>% 
  colMeans(.,na.rm=T) %>% 
  data.frame() ->medias_items_percibido
#relevant aspects  - means go from 1 to 5, median of scale is 3, ítems above median show a moderate perceived provider stigma       
#a_13 - 3.223108 Usuarios consideran que son percibidos como peligrosos.
#a_09 - 3.344622 Usuarios consideran que usuarios de sustancias no serían considerados para trabajar en el servicio
#a_06 - 3.464143 Usuarios consideran que son percibidos como personas que no podrían cuidar a menores, independiente de tratamiento
#a_04 - 3.218688 Usuarios consideran que son percibidos como personas que no podrían ser profesor de niños en escuela pública
#a_03 - 3.037773 Usuarios consideran que son percibidos como personas no confiables
              
data %>% 
  select(starts_with("b_")) %>% 
  colMeans(.,na.rm=T) %>%
  data.frame() -> medias_items_internalizado
       
#relevant aspects  - means go from 1 to 5, median of scale is 3, ítems above median show a moderate perceived provider stigma.
#all the items have a mean under the 
#b_01 2.025948
#b_02 2.088000
#b_03 2.084000
#b_04 2.064000
#b_05 2.054000
       
########we are also curious about the frequency per ítem on perceived provider stigma and discrimination########
parte_a %>% 
  pivot_longer(!suma_GHQ,names_to = "vars",values_to = "count") %>% 
  select(-suma_GHQ) %>% 
  group_by(vars) %>% 
  count(count) %>% 
  #na.omit() %>% 
  pivot_wider(names_from = "count",values_from = "n") %>% 
  mutate(validos = 507-`NA`) -> respuestas_totales_percibido

### you can also visualize how it looks per ítem ###
parte_a %>% 
  pivot_longer(!suma_GHQ,names_to = "vars",values_to = "count") %>% 
  select(-suma_GHQ) %>% 
  group_by(vars) %>% 
  count(count) %>% 
  na.omit() %>% 
  ggplot()+
  geom_line(aes(x=count,y=n))+
  facet_wrap(~vars)
  
parte_b %>% 
  pivot_longer(!suma_GHQ,names_to = "vars",values_to = "count") %>% 
  select(-suma_GHQ) %>% 
  group_by(vars) %>% 
  count(count) %>% 
  #na.omit() %>% 
  pivot_wider(names_from = "count",values_from = "n") %>% 
  mutate(validos = 507-`NA`) -> respuestas_totales_internalizado;respuestas_totales_internalizado

cbind(data.frame(respuestas_totales_percibido),data.frame(medias_items_percibido)) %>% 
  writexl::write_xlsx("promedios_estigmapercibido.xlsx")

cbind(data.frame(respuestas_totales_internalizado),data.frame(medias_items_internalizado)) %>% 
  writexl::write_xlsx("promedios_estigmainternalizado.xlsx")

 #finally, we have to create the base for the structural model equation in MPLUS.
 
library(fastDummies)
data %>% 
  select(
         -rel_status,
         -time_cesfam,
         -education_level,
         -suma_stigm, 
         -prom_stigm,
         -suma_disc,
         -prom_disc, 
         -suma_ghq) %>% 
  dummy_cols(select_columns = c("rel_status4",
                                "gender",
                                "religion2",
                                "time_cesfam5",
                                "substance_abuse2",
                                "education_level3"),
             remove_first_dummy = TRUE,
             ignore_na = TRUE, 
             remove_selected_columns = TRUE) %>% 
  writexl::write_xlsx("base_sem.xlsx")
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
       
