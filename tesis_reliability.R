library(ltm)
library(psych)
require(lavaan)
require(semPlot)
require(GPArotation)
require(psych)
require(knitr)
library(QuantPsyc)
library(MVN)
library(dplyr)

#correlation between items
Hmisc::rcorr(as.matrix(cronbach_stigma_reverso), type="spearman")

#Cronbach alfa
data %>% 
  dplyr::select(starts_with("a_")) -> cronbach_stigma_reverso
  
  alpha(cronbach_stigma_reverso %>% na.omit())->alpha_stigma_reverse
  
  
#correlation items-scale

cronbach_stigma_reverso %>% 
  mutate(prom = rowSums(cronbach_stigma)/16) ->cronbach_stigma_corr
  
rcorr(as.matrix(cronbach_stigma_corr),type = "spearman") -> cronbach_stigma_reverso_corr

cronbach_stigma_reverso_corr$r
alpha_stigma_reverse$item.stats[5]
alpha_stigma_reverse$alpha.drop[1]

#mean item dropped

algo <- c(paste0("a_0",1:9))
algo2 <- c(paste0("a_",10:16))
algo3<-c(algo,algo2)
list()-> mean.drop

for (i in seq(algo3)) {
  mate <- algo3[-i]
  data %>% 
    select(mate) %>%
    mutate(sum_stigm = rowSums(.[1:15]),
           mean_stigm = sum_stigm/15) %>% 
    summarise(por.item = mean(mean_stigm,na.rm=T)) -> datos.mean.stigm
  print(datos.mean.stigm)->  mean.drop[i]
}

data.frame(id=algo3,mean=unlist(mean.drop))-> mean.without.item

data.frame(id = mean.without.item$id,
           corr_item_scale = correlacion_reverso_con_stigma$r[,17][1:16],
           raw_alfa= alpha_stigma_reverse$alpha.drop$raw_alpha,
           std_alfa = alpha_stigma_reverse$alpha.drop$std.alpha,
           item_drop = alpha_stigma_reverse$item.stats$r.drop,
           mean_item_drop= mean.without.item$mean,
           total_alfa = alpha_stigma_reverse$total$raw_alpha) %>% 
  writexl::write_xlsx("confiabilidad_basetotal_reverso.xlsx")
  
  


  
 #KMO above > 0.6 is ok for CFA.
 KMO(cronbach_stigma_reverso)

#Kaiser-Meyer-Olkin factor adequacy
#Call: KMO(r = cronbach_stigma_reverso)
#Overall MSA =  0.94
#MSA for each item = 
#a_01 a_02 a_03 a_04 a_05 a_06 a_07 a_08 a_09 a_10 a_11 a_12 
#0.94 0.94 0.94 0.94 0.95 0.92 0.94 0.95 0.94 0.94 0.94 0.94 
#a_13 a_14 a_15 a_16 
#0.94 0.93 0.92 0.94

cortest.bartlett(cronbach_stigma_reverso)

#R was not square, finding R from data
#$chisq
#[1] 2940.448
#$p.value
#[1] 0
#$df
#[1] 120
 
#χ2 is high (p < 0,05)
#reject null hypothesis of identity matrix

#plot(scree(cronbach_stigma_reverso))
#elbow at 1 for PC and FA


fa(cronbach_stigma_reverso, # datos
   nfactors = 1, # n factores
   fm = "wls", # método estimación
   rotate = "geominT")$Vaccounted
  
   
#                   WLS1
#SS loadings    5.874224
#Proportion Var 0.367139

#pretty bad h2 (% de la varianza común entre las variables), 
#los factores extraídos explican el XX% de a_01 (h2[a_01])

#cargas factoriales (WLS1) sobre 0.4
#     WLS1   h2   u2 com
#a_01 0.59 0.35 0.65   1
#a_02 0.61 0.37 0.63   1
#a_03 0.62 0.38 0.62   1
#a_04 0.55 0.31 0.69   1
#a_05 0.52 0.27 0.73   1
#a_06 0.45 0.20 0.80   1
#a_07 0.65 0.42 0.58   1
#a_08 0.57 0.33 0.67   1
#a_09 0.55 0.30 0.70   1
#a_10 0.64 0.40 0.60   1
#a_11 0.51 0.26 0.74   1
#a_12 0.75 0.56 0.44   1
#a_13 0.68 0.46 0.54   1
#a_14 0.41 0.17 0.83   1
#a_15 0.77 0.59 0.41   1
#a_16 0.70 0.49 0.51   1


 
