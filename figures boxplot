#this code must be run after running "main data.R"

#perceived stigma boxplot
data %>% 
  ggplot(aes(x=prom_stigm))+
  geom_boxplot()+
  labs(x="Estigma Percibido")+
  theme_bw()+
  scale_x_continuous(labels=c(1:5),
                     breaks=c(1:5),
                     limits = c(1,5))
#discrimination
data %>% 
  ggplot(aes(x=suma_disc))+
  geom_boxplot()+
  labs(x="Discriminación (Estigma internalizado)")+
  theme_bw()+
  scale_x_continuous(labels=c(5,10,15,20,25),
                     breaks=c(5,10,15,20,25),
                     limits = c(5,25))
#ghq-12
data %>% 
  ggplot()+
  geom_boxplot(aes(x=suma_GHQ))+
  labs(x="Estigma Internalizado")+
  theme_bw()+
  scale_x_continuous(labels=c(10,20,30,40,50),
                     breaks=c(10,20,30,40,50),
                     limits = c(10,50))
