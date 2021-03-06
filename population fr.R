library(readxl)
library(tidyverse)
library(tsibble)
library(feasts)
library(fable)

# Population structure
popStruct<-read_xlsx("sources/fe_dod_struct_pop.xlsx", skip=3)[,1:8]
names(popStruct)<-c("Année", "...2", "15-19","0-14","20-59","60-64","65-75","75+")
popStruct[is.na(popStruct$...2),"...2"]<-""
popStruct<-popStruct[1:32,] %>%
  filter(...2 != "hors Mayotte")%>%
  mutate(`65-75`=`65-75`-`75+`,
         `15-19`=`15-19`-`0-14`)

popTs<-popStruct[,1:8]%>%
  mutate(Année=as.integer(Année))%>%
  as_tsibble(index=Année)%>%
  pivot_longer(cols=-c(1,2),names_to="Ages", values_to="Qty", values_transform=list(integer))

# Population review
popCroiss<-read_xlsx("sources/fe_dod_compo_crois.xlsx",skip = 2)
popCroiss[is.na(popCroiss$...2),"...2"]<-""
popCroiss<-popCroiss[1:41,] %>%
  filter(...2 !="hors Mayotte")

pop<-popCroiss %>%
  inner_join(popStruct,by = "Année") %>%
  slice(1:30) %>%
  mutate(Année=as.integer(Année),
         mortalité_1M_hab=as.numeric(`Nombre de décès`)/as.numeric(`Population au 1er janvier`)*1000000,
         ) %>%
  as_tsibble(index="Année")

# pop %>% str()
pop[,10:15]<-mapply(FUN = '/', pop[,10:15], pop[,3])

# plot death rate
p1<-pop%>%
  ggplot()+
  geom_line(aes(x=Année, y=`mortalité_1M_hab`))
# plot population
p2<-pop %>%
  ggplot(aes(x=Année,y=`Population au 1er janvier`))+
  geom_line()
#plot population per age bin
p3<-pop%>% 
  pivot_longer(cols=`15-19`:`75+`, names_to="Ages", values_to="Age_rate")%>%
  mutate(Ages=Ages, label=if_else(Année==max(Année),Ages,NA_character_))%>%
  ggplot(aes(x=Année,y=`Age_rate`, color=Ages))+
  geom_line()+
  theme(legend.position = "none")+
  scale_y_continuous(trans="log10")+
  ggrepel::geom_label_repel(aes(label = label), nudge_x = 1, na.rm = TRUE)

library(patchwork)
p1 / p2 / p3

# time serie decomposition
pop %>% features(mortalité_1M_hab,feat_stl)

# ACL features
pop %>% features(mortalité_1M_hab,feat_acf)

fit_out<-pop %>% model(ETS(mortalité_1M_hab))
report(fit_out)
augment(fit_out)

# MA smoothing
pop %>%
  filter(Année>2005) %>%
  mutate(lag=slider::slide_dbl(mortalité_1M_hab, .f = mean, .before=3)) %>%
  select(lag, mortalité_1M_hab)%>%
  ggplot()+
  geom_line(aes(x=Année,y=lag), color="red")+
  geom_line(aes(x=Année,y=mortalité_1M_hab), color="blue")
  
# ACF plot
pop %>% ACF(`mortalité_1M_hab`) %>%autoplot()
# death rate seems to be correlated with 1Y, 2Y, 3Y lagged data 

# forecasting
pop_train<-pop %>%
  filter(Année<2018 & Année>=2004)
  # slice(1:(n()-5))
pop_test<-pop %>%
  filter(Année>2017)

fit<-pop_train %>%
  model(ets=ETS(mortalité_1M_hab ~ trend("A")+error("A")),
        ets_auto=ETS(mortalité_1M_hab),
        arima=ARIMA(`mortalité_1M_hab` ~ 1 + pdq(0,0,3)))

# residuals review
fit %>% augment()%>% autoplot(.resid) 
fit %>% augment()%>%
  ggplot(aes(x=.innov))+
  geom_histogram()+
  facet_wrap(~.model,nrow=3)

# model accuracy
accuracy(fit)

# forecast accuracy
f<-fit %>% forecast(h=3) 
f %>% accuracy(pop_test)  

# plot forecast
f %>% autoplot(pop)
f %>% filter(Année==2020)
pop_test %>% filter(Année==2020) %>% select(mortalité_1M_hab)

70000/67
