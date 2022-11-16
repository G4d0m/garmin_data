library(tidyverse)
library(readxl)
library(readr)
library(hms)
library(ggplot2)
#library(magrittr)

#orginalny
garmin = read.csv("/Users/piotr/Downloads/Activities-4.csv", header = T)
#przerabiany - dzialajacy
garmin = read.csv("/Users/piotr/PlikiR/dane_garmin/Dane garmin/garmin2", header = T) 


garmin$Kalorie %<>% parse_number(garmin$Całkowity.wznios,
                                 locale = locale(grouping_mark = ","))

garmin$Całkowity.wznios %<>% parse_number(garmin$Typ.aktywności,
                                          locale = locale(grouping_mark = ","))

garmin$Data %<>% as.Date()
glimpse(garmin)
garmin$Data <- as.Date(garmin$Data)

# Podmiana danych i uzycie symbolu %<>% ponizej - wyszukaj w google lub link
# "How to change column data type of a tibble (with least typing)"
# https://stackoverflow.com/questions/43917904/
# how-to-change-column-data-type-of-a-tibble-with-least-typing

sum(is.na(garmin$Całkowity.wznios))


                  

garmin.tetno <- filter(garmin,Średnie.tętno>1)

garmin.road <-filter(garmin, Średnie.tętno>1,
                     !Typ.aktywności == "Jazda górska")

garmin.mountain <- filter(garmin, Średnie.tętno>1,
                          !Typ.aktywności == "Kolarstwo szosowe")

#wykres srednia predkosc srednie tetno
#dla dnaych maountain

garmin.mountain %>%
  group_by(Data) %>%
  summarise(
    Średnie.tętno,
    Średnia.prędkość
  ) %>%
  ggplot(mapping = aes(x = Średnie.tętno, y = Średnia.prędkość))+
  geom_point()+
  geom_smooth(se = F)

# *liniowy dla mountain

garmin.mountain %>%
  group_by(Data) %>%
  summarise(
    Średnie.tętno,
    Średnia.prędkość
  ) %>%
  ggplot(mapping = aes(x = Średnie.tętno, y = Średnia.prędkość))+
  geom_point()+
  geom_smooth(se = F, method = lm)


#wykres srednie tetno / predkosc szosa
garmin.road %>%
  group_by(Data) %>%
  summarise(
    Średnie.tętno,
    Średnia.prędkość
  ) %>%
  ggplot(mapping = aes(x = Średnie.tętno, y = Średnia.prędkość))+
  geom_point()+
  geom_smooth(se = F, method = lm)


#nieudany wykres dystans kalorie, ale ciekawe 2 skupiska - do zbadania
garmin %>%
  summarise(Dystans,Kalorie) %>%
  ggplot(aes(Dystans,Kalorie))+
  geom_point()+
  geom_smooth(se=F)


garmin.tetno  %>% 
  group_by(Data) %>%
  summarise(
    Średnie.tętno,
    Średnia.prędkość) %>% 
  ggplot(aes(x = Średnie.tętno, y = Średnia.prędkość)) +
  geom_point()+
  geom_smooth(se=F, method = lm)

garmin %>%
  group_by(Typ.aktywności) %>% 
  summarise(Dystans, Kalorie)

garmin.Typ.akt <- group_by(garmin, Typ.aktywności)

  
# chce wykres - jazda gorska vs jazda szoswa - calkowity dystans, a potem wykres

ggplot(garmin) + 
    geom_bar(mapping = aes(x = Typ.aktywności))

garmin$Data <- as.Date(garmin$Data)

garmin %>%
  group_by(Data)

#te operacje robia bledy
garmin$Dystans <-as.numeric(garmin$Dystans)
garmin$Kalorie <-as.numeric(garmin$Kalorie)
garmin$Kalorie <-as.numeric(garmin$Kalorie)

glimpse(garmin)


#dzialajacy dystans kalorie
garmin %>%
    ggplot(aes(Dystans,Kalorie))+
    geom_point() +
    geom_smooth(se=F)

ggplot(garmin) + 
  geom_point(mapping = aes(x = Dystans, y = Kalorie, color = Typ.aktywności))+
  geom_smooth(mapping = aes(x = Dystans, y = Kalorie), se = F)


group_by(garmin,Typ.aktywności) %>%
  summarise(sr.tetno= mean(Średnie.tętno, na.rm = TRUE) #trzeba usunac zera
            ,
            sr.wznios = mean(Całkowity.wznios, na.rm = TRUE),
            sr.predokosc = mean(Średnia.prędkość, na.rm = TRUE),
            max.pr = max(Maksymalna.prędkość),
            suma.km = sum(Dystans),
            suma.kcal = sum(Kalorie, na.rm = T),
            
  )

is.numeric(garmin$Kalorie)

### parsing calkowity wnios
#garmin <- garmin %>% filter(Całkowity.wznios>1)
#garmin1 <- parse_number(garmin$Całkowity.wznios, locale = locale(grouping_mark = ","))
#garmin <- garmin %>% 
 # mutate(Całkowity.wznios2 = garmin1)
#garmin <-select(garmin, Typ.aktywności:Maksymalna.wysokość, Całkowity.wznios2)


# Jakie bylo srednie tetno w poszczegolnych miesiacach

group_by(garmin,Data) %>%
  summarise(sr.tetno= mean(Średnie.tętno, na.rm = TRUE),
  )


filter(FB, date >= as.Date("2013-01-01"), date <= as.Date("2013-12-31"))



# Regresja liniowa dystans kalorie

Dyst_kcal <- lm(formula = Kalorie ~ Dystans, data = garmin_out)
summary(Dyst_kcal)

glimpse(garmin)

library(corrplot)
library(olsrr)

cook <- ols_plot_cooksd_bar(Dyst_kcal)

outliers <- cook$data$obs[cook$data$color == "outlier"]

garmin_out <- garmin[-outliers,]


ggplot(garmin_out) + 
  geom_point(mapping = aes(x = Dystans, y = Kalorie, color = Typ.aktywności))+
  geom_smooth(mapping = aes(x = Dystans, y = Kalorie), se = F)
