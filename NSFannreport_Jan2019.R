library(tidyverse)
library(xlsx)
BTNP_dat <- read.xlsx("C:\\Users\\tm9\\Dropbox\\Lab Documents\\Documents\\Ants\\Big Thicket Ants\\Michael Saucedo\\big_thicket_ants20142018.xlsx",sheetName="Pitfall Data")

BNTP <- BTNP_dat %>% filter(Month == 5 | Month == 9 | (Month == 11 & Year == 2017)) %>% 
  mutate(gen.sp = interaction(Genus,Species),
         Season = ifelse(Month == 5,"Spring","Fall")) 

sp_richness <- BNTP %>% 
  group_by(Year,Season,Site)%>% 
  summarise(n_spp=n_distinct(na.omit(gen.sp)))

total_ants <- BNTP %>% 
  group_by(Year,Season,Site) %>% 
  summarise(total_ants = sum(Abundance,na.rm=T))

invasive_ants <- BNTP %>% 
  filter(gen.sp=="Solenopsis.invicta" | gen.sp=="Nylanderia.fulva") %>% 
  group_by(Year,Season,Site) %>% 
  summarise(total_invasives = sum(Abundance,na.rm=T))

fraction_invasive <- full_join(total_ants,invasive_ants,by=c("Year","Season","Site")) %>% 
  mutate(total_invasives=ifelse(is.na(total_invasives),0,total_invasives),
         fraction_invasive = total_invasives / total_ants) 

ggplot(fraction_invasive)+
  geom_boxplot(aes(x=as.factor(Year),y=fraction_invasive,fill=fct_relevel(Season,"Spring")))+
  xlab("Year")+ylab("Proportion invasive")+labs(fill="Season")+
  theme_bw()

ggplot(fraction_invasive)+
  geom_point(aes(x=as.factor(Year),y=fraction_invasive,color=fct_relevel(Season,"Spring")))+
  xlab("Year")+ylab("Species richness")+labs(color="Season")+
  facet_wrap(~Site)+
  theme_bw()

ggplot(fraction_invasive)+
  geom_boxplot(aes(x=as.factor(Year),y=total_ants,fill=Season))

ggplot(sp_richness)+
  geom_boxplot(aes(x=as.factor(Year),y=n_spp,fill=fct_relevel(Season,"Spring")))+
  xlab("Year")+ylab("Species richness")+labs(fill="Season")+
  theme_bw()

## categorize site-years as having each of S.i. and N.f.

fraction_sites_invaded <- BNTP %>% 
  group_by(Year,Season,Site) %>% 
  summarise(fire_ant_traps = sum(na.omit(gen.sp)=="Solenopsis.invicta"),
            crazy_ant_traps = sum(na.omit(gen.sp)=="Nylanderia.fulva")) %>% 
  mutate(fire_ant_present = fire_ant_traps > 0,
         crazy_ant_present = crazy_ant_traps > 0) %>% 
  group_by(Year,Season) %>% 
  summarise(Fire_ant = mean(fire_ant_present),
            Crazy_ant = mean(crazy_ant_present)) %>% 
  gather(Fire_ant:Crazy_ant,key="Species",value="Fraction") 

win.graph()
ggplot(fraction_sites_invaded)+
  geom_col(aes(x=as.factor(Year),y=Fraction,fill=fct_relevel(Season,"Spring")),position="dodge")+
  facet_grid(~fct_relevel(Species,"Fire_ant"))+
  xlab("Year")+ylab("Fraction of sites invaded")+
  labs(fill="Season")+
  theme_bw()

