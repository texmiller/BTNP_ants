## Purpose: import,  'clean', and visualize the baiting data from Cassidy Kempf (fieldwork conducted in Sept/Oct 2016)

## load packages
library(tidyverse)

## read in data from the original file, located in MillerLab dropbox (may need to update this directory on different computers)
bait<-read.csv("kempf_baiting_data.csv")
str(bait)

## there is a weird entry of 10* for one species. replace this with just 10
bait$H.Pheidole.dentata[which(bait$H.Pheidole.dentata=="10*")] <- "10"
## convert factor to integer
bait$H.Pheidole.dentata <- (as.integer(levels(bait$H.Pheidole.dentata))[bait$H.Pheidole.dentata])

## There is one species that has no tuna. I am going to create it, which will make the next steps easier
bait$T.Unidentified.2 <- NA
## another column is mis-spelled
bait$H.Unidentified.1 <- bait$H.Undientified.1

##convert wide to long
honey <- bait[,c("Day","Site","Station","Round",
        "H.Pheidole.dentata","H.Solenopsis.invicta",
        "H.Tapinoma.sessile","H.Nylanderia.fulva","H.Pheidole.moerens",
        "H.Camponotus.pennsylvanicus","H.Crematogaster.sp","H.Unidentified.1",
        "H.Aphaenogaster.treatae","H.Unidentified.2","H.Cyphomyrmex.rimosus")]
honey$bait<-"honey"
names(honey)[5:15] <- str_sub(names(honey)[5:15],3)

tuna <- bait[,c("Day","Site","Station","Round",
                "T.Pheidole.dentata","T.Solenopsis.invicta",
                 "T.Tapinoma.sessile","T.Nylanderia.fulva","T.Pheidole.moerens",
                 "T.Camponotus.pennsylvanicus","T.Crematogaster.sp","T.Unidentified.1",
                 "T.Aphaenogaster.treatae","T.Unidentified.2","T.Cyphomyrmex.rimosus")]
tuna$bait<-"tuna"
names(tuna)[5:15] <- str_sub(names(tuna)[5:15],3)

##these should be the same dim
dim(honey);dim(tuna)

## read in and merge site data
sites <- read.csv("kempf_site_data.csv") %>% 
  mutate(site=interaction(Day,Site))

## now bind, replace NA with zero, make site unique, and convert "round" to minutes
bait_long <- left_join(bind_rows(honey,tuna)%>% 
  replace(is.na(.), 0) %>% 
  mutate(site=interaction(Day,Site),
         minute=Round*20),sites,by="site") %>% 
  mutate(other=Pheidole.dentata+Tapinoma.sessile+Pheidole.moerens+
           Camponotus.pennsylvanicus+Crematogaster.sp+Unidentified.1+
           Aphaenogaster.treatae+Unidentified.2+Cyphomyrmex.rimosus)

## aggregate time points by station and get fraction of stations visited by 2 invaders
bait_long %>% 
  group_by(site,Station,Nfulva) %>% 
  summarise(Si=sum(Solenopsis.invicta),
            Nf=sum(Nylanderia.fulva),
            other=sum(other)) %>% 
  mutate(Si_present=Si>0,
         Nf_present=Nf>0,
         other_present=other>0) %>% 
  group_by(Nfulva) %>% 
  summarise(Si=mean(Si_present),
            Nf=mean(Nf_present),
            other=mean(other_present)) %>% 
  gather(key="sp",value="fraction",Si,Nf,other) %>% 
  ggplot(aes(x=Nfulva,y=fraction,fill=sp))+
  geom_bar(stat="identity",position=position_dodge())+
  labs(y="Fraction of bait stations",
       x="Nylanderia status")+
  theme_bw()

## time to recruitment
bait_long %>% 
  select(site,Station,bait,Solenopsis.invicta,Nylanderia.fulva,other,minute) %>% 
  group_by(as.factor(minute),bait) %>% 
  summarise(Nylanderia.fulva = mean(Nylanderia.fulva),
            Solenopsis.invicta = mean(Solenopsis.invicta),
            other = mean(other)) %>% 
  gather(key="sp",value="Mean",Solenopsis.invicta,Nylanderia.fulva,other) %>% 
  mutate(sp = factor(sp,levels=c("Nylanderia.fulva","Solenopsis.invicta","other"))) %>% 
  ggplot(aes(x=`as.factor(minute)`,y=Mean,fill=bait))+
  geom_bar(stat="identity",position=position_dodge())+
  labs(y="Mean workers at bait",
       x="Minutes")+
  facet_grid(sp~.)+
  theme_bw()


bait_long %>% 
  select(site,Station,bait,Solenopsis.invicta,Nylanderia.fulva,other,minute) %>% 
  group_by(as.factor(minute),bait) %>% 
  gather(key="sp",value="workers",Solenopsis.invicta,Nylanderia.fulva,other) %>% 
  mutate(sp = factor(sp,levels=c("Nylanderia.fulva","Solenopsis.invicta","other"))) %>% 
  ggplot(aes(x=`as.factor(minute)`,y=workers,fill=bait))+
  geom_boxplot(position=position_dodge())+
  labs(y="Mean workers at bait",
       x="Minutes")+
  facet_grid(sp~.)
