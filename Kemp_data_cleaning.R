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
         minute=Round*20),sites,by="site")




