## Purpose: import and 'clean' the baiting data from Cassidy Kempf (fieldwork conducted in Sept/Oct 2016)

## load packages
library(tidyverse)
library(xlsx)

## read in data from the original file, located in MillerLab dropbox (may need to update this directory on different computers)
dat_raw<-read.xlsx("C:\\Users\\tm9\\Dropbox\\Lab Documents\\Documents\\Ants\\Big Thicket Ants\\Cassidy Kempf\\Data\\Kempf_BTNP_ants_combined.xlsx",
                   sheetName = "baiting data")

## Data cleanup:
## replace NAs with zero.
## replace the weird 10* observations with just NA (I think this is the best we can do) 
dat_raw <- dat_raw %>% 
  mutate(H.Pheidole.dentata. = ifelse(H.Pheidole.dentata.=="10*",10,H.Pheidole.dentata.)) %>% 
  replace(is.na(.), 0)

## Now the harder part: split the honey and 
