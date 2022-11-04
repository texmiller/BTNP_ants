### Title: Ant analysis
### Author: Marion Donald (pulling from scripts by Gabriela Zambrano and Meghan Hager)
### Date Started: 23 July 2019
### Purpose: Re-run analyses for species accumulation, richness, community composition, etc.
### Date Updated: 30 July 2019


library(tidyverse)
library(readxl)
library(dplyr)
library(tidyr)
library(vegan)
library(ggplot2)
library(BiodiversityR)
library(extrafont)
library(ggpubr)
library(broom)
library(gridExtra)
library(cowplot)
library(ggpubr)
library(wesanderson)


## read in data
data_MH <- read_excel("Meghan Hager Big Thicket Ant Data Updated 07-22-19 (1).xlsx")

data_GZ_all <- read_excel("big_thicket_ants20142018.xlsx", sheet = "Pitfall Data")

native_classification <- read.csv("ant_species_native_classification.csv") 



data_MH_2 <- data_MH %>% 
  filter(`Collection Method` == "Pit") %>% ## select just the pitfall trap data
  select("Aphenogaster carolinensis":"Trachymyrmex septentrionalis") ## select just the species names

## since MH data doesn't have as many species names as GZ data, we need to add these columns in, in order to merge the two dfs

## get list of species from GZ 
data_GZ_species <- data_GZ_all %>% 
                   filter(Genus != "NA") %>% 
                   unite("species", c("Genus", "Species"), remove = T, sep = " ") %>% 
  select(species) %>% 
  distinct(species) %>% 
  arrange(species) ## finding out that there are some data frame issues -- some of the species names are capitalized while others are not
## this is resulting in duplicates

data_GZ_clean <- data_GZ_all %>% 
  filter(Year == 2014 | ## pull out just the two years of Gabriela's data
           Year == 2015,
         Genus != "NA") %>% 
  mutate(Species = ifelse(Species == "Carolinensis", "carolinensis", ## correct the species names (mispellings and uppercase-to-lowercase)
                          ifelse(Species == "Depilis", "depilis", 
                          ifelse(Species == "Patagonicus", "patagonicus",
                          ifelse(Species == "Castaneus", "castaneus",
                          ifelse(Species == "pennsylanicus" | Species == "Pennsylvanicus" | Species == "pennsylvancius", "pennsylvanicus", 
                          ifelse(Species == "ashmeadii", "ashmeadi", 
                          ifelse(Species == "Rimosus", "rimosus",
                          ifelse(Species == "Opacior", "opacior",
                          ifelse(Species == "Coecus", "coecus", 
                          ifelse(Species == "Americana", "americana", 
                          ifelse(Species == "fasionensis", "faisonensis", 
                          ifelse(Species == "Fulva", "fulva",
                          ifelse(Species == "Harpax", "harpax",
                          ifelse(Species == "Dentata", "dentata",
                          ifelse(Species == "Dentigula", "dentigula",
                          ifelse(Species == "Metallescens", "metallescens",
                          ifelse(Species == "Invicta", "invicta", 
                          ifelse(Species == "Molesta", "molesta",
                          ifelse(Species == "Louisianae", "louisianae", Species))))))))))))))))))),
         Genus = ifelse(Genus == "Pachydondyla", "Pachycondyla", Genus))

data_GZ_clean_sp_check <- data_GZ_clean %>% 
  filter(Abundance != "NA") %>% 
  filter(Genus != "NA") %>% 
  unite("species", c("Genus", "Species"), remove = T, sep = " ") %>% 
  spread("species", "Abundance") %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  summarize_at(vars(`Aphaenogaster carolinensis`:`Trachymyrmex septentrionalis`), sum) %>% 
  gather(species, Abundance)
  
  
data_GZ_clean_sp <- data_GZ_clean_sp_check %>% 
  ungroup() %>% 
  select(species) %>% 
  distinct(species) %>% 
  arrange(species) 



## check species names in MH data **** THERE ARE SPELLING MISTAKES HERE
data_MH_clean <- data_MH %>% 
  rename("Aphaenogaster carolinensis" = 'Aphenogaster carolinensis',
         "Aphaenogaster texana" = "Aphenogaster texana",
         "Hypoponera opaciceps" = "Hyponera opaciceps",
         "Cyphomyrmex rimosus" = "Cyphomyrmex rimosous")

data_MH_sp <- data_MH_clean %>% 
  gather("Aphaenogaster carolinensis":"Trachymyrmex septentrionalis", key = "Species", value = "Abundance") %>% 
  select(Species) %>% 
  distinct(Species) %>% 
  arrange(Species) %>% 
  rename(species = Species)

full_sp_list <- full_join(data_GZ_clean_sp, data_MH_sp) ## this is the full species list across the two datasets (but including all data from 2014-2018)

#### data set up for species accumulation curve
##  transform to wide df for species accumulation curve 

data_GZ_wide <- data_GZ_clean %>% 
  filter(Abundance != "NA") %>% 
  select(Year, Site, Station, Month, Genus, Species, Abundance) %>% 
    unite("species", c("Genus", "Species"), remove = T, sep = " ") %>% 
  # group_by(species) %>% 
  #  mutate(grouped_id = row_number()) %>% 
  spread(key = "species", value = "Abundance") %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%  ## make all the NAs under the species names zeros (for this analysis) 
  select(-Site:-Month) ## drop the identifiers and just keep the species names and Year


## find the species that are in MH that we need to add to this df in order to merge them
missing_GZ <- anti_join(data_MH_sp, data_GZ_clean_sp) ## 5 species that are in MH that need to be added to GZ

data_GZ_wide2 <- data_GZ_wide %>% 
                mutate("Aphaenogaster texana" = 0,
                       "Brachymyrmex patagonicus" = 0,
                       "Nylanderia terricola" = 0,
                       "Pheidole moerens" = 0,
                       "Prenolepis imparis" = 0)

missing_MH <- anti_join(data_GZ_clean_sp, data_MH_sp) ## 14 spp that are in GZ that need to be added to MH

data_MH_2a <- data_MH_clean %>% 
  mutate("Aphaenogaster rudis-fulva-texana complex" = 0,
         "Aphaenogaster treatae" = 0,
         "Brachymyrmex depilis" = 0,
         "Crematogaster ashmeadi" = 0,
         "Crematogaster lineolata" = 0,
         "Crematogaster minutissima" = 0,
         "Crematogaster sp" = 0,
         "Hypoponera opacior" = 0,
         "Labidus coecus" = 0,
         "Pheidole flavens complex" = 0,
         "Pheidole sp" = 0, 
         "Pseudomyrmex pallidus" = 0, 
         "Solenopsis nickersoni" = 0, 
         "Tapinoma sessile" = 0) %>% 
  select("Aphaenogaster carolinensis":"Tapinoma sessile",
         -Site_Code, -`Inside or Outside (Inside=1)`) %>% 
  mutate(Year = 2015)

data_spacc_all <- data_MH_2a %>% 
  bind_rows(data_GZ_wide2) %>% 
  mutate(`Pheidole flavens complex` = `Pheidole flavens complex` + `Pheidole moerens`) %>% ## combining P. moerens since it can't be reliably identified from P. flavens complex
  select(-`Pheidole moerens`) ## drop P. moerens now that it's been combined with P. flavens


data_spacc_year <- data_spacc_all %>% 
                  select(Year)

data_spacc <- data_spacc_all %>% 
              select(-Year) # drop year, just keep the species by site matrix
curve <- specaccum(data_spacc, method = "exact")

pitfalls <- as.data.frame(curve$sites)
richness <- as.data.frame(curve$richness)
sd <- as.data.frame(curve$sd)

spacc_df <- pitfalls %>% 
  bind_cols(richness, sd) %>% 
  dplyr::rename(sites = `curve$sites`, 
         richness = `curve$richness`,
         sd = `curve$sd`)

ggplot(spacc_df, aes(sites, richness))+
  geom_point(alpha = .5)+
  geom_ribbon(aes(x = sites, ymin = richness-sd, ymax = richness+sd), alpha = 0.2)+
  theme_classic()+
  labs(x = "Number of pitfall traps",
       y = "Species richness")

## Chao estimated (36 species observed, ~42 +/- 6 species predicted by Chao, 42 predicted by jack1 +/- 2)
richness_est_df <- vegan::specpool(data_spacc)


data_spacc_df <- as.data.frame(data_spacc)


## calculate species rank abundance using the rankabundance package from BiodiversityR (only accepts dataframes not tibbles)
RankAbun.1 <- BiodiversityR::rankabundance(as.data.frame(data_spacc))

## conver to df for ggplot
rank_abu_df <- as.data.frame(RankAbun.1) %>% 
  rownames_to_column(var = "species") #%>% 

## native classification df to merge (native = 1, invasive = 0)
native_info <- native_classification %>% 
  rename(species = "Species") %>% 
  mutate(species = as.character(species))


rank_abu_df2 <- rank_abu_df  %>% 
  full_join(native_info) %>% 
  mutate(Native = as.character(Native))
         
## Pheidole flavens complex as Native for rank abundance
rank_abu_P.flav.native <- rank_abu_df2 %>% 
  mutate(Native = ifelse(species == "Pheidole flavens complex", 1, Native),
         Classification = ifelse(Native == 1, "native", "non-native")) 



## Rank abundance curve with Pheidole flavens complex as native (1)
rank_abu_fig <- ggplot(rank_abu_P.flav.native, aes(x=rank, y = abundance, label=species))+
  geom_point(aes(color = Classification, shape = Classification), size = 4)+
  geom_line()+
  theme_classic()+
  labs(x = "Species rank",
       y = "Abundance")+
  geom_text(aes(label=ifelse(abundance>100,as.character(species),'')),hjust=-0.08,vjust=0, check_overlap = F)+
  scale_color_manual(values = c("#055864","#04C0DD", "gray49"))+
  scale_shape_manual(values = c(16, 16))+
  theme(text = element_text(family = "Times New Roman", size = 14))

## Pheidole flavens complex as Native for rank abundance
rank_abu_P.flav.non.native <- rank_abu_df2 %>% 
  mutate(Native = ifelse(species == "Pheidole flavens complex", 0, Native),
         Classification = ifelse(Native == 1, "native", "non-native")) 



## Rank abundance curve with Pheidole flavens complex as non-native (NN) (0)
rank_abu_NN_fig <- ggplot(rank_abu_P.flav.non.native, aes(x=rank, y = abundance, label=species))+
  geom_point(aes(color = Classification, shape = Classification), size = 4)+
  geom_line()+
  theme_classic()+
  labs(x = "Species rank",
       y = "Abundance")+
  geom_text(aes(label=ifelse(abundance>100,as.character(species),'')),hjust=-0.08,vjust=0, check_overlap = F)+
  scale_color_manual(values = c("#055864","#04C0DD", "gray49"))+
  scale_shape_manual(values = c(16, 16))+
  theme(text = element_text(family = "Times New Roman", size = 14))


plot_grid(rank_abu_fig, rank_abu_NN_fig)

##

## diversity measures -- no difference in alpha diversity across the years
shannon_div <- diversity(data_spacc, "shannon")

shannon_div_df <- as.data.frame(shannon_div) %>% 
                  bind_cols(data_spacc_year) %>% 
  mutate(Year = as.factor(Year))

ggplot(shannon_div_df, aes(x = Year, y = shannon_div))+
  geom_boxplot()+
  geom_jitter(width = 0.05, height = 0)+
  theme_classic()+
  labs(x = "Sampling year",
       y = "Shannon diversity index")+
  stat_compare_means(method = "t.test")

## simpson div - no difference in alpha diversity across the years
simpson_div <- diversity(data_spacc, "simpson")

simpson_div_df <- as.data.frame(simpson_div) %>% 
  bind_cols(data_spacc_year) %>% 
  mutate(Year = as.factor(Year))

ggplot(simpson_div_df, aes(x = Year, y = simpson_div))+
  geom_boxplot()+
  geom_jitter(width = 0.05, height = 0)+
  theme_classic()+
  labs(x = "Sampling year",
       y = "Simpson diversity index")+
  stat_compare_means(method = "t.test")

## get df with info on sample ID (from row number) and year to get proportion of non-native to native ants (and also do it as occurrence for sp)
data_spacc_all_tidy <- data_spacc_all %>% 
  rownames_to_column("ID") %>% 
  gather(key = "species", value = "abundance", c(-Year, -ID)) %>% 
  full_join(native_info)


## Pheidole flavens complex is classed as "2" -- convert to native (1) or invasive (0) and run the analyses for proportion of invasives
data_spacc_all_tidy_PF_native <- data_spacc_all_tidy %>% 
  mutate(Native = ifelse(species == "Pheidole flavens complex", 1, Native))

##### P. flavens as native -- proportion non-native analysis
data_spacc_all_tidy_abu <- data_spacc_all_tidy_PF_native %>% 
  group_by(ID, Year, Native) %>% 
  summarize(abundance = sum(abundance))

total_abu <- data_spacc_all_tidy_abu %>% 
  ungroup() %>% 
  group_by(ID, Year) %>% 
  summarize(total_abundance = sum(abundance))

non_native_abu <- data_spacc_all_tidy_abu %>% 
  filter(Native == 0) %>% 
  rename(non_native_abu = abundance) %>% 
  select(-Native)

non_native_propotion_df <- total_abu %>% 
  left_join(non_native_abu) %>% 
  mutate(non_native_abu_prop = non_native_abu/total_abundance) %>% 
  ungroup() %>% 
  mutate(ID = as.numeric(ID)) %>% 
  arrange(ID) %>% 
  bind_cols(shannon_div_df,
            simpson_div_df)

non_native_propotion_df_cat <- non_native_propotion_df %>% 
  filter(total_abundance != 0) %>% 
  mutate(category = ifelse(non_native_abu_prop < .33, "Low", 
                           ifelse(non_native_abu_prop > .66, "High", "Medium"))) %>% 
  left_join(sp_richness, by = "ID")

non_native_propotion_df_cat$category <- factor(non_native_propotion_df_cat$category, levels = c("Low", "Medium", "High"))

my_comparisons <- list(   c("High", "Medium"),  c("Low", "Medium"), c("Low", "High") )



non_native_propotion_df_cat_tidy <- non_native_propotion_df_cat %>% 
  select(ID, Year, non_native_abu_prop, shannon_div, simpson_div, total_sp, category) %>% 
  gather(diversity_type, diversity_measure, -c(ID, Year, non_native_abu_prop, category)) %>% 
  mutate(diversity_type = case_when(diversity_type == "shannon_div" ~ "Shannon diversity index",
                                    diversity_type == "simpson_div" ~ "Simpson diversity index",
                                    diversity_type == "total_sp" ~ "Species richness"))

abundance_prop_alpha_fig <- ggplot(non_native_propotion_df_cat_tidy, aes(category, diversity_measure, fill = category))+
  geom_boxplot(position =position_dodge())+
  geom_point(position = position_jitterdodge(0.25), alpha = .75)+
  stat_compare_means(comparisons = my_comparisons, aes(label = ..p.signif..))+
  scale_fill_manual(values = wes_palette("GrandBudapest1"))+
  scale_color_manual(values = wes_palette("GrandBudapest1"))+
  labs(y = "Alpha diversity",
       x = "Proportion of non-native species (by abundance P.flavens as native)")+
  theme_classic()+
  theme(text = element_text(family = "Times New Roman", size = 14),
        legend.position = "bottom",
        legend.title = element_blank())+
  facet_grid(diversity_type~., scales="free_y")

#  linear model
lm_fit <- lm(shannon_div ~ non_native_abu_prop, data=non_native_propotion_df)
summary(lm_fit)
plot(lm_fit) ## residuals are not "random" -- they form an inverted U, linear model is not a great fit for these data
tidy(lm_fit)

glance(lm_fit)

## looks like there's a hump-shaped relationship between alpha diversity and proportion of non-native ants,
## not sure that the linear model is the best fit for these data
ggplot(non_native_propotion_df, aes(non_native_abu_prop, shannon_div))+
  geom_point()+
  #geom_line(data = broom::augment(lm_fit), aes(x = non_native_abu_prop, y = .fitted))+
  #facet_grid(~Year)+
  theme_classic()+
    labs(x = "Proportion of non-native ant abundance",
         y = "Shannon diversity index")

### for simpson div
#  linear model
lm_fit_simp<- lm(simpson_div ~ non_native_abu_prop, data=non_native_propotion_df)
#summary(lm_fit_simp)

#plot(lm_fit_simp)
#tidy(lm_fit_simp)

#glance(lm_fit_simp)

## looks like there's a hump-shaped relationship between alpha diversity and proportion of non-native ants,
## not sure that the linear model is the best fit for these data
ggplot(non_native_propotion_df, aes(non_native_abu_prop, simpson_div))+
  geom_point()+
  geom_line(data = broom::augment(lm_fit_simp), aes(x = non_native_abu_prop, y = .fitted))+
  #facet_grid(~Year)+
  theme_classic()+
  labs(x = "Proportion of non-native ant abundance",
       y = "Shannon diversity index")

### do the same as above but this time by species occurrence (presence/absence) rather than abundance (Pheidole flavens complex as native)
data_spacc_all_tidy_occ <- data_spacc_all_tidy_PF_native %>% 
  filter(abundance != 0) %>%  # sp has to be present to contribute
 filter(Native == 1) %>% 
  group_by(ID, Year) %>% 
  summarize(num_native_sp = n())

data_spacc_all_tidy_non_native <- data_spacc_all_tidy %>% 
  filter(abundance != 0) %>%  # sp has to be present to contribute
  filter(Native == 0) %>% 
  group_by(ID, Year) %>% 
  summarize(num_nonnative_sp = n())

occurrence_proportion_df <- data_spacc_all_tidy_occ %>% 
  full_join(data_spacc_all_tidy_non_native) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  mutate(total_sp = num_native_sp + num_nonnative_sp,
         prop_nonnative_sp = num_nonnative_sp/total_sp) %>% 
  ungroup() %>% 
  mutate(ID = as.numeric(ID)) %>% 
  arrange(ID) %>% 
  bind_cols(shannon_div_df, 
            simpson_div_df)


### for shannon div
#  linear model
lm_fit_shan_occ<- lm(shannon_div ~ prop_nonnative_sp, data=occurrence_proportion_df)
#summary(lm_fit_shan_occ)

#plot(lm_fit_shan_occ)
#tidy(lm_fit_shan_occ)

#glance(lm_fit_shan_occ)

## residuals plot looks more "random" -- linear model is more appropriate for these occurrence data than the abundance proportion
ggplot(occurrence_proportion_df, aes(prop_nonnative_sp, shannon_div))+
  geom_jitter()+
  geom_line(data = broom::augment(lm_fit_shan_occ), aes(x = prop_nonnative_sp, y = .fitted))+
  #facet_grid(~Year)+
  theme_classic()+
  labs(x = "Proportion of non-native ant species",
       y = "Shannon diversity index")

### for simpson div
#  linear model
lm_fit_simp_occ<- lm(simpson_div ~ prop_nonnative_sp, data=occurrence_proportion_df)
summary(lm_fit_simp_occ)

#plot(lm_fit_simp_occ)
#tidy(lm_fit_simp_occ)

#glance(lm_fit_simp_occ)


## residuals plot looks more "random" -- linear model is more appropriate for these data than the abundance proportion
ggplot(occurrence_proportion_df, aes(prop_nonnative_sp, simpson_div))+
  geom_jitter()+
  geom_smooth()+
  geom_line(data = broom::augment(lm_fit_simp_occ), aes(x = prop_nonnative_sp, y = .fitted))+
  #facet_grid(~Year)+
  theme_classic()+
  labs(x = "Proportion of non-native ant species",
       y = "Simpson diversity index")

occurrence_proportion_df_cat <- occurrence_proportion_df %>% 
  filter(total_sp != 0) %>% 
  mutate(category = ifelse(prop_nonnative_sp < .33, "Low", 
                           ifelse(prop_nonnative_sp > .66, "High", "Medium")))

occurrence_proportion_df_cat$category <- factor(occurrence_proportion_df_cat$category, levels = c("Low", "Medium", "High"))

my_comparisons <- list(   c("High", "Medium"),  c("Low", "Medium"), c("Low", "High") )

## get species richness (total # sp per trap) to use as the species richness diversity measure by abundance later
sp_richness<- occurrence_proportion_df_cat %>% 
  select(total_sp, ID)

occurrence_proportion_df_cat_tidy <- occurrence_proportion_df_cat %>% 
  gather(diversity_type, diversity_measure, -c(ID, Year, num_native_sp, num_nonnative_sp,
                                               prop_nonnative_sp, Year1, Year2, category)) %>% 
  mutate(diversity_type = case_when(diversity_type == "shannon_div" ~ "Shannon diversity index",
                                   diversity_type == "simpson_div" ~ "Simpson diversity index",
                                   diversity_type == "total_sp" ~ "Species richness"))

occurrence_prop_sp_fig <- ggplot(occurrence_proportion_df_cat_tidy, aes(category, diversity_measure, fill = category))+
  geom_boxplot(position =position_dodge())+
  geom_point(position = position_jitterdodge(0.25), alpha = .75)+
  stat_compare_means(comparisons = my_comparisons, aes(label = ..p.signif..))+
  scale_fill_manual(values = wes_palette("GrandBudapest1"))+
  scale_color_manual(values = wes_palette("GrandBudapest1"))+
  labs(y = "Alpha diversity",
       x = "Proportion of non-native species (by occurrence with P.flavens as native)")+
  theme_classic()+
  theme(text = element_text(family = "Times New Roman", size = 14),
        legend.position = "bottom",
        legend.title = element_blank())+
  facet_grid(diversity_type~., scales="free_y")



##### RUN THE ABOVE WITH P. flavens as non-native
data_spacc_all_tidy_PF_non_native <- data_spacc_all_tidy %>% 
  mutate(Native = ifelse(species == "Pheidole flavens complex", 0, Native))


data_spacc_all_tidy_abu_NN <- data_spacc_all_tidy_PF_non_native %>% 
  group_by(ID, Year, Native) %>% 
  summarize(abundance = sum(abundance))

total_abu_NN <- data_spacc_all_tidy_abu_NN %>% 
  ungroup() %>% 
  group_by(ID, Year) %>% 
  summarize(total_abundance = sum(abundance))

non_native_abu_NN <- data_spacc_all_tidy_abu_NN %>% 
  filter(Native == 0) %>% 
  rename(non_native_abu = abundance) %>% 
  select(-Native)

non_native_propotion_df_NN <- total_abu_NN %>% 
  left_join(non_native_abu) %>% 
  mutate(non_native_abu_prop = non_native_abu/total_abundance) %>% 
  ungroup() %>% 
  mutate(ID = as.numeric(ID)) %>% 
  arrange(ID) %>% 
  bind_cols(shannon_div_df,
            simpson_div_df)


non_native_propotion_df_NN_cat <- non_native_propotion_df_NN %>% 
  filter(total_abundance != 0) %>% 
  mutate(category = ifelse(non_native_abu_prop < .33, "Low", 
                           ifelse(non_native_abu_prop > .66, "High", "Medium"))) %>% 
  left_join(sp_richness, by = "ID")

non_native_propotion_df_NN_cat$category <- factor(non_native_propotion_df_NN_cat$category, levels = c("Low", "Medium", "High"))

my_comparisons <- list(   c("High", "Medium"),  c("Low", "Medium"), c("Low", "High") )



non_native_propotion_df_NN_cat_tidy <- non_native_propotion_df_NN_cat %>% 
  select(ID, Year, non_native_abu_prop, shannon_div, simpson_div, total_sp, category) %>% 
  gather(diversity_type, diversity_measure, -c(ID, Year, non_native_abu_prop, category)) %>% 
  mutate(diversity_type = case_when(diversity_type == "shannon_div" ~ "Shannon diversity index",
                                    diversity_type == "simpson_div" ~ "Simpson diversity index",
                                    diversity_type == "total_sp" ~ "Species richness"))

abundance_prop_alpha_fig_NN <- ggplot(non_native_propotion_df_NN_cat_tidy, aes(category, diversity_measure, fill = category))+
  geom_boxplot(position =position_dodge())+
  geom_point(position = position_jitterdodge(0.25), alpha = .75)+
  stat_compare_means(comparisons = my_comparisons, aes(label = ..p.signif..))+
  scale_fill_manual(values = wes_palette("GrandBudapest1"))+
  scale_color_manual(values = wes_palette("GrandBudapest1"))+
  labs(y = "Alpha diversity",
       x = "Proportion of non-native species (by abundance with P.flavens as non-native)")+
  theme_classic()+
  theme(text = element_text(family = "Times New Roman", size = 14),
        legend.position = "bottom",
        legend.title = element_blank())+
  facet_grid(diversity_type~., scales="free_y")


plot_grid(occurrence_prop_sp_fig, abundance_prop_alpha_fig)
#  linear model
lm_fit <- lm(shannon_div ~ non_native_abu_prop, data=non_native_propotion_df_NN)
summary(lm_fit)
#plot(lm_fit) ## residuals are not "random" -- they form an inverted U, linear model is not a great fit for these data
#tidy(lm_fit)

#glance(lm_fit)

## looks like there's a hump-shaped relationship between alpha diversity and proportion of non-native ants,
## not sure that the linear model is the best fit for these data
ggplot(non_native_propotion_df, aes(non_native_abu_prop, shannon_div), color = "red")+
  geom_jitter(data = non_native_propotion_df, color = "black")+
  geom_point(data= non_native_propotion_df_NN, aes(color = "red"))+
  geom_line(data = broom::augment(lm_fit), aes(x = non_native_abu_prop, y = .fitted))+
  #facet_grid(~Year)+
  theme_classic()+
  labs(x = "Proportion of non-native ant abundance",
       y = "Shannon diversity index")

### for simpson div
#  linear model
lm_fit_simp<- lm(simpson_div ~ non_native_abu_prop, data=non_native_propotion_df)
summary(lm_fit_simp)

plot(lm_fit_simp)
tidy(lm_fit_simp)

glance(lm_fit_simp)

## looks like there's a hump-shaped relationship between alpha diversity and proportion of non-native ants,
## not sure that the linear model is the best fit for these data
ggplot(non_native_propotion_df, aes(non_native_abu_prop, simpson_div))+
  geom_point()+
  geom_line(data = broom::augment(lm_fit), aes(x = non_native_abu_prop, y = .fitted))+
  #facet_grid(~Year)+
  theme_classic()+
  labs(x = "Proportion of non-native ant abundance",
       y = "Shannon diversity index")

### do the same as above but this time by species occurrence (presence/absence) rather than abundance (Pheidole flavens complex as native)
data_spacc_all_tidy_occ_NN <- data_spacc_all_tidy_PF_non_native %>% 
  filter(abundance != 0) %>%  # sp has to be present to contribute
  filter(Native == 1) %>% 
  group_by(ID, Year) %>% 
  summarize(num_native_sp = n())

data_spacc_all_tidy_non_native <- data_spacc_all_tidy %>% 
  filter(abundance != 0) %>%  # sp has to be present to contribute
  filter(Native == 0) %>% 
  group_by(ID, Year) %>% 
  summarize(num_nonnative_sp = n())

occurrence_proportion_df_NN <- data_spacc_all_tidy_occ_NN %>% 
  full_join(data_spacc_all_tidy_non_native) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  mutate(total_sp = num_native_sp + num_nonnative_sp,
         prop_nonnative_sp = num_nonnative_sp/total_sp) %>% 
  ungroup() %>% 
  mutate(ID = as.numeric(ID)) %>% 
  arrange(ID) %>% 
  bind_cols(shannon_div_df, 
            simpson_div_df)

occurrence_proportion_df_cat_NN <- occurrence_proportion_df_NN %>% 
  filter(total_sp != 0) %>% 
  mutate(category = ifelse(prop_nonnative_sp < .33, "Low", 
                           ifelse(prop_nonnative_sp > .66, "High", "Medium")))

occurrence_proportion_df_cat_NN$category <- factor(occurrence_proportion_df_cat_NN$category, levels = c("Low", "Medium", "High"))

my_comparisons <- list(   c("High", "Medium"),  c("Low", "Medium"), c("Low", "High") )


occurrence_proportion_df_cat_tidy_NN <- occurrence_proportion_df_cat_NN %>% 
  gather(diversity_type, diversity_measure, -c(ID, Year, num_native_sp, num_nonnative_sp,
                                               prop_nonnative_sp, Year1, Year2, category)) %>% 
  mutate(diversity_type = case_when(diversity_type == "shannon_div" ~ "Shannon diversity index",
                                    diversity_type == "simpson_div" ~ "Simpson diversity index",
                                    diversity_type == "total_sp" ~ "Species richness"))

occurrence_prop_sp_fig_NN <- ggplot(occurrence_proportion_df_cat_tidy_NN, aes(category, diversity_measure, fill = category))+
  geom_boxplot(position =position_dodge())+
  geom_point(position = position_jitterdodge(0.25), alpha = .75)+
  stat_compare_means(comparisons = my_comparisons, aes(label = ..p.signif..))+
  scale_fill_manual(values = wes_palette("GrandBudapest1"))+
  scale_color_manual(values = wes_palette("GrandBudapest1"))+
  labs(y = "Alpha diversity",
       x = "Proportion of non-native species (by occurrence with P.flavens as non-native)")+
  theme_classic()+
  theme(text = element_text(family = "Times New Roman", size = 14),
        legend.position = "bottom",
        legend.title = element_blank())+
  facet_grid(diversity_type~., scales="free_y")


plot_grid(occurrence_prop_sp_fig, occurrence_prop_sp_fig_NN, abundance_prop_alpha_fig, abundance_prop_alpha_fig_NN, cols = 2, rows =2)
#### ^^ Not much (if any) real difference between outcomes of treating P. flavens as native or non-native
### or by having the proportion of non-native species be based on occurrence or abundance


### for shannon div
#  linear model
lm_fit_shan_occ<- lm(shannon_div ~ prop_nonnative_sp, data=occurrence_proportion_df)
summary(lm_fit_shan_occ)

plot(lm_fit_shan_occ)
tidy(lm_fit_shan_occ)

glance(lm_fit_shan_occ)

## residuals plot looks more "random" -- linear model is more appropriate for these occurrence data than the abundance proportion
ggplot(occurrence_proportion_df, aes(prop_nonnative_sp, shannon_div))+
  geom_point()+
  geom_line(data = broom::augment(lm_fit_shan_occ), aes(x = prop_nonnative_sp, y = .fitted))+
  #facet_grid(~Year)+
  theme_classic()+
  labs(x = "Proportion of non-native ant species",
       y = "Shannon diversity index")

### for simpson div
#  linear model
lm_fit_simp_occ<- lm(simpson_div ~ prop_nonnative_sp, data=occurrence_proportion_df)
summary(lm_fit_simp_occ)

plot(lm_fit_simp_occ)
tidy(lm_fit_simp_occ)

glance(lm_fit_simp_occ)


## residuals plot looks more "random" -- linear model is still not v. appropriate for these data than the abundance proportion
ggplot(occurrence_proportion_df, aes(prop_nonnative_sp, simpson_div))+
  geom_point()+
  geom_line(data = broom::augment(lm_fit_simp_occ), aes(x = prop_nonnative_sp, y = .fitted))+
  #facet_grid(~Year)+
  theme_classic()+
  labs(x = "Proportion of non-native ant species",
       y = "Simpson diversity index")


## Next step is to pull just Gabriela's data and look at year to year trends + seasonality in species composition
## using ordination (nMDS or PCoA and PERMANOVA + permdist)
data_NMDS <- data_GZ_wide %>% 
  select(-Year)




# Then calculate multivariate distances between samples using vegdist() with one of various distance metrics:
dist_jac <- vegdist(data_NMDS, method = "jaccard", binary = TRUE) # Binary jaccard distance (presence/absence)
dist_BC <- vegdist(data_NMDS, method = "bray")#, binary = TRUE) # Bray-Curtis distance

# Then generate an MDS object using metaMDS():
mds <- metaMDS(dist_BC) ## stress is too low with the full community -- too many super rare species
str(mds)



## NMDS isn't converging with all of the individual pitfalls -- likely because many of them have few but differenct ant spp
## try this again but now grouped by site within year and season

## drop super rare species (present in > 1% of of total abundance) -- this results in 11 species

data_new <- data_GZ_clean %>% 
  select(Year, Month, Genus, Species, Abundance) %>% 
  filter(Year == 2014 |
           Year == 2015) %>% 
  rownames_to_column() %>% 
  unite(species, c("Genus","Species"), sep =  " ", remove = F) %>% 
  spread(key = "species", value = "Abundance") %>% 
  mutate_all(~replace(., is.na(.), 0))%>%  ## make all the NAs under the species names zeros (for this analysis) 
  group_by(Year, Month) %>% 
  summarize_at(vars(`Aphaenogaster carolinensis`:`Trachymyrmex septentrionalis`), sum) %>% 
  gather(key = "species", value = "Abundance", c(-Year,-Month)) %>% 
  ungroup() %>% 
  group_by(species) %>% 
  summarize(Abundance = sum(Abundance)) %>% 
  mutate(percent_total = Abundance/1754) %>% 
  filter(percent_total > 0.01)

sp_list <- as.list(data_new$species)

total_abu <- data_new %>% 
  ungroup() %>% 
  summarize(total_abundance = sum(Abundance))

data_GZ_clean_subset <- data_GZ_clean  %>% 
  unite(species, c("Genus", "Species"), sep =" ", remove = F) %>% 
filter(species %in% sp_list) %>% 
spread("species", "Abundance") %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  group_by(Year, Month, Site) %>% ## summarize by site since we have multiple pitfall traps per site
  ## and the individual pitfall level resolution is too fine-scale
  summarize_at(vars(`Aphaenogaster carolinensis`:`Solenopsis molesta`), sum)


GZ_wide_NMDS_new <- data_GZ_clean_subset %>% 
  ungroup() %>% 
  select(-c(Year:Site))



# Then calculate multivariate distances between samples using vegdist() with one of various distance metrics:
dist_jac <- vegdist(GZ_wide_NMDS_new, method = "jaccard", binary = TRUE) # Binary jaccard distance (presence/absence)
dist_BC <- vegdist(GZ_wide_NMDS_new, method = "bray")#, binary = TRUE) # Bray-Curtis distance

# Then generate an MDS object using metaMDS(): ### hooray!! it converges and stress is 0.173
mds <- metaMDS(dist_BC, trymax = 300)

# Plot the mds using basic graphics
plot(mds)

# Make this into a ggplot:
# Extract the coordinates:
points <- as.data.frame(mds$points)
points

# Add metadata to the points:
data_GZ_clean_subset_2 <- data_GZ_clean_subset %>% 
  rownames_to_column() %>% 
  mutate(Season = ifelse(Month == 5, "Spring", "Fall")) %>% 
  unite(Season_yr, c(Season, Year), remove =F)

## get the proportion of non-native/total spp for this subset of data
subset_prop_NN <- data_GZ_clean_subset_2 %>% 
  gather("Species", "abundance", -c(rowname:Site, Season)) %>% 
  group_by(Season_yr, Site) %>% 
  left_join(native_classification) %>%
  filter(abundance >0) %>% 
  summarize(species_total = length(abundance),
            native_total = sum(Native)) %>% 
  mutate(non_native_total = species_total - native_total,
         prop_non_native = non_native_total/species_total,
  category = ifelse(prop_non_native < .33, "Low", 
                    ifelse(prop_non_native > .66, "High", "Medium")))


data_GZ_clean_subset_2 <- data_GZ_clean_subset_2 %>% 
  left_join(subset_prop_NN, by = c("Season_yr", "Site"))

points <- points %>% mutate(Sample = rownames(points),
                            Year = data_GZ_clean_subset_2$Year[match(rownames(points), data_GZ_clean_subset_2$rowname)],
                            Month = data_GZ_clean_subset_2$Month[match(rownames(points), data_GZ_clean_subset_2$rowname)],
                            Season = data_GZ_clean_subset_2$Season[match(rownames(points), data_GZ_clean_subset_2$rowname)],
                            Season_yr = data_GZ_clean_subset_2$Season_yr[match(rownames(points), data_GZ_clean_subset_2$rowname)],
                            category = data_GZ_clean_subset_2$category[match(rownames(points), data_GZ_clean_subset_2$rowname)],
                            prop_non_native = data_GZ_clean_subset_2$prop_non_native[match(rownames(points), data_GZ_clean_subset_2$rowname)]) 

points

# Q4: Now that we have the MDS ordination data, how can we make it into a nice-looking ggplot?

ggplot(data = points, aes(x = MDS1, y = MDS2))+
  geom_point(aes(color = Season))+
  stat_ellipse(aes(color = (Season)))+
  scale_color_manual(values = c("darkgray", "black"))+
  theme_classic()

ggplot(data = points, aes(x = MDS1, y = MDS2))+
  geom_point(aes(color = as.factor(Month)))+
  stat_ellipse(aes(color = as.factor(Month)))+
  labs(x = "nMDS1",
       y = "nMDS2")+
  theme_classic()

perm <- adonis(dist_BC ~ Season*Year, data = data_GZ_clean_subset_2, strata = data_GZ_clean_subset_2$Site)

perm


## Composition in ant community affected by seasonality but not by year

## check for homogeneity in dispersion - if there is, it may invalidate the PERMANOVA sp. composition results

group <- data_GZ_clean_subset_2$Month
## Calculate multivariate dispersions
mod <- betadisper(dist_BC, group)
mod

## Perform test
anova(mod)

## Permutation test for F
permutest(mod, pairwise = TRUE)

## Tukey's Honest Significant Differences
(mod.HSD <- TukeyHSD(mod))
plot(mod.HSD)

## Plot the groups and distances to centroids on the
## first two PCoA axes
plot(mod)

## Draw a boxplot of the distances to centroid for each group
boxplot(mod)

## no difference in homogeneity of dispersion, can continue with the seasonality differences being due to compositional differences
## rather than being driven by differences in composition within sample groups (higher vs lower beta diversity)

#############################
################## using Meghan's data test for correlation between proportion of non-native ants and distance to edge of preserve
### This section is just for pitfall traps

## GIS dataframe created by Meghan
GIS_df <- read_excel("BIG THICKET GIS DATA 4-6-2016.xlsx", sheet="Sheet1")

GIS_select <- GIS_df %>% 
  select(Site_Code, DistanceToEdgeOfPreserve)

data_MH_pit <- data_MH_clean %>% 
  filter(`Collection Method` == "Pit") %>% 
  gather("Species", "abundance", -c(Site:`Collector (1=SES)`,Site_Code, `Inside or Outside (Inside=1)`)) %>% 
  left_join(native_classification, by = "Species") %>% 
  filter(abundance != 0) 

data_MH_pit_summary <- data_MH_pit %>% 
  group_by(Site, `Specific Site Number`, Site_Code, Replicate) %>% 
  summarize(total_species = length(Species),
            total_native = sum(Native),
            total_non_native = total_species-total_native,
            prop_non_native = total_non_native/total_species) %>%
  mutate(non_native_binom = ifelse(total_non_native >0, 1, 0)) %>% 
  left_join(GIS_select) %>% 
  mutate(Site_ID = `Specific Site Number`,
dist_km = (DistanceToEdgeOfPreserve/1000)) ## convert to km since glmer is having trouble with the scale of the data


library(lme4)

## Binomial model of presence of non-native ant sp in pitfall trap by distance to preserve edge (Random effect of SITE)
## could also do model fitting

glmer_0 <- glmer(cbind(total_non_native, total_native) ~ (1|Site_ID), family = "binomial", data = data_MH_pit_summary)
glmer_fit <- glmer(cbind(total_non_native, total_native) ~ dist_km + (1|Site_ID), family = "binomial", data = data_MH_pit_summary)

summary(glmer_0)
summary(glmer_fit)

## model with the fixed effect of distance has the lower AIC value and the majority of the weight
AICtab(glmer_0, glmer_fit, weights = T)

plot(x=data_MH_pit_summary$dist_km, y = data_MH_pit_summary$non_native_binom)
MyData =data.frame(cbind(dist_km = seq(from = min(data_MH_pit_summary$dist_km), to = max(data_MH_pit_summary$dist_km, length.out = nrow(data_MH_pit_summary))),
                   Site_ID = seq(from = min(data_MH_pit_summary$dist_km), to = max(data_MH_pit_summary$dist_km, length.out = nrow(data_MH_pit_summary)))))
Pred <- predict(glmer_fit, newdata = MyData, type = "response", allow.new.levels=T)
lines(MyData$dist_km, Pred)

## make this in ggplot

pd <-with(data_MH_pit_summary,
          data.frame(cbind(dist_km = seq(min(data_MH_pit_summary$dist_km), max(data_MH_pit_summary$dist_km), length.out = nrow(data_MH_pit_summary))),
                     Site_ID = seq(min(data_MH_pit_summary$dist_km), max(data_MH_pit_summary$dist_km), length.out = nrow(data_MH_pit_summary))))

pd1 <- cbind(pd, predict(glmer_fit, newdata=pd, type = "response", allow.new.levels = T)) %>% 
  rename(fit_y = `predict(glmer_fit, newdata = pd, type = "response", allow.new.levels = T)`)


## binomial model fit to presence/absence of any non-native ant sp found in pitfall trap
ggplot(data_MH_pit_summary, aes((dist_km), non_native_binom))+
  geom_point(position = position_jitter(width =.02, height = 0.02), alpha = .6)+
geom_line(data = pd1, aes(x = dist_km_x, y = fit_y))+
  theme_classic()+
  labs(x = "Distance to edge of preserve (km)",
       y = "Probability of non-native ant species present")

anova(glmer_fit, test="Chisq")

##### visualize and test for relationship between the to dominant non-native ants and distance to edge
data_MH_pit_invasives <- data_MH_clean %>% 
  filter(`Collection Method` == "Pit") %>% 
  gather("Species", "abundance", -c(Site:`Collector (1=SES)`,Site_Code, `Inside or Outside (Inside=1)`)) %>% 
  left_join(native_classification, by = "Species") %>% 
  filter(abundance != 0,
         Species == "Nylanderia fulva" |
           Species == "Solenopsis invicta") %>% 
  left_join(GIS_select) %>% 
  mutate(dist_km = (DistanceToEdgeOfPreserve/1000))


ggplot(data_MH_pit_invasives,aes(dist_km, abundance))+
  geom_point(aes(color = Species))+
  theme_classic()



############################ same as above but for all collection methods

## GIS dataframe created by Meghan
GIS_select <- GIS_df %>% 
  select(Site_Code, DistanceToEdgeOfPreserve)

data_MH_all <- data_MH_clean %>% 
  #filter(`Collection Method` == "Pit") %>% 
  gather("Species", "abundance", -c(Site:`Collector (1=SES)`,Site_Code, `Inside or Outside (Inside=1)`)) %>% 
  left_join(native_classification, by = "Species") %>% 
  filter(abundance != 0) ## have to drop species that weren't found in the trap, since this is based on sp occurrence
  ## that is pooled at the Replicate level within site codes

data_MH_all_summary <- data_MH_all %>% 
  group_by(Site, `Specific Site Number`, Site_Code, Replicate, `Collection Method`) %>% 
  summarize(total_species = length(Species),
            total_native = sum(Native),
            total_non_native = total_species-total_native,
            prop_non_native = total_non_native/total_species) %>%
  mutate(non_native_binom = ifelse(total_non_native >0, 1, 0)) %>% 
  left_join(GIS_select) %>% 
  mutate(Site_ID = `Specific Site Number`,
         dist_km = (DistanceToEdgeOfPreserve/1000)) ## convert to km since glmer is having trouble with the scale of the data


library(lme4)

## Binomial model of presence of non-native ant sp in all trap types by distance to preserve edge (Random effect of SITE)
## could also do model fitting

glmer_all_0 <- glmer(cbind(total_non_native, total_native) ~ (1|Site_ID), family = "binomial", data = data_MH_all_summary)
glmer_all_fit <- glmer(cbind(total_non_native, total_native) ~ dist_km + (1|Site_ID), family = "binomial", data = data_MH_all_summary)

summary(glmer_0)
summary(glmer_fit)

## model with the fixed effect of distance has the lower AIC value and the majority of the weight
AICtab(glmer_0, glmer_fit, weights = T)

plot(x=data_MH_all_summary$dist_km, y = data_MH_all_summary$non_native_binom)
MyData =data.frame(cbind(dist_km = seq(from = min(data_MH_all_summary$dist_km), to = max(data_MH_all_summary$dist_km, length.out = nrow(data_MH_all_summary))),
                         Site_ID = seq(from = min(data_MH_all_summary$dist_km), to = max(data_MH_all_summary$dist_km, length.out = nrow(data_MH_all_summary)))))
Pred <- predict(glmer_fit, newdata = MyData, type = "response", allow.new.levels=T)
lines(MyData$dist_km, Pred)

## make this in ggplot

pd <-with(data_MH_all_summary,
          data.frame(cbind(dist_km = seq(min(data_MH_all_summary$dist_km), max(data_MH_all_summary$dist_km), length.out = nrow(data_MH_all_summary))),
                     Site_ID = seq(min(data_MH_all_summary$dist_km), max(data_MH_all_summary$dist_km), length.out = nrow(data_MH_all_summary))))

pd1 <- cbind(pd, predict(glmer_fit, newdata=pd, type = "response", allow.new.levels = T)) %>% 
  rename(fit_y = `predict(glmer_fit, newdata = pd, type = "response", allow.new.levels = T)`)


## binomial model fit to presence/absence of any non-native ant sp found in pitfall trap
ggplot(data_MH_all_summary, aes((dist_km), non_native_binom), color = `Collection Method`)+
  geom_point(aes(color = `Collection Method`),position = position_jitter(width =.02, height = 0.02), alpha = .6)+
  geom_line(data = pd1, aes(x = dist_km_x, y = fit_y))+
  theme_classic()+
  labs(x = "Distance to edge of preserve (km)",
       y = "Probability of non-native ant species present")

## comfortable with the spread of the different collection methods across the distance to edge,
## and occurrence of non-native ant spp
ggplot(data_MH_all_summary, aes((dist_km), non_native_binom))+
  geom_point(position = position_jitter(width =.02, height = 0.02), alpha = .6)+
  geom_line(data = pd1, aes(x = dist_km_x, y = fit_y))+
  theme_classic()+
  labs(x = "Distance to edge of preserve (km)",
       y = "Probability of non-native ant species present")

anova(glmer_fit, test="Chisq")

##### visualize and test for relationship between the to dominant non-native ants and distance to edge
data_MH_all_invasives <- data_MH_clean %>% 
  #filter(`Collection Method` == "Pit") %>% All collection methods
  gather("Species", "abundance", -c(Site:`Collector (1=SES)`,Site_Code, `Inside or Outside (Inside=1)`)) %>% 
  left_join(native_classification, by = "Species") %>% 
  filter(#abundance != 0, ## keeping in samples for which we did not observe the two spp of interest
         Species == "Nylanderia fulva" |
           Species == "Solenopsis invicta") %>% 
  left_join(GIS_select) %>% 
  filter(DistanceToEdgeOfPreserve != "NA") %>% 
  mutate(Site_ID = `Specific Site Number`,
         dist_km = (DistanceToEdgeOfPreserve/1000))

lmer_inv_0 <- lmer(abundance ~ (1|Site_ID), data = data_MH_all_invasives, REML=F)
lmer_inv_fit <- lmer(abundance ~ dist_km + (1|Site_ID), data = data_MH_all_invasives, REML=F)

summary(lmer_inv_0)
summary(lmer_inv_fit)

## model with the fixed effect of distance has the lower AIC value and the majority of the weight
AICtab(lmer_inv_0, lmer_inv_fit, weights = T)


pd_inv <-with(data_MH_all_invasives,
          data.frame(cbind(dist_km = seq(min(data_MH_all_invasives$dist_km), max(data_MH_all_invasives$dist_km), length.out = nrow(data_MH_all_invasives_no_zero))),
                     Site_ID = seq(min(data_MH_all_invasives$dist_km), max(data_MH_all_invasives$dist_km), length.out = nrow(data_MH_all_invasives_no_zero))))

pd1_inv <- cbind(pd_inv, predict(lmer_inv_fit, newdata=pd_inv, type = "response", allow.new.levels = T)) %>% 
  rename(fit_y = `predict(lmer_inv_fit, newdata = pd_inv, type = "response", allow.new.levels = T)`)



## similar trend across collection methods -- OK to "pool" 
ggplot(data_MH_pit_invasives,aes(dist_km, abundance))+
  geom_point(aes(color = Species, shape = `Collection Method`))+
  geom_line(data = pd1_inv, aes(x = dist_km, y = fit_y))+
  #geom_smooth(method = "lm", se=F) ## nearly identical fit to our estimate with random effects of site
  theme_classic()

ggplot(data_MH_pit_invasives,aes(dist_km, abundance))+
  geom_point(aes(color = Species))+
  theme_classic()+
  geom_smooth(method = "lm", se=F)

## combining across collection methods doesn't affect presence/absence probability of presence,
## but it does greatly increases the abundances

##### visualize and test for relationship between the to dominant non-native ants and distance to edge
data_MH_all_invasives_no_zero <- data_MH_clean %>% 
  #filter(`Collection Method` == "Pit") %>% All collection methods
  gather("Species", "abundance", -c(Site:`Collector (1=SES)`,Site_Code, `Inside or Outside (Inside=1)`)) %>% 
  left_join(native_classification, by = "Species") %>% 
  filter(abundance != 0, ## drop instances where we did not observe species
    Species == "Nylanderia fulva" |
      Species == "Solenopsis invicta") %>% 
  left_join(GIS_select) %>% 
  filter(DistanceToEdgeOfPreserve != "NA") %>% 
  mutate(Site_ID = `Specific Site Number`,
    dist_km = (DistanceToEdgeOfPreserve/1000)) ## convert to km since glmer is having trouble with the scale of the data

lmer_inv_0 <- lm(abundance ~ (1), data = data_MH_all_invasives_no_zero)
lmer_inv_fit <- lm(abundance ~ dist_km, data = data_MH_all_invasives_no_zero)
##^^ getting an error message "boundary (singular)" means variance is basically = 0 -- not fitting well
summary(lmer_inv_0)
summary(lmer_inv_fit)

## model with the random effect has the lower AIC value and the majority of the weight -- go with modelt hat includes zeros
AICtab(lmer_inv_0, lmer_inv_fit, weights = T)

#anova(lmer_inv_0, lmer_inv_fit)

plot(x=data_MH_all_invasives_no_zero$dist_km, y = data_MH_all_invasives_no_zero$abundance)
MyData =data.frame(cbind(dist_km = seq(from = min(data_MH_all_invasives_no_zero$dist_km), to = max(data_MH_all_invasives_no_zero$dist_km, length.out = nrow(data_MH_all_summary))),
                         Site_ID = seq(from = min(data_MH_all_invasives_no_zero$dist_km), to = max(data_MH_all_invasives_no_zero$dist_km, length.out = nrow(data_MH_all_summary)))))
Pred <- predict(lmer_inv_fit, newdata = MyData, type = "response", allow.new.levels=T)
lines(MyData$dist_km, Pred)

## make this in ggplot

pd <-with(data_MH_all_invasives_no_zero,
          data.frame(cbind(dist_km = seq(min(data_MH_all_invasives_no_zero$dist_km), max(data_MH_all_invasives_no_zero$dist_km), length.out = nrow(data_MH_all_invasives_no_zero))),
                     Site_ID = seq(min(data_MH_all_invasives_no_zero$dist_km), max(data_MH_all_invasives_no_zero$dist_km), length.out = nrow(data_MH_all_invasives_no_zero))))

pd1 <- cbind(pd, predict(glmer_fit, newdata=pd, type = "response", allow.new.levels = T)) %>% 
  rename(fit_y = `predict(glmer_fit, newdata = pd, type = "response", allow.new.levels = T)`)


## similar trend across collection methods -- OK to "pool" 
ggplot(data_MH_pit_invasives_no_zero,aes(dist_km, abundance))+
  geom_point(aes(color = Species, shape = `Collection Method`))+
  theme_classic()+
  geom_smooth(method = "lm", se=F)+
  facet_grid(~`Collection Method`)

ggplot(data_MH_pit_invasives_no_zero,aes(dist_km, abundance))+
  geom_point(aes(color = Species))+
  theme_classic()+
  geom_smooth(method = "lm", se=F)
