rm(list = ls())  # clear Rs brain
library(readr)   # read csv file
library(tidyr)   # tidying data
library(dplyr)   # manipulating df
library(ggplot2) # graphing
data_1 <- read.csv("les donnees.csv")
unique(data_1$host_id)
mono_data <- data_1 %>%
  filter(host_id != "NA") %>%# cheat way of getting rid of NA host id
  group_by(host_id, species) %>%
  mutate(sp_mean_size = mean(worm_length), # get species mean within a host
         sp_std_size = sd(worm_length))%>% # get standard deviation of the species mean within a host
  ungroup()%>% # ungroup so we can group it again
  group_by(host_id)%>%
  mutate(egg_per_worm = eggs/total)%>% # eggs per worm, but no egg data here, so just leaving it for future reference
  ungroup()%>%
  gather(parasite, abundance, 94:99)%>% # make this part tidy so you can easily multiple abun * size later on
  group_by(host_id, lake, species, parasite) %>% # group to get summary info
  summarise_at(c("sp_mean_size", "abundance"), mean) %>%# gives mean size and abundance of each specie
  filter(species == parasite) %>%# filter when these names match
  group_by(host_id)%>%
  mutate(size_cwm = weighted.mean(sp_mean_size, abundance)) # trait * weighted by abundance

mono_data<- filter(mono_data, species != "cleid")


write.csv(mono_data, "mono_data.csv")
