##########################################################################
# Code for analyzing data from occupational characteristics survey #
##########################################################################

#libraries
library(tidyverse)
library(actdata)
library(here)

#first, read in data
oc_data <- read_csv(here("activities/occ_char_data.csv")) 
oc_data <- oc_data %>% slice(3:nrow(oc_data))

#long form
oc_data_long <- oc_data %>% 
                pivot_longer(firefighter:scientist,
                             names_to = "occupation",
                             values_to = "characteristic") %>% 
                separate_rows(characteristic, 
                              sep = ",") %>% 
                mutate(occupation = str_replace_all(occupation, 
                                                    "elementarysch",
                                                    "elementary_school_teachers"),
                       occupation = str_replace_all(occupation, 
                                                    "sec_guard",
                                                    "security_guard"),
                       occupation = str_replace_all(occupation, 
                                                    "scientist",
                                                    "life_scientist"),
                       occupation = str_replace_all(occupation, 
                                                    "pilot",
                                                    "airline_pilot"),
                       occupation = str_replace_all(occupation, 
                                                    "nurse",
                                                    "registered_nurse"))

#read in BSRI data
bsri <- read_csv(here("activities/bsri.csv"))

oc_data_bsri <- oc_data_long %>% 
                count(occupation, characteristic) %>% ungroup() %>% 
                group_by(occupation) %>% 
                arrange(desc(n)) %>% slice(1:10) %>% 
                left_join(bsri, by = "characteristic")

ggplot(oc_data_bsri, mapping = aes(x = characteristic, 
                                   y = n, 
                                   fill = gender_association)) + 
      geom_col() + coord_flip() + 
      facet_wrap(~occupation, scales = "free_y")

#########################################################
# Relationship between EPA and characteristics 
#########################################################

#fake data just for prep purposes 
oc_data_fake <- tibble(unique_id = rep(rep(1:17, each = 12), each = 10),
                       occupation = rep(rep(unique(oc_data_bsri$occupation), 17), 
                                        each = 10))


oc_data_fake <- oc_data_fake %>% rowwise() %>% 
                mutate(characteristic = sample(bsri$characteristic, 1))


oc_data_fake <- oc_data_long %>% 
  mutate(occupation = str_replace_all(occupation, 
                                      "elementarysch",
                                      "elementary_school_teachers"),
         occupation = str_replace_all(occupation, 
                                      "sec_guard",
                                      "security_guard"),
         occupation = str_replace_all(occupation, 
                                      "scientist",
                                      "life_scientist"),
         occupation = str_replace_all(occupation, 
                                      "nurse",
                                      "registered_nurse"))


occs <- actdata::epa_subset(dataset = "occs2019")

occ_data_epa <- oc_data_fake %>% 
                left_join(bsri, by = "characteristic") %>% 
                count(occupation, gender_association) %>% 
                left_join(occs, by = c("occupation" = "term"))


ggplot(occ_data_epa %>% filter(gender_association != "N"), 
       mapping = aes(x = E, y = n, label = occupation)) + 
       geom_point() + facet_wrap(~gender_association) + geom_label() +
       geom_smooth() + 
       theme_minimal() + labs(x = "Evaluation of Occupational Identity",
                              y = "Count of characteristics for each gender association") 


ggplot(occ_data_epa %>% filter(gender_association != "N"), 
       mapping = aes(x = P, y = n, label = occupation)) + 
       geom_point() + facet_wrap(~gender_association) + geom_label() + 
       geom_smooth() + 
       theme_minimal() + labs(x = "Potency of Occupational Identity",
                         y = "Count of characteristics for each gender association") 

ggplot(occ_data_epa %>% filter(gender_association != "N"), 
       mapping = aes(x = A, y = n, label = occupation)) + 
      geom_point() + facet_wrap(~gender_association) + geom_label() + 
  geom_smooth() + 
      theme_minimal() + labs(x = "Activity of Occupational Identity",
                         y = "Count of characteristics for each gender association") 
