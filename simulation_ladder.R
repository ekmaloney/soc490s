library(tidyverse)
library(MASS)
library(readxl)

#occs data
occs_data <- read_excel(here::here("activities/occ_ladder.xlsx"))
occs_data <- read_excel(here::here("activities/toy_ratings.xlsx"))

occs_data_long <- occs_data %>% 
  pivot_longer(R1:R8,
               names_to = "respondent",
               values_to = "rating")

ggplot(occs_data_long, mapping = aes(x = rating)) + 
      geom_histogram() + facet_wrap(~respondent)

#want to vary:

#where occupations are placed on the occupational status ladder
#how the ratings are used (concentrated in the top or bottom of the scales)
#polarizing category means
#dispersion of variation in group 
# respondent status 

#so for high status 
df <- tibble(respondent = rep(c(1:10), each = 40),
             occupation = rep(occs_data$occupation, 10))
df <- df %>% left_join(occs_data)
df <- df %>% arrange(education_amount)

df_high <- df %>% rowwise() %>% filter(education_amount == "high") %>% 
           mutate(ladder_position = round(rnorm(n = 1, mean = 6.5, sd = 1), digits = 0))

df_low <- df %>% rowwise() %>% filter(education_amount == "low") %>% 
  mutate(ladder_position = round(rnorm(n = 1, mean = 4.5, sd = 0.5), digits = 0))

all_df <- bind_rows(df_high, df_low)

all_df %>% 
  ggplot(mapping = aes(x = ladder_position, fill = education_amount)) + 
  geom_histogram(binwidth = 1)

#figuring out some distribution stuff
sim_values <- tibble(x = rnorm(n = 1000, mean = 7.5, sd = 0.5),
                     x_round = round(x, digits = 1))

ggplot(sim_values, aes(x = x_round)) + geom_histogram(binwidth = 1)

#distance:
# absolute distance -- the Manhattan distance between respondents i and j, 
#which is the absolute difference between ranki and rankj for occupation k 
#summed over all occupa- tions; this corresponds to what we consider total 
#dissimilarity.

#Next, we convert raw scores to z-scores and then compute the Manhattan distance 
#again; this second dissimilarity score thus takes into account differences with 
#respect to pile sort mean and spread

