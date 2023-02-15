read.table("activities/09593-0001-Data.txt")

gal <- read.table("activities/09593-0001-Data.txt")


gal <- haven::read_spss("activities/09593-0001-Setup.sps")


histogram <- tibble(ranking = seq(1:9),
                    R1 = c(7, 7, 3, 2, 1, 2, 5, 7, 6),
                    R2 = c(4, 4, 4, 5, 5, 5, 5, 4, 4),
                    R3 = c(1, 3, 4, 5, 9, 7, 5, 4, 2))

histogram <- tibble(ranking = seq(1:9),
                    R1_high = c(0, 0, 0, 1, 0, 1, 5, 7, 6),
                    R1_low = c(7, 7, 3, 2, 1, 1, 0, 0, 0),
                    R2_high = c(2, 2, 2, 2, 3, 2, 3, 2, 2),
                    R2_low = c(2, 2, 2, 3, 2, 3, 2, 2, 2),
                    R3_high = c(0, 1, 1, 1, 4, 4, 4, 3, 2),
                    R3_low = c(1, 3, 3, 4, 5, 2, 1, 1, 0),
                    R4_low = c(0, 0, 1, 2, 4, 4, 4, 3, 2),
                    R4_high = c(1, 3, 3, 1, 5, 5, 1, 1, 1))

histogram_l <- histogram %>% 
                pivot_longer(R1_high:R4_high, 
                             names_to = "respondent",
                             values_to = "count") %>% 
                separate(respondent, into = c("respondent",
                                              "education_requirement"),
                         sep = "_") %>% 
                tidyr::uncount(weights = count)

histogram_l %>% 
  ggplot(mapping = aes(x = ranking)) + 
      geom_bar(stat = "count", show.legend = FALSE) + facet_wrap(~respondent) + 
  ggthemes::theme_fivethirtyeight() + scale_x_continuous(breaks = seq(1:9)) + 
  scale_y_continuous(breaks = c(1, 3, 5, 7, 9)) +
  labs(title = "Count of Occupations \nin Each Ranking Position",
       subtitle = "by respondent")

histogram_l %>% 
  ggplot(mapping = aes(x = ranking, fill = education_requirement, group = education_requirement)) + 
  geom_bar(stat = "count", show.legend = TRUE) + facet_wrap(~respondent) + 
  ggthemes::theme_fivethirtyeight() + scale_x_continuous(breaks = seq(1:9)) + 
  labs(title = "Count of Occupations \nin Each Ranking Position",
       subtitle = "by respondent",
       fill = "Educational credentials required?")

histogram_l %>% filter(respondent == "R3") %>% 
  ggplot(mapping = aes(x = ranking, fill = education_requirement, group = education_requirement)) + 
  geom_bar(stat = "count", show.legend = TRUE) + facet_wrap(~education_requirement) + 
  ggthemes::theme_fivethirtyeight() + scale_x_continuous(breaks = seq(1:9)) + 
  labs(title = "Count of Occupations \nin Each Ranking Position",
       subtitle = "by respondent",
       fill = "Educational credentials required?")

sd(c(8, 9, 7, 8, 5))

scale(c(8, 9, 7, 8, 5))
