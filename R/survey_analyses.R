

library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(forcats)

# Load edit-a-thon qualtrics data ####
raw <- read_csv('data/WikiProject_hackathon_qualtrics.csv') # Survey responses
key <- read_csv('data/questionKey.csv') # Question key

qq <- "Q6"

key %>%
  dplyr::filter(str_detect(Raw, qq)) %>%
  pull(2)

# percent of people who used the tutorials
sum(raw$Q6 == 'Yes') / nrow(raw) * 100

# comfort in using different functions of Wikipedia 
utility_data <- raw %>%
  dplyr::select(contains("Q10_")) %>% # pull out q8 answers
  gather(question, utility) %>% # reshape
  group_by(question, utility) %>% 
  summarise(n_utility = n()) %>%
  filter(!is.na(utility)) %>% # take out NAs
  left_join(key, by = c("question" = "Raw")) %>% # add names
  ungroup() %>% dplyr::select(-question) %>%
  mutate(Key = purrr::map_chr(.x = Key, ~stringr::str_sub(.x, start = 15))) %>% # shorten
  arrange(Key, desc(n_utility))
# spread(utility, n_utility) %>% View()

utility_levels <-  c("Extremely comfortable", "Very comfortable", "Moderately comfortable",
                     "Somewhat comfortable", "Not at all comfortable", "Not applicable/ Did not use")

utility_data <- utility_data %>%
  mutate(utility_fct = factor(utility, levels = utility_levels))


comfort_of_func = utility_data %>%
  ggplot(aes(x = Key, y = n_utility, fill = utility_fct)) +
  geom_bar(stat = "identity", 
           aes(fill = utility_fct), col = "black") +
  coord_flip() + theme_bw() +
  scale_fill_viridis_d() +
  theme(legend.position = "bottom", 
        legend.text = element_text(size = 12),
        legend.title=element_blank(),
        axis.text.y = element_text(size = 12)) +
  xlab(element_blank()) + ylab(element_blank())

ggsave(filename = 'figures/comfort_of_functions.png', plot = comfort_of_func, width = 12, height = 6)

