library(flexdashboard)
library(tidyverse)
library(tidyr)
library(scales)
library(gridExtra)
library(plotly)
library(DT)
library(readr)
library(dplyr)
library(stringr)
library(fuzzyjoin)

###
jeop_data <- readr::read_tsv(file = "../data/jeopardy_clue_dataset-master/master_season1-35.tsv")
bio_data <- readr::read_csv(file = "../data/ap_biology.csv", col_names = FALSE) %>% 
  dplyr::rename(vocab_term = X1) %>% 
  dplyr::mutate(subject = "biology") %>% 
  dplyr::select(subject, dplyr::everything())

ap_subjects <- c("biology", "chemistry", "physics", "history", "liturature")

school_jeop <- jeop_data %>% 
  dplyr::arrange(category) %>% 
  dplyr::mutate(category = stringr::str_to_lower(category)) %>% 
  dplyr::filter(category %in% ap_subjects)

joined_data <- fuzzyjoin::regex_inner_join(x = school_jeop, y = bio_data, by = c("question"= "vocab_term"))

join_by_subject <- function(jeop_df, subject_df) {
  # Make question column lower case
  jeop_df <- jeop_df %>% 
    dplyr::mutate(question = stringr::str_to_lower(question))
  
  subject_df <- subject_df %>% 
    dplyr::mutate(vocab_term = stringr::str_to_lower(vocab_term))
  
  joined_df <- fuzzyjoin::regex_inner_join(x = jeop_df, y = subject_df, by = c("question"= "vocab_term"))
  return(joined_df)
}

join_test <- join_by_subject(school_jeop, bio_data)
###

# data for round 1
heat_data_r1 <- join_test %>%
  dplyr::filter(daily_double %in% "no") %>% 
  dplyr::select(value, subject) %>% 
  dplyr::group_by(value, subject) %>%
  count() %>%
  ungroup()

heat_data_r1 %>% 
  slice(1:5)
###
hmap_1_interact <- ggplot(school_jeop,
                          aes(x = value)) +
  geom_bar() +
  theme_light() +
  xlim(0, 2000) +
  facet_wrap("category") 

ggplotly(hmap_1_interact) 
