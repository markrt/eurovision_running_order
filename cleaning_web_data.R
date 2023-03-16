### i've written some scripts to load the data in from the web
# and export to csv
# now time to clean it

# load packages
library(tidyverse)
library(janitor)


# load data
y_2014_semi_1 <- 
  read_csv("ESC-2014-first_semi-final-full_results.csv",
           skip = 1,
           na  = "\n") %>% 
  clean_names() %>% 
  mutate(event_type = 
           "semi-final 1",
         year = "2014") %>% 
  mutate(from_country = 
           fct_recode(from_country,
                      "Netherlands" = 
                        "The Netherlands")) %>% 
  mutate(to_country = 
           fct_recode(to_country,
                      "Netherlands" = 
                        "The Netherlands")) %>% 
  left_join(read_csv("ro_2014_semi_1.csv"))
y_2014_semi_2 <- 
  read_csv("ESC-2014-second_semi-final-full_results.csv",
           skip = 1,
           na  = "\n")%>% 
  clean_names()%>% 
  mutate(event_type = 
           "semi-final 2",
         year = "2014") %>% 
  mutate(from_country = 
           fct_recode(from_country,
                      "Netherlands" = 
                        "The Netherlands",
                      "North Macedonia" = 
                        "F.Y.R. Macedonia")) %>% 
  mutate(to_country = 
           fct_recode(to_country,
                      "Netherlands" = 
                        "The Netherlands",
                      "North Macedonia" = 
                        "F.Y.R. Macedonia")) %>% 
  left_join(read_csv("ro_2014_semi_2.csv"))
y_2014_final <- 
  read_csv("ESC-2014-grand_final-full_results.csv",
           skip = 1,
           na  = "\n")%>% 
  clean_names()%>% 
  mutate(event_type = 
           "final",
         year = "2014") %>% 
  mutate(from_country = 
           fct_recode(from_country,
                      "Netherlands" = 
                        "The Netherlands",
                      "North Macedonia" = 
                        "F.Y.R. Macedonia")) %>% 
  mutate(to_country = 
           fct_recode(to_country,
                      "Netherlands" = 
                        "The Netherlands",
                      "North Macedonia" = 
                        "F.Y.R. Macedonia")) %>% 
  left_join(read_csv("ro_2014_final.csv"))
y_2015_semi_1 <- 
  read_csv("ESC-2015-first_semi-final-full_results.csv",
           skip = 1,
           na  = "\n")%>% 
  clean_names()%>% 
  mutate(event_type = 
           "semi-final 1",
         year = "2015")%>% 
  mutate(from_country = 
           fct_recode(from_country,
                      "Netherlands" = 
                        "The Netherlands",
                      "North Macedonia" = 
                        "F.Y.R. Macedonia",
                      "Czechia" = 
                        "Czech Republic")) %>% 
  mutate(to_country = 
           fct_recode(to_country,
                      "Netherlands" = 
                        "The Netherlands",
                      "North Macedonia" = 
                        "F.Y.R. Macedonia",
                      "Czechia" = 
                        "Czech Republic")) %>% 
  left_join(read_csv("ro_2015_semi_1.csv"))
y_2015_semi_2 <- 
  read_csv("ESC-2015-second_semi-final-full_results.csv",
           skip = 1,
           na  = "\n")%>% 
  clean_names()%>% 
  mutate(event_type = 
           "semi-final 2",
         year = "2015")%>% 
  mutate(from_country = 
           fct_recode(from_country,
                      "Netherlands" = 
                        "The Netherlands",
                      "North Macedonia" = 
                        "F.Y.R. Macedonia",
                      "Czechia" = 
                        "Czech Republic")) %>% 
  mutate(to_country = 
           fct_recode(to_country,
                      "Netherlands" = 
                        "The Netherlands",
                      "North Macedonia" = 
                        "F.Y.R. Macedonia",
                      "Czechia" = 
                        "Czech Republic")) %>% 
  left_join(read_csv("ro_2015_semi_2.csv"))
y_2015_final <- 
  read_csv("ESC-2015-grand_final-full_results.csv",
           skip = 1,
           na  = "\n")%>% 
  clean_names()%>% 
  mutate(event_type = 
           "final",
         year = "2015")%>% 
  mutate(from_country = 
           fct_recode(from_country,
                      "Netherlands" = 
                        "The Netherlands",
                      "North Macedonia" = 
                        "F.Y.R. Macedonia",
                      "Czechia" = 
                        "Czech Republic")) %>% 
  mutate(to_country = 
           fct_recode(to_country,
                      "Netherlands" = 
                        "The Netherlands",
                      "North Macedonia" = 
                        "F.Y.R. Macedonia",
                      "Czechia" = 
                        "Czech Republic")) %>% 
  left_join(read_csv("ro_2015_final.csv"))
# nb in this case there's a missing juror
y_2016_semi_1 <- 
  read_csv("ESC-2016-first_semi-final-full_results.csv",
           skip = 1,
           na  = c("\n",
                   "-"))%>% 
  clean_names()%>% 
  mutate(event_type = 
           "semi-final 1",
         year = "2016")%>% 
  mutate(from_country = 
           fct_recode(from_country,
                      "Netherlands" = 
                        "The Netherlands",
                      "North Macedonia" = 
                        "F.Y.R. Macedonia",
                      "Czechia" = 
                        "Czech Republic")) %>% 
  mutate(to_country = 
           fct_recode(to_country,
                      "Netherlands" = 
                        "The Netherlands",
                      "North Macedonia" = 
                        "F.Y.R. Macedonia",
                      "Czechia" = 
                        "Czech Republic")) %>% 
  left_join(read_csv("ro_2016_semi_1.csv"))
y_2016_semi_2 <- 
  read_csv("ESC-2016-second_semi-final-full_results.csv",
           skip = 1,
           na  = "\n")%>% 
  clean_names()%>% 
  mutate(event_type = 
           "semi-final 2",
         year = "2016")%>% 
  mutate(from_country = 
           fct_recode(from_country,
                      "Netherlands" = 
                        "The Netherlands",
                      "North Macedonia" = 
                        "F.Y.R. Macedonia",
                      "Czechia" = 
                        "Czech Republic")) %>% 
  mutate(to_country = 
           fct_recode(to_country,
                      "Netherlands" = 
                        "The Netherlands",
                      "North Macedonia" = 
                        "F.Y.R. Macedonia",
                      "Czechia" = 
                        "Czech Republic")) %>% 
  left_join(read_csv("ro_2016_semi_2.csv"))
y_2016_final <- 
  read_csv("ESC-2016-grand_final-full_results.csv",
           skip = 1,
           na  = "\n")%>% 
  clean_names()%>% 
  mutate(event_type = 
           "final",
         year = "2016")%>% 
  mutate(to_country = 
           fct_recode(to_country,
                      "Netherlands" = 
                        "The Netherlands",
                      "North Macedonia" = 
                        "F.Y.R. Macedonia",
                      "Czechia" = 
                        "Czech Republic")) %>% 
  left_join(read_csv("ro_2016_final.csv"))
y_2017_semi_1 <- 
  read_csv("y_2017_semi_1.csv")%>% 
  mutate(event_type = 
           "semi-final 1",
         year = "2017")%>% 
  left_join(read_csv("ro_2017_semi_1.csv"))
y_2017_semi_2 <- 
  read_csv("y_2017_semi_2.csv")%>% 
  mutate(event_type = 
           "semi-final 2",
         year = "2017")%>% 
  left_join(read_csv("ro_2017_semi_2.csv"))
y_2017_final <- 
  read_csv("y_2017_final.csv")%>% 
  mutate(event_type = 
           "final",
         year = "2017")%>% 
  left_join(read_csv("ro_2017_final.csv"))
y_2018_semi_1 <- 
  read_csv("y_2018_semi_1.csv")%>% 
  mutate(event_type = 
           "semi-final 1",
         year = "2018")%>% 
  left_join(read_csv("ro_2018_semi_1.csv"))
y_2018_semi_2 <- 
  read_csv("y_2018_semi_2.csv")%>% 
  mutate(event_type = 
           "semi-final 2",
         year = "2018")%>% 
  left_join(read_csv("ro_2018_semi_2.csv"))
y_2018_final <- 
  read_csv("y_2018_final.csv")%>% 
  mutate(event_type = 
           "final",
         year = "2018")%>% 
  left_join(read_csv("ro_2018_final.csv"))
y_2019_semi_1 <- 
  read_csv("y_2019_semi_1.csv")%>% 
  mutate(event_type = 
           "semi-final 1",
         year = "2019")%>% 
  left_join(read_csv("ro_2019_semi_1.csv"))
y_2019_semi_2 <- 
  read_csv("y_2019_semi_2.csv")%>% 
  mutate(event_type = 
           "semi-final 2",
         year = "2019")%>% 
  left_join(read_csv("ro_2019_semi_2.csv"))
y_2019_final <- 
  read_csv("y_2019_final_WITH_BELARUS.csv")%>% 
  mutate(event_type = 
           "final",
         year = "2019")%>% 
  mutate(to_country =
           fct_recode(
             to_country,
             "Czechia" =
               "Czech Republic")) %>%
  left_join(read_csv("ro_2019_final.csv"))


y_2021_semi_1 <- 
  read_csv("y_2021_semi_1.csv")%>% 
  mutate(event_type = 
           "semi-final 1",
         year = "2021")%>%
  left_join(read_csv("ro_2021_semi_1.csv"))
y_2021_semi_2 <- 
  read_csv("y_2021_semi_2.csv")%>% 
  mutate(event_type = 
           "semi-final 2",
         year = "2021")%>%
  left_join(read_csv("ro_2021_semi_2.csv"))
y_2021_final <- 
  read_csv("y_2021_final.csv")%>% 
  mutate(event_type = 
           "final",
         year = "2021")%>%
  left_join(read_csv("ro_2021_final.csv"))

y_2022_semi_1 <- 
  read_csv("y_2022_semi_1.csv")%>% 
  mutate(event_type = 
           "semi-final 1",
         year = "2022")%>%
  left_join(read_csv("ro_2022_semi_1.csv"))
y_2022_semi_2 <- 
  read_csv("y_2022_semi_2_WITH_EXTRAS.csv")%>% 
  mutate(event_type = 
           "semi-final 2",
         year = "2022")%>%
  mutate(to_country =
           fct_recode(
             to_country,
             "Czechia" =
               "Czech Republic")) %>%
  left_join(read_csv("ro_2022_semi_2.csv"))
y_2022_final <- 
  read_csv("y_2022_final_WITH_EXTRAS.csv")%>% 
  mutate(event_type = 
           "final",
         year = "2022")%>%
  mutate(to_country =
           fct_recode(
             to_country,
             "Czechia" =
               "Czech Republic")) %>%
  left_join(read_csv("ro_2022_final.csv"))

# ok got everything loaded
# and paired with running order position
# get 2014-2017

everything_2014_2016 <- 
  y_2014_semi_1 %>% 
  select(c(from_country,
           to_country,
           rank,
           televote,
           event_type,
           year,
           running_order_position)) %>% 
  bind_rows(y_2014_semi_2 %>% 
              select(c(from_country,
                       to_country,
                       rank,
                       televote,
                       event_type,
                       year,
                       running_order_position)))%>% 
  bind_rows(y_2014_final %>% 
              select(c(from_country,
                       to_country,
                       rank,
                       televote,
                       event_type,
                       year,
                       running_order_position))) %>% 
  bind_rows(y_2015_semi_1 %>% 
              select(c(from_country,
                       to_country,
                       rank,
                       televote,
                       event_type,
                       year,
                       running_order_position)) %>% 
              bind_rows(y_2015_semi_2 %>% 
                          select(c(from_country,
                                   to_country,
                                   rank,
                                   televote,
                                   event_type,
                                   year,
                                   running_order_position)))%>% 
              bind_rows(y_2015_final %>% 
                          select(c(from_country,
                                   to_country,
                                   rank,
                                   televote,
                                   event_type,
                                   year,
                                   running_order_position))) )  %>% 
  bind_rows(y_2016_semi_1 %>% 
              rename(rank = 
                       jury_rank,
                     televote = 
                       televote_rank) %>% 
              select(c(from_country,
                       to_country,
                       rank,
                       televote,
                       event_type,
                       year,
                       running_order_position)) %>% 
              bind_rows(y_2016_semi_2 %>% 
                          rename(rank = 
                                   jury_rank,
                                 televote = 
                                   televote_rank) %>% 
                          select(c(from_country,
                                   to_country,
                                   rank,
                                   televote,
                                   event_type,
                                   year,
                                   running_order_position)))%>% 
              bind_rows(y_2016_final %>% 
                          rename(rank = 
                                   jury_rank,
                                 televote = 
                                   televote_rank) %>% 
                          select(c(from_country,
                                   to_country,
                                   rank,
                                   televote,
                                   event_type,
                                   year,
                                   running_order_position))) ) 


# do it from 2017 onwards
# define function

cleaning_2017_onwards <- 
  function(relevant_object){
    relevant_object%>% 
      rename(rank = 
               jury_rank,
             televote = 
               televoting_rank) %>% 
      select(from_country,
             to_country,
             rank,
             televote,
             event_type,
             year,
             running_order_position)
  }

# combine things
everything_2017_onwards <- 
  cleaning_2017_onwards(y_2017_semi_1) %>% 
  bind_rows(cleaning_2017_onwards(y_2017_semi_2)) %>% 
  bind_rows(cleaning_2017_onwards(y_2017_final))%>%
  bind_rows(cleaning_2017_onwards(y_2018_semi_1)) %>%
  bind_rows(cleaning_2017_onwards(y_2018_semi_2)) %>%
  bind_rows(cleaning_2017_onwards(y_2018_final))%>%
  bind_rows(cleaning_2017_onwards(y_2019_semi_1)) %>%
  bind_rows(cleaning_2017_onwards(y_2019_semi_2)) %>%
  bind_rows(cleaning_2017_onwards(y_2019_final))%>%
  bind_rows(cleaning_2017_onwards(y_2021_semi_1)) %>%
  bind_rows(cleaning_2017_onwards(y_2021_semi_2)) %>%
  bind_rows(cleaning_2017_onwards(y_2021_final))%>%
  bind_rows(cleaning_2017_onwards(y_2022_semi_1)) %>%
  bind_rows(cleaning_2017_onwards(y_2022_semi_2)) %>%
  bind_rows(cleaning_2017_onwards(y_2022_final)) %>% 
  filter(from_country != to_country)
# nb that final filter is just for the Wikipedia-pulled bits
# with the dodgy voting in 2022
# and Belarus in 2019

# create big object
all_rankings <- 
  everything_2017_onwards %>% 
  mutate(rank = 
           word(rank, -1)) %>% 
  mutate(televote = 
           word(televote, -1)) %>% 
  mutate(rank = 
           str_sub(rank, end = -3))%>% 
  mutate(televote = 
           str_sub(televote, end = -3)) %>% 
  mutate(rank = as.numeric(rank)) %>% 
  mutate(televote = 
           as.numeric(televote)) %>% 
  bind_rows(everything_2014_2016) %>% 
  mutate(event_type = 
           fct_relevel(event_type,
                       "semi-final 1",
                       "semi-final 2",
                       "final")) %>% 
  arrange(event_type, year) 

write_csv(all_rankings,
          "all_rankings.csv")
