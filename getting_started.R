# responding to this
# https://twitter.com/Bensvision/status/1635602528120233984

# let's have a go

# start with 2014-2016
# bc that's (relatively!) nicely formatted
# here
# https://eurovision.tv/history/full-split-results

# nb the 2017 data linked there
# is formatted v differently
# will have to pull it from the web instead

# for some reason it's not playing nicely
# so changing .xls to .csv first (in Excel)
# keeping .xlsx as is (so you can compare)

# load packages
library(tidyverse)
library(readxl)
library(janitor)
library(ggrepel)

# load data
y_2014_semi_1 <- 
  read_csv("ESC-2014-first_semi-final-full_results.csv",
             skip = 1,
           na  = "\n") %>% 
  clean_names()
y_2014_semi_2 <- 
  read_csv("ESC-2014-second_semi-final-full_results.csv",
           skip = 1,
           na  = "\n")%>% 
  clean_names()
y_2014_final <- 
  read_csv("ESC-2014-grand_final-full_results.csv",
           skip = 1,
           na  = "\n")%>% 
  clean_names()
y_2015_semi_1 <- 
  read_csv("ESC-2015-first_semi-final-full_results.csv",
           skip = 1,
           na  = "\n")%>% 
  clean_names()
y_2015_semi_2 <- 
  read_csv("ESC-2015-second_semi-final-full_results.csv",
           skip = 1,
           na  = "\n")%>% 
  clean_names()
y_2015_final <- 
  read_csv("ESC-2015-grand_final-full_results.csv",
           skip = 1,
           na  = "\n")%>% 
  clean_names()
# nb in this case there's a missing juror
y_2016_semi_1 <- 
  read_csv("ESC-2016-first_semi-final-full_results.csv",
           skip = 1,
           na  = c("\n",
                   "-"))%>% 
  clean_names()
y_2016_semi_2 <- 
  read_csv("ESC-2016-second_semi-final-full_results.csv",
           skip = 1,
           na  = "\n")%>% 
  clean_names()
y_2016_final <- 
  read_csv("ESC-2016-grand_final-full_results.csv",
           skip = 1,
           na  = "\n")%>% 
  clean_names()

# per above:
# 2017 is less beautifully formatted
# maybe it's on the eurovision website
# it is: here 
# https://eurovision.tv/event/kyiv-2017/first-semi-final/results
# but this is going to be a huge faff to convert

# get vector of countries in finals
finalists_2014 <- 
  y_2014_final %>% 
  select(to_country) %>% 
  distinct() %>% 
  pull()
finalists_2015 <- 
  y_2015_final %>% 
  select(to_country) %>% 
  distinct() %>% 
  pull()
finalists_2016 <- 
  y_2016_final %>% 
  select(to_country) %>% 
  distinct() %>% 
  pull()

# get rankings for each country pair
# just of those songs that qualified
# and rank from 1-10 (or 1-9 where applicable)
y_2014_semi_rankings <- 
  y_2014_semi_1 %>% 
  bind_rows(y_2014_semi_2) %>% 
  filter(!is.na(televote)) %>% 
  select(from_country,
         to_country,
         televote) %>% 
  filter(to_country %in% finalists_2014) %>% 
  group_by(from_country) %>% 
  mutate(televote_of_finalists = 
           rank(televote)) 

y_2015_semi_rankings <- 
  y_2015_semi_1 %>% 
  bind_rows(y_2015_semi_2) %>% 
  filter(!is.na(televote)) %>% 
  select(from_country,
         to_country,
         televote) %>% 
  filter(to_country %in% finalists_2015) %>% 
  group_by(from_country) %>% 
  mutate(televote_of_finalists = 
           rank(televote)) 
# names and formatting of the 2016 data are different
y_2016_semi_rankings <- 
  y_2016_semi_1 %>% 
  bind_rows(y_2016_semi_2) %>% 
  filter(!is.na(televote_rank)) %>% 
  select(from_country,
         to_country,
         televote_rank) %>% 
  filter(to_country %in% finalists_2016) %>% 
  group_by(from_country) %>% 
  mutate(televote_of_finalists = 
           rank(televote_rank)) 

# get semi pairs
# so we're comparing like with like
y_2014_semi_pairs <- 
  y_2014_semi_1 %>% 
  bind_rows(y_2014_semi_2) %>% 
  mutate(country_pair = 
           paste(from_country,
                 to_country)) %>% 
  pull(country_pair)
y_2015_semi_pairs <- 
  y_2015_semi_1 %>% 
  bind_rows(y_2015_semi_2) %>% 
  mutate(country_pair = 
           paste(from_country,
                 to_country)) %>% 
  pull(country_pair)
y_2016_semi_pairs <- 
  y_2016_semi_1 %>% 
  bind_rows(y_2016_semi_2) %>% 
  mutate(country_pair = 
           paste(from_country,
                 to_country)) %>% 
  pull(country_pair)

# have produced some running orders
# load them
y_2014_semi_1_ro <- 
  read_csv("y_2014_semi_1_ro.csv")
y_2014_semi_2_ro <- 
  read_csv("y_2014_semi_2_ro.csv")
y_2014_final_ro <- 
  read_csv("y_2014_final_ro.csv")
y_2015_semi_1_ro <- 
  read_csv("y_2015_semi_1_ro.csv")
y_2015_semi_2_ro <- 
  read_csv("y_2015_semi_2_ro.csv")
y_2015_final_ro <- 
  read_csv("y_2015_final_ro.csv")
y_2016_semi_1_ro <- 
  read_csv("y_2016_semi_1_ro.csv")
y_2016_semi_2_ro <- 
  read_csv("y_2016_semi_2_ro.csv")
y_2016_final_ro <- 
  read_csv("y_2016_final_ro.csv")


# retain this
# in case i crib any more wikipedia running orders
# 
# 
# data.frame(
#   stringsAsFactors = FALSE,
#        check.names = FALSE,
#          `R/O[65]` = c(1L,2L,3L,4L,5L,6L,7L,8L,
#                        9L,10L,11L,12L,13L,14L,15L,16L,17L,18L),
#            Country = c("Latvia","Poland",
#                        "Switzerland","Israel","Belarus","Serbia","Ireland","Macedonia",
#                        "Lithuania","Australia","Slovenia","Bulgaria",
#                        "Denmark","Ukraine","Norway","Georgia","Albania",
#                        "Belgium"),
#             Artist = c("Justs","Michał Szpak",
#                        "Rykka","Hovi Star","Ivan","Sanja Vučić Zaa","Nicky Byrne",
#                        "Kaliopi","Donny Montell","Dami Im","ManuElla",
#                        "Poli Genova","Lighthouse X","Jamala","Agnete",
#                        "Nika Kocharov and Young Georgian Lolitaz","Eneda Tarifa",
#                        "Laura Tesoro"),
#               Song = c("\"Heartbeat\"",
#                        "\"Color of Your Life\"","\"The Last of Our Kind\"",
#                        "\"Made of Stars\"","\"Help You Fly\"","\"Goodbye (Shelter)\"",
#                        "\"Sunlight\"","\"Dona\" (Дона)",
#                        "\"I've Been Waiting for This Night\"","\"Sound of Silence\"","\"Blue and Red\"",
#                        "\"If Love Was a Crime\"","\"Soldiers of Love\"",
#                        "\"1944\"","\"Icebreaker\"","\"Midnight Gold\"","\"Fairytale\"",
#                        "\"What's the Pressure\""),
#           Language = c("English","English","English",
#                        "English","English","English","English",
#                        "Macedonian","English","English","English","English, Bulgarian",
#                        "English","English, Crimean Tatar","English",
#                        "English","English","English"),
#             Points = c(132L,151L,28L,147L,84L,
#                        105L,46L,88L,222L,330L,57L,220L,34L,287L,63L,123L,
#                        45L,274L),
#        `Place[69]` = c(8L,6L,18L,7L,12L,10L,15L,
#                        11L,4L,1L,14L,5L,17L,2L,13L,9L,16L,3L)
# )%>% 
#   clean_names() %>% 
#   select(r_o_65,
#          country) %>% 
#   rename(running_order = 
#            r_o_65,
#          country = 
#            country) %>% 
#   select(running_order,
#          country) %>% 
#   write_csv("y_2016_semi_2_ro.csv")


# have a go at some quick analysis
# 2014 is a horrible case study
# bc UK wasn't in the semis
# 2015 is the same (Italy)
# 2016 is OK (Armenia)

# commented out so that i can use this script
# for something else
# 
# # look at San Marino (penultimate)
# # just to check what happens
# y_2014_final %>% 
#   filter(!is.na(televote)) %>% 
#   select(from_country, 
#          to_country,
#          televote) %>% 
#   mutate(country_pair = 
#            paste(from_country,
#                  to_country)) %>% 
#   filter(country_pair %in% y_2014_semi_pairs) %>%
#   group_by(from_country) %>% 
#   mutate(final_rank = 
#            rank(televote)) %>% 
#   select(-televote) %>% 
#   filter(to_country == "San Marino") %>%
#   full_join(y_2014_semi_rankings %>%
#               select(-televote),
#             by = c("from_country",
#                    "to_country")) %>%
#   na.omit %>% 
#   ggplot() +
#   aes(x = televote_of_finalists,
#       y = final_rank) + 
#   geom_point() + 
#   geom_abline(slope = 1, intercept = 0) + 
#   geom_label_repel(aes(label = 
#                          from_country)) + 
#   scale_x_reverse(limits = c(10, 1)) + 
#   scale_y_reverse(limits = c(10, 1)) +
#   labs(x = "San Marino's rank in the semi (of qualifiers)",
#        y = "San Marino's rank in the final\n (of the qualifiers the country could vote for in the semis)") + 
#   theme_minimal()
# 
# # do it with 2016
# # ok this works
# y_2016_final %>% 
#   filter(!is.na(televote_rank)) %>% 
#   select(from_country, 
#          to_country,
#          televote_rank) %>% 
#   mutate(country_pair = 
#            paste(from_country,
#                  to_country)) %>% 
#   filter(country_pair %in% y_2016_semi_pairs) %>%
#   group_by(from_country) %>% 
#   mutate(final_rank = 
#            rank(televote_rank)) %>% 
#   select(-televote_rank) %>% 
#   filter(to_country == "Armenia") %>%
#   full_join(y_2016_semi_rankings %>%
#               select(-televote_rank),
#             by = c("from_country",
#                    "to_country")) %>%
#   na.omit %>% 
#   ggplot() +
#   aes(x = televote_of_finalists,
#       y = final_rank) + 
#   geom_point() + 
#   geom_abline(slope = 1, intercept = 0) + 
#   geom_label_repel(aes(label = 
#                          from_country)) + 
#   scale_x_reverse(limits = c(10, 1)) + 
#   scale_y_reverse(limits = c(10, 1)) +
#   labs(x = "Armenia's rank in the 2016 semi (of qualifiers)",
#        y = "Armenia's rank in the 2016 final\n (of the qualifiers the country could vote for in the semis)") + 
#   theme_minimal()
