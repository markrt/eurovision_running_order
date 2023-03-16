# ok, i've done 2014-2016
# i'm basically happy with them
# 2017 onwards is harder
# let's go

# load packages, load data, etc
# run getting_started.R
# source("getting_started.R")

# load packages
library(rvest)
library(magrittr)
library(tidyverse)
library(janitor)

#### FINE
# i'll write a function
get_eurovision_page <- 
  function(eurovision_url,
           from_country_name){
    read_html(eurovision_url) %>% 
      html_table(fill = TRUE) %>% 
      extract2(1) %>% 
      clean_names() %>% 
      rename(to_country = 
               country) %>% 
      mutate(from_country = 
               from_country_name)
  }


# try an example page
# url_semi_1_2017_albania <- "https://eurovision.tv/event/kyiv-2017/first-semi-final/results/albania" 
# page_semi_1_2017_albania <- read_html(url_semi_1_2017_albania)
# table_semi_1_2017_albania <- html_table(page_semi_1_2017_albania, fill = TRUE)
# table_semi_1_2017_albania[[1]] %>% 
#   clean_names()


# speed it up
# # (it's all relative!)
# commented out so i don't hammer the website
# no i don't need to learn how to do loops
# shut up

# y_2017_semi_1 <- 
#   read_html("https://eurovision.tv/event/kyiv-2017/first-semi-final/results/albania" ) %>% 
#   html_table(fill = TRUE) %>% 
#   extract2(1) %>% 
#   clean_names() %>% 
#   rename(to_country = 
#            country) %>% 
#   mutate(from_country = 
#            "Albania") %>% 
#   bind_rows(read_html("https://eurovision.tv/event/kyiv-2017/first-semi-final/results/armenia" ) %>% 
#               html_table(fill = TRUE) %>% 
#               extract2(1) %>% 
#               clean_names() %>% 
#               rename(to_country = 
#                        country) %>% 
#               mutate(from_country = 
#                        "Armenia")) %>% 
#   bind_rows(read_html("https://eurovision.tv/event/kyiv-2017/first-semi-final/results/australia" ) %>% 
#               html_table(fill = TRUE) %>% 
#               extract2(1) %>% 
#               clean_names() %>% 
#               rename(to_country = 
#                        country) %>% 
#               mutate(from_country = 
#                        "Australia")) %>% 
#   bind_rows(read_html("https://eurovision.tv/event/kyiv-2017/first-semi-final/results/azerbaijan" ) %>% 
#               html_table(fill = TRUE) %>% 
#               extract2(1) %>% 
#               clean_names() %>% 
#               rename(to_country = 
#                        country) %>% 
#               mutate(from_country = 
#                        "Azerbaijan")) %>% 
#   bind_rows(read_html("https://eurovision.tv/event/kyiv-2017/first-semi-final/results/belgium" ) %>% 
#               html_table(fill = TRUE) %>% 
#               extract2(1) %>% 
#               clean_names() %>% 
#               rename(to_country = 
#                        country) %>% 
#               mutate(from_country = 
#                        "Belgium")) %>% 
#   bind_rows(read_html("https://eurovision.tv/event/kyiv-2017/first-semi-final/results/cyprus" ) %>% 
#               html_table(fill = TRUE) %>% 
#               extract2(1) %>% 
#               clean_names() %>% 
#               rename(to_country = 
#                        country) %>% 
#               mutate(from_country = 
#                        "Cyprus"))%>% 
#   bind_rows(read_html("https://eurovision.tv/event/kyiv-2017/first-semi-final/results/czechia" ) %>% 
#               html_table(fill = TRUE) %>% 
#               extract2(1) %>% 
#               clean_names() %>% 
#               rename(to_country = 
#                        country) %>% 
#               mutate(from_country = 
#                        "Czechia"))%>% 
#   bind_rows(read_html("https://eurovision.tv/event/kyiv-2017/first-semi-final/results/finland" ) %>% 
#               html_table(fill = TRUE) %>% 
#               extract2(1) %>% 
#               clean_names() %>% 
#               rename(to_country = 
#                        country) %>% 
#               mutate(from_country = 
#                        "Finland"))%>% 
#   bind_rows(read_html("https://eurovision.tv/event/kyiv-2017/first-semi-final/results/georgia" ) %>% 
#               html_table(fill = TRUE) %>% 
#               extract2(1) %>% 
#               clean_names() %>% 
#               rename(to_country = 
#                        country) %>% 
#               mutate(from_country = 
#                        "Georgia"))%>% 
#   bind_rows(read_html("https://eurovision.tv/event/kyiv-2017/first-semi-final/results/greece" ) %>% 
#               html_table(fill = TRUE) %>% 
#               extract2(1) %>% 
#               clean_names() %>% 
#               rename(to_country = 
#                        country) %>% 
#               mutate(from_country = 
#                        "Greece"))%>% 
#   bind_rows(read_html("https://eurovision.tv/event/kyiv-2017/first-semi-final/results/iceland" ) %>% 
#               html_table(fill = TRUE) %>% 
#               extract2(1) %>% 
#               clean_names() %>% 
#               rename(to_country = 
#                        country) %>% 
#               mutate(from_country = 
#                        "Iceland"))%>% 
#   bind_rows(read_html("https://eurovision.tv/event/kyiv-2017/first-semi-final/results/latvia" ) %>% 
#               html_table(fill = TRUE) %>% 
#               extract2(1) %>% 
#               clean_names() %>% 
#               rename(to_country = 
#                        country) %>% 
#               mutate(from_country = 
#                        "Latvia"))%>% 
#   bind_rows(read_html("https://eurovision.tv/event/kyiv-2017/first-semi-final/results/moldova" ) %>% 
#               html_table(fill = TRUE) %>% 
#               extract2(1) %>% 
#               clean_names() %>% 
#               rename(to_country = 
#                        country) %>% 
#               mutate(from_country = 
#                        "Moldova"))%>% 
#   bind_rows(read_html("https://eurovision.tv/event/kyiv-2017/first-semi-final/results/montenegro" ) %>% 
#               html_table(fill = TRUE) %>% 
#               extract2(1) %>% 
#               clean_names() %>% 
#               rename(to_country = 
#                        country) %>% 
#               mutate(from_country = 
#                        "Montenegro"))%>% 
#   bind_rows(read_html("https://eurovision.tv/event/kyiv-2017/first-semi-final/results/poland" ) %>% 
#               html_table(fill = TRUE) %>% 
#               extract2(1) %>% 
#               clean_names() %>% 
#               rename(to_country = 
#                        country) %>% 
#               mutate(from_country = 
#                        "Poland"))%>% 
#   bind_rows(read_html("https://eurovision.tv/event/kyiv-2017/first-semi-final/results/portugal" ) %>% 
#               html_table(fill = TRUE) %>% 
#               extract2(1) %>% 
#               clean_names() %>% 
#               rename(to_country = 
#                        country) %>% 
#               mutate(from_country = 
#                        "Portugal"))%>% 
#   bind_rows(read_html("https://eurovision.tv/event/kyiv-2017/first-semi-final/results/slovenia" ) %>% 
#               html_table(fill = TRUE) %>% 
#               extract2(1) %>% 
#               clean_names() %>% 
#               rename(to_country = 
#                        country) %>% 
#               mutate(from_country = 
#                        "Slovenia"))%>% 
#   bind_rows(read_html("https://eurovision.tv/event/kyiv-2017/first-semi-final/results/sweden" ) %>% 
#               html_table(fill = TRUE) %>% 
#               extract2(1) %>% 
#               clean_names() %>% 
#               rename(to_country = 
#                        country) %>% 
#               mutate(from_country = 
#                        "Sweden"))%>% 
#   bind_rows(read_html("https://eurovision.tv/event/kyiv-2017/first-semi-final/results/italy" ) %>% 
#               html_table(fill = TRUE) %>% 
#               extract2(1) %>% 
#               clean_names() %>% 
#               rename(to_country = 
#                        country) %>% 
#               mutate(from_country = 
#                        "Italy"))%>% 
#   bind_rows(read_html("https://eurovision.tv/event/kyiv-2017/first-semi-final/results/spain" ) %>% 
#               html_table(fill = TRUE) %>% 
#               extract2(1) %>% 
#               clean_names() %>% 
#               rename(to_country = 
#                        country) %>% 
#               mutate(from_country = 
#                        "Spain"))%>% 
#   bind_rows(read_html("https://eurovision.tv/event/kyiv-2017/first-semi-final/results/united-kingdom" ) %>% 
#               html_table(fill = TRUE) %>% 
#               extract2(1) %>% 
#               clean_names() %>% 
#               rename(to_country = 
#                        country) %>% 
#               mutate(from_country = 
#                        "United Kingdom"))
# export
# write_csv(y_2017_semi_1,
#           "y_2017_semi_1.csv")
# # 
# y_2017_semi_2 <-
#   read_html("https://eurovision.tv/event/kyiv-2017/second-semi-final/results/austria" ) %>%
#   html_table(fill = TRUE) %>%
#   extract2(1) %>%
#   clean_names() %>%
#   rename(to_country =
#            country) %>%
#   mutate(from_country =
#            "Austria") %>%
#   bind_rows(read_html("https://eurovision.tv/event/kyiv-2017/second-semi-final/results/belarus" ) %>%
#               html_table(fill = TRUE) %>%
#               extract2(1) %>%
#               clean_names() %>%
#               rename(to_country =
#                        country) %>%
#               mutate(from_country =
#                        "Belarus")) %>%
#   bind_rows(read_html("https://eurovision.tv/event/kyiv-2017/second-semi-final/results/bulgaria" ) %>%
#               html_table(fill = TRUE) %>%
#               extract2(1) %>%
#               clean_names() %>%
#               rename(to_country =
#                        country) %>%
#               mutate(from_country =
#                        "Bulgaria"))%>%
#   bind_rows(read_html("https://eurovision.tv/event/kyiv-2017/second-semi-final/results/croatia" ) %>%
#               html_table(fill = TRUE) %>%
#               extract2(1) %>%
#               clean_names() %>%
#               rename(to_country =
#                        country) %>%
#               mutate(from_country =
#                        "Croatia"))%>%
#   bind_rows(read_html("https://eurovision.tv/event/kyiv-2017/second-semi-final/results/denmark" ) %>%
#               html_table(fill = TRUE) %>%
#               extract2(1) %>%
#               clean_names() %>%
#               rename(to_country =
#                        country) %>%
#               mutate(from_country =
#                        "Denmark"))%>%
#   bind_rows(read_html("https://eurovision.tv/event/kyiv-2017/second-semi-final/results/estonia" ) %>%
#               html_table(fill = TRUE) %>%
#               extract2(1) %>%
#               clean_names() %>%
#               rename(to_country =
#                        country) %>%
#               mutate(from_country =
#                        "Estonia"))%>%
#   bind_rows(read_html("https://eurovision.tv/event/kyiv-2017/second-semi-final/results/france" ) %>%
#               html_table(fill = TRUE) %>%
#               extract2(1) %>%
#               clean_names() %>%
#               rename(to_country =
#                        country) %>%
#               mutate(from_country =
#                        "France"))%>%
#   bind_rows(read_html("https://eurovision.tv/event/kyiv-2017/second-semi-final/results/germany" ) %>%
#               html_table(fill = TRUE) %>%
#               extract2(1) %>%
#               clean_names() %>%
#               rename(to_country =
#                        country) %>%
#               mutate(from_country =
#                        "Germany"))%>%
#   bind_rows(read_html("https://eurovision.tv/event/kyiv-2017/second-semi-final/results/hungary" ) %>%
#               html_table(fill = TRUE) %>%
#               extract2(1) %>%
#               clean_names() %>%
#               rename(to_country =
#                        country) %>%
#               mutate(from_country =
#                        "Hungary"))%>%
#   bind_rows(read_html("https://eurovision.tv/event/kyiv-2017/second-semi-final/results/Ireland" ) %>%
#               html_table(fill = TRUE) %>%
#               extract2(1) %>%
#               clean_names() %>%
#               rename(to_country =
#                        country) %>%
#               mutate(from_country =
#                        "ireland"))%>%
#   bind_rows(read_html("https://eurovision.tv/event/kyiv-2017/second-semi-final/results/israel" ) %>%
#               html_table(fill = TRUE) %>%
#               extract2(1) %>%
#               clean_names() %>%
#               rename(to_country =
#                        country) %>%
#               mutate(from_country =
#                        "Israel"))%>%
#   bind_rows(read_html("https://eurovision.tv/event/kyiv-2017/second-semi-final/results/lithuania" ) %>%
#               html_table(fill = TRUE) %>%
#               extract2(1) %>%
#               clean_names() %>%
#               rename(to_country =
#                        country) %>%
#               mutate(from_country =
#                        "Lithuania"))%>%
#   bind_rows(read_html("https://eurovision.tv/event/kyiv-2017/second-semi-final/results/malta" ) %>%
#               html_table(fill = TRUE) %>%
#               extract2(1) %>%
#               clean_names() %>%
#               rename(to_country =
#                        country) %>%
#               mutate(from_country =
#                        "Malta"))%>%
#   bind_rows(read_html("https://eurovision.tv/event/kyiv-2017/second-semi-final/results/netherlands" ) %>%
#               html_table(fill = TRUE) %>%
#               extract2(1) %>%
#               clean_names() %>%
#               rename(to_country =
#                        country) %>%
#               mutate(from_country =
#                        "Netherlands"))%>%
#   bind_rows(read_html("https://eurovision.tv/event/kyiv-2017/second-semi-final/results/north-macedonia" ) %>%
#               html_table(fill = TRUE) %>%
#               extract2(1) %>%
#               clean_names() %>%
#               rename(to_country =
#                        country) %>%
#               mutate(from_country =
#                        "North Macedonia"))%>%
#   bind_rows(read_html("https://eurovision.tv/event/kyiv-2017/second-semi-final/results/norway" ) %>%
#               html_table(fill = TRUE) %>%
#               extract2(1) %>%
#               clean_names() %>%
#               rename(to_country =
#                        country) %>%
#               mutate(from_country =
#                        "Norway"))%>%
#   bind_rows(read_html("https://eurovision.tv/event/kyiv-2017/second-semi-final/results/romania" ) %>%
#               html_table(fill = TRUE) %>%
#               extract2(1) %>%
#               clean_names() %>%
#               rename(to_country =
#                        country) %>%
#               mutate(from_country =
#                        "Romania"))%>%
#   bind_rows(read_html("https://eurovision.tv/event/kyiv-2017/second-semi-final/results/san-marino" ) %>%
#               html_table(fill = TRUE) %>%
#               extract2(1) %>%
#               clean_names() %>%
#               rename(to_country =
#                        country) %>%
#               mutate(from_country =
#                        "San Marino"))%>%
#   bind_rows(read_html("https://eurovision.tv/event/kyiv-2017/second-semi-final/results/serbia" ) %>%
#               html_table(fill = TRUE) %>%
#               extract2(1) %>%
#               clean_names() %>%
#               rename(to_country =
#                        country) %>%
#               mutate(from_country =
#                        "Serbia"))%>%
#   bind_rows(read_html("https://eurovision.tv/event/kyiv-2017/second-semi-final/results/switzerland" ) %>%
#               html_table(fill = TRUE) %>%
#               extract2(1) %>%
#               clean_names() %>%
#               rename(to_country =
#                        country) %>%
#               mutate(from_country =
#                        "Switzerland"))%>%
#   bind_rows(read_html("https://eurovision.tv/event/kyiv-2017/second-semi-final/results/ukraine" ) %>%
#               html_table(fill = TRUE) %>%
#               extract2(1) %>%
#               clean_names() %>%
#               rename(to_country =
#                        country) %>%
#               mutate(from_country =
#                        "Ukraine"))
# 
# write_csv(y_2017_semi_2,
#           "y_2017_semi_2.csv")
# 
# y_2017_final <-
#   get_eurovision_page("https://eurovision.tv/event/kyiv-2017/grand-final/results/albania",
#                       "Albania") %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/kyiv-2017/grand-final/results/armenia",
#                                 "Armenia")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/kyiv-2017/grand-final/results/australia",
#                                 "Australia")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/kyiv-2017/grand-final/results/austria",
#                                 "Austria")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/kyiv-2017/grand-final/results/azerbaijan",
#                                 "Azerbaijan")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/kyiv-2017/grand-final/results/belarus",
#                                 "Belarus")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/kyiv-2017/grand-final/results/belgium",
#                                 "Belgium")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/kyiv-2017/grand-final/results/bulgaria",
#                                 "Bulgaria")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/kyiv-2017/grand-final/results/croatia",
#                                 "Croatia")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/kyiv-2017/grand-final/results/cyprus",
#                                 "Cyprus")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/kyiv-2017/grand-final/results/czechia",
#                                 "Czechia")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/kyiv-2017/grand-final/results/denmark",
#                                 "Denmark")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/kyiv-2017/grand-final/results/estonia",
#                                 "Estonia")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/kyiv-2017/grand-final/results/finland",
#                                 "Finland")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/kyiv-2017/grand-final/results/france",
#                                 "France")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/kyiv-2017/grand-final/results/georgia",
#                                 "Georgia")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/kyiv-2017/grand-final/results/germany",
#                                 "Germany")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/kyiv-2017/grand-final/results/greece",
#                                 "Greece")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/kyiv-2017/grand-final/results/hungary",
#                                 "Hungary")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/kyiv-2017/grand-final/results/iceland",
#                                 "Iceland")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/kyiv-2017/grand-final/results/ireland",
#                                 "Ireland")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/kyiv-2017/grand-final/results/israel",
#                                 "Israel")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/kyiv-2017/grand-final/results/italy",
#                                 "Italy")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/kyiv-2017/grand-final/results/latvia",
#                                 "Latvia")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/kyiv-2017/grand-final/results/lithuania",
#                                 "Lithuania")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/kyiv-2017/grand-final/results/malta",
#                                 "Malta")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/kyiv-2017/grand-final/results/moldova",
#                                 "Moldova")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/kyiv-2017/grand-final/results/montenegro",
#                                 "Montenegro")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/kyiv-2017/grand-final/results/netherlands",
#                                 "Netherlands")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/kyiv-2017/grand-final/results/north-macedonia",
#                                 "North Macedonia")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/kyiv-2017/grand-final/results/norway",
#                                 "Norway")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/kyiv-2017/grand-final/results/poland",
#                                 "Poland")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/kyiv-2017/grand-final/results/portugal",
#                                 "Portugal")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/kyiv-2017/grand-final/results/romania",
#                                 "Romania")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/kyiv-2017/grand-final/results/san-marino",
#                                 "San Marino")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/kyiv-2017/grand-final/results/serbia",
#                                 "Serbia")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/kyiv-2017/grand-final/results/slovenia",
#                                 "Slovenia")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/kyiv-2017/grand-final/results/spain",
#                                 "Spain")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/kyiv-2017/grand-final/results/sweden",
#                                 "Sweden")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/kyiv-2017/grand-final/results/switzerland",
#                                 "Switzerland")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/kyiv-2017/grand-final/results/ukraine",
#                                 "Ukraine")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/kyiv-2017/grand-final/results/united-kingdom",
#                                 "United Kingdom"))
# 
# write_csv(y_2017_final,
#           "y_2017_final.csv")

# y_2018_semi_1 <- 
#   get_eurovision_page("https://eurovision.tv/event/lisbon-2018/first-semi-final/results/albania",
#                       "Albania") %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/first-semi-final/results/armenia",
#                                 "Armenia")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/first-semi-final/results/azerbaijan",
#                                 "Azerbaijan")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/first-semi-final/results/belarus",
#                                 "Belarus")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/first-semi-final/results/belgium",
#                                 "Belgium")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/first-semi-final/results/bulgaria",
#                                 "Bulgaria")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/first-semi-final/results/croatia",
#                                 "Croatia")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/first-semi-final/results/cyprus",
#                                 "Cyprus")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/first-semi-final/results/czechia",
#                                 "Czechia")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/first-semi-final/results/estonia",
#                                 "Estonia")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/first-semi-final/results/finland",
#                                 "Finland")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/first-semi-final/results/greece",
#                                 "Greece")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/first-semi-final/results/iceland",
#                                 "Iceland")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/first-semi-final/results/ireland",
#                                 "Ireland")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/first-semi-final/results/israel",
#                                 "Israel")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/first-semi-final/results/lithuania",
#                                 "Lithuania")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/first-semi-final/results/north-macedonia",
#                                 "North Macedonia")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/first-semi-final/results/portugal",
#                                 "Portugal")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/first-semi-final/results/spain",
#                                 "Spain")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/first-semi-final/results/switzerland",
#                                 "Switzerland")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/first-semi-final/results/united-kingdom",
#                                 "United Kingdom")) 
# 
# write_csv(y_2018_semi_1,
#           "y_2018_semi_1.csv")
# 
# y_2018_semi_2 <- 
#   get_eurovision_page("https://eurovision.tv/event/lisbon-2018/second-semi-final/results/australia",
#                       "Australia") %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/second-semi-final/results/denmark",
#                                 "Denmark")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/second-semi-final/results/france",
#                                 "France")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/second-semi-final/results/georgia",
#                                 "Georgia")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/second-semi-final/results/germany",
#                                 "Germany")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/second-semi-final/results/hungary",
#                                 "Hungary")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/second-semi-final/results/italy",
#                                 "Italy")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/second-semi-final/results/latvia",
#                                 "Latvia")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/second-semi-final/results/malta",
#                                 "Malta")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/second-semi-final/results/moldova",
#                                 "Moldova")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/second-semi-final/results/montenegro",
#                                 "Montenegro")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/second-semi-final/results/netherlands",
#                                 "Netherlands")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/second-semi-final/results/norway",
#                                 "Norway")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/second-semi-final/results/poland",
#                                 "Poland")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/second-semi-final/results/romania",
#                                 "Romania")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/second-semi-final/results/russia",
#                                 "Russia")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/second-semi-final/results/san-marino",
#                                 "San Marino")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/second-semi-final/results/serbia",
#                                 "Serbia")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/second-semi-final/results/slovenia",
#                                 "Slovenia")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/second-semi-final/results/sweden",
#                                 "Sweden")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/second-semi-final/results/ukraine",
#                                 "Ukraine")) 
# write_csv(y_2018_semi_2,
#           "y_2018_semi_2.csv")
# 
# y_2018_final <- 
#   get_eurovision_page("https://eurovision.tv/event/lisbon-2018/grand-final/results/albania",
#                       "Albania") %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/grand-final/results/armenia",
#                                 "Armenia")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/grand-final/results/australia",
#                                 "Australia")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/grand-final/results/austria",
#                                 "Austria")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/grand-final/results/azerbaijan",
#                                 "Azerbaijan")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/grand-final/results/belarus",
#                                 "Belarus")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/grand-final/results/belgium",
#                                 "Belgium")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/grand-final/results/bulgaria",
#                                 "Bulgaria")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/grand-final/results/croatia",
#                                 "Croatia")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/grand-final/results/cyprus",
#                                 "Cyprus")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/grand-final/results/czechia",
#                                 "Czechia")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/grand-final/results/denmark",
#                                 "Denmark")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/grand-final/results/estonia",
#                                 "Estonia")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/grand-final/results/finland",
#                                 "Finland")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/grand-final/results/france",
#                                 "France")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/grand-final/results/georgia",
#                                 "Georgia")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/grand-final/results/germany",
#                                 "Germany")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/grand-final/results/greece",
#                                 "Greece")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/grand-final/results/hungary",
#                                 "Hungary")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/grand-final/results/iceland",
#                                 "Iceland")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/grand-final/results/ireland",
#                                 "Ireland")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/grand-final/results/israel",
#                                 "Israel")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/grand-final/results/italy",
#                                 "Italy")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/grand-final/results/latvia",
#                                 "Latvia")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/grand-final/results/lithuania",
#                                 "Lithuania")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/grand-final/results/malta",
#                                 "Malta")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/grand-final/results/moldova",
#                                 "Moldova")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/grand-final/results/montenegro",
#                                 "Montenegro")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/grand-final/results/netherlands",
#                                 "Netherlands")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/grand-final/results/north-macedonia",
#                                 "North Macedonia")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/grand-final/results/norway",
#                                 "Norway")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/grand-final/results/poland",
#                                 "Poland")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/grand-final/results/portugal",
#                                 "Portugal")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/grand-final/results/romania",
#                                 "Romania")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/grand-final/results/russia",
#                                 "Russia")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/grand-final/results/san-marino",
#                                 "San Marino")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/grand-final/results/serbia",
#                                 "Serbia")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/grand-final/results/slovenia",
#                                 "Slovenia")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/grand-final/results/spain",
#                                 "Spain")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/grand-final/results/sweden",
#                                 "Sweden")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/grand-final/results/switzerland",
#                                 "Switzerland")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/grand-final/results/ukraine",
#                                 "Ukraine")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/lisbon-2018/grand-final/results/united-kingdom",
#                                 "United Kingdom")) 
# write_csv(y_2018_final,
#           "y_2018_final.csv")
# 
# y_2019_semi_1 <- 
#   get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/first-semi-final/results/australia",
#                       "Australia") %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/first-semi-final/results/belarus",
#                                 "Belarus")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/first-semi-final/results/belgium",
#                                 "Belgium")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/first-semi-final/results/cyprus",
#                                 "Cyprus")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/first-semi-final/results/czechia",
#                                 "Czechia")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/first-semi-final/results/estonia",
#                                 "Estonia")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/first-semi-final/results/finland",
#                                 "Finland")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/first-semi-final/results/france",
#                                 "France")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/first-semi-final/results/georgia",
#                                 "Georgia")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/first-semi-final/results/greece",
#                                 "Greece")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/first-semi-final/results/hungary",
#                                 "Hungary")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/first-semi-final/results/iceland",
#                                 "Iceland")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/first-semi-final/results/israel",
#                                 "Israel")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/first-semi-final/results/montenegro",
#                                 "Montenegro")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/first-semi-final/results/poland",
#                                 "Poland")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/first-semi-final/results/portugal",
#                                 "Portugal")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/first-semi-final/results/san-marino",
#                                 "San Marino")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/first-semi-final/results/serbia",
#                                 "Serbia")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/first-semi-final/results/slovenia",
#                                 "Slovenia")) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/first-semi-final/results/spain",
#                                 "Spain")) 
# write_csv(y_2019_semi_1,
#           "y_2019_semi_1.csv")
# 
# y_2019_semi_2 <- 
#   get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/second-semi-final/results/albania",
#                       "Albania") %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/second-semi-final/results/armenia",
#                                 "Armenia") ) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/second-semi-final/results/austria",
#                                 "Austria") ) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/second-semi-final/results/azerbaijan",
#                                 "Azerbaijan") ) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/second-semi-final/results/croatia",
#                                 "Croatia") ) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/second-semi-final/results/denmark",
#                                 "Denmark") ) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/second-semi-final/results/germany",
#                                 "Germany") ) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/second-semi-final/results/ireland",
#                                 "Ireland") ) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/second-semi-final/results/italy",
#                                 "Italy") ) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/second-semi-final/results/latvia",
#                                 "Latvia") ) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/second-semi-final/results/lithuania",
#                                 "Lithuania") ) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/second-semi-final/results/malta",
#                                 "Malta") ) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/second-semi-final/results/moldova",
#                                 "Moldova") ) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/second-semi-final/results/netherlands",
#                                 "Netherlands") ) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/second-semi-final/results/north-macedonia",
#                                 "North Macedonia") ) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/second-semi-final/results/norway",
#                                 "Norway") ) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/second-semi-final/results/romania",
#                                 "Romania") ) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/second-semi-final/results/russia",
#                                 "Russia") ) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/second-semi-final/results/sweden",
#                                 "Sweden") ) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/second-semi-final/results/switzerland",
#                                 "Switzerland") ) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/second-semi-final/results/united-kingdom",
#                                 "United Kingdom") ) 
# 
# write_csv(y_2019_semi_2,
#           "y_2019_semi_2.csv")


# 
# 
# # Belarus is an issue here
# y_2019_final <-
#   get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/grand-final/results/albania",
#                       "Albania") %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/grand-final/results/armenia",
#                                 "Armenia")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/grand-final/results/australia",
#                                 "Australia")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/grand-final/results/austria",
#                                 "Austria")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/grand-final/results/azerbaijan",
#                                 "Azerbaijan")) %>%
#   # bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/grand-final/results/belarus",
#   #                               "Belarus")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/grand-final/results/belgium",
#                                 "Belgium")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/grand-final/results/croatia",
#                                 "Croatia")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/grand-final/results/cyprus",
#                                 "Cyprus")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/grand-final/results/czechia",
#                                 "Czechia")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/grand-final/results/denmark",
#                                 "Denmark")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/grand-final/results/estonia",
#                                 "Estonia")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/grand-final/results/finland",
#                                 "Finland")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/grand-final/results/france",
#                                 "France")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/grand-final/results/georgia",
#                                 "Georgia")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/grand-final/results/germany",
#                                 "Germany")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/grand-final/results/greece",
#                                 "Greece")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/grand-final/results/hungary",
#                                 "Hungary")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/grand-final/results/iceland",
#                                 "Iceland")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/grand-final/results/ireland",
#                                 "Ireland")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/grand-final/results/israel",
#                                 "Israel")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/grand-final/results/italy",
#                                 "Italy")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/grand-final/results/latvia",
#                                 "Latvia")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/grand-final/results/lithuania",
#                                 "Lithuania")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/grand-final/results/malta",
#                                 "Malta")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/grand-final/results/moldova",
#                                 "Moldova")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/grand-final/results/montenegro",
#                                 "Montenegro")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/grand-final/results/netherlands",
#                                 "Netherlands")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/grand-final/results/north-macedonia",
#                                 "North Macedonia")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/grand-final/results/norway",
#                                 "Norway")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/grand-final/results/poland",
#                                 "Poland")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/grand-final/results/portugal",
#                                 "Portugal")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/grand-final/results/romania",
#                                 "Romania")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/grand-final/results/russia",
#                                 "Russia")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/grand-final/results/san-marino",
#                                 "San Marino")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/grand-final/results/serbia",
#                                 "Serbia")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/grand-final/results/slovenia",
#                                 "Slovenia")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/grand-final/results/spain",
#                                 "Spain")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/grand-final/results/sweden",
#                                 "Sweden")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/grand-final/results/switzerland",
#                                 "Switzerland")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/grand-final/results/ukraine",
#                                 "Ukraine")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/grand-final/results/united-kingdom",
#                                 "United Kingdom"))
# 
# write_csv(y_2019_final,
#           "y_2019_final.csv")
# 
# # add Belarus
# belarus_2019 <- 
#   read_html("https://en.wikipedia.org/wiki/Belarus_in_the_Eurovision_Song_Contest_2019" ) %>%
#   html_table(fill = TRUE) %>%
#   extract2(12) %>%
#   clean_names() %>%
#   filter(draw != "Draw") %>% 
#   print(n = 26) %>% 
#   select(country, jury, televote) 
# 
# # export with Belarus
# y_2019_final
# belarus_2019 %>% 
#   rename(to_country = 
#            country,
#          jury_rank = 
#            jury,
#          televoting_rank = 
#            televote) %>% 
#   mutate(juror = NA,
#          a = NA,
#          b = NA,
#          c = NA,
#          d = NA,
#          e = NA,
#          from_country = "Belarus") %>% 
#   relocate(to_country,
#            juror,
#            a,
#            b,
#            c,
#            d,
#            e,
#            jury_rank,
#            televoting_rank,
#            from_country) %>% 
#   bind_rows(y_2019_final) %>% 
#   write_csv("y_2019_final_WITH_BELARUS.csv")

# 
# y_2021_semi_1 <-
#   get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/first-semi-final/results/australia",
#                       "Australia") %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/first-semi-final/results/azerbaijan",
#                                 "Azerbaijan")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/first-semi-final/results/belgium",
#                                 "Belgium")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/first-semi-final/results/croatia",
#                                 "Croatia")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/first-semi-final/results/cyprus",
#                                 "Cyprus")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/first-semi-final/results/germany",
#                                 "Germany")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/first-semi-final/results/ireland",
#                                 "Ireland")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/first-semi-final/results/israel",
#                                 "Israel")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/first-semi-final/results/italy",
#                                 "Italy")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/first-semi-final/results/lithuania",
#                                 "Lithuania")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/first-semi-final/results/malta",
#                                 "Malta")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/first-semi-final/results/netherlands",
#                                 "Netherlands")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/first-semi-final/results/north-macedonia",
#                                 "North Macedonia")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/first-semi-final/results/norway",
#                                 "Norway")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/first-semi-final/results/romania",
#                                 "Romania")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/first-semi-final/results/russia",
#                                 "Russia")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/first-semi-final/results/slovenia",
#                                 "Slovenia")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/first-semi-final/results/sweden",
#                                 "Sweden")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/first-semi-final/results/ukraine",
#                                 "Ukraine"))
# 
# write_csv(y_2021_semi_1,
#           "y_2021_semi_1.csv")
# # 
# #
# y_2021_semi_2 <-
#   get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/second-semi-final/results/albania",
#                       "Albania") %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/second-semi-final/results/austria",
#                                 "Austria")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/second-semi-final/results/bulgaria",
#                                 "Bulgaria")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/second-semi-final/results/czechia",
#                                 "Czechia")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/second-semi-final/results/denmark",
#                                 "Denmark")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/second-semi-final/results/estonia",
#                                 "Estonia")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/second-semi-final/results/finland",
#                                 "Finland")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/second-semi-final/results/france",
#                                 "France")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/second-semi-final/results/georgia",
#                                 "Georgia")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/second-semi-final/results/greece",
#                                 "Greece")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/second-semi-final/results/iceland",
#                                 "Iceland")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/second-semi-final/results/latvia",
#                                 "Latvia")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/second-semi-final/results/moldova",
#                                 "Moldova")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/second-semi-final/results/poland",
#                                 "Poland")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/second-semi-final/results/portugal",
#                                 "Portugal")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/second-semi-final/results/san-marino",
#                                 "San Marino")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/second-semi-final/results/serbia",
#                                 "Serbia")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/second-semi-final/results/spain",
#                                 "Spain")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/second-semi-final/results/switzerland",
#                                 "Switzerland")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/second-semi-final/results/united-kingdom",
#                                 "United Kingdom"))
# write_csv(y_2021_semi_2,
#           "y_2021_semi_2.csv")
# 
# 
# y_2021_final <-
#   get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/grand-final/results/albania",
#                       "Albania") %>%
#   # bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/grand-final/results/armenia",
#   #                               "Armenia")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/grand-final/results/australia",
#                                 "Australia")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/grand-final/results/austria",
#                                 "Austria")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/grand-final/results/azerbaijan",
#                                 "Azerbaijan")) %>%
#   # bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/grand-final/results/belarus",
#   #                               "Belarus")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/grand-final/results/belgium",
#                                 "Belgium")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/grand-final/results/bulgaria",
#                                 "Bulgaria")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/grand-final/results/croatia",
#                                 "Croatia")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/grand-final/results/cyprus",
#                                 "Cyprus")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/grand-final/results/czechia",
#                                 "Czechia")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/grand-final/results/denmark",
#                                 "Denmark")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/grand-final/results/estonia",
#                                 "Estonia")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/grand-final/results/finland",
#                                 "Finland")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/grand-final/results/france",
#                                 "France")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/grand-final/results/georgia",
#                                 "Georgia")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/grand-final/results/germany",
#                                 "Germany")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/grand-final/results/greece",
#                                 "Greece")) %>%
#   # bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/grand-final/results/hungary",
#   #                               "Hungary")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/grand-final/results/iceland",
#                                 "Iceland")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/grand-final/results/ireland",
#                                 "Ireland")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/grand-final/results/israel",
#                                 "Israel")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/grand-final/results/italy",
#                                 "Italy")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/grand-final/results/latvia",
#                                 "Latvia")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/grand-final/results/lithuania",
#                                 "Lithuania")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/grand-final/results/malta",
#                                 "Malta")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/grand-final/results/moldova",
#                                 "Moldova")) %>%
#   # bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/grand-final/results/montenegro",
#   #                               "Montenegro")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/grand-final/results/netherlands",
#                                 "Netherlands")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/grand-final/results/north-macedonia",
#                                 "North Macedonia")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/grand-final/results/norway",
#                                 "Norway")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/grand-final/results/poland",
#                                 "Poland")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/grand-final/results/portugal",
#                                 "Portugal")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/grand-final/results/romania",
#                                 "Romania")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/grand-final/results/russia",
#                                 "Russia")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/grand-final/results/san-marino",
#                                 "San Marino")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/grand-final/results/serbia",
#                                 "Serbia")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/grand-final/results/slovenia",
#                                 "Slovenia")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/grand-final/results/spain",
#                                 "Spain")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/grand-final/results/sweden",
#                                 "Sweden")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/grand-final/results/switzerland",
#                                 "Switzerland")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/grand-final/results/ukraine",
#                                 "Ukraine")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/grand-final/results/united-kingdom",
#                                 "United Kingdom"))
# 
# write_csv(y_2021_final,
#           "y_2021_final.csv")

#
# y_2022_semi_1 <-
#   get_eurovision_page("https://eurovision.tv/event/turin-2022/first-semi-final/results/albania",
#                       "Albania") %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/first-semi-final/results/armenia",
#                                 "Armenia")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/first-semi-final/results/austria",
#                                 "Austria")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/first-semi-final/results/bulgaria",
#                                 "Bulgaria")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/first-semi-final/results/croatia",
#                                 "Croatia")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/first-semi-final/results/denmark",
#                                 "Denmark")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/first-semi-final/results/france",
#                                 "France")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/first-semi-final/results/greece",
#                                 "Greece")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/first-semi-final/results/iceland",
#                                 "Iceland")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/first-semi-final/results/italy",
#                                 "Italy")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/first-semi-final/results/latvia",
#                                 "Latvia")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/first-semi-final/results/lithuania",
#                                 "Lithuania")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/first-semi-final/results/moldova",
#                                 "Moldova")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/first-semi-final/results/netherlands",
#                                 "Netherlands")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/first-semi-final/results/norway",
#                                 "Norway")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/first-semi-final/results/portugal",
#                                 "Portugal")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/first-semi-final/results/slovenia",
#                                 "Slovenia")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/first-semi-final/results/switzerland",
#                                 "Switzerland")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/first-semi-final/results/ukraine",
#                                 "Ukraine"))
# write_csv(y_2022_semi_1,
#           "y_2022_semi_1.csv")

# # lots of issues here
# y_2022_semi_2 <- 
#   get_eurovision_page("https://eurovision.tv/event/turin-2022/second-semi-final/results/australia",
#                     "Australia") %>% 
#   # bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/second-semi-final/results/azerbaijan",
#   #                               "Azerbaijan") ) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/second-semi-final/results/belgium",
#                                 "Belgium") ) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/second-semi-final/results/cyprus",
#                                 "Cyprus") ) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/second-semi-final/results/czechia",
#                                 "Czechia") ) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/second-semi-final/results/estonia",
#                                 "Estonia") ) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/second-semi-final/results/finland",
#                                 "Finland") ) %>% 
#   # bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/second-semi-final/results/georgia",
#   #                               "Georgia") ) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/second-semi-final/results/germany",
#                                 "Germany") ) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/second-semi-final/results/ireland",
#                                 "Ireland") ) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/second-semi-final/results/israel",
#                                 "Israel") ) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/second-semi-final/results/malta",
#                                 "Malta") ) %>% 
#   # bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/second-semi-final/results/montenegro",
#   #                               "Montenegro") ) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/second-semi-final/results/north-macedonia",
#                                 "North Macedonia") ) %>% 
#   # bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/second-semi-final/results/poland",
#   #                               "Poland") ) %>% 
#   # bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/second-semi-final/results/romania",
#   #                               "Romania") ) %>% 
#   # bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/second-semi-final/results/san-marino",
#   #                               "San Marino") ) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/second-semi-final/results/serbia",
#                                 "Serbia") ) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/second-semi-final/results/spain",
#                                 "Spain") ) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/second-semi-final/results/sweden",
#                                 "Sweden") ) %>% 
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/second-semi-final/results/united-kingdom",
#                                 "United Kingdom") )  
# write_csv(y_2022_semi_2,
#           "y_2022_semi_2.csv")

# get the extras
# 
# extras_2022_semi_2 <-
#   read_html("https://en.wikipedia.org/wiki/Azerbaijan_in_the_Eurovision_Song_Contest_2022" ) %>%
#   html_table(fill = TRUE) %>%
#   extract2(12) %>%
#   clean_names() %>%
#   filter(draw != "Draw") %>%
#   select(country, jury, televote) %>%
#   mutate(from_country = "Azerbaijan") %>%
#   bind_rows(read_html("https://en.wikipedia.org/wiki/Georgia_in_the_Eurovision_Song_Contest_2022" ) %>%
#               html_table(fill = TRUE) %>%
#               extract2(9) %>%
#               clean_names() %>%
#               filter(draw != "Draw") %>%
#               select(country, jury, televote) %>%
#               mutate(from_country = "Georgia"))%>%
#   bind_rows(read_html("https://en.wikipedia.org/wiki/Montenegro_in_the_Eurovision_Song_Contest_2022" ) %>%
#               html_table(fill = TRUE) %>%
#               extract2(9) %>%
#               clean_names() %>%
#               filter(draw != "Draw") %>%
#               select(country, jury, televote) %>%
#               mutate(from_country = "Montenegro"))%>%
#   bind_rows(read_html("https://en.wikipedia.org/wiki/Poland_in_the_Eurovision_Song_Contest_2022" ) %>%
#               html_table(fill = TRUE) %>%
#               extract2(15) %>%
#               clean_names() %>%
#               filter(draw != "Draw") %>%
#               select(country, jury, televote) %>%
#               mutate(from_country = "Poland"))%>%
#   bind_rows(read_html("https://en.wikipedia.org/wiki/Romania_in_the_Eurovision_Song_Contest_2022" ) %>%
#               html_table(fill = TRUE) %>%
#               extract2(20) %>%
#               clean_names() %>%
#               filter(draw != "Draw") %>%
#               select(country, jury, televote) %>%
#               mutate(from_country = "Romania"))%>%
#   bind_rows(read_html("https://en.wikipedia.org/wiki/San_Marino_in_the_Eurovision_Song_Contest_2022" ) %>%
#               html_table(fill = TRUE) %>%
#               extract2(17) %>%
#               clean_names() %>%
#               filter(draw != "Draw") %>%
#               select(country, jury, televote) %>%
#               mutate(from_country = "San Marino"))
# 
# extras_2022_semi_2%>%
#   rename(to_country =
#            country,
#          jury_rank =
#            jury,
#          televoting_rank =
#            televote) %>%
#   mutate(juror = NA,
#          a = NA,
#          b = NA,
#          c = NA,
#          d = NA,
#          e = NA) %>%
#   relocate(to_country,
#            juror,
#            a,
#            b,
#            c,
#            d,
#            e,
#            jury_rank,
#            televoting_rank,
#            from_country) %>%
#   bind_rows(y_2022_semi_2) %>%
#   write_csv("y_2022_semi_2_WITH_EXTRAS.csv")

# final
# # 
# y_2022_final <-
#   get_eurovision_page("https://eurovision.tv/event/turin-2022/grand-final/results/albania",
#                       "Albania") %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/grand-final/results/armenia",
#                                 "Armenia")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/grand-final/results/australia",
#                                 "Australia")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/grand-final/results/austria",
#                                 "Austria")) %>%
#   # bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/grand-final/results/azerbaijan",
#   #                               "Azerbaijan")) %>%
#   # bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/grand-final/results/belarus",
#   #                               "Belarus")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/grand-final/results/belgium",
#                                 "Belgium")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/grand-final/results/bulgaria",
#                                 "Bulgaria")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/grand-final/results/croatia",
#                                 "Croatia")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/grand-final/results/cyprus",
#                                 "Cyprus")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/grand-final/results/czechia",
#                                 "Czechia")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/grand-final/results/denmark",
#                                 "Denmark")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/grand-final/results/estonia",
#                                 "Estonia")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/grand-final/results/finland",
#                                 "Finland")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/grand-final/results/france",
#                                 "France")) %>%
#   # bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/grand-final/results/georgia",
#   #                               "Georgia")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/grand-final/results/germany",
#                                 "Germany")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/grand-final/results/greece",
#                                 "Greece")) %>%
#   # bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/grand-final/results/hungary",
#   #                               "Hungary")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/grand-final/results/iceland",
#                                 "Iceland")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/grand-final/results/ireland",
#                                 "Ireland")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/grand-final/results/israel",
#                                 "Israel")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/grand-final/results/italy",
#                                 "Italy")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/grand-final/results/latvia",
#                                 "Latvia")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/grand-final/results/lithuania",
#                                 "Lithuania")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/grand-final/results/malta",
#                                 "Malta")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/grand-final/results/moldova",
#                                 "Moldova")) %>%
#   # bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/grand-final/results/montenegro",
#   #                               "Montenegro")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/grand-final/results/netherlands",
#                                 "Netherlands")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/grand-final/results/north-macedonia",
#                                 "North Macedonia")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/grand-final/results/norway",
#                                 "Norway")) %>%
#   # bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/grand-final/results/poland",
#   #                               "Poland")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/grand-final/results/portugal",
#                                 "Portugal")) %>%
#   # bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/grand-final/results/romania",
#   #                               "Romania")) %>%
#   # bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/grand-final/results/russia",
#   #                               "Russia")) %>%
#   # bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/grand-final/results/san-marino",
#   #                               "San Marino")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/grand-final/results/serbia",
#                                 "Serbia")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/grand-final/results/slovenia",
#                                 "Slovenia")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/grand-final/results/spain",
#                                 "Spain")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/grand-final/results/sweden",
#                                 "Sweden")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/grand-final/results/switzerland",
#                                 "Switzerland")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/grand-final/results/ukraine",
#                                 "Ukraine")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/turin-2022/grand-final/results/united-kingdom",
#                                 "United Kingdom"))
# # 
# write_csv(y_2022_final,
#           "y_2022_final.csv")


# get the extras
# only have them for the qualifiers???
# tricky to work with
# 
# extras_2022_final <-
#   read_html("https://en.wikipedia.org/wiki/Azerbaijan_in_the_Eurovision_Song_Contest_2022" ) %>%
#   html_table(fill = TRUE) %>%
#   extract2(13) %>%
#   clean_names() %>%
#   filter(draw != "Draw") %>%
#   select(country, jury, televote) %>%
#   mutate(from_country = "Azerbaijan") %>%
#   # bind_rows(read_html("https://en.wikipedia.org/wiki/Georgia_in_the_Eurovision_Song_Contest_2022" ) %>%
#   #             html_table(fill = TRUE) %>%
#   #             extract2(9) %>%
#   #             clean_names() %>%
#   #             filter(draw != "Draw") %>%
#   #             select(country, jury, televote) %>%
#   #             mutate(from_country = "Georgia"))%>%
#   # bind_rows(read_html("https://en.wikipedia.org/wiki/Montenegro_in_the_Eurovision_Song_Contest_2022" ) %>%
#   #             html_table(fill = TRUE) %>%
#   #             extract2(9) %>%
#   #             clean_names() %>%
#   #             filter(draw != "Draw") %>%
#   #             select(country, jury, televote) %>%
#   #             mutate(from_country = "Montenegro"))%>%
#   bind_rows(read_html("https://en.wikipedia.org/wiki/Poland_in_the_Eurovision_Song_Contest_2022" ) %>%
#               html_table(fill = TRUE) %>%
#               extract2(16) %>%
#               clean_names() %>%
#               filter(draw != "Draw") %>%
#               select(country, jury, televote) %>%
#               mutate(from_country = "Poland"))%>%
#   bind_rows(read_html("https://en.wikipedia.org/wiki/Romania_in_the_Eurovision_Song_Contest_2022" ) %>%
#               html_table(fill = TRUE) %>%
#               extract2(21) %>%
#               clean_names() %>%
#               filter(draw != "Draw") %>%
#               select(country, jury, televote) %>%
#               mutate(from_country = "Romania"))# %>%
#   # bind_rows(read_html("https://en.wikipedia.org/wiki/San_Marino_in_the_Eurovision_Song_Contest_2022" ) %>%
#   #             html_table(fill = TRUE) %>%
#   #             extract2(17) %>%
#   #             clean_names() %>%
#   #             filter(draw != "Draw") %>%
#   #             select(country, jury, televote) %>%
#   #             mutate(from_country = "San Marino"))
# 
# extras_2022_final%>%
#   rename(to_country =
#            country,
#          jury_rank =
#            jury,
#          televoting_rank =
#            televote) %>%
#   mutate(juror = NA,
#          a = NA,
#          b = NA,
#          c = NA,
#          d = NA,
#          e = NA) %>%
#   relocate(to_country,
#            juror,
#            a,
#            b,
#            c,
#            d,
#            e,
#            jury_rank,
#            televoting_rank,
#            from_country) %>%
#   bind_rows(read_csv("y_2022_final.csv")) %>%
#   write_csv("y_2022_final_WITH_EXTRAS.csv")





# # get running orders
# get_eurovision_page("https://eurovision.tv/event/copenhagen-2014/first-semi-final",
#                     "dummy") %>% 
#   select(1, 3) %>% 
#   rename(running_order_position = 
#            r_o_sort_descending) %>% 
#   write_csv("ro_2014_semi_1.csv")
# 
# get_eurovision_page("https://eurovision.tv/event/copenhagen-2014/second-semi-final",
#                     "dummy") %>% 
#   select(1, 3) %>% 
#   rename(running_order_position = 
#            r_o_sort_descending) %>% 
#   write_csv("ro_2014_semi_2.csv")
# 
# get_eurovision_page("https://eurovision.tv/event/copenhagen-2014/grand-final",
#                     "dummy") %>% 
#   select(1, 3) %>% 
#   rename(running_order_position = 
#            r_o_sort_descending) %>% 
#   write_csv("ro_2014_final.csv")
# 
# get_eurovision_page("https://eurovision.tv/event/vienna-2015/first-semi-final",
#                     "dummy") %>% 
#   select(1, 3) %>% 
#   rename(running_order_position = 
#            r_o_sort_descending) %>% 
#   write_csv("ro_2015_semi_1.csv")
# 
# get_eurovision_page("https://eurovision.tv/event/vienna-2015/second-semi-final",
#                     "dummy") %>% 
#   select(1, 3) %>% 
#   rename(running_order_position = 
#            r_o_sort_descending) %>% 
#   write_csv("ro_2015_semi_2.csv")
# 
# get_eurovision_page("https://eurovision.tv/event/vienna-2015/grand-final",
#                     "dummy") %>% 
#   select(1, 3) %>% 
#   rename(running_order_position = 
#            r_o_sort_descending) %>% 
#   write_csv("ro_2015_final.csv")
# 
# get_eurovision_page("https://eurovision.tv/event/stockholm-2016/first-semi-final",
#                     "dummy") %>% 
#   select(1, 3) %>% 
#   rename(running_order_position = 
#            r_o_sort_descending) %>% 
#   write_csv("ro_2016_semi_1.csv")
# 
# get_eurovision_page("https://eurovision.tv/event/stockholm-2016/second-semi-final",
#                     "dummy") %>% 
#   select(1, 3) %>% 
#   rename(running_order_position = 
#            r_o_sort_descending) %>% 
#   write_csv("ro_2016_semi_2.csv")
# 
# get_eurovision_page("https://eurovision.tv/event/stockholm-2016/grand-final",
#                     "dummy") %>% 
#   select(1, 3) %>% 
#   rename(running_order_position = 
#            r_o_sort_descending) %>% 
#   write_csv("ro_2016_final.csv")
# 
# get_eurovision_page("https://eurovision.tv/event/kyiv-2017/first-semi-final",
#                     "dummy") %>% 
#   select(1, 3) %>% 
#   rename(running_order_position = 
#            r_o_sort_descending) %>% 
#   write_csv("ro_2017_semi_1.csv")
# 
# get_eurovision_page("https://eurovision.tv/event/kyiv-2017/second-semi-final",
#                     "dummy") %>% 
#   select(1, 3) %>% 
#   rename(running_order_position = 
#            r_o_sort_descending) %>% 
#   write_csv("ro_2017_semi_2.csv")
# 
# get_eurovision_page("https://eurovision.tv/event/kyiv-2017/grand-final",
#                     "dummy") %>% 
#   select(1, 3) %>% 
#   rename(running_order_position = 
#            r_o_sort_descending) %>% 
#   write_csv("ro_2017_final.csv")
# 
# get_eurovision_page("https://eurovision.tv/event/lisbon-2018/first-semi-final",
#                     "dummy") %>% 
#   select(1, 3) %>% 
#   rename(running_order_position = 
#            r_o_sort_descending) %>% 
#   write_csv("ro_2018_semi_1.csv")
# 
# get_eurovision_page("https://eurovision.tv/event/lisbon-2018/second-semi-final",
#                     "dummy") %>% 
#   select(1, 3) %>% 
#   rename(running_order_position = 
#            r_o_sort_descending) %>% 
#   write_csv("ro_2018_semi_2.csv")
# 
# get_eurovision_page("https://eurovision.tv/event/lisbon-2018/grand-final",
#                     "dummy") %>% 
#   select(1, 3) %>% 
#   rename(running_order_position = 
#            r_o_sort_descending) %>% 
#   write_csv("ro_2018_final.csv")
# 
# get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/first-semi-final",
#                     "dummy") %>% 
#   select(1, 3) %>% 
#   rename(running_order_position = 
#            r_o_sort_descending) %>% 
#   write_csv("ro_2019_semi_1.csv")
# 
# get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/second-semi-final",
#                     "dummy") %>% 
#   select(1, 3) %>% 
#   rename(running_order_position = 
#            r_o_sort_descending) %>% 
#   write_csv("ro_2019_semi_2.csv")
# 
# get_eurovision_page("https://eurovision.tv/event/tel-aviv-2019/grand-final",
#                     "dummy") %>% 
#   select(1, 3) %>% 
#   rename(running_order_position = 
#            r_o_sort_descending) %>% 
#   write_csv("ro_2019_final.csv")
# 
# get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/first-semi-final",
#                     "dummy") %>% 
#   select(1, 3) %>% 
#   rename(running_order_position = 
#            r_o_sort_descending) %>% 
#   write_csv("ro_2021_semi_1.csv")
# 
# get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/second-semi-final",
#                     "dummy") %>% 
#   select(1, 3) %>% 
#   rename(running_order_position = 
#            r_o_sort_descending) %>% 
#   write_csv("ro_2021_semi_2.csv")
# 
# get_eurovision_page("https://eurovision.tv/event/rotterdam-2021/grand-final",
#                     "dummy") %>% 
#   select(1, 3) %>% 
#   rename(running_order_position = 
#            r_o_sort_descending) %>% 
#   write_csv("ro_2021_final.csv")
# 
# get_eurovision_page("https://eurovision.tv/event/turin-2022/first-semi-final",
#                     "dummy") %>% 
#   select(1, 3) %>% 
#   rename(running_order_position = 
#            r_o_sort_descending) %>% 
#   write_csv("ro_2022_semi_1.csv")
# 
# get_eurovision_page("https://eurovision.tv/event/turin-2022/second-semi-final",
#                     "dummy") %>%
#   select(1, 3) %>%
#   rename(running_order_position =
#            r_o_sort_descending) %>%
#   write_csv("ro_2022_semi_2.csv")
# 
# get_eurovision_page("https://eurovision.tv/event/turin-2022/grand-final",
#                     "dummy") %>%
#   select(1, 3) %>%
#   rename(running_order_position =
#            r_o_sort_descending) %>%
#   write_csv("ro_2022_final.csv")

