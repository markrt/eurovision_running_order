# eurovision 2023 has taken place

# let's get the full voting data

# load packages
library(rvest)
library(magrittr)
library(tidyverse)
library(janitor)
library(hrbrthemes)
library(ggflags)
library(countrycode)

# function
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

# get data
# 
# y_2023_final <-
#   get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/albania",
#                       "Albania") %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/armenia",
#                                 "Armenia")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/australia",
#                                 "Australia")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/austria",
#                                 "Austria")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/azerbaijan",
#                                 "Azerbaijan")) %>%
#   # bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/belarus",
#   #                               "Belarus")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/belgium",
#                                 "Belgium")) %>%
#   # bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/bulgaria",
#   #                               "Bulgaria")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/croatia",
#                                 "Croatia")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/cyprus",
#                                 "Cyprus")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/czechia",
#                                 "Czechia")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/denmark",
#                                 "Denmark")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/estonia",
#                                 "Estonia")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/finland",
#                                 "Finland")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/france",
#                                 "France")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/georgia",
#                                 "Georgia")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/germany",
#                                 "Germany")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/greece",
#                                 "Greece")) %>%
#   # bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/hungary",
#   #                               "Hungary")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/iceland",
#                                 "Iceland")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/ireland",
#                                 "Ireland")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/israel",
#                                 "Israel")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/italy",
#                                 "Italy")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/latvia",
#                                 "Latvia")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/lithuania",
#                                 "Lithuania")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/malta",
#                                 "Malta")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/moldova",
#                                 "Moldova")) %>%
#   # bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/montenegro",
#   #                               "Montenegro")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/netherlands",
#                                 "Netherlands")) %>%
#   # bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/north-macedonia",
#   #                               "North Macedonia")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/norway",
#                                 "Norway")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/poland",
#                                 "Poland")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/portugal",
#                                 "Portugal")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/romania",
#                                 "Romania")) %>%
#   # bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/russia",
#   #                               "Russia")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/san-marino",
#                                 "San Marino")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/serbia",
#                                 "Serbia")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/slovenia",
#                                 "Slovenia")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/spain",
#                                 "Spain")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/sweden",
#                                 "Sweden")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/switzerland",
#                                 "Switzerland")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/ukraine",
#                                 "Ukraine")) %>%
#   bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/united-kingdom",
#                                 "United Kingdom"))
# #
# write_csv(y_2023_final, "y_2023_final.csv")

# load data
y_2023_final <- 
  read_csv("y_2023_final.csv")

# get distribution of televote ranks

y_2023_final %>% 
  mutate(televote = 
           word(televoting_rank, -1)) %>% 
  mutate(televote = 
           str_sub(televote, end = -3)) %>% 
  # mutate(rank = as.numeric(rank)) %>% 
  mutate(televote = 
           as.numeric(televote)) %>% 
  group_by(to_country) %>% 
  mutate(mean_televote_rank = 
           mean(televote)) %>% 
  ggplot() + 
  aes(x = televote) + 
  geom_bar() + 
  geom_vline(xintercept = 
                   10,
             colour = "powderblue") +
  facet_wrap(~fct_reorder(to_country,
                          mean_televote_rank))  +
  theme_ipsum_ps(plot_margin = margin(10, 10, 10, 10)) +
  theme(
    panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        strip.text.x = element_text(size = 8)) +
  theme(panel.spacing = unit(1, "lines")) +
  labs(x = "",
       y = "")
ggsave("2023_televote_ranks.png",
       width = 16/2,
       height = 9/2)

# get distribution of jury ranks

y_2023_final %>% 
  select(to_country,
         from_country,
         a:e) %>% 
  pivot_longer(a:e,
               names_to = "juror",
               values_to = "ranking") %>% 
  na.omit %>% 
  group_by(to_country) %>% 
  mutate(mean_ranking = 
           mean(ranking))%>% 
  ggplot() + 
  aes(x = ranking) + 
  geom_bar() +
  geom_vline(xintercept = 
               10,
             colour = "powderblue") +
  facet_wrap(~fct_reorder(to_country,
                          mean_ranking))  +
  theme_ipsum_ps(plot_margin = margin(10, 10, 10, 10)) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    strip.text.x = element_text(size = 8)) +
  theme(panel.spacing = unit(1, "lines")) +
  labs(x = "",
       y = "")
ggsave("2023_jury_ranks.png",
       width = 16/2,
       height = 9/2)

# make object for all allocated points
y_2023_points <- 
  y_2023_final %>% 
  filter(str_detect(jury_rank, "point") |
           str_detect(televoting_rank, "point")) %>% 
  mutate(jury_points = 
           case_when(str_detect(jury_rank, "point") == TRUE ~ 
                       word(jury_rank, 1)))%>% 
  mutate(televote_points = 
           case_when(str_detect(televoting_rank, "point") == TRUE ~ 
                       word(televoting_rank, 1))) %>% 
  # select(to_country,
  #        jury_points,
  #        televote_points) %>% 
  mutate(jury_points = 
           as.numeric(jury_points),
         televote_points = 
           as.numeric(televote_points)) %>% 
  mutate(jury_points = 
           replace_na(jury_points, 0)) %>% 
  mutate(televote_points = 
           replace_na(televote_points, 0)) %>% 
  select(from_country,
         to_country,
         televote_points,
         jury_points)

# make object for all ranks
y_2023_ranks <- 
  y_2023_final %>% 
  select(-juror) %>% 
  mutate(televote = 
           word(televoting_rank, -1)) %>% 
  mutate(televote = 
           str_sub(televote, end = -3)) %>% 
  # mutate(rank = as.numeric(rank)) %>% 
  mutate(televote = 
           as.numeric(televote)) %>% 
  rowwise() %>% 
  mutate(jury_mean = 
           rowMeans(across(a:e),
                    na.rm = TRUE)) %>% 
  select(from_country,
         to_country,
         televote,
         jury_mean) %>% 
  ungroup()



# make plot for relationship between ranking and points
# for the televote
y_2023_ranks %>% 
  left_join(y_2023_points) %>% 
  mutate(televote_points = 
           replace_na(televote_points, 0),
         jury_points = 
           replace_na(jury_points, 0)) %>% 
  group_by(to_country) %>% 
  mutate(jury_points = 
           sum(jury_points),
         televote_points = 
           sum(televote_points),
         televote = 
           mean(televote),
         jury_mean = 
           mean(jury_mean)) %>% 
  mutate(to_cc = 
           str_to_lower(countrycode(to_country,
                                    "country.name",
                                    "iso2c"))) %>% 
  ggplot() + 
  aes(x = televote_points,
      y = televote) + 
  geom_point(size = 6,
             colour = "gray80") + 
  geom_flag(aes(country = to_cc))  +
  scale_y_reverse() +
  theme_ipsum_ps() + 
  labs(x = "Televote points",
       y = "Average televote ranking")
ggsave("2023_televote_ranks_points.png",
       width = 16/2,
       height = 9/2)

# and jury vote

y_2023_ranks %>% 
  left_join(y_2023_points) %>% 
  mutate(televote_points = 
           replace_na(televote_points, 0),
         jury_points = 
           replace_na(jury_points, 0)) %>% 
  group_by(to_country) %>% 
  mutate(jury_points = 
           sum(jury_points),
         televote_points = 
           sum(televote_points),
         televote = 
           mean(televote),
         jury_mean = 
           mean(jury_mean)) %>% 
  mutate(to_cc = 
           str_to_lower(countrycode(to_country,
                                    "country.name",
                                    "iso2c"))) %>% 
  ggplot() + 
  aes(x = jury_points,
      y = jury_mean) + 
  geom_point(size = 6,
             colour = "gray80") + 
  geom_flag(aes(country = to_cc))  +
  scale_y_reverse() +
  theme_ipsum_ps() + 
  labs(x = "Jury points",
       y = "Average jury ranking")
ggsave("2023_jury_ranks_points.png",
       width = 16/2,
       height = 9/2)


y_2023_points %>% 
  filter(to_country == "Israel") %>%
  count(jury_points)r
2+2+3+4+2+8+15

# get rankings
# who got the most last places among jurors?
y_2023_final %>% 
  select(to_country,
         from_country,
         a:e) %>% 
  pivot_longer(a:e,
               names_to = "juror",
               values_to = "ranking") %>% 
  group_by(from_country) %>% 
  mutate(max_ranking = 
           max(ranking)) %>% 
  ungroup %>% 
  filter(max_ranking == ranking) %>% 
    count(to_country) %>% 
  arrange(-n)

# and televotes?
y_2023_ranks %>% 
  group_by(from_country) %>% 
  mutate(max_ranking = 
           max(televote)) %>% 
  filter(max_ranking == televote) %>% 
  ungroup %>% 
  count(to_country)

# how did UK do otherwise?
y_2023_ranks %>% 
  filter(to_country == "United Kingdom") %>% 
  count(televote)

# Albania?
y_2023_ranks %>% 
  filter(to_country == "Albania") %>% 
  count(televote)

# illustrative comparison - Germany and Portugal
y_2023_ranks %>% 
  group_by(to_country) %>% 
  summarise(mean_jury_mean = 
           mean(jury_mean),
           mean_televote = 
             mean(televote)) %>% filter(to_country == "Germany" |
                                          to_country == "Portugal")

# get some more illustrative comparisons
y_2023_ranks %>% 
  group_by(to_country) %>% 
  summarise(mean_jury_mean = 
              mean(jury_mean))
