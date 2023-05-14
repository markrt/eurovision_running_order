# eurovision 2023 has taken place

# let's get the full voting data

# load packages
library(rvest)
library(magrittr)
library(tidyverse)
library(janitor)
library(hrbrthemes)

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


y_2023_final <-
  get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/albania",
                      "Albania") %>%
  bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/armenia",
                                "Armenia")) %>%
  bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/australia",
                                "Australia")) %>%
  bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/austria",
                                "Austria")) %>%
  bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/azerbaijan",
                                "Azerbaijan")) %>%
  # bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/belarus",
  #                               "Belarus")) %>%
  bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/belgium",
                                "Belgium")) %>%
  # bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/bulgaria",
  #                               "Bulgaria")) %>%
  bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/croatia",
                                "Croatia")) %>%
  bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/cyprus",
                                "Cyprus")) %>%
  bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/czechia",
                                "Czechia")) %>%
  bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/denmark",
                                "Denmark")) %>%
  bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/estonia",
                                "Estonia")) %>%
  bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/finland",
                                "Finland")) %>%
  bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/france",
                                "France")) %>%
  bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/georgia",
                                "Georgia")) %>%
  bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/germany",
                                "Germany")) %>%
  bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/greece",
                                "Greece")) %>%
  # bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/hungary",
  #                               "Hungary")) %>%
  bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/iceland",
                                "Iceland")) %>%
  bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/ireland",
                                "Ireland")) %>%
  bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/israel",
                                "Israel")) %>%
  bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/italy",
                                "Italy")) %>%
  bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/latvia",
                                "Latvia")) %>%
  bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/lithuania",
                                "Lithuania")) %>%
  bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/malta",
                                "Malta")) %>%
  bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/moldova",
                                "Moldova")) %>%
  # bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/montenegro",
  #                               "Montenegro")) %>%
  bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/netherlands",
                                "Netherlands")) %>%
  # bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/north-macedonia",
  #                               "North Macedonia")) %>%
  bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/norway",
                                "Norway")) %>%
  bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/poland",
                                "Poland")) %>%
  bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/portugal",
                                "Portugal")) %>%
  # bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/romania",
  #                               "Romania")) %>%
  # bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/russia",
  #                               "Russia")) %>%
  bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/san-marino",
                                "San Marino")) %>%
  bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/serbia",
                                "Serbia")) %>%
  bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/slovenia",
                                "Slovenia")) %>%
  bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/spain",
                                "Spain")) %>%
  bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/sweden",
                                "Sweden")) %>%
  bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/switzerland",
                                "Switzerland")) %>%
  bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/ukraine",
                                "Ukraine")) %>%
  bind_rows(get_eurovision_page("https://eurovision.tv/event/liverpool-2023/grand-final/results/united-kingdom",
                                "United Kingdom"))
# # 
write_csv(y_2023_final, "y_2023_final.csv")
y_2023_final 

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
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        strip.text.x = element_text(size = 8)) +
  labs(x = "",
       y = "")

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
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    strip.text.x = element_text(size = 8)) +
  labs(x = "",
       y = "")

  
