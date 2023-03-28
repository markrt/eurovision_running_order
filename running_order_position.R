#### # responding to this
# https://twitter.com/Bensvision/status/1635602528120233984

# let's have a go

# i've done lots of data cleaning

# cleaning 2014-2016 is in getting_started.R
# but that's not necessary
# it's more data exploration
# data is from https://eurovision.tv/history/full-split-results
# though i've converted to csv (xls files also in this folder)
# (so you can check for any errors)

# pulling 2017-2022 is in getting_web_data.R
# in this script i pull stuff from the official Eurovision pages

# i then clean this data, and combine with the 2014-2016 stuff
# in cleaning_web_data.R

# i eventually produce a file called all_rankings.csv
# let's load some packages and data
# and call a function for fonts

library(tidyverse)
library(ggrepel)
library(hrbrthemes)
library(ggforce)


# define label fonts
for (geom in c("label", "text", "label_repel", "text_repel")) {
  update_geom_defaults(geom, list(family = font_es))
}
# load data
all_rankings <- 
  read_csv("all_rankings.csv")

# columns as follows
# from_country: country awarding ranking
# to_country: country in receipt of ranking
# rank: OVERALL JURY ranking (combined from all judges,
# with the combined algorithm from each relevant year)
# televote: TELEVOTE ranking from that country
# event_type: semi-final (1 or 2) or final
# year: year
# running_order_position: position to_country performed in
# in that broadcast

# vector of acts that performed in the final
finalists <- 
  all_rankings %>% 
  mutate(country_year = 
           paste(to_country, year)) %>% 
  filter(event_type == "final") %>% 
  select(country_year) %>% 
  distinct() %>% 
  pull()

# vector of voter-votee pairs in semi-finals
voter_votee_pairs <- 
  all_rankings %>% 
  mutate(country_country_year_trio = 
           paste(from_country,
                 to_country,
                 year)) %>% 
  filter(event_type != "final") %>% 
  select(country_country_year_trio) %>% 
  distinct() %>% 
  pull()

# data manipulation:
# make a table of
# country-country-year combinations
# jury and televote rankings
# in each of the semi and the final
# running order position
# in each of the semi and final
rankings_wide <- 
  all_rankings %>% 
  mutate(country_year = 
           paste(to_country, year)) %>% 
  filter(country_year %in% finalists) %>% 
  mutate(country_country_year_trio = 
           paste(from_country,
                 to_country,
                 year)) %>% 
  filter(country_country_year_trio %in% voter_votee_pairs) %>% 
  group_by(from_country,
           event_type,
           year) %>% 
  mutate(event_jury_ranking = 
           rank(rank)) %>%
  mutate(event_televote_ranking = 
           rank(televote)) %>% 
  na.omit %>% 
  ungroup %>% 
  select(event_jury_ranking,
         event_televote_ranking,
         from_country,
         to_country,
         event_type,
         year,
         running_order_position) %>% 
  mutate(event_type = 
           fct_recode(event_type,
                      "semi-final" = 
                        "semi-final 1",
                      "semi-final" = 
                        "semi-final 2")) %>% 
  pivot_wider(names_from = event_type,
              values_from = c(event_jury_ranking,
                              event_televote_ranking,
                              running_order_position))

# change names
# to make more comprehensible
names(rankings_wide) <- 
  c("from_country",
    "to_country",
    "year",
    "jury_ranking_semi",
    "jury_ranking_final",
    "televote_ranking_semi",
    "televote_ranking_final",
    "running_order_semi",
    "running_order_final")

# look at it
rankings_wide %>% 
  mutate(jury_difference = 
           jury_ranking_final - 
           jury_ranking_semi,
         televote_difference = 
           televote_ranking_final - 
           televote_ranking_semi) %>% 
  arrange(jury_difference)
# Belarus' jury didn't like Cyprus in the 2018 semi
# but did like them in the final
# (I've checked this, it's correct)

# flip it over

rankings_wide %>% 
  mutate(jury_difference = 
           jury_ranking_final - 
           jury_ranking_semi,
         televote_difference = 
           televote_ranking_final - 
           televote_ranking_semi) %>% 
  arrange(-jury_difference)

# Australia 2015 is a problem!!!!
# sort out separately
# this is ridiculously complicated

australia_2015_semi_1 <- 
  all_rankings %>% 
  filter(from_country == "Australia") %>% 
  filter(year == 2015) %>% 
  filter(event_type == "semi-final 1")%>% 
  mutate(country_pair_year = 
           paste(from_country,
                 to_country,
                 year)) %>% 
  filter((country_pair_year) %in% voter_votee_pairs) %>% 
  mutate(eventual_performance = 
           paste(to_country, year)) %>% 
  filter(eventual_performance %in% finalists) %>% 
  select(country_pair_year) %>% 
  pull()


australia_2015_semi_2 <- 
  all_rankings %>% 
  filter(from_country == "Australia") %>% 
  filter(year == 2015) %>% 
  filter(event_type == "semi-final 2")%>% 
  mutate(country_pair_year = 
           paste(from_country,
                 to_country,
                 year)) %>% 
  filter((country_pair_year) %in% voter_votee_pairs) %>% 
  mutate(eventual_performance = 
           paste(to_country, year)) %>% 
  filter(eventual_performance %in% finalists) %>% 
  select(country_pair_year) %>% 
  pull()

australia_2015_updated_ranks <- 
  all_rankings %>% 
  filter(from_country == "Australia") %>% 
  filter(year == 2015) %>% 
  filter(event_type == "final") %>% 
  filter(paste(from_country, to_country, year) %in% voter_votee_pairs) %>% 
  filter(paste(from_country, to_country, year) %in% australia_2015_semi_1) %>% 
  mutate(rank = rank(rank),
         televote = rank(televote)) %>% 
  bind_rows(all_rankings %>% 
              filter(from_country == "Australia") %>% 
              filter(year == 2015) %>% 
              filter(event_type == "final") %>% 
              filter(paste(from_country, to_country, year) %in% voter_votee_pairs) %>% 
              filter(paste(from_country, to_country, year) %in% australia_2015_semi_2) %>% 
              mutate(rank = rank(rank),
                     televote = rank(televote)) )

all_rankings_clean <- 
  all_rankings %>% 
  mutate(country_year_pairs = 
           paste(from_country,
                 to_country,
                 year)) %>% 
  filter(!(country_year_pairs %in% australia_2015_semi_1 &
             event_type == "semi-final 1") &
           !((country_year_pairs %in% australia_2015_semi_2 &
                event_type == "semi-final 2"))) %>% 
  bind_rows(australia_2015_updated_ranks)

# reshape again

rankings_wide <- 
  all_rankings_clean %>% 
  mutate(country_year = 
           paste(to_country, year)) %>% 
  filter(country_year %in% finalists) %>% 
  mutate(country_country_year_trio = 
           paste(from_country,
                 to_country,
                 year)) %>% 
  filter(country_country_year_trio %in% voter_votee_pairs) %>% 
  group_by(from_country,
           event_type,
           year) %>% 
  mutate(event_jury_ranking = 
           rank(rank)) %>%
  mutate(event_televote_ranking = 
           rank(televote)) %>% 
  na.omit %>% 
  ungroup %>% 
  select(event_jury_ranking,
         event_televote_ranking,
         from_country,
         to_country,
         event_type,
         year,
         running_order_position) %>% 
  mutate(event_type = 
           fct_recode(event_type,
                      "semi-final" = 
                        "semi-final 1",
                      "semi-final" = 
                        "semi-final 2")) %>% 
  pivot_wider(names_from = event_type,
              values_from = c(event_jury_ranking,
                              event_televote_ranking,
                              running_order_position))

# change names
# to make more comprehensible
names(rankings_wide) <- 
  c("from_country",
    "to_country",
    "year",
    "jury_ranking_semi",
    "jury_ranking_final",
    "televote_ranking_semi",
    "televote_ranking_final",
    "running_order_semi",
    "running_order_final")

# check biggest drop

rankings_wide %>% 
  mutate(jury_difference = 
           jury_ranking_final - 
           jury_ranking_semi,
         televote_difference = 
           televote_ranking_final - 
           televote_ranking_semi) %>% 
  arrange(-jury_difference)
# yeah,
# Montenegro's jury loved Greece in the semi in 2019
# and weren't fussed in the final

# ok they love putting the big five last
# look at televote first
rankings_wide %>% 
  filter((year == 2016 &
           to_country == "Armenia" )|
           (year == 2021 &
              to_country == "San Marino") |
           (year == 2022 & 
              to_country == "Estonia")) %>% 
  select(from_country,
         to_country,
         year,
         televote_ranking_semi,
         televote_ranking_final,
         running_order_final) %>% 
  na.omit %>% 
  group_by(to_country) %>% 
  summarise(mean_gulf = 
              mean(televote_ranking_final) -
                     mean(televote_ranking_semi))
# this has been a lot of work for three datapoints
# but all the final-performing countries
# did a bit better in the televote in the final
# than in the semi
# when you're just looking at countries that could vote in both

# ok, what about juries

rankings_wide %>% 
  filter((year == 2016 &
            to_country == "Armenia" )|
           (year == 2021 &
              to_country == "San Marino") |
           (year == 2022 & 
              to_country == "Estonia")) %>% 
  select(from_country,
         to_country,
         year,
         jury_ranking_semi,
         jury_ranking_final,
         running_order_final) %>% 
  na.omit %>% 
  group_by(to_country) %>% 
  summarise(mean_gulf = 
              mean(jury_ranking_final) -
              mean(jury_ranking_semi))
# Armenia did a bit better
# Estonia did a fair bit worse
# San Marino also did a bit worse

# ok let's eyeball the biggest risers and fallers
# for televote first
rankings_wide %>% 
  mutate(televote_discrepancy = 
           televote_ranking_final - 
           televote_ranking_semi) %>% 
  select(from_country,
         to_country,
         year,
         televote_discrepancy,
         running_order_final)  %>% 
  na.omit %>% 
  group_by(to_country,
           year) %>% 
  summarise(mean_discrepancy = 
              mean(televote_discrepancy)) %>% 
  arrange(mean_discrepancy)
# Sweden 2021: this looks about right
# jury
rankings_wide %>% 
  mutate(jury_discrepancy = 
           jury_ranking_final - 
           jury_ranking_semi) %>% 
  select(from_country,
         to_country,
         year,
         jury_discrepancy,
         running_order_final)  %>% 
  na.omit %>% 
  group_by(to_country,
           year) %>% 
  summarise(mean_discrepancy = 
              mean(jury_discrepancy)) %>% 
  arrange(mean_discrepancy)
# Belgium 2017: again looks plausible

# best position?  square
rankings_wide %>% 
  mutate(jury_discrepancy = 
           jury_ranking_final - 
           jury_ranking_semi) %>% 
  mutate(televote_discrepancy = 
           televote_ranking_final - 
           televote_ranking_semi) %>% 
  select(from_country,
         to_country,
         year,
         jury_discrepancy,
         televote_discrepancy,
         running_order_final)  %>% 
  na.omit %>% 
  group_by(to_country,
           year,
           running_order_final) %>% 
  summarise(mean_jury_discrepancy = 
              mean(jury_discrepancy),
            mean_televote_discrepancy = 
              mean(televote_discrepancy)) %>% 
  group_by(running_order_final) %>% 
  summarise(mean_jury_discrepancy = 
              mean(mean_jury_discrepancy),
            mean_televote_discrepancy = 
              mean(mean_televote_discrepancy)) %>% 
  ggplot() + 
  aes(x = mean_jury_discrepancy,
      y = mean_televote_discrepancy) + 
  annotate("rect", xmin = Inf, xmax = 0, ymin = Inf, ymax = 0, fill= "#F79256")  + 
  annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = 0 , fill= "#FBD1A2") + 
  annotate("rect", xmin = 0, xmax = Inf, ymin = 0, ymax = -Inf, fill= "#7DCFB6") + 
  annotate("rect", xmin = 0, xmax = -Inf, ymin = Inf, ymax = 0, fill= "#00B2CA") + 
  geom_point() +
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) + 
  geom_circle(aes(x0 = 0, y0 = 0, r = .25),
              inherit.aes = FALSE,
              fill = "white") + 
  annotate("text",
           x = 0.3,
           y = 0.8,
           label =  "Televoters and juries \npreferred it in the semi") +
  annotate("text",
           x = -0.23,
           y = 0.8,
           label =  "Televoters preferred it in the semi,\njuries preferred it in the final ") +
  annotate("text",
           x = .3,
           y = -.6,
           label =  "Televoters preferred it in the final,\njuries preferred it in the semi ") +
  annotate("text",
           x = -.23,
           y = -.6,
           label =  "Televoters and juries \npreferred it in the final") +
  annotate("text",
           x = -0,
           y = 0,
           label =  "Televoters and juries \ndidn't change their minds") +
  geom_label_repel(aes(label = running_order_final)) + 
  theme_ipsum_ps() + 
  labs(x = "Jury",
       y = "Televote") + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# change axes, export to 16:9

# best position?  square
rankings_wide %>% 
  mutate(jury_discrepancy = 
           jury_ranking_final - 
           jury_ranking_semi) %>% 
  mutate(televote_discrepancy = 
           televote_ranking_final - 
           televote_ranking_semi) %>% 
  select(from_country,
         to_country,
         year,
         jury_discrepancy,
         televote_discrepancy,
         running_order_final)  %>% 
  na.omit %>% 
  group_by(to_country,
           year,
           running_order_final) %>% 
  summarise(mean_jury_discrepancy = 
              mean(jury_discrepancy),
            mean_televote_discrepancy = 
              mean(televote_discrepancy)) %>% 
  group_by(running_order_final) %>% 
  summarise(mean_jury_discrepancy = 
              mean(mean_jury_discrepancy),
            mean_televote_discrepancy = 
              mean(mean_televote_discrepancy)) %>% 
  ggplot() + 
  aes(y = mean_jury_discrepancy,
      x = mean_televote_discrepancy) + 
  annotate("rect", xmin = Inf, xmax = 0, ymin = Inf, ymax = 0, fill= "#F79256")  + 
  annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = 0 , fill= "#FBD1A2") + 
  annotate("rect", xmin = 0, xmax = Inf, ymin = 0, ymax = -Inf, fill= "#7DCFB6") + 
  annotate("rect", xmin = 0, xmax = -Inf, ymin = Inf, ymax = 0, fill= "#00B2CA") + 
  geom_point() +
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) + 
  geom_circle(aes(x0 = 0, y0 = 0, r = .25),
              inherit.aes = FALSE,
              fill = "white") + 
  annotate("text",
           y = 0.3,
           x = 0.8,
           label =  "Televoters and juries \npreferred it in the semi") +
  annotate("text",
           y = -0.23,
           x = 0.8,
           label =  "Televoters preferred it in the semi,\njuries preferred it in the final ") +
  annotate("text",
           y = .3,
           x = -.5,
           label =  "Televoters preferred it \nin the final, juries \npreferred it in the semi ") +
  annotate("text",
           y = -.23,
           x = -.5,
           label =  "Televoters and juries \npreferred it in the final") +
  annotate("text",
           x = -0,
           y = 0,
           label =  "Televoters and \njuries didn't \nchange their \nminds") +
  geom_label_repel(aes(label = running_order_final)) + 
  theme_ipsum_ps() + 
  labs(y = "Jury",
       x = "Televote") + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) + 
  coord_fixed()
ggsave("key_graph_16_9.png",
       width = 16/2,
       height = 9/2)


# best position? fixed coordinate system
rankings_wide %>% 
  mutate(jury_discrepancy = 
           jury_ranking_final - 
           jury_ranking_semi) %>% 
  mutate(televote_discrepancy = 
           televote_ranking_final - 
           televote_ranking_semi) %>% 
  select(from_country,
         to_country,
         year,
         jury_discrepancy,
         televote_discrepancy,
         running_order_final)  %>% 
  na.omit %>% 
  group_by(to_country,
           year,
           running_order_final) %>% 
  summarise(mean_jury_discrepancy = 
              mean(jury_discrepancy),
            mean_televote_discrepancy = 
              mean(televote_discrepancy)) %>% 
  group_by(running_order_final) %>% 
  summarise(mean_jury_discrepancy = 
              mean(mean_jury_discrepancy),
            mean_televote_discrepancy = 
              mean(mean_televote_discrepancy)) %>% 
  ggplot() + 
  aes(x = mean_jury_discrepancy,
      y = mean_televote_discrepancy) + 
  annotate("rect", xmin = Inf, xmax = 0, ymin = Inf, ymax = 0, fill= "#F79256")  + 
  annotate("rect", xmin = -Inf, xmax = 0, ymin = -Inf, ymax = 0 , fill= "#FBD1A2") + 
  annotate("rect", xmin = 0, xmax = Inf, ymin = 0, ymax = -Inf, fill= "#7DCFB6") + 
  annotate("rect", xmin = 0, xmax = -Inf, ymin = Inf, ymax = 0, fill= "#00B2CA") + 
  geom_point() +
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) + 
  geom_circle(aes(x0 = 0, y0 = 0, r = .25),
              inherit.aes = FALSE,
              fill = "white") + 
  annotate("text",
           x = 0.3,
           y = 0.8,
           label =  "Televoters and juries \npreferred it in the semi") +
  annotate("text",
           x = -0.23,
           y = 0.8,
           label =  "Televoters preferred it \nin the semi,\njuries preferred it \nin the final ") +
  annotate("text",
           x = .3,
           y = -.6,
           label =  "Televoters preferred it \nin the final,\njuries preferred it \nin the semi ") +
  annotate("text",
           x = -.23,
           y = -.6,
           label =  "Televoters and juries \npreferred it in the final") +
  annotate("text",
           x = -0,
           y = 0,
           label =  "Televoters and juries \ndidn't change their minds") +
  geom_label_repel(aes(label = running_order_final)) + 
  theme_ipsum_ps() + 
  labs(x = "Jury",
       y = "Televote") + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

### draw figure for discrepancies for finalists

rankings_wide %>% 
  filter((year == 2016 &
            to_country == "Armenia") |
           (year == 2021 &
              to_country == "San Marino") | 
           (year == 2022 &
              to_country == "Estonia")) %>% 
  ggplot() + 
  aes(x = jury_ranking_semi,
      y = jury_ranking_final) + 
  geom_point() + 
  facet_wrap(~ year) + 
  geom_abline(slope = 1,
              intercept = 0) + 
  theme_ipsum_es() + 
  scale_x_reverse(breaks = c(10:1)) + 
  scale_y_reverse(breaks = c(10:1)) + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


rankings_wide %>% 
  filter((year == 2016 &
            to_country == "Armenia") |
           (year == 2021 &
              to_country == "San Marino") | 
           (year == 2022 &
              to_country == "Estonia")) %>% 
  mutate(year_label = 
           case_when(year == 2016 ~ "2016: Armenia",
                     year == 2021 ~ "2021: San Marino",
                     year == 2022 ~ "2022: Estonia")) %>% 
  select(-c(running_order_semi,
         running_order_final,
         televote_ranking_semi,
         televote_ranking_final)) %>% 
  rename(ranking_semi = 
           jury_ranking_semi,
         ranking_final = 
           jury_ranking_final) %>% 
  mutate(group = "Jury") %>% 
  bind_rows(rankings_wide %>% 
              filter((year == 2016 &
                        to_country == "Armenia") |
                       (year == 2021 &
                          to_country == "San Marino") | 
                       (year == 2022 &
                          to_country == "Estonia")) %>% 
              mutate(year_label = 
                       case_when(year == 2016 ~ "2016: Armenia",
                                 year == 2021 ~ "2021: San Marino",
                                 year == 2022 ~ "2022: Estonia")) %>% 
              select(-c(running_order_semi,
                        running_order_final,
                        jury_ranking_semi,
                        jury_ranking_final)) %>% 
              rename(ranking_semi = 
                       televote_ranking_semi,
                     ranking_final = 
                       televote_ranking_final) %>% 
              mutate(group = "Televote") ) %>% 
  ggplot() + 
  aes(x = ranking_semi,
      y = ranking_final) + 
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), 
           y = c(-Inf, Inf, -Inf), 
           fill = "#FBD1A2" )+
  annotate(geom = "polygon", 
           x = c(Inf, -Inf, -Inf), 
           y = c(Inf, -Inf, Inf), 
           fill = "#7DCFB6" )+
  geom_point() + 
  facet_grid(group~ year_label) + 
  geom_abline(slope = 1,
              intercept = 0) + 
  theme_ipsum_es() + 
  scale_x_reverse(breaks = c(10:1)) + 
  scale_y_reverse(breaks = c(10:1)) + 
  theme(panel.grid.major = 
          element_blank(),
        panel.grid.minor = 
          element_blank()) + 
  labs(x = "Ranking (semi-final)",
       y = "Ranking (final)")

### allow different points for more observations


rankings_wide %>% 
  filter((year == 2016 &
            to_country == "Armenia") |
           (year == 2021 &
              to_country == "San Marino") | 
           (year == 2022 &
              to_country == "Estonia")) %>% 
  mutate(year_label = 
           case_when(year == 2016 ~ "2016: Armenia",
                     year == 2021 ~ "2021: San Marino",
                     year == 2022 ~ "2022: Estonia")) %>% 
  select(-c(running_order_semi,
            running_order_final,
            televote_ranking_semi,
            televote_ranking_final)) %>% 
  rename(ranking_semi = 
           jury_ranking_semi,
         ranking_final = 
           jury_ranking_final) %>% 
  mutate(group = "Jury") %>% 
  bind_rows(rankings_wide %>% 
              filter((year == 2016 &
                        to_country == "Armenia") |
                       (year == 2021 &
                          to_country == "San Marino") | 
                       (year == 2022 &
                          to_country == "Estonia")) %>% 
              mutate(year_label = 
                       case_when(year == 2016 ~ "2016: Armenia",
                                 year == 2021 ~ "2021: San Marino",
                                 year == 2022 ~ "2022: Estonia")) %>% 
              select(-c(running_order_semi,
                        running_order_final,
                        jury_ranking_semi,
                        jury_ranking_final)) %>% 
              rename(ranking_semi = 
                       televote_ranking_semi,
                     ranking_final = 
                       televote_ranking_final) %>% 
              mutate(group = "Televote") ) %>% 
  na.omit %>% 
  group_by(year_label,
           ranking_semi,
           ranking_final,
           group) %>% 
  summarise(howmany = n()) %>% 
  ggplot() + 
  aes(x = ranking_semi,
      y = ranking_final,
      fill = as.factor(howmany)) + 
  annotate(geom = "polygon", 
           x = c(-Inf, Inf, Inf), 
           y = c(-Inf, Inf, -Inf), 
           fill = "#FBD1A2" )+
  annotate(geom = "polygon", 
           x = c(Inf, -Inf, -Inf), 
           y = c(Inf, -Inf, Inf), 
           fill = "#7DCFB6" )+
  geom_abline(slope = 1,
              intercept = 0) + 
  geom_point(shape = 21) + 
  facet_grid(group~ year_label) + 
  theme_ipsum_es() + 
  # scale_x_reverse(breaks = c(10:1)) +
  # scale_y_reverse(breaks = c(10:1)) +
  scale_x_continuous(breaks = c(1:10)) + 
  scale_y_continuous(breaks = c(1:10)) +
  scale_fill_manual(values = c("black",
                               "gray70",
                               "white")) +
  theme(panel.grid.major = 
          element_blank(),
        panel.grid.minor = 
          element_blank(),
        legend.position = "bottom") + 
  labs(x = "Ranking (semi-final)",
       y = "Ranking (final)",
       fill = "Frequency") + 
  coord_fixed()



# stray maths for detail
rankings_wide %>% 
  filter((year == 2016 &
            to_country == "Armenia") |
           (year == 2021 &
              to_country == "San Marino") | 
           (year == 2022 &
              to_country == "Estonia")) %>% 
  mutate(year_label = 
           case_when(year == 2016 ~ "2016: Armenia",
                     year == 2021 ~ "2021: San Marino",
                     year == 2022 ~ "2022: Estonia")) %>% 
  select(-c(running_order_semi,
            running_order_final,
            televote_ranking_semi,
            televote_ranking_final)) %>% 
  rename(ranking_semi = 
           jury_ranking_semi,
         ranking_final = 
           jury_ranking_final) %>% 
  mutate(group = "Jury") %>% 
  bind_rows(rankings_wide %>% 
              filter((year == 2016 &
                        to_country == "Armenia") |
                       (year == 2021 &
                          to_country == "San Marino") | 
                       (year == 2022 &
                          to_country == "Estonia")) %>% 
              mutate(year_label = 
                       case_when(year == 2016 ~ "2016: Armenia",
                                 year == 2021 ~ "2021: San Marino",
                                 year == 2022 ~ "2022: Estonia")) %>% 
              select(-c(running_order_semi,
                        running_order_final,
                        jury_ranking_semi,
                        jury_ranking_final)) %>% 
              rename(ranking_semi = 
                       televote_ranking_semi,
                     ranking_final = 
                       televote_ranking_final) %>% 
              mutate(group = "Televote") ) %>% 
  mutate(change = 
           case_when(ranking_semi < ranking_final ~ "decrease",
                     ranking_semi == ranking_final ~ "stable",
                     ranking_semi > ranking_final ~ "increase")) %>% 
  group_by(
    # to_country
           # , 
    group
           ) %>% 
  count(change)


rankings_wide %>% 
  filter((year == 2016 &
            to_country == "Armenia") |
           (year == 2021 &
              to_country == "San Marino") | 
           (year == 2022 &
              to_country == "Estonia")) %>% 
  mutate(year_label = 
           case_when(year == 2016 ~ "2016: Armenia",
                     year == 2021 ~ "2021: San Marino",
                     year == 2022 ~ "2022: Estonia")) %>% 
  select(-c(running_order_semi,
            running_order_final,
            televote_ranking_semi,
            televote_ranking_final)) %>% 
  rename(ranking_semi = 
           jury_ranking_semi,
         ranking_final = 
           jury_ranking_final) %>% 
  mutate(group = "Jury") %>% 
  bind_rows(rankings_wide %>% 
              filter((year == 2016 &
                        to_country == "Armenia") |
                       (year == 2021 &
                          to_country == "San Marino") | 
                       (year == 2022 &
                          to_country == "Estonia")) %>% 
              mutate(year_label = 
                       case_when(year == 2016 ~ "2016: Armenia",
                                 year == 2021 ~ "2021: San Marino",
                                 year == 2022 ~ "2022: Estonia")) %>% 
              select(-c(running_order_semi,
                        running_order_final,
                        jury_ranking_semi,
                        jury_ranking_final)) %>% 
              rename(ranking_semi = 
                       televote_ranking_semi,
                     ranking_final = 
                       televote_ranking_final) %>% 
              mutate(group = "Televote") ) %>% 
  mutate(change = 
           ranking_semi - ranking_final) %>% 
  arrange(-change) %>% 
  filter(group == "Jury")

# look at first position
rankings_wide %>% 
  mutate(televote_discrepancy = 
           (televote_ranking_semi - televote_ranking_final),
         jury_discrepancy = 
           (jury_ranking_semi - jury_ranking_final))%>% 
  filter(running_order_final == 1) %>%
  na.omit %>% 
  group_by(year) %>% 
  summarise(televote_discrepancy = 
              mean(televote_discrepancy),
            jury_discrepancy = 
              mean(jury_discrepancy)) %>% 
  arrange(televote_discrepancy) 


rankings_wide %>% 
  mutate(televote_discrepancy = 
           (televote_ranking_semi - televote_ranking_final),
         jury_discrepancy = 
           (jury_ranking_semi - jury_ranking_final))%>% 
  filter(running_order_final == 1) %>% 
  filter(year == 2022) %>% 
  select(from_country, televote_discrepancy, televote_ranking_semi, televote_ranking_final)


rankings_wide %>% 
  mutate(jury_discrepancy = 
           jury_ranking_final - 
           jury_ranking_semi) %>% 
  mutate(televote_discrepancy = 
           televote_ranking_final - 
           televote_ranking_semi) %>% 
  select(from_country,
         to_country,
         year,
         jury_discrepancy,
         televote_discrepancy,
         running_order_final)  %>% 
  na.omit %>% 
  group_by(to_country,
           year,
           running_order_final) %>% 
  summarise(mean_jury_discrepancy = 
              mean(jury_discrepancy),
            mean_televote_discrepancy = 
              mean(televote_discrepancy)) %>% 
  group_by(running_order_final) %>% 
  summarise(mean_jury_discrepancy = 
              mean(mean_jury_discrepancy),
            mean_televote_discrepancy = 
              mean(mean_televote_discrepancy)) %>% 
  mutate(mean_overall_discrepancy = mean_jury_discrepancy + mean_televote_discrepancy) %>% 
  arrange(mean_overall_discrepancy)


rankings_wide %>% 
  mutate(jury_discrepancy = 
           jury_ranking_final - 
           jury_ranking_semi) %>% 
  mutate(televote_discrepancy = 
           televote_ranking_final - 
           televote_ranking_semi) %>% 
  select(from_country,
         to_country,
         year,
         jury_discrepancy,
         televote_discrepancy,
         running_order_final)  %>% 
  na.omit %>% 
  group_by(to_country,
           year,
           running_order_final) %>% 
  summarise(mean_jury_discrepancy = 
              mean(jury_discrepancy),
            mean_televote_discrepancy = 
              mean(televote_discrepancy)) %>% 
  group_by(running_order_final) %>% 
  summarise(mean_jury_discrepancy = 
              mean(mean_jury_discrepancy),
            mean_televote_discrepancy = 
              mean(mean_televote_discrepancy)) %>% 
  mutate(abs_jury_discrepancy = 
           abs(mean_jury_discrepancy),
         abs_televote_discrepancy = 
           abs(mean_televote_discrepancy)) %>% 
  # filter(running_order_final != 1) %>% 
  summarise(jury = 
              mean(abs_jury_discrepancy),
            televote = 
              mean(abs_televote_discrepancy))


rankings_wide %>% 
  mutate(televote_discrepancy = 
           (televote_ranking_semi - televote_ranking_final),
         jury_discrepancy = 
           (jury_ranking_semi - jury_ranking_final))%>% 
  filter(running_order_final == 13) %>% 
  # filter(year == 2022) %>% 
  select(from_country, 
         to_country,
         televote_discrepancy, jury_discrepancy, 
         year,
         televote_ranking_semi, televote_ranking_final,
         jury_ranking_semi, jury_ranking_final) %>% 
  arrange(televote_discrepancy) %>% 
  na.omit %>% 
  group_by(year) %>% 
  summarise(televote_discrepancy = 
           mean(televote_discrepancy),
           jury_discrepancy = 
             mean(jury_discrepancy))

# 2016 is interesting
rankings_wide %>% 
  mutate(televote_discrepancy = 
           (televote_ranking_semi - televote_ranking_final),
         jury_discrepancy = 
           (jury_ranking_semi - jury_ranking_final))%>% 
  filter(running_order_final == 13) %>% 
  filter(year == 2016) %>% 
  select(from_country, jury_discrepancy, jury_ranking_semi, jury_ranking_final)

all_rankings %>% 
  filter(year == 2016) %>% 
  filter(event_type == "semi-final 2") %>% 
  select(to_country, running_order_position) %>% 
  distinct()

rankings_wide %>% 
  filter(year == 2016 &
           to_country == "Australia") %>% 
  mutate(jury_discrepancy = 
           jury_ranking_final - 
           jury_ranking_semi) %>% 
  arrange(jury_discrepancy)
