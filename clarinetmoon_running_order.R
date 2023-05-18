# do what i promised here
# https://twitter.com/clarinetmoonesc/status/1658970218519986176

# load packages
library(tidyverse)
library(janitor)
library(hrbrthemes)
library(patchwork)

# downloaded this from google sheets

clarinetmoon <- 
  read_csv("Running Order Stats - ESC Entries.csv",
           skip = 1) %>% 
  clean_names()

clarinetmoon %>% 
  select(percent_4, percent_17) %>% 
  mutate(percent_4 = 
           str_sub(percent_4, 
                   1, -2) %>% 
           as.numeric())%>% 
  mutate(percent_17 = 
           str_sub(percent_17, 
                   1, -2) %>% 
           as.numeric()) %>% 
  ggplot() + 
  aes(x = percent_4,
      y = percent_17) + 
  geom_density2d_filled() + 
  theme_ipsum_ps() + 
  theme(legend.position = "none")  + 
  coord_fixed() + 
  labs(x = "Running order\n(% of acts that performed before you)",
       y = "Grand Final placement\n(% of acts that placed below you)",
       fill = "")


clarinetmoon %>% 
  select(percent_4, percent_17) %>% 
  mutate(percent_4 = 
           str_sub(percent_4, 
                   1, -2) %>% 
           as.numeric())%>% 
  mutate(percent_17 = 
           str_sub(percent_17, 
                   1, -2) %>% 
           as.numeric()) %>% 
  ggplot() + 
  aes(x = percent_4,
      y = percent_17) + 
  geom_bin2d(bins = 15) + 
  scale_fill_viridis_c() + 
  theme_ipsum_ps() + 
  coord_fixed() + 
  theme(legend.position = "none") + 
  labs(x = "Running order\n(% of acts that performed before you)",
       y = "Grand Final placement\n(% of acts that placed below you)",
       fill = "")

