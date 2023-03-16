# eurovision_running_order
What are the implications of final position in Eurovision?

## Getting the results 

If you're just interested in producing the figures in LINK LINK LINK, you want running_order_position.R. The figures are the ggplots, the table can be found under the comment "stray maths for detail".

## Nerd stuff (scripts)

You are my people.

**getting_started.R** is a scrappy script I put together when I originally had the idea for this project, just to see how the Eurovision-hosted data looked. 

**getting_web_data.R** exists because Eurovision hosts its voting behaviour in a relatively manageable format online, but on loads of different pages. 

This script does a few things:
- pulls data from the Eurovision pages with the full rankings from each jury and televote for each semi-final and final since 2017 (as well as individual jurors, but I don't use that data)
- pulls data from Wikipedia for those cases where Eurovision doesn't host the full data - for example, this includes televotes for the countries whose jury votes were a composite of other juries due to suspicious activity in semi-final 2 in 2022
- pulls running orders from the Eurovision pages for each semi-final and Grand Final since 2014
- exports each of these things as a csv

Please note that this is basically all commented out because I don't want to repeatedly hammer the Eurovision pages with requests. 

**cleaning_web_data.R** also does a few things:

- loads the data from 2014-2016 hosted [here](https://eurovision.tv/history/full-split-results), which I'd already downloaded and converted to csv (there's something up wth the xls files that readxl isn't keen on)
- loads the data produced in getting_web_data.R
- generates a consistent set of variables
- cleans up annoyingly-formatted columns (eg changes "7 points 4th" to 4)

Finally, **running_order_position.R** produces the analysis published online.

## Nerd stuff (csvs)

Naming convention as follows:
- anything starting **ESC** is downloaded direct from the ESC pages
- anything starting **ro_** is a running order: the other parts of the file name are the year and the event (so ro_2014_semi_1.csv is the running order for the first semi-final in 2014)
- anything starting y_ is a set of rankings for both juries and televotes: the other parts of the file name are the year and the event (so y_2017_semi_1.csv is the rankings for the first semi-final in 2017)
- anything including WITH_ includes data that I've pulled from Wikipedia where relevant data wasn't available from ESC. This needs some health warnings - the differences between these files and the ones without WITH_ are cases where the rankings were in some way suspicious. 
- **all_rankings.csv** is the fle with everything in it, that (for most people?) will be the one you want to include

Basically, everything other than **all_rankings.csv** is me showing my working.
