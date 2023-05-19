install.packages("academictwitteR")
install.packages("sentimentr")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("corrplot")
install.packages("ggplot2")
install.packages("plotly")
install.packages("plyr")
install.packages("highcharter")
install.packages("ggpubr")
install.packages("rstatix")

library(academictwitteR)
library(sentimentr)
library(tidyverse)
library(dplyr)
library(corrplot)
library(ggplot2)
library(plotly)
library(plyr)
library(highcharter)
library(ggpubr)
library(rstatix)
set_bearer()
get_bearer()





### Two ANOVA Tests (and Two Post-Hoc Tests if necessary) - Wins at 
### Home Games (group 1) versus Away Games (group 2) from NOP 2019-20 Season

## New Orleans Pelicans 2019-20 Home Wins (Games #1-15)
#1 vs DEN on 10/31/19
#2 vs LAC on 11/9/19
#3 vs GSW on 11/17/19
#4 vs POR on 11/19/19
#5 vs IND on 12/28/19
#6 vs HOU on 12/29/19
#7 vs CHI on 1/8/20
#8 vs UTA on 1/16/20
#9 vs BOS on 1/26/20
#10 vs MEM on 1/31/20
#11 vs POR on 2/11/20
#12 vs CLE on 2/28/20
#13 vs MIA on 3/6/20
#14 vs MEM on 8/3/20
#15 vs WAS on 8/7/20

## New Orleans Pelicans 2019-20 Away Wins (Games #16-30)
#16 vs cHA on 11/9/19
#17 vs PHX on 11/21/19
#18 vs MIN on 12/18/19
#19 vs POR on 12/23/19
#20 vs DEN on 12/25/19
#21 vs SAC on 1/4/20
#22 vs NYK on 1/10/20
#23 vs DET on 1/13/20
#24 vs MEM on 1/20/20
#25 vs CLE on 1/28/20
#26 vs CHI on 2/6/20
#27 vs IND on 2/8/20
#28 vs POR on 2/21/20
#29 vs GSW on 2/23/20
#30 vs MIN on 3/8/20





### Data Collection

## gm_1
gm1_tweets <-
  get_all_tweets(
    query = "@PelicansNBA",
    start_tweets = "2019-10-31T07:00:00Z",
    end_tweets = "2019-11-01T06:59:00Z",
    data_path = "anova_gm_1/",
    bind_tweets = FALSE,
    n = 1000
  )
gm1_tweets <- bind_tweets(data_path = "anova_gm_1/")

gm1_tweets$text <- gsub("[^0-9A-Za-z///' ]","", gm1_tweets$text)
gm1_tweets$text <- gsub("http\\w+", "", gm1_tweets$text)
gm1_tweets$text <- gsub("rt", "", gm1_tweets$text)
gm1_tweets$text <- gsub("@\\w+", "", gm1_tweets$text)
gm1_tweets$text <- tolower(gm1_tweets$text)

sent_gm1_tweets <- sentiment(gm1_tweets$text)
gm1_tweets$sentiment <- sent_gm1_tweets$sentiment

gm1_tweets <- gm1_tweets %>% mutate(game_id = "gm_1_den")
gm1_tweets <- gm1_tweets %>% mutate(location_id = "home")

rand_gm1_tweets <- gm1_tweets[sample(nrow(gm1_tweets), size=1000), ]
# View(rand_gm1_tweets)


## gm_2
gm2_tweets <-
  get_all_tweets(
    query = "@PelicansNBA",
    start_tweets = "2019-11-14T07:00:00Z",
    end_tweets = "2019-11-15T06:59:00Z",
    data_path = "anova_gm_2/",
    bind_tweets = FALSE,
    n = 1000
  )
gm2_tweets <- bind_tweets(data_path = "anova_gm_2/")

gm2_tweets$text <- gsub("[^0-9A-Za-z///' ]","", gm2_tweets$text)
gm2_tweets$text <- gsub("http\\w+", "", gm2_tweets$text)
gm2_tweets$text <- gsub("rt", "", gm2_tweets$text)
gm2_tweets$text <- gsub("@\\w+", "", gm2_tweets$text)
gm2_tweets$text <- tolower(gm2_tweets$text)

sent_gm2_tweets <- sentiment(gm2_tweets$text)
gm2_tweets$sentiment <- sent_gm2_tweets$sentiment

gm2_tweets <- gm2_tweets %>% mutate(game_id = "gm_2_lac")
gm2_tweets <- gm2_tweets %>% mutate(location_id = "home")

rand_gm2_tweets <- gm2_tweets[sample(nrow(gm2_tweets), size=1000), ]
# View(rand_gm2_tweets)


## gm_3
gm3_tweets <-
  get_all_tweets(
    query = "@PelicansNBA",
    start_tweets = "2019-11-17T07:00:00Z",
    end_tweets = "2019-11-18T06:59:00Z",
    data_path = "anova_gm_3/",
    bind_tweets = FALSE,
    n = 1000
  )
gm3_tweets <- bind_tweets(data_path = "anova_gm_3/")

gm3_tweets$text <- gsub("[^0-9A-Za-z///' ]","", gm3_tweets$text)
gm3_tweets$text <- gsub("http\\w+", "", gm3_tweets$text)
gm3_tweets$text <- gsub("rt", "", gm3_tweets$text)
gm3_tweets$text <- gsub("@\\w+", "", gm3_tweets$text)
gm3_tweets$text <- tolower(gm3_tweets$text)

sent_gm3_tweets <- sentiment(gm3_tweets$text)
gm3_tweets$sentiment <- sent_gm3_tweets$sentiment

gm3_tweets <- gm3_tweets %>% mutate(game_id = "gm_3_gsw")
gm3_tweets <- gm3_tweets %>% mutate(location_id = "home")

rand_gm3_tweets <- gm3_tweets[sample(nrow(gm3_tweets), size=1000), ]
# View(rand_gm3_tweets)


## gm_4
gm4_tweets <-
  get_all_tweets(
    query = "@PelicansNBA",
    start_tweets = "2019-11-19T07:00:00Z",
    end_tweets = "2019-11-20T06:59:00Z",
    data_path = "anova_gm_4/",
    bind_tweets = FALSE,
    n = 1000
  )
gm4_tweets <- bind_tweets(data_path = "anova_gm_4/")

gm4_tweets$text <- gsub("[^0-9A-Za-z///' ]","", gm4_tweets$text)
gm4_tweets$text <- gsub("http\\w+", "", gm4_tweets$text)
gm4_tweets$text <- gsub("rt", "", gm4_tweets$text)
gm4_tweets$text <- gsub("@\\w+", "", gm4_tweets$text)
gm4_tweets$text <- tolower(gm4_tweets$text)

sent_gm4_tweets <- sentiment(gm4_tweets$text)
gm4_tweets$sentiment <- sent_gm4_tweets$sentiment

gm4_tweets <- gm4_tweets %>% mutate(game_id = "gm_4_por")
gm4_tweets <- gm4_tweets %>% mutate(location_id = "home")

rand_gm4_tweets <- gm4_tweets[sample(nrow(gm4_tweets), size=1000), ]
# View(rand_gm4_tweets)


## gm_5
gm5_tweets <-
  get_all_tweets(
    query = "@PelicansNBA",
    start_tweets = "2019-12-28T07:00:00Z",
    end_tweets = "2019-12-29T06:59:00Z",
    data_path = "anova_gm_5/",
    bind_tweets = FALSE,
    n = 1000
  )
gm5_tweets <- bind_tweets(data_path = "anova_gm_5/")

gm5_tweets$text <- gsub("[^0-9A-Za-z///' ]","", gm5_tweets$text)
gm5_tweets$text <- gsub("http\\w+", "", gm5_tweets$text)
gm5_tweets$text <- gsub("rt", "", gm5_tweets$text)
gm5_tweets$text <- gsub("@\\w+", "", gm5_tweets$text)
gm5_tweets$text <- tolower(gm5_tweets$text)

sent_gm5_tweets <- sentiment(gm5_tweets$text)
gm5_tweets$sentiment <- sent_gm5_tweets$sentiment

gm5_tweets <- gm5_tweets %>% mutate(game_id = "gm_5_ind")
gm5_tweets <- gm5_tweets %>% mutate(location_id = "home")

rand_gm5_tweets <- gm5_tweets[sample(nrow(gm5_tweets), size=1000), ]
# View(rand_gm5_tweets)


## gm_6
gm6_tweets <-
  get_all_tweets(
    query = "@PelicansNBA",
    start_tweets = "2019-12-29T07:00:00Z",
    end_tweets = "2019-12-30T06:59:00Z",
    data_path = "anova_gm_6/",
    bind_tweets = FALSE,
    n = 1000
  )
gm6_tweets <- bind_tweets(data_path = "anova_gm_6/")

gm6_tweets$text <- gsub("[^0-9A-Za-z///' ]","", gm6_tweets$text)
gm6_tweets$text <- gsub("http\\w+", "", gm6_tweets$text)
gm6_tweets$text <- gsub("rt", "", gm6_tweets$text)
gm6_tweets$text <- gsub("@\\w+", "", gm6_tweets$text)
gm6_tweets$text <- tolower(gm6_tweets$text)

sent_gm6_tweets <- sentiment(gm6_tweets$text)
gm6_tweets$sentiment <- sent_gm6_tweets$sentiment

gm6_tweets <- gm6_tweets %>% mutate(game_id = "gm_6_hou")
gm6_tweets <- gm6_tweets %>% mutate(location_id = "home")

rand_gm6_tweets <- gm6_tweets[sample(nrow(gm6_tweets), size=1000), ]
# View(rand_gm6_tweets)


## gm_7
gm7_tweets <-
  get_all_tweets(
    query = "@PelicansNBA",
    start_tweets = "2020-01-08T07:00:00Z",
    end_tweets = "2020-01-09T06:59:00Z",
    data_path = "anova_gm_7/",
    bind_tweets = FALSE,
    n = 1000
  )
gm7_tweets <- bind_tweets(data_path = "anova_gm_7/")

gm7_tweets$text <- gsub("[^0-9A-Za-z///' ]","", gm7_tweets$text)
gm7_tweets$text <- gsub("http\\w+", "", gm7_tweets$text)
gm7_tweets$text <- gsub("rt", "", gm7_tweets$text)
gm7_tweets$text <- gsub("@\\w+", "", gm7_tweets$text)
gm7_tweets$text <- tolower(gm7_tweets$text)

sent_gm7_tweets <- sentiment(gm7_tweets$text)
gm7_tweets$sentiment <- sent_gm7_tweets$sentiment

gm7_tweets <- gm7_tweets %>% mutate(game_id = "gm_7_chi")
gm7_tweets <- gm7_tweets %>% mutate(location_id = "home")

rand_gm7_tweets <- gm7_tweets[sample(nrow(gm7_tweets), size=1000), ]
# View(rand_gm7_tweets)


## gm_8
gm8_tweets <-
  get_all_tweets(
    query = "@PelicansNBA",
    start_tweets = "2020-01-16T07:00:00Z",
    end_tweets = "2020-01-17T06:59:00Z",
    data_path = "anova_gm_8/",
    bind_tweets = FALSE,
    n = 1000
  )
gm8_tweets <- bind_tweets(data_path = "anova_gm_8/")

gm8_tweets$text <- gsub("[^0-9A-Za-z///' ]","", gm8_tweets$text)
gm8_tweets$text <- gsub("http\\w+", "", gm8_tweets$text)
gm8_tweets$text <- gsub("rt", "", gm8_tweets$text)
gm8_tweets$text <- gsub("@\\w+", "", gm8_tweets$text)
gm8_tweets$text <- tolower(gm8_tweets$text)

sent_gm8_tweets <- sentiment(gm8_tweets$text)
gm8_tweets$sentiment <- sent_gm8_tweets$sentiment

gm8_tweets <- gm8_tweets %>% mutate(game_id = "gm_8_uta")
gm8_tweets <- gm8_tweets %>% mutate(location_id = "home")

rand_gm8_tweets <- gm8_tweets[sample(nrow(gm8_tweets), size=1000), ]
# View(rand_gm8_tweets)


## gm_9
gm9_tweets <-
  get_all_tweets(
    query = "@PelicansNBA",
    start_tweets = "2020-01-26T07:00:00Z",
    end_tweets = "2020-01-27T06:59:00Z",
    data_path = "anova_gm_9/",
    bind_tweets = FALSE,
    n = 1000
  )
gm9_tweets <- bind_tweets(data_path = "anova_gm_9/")

gm9_tweets$text <- gsub("[^0-9A-Za-z///' ]","", gm9_tweets$text)
gm9_tweets$text <- gsub("http\\w+", "", gm9_tweets$text)
gm9_tweets$text <- gsub("rt", "", gm9_tweets$text)
gm9_tweets$text <- gsub("@\\w+", "", gm9_tweets$text)
gm9_tweets$text <- tolower(gm9_tweets$text)

sent_gm9_tweets <- sentiment(gm9_tweets$text)
gm9_tweets$sentiment <- sent_gm9_tweets$sentiment

gm9_tweets <- gm9_tweets %>% mutate(game_id = "gm_9_bos")
gm9_tweets <- gm9_tweets %>% mutate(location_id = "home")

rand_gm9_tweets <- gm9_tweets[sample(nrow(gm9_tweets), size=1000), ]
# View(rand_gm9_tweets)


## gm_10
gm10_tweets <-
  get_all_tweets(
    query = "@PelicansNBA",
    start_tweets = "2020-01-31T07:00:00Z",
    end_tweets = "2020-02-01T06:59:00Z",
    data_path = "anova_gm_10/",
    bind_tweets = FALSE,
    n = 1000
  )
gm10_tweets <- bind_tweets(data_path = "anova_gm_10/")

gm10_tweets$text <- gsub("[^0-9A-Za-z///' ]","", gm10_tweets$text)
gm10_tweets$text <- gsub("http\\w+", "", gm10_tweets$text)
gm10_tweets$text <- gsub("rt", "", gm10_tweets$text)
gm10_tweets$text <- gsub("@\\w+", "", gm10_tweets$text)
gm10_tweets$text <- tolower(gm10_tweets$text)

sent_gm10_tweets <- sentiment(gm10_tweets$text)
gm10_tweets$sentiment <- sent_gm10_tweets$sentiment

gm10_tweets <- gm10_tweets %>% mutate(game_id = "gm_10_mem")
gm10_tweets <- gm10_tweets %>% mutate(location_id = "home")

rand_gm10_tweets <- gm10_tweets[sample(nrow(gm10_tweets), size=1000), ]
# View(rand_gm10_tweets)


## gm_11
gm11_tweets <-
  get_all_tweets(
    query = "@PelicansNBA",
    start_tweets = "2020-02-11T07:00:00Z",
    end_tweets = "2020-02-12T06:59:00Z",
    data_path = "anova_gm_11/",
    bind_tweets = FALSE,
    n = 1000
  )
gm11_tweets <- bind_tweets(data_path = "anova_gm_11/")

gm11_tweets$text <- gsub("[^0-9A-Za-z///' ]","", gm11_tweets$text)
gm11_tweets$text <- gsub("http\\w+", "", gm11_tweets$text)
gm11_tweets$text <- gsub("rt", "", gm11_tweets$text)
gm11_tweets$text <- gsub("@\\w+", "", gm11_tweets$text)
gm11_tweets$text <- tolower(gm11_tweets$text)

sent_gm11_tweets <- sentiment(gm11_tweets$text)
gm11_tweets$sentiment <- sent_gm11_tweets$sentiment

gm11_tweets <- gm11_tweets %>% mutate(game_id = "gm_11_por")
gm11_tweets <- gm11_tweets %>% mutate(location_id = "home")

rand_gm11_tweets <- gm11_tweets[sample(nrow(gm11_tweets), size=1000), ]
# View(rand_gm11_tweets)


## gm_12
gm12_tweets <-
  get_all_tweets(
    query = "@PelicansNBA",
    start_tweets = "2020-02-28T07:00:00Z",
    end_tweets = "2020-02-29T06:59:00Z",
    data_path = "anova_gm_12/",
    bind_tweets = FALSE,
    n = 1000
  )
gm12_tweets <- bind_tweets(data_path = "anova_gm_12/")

gm12_tweets$text <- gsub("[^0-9A-Za-z///' ]","", gm12_tweets$text)
gm12_tweets$text <- gsub("http\\w+", "", gm12_tweets$text)
gm12_tweets$text <- gsub("rt", "", gm12_tweets$text)
gm12_tweets$text <- gsub("@\\w+", "", gm12_tweets$text)
gm12_tweets$text <- tolower(gm12_tweets$text)

sent_gm12_tweets <- sentiment(gm12_tweets$text)
gm12_tweets$sentiment <- sent_gm12_tweets$sentiment

gm12_tweets <- gm12_tweets %>% mutate(game_id = "gm_12_cle")
gm12_tweets <- gm12_tweets %>% mutate(location_id = "home")

rand_gm12_tweets <- gm12_tweets[sample(nrow(gm12_tweets), size=1000), ]
# View(rand_gm12_tweets)


## gm_13
gm13_tweets <-
  get_all_tweets(
    query = "@PelicansNBA",
    start_tweets = "2020-03-06T07:00:00Z",
    end_tweets = "2020-03-07T06:59:00Z",
    data_path = "anova_gm_13/",
    bind_tweets = FALSE,
    n = 1000
  )
gm13_tweets <- bind_tweets(data_path = "anova_gm_13/")

gm13_tweets$text <- gsub("[^0-9A-Za-z///' ]","", gm13_tweets$text)
gm13_tweets$text <- gsub("http\\w+", "", gm13_tweets$text)
gm13_tweets$text <- gsub("rt", "", gm13_tweets$text)
gm13_tweets$text <- gsub("@\\w+", "", gm13_tweets$text)
gm13_tweets$text <- tolower(gm13_tweets$text)

sent_gm13_tweets <- sentiment(gm13_tweets$text)
gm13_tweets$sentiment <- sent_gm13_tweets$sentiment

gm13_tweets <- gm13_tweets %>% mutate(game_id = "gm_13_mia")
gm13_tweets <- gm13_tweets %>% mutate(location_id = "home")

rand_gm13_tweets <- gm13_tweets[sample(nrow(gm13_tweets), size=1000), ]
# View(rand_gm13_tweets)


## gm_14
gm14_tweets <-
  get_all_tweets(
    query = "@PelicansNBA",
    start_tweets = "2020-08-03T07:00:00Z",
    end_tweets = "2020-08-04T06:59:00Z",
    data_path = "anova_gm_14/",
    bind_tweets = FALSE,
    n = 1000
  )
gm14_tweets <- bind_tweets(data_path = "anova_gm_14/")

gm14_tweets$text <- gsub("[^0-9A-Za-z///' ]","", gm14_tweets$text)
gm14_tweets$text <- gsub("http\\w+", "", gm14_tweets$text)
gm14_tweets$text <- gsub("rt", "", gm14_tweets$text)
gm14_tweets$text <- gsub("@\\w+", "", gm14_tweets$text)
gm14_tweets$text <- tolower(gm14_tweets$text)

sent_gm14_tweets <- sentiment(gm14_tweets$text)
gm14_tweets$sentiment <- sent_gm14_tweets$sentiment

gm14_tweets <- gm14_tweets %>% mutate(game_id = "gm_14_mem")
gm14_tweets <- gm14_tweets %>% mutate(location_id = "home")

rand_gm14_tweets <- gm14_tweets[sample(nrow(gm14_tweets), size=1000), ]
# View(rand_gm14_tweets)


## gm_15
gm15_tweets <-
  get_all_tweets(
    query = "@PelicansNBA",
    start_tweets = "2020-08-07T07:00:00Z",
    end_tweets = "2020-08-08T06:59:00Z",
    data_path = "anova_gm_15/",
    bind_tweets = FALSE,
    n = 1000
  )
gm15_tweets <- bind_tweets(data_path = "anova_gm_15/")

gm15_tweets$text <- gsub("[^0-9A-Za-z///' ]","", gm15_tweets$text)
gm15_tweets$text <- gsub("http\\w+", "", gm15_tweets$text)
gm15_tweets$text <- gsub("rt", "", gm15_tweets$text)
gm15_tweets$text <- gsub("@\\w+", "", gm15_tweets$text)
gm15_tweets$text <- tolower(gm15_tweets$text)

sent_gm15_tweets <- sentiment(gm15_tweets$text)
gm15_tweets$sentiment <- sent_gm15_tweets$sentiment

gm15_tweets <- gm15_tweets %>% mutate(game_id = "gm_15_was")
gm15_tweets <- gm15_tweets %>% mutate(location_id = "home")

rand_gm15_tweets <- gm15_tweets[sample(nrow(gm15_tweets), size=1000), ]
# View(rand_gm15_tweets)


## gm_16
gm16_tweets <-
  get_all_tweets(
    query = "@PelicansNBA",
    start_tweets = "2019-11-09T07:00:00Z",
    end_tweets = "2019-11-10T06:59:00Z",
    data_path = "anova_gm_16/",
    bind_tweets = FALSE,
    n = 1000
  )
gm16_tweets <- bind_tweets(data_path = "anova_gm_16/")

gm16_tweets$text <- gsub("[^0-9A-Za-z///' ]","", gm16_tweets$text)
gm16_tweets$text <- gsub("http\\w+", "", gm16_tweets$text)
gm16_tweets$text <- gsub("rt", "", gm16_tweets$text)
gm16_tweets$text <- gsub("@\\w+", "", gm16_tweets$text)
gm16_tweets$text <- tolower(gm16_tweets$text)

sent_gm16_tweets <- sentiment(gm16_tweets$text)
gm16_tweets$sentiment <- sent_gm16_tweets$sentiment

gm16_tweets <- gm16_tweets %>% mutate(game_id = "gm_16_cha")
gm16_tweets <- gm16_tweets %>% mutate(location_id = "away")

rand_gm16_tweets <- gm16_tweets[sample(nrow(gm16_tweets), size=1000), ]
# View(rand_gm16_tweets)


## gm_17
gm17_tweets <-
  get_all_tweets(
    query = "@PelicansNBA",
    start_tweets = "2019-11-21T07:00:00Z",
    end_tweets = "2019-11-22T06:59:00Z",
    data_path = "anova_gm_17/",
    bind_tweets = FALSE,
    n = 1000
  )
gm17_tweets <- bind_tweets(data_path = "anova_gm_17/")

gm17_tweets$text <- gsub("[^0-9A-Za-z///' ]","", gm17_tweets$text)
gm17_tweets$text <- gsub("http\\w+", "", gm17_tweets$text)
gm17_tweets$text <- gsub("rt", "", gm17_tweets$text)
gm17_tweets$text <- gsub("@\\w+", "", gm17_tweets$text)
gm17_tweets$text <- tolower(gm17_tweets$text)

sent_gm17_tweets <- sentiment(gm17_tweets$text)
gm17_tweets$sentiment <- sent_gm17_tweets$sentiment

gm17_tweets <- gm17_tweets %>% mutate(game_id = "gm_17_phx")
gm17_tweets <- gm17_tweets %>% mutate(location_id = "away")

rand_gm17_tweets <- gm17_tweets[sample(nrow(gm17_tweets), size=1000), ]
# View(rand_gm17_tweets)


## gm_18
gm18_tweets <-
  get_all_tweets(
    query = "@PelicansNBA",
    start_tweets = "2019-12-18T07:00:00Z",
    end_tweets = "2019-12-19T06:59:00Z",
    data_path = "anova_gm_18/",
    bind_tweets = FALSE,
    n = 1000
  )
gm18_tweets <- bind_tweets(data_path = "anova_gm_18/")

gm18_tweets$text <- gsub("[^0-9A-Za-z///' ]","", gm18_tweets$text)
gm18_tweets$text <- gsub("http\\w+", "", gm18_tweets$text)
gm18_tweets$text <- gsub("rt", "", gm18_tweets$text)
gm18_tweets$text <- gsub("@\\w+", "", gm18_tweets$text)
gm18_tweets$text <- tolower(gm18_tweets$text)

sent_gm18_tweets <- sentiment(gm18_tweets$text)
gm18_tweets$sentiment <- sent_gm18_tweets$sentiment

gm18_tweets <- gm18_tweets %>% mutate(game_id = "gm_18_min")
gm18_tweets <- gm18_tweets %>% mutate(location_id = "away")

rand_gm18_tweets <- gm18_tweets[sample(nrow(gm18_tweets), size=1000), ]
# View(rand_gm18_tweets)


## gm_19
gm19_tweets <-
  get_all_tweets(
    query = "@PelicansNBA",
    start_tweets = "2019-12-23T07:00:00Z",
    end_tweets = "2019-12-24T06:59:00Z",
    data_path = "anova_gm_19/",
    bind_tweets = FALSE,
    n = 1000
  )
gm19_tweets <- bind_tweets(data_path = "anova_gm_19/")

gm19_tweets$text <- gsub("[^0-9A-Za-z///' ]","", gm19_tweets$text)
gm19_tweets$text <- gsub("http\\w+", "", gm19_tweets$text)
gm19_tweets$text <- gsub("rt", "", gm19_tweets$text)
gm19_tweets$text <- gsub("@\\w+", "", gm19_tweets$text)
gm19_tweets$text <- tolower(gm19_tweets$text)

sent_gm19_tweets <- sentiment(gm19_tweets$text)
gm19_tweets$sentiment <- sent_gm19_tweets$sentiment

gm19_tweets <- gm19_tweets %>% mutate(game_id = "gm_19_por")
gm19_tweets <- gm19_tweets %>% mutate(location_id = "away")

rand_gm19_tweets <- gm19_tweets[sample(nrow(gm19_tweets), size=1000), ]
# View(rand_gm19_tweets)


## gm_20
gm20_tweets <-
  get_all_tweets(
    query = "@PelicansNBA",
    start_tweets = "2019-12-25T07:00:00Z",
    end_tweets = "2019-12-26T06:59:00Z",
    data_path = "anova_gm_20/",
    bind_tweets = FALSE,
    n = 1000
  )
gm20_tweets <- bind_tweets(data_path = "anova_gm_20/")

gm20_tweets$text <- gsub("[^0-9A-Za-z///' ]","", gm20_tweets$text)
gm20_tweets$text <- gsub("http\\w+", "", gm20_tweets$text)
gm20_tweets$text <- gsub("rt", "", gm20_tweets$text)
gm20_tweets$text <- gsub("@\\w+", "", gm20_tweets$text)
gm20_tweets$text <- tolower(gm20_tweets$text)

sent_gm20_tweets <- sentiment(gm20_tweets$text)
gm20_tweets$sentiment <- sent_gm20_tweets$sentiment

gm20_tweets <- gm20_tweets %>% mutate(game_id = "gm_20_den")
gm20_tweets <- gm20_tweets %>% mutate(location_id = "away")

rand_gm20_tweets <- gm20_tweets[sample(nrow(gm20_tweets), size=1000), ]
# View(rand_gm20_tweets)


## gm_21
gm21_tweets <-
  get_all_tweets(
    query = "@PelicansNBA",
    start_tweets = "2020-01-04T07:00:00Z",
    end_tweets = "2020-01-05T06:59:00Z",
    data_path = "anova_gm_21/",
    bind_tweets = FALSE,
    n = 1000
  )
gm21_tweets <- bind_tweets(data_path = "anova_gm_21/")

gm21_tweets$text <- gsub("[^0-9A-Za-z///' ]","", gm21_tweets$text)
gm21_tweets$text <- gsub("http\\w+", "", gm21_tweets$text)
gm21_tweets$text <- gsub("rt", "", gm21_tweets$text)
gm21_tweets$text <- gsub("@\\w+", "", gm21_tweets$text)
gm21_tweets$text <- tolower(gm21_tweets$text)

sent_gm21_tweets <- sentiment(gm21_tweets$text)
gm21_tweets$sentiment <- sent_gm21_tweets$sentiment

gm21_tweets <- gm21_tweets %>% mutate(game_id = "gm_21_sac")
gm21_tweets <- gm21_tweets %>% mutate(location_id = "away")

rand_gm21_tweets <- gm21_tweets[sample(nrow(gm21_tweets), size=1000), ]
# View(rand_gm21_tweets)


## gm_22
gm22_tweets <-
  get_all_tweets(
    query = "@PelicansNBA",
    start_tweets = "2020-01-10T07:00:00Z",
    end_tweets = "2020-01-11T06:59:00Z",
    data_path = "anova_gm_22/",
    bind_tweets = FALSE,
    n = 1000
  )
gm22_tweets <- bind_tweets(data_path = "anova_gm_22/")

gm22_tweets$text <- gsub("[^0-9A-Za-z///' ]","", gm22_tweets$text)
gm22_tweets$text <- gsub("http\\w+", "", gm22_tweets$text)
gm22_tweets$text <- gsub("rt", "", gm22_tweets$text)
gm22_tweets$text <- gsub("@\\w+", "", gm22_tweets$text)
gm22_tweets$text <- tolower(gm22_tweets$text)

sent_gm22_tweets <- sentiment(gm22_tweets$text)
gm22_tweets$sentiment <- sent_gm22_tweets$sentiment

gm22_tweets <- gm22_tweets %>% mutate(game_id = "gm_22_nyk")
gm22_tweets <- gm22_tweets %>% mutate(location_id = "away")

rand_gm22_tweets <- gm22_tweets[sample(nrow(gm22_tweets), size=1000), ]
# View(rand_gm22_tweets)


## gm_23
gm23_tweets <-
  get_all_tweets(
    query = "@PelicansNBA",
    start_tweets = "2020-01-13T07:00:00Z",
    end_tweets = "2020-01-14T06:59:00Z",
    data_path = "anova_gm_23/",
    bind_tweets = FALSE,
    n = 1000
  )
gm23_tweets <- bind_tweets(data_path = "anova_gm_23/")

gm23_tweets$text <- gsub("[^0-9A-Za-z///' ]","", gm23_tweets$text)
gm23_tweets$text <- gsub("http\\w+", "", gm23_tweets$text)
gm23_tweets$text <- gsub("rt", "", gm23_tweets$text)
gm23_tweets$text <- gsub("@\\w+", "", gm23_tweets$text)
gm23_tweets$text <- tolower(gm23_tweets$text)

sent_gm23_tweets <- sentiment(gm23_tweets$text)
gm23_tweets$sentiment <- sent_gm23_tweets$sentiment

gm23_tweets <- gm23_tweets %>% mutate(game_id = "gm_23_det")
gm23_tweets <- gm23_tweets %>% mutate(location_id = "away")

rand_gm23_tweets <- gm23_tweets[sample(nrow(gm23_tweets), size=1000), ]
# View(rand_gm23_tweets)


## gm_24
gm24_tweets <-
  get_all_tweets(
    query = "@PelicansNBA",
    start_tweets = "2020-01-20T07:00:00Z",
    end_tweets = "2020-01-21T06:59:00Z",
    data_path = "anova_gm_24/",
    bind_tweets = FALSE,
    n = 1000
  )
gm24_tweets <- bind_tweets(data_path = "anova_gm_24/")

gm24_tweets$text <- gsub("[^0-9A-Za-z///' ]","", gm24_tweets$text)
gm24_tweets$text <- gsub("http\\w+", "", gm24_tweets$text)
gm24_tweets$text <- gsub("rt", "", gm24_tweets$text)
gm24_tweets$text <- gsub("@\\w+", "", gm24_tweets$text)
gm24_tweets$text <- tolower(gm24_tweets$text)

sent_gm24_tweets <- sentiment(gm24_tweets$text)
gm24_tweets$sentiment <- sent_gm24_tweets$sentiment

gm24_tweets <- gm24_tweets %>% mutate(game_id = "gm_24_mem")
gm24_tweets <- gm24_tweets %>% mutate(location_id = "away")

rand_gm24_tweets <- gm24_tweets[sample(nrow(gm24_tweets), size=1000), ]
# View(rand_gm24_tweets)


## gm_25
gm25_tweets <-
  get_all_tweets(
    query = "@PelicansNBA",
    start_tweets = "2020-01-28T07:00:00Z",
    end_tweets = "2020-01-29T06:59:00Z",
    data_path = "anova_gm_25/",
    bind_tweets = FALSE,
    n = 1000
  )
gm25_tweets <- bind_tweets(data_path = "anova_gm_25/")

gm25_tweets$text <- gsub("[^0-9A-Za-z///' ]","", gm25_tweets$text)
gm25_tweets$text <- gsub("http\\w+", "", gm25_tweets$text)
gm25_tweets$text <- gsub("rt", "", gm25_tweets$text)
gm25_tweets$text <- gsub("@\\w+", "", gm25_tweets$text)
gm25_tweets$text <- tolower(gm25_tweets$text)

sent_gm25_tweets <- sentiment(gm25_tweets$text)
gm25_tweets$sentiment <- sent_gm25_tweets$sentiment

gm25_tweets <- gm25_tweets %>% mutate(game_id = "gm_25_cle")
gm25_tweets <- gm25_tweets %>% mutate(location_id = "away")

rand_gm25_tweets <- gm25_tweets[sample(nrow(gm25_tweets), size=1000), ]
# View(rand_gm25_tweets)


## gm_26
gm26_tweets <-
  get_all_tweets(
    query = "@PelicansNBA",
    start_tweets = "2020-02-06T07:00:00Z",
    end_tweets = "2020-02-07T06:59:00Z",
    data_path = "anova_gm_26/",
    bind_tweets = FALSE,
    n = 1000
  )
gm26_tweets <- bind_tweets(data_path = "anova_gm_26/")

gm26_tweets$text <- gsub("[^0-9A-Za-z///' ]","", gm26_tweets$text)
gm26_tweets$text <- gsub("http\\w+", "", gm26_tweets$text)
gm26_tweets$text <- gsub("rt", "", gm26_tweets$text)
gm26_tweets$text <- gsub("@\\w+", "", gm26_tweets$text)
gm26_tweets$text <- tolower(gm26_tweets$text)

sent_gm26_tweets <- sentiment(gm26_tweets$text)
gm26_tweets$sentiment <- sent_gm26_tweets$sentiment

gm26_tweets <- gm26_tweets %>% mutate(game_id = "gm_26_chi")
gm26_tweets <- gm26_tweets %>% mutate(location_id = "away")

rand_gm26_tweets <- gm26_tweets[sample(nrow(gm26_tweets), size=1000), ]
# View(rand_gm26_tweets)


## gm_27
gm27_tweets <-
  get_all_tweets(
    query = "@PelicansNBA",
    start_tweets = "2020-02-08T07:00:00Z",
    end_tweets = "2020-02-09T06:59:00Z",
    data_path = "anova_gm_27/",
    bind_tweets = FALSE,
    n = 1000
  )
gm27_tweets <- bind_tweets(data_path = "anova_gm_27/")

gm27_tweets$text <- gsub("[^0-9A-Za-z///' ]","", gm27_tweets$text)
gm27_tweets$text <- gsub("http\\w+", "", gm27_tweets$text)
gm27_tweets$text <- gsub("rt", "", gm27_tweets$text)
gm27_tweets$text <- gsub("@\\w+", "", gm27_tweets$text)
gm27_tweets$text <- tolower(gm27_tweets$text)

sent_gm27_tweets <- sentiment(gm27_tweets$text)
gm27_tweets$sentiment <- sent_gm27_tweets$sentiment

gm27_tweets <- gm27_tweets %>% mutate(game_id = "gm_27_ind")
gm27_tweets <- gm27_tweets %>% mutate(location_id = "away")

rand_gm27_tweets <- gm27_tweets[sample(nrow(gm27_tweets), size=1000), ]
# View(rand_gm27_tweets)


## gm_28
gm28_tweets <-
  get_all_tweets(
    query = "@PelicansNBA",
    start_tweets = "2020-02-21T07:00:00Z",
    end_tweets = "2020-02-22T06:59:00Z",
    data_path = "anova_gm_28/",
    bind_tweets = FALSE,
    n = 1000
  )
gm28_tweets <- bind_tweets(data_path = "anova_gm_28/")

gm28_tweets$text <- gsub("[^0-9A-Za-z///' ]","", gm28_tweets$text)
gm28_tweets$text <- gsub("http\\w+", "", gm28_tweets$text)
gm28_tweets$text <- gsub("rt", "", gm28_tweets$text)
gm28_tweets$text <- gsub("@\\w+", "", gm28_tweets$text)
gm28_tweets$text <- tolower(gm28_tweets$text)

sent_gm28_tweets <- sentiment(gm28_tweets$text)
gm28_tweets$sentiment <- sent_gm28_tweets$sentiment

gm28_tweets <- gm28_tweets %>% mutate(game_id = "gm_28_por")
gm28_tweets <- gm28_tweets %>% mutate(location_id = "away")

rand_gm28_tweets <- gm28_tweets[sample(nrow(gm28_tweets), size=1000), ]
# View(rand_gm28_tweets)


## gm_29
gm29_tweets <-
  get_all_tweets(
    query = "@PelicansNBA",
    start_tweets = "2020-02-23T07:00:00Z",
    end_tweets = "2020-02-24T06:59:00Z",
    data_path = "anova_gm_29/",
    bind_tweets = FALSE,
    n = 1000
  )
gm29_tweets <- bind_tweets(data_path = "anova_gm_29/")

gm29_tweets$text <- gsub("[^0-9A-Za-z///' ]","", gm29_tweets$text)
gm29_tweets$text <- gsub("http\\w+", "", gm29_tweets$text)
gm29_tweets$text <- gsub("rt", "", gm29_tweets$text)
gm29_tweets$text <- gsub("@\\w+", "", gm29_tweets$text)
gm29_tweets$text <- tolower(gm29_tweets$text)

sent_gm29_tweets <- sentiment(gm29_tweets$text)
gm29_tweets$sentiment <- sent_gm29_tweets$sentiment

gm29_tweets <- gm29_tweets %>% mutate(game_id = "gm_29_gsw")
gm29_tweets <- gm29_tweets %>% mutate(location_id = "away")

rand_gm29_tweets <- gm29_tweets[sample(nrow(gm29_tweets), size=1000), ]
# View(rand_gm29_tweets)


## gm_30
gm30_tweets <-
  get_all_tweets(
    query = "@PelicansNBA",
    start_tweets = "2020-03-08T07:00:00Z",
    end_tweets = "2020-03-09T06:59:00Z",
    data_path = "anova_gm_30/",
    bind_tweets = FALSE,
    n = 1000
  )
gm30_tweets <- bind_tweets(data_path = "anova_gm_30/")

gm30_tweets$text <- gsub("[^0-9A-Za-z///' ]","", gm30_tweets$text)
gm30_tweets$text <- gsub("http\\w+", "", gm30_tweets$text)
gm30_tweets$text <- gsub("rt", "", gm30_tweets$text)
gm30_tweets$text <- gsub("@\\w+", "", gm30_tweets$text)
gm30_tweets$text <- tolower(gm30_tweets$text)

sent_gm30_tweets <- sentiment(gm30_tweets$text)
gm30_tweets$sentiment <- sent_gm30_tweets$sentiment

gm30_tweets <- gm30_tweets %>% mutate(game_id = "gm_30_min")
gm30_tweets <- gm30_tweets %>% mutate(location_id = "away")

rand_gm30_tweets <- gm30_tweets[sample(nrow(gm30_tweets), size=1000), ]
# View(rand_gm30_tweets)





### Small Versions of Random 1000-Tweet Samples for Home Wins
### 'Small' meaning reduced number of columns
sm_rand_gm1_tweets <- rand_gm1_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id, location_id)
# View(sm_rand_gm1_tweets)

sm_rand_gm2_tweets <- rand_gm2_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id, location_id)
# View(sm_rand_gm2_tweets)

sm_rand_gm3_tweets <- rand_gm3_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id, location_id)
# View(sm_rand_gm3_tweets)

sm_rand_gm4_tweets <- rand_gm4_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id, location_id)
# View(sm_rand_gm4_tweets)

sm_rand_gm5_tweets <- rand_gm5_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id, location_id)
# View(sm_rand_gm5_tweets)

sm_rand_gm6_tweets <- rand_gm6_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id, location_id)
# View(sm_rand_gm6_tweets)

sm_rand_gm7_tweets <- rand_gm7_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id, location_id)
# View(sm_rand_gm7_tweets)

sm_rand_gm8_tweets <- rand_gm8_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id, location_id)
# View(sm_rand_gm8_tweets)

sm_rand_gm9_tweets <- rand_gm9_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id, location_id)
# View(sm_rand_gm9_tweets)

sm_rand_gm10_tweets <- rand_gm10_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id, location_id)
# View(sm_rand_gm10_tweets)

sm_rand_gm11_tweets <- rand_gm11_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id, location_id)
# View(sm_rand_gm11_tweets)

sm_rand_gm12_tweets <- rand_gm12_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id, location_id)
# View(sm_rand_gm12_tweets)

sm_rand_gm13_tweets <- rand_gm13_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id, location_id)
# View(sm_rand_gm13_tweets)

sm_rand_gm14_tweets <- rand_gm14_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id, location_id)
# View(sm_rand_gm14_tweets)

sm_rand_gm15_tweets <- rand_gm15_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id, location_id)
# View(sm_rand_gm15_tweets)





### Small Versions of Random 1000-Tweet Samples for Away Wins
### 'Small' meaning reduced number of columns
sm_rand_gm16_tweets <- rand_gm16_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id, location_id)
# View(sm_rand_gm16_tweets)

sm_rand_gm17_tweets <- rand_gm17_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id, location_id)
# View(sm_rand_gm17_tweets)

sm_rand_gm18_tweets <- rand_gm18_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id, location_id)
# View(sm_rand_gm18_tweets)

sm_rand_gm19_tweets <- rand_gm19_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id, location_id)
# View(sm_rand_gm19_tweets)

sm_rand_gm20_tweets <- rand_gm20_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id, location_id)
# View(sm_rand_gm20_tweets)

sm_rand_gm21_tweets <- rand_gm21_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id, location_id)
# View(sm_rand_gm21_tweets)

sm_rand_gm22_tweets <- rand_gm22_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id, location_id)
# View(sm_rand_gm22_tweets)

sm_rand_gm23_tweets <- rand_gm23_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id, location_id)
# View(sm_rand_gm23_tweets)

sm_rand_gm24_tweets <- rand_gm24_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id, location_id)
# View(sm_rand_gm24_tweets)

sm_rand_gm25_tweets <- rand_gm25_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id, location_id)
# View(sm_rand_gm25_tweets)

sm_rand_gm26_tweets <- rand_gm26_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id, location_id)
# View(sm_rand_gm26_tweets)

sm_rand_gm27_tweets <- rand_gm27_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id, location_id)
# View(sm_rand_gm27_tweets)

sm_rand_gm28_tweets <- rand_gm28_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id, location_id)
# View(sm_rand_gm28_tweets)

sm_rand_gm29_tweets <- rand_gm29_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id, location_id)
# View(sm_rand_gm29_tweets)

sm_rand_gm30_tweets <- rand_gm30_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id, location_id)
# View(sm_rand_gm30_tweets)





### Merging of All 30 Games
merge_gms_h_and_a <- bind_rows(sm_rand_gm1_tweets, sm_rand_gm2_tweets, sm_rand_gm3_tweets, 
                               sm_rand_gm4_tweets, sm_rand_gm5_tweets, sm_rand_gm6_tweets, 
                               sm_rand_gm7_tweets, sm_rand_gm8_tweets, sm_rand_gm9_tweets, 
                               sm_rand_gm10_tweets, sm_rand_gm11_tweets, sm_rand_gm12_tweets, 
                               sm_rand_gm13_tweets, sm_rand_gm14_tweets, sm_rand_gm15_tweets, 
                               sm_rand_gm16_tweets, sm_rand_gm17_tweets, sm_rand_gm18_tweets, 
                               sm_rand_gm19_tweets, sm_rand_gm20_tweets, sm_rand_gm21_tweets, 
                               sm_rand_gm22_tweets, sm_rand_gm23_tweets, sm_rand_gm24_tweets, 
                               sm_rand_gm25_tweets, sm_rand_gm26_tweets, sm_rand_gm27_tweets, 
                               sm_rand_gm28_tweets, sm_rand_gm29_tweets, sm_rand_gm30_tweets)
View(merge_gms_h_and_a)





### Running an ANOVA Test based on sentiment and game_id
anova_model <- aov(sentiment ~ game_id, data = merge_gms_h_and_a)
summary(anova_model)





### Running Post-Hoc Test (If Necessary)
options(max.print=2000) #Added on 2-6-22 to increase how many rows are returned and visible in the Console after running Post-Hoc test
TukeyHSD(anova_model)





### Attempt to visualize the confidence intervals
plot(TukeyHSD(anova_model, conf.level=.95), las = 2)





### Running the first ANOVA Test of two based on sentiment and location_id
anova_model <- aov(sentiment ~ location_id, data = merge_gms_h_and_a)
summary(anova_model)

## Running a Post-Hoc Test
TukeyHSD(anova_model)





### Using plotly to plot a box plot of the first ANOVA test
plot_ly(
  data = merge_gms_h_and_a,
  y = ~sentiment,
  x = ~location_id,
  type = "box",
  color = ~location_id,
  showlegend = FALSE
)




### Attempt to plot a box plot of original ANOVA and merged data frame with all
### 30 games using plotly
plot_ly(
  data = merge_gms_h_and_a,
  y = ~sentiment,
  x = ~game_id,
  type = "box",
  color = ~game_id,
  showlegend = FALSE
)





### Making a Data Frame Based on Home vs Away on a Game vs Game Basis for Games
### 11 and 19
merge_gms_11_19 <- bind_rows(sm_rand_gm11_tweets, sm_rand_gm19_tweets)





### Running the second ANOVA test of two involving the data frame directly above
anova_model_gms_11_19 <- aov(sentiment ~ game_id, data = merge_gms_11_19)
summary(anova_model_gms_11_19)
# ggboxplot(merge_gms_11_19, x = "game_id", y = "sentiment")





### Running a Post-Hoc Test based on the ANOVA directly above
TukeyHSD(anova_model_gms_11_19)
# plot(TukeyHSD(anova_model_gms_11_19, conf.level=.95), las = 2)





### Using plotly to plot a boxplot of the second ANOVA test
plot_ly(
  data = merge_gms_11_19,
  y = ~sentiment,
  x = ~game_id,
  type = "box",
  color = ~game_id,
  showlegend = FALSE
)