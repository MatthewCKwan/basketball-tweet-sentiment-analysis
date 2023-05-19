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





### Hypothesis Test 2 - Game Wins vs Game Losses from LAL 2020-21 Season

##15 Wins and 15 Losses from Los Angeles Lakers' 2020-21 Regular Season 
##(Drawn from Games #1-72)
#15 W 15 L
sample(1:72, 1)
#43 L vs PHX
#33 L vs UTA
#52 L vs MIA
#4  L vs POR
#40 W vs MIN
#54 L vs NYK
#36 L vs PHX
#17 W vs CHI
#10 W vs CHI
#57 W vs UTA
#71 W vs IND
#66 L vs LAC
#55 W vs CHA
#38 W vs IND
#18 W vs CLE
#34 W vs POR
#6  W vs SAS
#44 L vs NOP
#72 W vs NOP
#15 L vs GSW
#64 L vs TOR
#8  W vs MEM
#25 W vs OKC
#48 L vs MIL
#20 L vs DET
#1  L vs LAC
#35 W vs GSW
#41 W vs CHA
#9  L vs SAS
#59 L vs DAL

#W: 6, 8, 10, 17, 18, 25, 34, 35, 38, 40, 41, 55, 57, 71, 72
#L: 1, 4, 9, 15, 20, 33, 36, 43, 44, 48, 52, 54, 59, 64, 66

##Rearranged for easier readability
#43 L vs PHX -- #1
#33 L vs UTA -- #2
#52 L vs MIA -- #3
#4  L vs POR -- #4
#54 L vs NYK -- #5
#36 L vs PHX -- #6
#66 L vs LAC -- #7
#44 L vs NOP -- #8
#15 L vs GSW -- #9
#64 L vs TOR -- #10
#48 L vs MIL -- #11
#20 L vs DET -- #12
#1  L vs LAC -- #13
#9  L vs SAS -- #14
#59 L vs DAL -- #15

#40 W vs MIN -- #1
#17 W vs CHI -- #2
#10 W vs CHI -- #3
#57 W vs UTA -- #4
#71 W vs IND -- #5
#55 W vs CHA -- #6
#38 W vs IND -- #7
#18 W vs CLE -- #8
#34 W vs POR -- #9
#6  W vs SAS -- #10
#72 W vs NOP -- #11
#8  W vs MEM -- #12
#25 W vs OKC -- #13
#35 W vs GSW -- #14
#41 W vs CHA -- #15





### Data Collection
## Note: the 'h2' in the data_path name stands for hypothesis 2 so I can 
## differentiate between the files from the first hypothesis

## Lakers vs PHX 3-21-21 (Beginning of the game losses)
l_gm1_lakers_phx_tweets <-
  get_all_tweets(
    query = "lakers",
    start_tweets = "2021-03-21T07:00:00Z",
    end_tweets = "2021-03-22T06:59:00Z",
    data_path = "h2_lakers_phx_3-21/",
    bind_tweets = FALSE,
    n = 1000
  )
l_gm1_lakers_phx_tweets <- bind_tweets(data_path = "h2_lakers_phx_3-21/")

l_gm1_lakers_phx_tweets$text <- gsub("[^0-9A-Za-z///' ]","", l_gm1_lakers_phx_tweets$text)
l_gm1_lakers_phx_tweets$text <- gsub("http\\w+", "", l_gm1_lakers_phx_tweets$text)
l_gm1_lakers_phx_tweets$text <- gsub("rt", "", l_gm1_lakers_phx_tweets$text)
l_gm1_lakers_phx_tweets$text <- gsub("@\\w+", "", l_gm1_lakers_phx_tweets$text)
l_gm1_lakers_phx_tweets$text <- tolower(l_gm1_lakers_phx_tweets$text)

sent_l_gm1_lakers_phx_tweets <- sentiment(l_gm1_lakers_phx_tweets$text)
l_gm1_lakers_phx_tweets$sentiment <- sent_l_gm1_lakers_phx_tweets$sentiment

l_gm1_lakers_phx_tweets <- l_gm1_lakers_phx_tweets %>% mutate(game_id = "l_gm_1")

rand_l_gm1_lakers_phx_tweets <- l_gm1_lakers_phx_tweets[sample(nrow(l_gm1_lakers_phx_tweets), size=1000), ]
# View(rand_l_gm1_lakers_phx_tweets)


## Lakers vs UTA 2-24-21
l_gm2_lakers_uta_tweets <-
  get_all_tweets(
    query = "lakers",
    start_tweets = "2021-02-24T07:00:00Z",
    end_tweets = "2021-02-25T06:59:00Z",
    data_path = "h2_lakers_uta_2-24/",
    bind_tweets = FALSE,
    n = 1000
  )
l_gm2_lakers_uta_tweets <- bind_tweets(data_path = "h2_lakers_uta_2-24/")

l_gm2_lakers_uta_tweets$text <- gsub("[^0-9A-Za-z///' ]","", l_gm2_lakers_uta_tweets$text)
l_gm2_lakers_uta_tweets$text <- gsub("http\\w+", "", l_gm2_lakers_uta_tweets$text)
l_gm2_lakers_uta_tweets$text <- gsub("rt", "", l_gm2_lakers_uta_tweets$text)
l_gm2_lakers_uta_tweets$text <- gsub("@\\w+", "", l_gm2_lakers_uta_tweets$text)
l_gm2_lakers_uta_tweets$text <- tolower(l_gm2_lakers_uta_tweets$text)

sent_l_gm2_lakers_uta_tweets <- sentiment(l_gm2_lakers_uta_tweets$text)
l_gm2_lakers_uta_tweets$sentiment <- sent_l_gm2_lakers_uta_tweets$sentiment

l_gm2_lakers_uta_tweets <- l_gm2_lakers_uta_tweets %>% mutate(game_id = "l_gm_2")

rand_l_gm2_lakers_uta_tweets <- l_gm2_lakers_uta_tweets[sample(nrow(l_gm2_lakers_uta_tweets), size=1000), ]
# View(rand_l_gm2_lakers_uta_tweets)


## Lakers vs MIA 4-8-21
l_gm3_lakers_mia_tweets <-
  get_all_tweets(
    query = "lakers",
    start_tweets = "2021-04-08T07:00:00Z",
    end_tweets = "2021-04-09T06:59:00Z",
    data_path = "h2_lakers_mia_4-8/",
    bind_tweets = FALSE,
    n = 1000
  )
l_gm3_lakers_mia_tweets <- bind_tweets(data_path = "h2_lakers_mia_4-8/")

l_gm3_lakers_mia_tweets$text <- gsub("[^0-9A-Za-z///' ]","", l_gm3_lakers_mia_tweets$text)
l_gm3_lakers_mia_tweets$text <- gsub("http\\w+", "", l_gm3_lakers_mia_tweets$text)
l_gm3_lakers_mia_tweets$text <- gsub("rt", "", l_gm3_lakers_mia_tweets$text)
l_gm3_lakers_mia_tweets$text <- gsub("@\\w+", "", l_gm3_lakers_mia_tweets$text)
l_gm3_lakers_mia_tweets$text <- tolower(l_gm3_lakers_mia_tweets$text)

sent_l_gm3_lakers_mia_tweets <- sentiment(l_gm3_lakers_mia_tweets$text)
l_gm3_lakers_mia_tweets$sentiment <- sent_l_gm3_lakers_mia_tweets$sentiment

l_gm3_lakers_mia_tweets <- l_gm3_lakers_mia_tweets %>% mutate(game_id = "l_gm_3")

rand_l_gm3_lakers_mia_tweets <- l_gm3_lakers_mia_tweets[sample(nrow(l_gm3_lakers_mia_tweets), size=1000), ]
# View(rand_l_gm3_lakers_mia_tweets)


## Lakers vs POR 12-28-20
l_gm4_lakers_por_tweets <-
  get_all_tweets(
    query = "lakers",
    start_tweets = "2020-12-28T07:00:00Z",
    end_tweets = "2020-12-29T06:59:00Z",
    data_path = "h2_lakers_por_12-28/",
    bind_tweets = FALSE,
    n = 1000
  )
l_gm4_lakers_por_tweets <- bind_tweets(data_path = "h2_lakers_por_12-28/")

l_gm4_lakers_por_tweets$text <- gsub("[^0-9A-Za-z///' ]","", l_gm4_lakers_por_tweets$text)
l_gm4_lakers_por_tweets$text <- gsub("http\\w+", "", l_gm4_lakers_por_tweets$text)
l_gm4_lakers_por_tweets$text <- gsub("rt", "", l_gm4_lakers_por_tweets$text)
l_gm4_lakers_por_tweets$text <- gsub("@\\w+", "", l_gm4_lakers_por_tweets$text)
l_gm4_lakers_por_tweets$text <- tolower(l_gm4_lakers_por_tweets$text)

sent_l_gm4_lakers_por_tweets <- sentiment(l_gm4_lakers_por_tweets$text)
l_gm4_lakers_por_tweets$sentiment <- sent_l_gm4_lakers_por_tweets$sentiment

l_gm4_lakers_por_tweets <- l_gm4_lakers_por_tweets %>% mutate(game_id = "l_gm_4")

rand_l_gm4_lakers_por_tweets <- l_gm4_lakers_por_tweets[sample(nrow(l_gm4_lakers_por_tweets), size=1000), ]
# View(rand_l_gm4_lakers_por_tweets)


## Lakers vs NYK 4-12-21
l_gm5_lakers_nyk_tweets <-
  get_all_tweets(
    query = "lakers",
    start_tweets = "2021-04-12T07:00:00Z",
    end_tweets = "2021-04-13T06:59:00Z",
    data_path = "h2_lakers_nyk_4-12/",
    bind_tweets = FALSE,
    n = 1000
  )
l_gm5_lakers_nyk_tweets <- bind_tweets(data_path = "h2_lakers_nyk_4-12/")

l_gm5_lakers_nyk_tweets$text <- gsub("[^0-9A-Za-z///' ]","", l_gm5_lakers_nyk_tweets$text)
l_gm5_lakers_nyk_tweets$text <- gsub("http\\w+", "", l_gm5_lakers_nyk_tweets$text)
l_gm5_lakers_nyk_tweets$text <- gsub("rt", "", l_gm5_lakers_nyk_tweets$text)
l_gm5_lakers_nyk_tweets$text <- gsub("@\\w+", "", l_gm5_lakers_nyk_tweets$text)
l_gm5_lakers_nyk_tweets$text <- tolower(l_gm5_lakers_nyk_tweets$text)

sent_l_gm5_lakers_nyk_tweets <- sentiment(l_gm5_lakers_nyk_tweets$text)
l_gm5_lakers_nyk_tweets$sentiment <- sent_l_gm5_lakers_nyk_tweets$sentiment

l_gm5_lakers_nyk_tweets <- l_gm5_lakers_nyk_tweets %>% mutate(game_id = "l_gm_5")

rand_l_gm5_lakers_nyk_tweets <- l_gm5_lakers_nyk_tweets[sample(nrow(l_gm5_lakers_nyk_tweets), size=1000), ]
# View(rand_l_gm5_lakers_nyk_tweets)


## Lakers vs PHX 3-2-21
l_gm6_lakers_phx_tweets <-
  get_all_tweets(
    query = "lakers",
    start_tweets = "2021-03-02T07:00:00Z",
    end_tweets = "2021-03-03T06:59:00Z",
    data_path = "h2_lakers_phx_3-2/",
    bind_tweets = FALSE,
    n = 1000
  )
l_gm6_lakers_phx_tweets <- bind_tweets(data_path = "h2_lakers_phx_3-2/")

l_gm6_lakers_phx_tweets$text <- gsub("[^0-9A-Za-z///' ]","", l_gm6_lakers_phx_tweets$text)
l_gm6_lakers_phx_tweets$text <- gsub("http\\w+", "", l_gm6_lakers_phx_tweets$text)
l_gm6_lakers_phx_tweets$text <- gsub("rt", "", l_gm6_lakers_phx_tweets$text)
l_gm6_lakers_phx_tweets$text <- gsub("@\\w+", "", l_gm6_lakers_phx_tweets$text)
l_gm6_lakers_phx_tweets$text <- tolower(l_gm6_lakers_phx_tweets$text)

sent_l_gm6_lakers_phx_tweets <- sentiment(l_gm6_lakers_phx_tweets$text)
l_gm6_lakers_phx_tweets$sentiment <- sent_l_gm6_lakers_phx_tweets$sentiment

l_gm6_lakers_phx_tweets <- l_gm6_lakers_phx_tweets %>% mutate(game_id = "l_gm_6")

rand_l_gm6_lakers_phx_tweets <- l_gm6_lakers_phx_tweets[sample(nrow(l_gm6_lakers_phx_tweets), size=1000), ]
# View(rand_l_gm6_lakers_phx_tweets)


## Lakers vs LAC 5-6-21
l_gm7_lakers_lac_tweets <-
  get_all_tweets(
    query = "lakers",
    start_tweets = "2021-05-06T07:00:00Z",
    end_tweets = "2021-05-07T06:59:00Z",
    data_path = "h2_lakers_lac_5-6/",
    bind_tweets = FALSE,
    n = 1000
  )
l_gm7_lakers_lac_tweets <- bind_tweets(data_path = "h2_lakers_lac_5-6/")

l_gm7_lakers_lac_tweets$text <- gsub("[^0-9A-Za-z///' ]","", l_gm7_lakers_lac_tweets$text)
l_gm7_lakers_lac_tweets$text <- gsub("http\\w+", "", l_gm7_lakers_lac_tweets$text)
l_gm7_lakers_lac_tweets$text <- gsub("rt", "", l_gm7_lakers_lac_tweets$text)
l_gm7_lakers_lac_tweets$text <- gsub("@\\w+", "", l_gm7_lakers_lac_tweets$text)
l_gm7_lakers_lac_tweets$text <- tolower(l_gm7_lakers_lac_tweets$text)

sent_l_gm7_lakers_lac_tweets <- sentiment(l_gm7_lakers_lac_tweets$text)
l_gm7_lakers_lac_tweets$sentiment <- sent_l_gm7_lakers_lac_tweets$sentiment

l_gm7_lakers_lac_tweets <- l_gm7_lakers_lac_tweets %>% mutate(game_id = "l_gm_7")

rand_l_gm7_lakers_lac_tweets <- l_gm7_lakers_lac_tweets[sample(nrow(l_gm7_lakers_lac_tweets), size=1000), ]
# View(rand_l_gm7_lakers_lac_tweets)


## Lakers vs NOP 3-23-21
l_gm8_lakers_nop_tweets <-
  get_all_tweets(
    query = "lakers",
    start_tweets = "2021-03-23T07:00:00Z",
    end_tweets = "2021-03-24T06:59:00Z",
    data_path = "h2_lakers_nop_3-23/",
    bind_tweets = FALSE,
    n = 1000
  )
l_gm8_lakers_nop_tweets <- bind_tweets(data_path = "h2_lakers_nop_3-23/")

l_gm8_lakers_nop_tweets$text <- gsub("[^0-9A-Za-z///' ]","", l_gm8_lakers_nop_tweets$text)
l_gm8_lakers_nop_tweets$text <- gsub("http\\w+", "", l_gm8_lakers_nop_tweets$text)
l_gm8_lakers_nop_tweets$text <- gsub("rt", "", l_gm8_lakers_nop_tweets$text)
l_gm8_lakers_nop_tweets$text <- gsub("@\\w+", "", l_gm8_lakers_nop_tweets$text)
l_gm8_lakers_nop_tweets$text <- tolower(l_gm8_lakers_nop_tweets$text)

sent_l_gm8_lakers_nop_tweets <- sentiment(l_gm8_lakers_nop_tweets$text)
l_gm8_lakers_nop_tweets$sentiment <- sent_l_gm8_lakers_nop_tweets$sentiment

l_gm8_lakers_nop_tweets <- l_gm8_lakers_nop_tweets %>% mutate(game_id = "l_gm_8")

rand_l_gm8_lakers_nop_tweets <- l_gm8_lakers_nop_tweets[sample(nrow(l_gm8_lakers_nop_tweets), size=1000), ]
# View(rand_l_gm8_lakers_nop_tweets)


## Lakers vs GSW 1-18-21
l_gm9_lakers_gsw_tweets <-
  get_all_tweets(
    query = "lakers",
    start_tweets = "2021-01-18T07:00:00Z",
    end_tweets = "2021-01-19T06:59:00Z",
    data_path = "h2_lakers_gsw_1-18/",
    bind_tweets = FALSE,
    n = 1000
  )
l_gm9_lakers_gsw_tweets <- bind_tweets(data_path = "h2_lakers_gsw_1-18/")

l_gm9_lakers_gsw_tweets$text <- gsub("[^0-9A-Za-z///' ]","", l_gm9_lakers_gsw_tweets$text)
l_gm9_lakers_gsw_tweets$text <- gsub("http\\w+", "", l_gm9_lakers_gsw_tweets$text)
l_gm9_lakers_gsw_tweets$text <- gsub("rt", "", l_gm9_lakers_gsw_tweets$text)
l_gm9_lakers_gsw_tweets$text <- gsub("@\\w+", "", l_gm9_lakers_gsw_tweets$text)
l_gm9_lakers_gsw_tweets$text <- tolower(l_gm9_lakers_gsw_tweets$text)

sent_l_gm9_lakers_gsw_tweets <- sentiment(l_gm9_lakers_gsw_tweets$text)
l_gm9_lakers_gsw_tweets$sentiment <- sent_l_gm9_lakers_gsw_tweets$sentiment

l_gm9_lakers_gsw_tweets <- l_gm9_lakers_gsw_tweets %>% mutate(game_id = "l_gm_9")

rand_l_gm9_lakers_gsw_tweets <- l_gm9_lakers_gsw_tweets[sample(nrow(l_gm9_lakers_gsw_tweets), size=1000), ]
# View(rand_l_gm9_lakers_gsw_tweets)


## Lakers vs TOR 5-2-21
l_gm10_lakers_tor_tweets <-
  get_all_tweets(
    query = "lakers",
    start_tweets = "2021-05-02T07:00:00Z",
    end_tweets = "2021-05-03T06:59:00Z",
    data_path = "h2_lakers_tor_1-18/",
    bind_tweets = FALSE,
    n = 1000
  )
l_gm10_lakers_tor_tweets <- bind_tweets(data_path = "h2_lakers_tor_1-18/")

l_gm10_lakers_tor_tweets$text <- gsub("[^0-9A-Za-z///' ]","", l_gm10_lakers_tor_tweets$text)
l_gm10_lakers_tor_tweets$text <- gsub("http\\w+", "", l_gm10_lakers_tor_tweets$text)
l_gm10_lakers_tor_tweets$text <- gsub("rt", "", l_gm10_lakers_tor_tweets$text)
l_gm10_lakers_tor_tweets$text <- gsub("@\\w+", "", l_gm10_lakers_tor_tweets$text)
l_gm10_lakers_tor_tweets$text <- tolower(l_gm10_lakers_tor_tweets$text)

sent_l_gm10_lakers_tor_tweets <- sentiment(l_gm10_lakers_tor_tweets$text)
l_gm10_lakers_tor_tweets$sentiment <- sent_l_gm10_lakers_tor_tweets$sentiment

l_gm10_lakers_tor_tweets <- l_gm10_lakers_tor_tweets %>% mutate(game_id = "l_gm_10")

rand_l_gm10_lakers_tor_tweets <- l_gm10_lakers_tor_tweets[sample(nrow(l_gm10_lakers_tor_tweets), size=1000), ]
# View(rand_l_gm10_lakers_tor_tweets)


## Lakers vs MIL 3-31-21
l_gm11_lakers_mil_tweets <-
  get_all_tweets(
    query = "lakers",
    start_tweets = "2021-03-31T07:00:00Z",
    end_tweets = "2021-04-01T06:59:00Z",
    data_path = "h2_lakers_mil_3-31/",
    bind_tweets = FALSE,
    n = 1000
  )
l_gm11_lakers_mil_tweets <- bind_tweets(data_path = "h2_lakers_mil_3-31/")

l_gm11_lakers_mil_tweets$text <- gsub("[^0-9A-Za-z///' ]","", l_gm11_lakers_mil_tweets$text)
l_gm11_lakers_mil_tweets$text <- gsub("http\\w+", "", l_gm11_lakers_mil_tweets$text)
l_gm11_lakers_mil_tweets$text <- gsub("rt", "", l_gm11_lakers_mil_tweets$text)
l_gm11_lakers_mil_tweets$text <- gsub("@\\w+", "", l_gm11_lakers_mil_tweets$text)
l_gm11_lakers_mil_tweets$text <- tolower(l_gm11_lakers_mil_tweets$text)

sent_l_gm11_lakers_mil_tweets <- sentiment(l_gm11_lakers_mil_tweets$text)
l_gm11_lakers_mil_tweets$sentiment <- sent_l_gm11_lakers_mil_tweets$sentiment

l_gm11_lakers_mil_tweets <- l_gm11_lakers_mil_tweets %>% mutate(game_id = "l_gm_11")

rand_l_gm11_lakers_mil_tweets <- l_gm11_lakers_mil_tweets[sample(nrow(l_gm11_lakers_mil_tweets), size=1000), ]
# View(rand_l_gm11_lakers_mil_tweets)


## Lakers vs DET 1-28-21
l_gm12_lakers_det_tweets <-
  get_all_tweets(
    query = "lakers",
    start_tweets = "2021-01-28T07:00:00Z",
    end_tweets = "2021-01-29T06:59:00Z",
    data_path = "h2_lakers_det_1-28/",
    bind_tweets = FALSE,
    n = 1000
  )
l_gm12_lakers_det_tweets <- bind_tweets(data_path = "h2_lakers_det_1-28/")

l_gm12_lakers_det_tweets$text <- gsub("[^0-9A-Za-z///' ]","", l_gm12_lakers_det_tweets$text)
l_gm12_lakers_det_tweets$text <- gsub("http\\w+", "", l_gm12_lakers_det_tweets$text)
l_gm12_lakers_det_tweets$text <- gsub("rt", "", l_gm12_lakers_det_tweets$text)
l_gm12_lakers_det_tweets$text <- gsub("@\\w+", "", l_gm12_lakers_det_tweets$text)
l_gm12_lakers_det_tweets$text <- tolower(l_gm12_lakers_det_tweets$text)

sent_l_gm12_lakers_det_tweets <- sentiment(l_gm12_lakers_det_tweets$text)
l_gm12_lakers_det_tweets$sentiment <- sent_l_gm12_lakers_det_tweets$sentiment

l_gm12_lakers_det_tweets <- l_gm12_lakers_det_tweets %>% mutate(game_id = "l_gm_12")

rand_l_gm12_lakers_det_tweets <- l_gm12_lakers_det_tweets[sample(nrow(l_gm12_lakers_det_tweets), size=1000), ]
# View(rand_l_gm12_lakers_det_tweets)


## Lakers vs LAC 12-22-20
l_gm13_lakers_lac_tweets <-
  get_all_tweets(
    query = "lakers",
    start_tweets = "2020-12-22T07:00:00Z",
    end_tweets = "2020-12-23T06:59:00Z",
    data_path = "h2_lakers_lac_12-22/",
    bind_tweets = FALSE,
    n = 1000
  )
l_gm13_lakers_lac_tweets <- bind_tweets(data_path = "h2_lakers_lac_12-22/")

l_gm13_lakers_lac_tweets$text <- gsub("[^0-9A-Za-z///' ]","", l_gm13_lakers_lac_tweets$text)
l_gm13_lakers_lac_tweets$text <- gsub("http\\w+", "", l_gm13_lakers_lac_tweets$text)
l_gm13_lakers_lac_tweets$text <- gsub("rt", "", l_gm13_lakers_lac_tweets$text)
l_gm13_lakers_lac_tweets$text <- gsub("@\\w+", "", l_gm13_lakers_lac_tweets$text)
l_gm13_lakers_lac_tweets$text <- tolower(l_gm13_lakers_lac_tweets$text)

sent_l_gm13_lakers_lac_tweets <- sentiment(l_gm13_lakers_lac_tweets$text)
l_gm13_lakers_lac_tweets$sentiment <- sent_l_gm13_lakers_lac_tweets$sentiment

l_gm13_lakers_lac_tweets <- l_gm13_lakers_lac_tweets %>% mutate(game_id = "l_gm_13")

rand_l_gm13_lakers_lac_tweets <- l_gm13_lakers_lac_tweets[sample(nrow(l_gm13_lakers_lac_tweets), size=1000), ]
# View(rand_l_gm13_lakers_lac_tweets)


## Lakers vs SAS 1-7-21
l_gm14_lakers_sas_tweets <-
  get_all_tweets(
    query = "lakers",
    start_tweets = "2021-01-07T07:00:00Z",
    end_tweets = "2021-01-08T06:59:00Z",
    data_path = "h2_lakers_sas_1-7/",
    bind_tweets = FALSE,
    n = 1000
  )
l_gm14_lakers_sas_tweets <- bind_tweets(data_path = "h2_lakers_sas_1-7/")

l_gm14_lakers_sas_tweets$text <- gsub("[^0-9A-Za-z///' ]","", l_gm14_lakers_sas_tweets$text)
l_gm14_lakers_sas_tweets$text <- gsub("http\\w+", "", l_gm14_lakers_sas_tweets$text)
l_gm14_lakers_sas_tweets$text <- gsub("rt", "", l_gm14_lakers_sas_tweets$text)
l_gm14_lakers_sas_tweets$text <- gsub("@\\w+", "", l_gm14_lakers_sas_tweets$text)
l_gm14_lakers_sas_tweets$text <- tolower(l_gm14_lakers_sas_tweets$text)

sent_l_gm14_lakers_sas_tweets <- sentiment(l_gm14_lakers_sas_tweets$text)
l_gm14_lakers_sas_tweets$sentiment <- sent_l_gm14_lakers_sas_tweets$sentiment

l_gm14_lakers_sas_tweets <- l_gm14_lakers_sas_tweets %>% mutate(game_id = "l_gm_14")

rand_l_gm14_lakers_sas_tweets <- l_gm14_lakers_sas_tweets[sample(nrow(l_gm14_lakers_sas_tweets), size=1000), ]
# View(rand_l_gm14_lakers_sas_tweets)


## Lakers vs DAL 4-22-21
l_gm15_lakers_dal_tweets <-
  get_all_tweets(
    query = "lakers",
    start_tweets = "2021-04-22T07:00:00Z",
    end_tweets = "2021-04-23T06:59:00Z",
    data_path = "h2_lakers_dal_4-22/",
    bind_tweets = FALSE,
    n = 1000
  )
l_gm15_lakers_dal_tweets <- bind_tweets(data_path = "h2_lakers_dal_4-22/")

l_gm15_lakers_dal_tweets$text <- gsub("[^0-9A-Za-z///' ]","", l_gm15_lakers_dal_tweets$text)
l_gm15_lakers_dal_tweets$text <- gsub("http\\w+", "", l_gm15_lakers_dal_tweets$text)
l_gm15_lakers_dal_tweets$text <- gsub("rt", "", l_gm15_lakers_dal_tweets$text)
l_gm15_lakers_dal_tweets$text <- gsub("@\\w+", "", l_gm15_lakers_dal_tweets$text)
l_gm15_lakers_dal_tweets$text <- tolower(l_gm15_lakers_dal_tweets$text)

sent_l_gm15_lakers_dal_tweets <- sentiment(l_gm15_lakers_dal_tweets$text)
l_gm15_lakers_dal_tweets$sentiment <- sent_l_gm15_lakers_dal_tweets$sentiment

l_gm15_lakers_dal_tweets <- l_gm15_lakers_dal_tweets %>% mutate(game_id = "l_gm_15")

rand_l_gm15_lakers_dal_tweets <- l_gm15_lakers_dal_tweets[sample(nrow(l_gm15_lakers_dal_tweets), size=1000), ]
# View(rand_l_gm15_lakers_dal_tweets)


## Lakers vs MIN 3-16-21 (Beginning of the game wins)
w_gm1_lakers_min_tweets <-
  get_all_tweets(
    query = "lakers",
    start_tweets = "2021-03-16T07:00:00Z",
    end_tweets = "2021-03-17T06:59:00Z",
    data_path = "h2_lakers_min_3-16/",
    bind_tweets = FALSE,
    n = 1000
  )
w_gm1_lakers_min_tweets <- bind_tweets(data_path = "h2_lakers_min_3-16/")

w_gm1_lakers_min_tweets$text <- gsub("[^0-9A-Za-z///' ]","", w_gm1_lakers_min_tweets$text)
w_gm1_lakers_min_tweets$text <- gsub("http\\w+", "", w_gm1_lakers_min_tweets$text)
w_gm1_lakers_min_tweets$text <- gsub("rt", "", w_gm1_lakers_min_tweets$text)
w_gm1_lakers_min_tweets$text <- gsub("@\\w+", "", w_gm1_lakers_min_tweets$text)
w_gm1_lakers_min_tweets$text <- tolower(w_gm1_lakers_min_tweets$text)

sent_w_gm1_lakers_min_tweets <- sentiment(w_gm1_lakers_min_tweets$text)
w_gm1_lakers_min_tweets$sentiment <- sent_w_gm1_lakers_min_tweets$sentiment

w_gm1_lakers_min_tweets <- w_gm1_lakers_min_tweets %>% mutate(game_id = "w_gm_1")

rand_w_gm1_lakers_min_tweets <- w_gm1_lakers_min_tweets[sample(nrow(w_gm1_lakers_min_tweets), size=1000), ]
# View(rand_w_gm1_lakers_min_tweets)


## Lakers vs CHI 1-23-21
w_gm2_lakers_chi_tweets <-
  get_all_tweets(
    query = "lakers",
    start_tweets = "2021-01-23T07:00:00Z",
    end_tweets = "2021-01-24T06:59:00Z",
    data_path = "h2_lakers_chi_1-23/",
    bind_tweets = FALSE,
    n = 1000
  )
w_gm2_lakers_chi_tweets <- bind_tweets(data_path = "h2_lakers_chi_1-23/")

w_gm2_lakers_chi_tweets$text <- gsub("[^0-9A-Za-z///' ]","", w_gm2_lakers_chi_tweets$text)
w_gm2_lakers_chi_tweets$text <- gsub("http\\w+", "", w_gm2_lakers_chi_tweets$text)
w_gm2_lakers_chi_tweets$text <- gsub("rt", "", w_gm2_lakers_chi_tweets$text)
w_gm2_lakers_chi_tweets$text <- gsub("@\\w+", "", w_gm2_lakers_chi_tweets$text)
w_gm2_lakers_chi_tweets$text <- tolower(w_gm2_lakers_chi_tweets$text)

sent_w_gm2_lakers_chi_tweets <- sentiment(w_gm2_lakers_chi_tweets$text)
w_gm2_lakers_chi_tweets$sentiment <- sent_w_gm2_lakers_chi_tweets$sentiment

w_gm2_lakers_chi_tweets <- w_gm2_lakers_chi_tweets %>% mutate(game_id = "w_gm_2")

rand_w_gm2_lakers_chi_tweets <- w_gm2_lakers_chi_tweets[sample(nrow(w_gm2_lakers_chi_tweets), size=1000), ]
# View(rand_w_gm2_lakers_chi_tweets)


## Lakers vs CHI 1-8-21
w_gm3_lakers_chi_tweets <-
  get_all_tweets(
    query = "lakers",
    start_tweets = "2021-01-08T07:00:00Z",
    end_tweets = "2021-01-09T06:59:00Z",
    data_path = "h2_lakers_chi_1-8/",
    bind_tweets = FALSE,
    n = 1000
  )
w_gm3_lakers_chi_tweets <- bind_tweets(data_path = "h2_lakers_chi_1-8/")

w_gm3_lakers_chi_tweets$text <- gsub("[^0-9A-Za-z///' ]","", w_gm3_lakers_chi_tweets$text)
w_gm3_lakers_chi_tweets$text <- gsub("http\\w+", "", w_gm3_lakers_chi_tweets$text)
w_gm3_lakers_chi_tweets$text <- gsub("rt", "", w_gm3_lakers_chi_tweets$text)
w_gm3_lakers_chi_tweets$text <- gsub("@\\w+", "", w_gm3_lakers_chi_tweets$text)
w_gm3_lakers_chi_tweets$text <- tolower(w_gm3_lakers_chi_tweets$text)

sent_w_gm3_lakers_chi_tweets <- sentiment(w_gm3_lakers_chi_tweets$text)
w_gm3_lakers_chi_tweets$sentiment <- sent_w_gm3_lakers_chi_tweets$sentiment

w_gm3_lakers_chi_tweets <- w_gm3_lakers_chi_tweets %>% mutate(game_id = "w_gm_3")

rand_w_gm3_lakers_chi_tweets <- w_gm3_lakers_chi_tweets[sample(nrow(w_gm3_lakers_chi_tweets), size=1000), ]
# View(rand_w_gm3_lakers_chi_tweets)


## Lakers vs UTA 4-17-21
w_gm4_lakers_uta_tweets <-
  get_all_tweets(
    query = "lakers",
    start_tweets = "2021-04-17T07:00:00Z",
    end_tweets = "2021-04-18T06:59:00Z",
    data_path = "h2_lakers_uta_4-17/",
    bind_tweets = FALSE,
    n = 1000
  )
w_gm4_lakers_uta_tweets <- bind_tweets(data_path = "h2_lakers_uta_4-17/")

w_gm4_lakers_uta_tweets$text <- gsub("[^0-9A-Za-z///' ]","", w_gm4_lakers_uta_tweets$text)
w_gm4_lakers_uta_tweets$text <- gsub("http\\w+", "", w_gm4_lakers_uta_tweets$text)
w_gm4_lakers_uta_tweets$text <- gsub("rt", "", w_gm4_lakers_uta_tweets$text)
w_gm4_lakers_uta_tweets$text <- gsub("@\\w+", "", w_gm4_lakers_uta_tweets$text)
w_gm4_lakers_uta_tweets$text <- tolower(w_gm4_lakers_uta_tweets$text)

sent_w_gm4_lakers_uta_tweets <- sentiment(w_gm4_lakers_uta_tweets$text)
w_gm4_lakers_uta_tweets$sentiment <- sent_w_gm4_lakers_uta_tweets$sentiment

w_gm4_lakers_uta_tweets <- w_gm4_lakers_uta_tweets %>% mutate(game_id = "w_gm_4")

rand_w_gm4_lakers_uta_tweets <- w_gm4_lakers_uta_tweets[sample(nrow(w_gm4_lakers_uta_tweets), size=1000), ]
# View(rand_w_gm4_lakers_uta_tweets)


## Lakers vs IND 5-15-21
w_gm5_lakers_ind_tweets <-
  get_all_tweets(
    query = "lakers",
    start_tweets = "2021-05-15T07:00:00Z",
    end_tweets = "2021-05-16T06:59:00Z",
    data_path = "h2_lakers_ind_5-15/",
    bind_tweets = FALSE,
    n = 1000
  )
w_gm5_lakers_ind_tweets <- bind_tweets(data_path = "h2_lakers_ind_5-15/")

w_gm5_lakers_ind_tweets$text <- gsub("[^0-9A-Za-z///' ]","", w_gm5_lakers_ind_tweets$text)
w_gm5_lakers_ind_tweets$text <- gsub("http\\w+", "", w_gm5_lakers_ind_tweets$text)
w_gm5_lakers_ind_tweets$text <- gsub("rt", "", w_gm5_lakers_ind_tweets$text)
w_gm5_lakers_ind_tweets$text <- gsub("@\\w+", "", w_gm5_lakers_ind_tweets$text)
w_gm5_lakers_ind_tweets$text <- tolower(w_gm5_lakers_ind_tweets$text)

sent_w_gm5_lakers_ind_tweets <- sentiment(w_gm5_lakers_ind_tweets$text)
w_gm5_lakers_ind_tweets$sentiment <- sent_w_gm5_lakers_ind_tweets$sentiment

w_gm5_lakers_ind_tweets <- w_gm5_lakers_ind_tweets %>% mutate(game_id = "w_gm_5")

rand_w_gm5_lakers_ind_tweets <- w_gm5_lakers_ind_tweets[sample(nrow(w_gm5_lakers_ind_tweets), size=1000), ]
# View(rand_w_gm5_lakers_ind_tweets)


## Lakers vs CHA 4-13-21
w_gm6_lakers_cha_tweets <-
  get_all_tweets(
    query = "lakers",
    start_tweets = "2021-04-13T07:00:00Z",
    end_tweets = "2021-04-14T06:59:00Z",
    data_path = "h2_lakers_cha_4-13/",
    bind_tweets = FALSE,
    n = 1000
  )
w_gm6_lakers_cha_tweets <- bind_tweets(data_path = "h2_lakers_cha_4-13/")

w_gm6_lakers_cha_tweets$text <- gsub("[^0-9A-Za-z///' ]","", w_gm6_lakers_cha_tweets$text)
w_gm6_lakers_cha_tweets$text <- gsub("http\\w+", "", w_gm6_lakers_cha_tweets$text)
w_gm6_lakers_cha_tweets$text <- gsub("rt", "", w_gm6_lakers_cha_tweets$text)
w_gm6_lakers_cha_tweets$text <- gsub("@\\w+", "", w_gm6_lakers_cha_tweets$text)
w_gm6_lakers_cha_tweets$text <- tolower(w_gm6_lakers_cha_tweets$text)

sent_w_gm6_lakers_cha_tweets <- sentiment(w_gm6_lakers_cha_tweets$text)
w_gm6_lakers_cha_tweets$sentiment <- sent_w_gm6_lakers_cha_tweets$sentiment

w_gm6_lakers_cha_tweets <- w_gm6_lakers_cha_tweets %>% mutate(game_id = "w_gm_6")

rand_w_gm6_lakers_cha_tweets <- w_gm6_lakers_cha_tweets[sample(nrow(w_gm6_lakers_cha_tweets), size=1000), ]
# View(rand_w_gm6_lakers_cha_tweets)


## Lakers vs IND 3-12-21
w_gm7_lakers_ind_tweets <-
  get_all_tweets(
    query = "lakers",
    start_tweets = "2021-03-12T07:00:00Z",
    end_tweets = "2021-03-13T06:59:00Z",
    data_path = "h2_lakers_ind_3-12/",
    bind_tweets = FALSE,
    n = 1000
  )
w_gm7_lakers_ind_tweets <- bind_tweets(data_path = "h2_lakers_ind_3-12/")

w_gm7_lakers_ind_tweets$text <- gsub("[^0-9A-Za-z///' ]","", w_gm7_lakers_ind_tweets$text)
w_gm7_lakers_ind_tweets$text <- gsub("http\\w+", "", w_gm7_lakers_ind_tweets$text)
w_gm7_lakers_ind_tweets$text <- gsub("rt", "", w_gm7_lakers_ind_tweets$text)
w_gm7_lakers_ind_tweets$text <- gsub("@\\w+", "", w_gm7_lakers_ind_tweets$text)
w_gm7_lakers_ind_tweets$text <- tolower(w_gm7_lakers_ind_tweets$text)

sent_w_gm7_lakers_ind_tweets <- sentiment(w_gm7_lakers_ind_tweets$text)
w_gm7_lakers_ind_tweets$sentiment <- sent_w_gm7_lakers_ind_tweets$sentiment

w_gm7_lakers_ind_tweets <- w_gm7_lakers_ind_tweets %>% mutate(game_id = "w_gm_7")

rand_w_gm7_lakers_ind_tweets <- w_gm7_lakers_ind_tweets[sample(nrow(w_gm7_lakers_ind_tweets), size=1000), ]
# View(rand_w_gm7_lakers_ind_tweets)


## Lakers vs CLE 1-25-21
w_gm8_lakers_cle_tweets <-
  get_all_tweets(
    query = "lakers",
    start_tweets = "2021-01-25T07:00:00Z",
    end_tweets = "2021-01-26T06:59:00Z",
    data_path = "h2_lakers_cle_1-25/",
    bind_tweets = FALSE,
    n = 1000
  )
w_gm8_lakers_cle_tweets <- bind_tweets(data_path = "h2_lakers_cle_1-25/")

w_gm8_lakers_cle_tweets$text <- gsub("[^0-9A-Za-z///' ]","", w_gm8_lakers_cle_tweets$text)
w_gm8_lakers_cle_tweets$text <- gsub("http\\w+", "", w_gm8_lakers_cle_tweets$text)
w_gm8_lakers_cle_tweets$text <- gsub("rt", "", w_gm8_lakers_cle_tweets$text)
w_gm8_lakers_cle_tweets$text <- gsub("@\\w+", "", w_gm8_lakers_cle_tweets$text)
w_gm8_lakers_cle_tweets$text <- tolower(w_gm8_lakers_cle_tweets$text)

sent_w_gm8_lakers_cle_tweets <- sentiment(w_gm8_lakers_cle_tweets$text)
w_gm8_lakers_cle_tweets$sentiment <- sent_w_gm8_lakers_cle_tweets$sentiment

w_gm8_lakers_cle_tweets <- w_gm8_lakers_cle_tweets %>% mutate(game_id = "w_gm_8")

rand_w_gm8_lakers_cle_tweets <- w_gm8_lakers_cle_tweets[sample(nrow(w_gm8_lakers_cle_tweets), size=1000), ]
# View(rand_w_gm8_lakers_cle_tweets)


## Lakers vs POR 2-26-21
w_gm9_lakers_por_tweets <-
  get_all_tweets(
    query = "lakers",
    start_tweets = "2021-02-26T07:00:00Z",
    end_tweets = "2021-02-27T06:59:00Z",
    data_path = "h2_lakers_por_2-26/",
    bind_tweets = FALSE,
    n = 1000
  )
w_gm9_lakers_por_tweets <- bind_tweets(data_path = "h2_lakers_por_2-26/")

w_gm9_lakers_por_tweets$text <- gsub("[^0-9A-Za-z///' ]","", w_gm9_lakers_por_tweets$text)
w_gm9_lakers_por_tweets$text <- gsub("http\\w+", "", w_gm9_lakers_por_tweets$text)
w_gm9_lakers_por_tweets$text <- gsub("rt", "", w_gm9_lakers_por_tweets$text)
w_gm9_lakers_por_tweets$text <- gsub("@\\w+", "", w_gm9_lakers_por_tweets$text)
w_gm9_lakers_por_tweets$text <- tolower(w_gm9_lakers_por_tweets$text)

sent_w_gm9_lakers_por_tweets <- sentiment(w_gm9_lakers_por_tweets$text)
w_gm9_lakers_por_tweets$sentiment <- sent_w_gm9_lakers_por_tweets$sentiment

w_gm9_lakers_por_tweets <- w_gm9_lakers_por_tweets %>% mutate(game_id = "w_gm_9")

rand_w_gm9_lakers_por_tweets <- w_gm9_lakers_por_tweets[sample(nrow(w_gm9_lakers_por_tweets), size=1000), ]
# View(rand_w_gm9_lakers_por_tweets)


## Lakers vs SAS 1-1-21
w_gm10_lakers_sas_tweets <-
  get_all_tweets(
    query = "lakers",
    start_tweets = "2021-01-01T07:00:00Z",
    end_tweets = "2021-01-02T06:59:00Z",
    data_path = "h2_lakers_sas_1-1/",
    bind_tweets = FALSE,
    n = 1000
  )
w_gm10_lakers_sas_tweets <- bind_tweets(data_path = "h2_lakers_sas_1-1/")

w_gm10_lakers_sas_tweets$text <- gsub("[^0-9A-Za-z///' ]","", w_gm10_lakers_sas_tweets$text)
w_gm10_lakers_sas_tweets$text <- gsub("http\\w+", "", w_gm10_lakers_sas_tweets$text)
w_gm10_lakers_sas_tweets$text <- gsub("rt", "", w_gm10_lakers_sas_tweets$text)
w_gm10_lakers_sas_tweets$text <- gsub("@\\w+", "", w_gm10_lakers_sas_tweets$text)
w_gm10_lakers_sas_tweets$text <- tolower(w_gm10_lakers_sas_tweets$text)

sent_w_gm10_lakers_sas_tweets <- sentiment(w_gm10_lakers_sas_tweets$text)
w_gm10_lakers_sas_tweets$sentiment <- sent_w_gm10_lakers_sas_tweets$sentiment

w_gm10_lakers_sas_tweets <- w_gm10_lakers_sas_tweets %>% mutate(game_id = "w_gm_10")

rand_w_gm10_lakers_sas_tweets <- w_gm10_lakers_sas_tweets[sample(nrow(w_gm10_lakers_sas_tweets), size=1000), ]
# View(rand_w_gm10_lakers_sas_tweets)


## Lakers vs NOP 5-16-21
w_gm11_lakers_nop_tweets <-
  get_all_tweets(
    query = "lakers",
    start_tweets = "2021-05-16T07:00:00Z",
    end_tweets = "2021-05-17T06:59:00Z",
    data_path = "h2_lakers_nop_5-16/",
    bind_tweets = FALSE,
    n = 1000
  )
w_gm11_lakers_nop_tweets <- bind_tweets(data_path = "h2_lakers_nop_5-16/")

w_gm11_lakers_nop_tweets$text <- gsub("[^0-9A-Za-z///' ]","", w_gm11_lakers_nop_tweets$text)
w_gm11_lakers_nop_tweets$text <- gsub("http\\w+", "", w_gm11_lakers_nop_tweets$text)
w_gm11_lakers_nop_tweets$text <- gsub("rt", "", w_gm11_lakers_nop_tweets$text)
w_gm11_lakers_nop_tweets$text <- gsub("@\\w+", "", w_gm11_lakers_nop_tweets$text)
w_gm11_lakers_nop_tweets$text <- tolower(w_gm11_lakers_nop_tweets$text)

sent_w_gm11_lakers_nop_tweets <- sentiment(w_gm11_lakers_nop_tweets$text)
w_gm11_lakers_nop_tweets$sentiment <- sent_w_gm11_lakers_nop_tweets$sentiment

w_gm11_lakers_nop_tweets <- w_gm11_lakers_nop_tweets %>% mutate(game_id = "w_gm_11")

rand_w_gm11_lakers_nop_tweets <- w_gm11_lakers_nop_tweets[sample(nrow(w_gm11_lakers_nop_tweets), size=1000), ]
# View(rand_w_gm11_lakers_nop_tweets)


## Lakers vs MEM 1-5-21
w_gm12_lakers_mem_tweets <-
  get_all_tweets(
    query = "lakers",
    start_tweets = "2021-01-05T07:00:00Z",
    end_tweets = "2021-01-06T06:59:00Z",
    data_path = "h2_lakers_mem_1-5/",
    bind_tweets = FALSE,
    n = 1000
  )
w_gm12_lakers_mem_tweets <- bind_tweets(data_path = "h2_lakers_mem_1-5/")

w_gm12_lakers_mem_tweets$text <- gsub("[^0-9A-Za-z///' ]","", w_gm12_lakers_mem_tweets$text)
w_gm12_lakers_mem_tweets$text <- gsub("http\\w+", "", w_gm12_lakers_mem_tweets$text)
w_gm12_lakers_mem_tweets$text <- gsub("rt", "", w_gm12_lakers_mem_tweets$text)
w_gm12_lakers_mem_tweets$text <- gsub("@\\w+", "", w_gm12_lakers_mem_tweets$text)
w_gm12_lakers_mem_tweets$text <- tolower(w_gm12_lakers_mem_tweets$text)

sent_w_gm12_lakers_mem_tweets <- sentiment(w_gm12_lakers_mem_tweets$text)
w_gm12_lakers_mem_tweets$sentiment <- sent_w_gm12_lakers_mem_tweets$sentiment

w_gm12_lakers_mem_tweets <- w_gm12_lakers_mem_tweets %>% mutate(game_id = "w_gm_12")

rand_w_gm12_lakers_mem_tweets <- w_gm12_lakers_mem_tweets[sample(nrow(w_gm12_lakers_mem_tweets), size=1000), ]
# View(rand_w_gm12_lakers_mem_tweets)


## Lakers vs OKC 2-8-21
w_gm13_lakers_okc_tweets <-
  get_all_tweets(
    query = "lakers",
    start_tweets = "2021-02-08T07:00:00Z",
    end_tweets = "2021-02-09T06:59:00Z",
    data_path = "h2_lakers_okc_2-8/",
    bind_tweets = FALSE,
    n = 1000
  )
w_gm13_lakers_okc_tweets <- bind_tweets(data_path = "h2_lakers_okc_2-8/")

w_gm13_lakers_okc_tweets$text <- gsub("[^0-9A-Za-z///' ]","", w_gm13_lakers_okc_tweets$text)
w_gm13_lakers_okc_tweets$text <- gsub("http\\w+", "", w_gm13_lakers_okc_tweets$text)
w_gm13_lakers_okc_tweets$text <- gsub("rt", "", w_gm13_lakers_okc_tweets$text)
w_gm13_lakers_okc_tweets$text <- gsub("@\\w+", "", w_gm13_lakers_okc_tweets$text)
w_gm13_lakers_okc_tweets$text <- tolower(w_gm13_lakers_okc_tweets$text)

sent_w_gm13_lakers_okc_tweets <- sentiment(w_gm13_lakers_okc_tweets$text)
w_gm13_lakers_okc_tweets$sentiment <- sent_w_gm13_lakers_okc_tweets$sentiment

w_gm13_lakers_okc_tweets <- w_gm13_lakers_okc_tweets %>% mutate(game_id = "w_gm_13")

rand_w_gm13_lakers_okc_tweets <- w_gm13_lakers_okc_tweets[sample(nrow(w_gm13_lakers_okc_tweets), size=1000), ]
# View(rand_w_gm13_lakers_okc_tweets)


## Lakers vs GSW 2-28-21
w_gm14_lakers_gsw_tweets <-
  get_all_tweets(
    query = "lakers",
    start_tweets = "2021-02-28T07:00:00Z",
    end_tweets = "2021-03-01T06:59:00Z",
    data_path = "h2_lakers_gsw_2-28/",
    bind_tweets = FALSE,
    n = 1000
  )
w_gm14_lakers_gsw_tweets <- bind_tweets(data_path = "h2_lakers_gsw_2-28/")

w_gm14_lakers_gsw_tweets$text <- gsub("[^0-9A-Za-z///' ]","", w_gm14_lakers_gsw_tweets$text)
w_gm14_lakers_gsw_tweets$text <- gsub("http\\w+", "", w_gm14_lakers_gsw_tweets$text)
w_gm14_lakers_gsw_tweets$text <- gsub("rt", "", w_gm14_lakers_gsw_tweets$text)
w_gm14_lakers_gsw_tweets$text <- gsub("@\\w+", "", w_gm14_lakers_gsw_tweets$text)
w_gm14_lakers_gsw_tweets$text <- tolower(w_gm14_lakers_gsw_tweets$text)

sent_w_gm14_lakers_gsw_tweets <- sentiment(w_gm14_lakers_gsw_tweets$text)
w_gm14_lakers_gsw_tweets$sentiment <- sent_w_gm14_lakers_gsw_tweets$sentiment

w_gm14_lakers_gsw_tweets <- w_gm14_lakers_gsw_tweets %>% mutate(game_id = "w_gm_14")

rand_w_gm14_lakers_gsw_tweets <- w_gm14_lakers_gsw_tweets[sample(nrow(w_gm14_lakers_gsw_tweets), size=1000), ]
# View(rand_w_gm14_lakers_gsw_tweets)


## Lakers vs CHA 3-18-21
w_gm15_lakers_cha_tweets <-
  get_all_tweets(
    query = "lakers",
    start_tweets = "2021-03-18T07:00:00Z",
    end_tweets = "2021-03-19T06:59:00Z",
    data_path = "h2_lakers_cha_3-18/",
    bind_tweets = FALSE,
    n = 1000
  )
w_gm15_lakers_cha_tweets <- bind_tweets(data_path = "h2_lakers_cha_3-18/")

w_gm15_lakers_cha_tweets$text <- gsub("[^0-9A-Za-z///' ]","", w_gm15_lakers_cha_tweets$text)
w_gm15_lakers_cha_tweets$text <- gsub("http\\w+", "", w_gm15_lakers_cha_tweets$text)
w_gm15_lakers_cha_tweets$text <- gsub("rt", "", w_gm15_lakers_cha_tweets$text)
w_gm15_lakers_cha_tweets$text <- gsub("@\\w+", "", w_gm15_lakers_cha_tweets$text)
w_gm15_lakers_cha_tweets$text <- tolower(w_gm15_lakers_cha_tweets$text)

sent_w_gm15_lakers_cha_tweets <- sentiment(w_gm15_lakers_cha_tweets$text)
w_gm15_lakers_cha_tweets$sentiment <- sent_w_gm15_lakers_cha_tweets$sentiment

w_gm15_lakers_cha_tweets <- w_gm15_lakers_cha_tweets %>% mutate(game_id = "w_gm_15")

rand_w_gm15_lakers_cha_tweets <- w_gm15_lakers_cha_tweets[sample(nrow(w_gm15_lakers_cha_tweets), size=1000), ]
# View(rand_w_gm15_lakers_cha_tweets)





### Small Versions of Random 1000-Tweet Samples for Game Losses
### 'Small' meaning reduced number of columns
sm_rand_l_gm1_lakers_phx_tweets <- rand_l_gm1_lakers_phx_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id)
# View(sm_rand_l_gm1_lakers_phx_tweets)

sm_rand_l_gm2_lakers_uta_tweets <- rand_l_gm2_lakers_uta_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id)
# View(sm_rand_l_gm2_lakers_uta_tweets)

sm_rand_l_gm3_lakers_mia_tweets <- rand_l_gm3_lakers_mia_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id)
# View(sm_rand_l_gm3_lakers_mia_tweets)

sm_rand_l_gm4_lakers_por_tweets <- rand_l_gm4_lakers_por_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id)
# View(sm_rand_l_gm4_lakers_por_tweets)

sm_rand_l_gm5_lakers_nyk_tweets <- rand_l_gm5_lakers_nyk_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id)
# View(sm_rand_l_gm5_lakers_nyk_tweets)

sm_rand_l_gm6_lakers_phx_tweets <- rand_l_gm6_lakers_phx_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id)
# View(sm_rand_l_gm6_lakers_phx_tweets)

sm_rand_l_gm7_lakers_lac_tweets <- rand_l_gm7_lakers_lac_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id)
# View(sm_rand_l_gm7_lakers_lac_tweets)

sm_rand_l_gm8_lakers_nop_tweets <- rand_l_gm8_lakers_nop_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id)
# View(sm_rand_l_gm8_lakers_nop_tweets)

sm_rand_l_gm9_lakers_gsw_tweets <- rand_l_gm9_lakers_gsw_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id)
# View(sm_rand_l_gm9_lakers_gsw_tweets)

sm_rand_l_gm10_lakers_tor_tweets <- rand_l_gm10_lakers_tor_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id)
# View(sm_rand_l_gm10_lakers_tor_tweets)

sm_rand_l_gm11_lakers_mil_tweets <- rand_l_gm11_lakers_mil_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id)
# View(sm_rand_l_gm11_lakers_mil_tweets)

sm_rand_l_gm12_lakers_det_tweets <- rand_l_gm12_lakers_det_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id)
# View(sm_rand_l_gm12_lakers_det_tweets)

sm_rand_l_gm13_lakers_lac_tweets <- rand_l_gm13_lakers_lac_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id)
# View(sm_rand_l_gm13_lakers_lac_tweets)

sm_rand_l_gm14_lakers_sas_tweets <- rand_l_gm14_lakers_sas_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id)
# View(sm_rand_l_gm14_lakers_sas_tweets)

sm_rand_l_gm15_lakers_dal_tweets <- rand_l_gm15_lakers_dal_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id)
# View(sm_rand_l_gm15_lakers_dal_tweets)





### Small Versions of Random 1000-Tweet Samples for Game Wins
### 'Small' meaning reduced number of columns
sm_rand_w_gm1_lakers_min_tweets <- rand_w_gm1_lakers_min_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id)
# View(sm_rand_w_gm1_lakers_min_tweets)

sm_rand_w_gm2_lakers_chi_tweets <- rand_w_gm2_lakers_chi_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id)
# View(sm_rand_w_gm2_lakers_chi_tweets)

sm_rand_w_gm3_lakers_chi_tweets <- rand_w_gm3_lakers_chi_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id)
# View(sm_rand_w_gm3_lakers_chi_tweets)

sm_rand_w_gm4_lakers_uta_tweets <- rand_w_gm4_lakers_uta_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id)
# View(sm_rand_w_gm4_lakers_uta_tweets)

sm_rand_w_gm5_lakers_ind_tweets <- rand_w_gm5_lakers_ind_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id)
# View(sm_rand_w_gm5_lakers_ind_tweets)

sm_rand_w_gm6_lakers_cha_tweets <- rand_w_gm6_lakers_cha_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id)
# View(sm_rand_w_gm6_lakers_cha_tweets)

sm_rand_w_gm7_lakers_ind_tweets <- rand_w_gm7_lakers_ind_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id)
# View(sm_rand_w_gm7_lakers_ind_tweets)

sm_rand_w_gm8_lakers_cle_tweets <- rand_w_gm8_lakers_cle_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id)
# View(sm_rand_w_gm8_lakers_cle_tweets)

sm_rand_w_gm9_lakers_por_tweets <- rand_w_gm9_lakers_por_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id)
# View(sm_rand_w_gm9_lakers_por_tweets)

sm_rand_w_gm10_lakers_sas_tweets <- rand_w_gm10_lakers_sas_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id)
# View(sm_rand_w_gm10_lakers_sas_tweets)

sm_rand_w_gm11_lakers_nop_tweets <- rand_w_gm11_lakers_nop_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id)
# View(sm_rand_w_gm11_lakers_nop_tweets)

sm_rand_w_gm12_lakers_mem_tweets <- rand_w_gm12_lakers_mem_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id)
# View(sm_rand_w_gm12_lakers_mem_tweets)

sm_rand_w_gm13_lakers_okc_tweets <- rand_w_gm13_lakers_okc_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id)
# View(sm_rand_w_gm13_lakers_okc_tweets)

sm_rand_w_gm14_lakers_gsw_tweets <- rand_w_gm14_lakers_gsw_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id)
# View(sm_rand_w_gm14_lakers_gsw_tweets)

sm_rand_w_gm15_lakers_cha_tweets <- rand_w_gm15_lakers_cha_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id)
# View(sm_rand_w_gm15_lakers_cha_tweets)


### Merging of Game Losses
merge_gms_l <- bind_rows(sm_rand_l_gm1_lakers_phx_tweets, sm_rand_l_gm2_lakers_uta_tweets, sm_rand_l_gm3_lakers_mia_tweets, sm_rand_l_gm4_lakers_por_tweets, sm_rand_l_gm5_lakers_nyk_tweets, 
                         sm_rand_l_gm6_lakers_phx_tweets, sm_rand_l_gm7_lakers_lac_tweets, sm_rand_l_gm8_lakers_nop_tweets, sm_rand_l_gm9_lakers_gsw_tweets, sm_rand_l_gm10_lakers_tor_tweets, 
                         sm_rand_l_gm11_lakers_mil_tweets, sm_rand_l_gm12_lakers_det_tweets, sm_rand_l_gm13_lakers_lac_tweets, sm_rand_l_gm14_lakers_sas_tweets, sm_rand_l_gm15_lakers_dal_tweets)





### Merging of Game Wins
merge_gms_w <- bind_rows(sm_rand_w_gm1_lakers_min_tweets, sm_rand_w_gm2_lakers_chi_tweets, sm_rand_w_gm3_lakers_chi_tweets, sm_rand_w_gm4_lakers_uta_tweets, sm_rand_w_gm5_lakers_ind_tweets, 
                         sm_rand_w_gm6_lakers_cha_tweets, sm_rand_w_gm7_lakers_ind_tweets, sm_rand_w_gm8_lakers_cle_tweets, sm_rand_w_gm9_lakers_por_tweets, sm_rand_w_gm10_lakers_sas_tweets, 
                         sm_rand_w_gm11_lakers_nop_tweets, sm_rand_w_gm12_lakers_mem_tweets, sm_rand_w_gm13_lakers_okc_tweets, sm_rand_w_gm14_lakers_gsw_tweets, sm_rand_w_gm15_lakers_cha_tweets)





### Running the T-Test
t.test(merge_gms_l$sentiment, merge_gms_w$sentiment, mu=0, alt="two.sided", conf=0.95, var.eq=F, paired=F)





### Calculating standard deviations
sd(merge_gms_l$sentiment)

sd(merge_gms_w$sentiment)





### Box plot

plot_ly(
  data = merge_l_and_w_gms,
  y = ~sentiment,
  x = ~game_outcome,
  type = "box",
  color = ~game_outcome,
  showlegend = FALSE
)