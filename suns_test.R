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





### Hypothesis Test 1 - High-Scoring Games vs Low-Scoring Games 
### from PHX 2020-21 Season





### Data Collection

## Suns vs OKC 4-2-21
suns_okc_hi_gm1_tweets <-
  get_all_tweets(
    query = "suns",
    start_tweets = "2021-04-02T07:00:00Z",
    end_tweets = "2021-04-03T06:59:00Z",
    data_path = "suns_okc_4-2/",
    bind_tweets = FALSE,
    n = 1000
  )
suns_okc_hi_gm1_tweets <- bind_tweets(data_path = "suns_okc_4-2/")

suns_okc_hi_gm1_tweets$text <- gsub("[^0-9A-Za-z///' ]","", suns_okc_hi_gm1_tweets$text)
suns_okc_hi_gm1_tweets$text <- gsub("http\\w+", "", suns_okc_hi_gm1_tweets$text)
suns_okc_hi_gm1_tweets$text <- gsub("rt", "", suns_okc_hi_gm1_tweets$text)
suns_okc_hi_gm1_tweets$text <- gsub("@\\w+", "", suns_okc_hi_gm1_tweets$text)
suns_okc_hi_gm1_tweets$text <- tolower(suns_okc_hi_gm1_tweets$text)

sent_suns_okc_hi_gm1_tweets <- sentiment(suns_okc_hi_gm1_tweets$text)
suns_okc_hi_gm1_tweets$sentiment <- sent_suns_okc_hi_gm1_tweets$sentiment

suns_okc_hi_gm1_tweets <- suns_okc_hi_gm1_tweets %>% mutate(game_id = "hi_gm_1")

rand_suns_okc_hi_gm1_tweets <- suns_okc_hi_gm1_tweets[sample(nrow(suns_okc_hi_gm1_tweets), size=1000), ]
# View(rand_suns_okc_hi_gm1_tweets)


## Suns vs SAS 5-15-21
suns_sas_hi_gm2_tweets <-
  get_all_tweets(
    query = "suns",
    start_tweets = "2021-05-15T07:00:00Z",
    end_tweets = "2021-05-16T06:59:00Z",
    data_path = "suns_sas_5-15/",
    bind_tweets = FALSE,
    n = 1000
  )
suns_sas_hi_gm2_tweets <- bind_tweets(data_path = "suns_sas_5-15/")

suns_sas_hi_gm2_tweets$text <- gsub("[^0-9A-Za-z///' ]","", suns_sas_hi_gm2_tweets$text)
suns_sas_hi_gm2_tweets$text <- gsub("http\\w+", "", suns_sas_hi_gm2_tweets$text)
suns_sas_hi_gm2_tweets$text <- gsub("rt", "", suns_sas_hi_gm2_tweets$text)
suns_sas_hi_gm2_tweets$text <- gsub("@\\w+", "", suns_sas_hi_gm2_tweets$text)
suns_sas_hi_gm2_tweets$text <- tolower(suns_sas_hi_gm2_tweets$text)

sent_suns_sas_hi_gm2_tweets <- sentiment(suns_sas_hi_gm2_tweets$text)
suns_sas_hi_gm2_tweets$sentiment <- sent_suns_sas_hi_gm2_tweets$sentiment

suns_sas_hi_gm2_tweets <- suns_sas_hi_gm2_tweets %>% mutate(game_id = "hi_gm_2")

rand_suns_sas_hi_gm2_tweets <- suns_sas_hi_gm2_tweets[sample(nrow(suns_sas_hi_gm2_tweets), size=1000), ]
# View(rand_suns_sas_hi_gm2_tweets)


## Suns vs WAS 4-10-21
suns_was_hi_gm3_tweets <-
  get_all_tweets(
    query = "suns",
    start_tweets = "2021-04-10T07:00:00Z",
    end_tweets = "2021-04-11T06:59:00Z",
    data_path = "suns_was_4-10/",
    bind_tweets = FALSE,
    n = 1000
  )
suns_was_hi_gm3_tweets <- bind_tweets(data_path = "suns_was_4-10/")

suns_was_hi_gm3_tweets$text <- gsub("[^0-9A-Za-z///' ]","", suns_was_hi_gm3_tweets$text)
suns_was_hi_gm3_tweets$text <- gsub("http\\w+", "", suns_was_hi_gm3_tweets$text)
suns_was_hi_gm3_tweets$text <- gsub("rt", "", suns_was_hi_gm3_tweets$text)
suns_was_hi_gm3_tweets$text <- gsub("@\\w+", "", suns_was_hi_gm3_tweets$text)
suns_was_hi_gm3_tweets$text <- tolower(suns_was_hi_gm3_tweets$text)

sent_suns_was_hi_gm3_tweets <- sentiment(suns_was_hi_gm3_tweets$text)
suns_was_hi_gm3_tweets$sentiment <- sent_suns_was_hi_gm3_tweets$sentiment

suns_was_hi_gm3_tweets <- suns_was_hi_gm3_tweets %>% mutate(game_id = "hi_gm_3")

rand_suns_was_hi_gm3_tweets <- suns_was_hi_gm3_tweets[sample(nrow(suns_was_hi_gm3_tweets), size=1000), ]
# View(rand_suns_was_hi_gm3_tweets)


## Suns vs CLE 5-4-21
suns_cle_hi_gm4_tweets <-
  get_all_tweets(
    query = "suns",
    start_tweets = "2021-05-04T07:00:00Z",
    end_tweets = "2021-05-05T06:59:00Z",
    data_path = "suns_cle_5-4/",
    bind_tweets = FALSE,
    n = 1000
  )
suns_cle_hi_gm4_tweets <- bind_tweets(data_path = "suns_cle_5-4/")

suns_cle_hi_gm4_tweets$text <- gsub("[^0-9A-Za-z///' ]","", suns_cle_hi_gm4_tweets$text)
suns_cle_hi_gm4_tweets$text <- gsub("http\\w+", "", suns_cle_hi_gm4_tweets$text)
suns_cle_hi_gm4_tweets$text <- gsub("rt", "", suns_cle_hi_gm4_tweets$text)
suns_cle_hi_gm4_tweets$text <- gsub("@\\w+", "", suns_cle_hi_gm4_tweets$text)
suns_cle_hi_gm4_tweets$text <- tolower(suns_cle_hi_gm4_tweets$text)

sent_suns_cle_hi_gm4_tweets <- sentiment(suns_cle_hi_gm4_tweets$text)
suns_cle_hi_gm4_tweets$sentiment <- sent_suns_cle_hi_gm4_tweets$sentiment

suns_cle_hi_gm4_tweets <- suns_cle_hi_gm4_tweets %>% mutate(game_id = "hi_gm_4")

rand_suns_cle_hi_gm4_tweets <- suns_cle_hi_gm4_tweets[sample(nrow(suns_cle_hi_gm4_tweets), size=1000), ]
# View(rand_suns_cle_hi_gm4_tweets)


## Suns vs HOU 4-5-21
suns_hou_hi_gm5_tweets <-
  get_all_tweets(
    query = "suns",
    start_tweets = "2021-04-05T07:00:00Z",
    end_tweets = "2021-04-06T06:59:00Z",
    data_path = "suns_hou_4-5/",
    bind_tweets = FALSE,
    n = 1000
  )
suns_hou_hi_gm5_tweets <- bind_tweets(data_path = "suns_hou_4-5/")

suns_hou_hi_gm5_tweets$text <- gsub("[^0-9A-Za-z///' ]","", suns_hou_hi_gm5_tweets$text)
suns_hou_hi_gm5_tweets$text <- gsub("http\\w+", "", suns_hou_hi_gm5_tweets$text)
suns_hou_hi_gm5_tweets$text <- gsub("rt", "", suns_hou_hi_gm5_tweets$text)
suns_hou_hi_gm5_tweets$text <- gsub("@\\w+", "", suns_hou_hi_gm5_tweets$text)
suns_hou_hi_gm5_tweets$text <- tolower(suns_hou_hi_gm5_tweets$text)

sent_suns_hou_hi_gm5_tweets <- sentiment(suns_hou_hi_gm5_tweets$text)
suns_hou_hi_gm5_tweets$sentiment <- sent_suns_hou_hi_gm5_tweets$sentiment

suns_hou_hi_gm5_tweets <- suns_hou_hi_gm5_tweets %>% mutate(game_id = "hi_gm_5")

rand_suns_hou_hi_gm5_tweets <- suns_hou_hi_gm5_tweets[sample(nrow(suns_hou_hi_gm5_tweets), size=1000), ]
# View(rand_suns_hou_hi_gm5_tweets)


## Suns vs NOP 2-19-21
suns_nop_hi_gm6_tweets <-
  get_all_tweets(
    query = "suns",
    start_tweets = "2021-02-19T07:00:00Z",
    end_tweets = "2021-02-20T06:59:00Z",
    data_path = "suns_nop_2-19/",
    bind_tweets = FALSE,
    n = 1000
  )
suns_nop_hi_gm6_tweets <- bind_tweets(data_path = "suns_nop_2-19/")

suns_nop_hi_gm6_tweets$text <- gsub("[^0-9A-Za-z///' ]","", suns_nop_hi_gm6_tweets$text)
suns_nop_hi_gm6_tweets$text <- gsub("http\\w+", "", suns_nop_hi_gm6_tweets$text)
suns_nop_hi_gm6_tweets$text <- gsub("rt", "", suns_nop_hi_gm6_tweets$text)
suns_nop_hi_gm6_tweets$text <- gsub("@\\w+", "", suns_nop_hi_gm6_tweets$text)
suns_nop_hi_gm6_tweets$text <- tolower(suns_nop_hi_gm6_tweets$text)

sent_suns_nop_hi_gm6_tweets <- sentiment(suns_nop_hi_gm6_tweets$text)
suns_nop_hi_gm6_tweets$sentiment <- sent_suns_nop_hi_gm6_tweets$sentiment

suns_nop_hi_gm6_tweets <- suns_nop_hi_gm6_tweets %>% mutate(game_id = "hi_gm_6")

rand_suns_nop_hi_gm6_tweets <- suns_nop_hi_gm6_tweets[sample(nrow(suns_nop_hi_gm6_tweets), size=1000), ]
# View(rand_suns_nop_hi_gm6_tweets)


## Suns vs POR 2-22-21
suns_por_hi_gm7_tweets <-
  get_all_tweets(
    query = "suns",
    start_tweets = "2021-02-22T07:00:00Z",
    end_tweets = "2021-02-23T06:59:00Z",
    data_path = "suns_por_2-22/",
    bind_tweets = FALSE,
    n = 1000
  )
suns_por_hi_gm7_tweets <- bind_tweets(data_path = "suns_por_2-22/")

suns_por_hi_gm7_tweets$text <- gsub("[^0-9A-Za-z///' ]","", suns_por_hi_gm7_tweets$text)
suns_por_hi_gm7_tweets$text <- gsub("http\\w+", "", suns_por_hi_gm7_tweets$text)
suns_por_hi_gm7_tweets$text <- gsub("rt", "", suns_por_hi_gm7_tweets$text)
suns_por_hi_gm7_tweets$text <- gsub("@\\w+", "", suns_por_hi_gm7_tweets$text)
suns_por_hi_gm7_tweets$text <- tolower(suns_por_hi_gm7_tweets$text)

sent_suns_por_hi_gm7_tweets <- sentiment(suns_por_hi_gm7_tweets$text)
suns_por_hi_gm7_tweets$sentiment <- sent_suns_por_hi_gm7_tweets$sentiment

suns_por_hi_gm7_tweets <- suns_por_hi_gm7_tweets %>% mutate(game_id = "hi_gm_7")

rand_suns_por_hi_gm7_tweets <- suns_por_hi_gm7_tweets[sample(nrow(suns_por_hi_gm7_tweets), size=1000), ]
# View(rand_suns_por_hi_gm7_tweets)


## Suns vs MEM 2-20-21
suns_mem_hi_gm8_tweets <-
  get_all_tweets(
    query = "suns",
    start_tweets = "2021-02-20T07:00:00Z",
    end_tweets = "2021-02-21T06:59:00Z",
    data_path = "suns_mem_2-20/",
    bind_tweets = FALSE,
    n = 1000
  )
suns_mem_hi_gm8_tweets <- bind_tweets(data_path = "suns_mem_2-20/")

suns_mem_hi_gm8_tweets$text <- gsub("[^0-9A-Za-z///' ]","", suns_mem_hi_gm8_tweets$text)
suns_mem_hi_gm8_tweets$text <- gsub("http\\w+", "", suns_mem_hi_gm8_tweets$text)
suns_mem_hi_gm8_tweets$text <- gsub("rt", "", suns_mem_hi_gm8_tweets$text)
suns_mem_hi_gm8_tweets$text <- gsub("@\\w+", "", suns_mem_hi_gm8_tweets$text)
suns_mem_hi_gm8_tweets$text <- tolower(suns_mem_hi_gm8_tweets$text)

sent_suns_mem_hi_gm8_tweets <- sentiment(suns_mem_hi_gm8_tweets$text)
suns_mem_hi_gm8_tweets$sentiment <- sent_suns_mem_hi_gm8_tweets$sentiment

suns_mem_hi_gm8_tweets <- suns_mem_hi_gm8_tweets %>% mutate(game_id = "hi_gm_8")

rand_suns_mem_hi_gm8_tweets <- suns_mem_hi_gm8_tweets[sample(nrow(suns_mem_hi_gm8_tweets), size=1000), ]
# View(rand_suns_mem_hi_gm8_tweets)


## Suns vs MIL 4-19-21
suns_mil_hi_gm9_tweets <-
  get_all_tweets(
    query = "suns",
    start_tweets = "2021-04-19T07:00:00Z",
    end_tweets = "2021-04-20T06:59:00Z",
    data_path = "suns_mil_4-19/",
    bind_tweets = FALSE,
    n = 1000
  )
suns_mil_hi_gm9_tweets <- bind_tweets(data_path = "suns_mil_4-19/")

suns_mil_hi_gm9_tweets$text <- gsub("[^0-9A-Za-z///' ]","", suns_mil_hi_gm9_tweets$text)
suns_mil_hi_gm9_tweets$text <- gsub("http\\w+", "", suns_mil_hi_gm9_tweets$text)
suns_mil_hi_gm9_tweets$text <- gsub("rt", "", suns_mil_hi_gm9_tweets$text)
suns_mil_hi_gm9_tweets$text <- gsub("@\\w+", "", suns_mil_hi_gm9_tweets$text)
suns_mil_hi_gm9_tweets$text <- tolower(suns_mil_hi_gm9_tweets$text)

sent_suns_mil_hi_gm9_tweets <- sentiment(suns_mil_hi_gm9_tweets$text)
suns_mil_hi_gm9_tweets$sentiment <- sent_suns_mil_hi_gm9_tweets$sentiment

suns_mil_hi_gm9_tweets <- suns_mil_hi_gm9_tweets %>% mutate(game_id = "hi_gm_9")

rand_suns_mil_hi_gm9_tweets <- suns_mil_hi_gm9_tweets[sample(nrow(suns_mil_hi_gm9_tweets), size=1000), ]
# View(rand_suns_mil_hi_gm9_tweets)


## Suns vs NYK 5-7-21
suns_nyk_hi_gm10_tweets <-
  get_all_tweets(
    query = "suns",
    start_tweets = "2021-05-07T07:00:00Z",
    end_tweets = "2021-05-08T06:59:00Z",
    data_path = "suns_nyk_5-7/",
    bind_tweets = FALSE,
    n = 1000
  )
suns_nyk_hi_gm10_tweets <- bind_tweets(data_path = "suns_nyk_5-7/")

suns_nyk_hi_gm10_tweets$text <- gsub("[^0-9A-Za-z///' ]","", suns_nyk_hi_gm10_tweets$text)
suns_nyk_hi_gm10_tweets$text <- gsub("http\\w+", "", suns_nyk_hi_gm10_tweets$text)
suns_nyk_hi_gm10_tweets$text <- gsub("rt", "", suns_nyk_hi_gm10_tweets$text)
suns_nyk_hi_gm10_tweets$text <- gsub("@\\w+", "", suns_nyk_hi_gm10_tweets$text)
suns_nyk_hi_gm10_tweets$text <- tolower(suns_nyk_hi_gm10_tweets$text)

sent_suns_nyk_hi_gm10_tweets <- sentiment(suns_nyk_hi_gm10_tweets$text)
suns_nyk_hi_gm10_tweets$sentiment <- sent_suns_nyk_hi_gm10_tweets$sentiment

suns_nyk_hi_gm10_tweets <- suns_nyk_hi_gm10_tweets %>% mutate(game_id = "hi_gm_10")

rand_suns_nyk_hi_gm10_tweets <- suns_nyk_hi_gm10_tweets[sample(nrow(suns_nyk_hi_gm10_tweets), size=1000), ]
# View(rand_suns_nyk_hi_gm10_tweets)


## Suns vs POR 3-11-21
suns_por_hi_gm11_tweets <-
  get_all_tweets(
    query = "suns",
    start_tweets = "2021-03-11T07:00:00Z",
    end_tweets = "2021-03-12T06:59:00Z",
    data_path = "suns_por_3-11/",
    bind_tweets = FALSE,
    n = 1000
  )
suns_por_hi_gm11_tweets <- bind_tweets(data_path = "suns_por_3-11/")

suns_por_hi_gm11_tweets$text <- gsub("[^0-9A-Za-z///' ]","", suns_por_hi_gm11_tweets$text)
suns_por_hi_gm11_tweets$text <- gsub("http\\w+", "", suns_por_hi_gm11_tweets$text)
suns_por_hi_gm11_tweets$text <- gsub("rt", "", suns_por_hi_gm11_tweets$text)
suns_por_hi_gm11_tweets$text <- gsub("@\\w+", "", suns_por_hi_gm11_tweets$text)
suns_por_hi_gm11_tweets$text <- tolower(suns_por_hi_gm11_tweets$text)

sent_suns_por_hi_gm11_tweets <- sentiment(suns_por_hi_gm11_tweets$text)
suns_por_hi_gm11_tweets$sentiment <- sent_suns_por_hi_gm11_tweets$sentiment

suns_por_hi_gm11_tweets <- suns_por_hi_gm11_tweets %>% mutate(game_id = "hi_gm_11")

rand_suns_por_hi_gm11_tweets <- suns_por_hi_gm11_tweets[sample(nrow(suns_por_hi_gm11_tweets), size=1000), ]
# View(rand_suns_por_hi_gm11_tweets)


## Suns vs DEN 1-22-21
suns_den_hi_gm12_tweets <-
  get_all_tweets(
    query = "suns",
    start_tweets = "2021-01-22T07:00:00Z",
    end_tweets = "2021-01-23T06:59:00Z",
    data_path = "suns_den_1-22/",
    bind_tweets = FALSE,
    n = 1000
  )
suns_den_hi_gm12_tweets <- bind_tweets(data_path = "suns_den_1-22/")

suns_den_hi_gm12_tweets$text <- gsub("[^0-9A-Za-z///' ]","", suns_den_hi_gm12_tweets$text)
suns_den_hi_gm12_tweets$text <- gsub("http\\w+", "", suns_den_hi_gm12_tweets$text)
suns_den_hi_gm12_tweets$text <- gsub("rt", "", suns_den_hi_gm12_tweets$text)
suns_den_hi_gm12_tweets$text <- gsub("@\\w+", "", suns_den_hi_gm12_tweets$text)
suns_den_hi_gm12_tweets$text <- tolower(suns_den_hi_gm12_tweets$text)

sent_suns_den_hi_gm12_tweets <- sentiment(suns_den_hi_gm12_tweets$text)
suns_den_hi_gm12_tweets$sentiment <- sent_suns_den_hi_gm12_tweets$sentiment

suns_den_hi_gm12_tweets <- suns_den_hi_gm12_tweets %>% mutate(game_id = "hi_gm_12")

rand_suns_den_hi_gm12_tweets <- suns_den_hi_gm12_tweets[sample(nrow(suns_den_hi_gm12_tweets), size=1000), ]
# View(rand_suns_den_hi_gm12_tweets)


## Suns vs HOU 4-12-21
suns_hou_hi_gm13_tweets <-
  get_all_tweets(
    query = "suns",
    start_tweets = "2021-04-12T07:00:00Z",
    end_tweets = "2021-04-13T06:59:00Z",
    data_path = "suns_hou_4-12/",
    bind_tweets = FALSE,
    n = 1000
  )
suns_hou_hi_gm13_tweets <- bind_tweets(data_path = "suns_hou_4-12/")

suns_hou_hi_gm13_tweets$text <- gsub("[^0-9A-Za-z///' ]","", suns_hou_hi_gm13_tweets$text)
suns_hou_hi_gm13_tweets$text <- gsub("http\\w+", "", suns_hou_hi_gm13_tweets$text)
suns_hou_hi_gm13_tweets$text <- gsub("rt", "", suns_hou_hi_gm13_tweets$text)
suns_hou_hi_gm13_tweets$text <- gsub("@\\w+", "", suns_hou_hi_gm13_tweets$text)
suns_hou_hi_gm13_tweets$text <- tolower(suns_hou_hi_gm13_tweets$text)

sent_suns_hou_hi_gm13_tweets <- sentiment(suns_hou_hi_gm13_tweets$text)
suns_hou_hi_gm13_tweets$sentiment <- sent_suns_hou_hi_gm13_tweets$sentiment

suns_hou_hi_gm13_tweets <- suns_hou_hi_gm13_tweets %>% mutate(game_id = "hi_gm_13")

rand_suns_hou_hi_gm13_tweets <- suns_hou_hi_gm13_tweets[sample(nrow(suns_hou_hi_gm13_tweets), size=1000), ]
# View(rand_suns_hou_hi_gm13_tweets)


## Suns vs IND 1-9-21
suns_ind_hi_gm14_tweets <-
  get_all_tweets(
    query = "suns",
    start_tweets = "2021-01-09T07:00:00Z",
    end_tweets = "2021-01-10T06:59:00Z",
    data_path = "suns_ind_1-9/",
    bind_tweets = FALSE,
    n = 1000
  )
suns_ind_hi_gm14_tweets <- bind_tweets(data_path = "suns_ind_1-9/")

suns_ind_hi_gm14_tweets$text <- gsub("[^0-9A-Za-z///' ]","", suns_ind_hi_gm14_tweets$text)
suns_ind_hi_gm14_tweets$text <- gsub("http\\w+", "", suns_ind_hi_gm14_tweets$text)
suns_ind_hi_gm14_tweets$text <- gsub("rt", "", suns_ind_hi_gm14_tweets$text)
suns_ind_hi_gm14_tweets$text <- gsub("@\\w+", "", suns_ind_hi_gm14_tweets$text)
suns_ind_hi_gm14_tweets$text <- tolower(suns_ind_hi_gm14_tweets$text)

sent_suns_ind_hi_gm14_tweets <- sentiment(suns_ind_hi_gm14_tweets$text)
suns_ind_hi_gm14_tweets$sentiment <- sent_suns_ind_hi_gm14_tweets$sentiment

suns_ind_hi_gm14_tweets <- suns_ind_hi_gm14_tweets %>% mutate(game_id = "hi_gm_14")

rand_suns_ind_hi_gm14_tweets <- suns_ind_hi_gm14_tweets[sample(nrow(suns_ind_hi_gm14_tweets), size=1000), ]
# View(rand_suns_ind_hi_gm14_tweets)


## Suns VS MIL 2-10-21
suns_mil_hi_gm15_tweets <-
  get_all_tweets(
    query = "suns",
    start_tweets = "2021-02-10T07:00:00Z",
    end_tweets = "2021-02-11T06:59:00Z",
    data_path = "suns_mil_2-10/",
    bind_tweets = FALSE,
    n = 1000
  )
suns_mil_hi_gm15_tweets <- bind_tweets(data_path = "suns_mil_2-10/")

suns_mil_hi_gm15_tweets$text <- gsub("[^0-9A-Za-z///' ]","", suns_mil_hi_gm15_tweets$text)
suns_mil_hi_gm15_tweets$text <- gsub("http\\w+", "", suns_mil_hi_gm15_tweets$text)
suns_mil_hi_gm15_tweets$text <- gsub("rt", "", suns_mil_hi_gm15_tweets$text)
suns_mil_hi_gm15_tweets$text <- gsub("@\\w+", "", suns_mil_hi_gm15_tweets$text)
suns_mil_hi_gm15_tweets$text <- tolower(suns_mil_hi_gm15_tweets$text)

sent_suns_mil_hi_gm15_tweets <- sentiment(suns_mil_hi_gm15_tweets$text)
suns_mil_hi_gm15_tweets$sentiment <- sent_suns_mil_hi_gm15_tweets$sentiment

suns_mil_hi_gm15_tweets <- suns_mil_hi_gm15_tweets %>% mutate(game_id = "hi_gm_15")

rand_suns_mil_hi_gm15_tweets <- suns_mil_hi_gm15_tweets[sample(nrow(suns_mil_hi_gm15_tweets), size=1000), ]
# View(rand_suns_mil_hi_gm15_tweets)


## Suns vs DEN 1-1-21 (Beginning of the low-scoring games)
suns_den_lo_gm1_tweets <-
  get_all_tweets(
    query = "suns",
    start_tweets = "2021-01-01T07:00:00Z",
    end_tweets = "2021-01-02T06:59:00Z",
    data_path = "suns_den_1-1/",
    bind_tweets = FALSE,
    n = 1000
  )
suns_den_lo_gm1_tweets <- bind_tweets(data_path = "suns_den_1-1/")

suns_den_lo_gm1_tweets$text <- gsub("[^0-9A-Za-z///' ]","", suns_den_lo_gm1_tweets$text)
suns_den_lo_gm1_tweets$text <- gsub("http\\w+", "", suns_den_lo_gm1_tweets$text)
suns_den_lo_gm1_tweets$text <- gsub("rt", "", suns_den_lo_gm1_tweets$text)
suns_den_lo_gm1_tweets$text <- gsub("@\\w+", "", suns_den_lo_gm1_tweets$text)
suns_den_lo_gm1_tweets$text <- tolower(suns_den_lo_gm1_tweets$text)

sent_suns_den_lo_gm1_tweets <- sentiment(suns_den_lo_gm1_tweets$text)
suns_den_lo_gm1_tweets$sentiment <- sent_suns_den_lo_gm1_tweets$sentiment

suns_den_lo_gm1_tweets <- suns_den_lo_gm1_tweets %>% mutate(game_id = "lo_gm_1")

rand_suns_den_lo_gm1_tweets <- suns_den_lo_gm1_tweets[sample(nrow(suns_den_lo_gm1_tweets), size=1000), ]
# View(rand_suns_den_lo_gm1_tweets)


## Suns vs CHI 2-26-21
suns_chi_lo_gm2_tweets <-
  get_all_tweets(
    query = "suns",
    start_tweets = "2021-02-26T07:00:00Z",
    end_tweets = "2021-02-27T06:59:00Z",
    data_path = "suns_chi_2-26/",
    bind_tweets = FALSE,
    n = 1000
  )
suns_chi_lo_gm2_tweets <- bind_tweets(data_path = "suns_chi_2-26/")

suns_chi_lo_gm2_tweets$text <- gsub("[^0-9A-Za-z///' ]","", suns_chi_lo_gm2_tweets$text)
suns_chi_lo_gm2_tweets$text <- gsub("http\\w+", "", suns_chi_lo_gm2_tweets$text)
suns_chi_lo_gm2_tweets$text <- gsub("rt", "", suns_chi_lo_gm2_tweets$text)
suns_chi_lo_gm2_tweets$text <- gsub("@\\w+", "", suns_chi_lo_gm2_tweets$text)
suns_chi_lo_gm2_tweets$text <- tolower(suns_chi_lo_gm2_tweets$text)

sent_suns_chi_lo_gm2_tweets <- sentiment(suns_chi_lo_gm2_tweets$text)
suns_chi_lo_gm2_tweets$sentiment <- sent_suns_chi_lo_gm2_tweets$sentiment

suns_chi_lo_gm2_tweets <- suns_chi_lo_gm2_tweets %>% mutate(game_id = "lo_gm_2")

rand_suns_chi_lo_gm2_tweets <- suns_chi_lo_gm2_tweets[sample(nrow(suns_chi_lo_gm2_tweets), size=1000), ]
# View(rand_suns_chi_lo_gm2_tweets)


## Suns vs MIA 4-13-21
suns_mia_lo_gm3_tweets <-
  get_all_tweets(
    query = "suns",
    start_tweets = "2021-04-13T07:00:00Z",
    end_tweets = "2021-04-14T06:59:00Z",
    data_path = "suns_mia_4-13/",
    bind_tweets = FALSE,
    n = 1000
  )
suns_mia_lo_gm3_tweets <- bind_tweets(data_path = "suns_mia_4-13/")

suns_mia_lo_gm3_tweets$text <- gsub("[^0-9A-Za-z///' ]","", suns_mia_lo_gm3_tweets$text)
suns_mia_lo_gm3_tweets$text <- gsub("http\\w+", "", suns_mia_lo_gm3_tweets$text)
suns_mia_lo_gm3_tweets$text <- gsub("rt", "", suns_mia_lo_gm3_tweets$text)
suns_mia_lo_gm3_tweets$text <- gsub("@\\w+", "", suns_mia_lo_gm3_tweets$text)
suns_mia_lo_gm3_tweets$text <- tolower(suns_mia_lo_gm3_tweets$text)

sent_suns_mia_lo_gm3_tweets <- sentiment(suns_mia_lo_gm3_tweets$text)
suns_mia_lo_gm3_tweets$sentiment <- sent_suns_mia_lo_gm3_tweets$sentiment

suns_mia_lo_gm3_tweets <- suns_mia_lo_gm3_tweets %>% mutate(game_id = "lo_gm_3")

rand_suns_mia_lo_gm3_tweets <- suns_mia_lo_gm3_tweets[sample(nrow(suns_mia_lo_gm3_tweets), size=1000), ]
# View(rand_suns_mia_lo_gm3_tweets)


## Suns vs DET 1-8-21
suns_det_lo_gm4_tweets <-
  get_all_tweets(
    query = "suns",
    start_tweets = "2021-01-08T07:00:00Z",
    end_tweets = "2021-01-09T06:59:00Z",
    data_path = "suns_det_1-8/",
    bind_tweets = FALSE,
    n = 1000
  )
suns_det_lo_gm4_tweets <- bind_tweets(data_path = "suns_det_1-8/")

suns_det_lo_gm4_tweets$text <- gsub("[^0-9A-Za-z///' ]","", suns_det_lo_gm4_tweets$text)
suns_det_lo_gm4_tweets$text <- gsub("http\\w+", "", suns_det_lo_gm4_tweets$text)
suns_det_lo_gm4_tweets$text <- gsub("rt", "", suns_det_lo_gm4_tweets$text)
suns_det_lo_gm4_tweets$text <- gsub("@\\w+", "", suns_det_lo_gm4_tweets$text)
suns_det_lo_gm4_tweets$text <- tolower(suns_det_lo_gm4_tweets$text)

sent_suns_det_lo_gm4_tweets <- sentiment(suns_det_lo_gm4_tweets$text)
suns_det_lo_gm4_tweets$sentiment <- sent_suns_det_lo_gm4_tweets$sentiment

suns_det_lo_gm4_tweets <- suns_det_lo_gm4_tweets %>% mutate(game_id = "lo_gm_4")

rand_suns_det_lo_gm4_tweets <- suns_det_lo_gm4_tweets[sample(nrow(suns_det_lo_gm4_tweets), size=1000), ]
# View(rand_suns_det_lo_gm4_tweets)


## Suns vs MEM 1-18-21
suns_mem_lo_gm5_tweets <-
  get_all_tweets(
    query = "suns",
    start_tweets = "2021-01-18T07:00:00Z",
    end_tweets = "2021-01-19T06:59:00Z",
    data_path = "suns_mem_1-18/",
    bind_tweets = FALSE,
    n = 1000
  )
suns_mem_lo_gm5_tweets <- bind_tweets(data_path = "suns_mem_1-18/")

suns_mem_lo_gm5_tweets$text <- gsub("[^0-9A-Za-z///' ]","", suns_mem_lo_gm5_tweets$text)
suns_mem_lo_gm5_tweets$text <- gsub("http\\w+", "", suns_mem_lo_gm5_tweets$text)
suns_mem_lo_gm5_tweets$text <- gsub("rt", "", suns_mem_lo_gm5_tweets$text)
suns_mem_lo_gm5_tweets$text <- gsub("@\\w+", "", suns_mem_lo_gm5_tweets$text)
suns_mem_lo_gm5_tweets$text <- tolower(suns_mem_lo_gm5_tweets$text)

sent_suns_mem_lo_gm5_tweets <- sentiment(suns_mem_lo_gm5_tweets$text)
suns_mem_lo_gm5_tweets$sentiment <- sent_suns_mem_lo_gm5_tweets$sentiment

suns_mem_lo_gm5_tweets <- suns_mem_lo_gm5_tweets %>% mutate(game_id = "lo_gm_5")

rand_suns_mem_lo_gm5_tweets <- suns_mem_lo_gm5_tweets[sample(nrow(suns_mem_lo_gm5_tweets), size=1000), ]
# View(rand_suns_mem_lo_gm5_tweets)


## Suns vs TOR 3-26-21
suns_tor_lo_gm6_tweets <-
  get_all_tweets(
    query = "suns",
    start_tweets = "2021-03-26T07:00:00Z",
    end_tweets = "2021-03-27T06:59:00Z",
    data_path = "suns_tor_3-26/",
    bind_tweets = FALSE,
    n = 1000
  )
suns_tor_lo_gm6_tweets <- bind_tweets(data_path = "suns_tor_3-26/")

suns_tor_lo_gm6_tweets$text <- gsub("[^0-9A-Za-z///' ]","", suns_tor_lo_gm6_tweets$text)
suns_tor_lo_gm6_tweets$text <- gsub("http\\w+", "", suns_tor_lo_gm6_tweets$text)
suns_tor_lo_gm6_tweets$text <- gsub("rt", "", suns_tor_lo_gm6_tweets$text)
suns_tor_lo_gm6_tweets$text <- gsub("@\\w+", "", suns_tor_lo_gm6_tweets$text)
suns_tor_lo_gm6_tweets$text <- tolower(suns_tor_lo_gm6_tweets$text)

sent_suns_tor_lo_gm6_tweets <- sentiment(suns_tor_lo_gm6_tweets$text)
suns_tor_lo_gm6_tweets$sentiment <- sent_suns_tor_lo_gm6_tweets$sentiment

suns_tor_lo_gm6_tweets <- suns_tor_lo_gm6_tweets %>% mutate(game_id = "lo_gm_6")

rand_suns_tor_lo_gm6_tweets <- suns_tor_lo_gm6_tweets[sample(nrow(suns_tor_lo_gm6_tweets), size=1000), ]
# View(rand_suns_tor_lo_gm6_tweets)


## Suns vs SAC 12-26-20
suns_sac_lo_gm7_tweets <-
  get_all_tweets(
    query = "suns",
    start_tweets = "2020-12-26T07:00:00Z",
    end_tweets = "2020-12-27T06:59:00Z",
    data_path = "suns_sac_12-26/",
    bind_tweets = FALSE,
    n = 1000
  )
suns_sac_lo_gm7_tweets <- bind_tweets(data_path = "suns_sac_12-26/")

suns_sac_lo_gm7_tweets$text <- gsub("[^0-9A-Za-z///' ]","", suns_sac_lo_gm7_tweets$text)
suns_sac_lo_gm7_tweets$text <- gsub("http\\w+", "", suns_sac_lo_gm7_tweets$text)
suns_sac_lo_gm7_tweets$text <- gsub("rt", "", suns_sac_lo_gm7_tweets$text)
suns_sac_lo_gm7_tweets$text <- gsub("@\\w+", "", suns_sac_lo_gm7_tweets$text)
suns_sac_lo_gm7_tweets$text <- tolower(suns_sac_lo_gm7_tweets$text)

sent_suns_sac_lo_gm7_tweets <- sentiment(suns_sac_lo_gm7_tweets$text)
suns_sac_lo_gm7_tweets$sentiment <- sent_suns_sac_lo_gm7_tweets$sentiment

suns_sac_lo_gm7_tweets <- suns_sac_lo_gm7_tweets %>% mutate(game_id = "lo_gm_7")

rand_suns_sac_lo_gm7_tweets <- suns_sac_lo_gm7_tweets[sample(nrow(suns_sac_lo_gm7_tweets), size=1000), ]
# View(rand_suns_sac_lo_gm7_tweets)


## Suns vs LAC 4-8-21
suns_lac_lo_gm8_tweets <-
  get_all_tweets(
    query = "suns",
    start_tweets = "2021-04-08T07:00:00Z",
    end_tweets = "2021-04-09T06:59:00Z",
    data_path = "suns_lac_4-8/",
    bind_tweets = FALSE,
    n = 1000
  )
suns_lac_lo_gm8_tweets <- bind_tweets(data_path = "suns_lac_4-8/")

suns_lac_lo_gm8_tweets$text <- gsub("[^0-9A-Za-z///' ]","", suns_lac_lo_gm8_tweets$text)
suns_lac_lo_gm8_tweets$text <- gsub("http\\w+", "", suns_lac_lo_gm8_tweets$text)
suns_lac_lo_gm8_tweets$text <- gsub("rt", "", suns_lac_lo_gm8_tweets$text)
suns_lac_lo_gm8_tweets$text <- gsub("@\\w+", "", suns_lac_lo_gm8_tweets$text)
suns_lac_lo_gm8_tweets$text <- tolower(suns_lac_lo_gm8_tweets$text)

sent_suns_lac_lo_gm8_tweets <- sentiment(suns_lac_lo_gm8_tweets$text)
suns_lac_lo_gm8_tweets$sentiment <- sent_suns_lac_lo_gm8_tweets$sentiment

suns_lac_lo_gm8_tweets <- suns_lac_lo_gm8_tweets %>% mutate(game_id = "lo_gm_8")

rand_suns_lac_lo_gm8_tweets <- suns_lac_lo_gm8_tweets[sample(nrow(suns_lac_lo_gm8_tweets), size=1000), ]
# View(rand_suns_lac_lo_gm8_tweets)


## Suns vs ATL 5-5-21
suns_atl_lo_gm9_tweets <-
  get_all_tweets(
    query = "suns",
    start_tweets = "2021-05-05T07:00:00Z",
    end_tweets = "2021-05-06T06:59:00Z",
    data_path = "suns_atl_5-5/",
    bind_tweets = FALSE,
    n = 1000
  )
suns_atl_lo_gm9_tweets <- bind_tweets(data_path = "suns_atl_5-5/")

suns_atl_lo_gm9_tweets$text <- gsub("[^0-9A-Za-z///' ]","", suns_atl_lo_gm9_tweets$text)
suns_atl_lo_gm9_tweets$text <- gsub("http\\w+", "", suns_atl_lo_gm9_tweets$text)
suns_atl_lo_gm9_tweets$text <- gsub("rt", "", suns_atl_lo_gm9_tweets$text)
suns_atl_lo_gm9_tweets$text <- gsub("@\\w+", "", suns_atl_lo_gm9_tweets$text)
suns_atl_lo_gm9_tweets$text <- tolower(suns_atl_lo_gm9_tweets$text)

sent_suns_atl_lo_gm9_tweets <- sentiment(suns_atl_lo_gm9_tweets$text)
suns_atl_lo_gm9_tweets$sentiment <- sent_suns_atl_lo_gm9_tweets$sentiment

suns_atl_lo_gm9_tweets <- suns_atl_lo_gm9_tweets %>% mutate(game_id = "lo_gm_9")

rand_suns_atl_lo_gm9_tweets <- suns_atl_lo_gm9_tweets[sample(nrow(suns_atl_lo_gm9_tweets), size=1000), ]
# View(rand_suns_atl_lo_gm9_tweets)


## Suns vs NOP 2-3-21
suns_nop_lo_gm10_tweets <-
  get_all_tweets(
    query = "suns",
    start_tweets = "2021-02-03T07:00:00Z",
    end_tweets = "2021-02-04T06:59:00Z",
    data_path = "suns_nop_2-3/",
    bind_tweets = FALSE,
    n = 1000
  )
suns_nop_lo_gm10_tweets <- bind_tweets(data_path = "suns_nop_2-3/")

suns_nop_lo_gm10_tweets$text <- gsub("[^0-9A-Za-z///' ]","", suns_nop_lo_gm10_tweets$text)
suns_nop_lo_gm10_tweets$text <- gsub("http\\w+", "", suns_nop_lo_gm10_tweets$text)
suns_nop_lo_gm10_tweets$text <- gsub("rt", "", suns_nop_lo_gm10_tweets$text)
suns_nop_lo_gm10_tweets$text <- gsub("@\\w+", "", suns_nop_lo_gm10_tweets$text)
suns_nop_lo_gm10_tweets$text <- tolower(suns_nop_lo_gm10_tweets$text)

sent_suns_nop_lo_gm10_tweets <- sentiment(suns_nop_lo_gm10_tweets$text)
suns_nop_lo_gm10_tweets$sentiment <- sent_suns_nop_lo_gm10_tweets$sentiment

suns_nop_lo_gm10_tweets <- suns_nop_lo_gm10_tweets %>% mutate(game_id = "lo_gm_10")

rand_suns_nop_lo_gm10_tweets <- suns_nop_lo_gm10_tweets[sample(nrow(suns_nop_lo_gm10_tweets), size=1000), ]
# View(rand_suns_nop_lo_gm10_tweets)


## Suns vs CHA 3-28-21
suns_cha_lo_gm11_tweets <-
  get_all_tweets(
    query = "suns",
    start_tweets = "2021-03-28T07:00:00Z",
    end_tweets = "2021-03-29T06:59:00Z",
    data_path = "suns_cha_3-28/",
    bind_tweets = FALSE,
    n = 1000
  )
suns_cha_lo_gm11_tweets <- bind_tweets(data_path = "suns_cha_3-28/")

suns_cha_lo_gm11_tweets$text <- gsub("[^0-9A-Za-z///' ]","", suns_cha_lo_gm11_tweets$text)
suns_cha_lo_gm11_tweets$text <- gsub("http\\w+", "", suns_cha_lo_gm11_tweets$text)
suns_cha_lo_gm11_tweets$text <- gsub("rt", "", suns_cha_lo_gm11_tweets$text)
suns_cha_lo_gm11_tweets$text <- gsub("@\\w+", "", suns_cha_lo_gm11_tweets$text)
suns_cha_lo_gm11_tweets$text <- tolower(suns_cha_lo_gm11_tweets$text)

sent_suns_cha_lo_gm11_tweets <- sentiment(suns_cha_lo_gm11_tweets$text)
suns_cha_lo_gm11_tweets$sentiment <- sent_suns_cha_lo_gm11_tweets$sentiment

suns_cha_lo_gm11_tweets <- suns_cha_lo_gm11_tweets %>% mutate(game_id = "lo_gm_11")

rand_suns_cha_lo_gm11_tweets <- suns_cha_lo_gm11_tweets[sample(nrow(suns_cha_lo_gm11_tweets), size=1000), ]
# View(rand_suns_cha_lo_gm11_tweets)


## Suns vs BOS 2-7-21
suns_bos_lo_gm12_tweets <-
  get_all_tweets(
    query = "suns",
    start_tweets = "2021-02-07T07:00:00Z",
    end_tweets = "2021-02-08T06:59:00Z",
    data_path = "suns_bos_2-7/",
    bind_tweets = FALSE,
    n = 1000
  )
suns_bos_lo_gm12_tweets <- bind_tweets(data_path = "suns_bos_2-7/")

suns_bos_lo_gm12_tweets$text <- gsub("[^0-9A-Za-z///' ]","", suns_bos_lo_gm12_tweets$text)
suns_bos_lo_gm12_tweets$text <- gsub("http\\w+", "", suns_bos_lo_gm12_tweets$text)
suns_bos_lo_gm12_tweets$text <- gsub("rt", "", suns_bos_lo_gm12_tweets$text)
suns_bos_lo_gm12_tweets$text <- gsub("@\\w+", "", suns_bos_lo_gm12_tweets$text)
suns_bos_lo_gm12_tweets$text <- tolower(suns_bos_lo_gm12_tweets$text)

sent_suns_bos_lo_gm12_tweets <- sentiment(suns_bos_lo_gm12_tweets$text)
suns_bos_lo_gm12_tweets$sentiment <- sent_suns_bos_lo_gm12_tweets$sentiment

suns_bos_lo_gm12_tweets <- suns_bos_lo_gm12_tweets %>% mutate(game_id = "lo_gm_12")

rand_suns_bos_lo_gm12_tweets <- suns_bos_lo_gm12_tweets[sample(nrow(suns_bos_lo_gm12_tweets), size=1000), ]
# View(rand_suns_bos_lo_gm12_tweets)


## Suns vs OKC 1-27-21
suns_okc_lo_gm13_tweets <-
  get_all_tweets(
    query = "suns",
    start_tweets = "2021-01-27T07:00:00Z",
    end_tweets = "2021-01-28T06:59:00Z",
    data_path = "suns_okc_1-27/",
    bind_tweets = FALSE,
    n = 1000
  )
suns_okc_lo_gm13_tweets <- bind_tweets(data_path = "suns_okc_1-27/")

suns_okc_lo_gm13_tweets$text <- gsub("[^0-9A-Za-z///' ]","", suns_okc_lo_gm13_tweets$text)
suns_okc_lo_gm13_tweets$text <- gsub("http\\w+", "", suns_okc_lo_gm13_tweets$text)
suns_okc_lo_gm13_tweets$text <- gsub("rt", "", suns_okc_lo_gm13_tweets$text)
suns_okc_lo_gm13_tweets$text <- gsub("@\\w+", "", suns_okc_lo_gm13_tweets$text)
suns_okc_lo_gm13_tweets$text <- tolower(suns_okc_lo_gm13_tweets$text)

sent_suns_okc_lo_gm13_tweets <- sentiment(suns_okc_lo_gm13_tweets$text)
suns_okc_lo_gm13_tweets$sentiment <- sent_suns_okc_lo_gm13_tweets$sentiment

suns_okc_lo_gm13_tweets <- suns_okc_lo_gm13_tweets %>% mutate(game_id = "lo_gm_13")

rand_suns_okc_lo_gm13_tweets <- suns_okc_lo_gm13_tweets[sample(nrow(suns_okc_lo_gm13_tweets), size=1000), ]
# View(rand_suns_okc_lo_gm13_tweets)


## Suns vs BOS 4-22-21
suns_bos_lo_gm14_tweets <-
  get_all_tweets(
    query = "suns",
    start_tweets = "2021-04-22T07:00:00Z",
    end_tweets = "2021-04-23T06:59:00Z",
    data_path = "suns_bos_4-22/",
    bind_tweets = FALSE,
    n = 1000
  )
suns_bos_lo_gm14_tweets <- bind_tweets(data_path = "suns_bos_4-22/")

suns_bos_lo_gm14_tweets$text <- gsub("[^0-9A-Za-z///' ]","", suns_bos_lo_gm14_tweets$text)
suns_bos_lo_gm14_tweets$text <- gsub("http\\w+", "", suns_bos_lo_gm14_tweets$text)
suns_bos_lo_gm14_tweets$text <- gsub("rt", "", suns_bos_lo_gm14_tweets$text)
suns_bos_lo_gm14_tweets$text <- gsub("@\\w+", "", suns_bos_lo_gm14_tweets$text)
suns_bos_lo_gm14_tweets$text <- tolower(suns_bos_lo_gm14_tweets$text)

sent_suns_bos_lo_gm14_tweets <- sentiment(suns_bos_lo_gm14_tweets$text)
suns_bos_lo_gm14_tweets$sentiment <- sent_suns_bos_lo_gm14_tweets$sentiment

suns_bos_lo_gm14_tweets <- suns_bos_lo_gm14_tweets %>% mutate(game_id = "lo_gm_14")

rand_suns_bos_lo_gm14_tweets <- suns_bos_lo_gm14_tweets[sample(nrow(suns_bos_lo_gm14_tweets), size=1000), ]
# View(rand_suns_bos_lo_gm14_tweets)


## Suns vs SAS 4-17-21
suns_sas_lo_gm15_tweets <-
  get_all_tweets(
    query = "suns",
    start_tweets = "2021-04-17T07:00:00Z",
    end_tweets = "2021-04-18T06:59:00Z",
    data_path = "suns_sas_4-17/",
    bind_tweets = FALSE,
    n = 1000
  )
suns_sas_lo_gm15_tweets <- bind_tweets(data_path = "suns_sas_4-17/")

suns_sas_lo_gm15_tweets$text <- gsub("[^0-9A-Za-z///' ]","", suns_sas_lo_gm15_tweets$text)
suns_sas_lo_gm15_tweets$text <- gsub("http\\w+", "", suns_sas_lo_gm15_tweets$text)
suns_sas_lo_gm15_tweets$text <- gsub("rt", "", suns_sas_lo_gm15_tweets$text)
suns_sas_lo_gm15_tweets$text <- gsub("@\\w+", "", suns_sas_lo_gm15_tweets$text)
suns_sas_lo_gm15_tweets$text <- tolower(suns_sas_lo_gm15_tweets$text)

sent_suns_sas_lo_gm15_tweets <- sentiment(suns_sas_lo_gm15_tweets$text)
suns_sas_lo_gm15_tweets$sentiment <- sent_suns_sas_lo_gm15_tweets$sentiment

suns_sas_lo_gm15_tweets <- suns_sas_lo_gm15_tweets %>% mutate(game_id = "lo_gm_15")

rand_suns_sas_lo_gm15_tweets <- suns_sas_lo_gm15_tweets[sample(nrow(suns_sas_lo_gm15_tweets), size=1000), ]
# View(rand_suns_sas_lo_gm15_tweets)





### Small Versions of Random 1000-Tweet Samples for High-Scoring Games
### 'Small' meaning reduced number of columns
sm_rand_suns_okc_hi_gm1_tweets <- rand_suns_okc_hi_gm1_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id)
# View(sm_rand_suns_okc_hi_gm1_tweets)

sm_rand_suns_sas_hi_gm2_tweets <- rand_suns_sas_hi_gm2_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id)
# View(sm_rand_suns_sas_hi_gm2_tweets)

sm_rand_suns_was_hi_gm3_tweets <- rand_suns_was_hi_gm3_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id)
# View(sm_rand_suns_was_hi_gm3_tweets)

sm_rand_suns_cle_hi_gm4_tweets <- rand_suns_cle_hi_gm4_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id)
# View(sm_rand_suns_cle_hi_gm4_tweets)

sm_rand_suns_hou_hi_gm5_tweets <- rand_suns_hou_hi_gm5_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id)
# View(sm_rand_suns_hou_hi_gm5_tweets)

sm_rand_suns_nop_hi_gm6_tweets <- rand_suns_nop_hi_gm6_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id)
# View(sm_rand_suns_nop_hi_gm6_tweets)

sm_rand_suns_por_hi_gm7_tweets <- rand_suns_por_hi_gm7_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id)
# View(sm_rand_suns_por_hi_gm7_tweets)

sm_rand_suns_mem_hi_gm8_tweets <- rand_suns_mem_hi_gm8_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id)
# View(sm_rand_suns_mem_hi_gm8_tweets)

sm_rand_suns_mil_hi_gm9_tweets <- rand_suns_mil_hi_gm9_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id)
# View(sm_rand_suns_mil_hi_gm9_tweets)

sm_rand_suns_nyk_hi_gm10_tweets <- rand_suns_nyk_hi_gm10_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id)
# View(sm_rand_suns_nyk_hi_gm10_tweets)

sm_rand_suns_por_hi_gm11_tweets <- rand_suns_por_hi_gm11_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id)
# View(sm_rand_suns_por_hi_gm11_tweets)

sm_rand_suns_den_hi_gm12_tweets <- rand_suns_den_hi_gm12_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id)
# View(sm_rand_suns_den_hi_gm12_tweets)

sm_rand_suns_hou_hi_gm13_tweets <- rand_suns_hou_hi_gm13_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id)
# View(sm_rand_suns_hou_hi_gm13_tweets)

sm_rand_suns_ind_hi_gm14_tweets <- rand_suns_ind_hi_gm14_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id)
# View(sm_rand_suns_ind_hi_gm14_tweets)

sm_rand_suns_mil_hi_gm15_tweets <- rand_suns_mil_hi_gm15_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id)
# View(sm_rand_suns_mil_hi_gm15_tweets)





### Small Versions of Random 1000-Tweet Samples for Low-Scoring Games
### 'Small' meaning reduced number of columns
sm_rand_suns_den_lo_gm1_tweets <- rand_suns_den_lo_gm1_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id)
# View(sm_rand_suns_den_lo_gm1_tweets)

sm_rand_suns_chi_lo_gm2_tweets <- rand_suns_chi_lo_gm2_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id)
# View(sm_rand_suns_chi_lo_gm2_tweets)

sm_rand_suns_mia_lo_gm3_tweets <- rand_suns_mia_lo_gm3_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id)
# View(sm_rand_suns_mia_lo_gm3_tweets)

sm_rand_suns_det_lo_gm4_tweets <- rand_suns_det_lo_gm4_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id)
# View(sm_rand_suns_det_lo_gm4_tweets)

sm_rand_suns_mem_lo_gm5_tweets <- rand_suns_mem_lo_gm5_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id)
# View(sm_rand_suns_mem_lo_gm5_tweets)

sm_rand_suns_tor_lo_gm6_tweets <- rand_suns_tor_lo_gm6_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id)
# View(sm_rand_suns_tor_lo_gm6_tweets)

sm_rand_suns_sac_lo_gm7_tweets <- rand_suns_sac_lo_gm7_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id)
# View(sm_rand_suns_sac_lo_gm7_tweets)

sm_rand_suns_lac_lo_gm8_tweets <- rand_suns_lac_lo_gm8_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id)
# View(sm_rand_suns_lac_lo_gm8_tweets)

sm_rand_suns_atl_lo_gm9_tweets <- rand_suns_atl_lo_gm9_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id)
# View(sm_rand_suns_atl_lo_gm9_tweets)

sm_rand_suns_nop_lo_gm10_tweets <- rand_suns_nop_lo_gm10_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id)
# View(sm_rand_suns_nop_lo_gm10_tweets)

sm_rand_suns_cha_lo_gm11_tweets <- rand_suns_cha_lo_gm11_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id)
# View(sm_rand_suns_cha_lo_gm11_tweets)

sm_rand_suns_bos_lo_gm12_tweets <- rand_suns_bos_lo_gm12_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id)
# View(sm_rand_suns_bos_lo_gm12_tweets)

sm_rand_suns_okc_lo_gm13_tweets <- rand_suns_okc_lo_gm13_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id)
# View(sm_rand_suns_okc_lo_gm13_tweets)

sm_rand_suns_bos_lo_gm14_tweets <- rand_suns_bos_lo_gm14_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id)
# View(sm_rand_suns_bos_lo_gm14_tweets)

sm_rand_suns_sas_lo_gm15_tweets <- rand_suns_sas_lo_gm15_tweets %>% select(created_at, id, author_id, possibly_sensitive, conversation_id, text, lang, source, in_reply_to_user_id, sentiment, game_id)
# View(sm_rand_suns_sas_lo_gm15_tweets)





### Merging of High-Scoring Games
merge_hi_gms <- bind_rows(sm_rand_suns_okc_hi_gm1_tweets, sm_rand_suns_sas_hi_gm2_tweets, sm_rand_suns_was_hi_gm3_tweets, sm_rand_suns_cle_hi_gm4_tweets, sm_rand_suns_hou_hi_gm5_tweets, 
                          sm_rand_suns_nop_hi_gm6_tweets, sm_rand_suns_por_hi_gm7_tweets, sm_rand_suns_mem_hi_gm8_tweets, sm_rand_suns_mil_hi_gm9_tweets, sm_rand_suns_nyk_hi_gm10_tweets, 
                          sm_rand_suns_por_hi_gm11_tweets, sm_rand_suns_den_hi_gm12_tweets, sm_rand_suns_hou_hi_gm13_tweets, sm_rand_suns_ind_hi_gm14_tweets, sm_rand_suns_mil_hi_gm15_tweets)





### Merging of Low-Scoring Games
merge_lo_gms <- bind_rows(sm_rand_suns_den_lo_gm1_tweets, sm_rand_suns_chi_lo_gm2_tweets, sm_rand_suns_mia_lo_gm3_tweets, sm_rand_suns_det_lo_gm4_tweets, sm_rand_suns_mem_lo_gm5_tweets, 
                          sm_rand_suns_tor_lo_gm6_tweets, sm_rand_suns_sac_lo_gm7_tweets, sm_rand_suns_lac_lo_gm8_tweets, sm_rand_suns_atl_lo_gm9_tweets, sm_rand_suns_nop_lo_gm10_tweets, 
                          sm_rand_suns_cha_lo_gm11_tweets, sm_rand_suns_bos_lo_gm12_tweets, sm_rand_suns_okc_lo_gm13_tweets, sm_rand_suns_bos_lo_gm14_tweets, sm_rand_suns_sas_lo_gm15_tweets)





### Running the T-Test
t.test(merge_hi_gms$sentiment, merge_lo_gms$sentiment, mu=0, alt="two.sided", conf=0.95, var.eq=F, paired=F)





### Calculating standard deviations
sd(merge_hi_gms$sentiment)

sd(merge_lo_gms$sentiment)





### Box plot

plot_ly(
  data = merge_hi_and_lo_gms,
  y = ~sentiment,
  x = ~scoring_type,
  type = "box",
  color = ~scoring_type,
  showlegend = FALSE
)