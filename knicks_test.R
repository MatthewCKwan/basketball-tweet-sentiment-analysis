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





### Correlation Test - Twitter Sentiment and Attendance During 
### the NYK 2018-19 Season

## New York Knicks' First 30 Home Games of 2018-19 Season and Game Attendance 
## (Out of 19763)
# W vs ATL 10/17/18 - 18249 - #1
# L vs BOS 10/20/18 - 19427 - #2
# L vs GSW 10/26/18 - 19812 - #3
# W vs BKN 10/29/18 - 19221 - #4
# L vs IND 10/31/18 - 18295 - #5
# L vs CHI 11/05/18 - 19812 - #6
# L vs ORL 11/11/18 - 19812 - #7
# L vs POR 11/20/18 - 19812 - #8
# W vs NOP 11/23/18 - 18948 - #9
# W vs MIL 12/01/18 - 19812 - #10
# L vs WAS 12/03/18 - 19440 - #11
# L vs BKN 12/08/18 - 18662 - #12
# L vs CHA 12/09/18 - 18602 - #13
# L vs PHX 12/17/18 - 18437 - #14
# L vs ATL 12/21/18 - 19080 - #15
# L vs MIL 12/25/18 - 19812 - #16
# L vs IND 01/11/19 - 19812 - #17
# L vs PHI 01/13/19 - 18596 - #18
# L vs OKC 01/21/19 - 19493 - #19
# L vs HOU 01/23/19 - 18819 - #20
# L vs MIA 01/27/19 - 18852 - #21
# L vs DAL 01/30/19 - 18842 - #22
# L vs BOS 02/01/19 - 18343 - #23
# L vs MEM 02/03/19 - 17025 - #24
# L vs DET 02/05/19 - 17853 - #25
# L vs TOR 02/09/19 - 18886 - #26
# L vs PHI 02/13/19 - 18983 - #27
# L vs MIN 02/22/19 - 19096 - #28
# W vs SAS 02/24/19 - 18019 - #29
# W vs ORL 02/26/19 - 17833 - #30





### Data Collection

## W vs ATL 10/17/18 - 18249 - #1
gm1_tweets <-
  get_all_tweets(
    query = "knicks",
    start_tweets = "2018-10-17T07:00:00Z",
    end_tweets = "2018-10-18T06:59:00Z",
    data_path = "cor_gm1/",
    bind_tweets = FALSE,
    n = 1000
  )
gm1_tweets <- bind_tweets(data_path = "cor_gm1/")

gm1_tweets$text <- gsub("[^0-9A-Za-z///' ]","", gm1_tweets$text)
gm1_tweets$text <- gsub("http\\w+", "", gm1_tweets$text)
gm1_tweets$text <- gsub("rt", "", gm1_tweets$text)
gm1_tweets$text <- gsub("@\\w+", "", gm1_tweets$text)
gm1_tweets$text <- tolower(gm1_tweets$text)

sent_gm1_tweets <- sentiment(gm1_tweets$text)
gm1_tweets$sentiment <- sent_gm1_tweets$sentiment

gm1_tweets <- gm1_tweets %>% mutate(game_id = "gm1_atl")

rand_gm1_tweets <- gm1_tweets[sample(nrow(gm1_tweets), size=1000), ]

gm1_mean_sent <- mean(rand_gm1_tweets$sentiment)


## L vs BOS 10/20/18 - 19427 - #2
gm2_tweets <-
  get_all_tweets(
    query = "knicks",
    start_tweets = "2018-10-20T07:00:00Z",
    end_tweets = "2018-10-21T06:59:00Z",
    data_path = "cor_gm2/",
    bind_tweets = FALSE,
    n = 1000
  )
gm2_tweets <- bind_tweets(data_path = "cor_gm2/")

gm2_tweets$text <- gsub("[^0-9A-Za-z///' ]","", gm2_tweets$text)
gm2_tweets$text <- gsub("http\\w+", "", gm2_tweets$text)
gm2_tweets$text <- gsub("rt", "", gm2_tweets$text)
gm2_tweets$text <- gsub("@\\w+", "", gm2_tweets$text)
gm2_tweets$text <- tolower(gm2_tweets$text)

sent_gm2_tweets <- sentiment(gm2_tweets$text)
gm2_tweets$sentiment <- sent_gm2_tweets$sentiment

gm2_tweets <- gm2_tweets %>% mutate(game_id = "gm2_bos")

rand_gm2_tweets <- gm2_tweets[sample(nrow(gm2_tweets), size=1000), ]

gm2_mean_sent <- mean(rand_gm2_tweets$sentiment)


## L vs GSW 10/26/18 - 19812 - #3
gm3_tweets <-
  get_all_tweets(
    query = "knicks",
    start_tweets = "2018-10-26T07:00:00Z",
    end_tweets = "2018-10-27T06:59:00Z",
    data_path = "cor_gm3/",
    bind_tweets = FALSE,
    n = 1000
  )
gm3_tweets <- bind_tweets(data_path = "cor_gm3/")

gm3_tweets$text <- gsub("[^0-9A-Za-z///' ]","", gm3_tweets$text)
gm3_tweets$text <- gsub("http\\w+", "", gm3_tweets$text)
gm3_tweets$text <- gsub("rt", "", gm3_tweets$text)
gm3_tweets$text <- gsub("@\\w+", "", gm3_tweets$text)
gm3_tweets$text <- tolower(gm3_tweets$text)

sent_gm3_tweets <- sentiment(gm3_tweets$text)
gm3_tweets$sentiment <- sent_gm3_tweets$sentiment

gm3_tweets <- gm3_tweets %>% mutate(game_id = "gm3_gsw")

rand_gm3_tweets <- gm3_tweets[sample(nrow(gm3_tweets), size=1000), ]

gm3_mean_sent <- mean(rand_gm3_tweets$sentiment)


## W vs BKN 10/29/18 - 19221 - #4
gm4_tweets <-
  get_all_tweets(
    query = "knicks",
    start_tweets = "2018-10-29T07:00:00Z",
    end_tweets = "2018-10-30T06:59:00Z",
    data_path = "cor_gm4/",
    bind_tweets = FALSE,
    n = 1000
  )
gm4_tweets <- bind_tweets(data_path = "cor_gm4/")

gm4_tweets$text <- gsub("[^0-9A-Za-z///' ]","", gm4_tweets$text)
gm4_tweets$text <- gsub("http\\w+", "", gm4_tweets$text)
gm4_tweets$text <- gsub("rt", "", gm4_tweets$text)
gm4_tweets$text <- gsub("@\\w+", "", gm4_tweets$text)
gm4_tweets$text <- tolower(gm4_tweets$text)

sent_gm4_tweets <- sentiment(gm4_tweets$text)
gm4_tweets$sentiment <- sent_gm4_tweets$sentiment

gm4_tweets <- gm4_tweets %>% mutate(game_id = "gm4_bkn")

rand_gm4_tweets <- gm4_tweets[sample(nrow(gm4_tweets), size=1000), ]

gm4_mean_sent <- mean(rand_gm4_tweets$sentiment)


## L vs IND 10/31/18 - 18295 - #5
gm5_tweets <-
  get_all_tweets(
    query = "knicks",
    start_tweets = "2018-10-31T07:00:00Z",
    end_tweets = "2018-11-01T06:59:00Z",
    data_path = "cor_gm5/",
    bind_tweets = FALSE,
    n = 1000
  )
gm5_tweets <- bind_tweets(data_path = "cor_gm5/")

gm5_tweets$text <- gsub("[^0-9A-Za-z///' ]","", gm5_tweets$text)
gm5_tweets$text <- gsub("http\\w+", "", gm5_tweets$text)
gm5_tweets$text <- gsub("rt", "", gm5_tweets$text)
gm5_tweets$text <- gsub("@\\w+", "", gm5_tweets$text)
gm5_tweets$text <- tolower(gm5_tweets$text)

sent_gm5_tweets <- sentiment(gm5_tweets$text)
gm5_tweets$sentiment <- sent_gm5_tweets$sentiment

gm5_tweets <- gm5_tweets %>% mutate(game_id = "gm5_ind")

rand_gm5_tweets <- gm5_tweets[sample(nrow(gm5_tweets), size=1000), ]

gm5_mean_sent <- mean(rand_gm5_tweets$sentiment)


## L vs CHI 11/05/18 - 19812 - #6
gm6_tweets <-
  get_all_tweets(
    query = "knicks",
    start_tweets = "2018-11-05T07:00:00Z",
    end_tweets = "2018-11-06T06:59:00Z",
    data_path = "cor_gm6/",
    bind_tweets = FALSE,
    n = 1000
  )
gm6_tweets <- bind_tweets(data_path = "cor_gm6/")

gm6_tweets$text <- gsub("[^0-9A-Za-z///' ]","", gm6_tweets$text)
gm6_tweets$text <- gsub("http\\w+", "", gm6_tweets$text)
gm6_tweets$text <- gsub("rt", "", gm6_tweets$text)
gm6_tweets$text <- gsub("@\\w+", "", gm6_tweets$text)
gm6_tweets$text <- tolower(gm6_tweets$text)

sent_gm6_tweets <- sentiment(gm6_tweets$text)
gm6_tweets$sentiment <- sent_gm6_tweets$sentiment

gm6_tweets <- gm6_tweets %>% mutate(game_id = "gm6_chi")

rand_gm6_tweets <- gm6_tweets[sample(nrow(gm6_tweets), size=1000), ]

gm6_mean_sent <- mean(rand_gm6_tweets$sentiment)


## L vs ORL 11/11/18 - 19812 - #7
gm7_tweets <-
  get_all_tweets(
    query = "knicks",
    start_tweets = "2018-11-11T07:00:00Z",
    end_tweets = "2018-11-12T06:59:00Z",
    data_path = "cor_gm7/",
    bind_tweets = FALSE,
    n = 1000
  )
gm7_tweets <- bind_tweets(data_path = "cor_gm7/")

gm7_tweets$text <- gsub("[^0-9A-Za-z///' ]","", gm7_tweets$text)
gm7_tweets$text <- gsub("http\\w+", "", gm7_tweets$text)
gm7_tweets$text <- gsub("rt", "", gm7_tweets$text)
gm7_tweets$text <- gsub("@\\w+", "", gm7_tweets$text)
gm7_tweets$text <- tolower(gm7_tweets$text)

sent_gm7_tweets <- sentiment(gm7_tweets$text)
gm7_tweets$sentiment <- sent_gm7_tweets$sentiment

gm7_tweets <- gm7_tweets %>% mutate(game_id = "gm7_orl")

rand_gm7_tweets <- gm7_tweets[sample(nrow(gm7_tweets), size=1000), ]

gm7_mean_sent <- mean(rand_gm7_tweets$sentiment)


## L vs POR 11/20/18 - 19812 - #8
gm8_tweets <-
  get_all_tweets(
    query = "knicks",
    start_tweets = "2018-11-20T07:00:00Z",
    end_tweets = "2018-11-21T06:59:00Z",
    data_path = "cor_gm8/",
    bind_tweets = FALSE,
    n = 1000
  )
gm8_tweets <- bind_tweets(data_path = "cor_gm8/")

gm8_tweets$text <- gsub("[^0-9A-Za-z///' ]","", gm8_tweets$text)
gm8_tweets$text <- gsub("http\\w+", "", gm8_tweets$text)
gm8_tweets$text <- gsub("rt", "", gm8_tweets$text)
gm8_tweets$text <- gsub("@\\w+", "", gm8_tweets$text)
gm8_tweets$text <- tolower(gm8_tweets$text)

sent_gm8_tweets <- sentiment(gm8_tweets$text)
gm8_tweets$sentiment <- sent_gm8_tweets$sentiment

gm8_tweets <- gm8_tweets %>% mutate(game_id = "gm8_por")

rand_gm8_tweets <- gm8_tweets[sample(nrow(gm8_tweets), size=1000), ]

gm8_mean_sent <- mean(rand_gm8_tweets$sentiment)


## W vs NOP 11/23/18 - 18948 - #9
gm9_tweets <-
  get_all_tweets(
    query = "knicks",
    start_tweets = "2018-11-23T07:00:00Z",
    end_tweets = "2018-11-24T06:59:00Z",
    data_path = "cor_gm9/",
    bind_tweets = FALSE,
    n = 1000
  )
gm9_tweets <- bind_tweets(data_path = "cor_gm9/")

gm9_tweets$text <- gsub("[^0-9A-Za-z///' ]","", gm9_tweets$text)
gm9_tweets$text <- gsub("http\\w+", "", gm9_tweets$text)
gm9_tweets$text <- gsub("rt", "", gm9_tweets$text)
gm9_tweets$text <- gsub("@\\w+", "", gm9_tweets$text)
gm9_tweets$text <- tolower(gm9_tweets$text)

sent_gm9_tweets <- sentiment(gm9_tweets$text)
gm9_tweets$sentiment <- sent_gm9_tweets$sentiment

gm9_tweets <- gm9_tweets %>% mutate(game_id = "gm9_nop")

rand_gm9_tweets <- gm9_tweets[sample(nrow(gm9_tweets), size=1000), ]

gm9_mean_sent <- mean(rand_gm9_tweets$sentiment)


## W vs MIL 12/01/18 - 19812 - #10
gm10_tweets <-
  get_all_tweets(
    query = "knicks",
    start_tweets = "2018-12-01T07:00:00Z",
    end_tweets = "2018-12-02T06:59:00Z",
    data_path = "cor_gm10/",
    bind_tweets = FALSE,
    n = 1000
  )
gm10_tweets <- bind_tweets(data_path = "cor_gm10/")

gm10_tweets$text <- gsub("[^0-9A-Za-z///' ]","", gm10_tweets$text)
gm10_tweets$text <- gsub("http\\w+", "", gm10_tweets$text)
gm10_tweets$text <- gsub("rt", "", gm10_tweets$text)
gm10_tweets$text <- gsub("@\\w+", "", gm10_tweets$text)
gm10_tweets$text <- tolower(gm10_tweets$text)

sent_gm10_tweets <- sentiment(gm10_tweets$text)
gm10_tweets$sentiment <- sent_gm10_tweets$sentiment

gm10_tweets <- gm10_tweets %>% mutate(game_id = "gm10_mil")

rand_gm10_tweets <- gm10_tweets[sample(nrow(gm10_tweets), size=1000), ]

gm10_mean_sent <- mean(rand_gm10_tweets$sentiment)


## L vs WAS 12/03/18 - 19440 - #11
gm11_tweets <-
  get_all_tweets(
    query = "knicks",
    start_tweets = "2018-12-03T07:00:00Z",
    end_tweets = "2018-12-04T06:59:00Z",
    data_path = "cor_gm11/",
    bind_tweets = FALSE,
    n = 1000
  )
gm11_tweets <- bind_tweets(data_path = "cor_gm11/")

gm11_tweets$text <- gsub("[^0-9A-Za-z///' ]","", gm11_tweets$text)
gm11_tweets$text <- gsub("http\\w+", "", gm11_tweets$text)
gm11_tweets$text <- gsub("rt", "", gm11_tweets$text)
gm11_tweets$text <- gsub("@\\w+", "", gm11_tweets$text)
gm11_tweets$text <- tolower(gm11_tweets$text)

sent_gm11_tweets <- sentiment(gm11_tweets$text)
gm11_tweets$sentiment <- sent_gm11_tweets$sentiment

gm11_tweets <- gm11_tweets %>% mutate(game_id = "gm11_was")

rand_gm11_tweets <- gm11_tweets[sample(nrow(gm11_tweets), size=1000), ]

gm11_mean_sent <- mean(rand_gm11_tweets$sentiment)


## L vs BKN 12/08/18 - 18662 - #12
gm12_tweets <-
  get_all_tweets(
    query = "knicks",
    start_tweets = "2018-12-08T07:00:00Z",
    end_tweets = "2018-12-09T06:59:00Z",
    data_path = "cor_gm12/",
    bind_tweets = FALSE,
    n = 1000
  )
gm12_tweets <- bind_tweets(data_path = "cor_gm12/")

gm12_tweets$text <- gsub("[^0-9A-Za-z///' ]","", gm12_tweets$text)
gm12_tweets$text <- gsub("http\\w+", "", gm12_tweets$text)
gm12_tweets$text <- gsub("rt", "", gm12_tweets$text)
gm12_tweets$text <- gsub("@\\w+", "", gm12_tweets$text)
gm12_tweets$text <- tolower(gm12_tweets$text)

sent_gm12_tweets <- sentiment(gm12_tweets$text)
gm12_tweets$sentiment <- sent_gm12_tweets$sentiment

gm12_tweets <- gm12_tweets %>% mutate(game_id = "gm12_bkn")

rand_gm12_tweets <- gm12_tweets[sample(nrow(gm12_tweets), size=1000), ]

gm12_mean_sent <- mean(rand_gm12_tweets$sentiment)


## L vs CHA 12/09/18 - 18602 - #13
gm13_tweets <-
  get_all_tweets(
    query = "knicks",
    start_tweets = "2018-12-09T07:00:00Z",
    end_tweets = "2018-12-10T06:59:00Z",
    data_path = "cor_gm13/",
    bind_tweets = FALSE,
    n = 1000
  )
gm13_tweets <- bind_tweets(data_path = "cor_gm13/")

gm13_tweets$text <- gsub("[^0-9A-Za-z///' ]","", gm13_tweets$text)
gm13_tweets$text <- gsub("http\\w+", "", gm13_tweets$text)
gm13_tweets$text <- gsub("rt", "", gm13_tweets$text)
gm13_tweets$text <- gsub("@\\w+", "", gm13_tweets$text)
gm13_tweets$text <- tolower(gm13_tweets$text)

sent_gm13_tweets <- sentiment(gm13_tweets$text)
gm13_tweets$sentiment <- sent_gm13_tweets$sentiment

gm13_tweets <- gm13_tweets %>% mutate(game_id = "gm13_cha")

rand_gm13_tweets <- gm13_tweets[sample(nrow(gm13_tweets), size=1000), ]

gm13_mean_sent <- mean(rand_gm13_tweets$sentiment)


## L vs PHX 12/17/18 - 18437 - #14
gm14_tweets <-
  get_all_tweets(
    query = "knicks",
    start_tweets = "2018-12-17T07:00:00Z",
    end_tweets = "2018-12-18T06:59:00Z",
    data_path = "cor_gm14/",
    bind_tweets = FALSE,
    n = 1000
  )
gm14_tweets <- bind_tweets(data_path = "cor_gm14/")

gm14_tweets$text <- gsub("[^0-9A-Za-z///' ]","", gm14_tweets$text)
gm14_tweets$text <- gsub("http\\w+", "", gm14_tweets$text)
gm14_tweets$text <- gsub("rt", "", gm14_tweets$text)
gm14_tweets$text <- gsub("@\\w+", "", gm14_tweets$text)
gm14_tweets$text <- tolower(gm14_tweets$text)

sent_gm14_tweets <- sentiment(gm14_tweets$text)
gm14_tweets$sentiment <- sent_gm14_tweets$sentiment

gm14_tweets <- gm14_tweets %>% mutate(game_id = "gm14_phx")

rand_gm14_tweets <- gm14_tweets[sample(nrow(gm14_tweets), size=1000), ]

gm14_mean_sent <- mean(rand_gm14_tweets$sentiment)


## L vs ATL 12/21/18 - 19080 - #15
gm15_tweets <-
  get_all_tweets(
    query = "knicks",
    start_tweets = "2018-12-21T07:00:00Z",
    end_tweets = "2018-12-22T06:59:00Z",
    data_path = "cor_gm15/",
    bind_tweets = FALSE,
    n = 1000
  )
gm15_tweets <- bind_tweets(data_path = "cor_gm15/")

gm15_tweets$text <- gsub("[^0-9A-Za-z///' ]","", gm15_tweets$text)
gm15_tweets$text <- gsub("http\\w+", "", gm15_tweets$text)
gm15_tweets$text <- gsub("rt", "", gm15_tweets$text)
gm15_tweets$text <- gsub("@\\w+", "", gm15_tweets$text)
gm15_tweets$text <- tolower(gm15_tweets$text)

sent_gm15_tweets <- sentiment(gm15_tweets$text)
gm15_tweets$sentiment <- sent_gm15_tweets$sentiment

gm15_tweets <- gm15_tweets %>% mutate(game_id = "gm15_atl")

rand_gm15_tweets <- gm15_tweets[sample(nrow(gm15_tweets), size=1000), ]

gm15_mean_sent <- mean(rand_gm15_tweets$sentiment)


## L vs MIL 12/25/18 - 19812 - #16
gm16_tweets <-
  get_all_tweets(
    query = "knicks",
    start_tweets = "2018-12-25T07:00:00Z",
    end_tweets = "2018-12-26T06:59:00Z",
    data_path = "cor_gm16/",
    bind_tweets = FALSE,
    n = 1000
  )
gm16_tweets <- bind_tweets(data_path = "cor_gm16/")

gm16_tweets$text <- gsub("[^0-9A-Za-z///' ]","", gm16_tweets$text)
gm16_tweets$text <- gsub("http\\w+", "", gm16_tweets$text)
gm16_tweets$text <- gsub("rt", "", gm16_tweets$text)
gm16_tweets$text <- gsub("@\\w+", "", gm16_tweets$text)
gm16_tweets$text <- tolower(gm16_tweets$text)

sent_gm16_tweets <- sentiment(gm16_tweets$text)
gm16_tweets$sentiment <- sent_gm16_tweets$sentiment

gm16_tweets <- gm16_tweets %>% mutate(game_id = "gm16_mil")

rand_gm16_tweets <- gm16_tweets[sample(nrow(gm16_tweets), size=1000), ]

gm16_mean_sent <- mean(rand_gm16_tweets$sentiment)


## L vs IND 01/11/19 - 19812 - #17
gm17_tweets <-
  get_all_tweets(
    query = "knicks",
    start_tweets = "2019-01-11T07:00:00Z",
    end_tweets = "2019-01-12T06:59:00Z",
    data_path = "cor_gm17/",
    bind_tweets = FALSE,
    n = 1000
  )
gm17_tweets <- bind_tweets(data_path = "cor_gm17/")

gm17_tweets$text <- gsub("[^0-9A-Za-z///' ]","", gm17_tweets$text)
gm17_tweets$text <- gsub("http\\w+", "", gm17_tweets$text)
gm17_tweets$text <- gsub("rt", "", gm17_tweets$text)
gm17_tweets$text <- gsub("@\\w+", "", gm17_tweets$text)
gm17_tweets$text <- tolower(gm17_tweets$text)

sent_gm17_tweets <- sentiment(gm17_tweets$text)
gm17_tweets$sentiment <- sent_gm17_tweets$sentiment

gm17_tweets <- gm17_tweets %>% mutate(game_id = "gm17_ind")

rand_gm17_tweets <- gm17_tweets[sample(nrow(gm17_tweets), size=1000), ]

gm17_mean_sent <- mean(rand_gm17_tweets$sentiment)


## L vs PHI 01/13/19 - 18596 - #18
gm18_tweets <-
  get_all_tweets(
    query = "knicks",
    start_tweets = "2019-01-13T07:00:00Z",
    end_tweets = "2019-01-14T06:59:00Z",
    data_path = "cor_gm18/",
    bind_tweets = FALSE,
    n = 1000
  )
gm18_tweets <- bind_tweets(data_path = "cor_gm18/")

gm18_tweets$text <- gsub("[^0-9A-Za-z///' ]","", gm18_tweets$text)
gm18_tweets$text <- gsub("http\\w+", "", gm18_tweets$text)
gm18_tweets$text <- gsub("rt", "", gm18_tweets$text)
gm18_tweets$text <- gsub("@\\w+", "", gm18_tweets$text)
gm18_tweets$text <- tolower(gm18_tweets$text)

sent_gm18_tweets <- sentiment(gm18_tweets$text)
gm18_tweets$sentiment <- sent_gm18_tweets$sentiment

gm18_tweets <- gm18_tweets %>% mutate(game_id = "gm18_phi")

rand_gm18_tweets <- gm18_tweets[sample(nrow(gm18_tweets), size=1000), ]

gm18_mean_sent <- mean(rand_gm18_tweets$sentiment)


## L vs OKC 01/21/19 - 19493 - #19
gm19_tweets <-
  get_all_tweets(
    query = "knicks",
    start_tweets = "2019-01-21T07:00:00Z",
    end_tweets = "2019-01-22T06:59:00Z",
    data_path = "cor_gm19/",
    bind_tweets = FALSE,
    n = 1000
  )
gm19_tweets <- bind_tweets(data_path = "cor_gm19/")

gm19_tweets$text <- gsub("[^0-9A-Za-z///' ]","", gm19_tweets$text)
gm19_tweets$text <- gsub("http\\w+", "", gm19_tweets$text)
gm19_tweets$text <- gsub("rt", "", gm19_tweets$text)
gm19_tweets$text <- gsub("@\\w+", "", gm19_tweets$text)
gm19_tweets$text <- tolower(gm19_tweets$text)

sent_gm19_tweets <- sentiment(gm19_tweets$text)
gm19_tweets$sentiment <- sent_gm19_tweets$sentiment

gm19_tweets <- gm19_tweets %>% mutate(game_id = "gm19_okc")

rand_gm19_tweets <- gm19_tweets[sample(nrow(gm19_tweets), size=1000), ]

gm19_mean_sent <- mean(rand_gm19_tweets$sentiment)


## L vs HOU 01/23/19 - 18819 - #20
gm20_tweets <-
  get_all_tweets(
    query = "knicks",
    start_tweets = "2019-01-23T07:00:00Z",
    end_tweets = "2019-01-24T06:59:00Z",
    data_path = "cor_gm20/",
    bind_tweets = FALSE,
    n = 1000
  )
gm20_tweets <- bind_tweets(data_path = "cor_gm20/")

gm20_tweets$text <- gsub("[^0-9A-Za-z///' ]","", gm20_tweets$text)
gm20_tweets$text <- gsub("http\\w+", "", gm20_tweets$text)
gm20_tweets$text <- gsub("rt", "", gm20_tweets$text)
gm20_tweets$text <- gsub("@\\w+", "", gm20_tweets$text)
gm20_tweets$text <- tolower(gm20_tweets$text)

sent_gm20_tweets <- sentiment(gm20_tweets$text)
gm20_tweets$sentiment <- sent_gm20_tweets$sentiment

gm20_tweets <- gm20_tweets %>% mutate(game_id = "gm20_hou")

rand_gm20_tweets <- gm20_tweets[sample(nrow(gm20_tweets), size=1000), ]

gm20_mean_sent <- mean(rand_gm20_tweets$sentiment)


## L vs MIA 01/27/19 - 18852 - #21
gm21_tweets <-
  get_all_tweets(
    query = "knicks",
    start_tweets = "2019-01-27T07:00:00Z",
    end_tweets = "2019-01-28T06:59:00Z",
    data_path = "cor_gm21/",
    bind_tweets = FALSE,
    n = 1000
  )
gm21_tweets <- bind_tweets(data_path = "cor_gm21/")

gm21_tweets$text <- gsub("[^0-9A-Za-z///' ]","", gm21_tweets$text)
gm21_tweets$text <- gsub("http\\w+", "", gm21_tweets$text)
gm21_tweets$text <- gsub("rt", "", gm21_tweets$text)
gm21_tweets$text <- gsub("@\\w+", "", gm21_tweets$text)
gm21_tweets$text <- tolower(gm21_tweets$text)

sent_gm21_tweets <- sentiment(gm21_tweets$text)
gm21_tweets$sentiment <- sent_gm21_tweets$sentiment

gm21_tweets <- gm21_tweets %>% mutate(game_id = "gm21_mia")

rand_gm21_tweets <- gm21_tweets[sample(nrow(gm21_tweets), size=1000), ]

gm21_mean_sent <- mean(rand_gm21_tweets$sentiment)


## L vs DAL 01/30/19 - 18842 - #22
gm22_tweets <-
  get_all_tweets(
    query = "knicks",
    start_tweets = "2019-01-30T07:00:00Z",
    end_tweets = "2019-01-31T06:59:00Z",
    data_path = "cor_gm22/",
    bind_tweets = FALSE,
    n = 1000
  )
gm22_tweets <- bind_tweets(data_path = "cor_gm22/")

gm22_tweets$text <- gsub("[^0-9A-Za-z///' ]","", gm22_tweets$text)
gm22_tweets$text <- gsub("http\\w+", "", gm22_tweets$text)
gm22_tweets$text <- gsub("rt", "", gm22_tweets$text)
gm22_tweets$text <- gsub("@\\w+", "", gm22_tweets$text)
gm22_tweets$text <- tolower(gm22_tweets$text)

sent_gm22_tweets <- sentiment(gm22_tweets$text)
gm22_tweets$sentiment <- sent_gm22_tweets$sentiment

gm22_tweets <- gm22_tweets %>% mutate(game_id = "gm22_dal")

rand_gm22_tweets <- gm22_tweets[sample(nrow(gm22_tweets), size=1000), ]

gm22_mean_sent <- mean(rand_gm22_tweets$sentiment)


## L vs BOS 02/01/19 - 18343 - #23
gm23_tweets <-
  get_all_tweets(
    query = "knicks",
    start_tweets = "2019-02-01T07:00:00Z",
    end_tweets = "2019-02-02T06:59:00Z",
    data_path = "cor_gm23/",
    bind_tweets = FALSE,
    n = 1000
  )
gm23_tweets <- bind_tweets(data_path = "cor_gm23/")

gm23_tweets$text <- gsub("[^0-9A-Za-z///' ]","", gm23_tweets$text)
gm23_tweets$text <- gsub("http\\w+", "", gm23_tweets$text)
gm23_tweets$text <- gsub("rt", "", gm23_tweets$text)
gm23_tweets$text <- gsub("@\\w+", "", gm23_tweets$text)
gm23_tweets$text <- tolower(gm23_tweets$text)

sent_gm23_tweets <- sentiment(gm23_tweets$text)
gm23_tweets$sentiment <- sent_gm23_tweets$sentiment

gm23_tweets <- gm23_tweets %>% mutate(game_id = "gm23_bos")

rand_gm23_tweets <- gm23_tweets[sample(nrow(gm23_tweets), size=1000), ]

gm23_mean_sent <- mean(rand_gm23_tweets$sentiment)


## L vs MEM 02/03/19 - 17025 - #24
gm24_tweets <-
  get_all_tweets(
    query = "knicks",
    start_tweets = "2019-02-03T07:00:00Z",
    end_tweets = "2019-02-04T06:59:00Z",
    data_path = "cor_gm24/",
    bind_tweets = FALSE,
    n = 1000
  )
gm24_tweets <- bind_tweets(data_path = "cor_gm24/")

gm24_tweets$text <- gsub("[^0-9A-Za-z///' ]","", gm24_tweets$text)
gm24_tweets$text <- gsub("http\\w+", "", gm24_tweets$text)
gm24_tweets$text <- gsub("rt", "", gm24_tweets$text)
gm24_tweets$text <- gsub("@\\w+", "", gm24_tweets$text)
gm24_tweets$text <- tolower(gm24_tweets$text)

sent_gm24_tweets <- sentiment(gm24_tweets$text)
gm24_tweets$sentiment <- sent_gm24_tweets$sentiment

gm24_tweets <- gm24_tweets %>% mutate(game_id = "gm24_mem")

rand_gm24_tweets <- gm24_tweets[sample(nrow(gm24_tweets), size=1000), ]

gm24_mean_sent <- mean(rand_gm24_tweets$sentiment)


## L vs DET 02/05/19 - 17853 - #25
gm25_tweets <-
  get_all_tweets(
    query = "knicks",
    start_tweets = "2019-02-05T07:00:00Z",
    end_tweets = "2019-02-06T06:59:00Z",
    data_path = "cor_gm25/",
    bind_tweets = FALSE,
    n = 1000
  )
gm25_tweets <- bind_tweets(data_path = "cor_gm25/")

gm25_tweets$text <- gsub("[^0-9A-Za-z///' ]","", gm25_tweets$text)
gm25_tweets$text <- gsub("http\\w+", "", gm25_tweets$text)
gm25_tweets$text <- gsub("rt", "", gm25_tweets$text)
gm25_tweets$text <- gsub("@\\w+", "", gm25_tweets$text)
gm25_tweets$text <- tolower(gm25_tweets$text)

sent_gm25_tweets <- sentiment(gm25_tweets$text)
gm25_tweets$sentiment <- sent_gm25_tweets$sentiment

gm25_tweets <- gm25_tweets %>% mutate(game_id = "gm25_det")

rand_gm25_tweets <- gm25_tweets[sample(nrow(gm25_tweets), size=1000), ]

gm25_mean_sent <- mean(rand_gm25_tweets$sentiment)


## L vs TOR 02/09/19 - 18886 - #26
gm26_tweets <-
  get_all_tweets(
    query = "knicks",
    start_tweets = "2019-02-09T07:00:00Z",
    end_tweets = "2019-02-10T06:59:00Z",
    data_path = "cor_gm26/",
    bind_tweets = FALSE,
    n = 1000
  )
gm26_tweets <- bind_tweets(data_path = "cor_gm26/")

gm26_tweets$text <- gsub("[^0-9A-Za-z///' ]","", gm26_tweets$text)
gm26_tweets$text <- gsub("http\\w+", "", gm26_tweets$text)
gm26_tweets$text <- gsub("rt", "", gm26_tweets$text)
gm26_tweets$text <- gsub("@\\w+", "", gm26_tweets$text)
gm26_tweets$text <- tolower(gm26_tweets$text)

sent_gm26_tweets <- sentiment(gm26_tweets$text)
gm26_tweets$sentiment <- sent_gm26_tweets$sentiment

gm26_tweets <- gm26_tweets %>% mutate(game_id = "gm26_tor")

rand_gm26_tweets <- gm26_tweets[sample(nrow(gm26_tweets), size=1000), ]

gm26_mean_sent <- mean(rand_gm26_tweets$sentiment)


## L vs PHI 02/13/19 - 18983 - #27
gm27_tweets <-
  get_all_tweets(
    query = "knicks",
    start_tweets = "2019-02-13T07:00:00Z",
    end_tweets = "2019-02-14T06:59:00Z",
    data_path = "cor_gm27/",
    bind_tweets = FALSE,
    n = 1000
  )
gm27_tweets <- bind_tweets(data_path = "cor_gm27/")

gm27_tweets$text <- gsub("[^0-9A-Za-z///' ]","", gm27_tweets$text)
gm27_tweets$text <- gsub("http\\w+", "", gm27_tweets$text)
gm27_tweets$text <- gsub("rt", "", gm27_tweets$text)
gm27_tweets$text <- gsub("@\\w+", "", gm27_tweets$text)
gm27_tweets$text <- tolower(gm27_tweets$text)

sent_gm27_tweets <- sentiment(gm27_tweets$text)
gm27_tweets$sentiment <- sent_gm27_tweets$sentiment

gm27_tweets <- gm27_tweets %>% mutate(game_id = "gm27_phi")

rand_gm27_tweets <- gm27_tweets[sample(nrow(gm27_tweets), size=1000), ]

gm27_mean_sent <- mean(rand_gm27_tweets$sentiment)


## L vs MIN 02/22/19 - 19096 - #28
gm28_tweets <-
  get_all_tweets(
    query = "knicks",
    start_tweets = "2019-02-22T07:00:00Z",
    end_tweets = "2019-02-23T06:59:00Z",
    data_path = "cor_gm28/",
    bind_tweets = FALSE,
    n = 1000
  )
gm28_tweets <- bind_tweets(data_path = "cor_gm28/")

gm28_tweets$text <- gsub("[^0-9A-Za-z///' ]","", gm28_tweets$text)
gm28_tweets$text <- gsub("http\\w+", "", gm28_tweets$text)
gm28_tweets$text <- gsub("rt", "", gm28_tweets$text)
gm28_tweets$text <- gsub("@\\w+", "", gm28_tweets$text)
gm28_tweets$text <- tolower(gm28_tweets$text)

sent_gm28_tweets <- sentiment(gm28_tweets$text)
gm28_tweets$sentiment <- sent_gm28_tweets$sentiment

gm28_tweets <- gm28_tweets %>% mutate(game_id = "gm28_min")

rand_gm28_tweets <- gm28_tweets[sample(nrow(gm28_tweets), size=1000), ]

gm28_mean_sent <- mean(rand_gm28_tweets$sentiment)


## W vs SAS 02/24/19 - 18019 - #29
gm29_tweets <-
  get_all_tweets(
    query = "knicks",
    start_tweets = "2019-02-24T07:00:00Z",
    end_tweets = "2019-02-25T06:59:00Z",
    data_path = "cor_gm29/",
    bind_tweets = FALSE,
    n = 1000
  )
gm29_tweets <- bind_tweets(data_path = "cor_gm29/")

gm29_tweets$text <- gsub("[^0-9A-Za-z///' ]","", gm29_tweets$text)
gm29_tweets$text <- gsub("http\\w+", "", gm29_tweets$text)
gm29_tweets$text <- gsub("rt", "", gm29_tweets$text)
gm29_tweets$text <- gsub("@\\w+", "", gm29_tweets$text)
gm29_tweets$text <- tolower(gm29_tweets$text)

sent_gm29_tweets <- sentiment(gm29_tweets$text)
gm29_tweets$sentiment <- sent_gm29_tweets$sentiment

gm29_tweets <- gm29_tweets %>% mutate(game_id = "gm29_sas")

rand_gm29_tweets <- gm29_tweets[sample(nrow(gm29_tweets), size=1000), ]

gm29_mean_sent <- mean(rand_gm29_tweets$sentiment)


## W vs ORL 02/26/19 - 17833 - #30
gm30_tweets <-
  get_all_tweets(
    query = "knicks",
    start_tweets = "2019-02-26T07:00:00Z",
    end_tweets = "2019-02-27T06:59:00Z",
    data_path = "cor_gm30/",
    bind_tweets = FALSE,
    n = 1000
  )
gm30_tweets <- bind_tweets(data_path = "cor_gm30/")

gm30_tweets$text <- gsub("[^0-9A-Za-z///' ]","", gm30_tweets$text)
gm30_tweets$text <- gsub("http\\w+", "", gm30_tweets$text)
gm30_tweets$text <- gsub("rt", "", gm30_tweets$text)
gm30_tweets$text <- gsub("@\\w+", "", gm30_tweets$text)
gm30_tweets$text <- tolower(gm30_tweets$text)

sent_gm30_tweets <- sentiment(gm30_tweets$text)
gm30_tweets$sentiment <- sent_gm30_tweets$sentiment

gm30_tweets <- gm30_tweets %>% mutate(game_id = "gm30_orl")

rand_gm30_tweets <- gm30_tweets[sample(nrow(gm30_tweets), size=1000), ]

gm30_mean_sent <- mean(rand_gm30_tweets$sentiment)





### Creating Data Frame of Mean Sentiment Scores and Game Attendance Numbers
cor_df <- data.frame(game_mean_sentiment = c(gm1_mean_sent, gm2_mean_sent, 
                                             gm3_mean_sent, gm4_mean_sent, 
                                             gm5_mean_sent, gm6_mean_sent, 
                                             gm7_mean_sent, gm8_mean_sent, 
                                             gm9_mean_sent, gm10_mean_sent, 
                                             gm11_mean_sent, gm12_mean_sent, 
                                             gm13_mean_sent, gm14_mean_sent, 
                                             gm15_mean_sent, gm16_mean_sent, 
                                             gm17_mean_sent, gm18_mean_sent, 
                                             gm19_mean_sent, gm20_mean_sent, 
                                             gm21_mean_sent, gm22_mean_sent, 
                                             gm23_mean_sent, gm24_mean_sent, 
                                             gm25_mean_sent, gm26_mean_sent, 
                                             gm27_mean_sent, gm28_mean_sent, 
                                             gm29_mean_sent, gm30_mean_sent), 
                     game_attendance = c(18249, 19427, 19812, 19221, 18295, 
                                         19812, 19812, 19812, 18948, 19812, 
                                         19440, 18662, 18602, 18437, 19080, 
                                         19812, 19812, 18596, 19493, 18819, 
                                         18852, 18842, 18343, 17025, 17853, 
                                         18886, 18983, 19096, 18019, 17833),
                     g_a_div_by_100k = c(18249/100000, 19427/100000, 
                                         19812/100000, 19221/100000, 
                                         18295/100000, 19812/100000, 
                                         19812/100000, 19812/100000, 
                                         18948/100000, 19812/100000, 
                                         19440/100000, 18662/100000, 
                                         18602/100000, 18437/100000, 
                                         19080/100000, 19812/100000, 
                                         19812/100000, 18596/100000, 
                                         19493/100000, 18819/100000, 
                                         18852/100000, 18842/100000, 
                                         18343/100000, 17025/100000, 
                                         17853/100000, 18886/100000, 
                                         18983/100000, 19096/100000, 
                                         18019/100000, 17833/100000),
                     g_a_div_by_1mil = c(18249/1000000, 19427/1000000, 
                                         19812/1000000, 19221/1000000, 
                                         18295/1000000, 19812/1000000, 
                                         19812/1000000, 19812/1000000, 
                                         18948/1000000, 19812/1000000, 
                                         19440/1000000, 18662/1000000, 
                                         18602/1000000, 18437/1000000, 
                                         19080/1000000, 19812/1000000, 
                                         19812/1000000, 18596/1000000, 
                                         19493/1000000, 18819/1000000, 
                                         18852/1000000, 18842/1000000, 
                                         18343/1000000, 17025/1000000, 
                                         17853/1000000, 18886/1000000, 
                                         18983/1000000, 19096/1000000, 
                                         18019/1000000, 17833/1000000),
                     game_num = c("gm_1", "gm_2", "gm_3", "gm_4", "gm_5", 
                                  "gm_6", "gm_7", "gm_8", "gm_9", "gm_10", 
                                  "gm_11", "gm_12", "gm_13", "gm_14", "gm_15", 
                                  "gm_16", "gm_17", "gm_18", "gm_19", "gm_20", 
                                  "gm_21", "gm_22", "gm_23", "gm_24", "gm_25", 
                                  "gm_26", "gm_27", "gm_28", "gm_29", "gm_30"),
                     date = c("2018-10-17", "2018-10-20", "2018-10-26", 
                              "2018-10-29", "2018-10-31", "2018-11-05", 
                              "2018-11-11", "2018-11-20", "2018-11-23", 
                              "2018-12-01", "2018-12-03", "2018-12-08", 
                              "2018-12-09", "2018-12-17", "2018-12-21", 
                              "2018-12-25", "2019-01-11", "2019-01-13", 
                              "2019-01-21", "2019-01-23", "2019-01-27", 
                              "2019-01-30", "2019-02-01", "2019-02-03", 
                              "2019-02-05", "2019-02-09", "2019-02-13", 
                              "2019-02-22", "2019-02-24", "2019-02-26")
)

cor_df[['date']] <- as.Date(cor_df[['date']], 
                            format = "%Y-%m-%d")

# View(cor_df)
# summary(cor_df)
# str(cor_df)





### Running Correlation Test
cor.test(cor_df$game_mean_sentiment, cor_df$game_attendance)





### Making interactive plots

## Line plots
sentiment_hc_lp_1 <- hchart(
  cor_df, "line",
  hcaes(x = date, y = game_mean_sentiment)
)

sentiment_hc_lp_1


sentiment_hc_lp_2 <- hchart(
  cor_df, "line",
  hcaes(x = date, y = game_mean_sentiment, group = game_num)
)

sentiment_hc_lp_2


game_attendance_hc_lp_1 <- hchart(
  cor_df, "line",
  hcaes(x = date, y = game_attendance)
)

game_attendance_hc_lp_1


game_attendance_hc_lp_2 <- hchart(
  cor_df, "line",
  hcaes(x = date, y = game_attendance, group = game_num)
)

game_attendance_hc_lp_2


## Scatter plots
sentiment_hc_sp_1 <- hchart(
  cor_df, "scatter", 
  hcaes(x = date, y = game_mean_sentiment)
)

sentiment_hc_sp_1


sentiment_hc_sp_2 <- hchart(
  cor_df, "scatter", 
  hcaes(x = date, y = game_mean_sentiment, group = game_num)
)

sentiment_hc_sp_2


game_attendance_hc_sp_1 <- hchart(
  cor_df, "scatter", 
  hcaes(x = date, y = game_attendance)
)

game_attendance_hc_sp_1


game_attendance_hc_sp_2 <- hchart(
  cor_df, "scatter", 
  hcaes(x = date, y = game_attendance, group = game_num)
)

game_attendance_hc_sp_2


## Plots that exclude 'date'
# Scatter plots
hc_sp_1 <- hchart(
  cor_df, "scatter", 
  hcaes(x = game_mean_sentiment, y = game_attendance)
)

hc_sp_1

hc_sp_2 <- hchart(
  cor_df, "scatter", 
  hcaes(x = game_attendance, y = game_mean_sentiment)
)

hc_sp_2


# Line plots
hc_lp_1 <- hchart(
  cor_df, "line", 
  hcaes(x = game_mean_sentiment, y = game_attendance)
)

hc_lp_1

hc_lp_2 <- hchart(
  cor_df, "line", 
  hcaes(x = game_attendance, y = game_mean_sentiment)
)

hc_lp_2