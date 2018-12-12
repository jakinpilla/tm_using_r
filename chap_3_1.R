# 감정어휘 사전을 활요한 텍스트 감정분석

install.packages("tidytext")
library(tidytext)

# AFFINN 감정어휘 사전 호출
get_sentiments("afinn")
AFINN <- data.frame(get_sentiments("afinn"))
summary(AFINN)

hist(AFINN$score, breaks = 20, xlim = c(-6, 6), col = "steelblue", 
     xlab = "sentiment score in AFINN", 
     main = "")

# 빙 류 연구팀의 opinion lexicon 사전 호출
get_sentiments("bing")

# EmoLex 사전 호출
get_sentiments("nrc")

# opinion lexicon, EmoLex 사전 저장 후 기초통계분석
oplex <- data.frame(get_sentiments("bing"))
table(oplex$sentiment)

emolex <- data.frame(get_sentiments("nrc"))
table(emolex$sentiment)

emolex$word[emolex$sentiment == "anger"]

# 텍스트 데이터 호출
install.packages("tm")
install.packages("stringr")
install.packages("tidyverse")

library(tm)
library(stringr)
library(dplyr)
library(tidyverse)

my.text.location <- "./ymbaek_papers"
mypaper <- VCorpus(DirSource(my.text.location),
                   readerControl = list(language = "en"))

mytxt <- rep(NA, 24)
for (i in 1:24) {
  mytxt[i] <- as.character(mypaper[[i]][1])
}

# tidytext 형태의 데이터 구성
my.df.text <- data_frame(paper.id = 1:24, doc = mytxt)
my.df.text

# 이제 문서-단어로 구성된 행렬을 구성
# unnest_tokens() 함수는 주어진 tidytext 오브젝트의 text 변수를 word로 구분한다는 의미
my.df.text.word <- my.df.text %>%
  unnest_tokens(word, doc)
my.df.text.word

# inner_join() 함수를 이용하면 결과를 쉽게 얻을 수 있다.
myresult.sa <- my.df.text.word %>%
  inner_join(get_sentiments("bing")) 
  
my.df.text.word %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, paper.id, sentiment) %>% 
  spread(sentiment, n, fill=0) -> myresult.sa

myresult.sa

# 문서별로 긍정단어와 부정단어를 합쳐보자.
# 긍정적 감정어휘와 부정적 감정어휘의 차이값도 구하였다.
myresult.sa %>% 
  group_by(paper.id) %>%
  summarise(pos.sum = sum(positive), 
            neg.sum = sum(negative),
            pos.sent= pos.sum - neg.sum) -> myagg

myagg

# 문서의 메타데이터를 구한 후 합쳐보자
myfilenames <- list.files(path = my.text.location,
                          pattern = NULL, all.files = T)

paper.name <- myfilenames[3:26]
pub.year <- as.numeric(unlist(str_extract_all(paper.name, "[[:digit:]]{4}")))
paper.id <- 1:24

pub.year.df <- data.frame(paper.id, paper.name, pub.year)

myagg <- merge(myagg, pub.year.df, by='paper.id', all =T)
myagg

myagg$pos.sent <- ifelse(is.na(myagg$pos.sent), 0, myagg$pos.sent)
myagg$pos.sum <- ifelse(is.na(myagg$pos.sum), 0, myagg$pos.sum)
myagg$neg.sum <- ifelse(is.na(myagg$neg.sum), 0, myagg$neg.sum)
myagg

head(myagg)
library(reshape2)


myagg.long <- reshape(myagg, idvar = "paper.id", 
                      varying = list(2:4), timevar = "category", 
                      v.names = "value", direction= "long")

myagg.long

melt(myagg, id.vars = c("paper.id", "paper.name", "pub.year"),
     value.name = "value") %>% head

myagg.long$cate[myagg.long$category == 1] <- "Positive words"
myagg.long$cate[myagg.long$category == 2] <- "Negative words"
myagg.long$cate[myagg.long$category == 3] <- "Positivity words"

library(ggplot2)
ggplot(data = myagg.long, aes(pub.year, y=value)) +
  geom_bar(stat = "identity") +
  labs(x = "Publication year", y = "value") +
  scale_x_continuous(limits = c(2009, 2015)) + 
  facet_grid(cate~.)

myagg.long.year <- aggregate(value ~ pub.year + cate, myagg.long, mean)
myagg.long.year %>% head

ggplot(data = myagg.long.year, aes(x = pub.year, y=value)) +
  geom_line() +
  labs(x = "Publication year", y = "value") +
  scale_x_continuous(limits = c(2009, 2015)) +
  facet_grid(cate ~.)

# install.packages("RTextTools")
# 지도 기계학습을 이용한 감성분석
library(RTextTools)

h.train <- readLines("happy.txt")
s.train <- readLines("sad.txt")
h.test <- readLines("happy_test.txt")
s.test <- readLines("sad_test.txt")

# 데이터를 각각 훈련 데이터, 테스트 데이터 순으로 결합
tweet <- c(h.train, s.train, h.test, s.test)

# 해당 트윗에 맞는 감정을 부여(happy=1, sad=0)
sent <- c(rep(1, length(h.train)), 
          rep(0, length(s.train)),
          rep(1, length(h.test)),
          rep(0, length(s.test)))
sent

# 데이터의 사전처리
tweet.pp <- create_matrix(tweet, language = "english", 
                          removeStopwords = F,
                          removeNumbers = T, 
                          stripWhitespace = T, 
                          removePunctuation = T, 
                          toLower = T, 
                          stemWords = F, tm :: weightTfIdf)

tweet.pp <- as.matrix(tweet.pp)

# 여러 기계학습 알고리즘을 적용
train.end <- length(h.train) + length(s.train)
all.end <- length(sent)
container <- create_container(tweet.pp, 
                              as.numeric(sent), 
                              trainSize = 1:train.end, 
                              testSize = (1+train.end) : all.end,
                              virgin = F)

myclassifier <- train_models(container, 
                             algorithms = c("SVM", "GLMNET", "SLDA", "TREE", "BAGGING",
                                            "RF", "MAXENT", "BOOSTING"))

# 기계학습 알고리즘을 활용해 테스트 데이터의 문서에 드러난 감정을 예측
myresult <- classify_models(container, myclassifier)
dim(myresult)
head(myresult)

# 테스트 데이터 문서에 대한 인간의 판단을 추출
mytestlabel <- as.numeric(sent)[(1+train.end) : all.end]

table(myresult$SVM_LABEL, mytestlabel)
table(myresult$GLMNET_LABEL, mytestlabel)
table(myresult$GLMNET_LABEL, mytestlabel)
table(myresult$TREE_LABEL, mytestlabel)
table(myresult$BAGGING_LABEL, mytestlabel)
table(myresult$FORESTS_LABEL, mytestlabel)
table(myresult$MAXENTROPY_LABEL, mytestlabel)
table(myresult$LOGITBOOST_LABEL, mytestlabel)









