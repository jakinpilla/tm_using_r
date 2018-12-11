# 구조적 토픽모형

library("tm")
library("stringr")
library("stm")

my.text.location <- "./ymbaek_papers"
mypaper <- VCorpus(DirSource(my.text.location),
                   readerControl = list(language = "en"))

dim(gadarian)
colnames(gadarian)
summary(gadarian)

# list.files() 함수를 이용하면 해당 폴더의 파일 이름을 추출할 수 있다.
myfilenames <- list.files(path = my.text.location, pattern= NULL, all.files = T)
myfilenames
length(myfilenames)

# 파일 이름을 데이트프레임으로 저장하였다.
mytxtdf <- data.frame(myfilenames[3:26])
mytxtdf
# 해당 영문 논믄의 발간년도를 추출하였다.
str_extract_all(mytxtdf[, 1], "[[:digit:]]{4}") # 리스트를 반환
unlist(str_extract_all(mytxtdf[, 1], "[[:digit:]]{4}"))
as.numeric(unlist(str_extract_all(mytxtdf[, 1], "[[:digit:]]{4}")))

mytxtdf$year <- as.numeric(as.numeric(unlist(str_extract_all(mytxtdf[, 1], "[[:digit:]]{4}"))))

colnames(mytxtdf)[1] <- "file.name"

# 저자가 미국에 체류할 때를 0, 귀국하였을 때를 1로 하는 가변수를 생성
mytxtdf$return.kor <- ifelse(mytxtdf$year > 2011, 1, 0)

# 논문의 초록을 문자형 자료로 입력
mytxtdf$abstract <- NA
for (i in 1:24) {
  mytxtdf$abstract[i] <- as.character(mypaper[[i]][1])
}

mytxtdf
summary(mytxtdf)
head(mytxtdf, 3) 

# 텍스트 사전처리(tm 라이브러리 이용)
mypreprocess <- textProcessor(mytxtdf$abstract, metadata = mytxtdf)

mypreprocess$documents[1]

# DTM을 구성함
myout <- prepDocuments(mypreprocess$documents, mypreprocess$vocab, 
                       mypreprocess$meta, lower.thresh = 0)

# STM 추정(5개의 토픽)
mystm <- stm(myout$documents, myout$vocab,K=5, prevalence = ~return.kor, data = myout$meta,
             seed = 2494, init.type = "Spectral")

# 각 토픽이 발현되는 단어들 점검
labelTopics(mystm, topics = 1:5)

# 토픽들 사이의 관계
mystm.corr <- topicCorr(mystm)
mystm.corr

install.packages("igraph")
plot(mystm.corr)

# 메타데이터와 토픽 발현가능성의 관계 테스트
myresult <- estimateEffect(c(1:5) ~ return.kor, mystm, mytxtdf)
summary(myresult)

plot(myresult, covariate = "return.kor", 
     topics=4, model=mystm, xlim=c(-1.5, 1.5))
























