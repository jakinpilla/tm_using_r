#if you need to install the packages needed, the following lines of code will do so
install.packages("kernlab")
install.packages("caret")
install.packages("tm")
install.packages("dplyr")
install.packages("splitstackshape")
install.packages("e1071")

#the following commands will activate the needed libraries
library("kernlab")
library("caret")
library("tm")
library("dplyr")
library("splitstackshape")
library("e1071")

# Step 1. Ingest your training data and clean it.
train <- VCorpus(DirSource("Training", encoding = "UTF-8"), readerControl=list(language="English"))
train <- tm_map(train, content_transformer(stripWhitespace))
train <- tm_map(train, content_transformer(tolower))
train <- tm_map(train, content_transformer(removeNumbers))
train <- tm_map(train, content_transformer(removePunctuation))

# 한국어 텍스트 데이터를 불어오기
# 먼저 제목만으로된 텍스트를 분류할 수 있는지 시도해보자.
mydata <- read.csv('./labeled_dataset.csv')
head(mydata)
mydata %>% 
  select(-X) %>% 
  rename(date = Date,
         title = Title,
         cate  = Category) -> mydata; head(mydata)

# 코퍼스 만들고 데이터를 정제하기
cps <- VCorpus(VectorSource(mydata$title))
cps
cps[[1]]$content

# 카테고리의 구성 비율은 어떻게 될까?
mydata %>%
  group_by(cate) %>%
  tally( ) %>%
  mutate(pct = (n / sum(n) * 100)) %>%
  rename(cnt = n)

# 이런 비율을 유지하면서 데이터를 나눌 수 있을까?
# install.packages('caTools')
library(caTools)
head(mydata)

Y = mydata[, "cate"] # extract labels from the data

msk = sample.split(Y, SplitRatio=.8)
table(Y,msk)
t=sum( msk); t  # number of elements in one class
f=sum(!msk); f  # number of elements in the other class

# use results
train = mydata[msk, 2:3]  # use output of sample.split to ...
# train %>% View

train %>%
  group_by(cate) %>%
  tally() %>%
  mutate(pct = (n / sum(n) * 100)) %>%
  rename(cnt = n)

test= mydata[!msk,2:3]  # create train and test subsets
test %>%
  group_by(cate) %>% 
  tally() %>% 
  mutate(pct = n / sum(n) * 100) %>% 
  rename(cnt = n)

train.txt <- train$title
test.txt <- test$title

train.label <- train$cate
test.label <-test$cate

# Step 2. Create your document term matrices for the training data.
train.dtm <- as.matrix(DocumentTermMatrix(train, control=list(wordLengths=c(1,Inf))))
#train.matrix <- as.matrix(train.dtm, stringsAsFactors=F)

# Step 3. Repeat steps 1 & 2 above for the Test set.
test <- VCorpus(DirSource("Test", encoding = "UTF-8"), readerControl=list(language="English"))
test <- tm_map(test, content_transformer(stripWhitespace))
test <- tm_map(test, content_transformer(tolower))
test <- tm_map(test, content_transformer(removeNumbers))
test <- tm_map(test, content_transformer(removePunctuation))
test.dtm <- as.matrix(DocumentTermMatrix(test, control=list(wordLengths=c(1,Inf))))

# Step 4. Make test and train matrices of identical length (find intersection)
train.df <- data.frame(train.dtm[,intersect(colnames(train.dtm), colnames(test.dtm))])
test.df <- data.frame(test.dtm[,intersect(colnames(test.dtm), colnames(train.dtm))])

# Step 5. Retrieve the correct labels for training data and put dummy values for testing data
label.df <- data.frame(row.names(train.df))
colnames(label.df) <- c("filenames")
label.df<- cSplit(label.df, 'filenames', sep="_", type.convert=FALSE)
train.df$corpus<- label.df$filenames_1
test.df$corpus <- c("Neg")

# Step 6. Create folds of your data, then run the training once to inspect results
df.train <- train.df
df.test <- train.df
df.model<-ksvm(corpus~., data= df.train, kernel="rbfdot")
df.pred<-predict(df.model, df.test)
df.pred %>% as.factor -> df.pred; df.pred %>% class
df.test$corpus <- as.factor(df.test$corpus) ; df.test$corpus %>% class
con.matrix<-confusionMatrix(df.pred, df.test$corpus)
print(con.matrix)

# Step 7. Run the final prediction on the test data and re-attach file names. 
df.test <- test.df
df.pred<-predict(df.model, df.test)
results <- as.data.frame(df.pred)
rownames(results) <- rownames(test.df)
print(results)

