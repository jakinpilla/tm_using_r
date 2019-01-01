# text as data approach

# 알고리즘에 기초한 텍스트 분석 방법은 텍스트를 해석하는 인간의 지능을 유사하게 혹은 거칠게
# 모방한 인공지능이다.

# 말뭉치 > 문서> 단락 > 문장 > 단어 > 형태소

# list() : 리스트 형식의 오브젝트



library(tensorflow)

hello<-tf$constant("Hello,tensorflow")
sess<-tf$Session()
print(sess$run(hello))
sess$close()

#노드수
node1<-tf$constant(3.0,tf$float32)
node2<-tf$constant(5.0)
node3<-tf$add(node1,node2)
print(node2)
sess$close()

#placeholder
a<-tf$placeholder(tf$float32)
b<-tf$placeholder(tf$float32)

#모델설정
adder.node<-a+b

#automatically close
with(tf$Session() %as% sess, {
  print(sess$run(adder.node,
                 feed_dict = dict(a = 3, b = 4.5)))
  print(sess$run(adder.node,
                 feed_dict = dict(a = c(1:10), b = c(21:30))))
})













