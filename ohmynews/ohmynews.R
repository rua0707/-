#준비
rm(list=ls())
dyn.load('C:/Program Files/Java/jre1.8.0_201/bin/server/jvm.dll')

install.packages("rvest")
install.packages("dplyr")
install.packages("stringr")
install.packages('KoNLP')
install.packages('wordcloud')
install.packages('RColorBrewer')
install.packages("rJava")

require(rJava)
require(stringr)
require(rvest)
require(dplyr)
library(wordcloud)
library(RColorBrewer)
library(KoNLP)

useNIADic()
#KONLP패키지 안에 sejong패키지
dics <- c('sejong','woorimalsam','insighter')
category <- c('name')
user_d <- data.frame(term="정준영", tag='ncn', category = 'people_names')
buildDictionary(ext_dic = dics,category_dic_nms = category, user_dic = user_d, replace_usr_dic=TRUE)
#################################################################################

#url 따오기
ohmy_url <- list()
for (i in 1:100) {
  ohmy_url[i] <- paste0("http://www.ohmynews.com/NWS_Web/Articlepage/Total_Article.aspx?PAGE_CD=N0120&pageno=",i)
}
ohmy_url

title1 <- c()

for (i in 1:length(ohmy_url)) {
  html <- read_html(ohmy_url[[i]])
  temp <- repair_encoding(html_text(html_nodes(html,'.news_list') %>% 
                                    html_nodes('dt') %>% 
                                    html_nodes('a'), trim = FALSE),from = 'utf-8')
  title1 <- c(title1,temp)
}
title1

#불필요한(지저분한) 특수문자 제거
for (j in 1:length(title1)) {
  title1[j] <- str_replace_all(title1[j],"[\n.\"()/~:,%#?'-]", "")
}
title1

# ] 기준으로 문자열 자르기
for (k in 1:length(title1)) {
  if (substr(title1[k],1,1) == "[") {
    title1[k] <- str_split(title1[k], pattern = "]", n = 2, simplify = FALSE)
  }
}
title1

#위에서 잘라서 list원소가 2개가 된것들 정리
for (l in 1:length(title1)) {
  if (length(title1[[l]]) == "2") {
    title1[[l]] <- title1[[l]][2]
  }
}

# 나머지 잔재물 정리
for (l in 1:length(title1)) {
  if (substr(title1[[l]],1,1) == "[") {
    title1[l] <- str_sub(title1[l],10)
  }
}

title1 <- unlist(title1)

#[]키워드를 제거하다가 왼쪽에 남은 공백을 제거한다.
title1 <- str_trim(title1, side = "left")
title1
###############################################################################

data1 <- title1
data1

#extractNoun: 각 기사제목으로부터 명사들만 추출
#USE.NAMES=T 이름 속성 반환 / USE.NAMES=F 이름 속성 없이 반환
data2<-sapply(data1,extractNoun,USE.NAMES=FALSE)
data2

head(unlist(data2),30)

data3<-unlist(data2)
length(data3)

data3<-Filter(function(x){nchar(x)>=2},data3)
length(data3)
data3

write(unlist(data3),"ohmy.txt")

data4<- read.table("ohmy.txt")
data4
nrow(data4)

wordcount <- table(data3)
wordcount
head(sort(wordcount, decreasing=TRUE),20)

palete <- brewer.pal(9,"Set1") 

wordcloud(names(wordcount),freq=wordcount,scale=c(5,0.25),rot.per=0.25,min.freq=5,random.order=FALSE,random.color=TRUE,colors=palete)

top<-head(sort(wordcount,decreasing=TRUE),15)
top

bp<-barplot(top,main="Top 15",col=rainbow(15),cex.names=1.2,las=2,ylim=c(0,250))

text(x=bp,y=top*1.05,labels=paste(top,"건"),col="black",cex=1)

