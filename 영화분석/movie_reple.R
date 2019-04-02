dyn.load('C:/Program Files (x86)/Java/jre1.8.0_111/bin/client/jvm.dll')
#rjava??
install.packages('rvest')
install.packages("dplyr")
install.packages("stringr")
require(rJava)
#문자를 보다 쉽게 처리하고 작업할 수 있는 패키지
#install.packages("stringr")
require(stringr)
require(rvest)
require(dplyr)

#require VS library

#require() 함수와 library() 함수의 차이는 설치되어 있지 않은 
#패키지를 불러오는 경우에 library() 함수는 오류를 발생시키지만, 
#require() 함수는 경고 메시지를 보여주는 차이가 있다. 
#require() 함수는 패키지를 불러오는 데 성공하면 TRUE, 
#실패하면 FALSE를 출력한다.


#영화 페이지 URL따오기
movie_url='https://movie.naver.com/movie/running/current.nhn?view=list&tab=normal&order=reserve'



#각각 영화의 url따오기
#require(rvest)

#html=read_html(movie_url)
#html_1=html_nodes(html,'.tit')
#html_2=html_nodes(html_1,'a')
#html_3=html_attr(html_2,'href')
#View(html_3)

#위에꺼 한줄로 합치기
#require(dplyr)

html=read_html(movie_url)
movie_link=html %>% html_nodes('.tit') %>% 
  html_nodes('a') %>% html_attr('href')

#상위 10개영화의 url에서 코드 값만 따오기
#require(stringr)
m_code=movie_link[1:10]
movie_code=str_sub(m_code,-6,-1)


#각영화의 이름으로 리스트 만든다.
name_html=read_html(movie_url)
m_name=name_html %>% html_nodes('.tit') %>% html_nodes('a')
str(m_name)

#인덱싱 해주기 위해 character로 바꿔준다.
as.character(m_name)
movie_name=vector()
for (i in 1:10){
  name=as.character(m_name[i])
  movie_name[i]=str_sub(name,46,-5)
}
movie_name


#각 영화의 url만들기
#각 영화의 댓글을 보기 위해서는 각 영화의 페이지로 들어가야한다.
Each_movie=vector()
for (i in 1:10){
  Each_movie[i]=paste('https://movie.naver.com/movie/bi/mi/point.nhn?code=',
                           movie_code[i],'#tab',seq='')
}
#띄어쓰기 제거
Each_movie=gsub('\\s','',Each_movie)


#각 영화 페이지에서 댓글프레임의 URL을 따온다.
reple_html=read_html(Each_movie[1])
reple_frame=reple_html %>% html_nodes('.ifr_module2') %>% 
  html_nodes('iframe') %>% html_attr('src')

#URL의 구조가 
#"https://movie.naver.com/movie/bi/mi/pointWriteFormList.nhn?code=163608&type=after&isActualPointWriteExecute=
#false&isMileageSubscriptionAlready=false&isMileageSubscriptionReject=false&page=1"
#규칙적이다. 각 영화에서 댓글창의 다음페이지로 넘어가려면 맨끝에 페이지숫자를 바꿔주면된다.
#다른영화의 댓글로 넘어가려면 첫줄의 코드값을 위에서 만든 movie_code값으로 바꿔준다.


#댓글 페이지 url따오기
#한영화당 10페이지씨
reple_url=list()
page=vector()
for (i in 1:10){
  substr(reple_frame,42,47)=movie_code[i]
  for (j in 1:30){
    page[j]=paste('https://movie.naver.com',
                  reple_frame,'&page=',j,seq='')
    #paste로 붙히면 띄어쓰기가 돼서 공백을 없애줘야한다.
    page=gsub('\\s','',page)
  }
  reple_url[[i]]=page
}
head(reple_url)
names(reple_url)=movie_name
head(reple_url)


#댓글 추출
movie_reple=list()
movie_reple_all=list()
reple=vector()
for (i in 1:10){
  for (k in 1:10){
    reple_html_2=read_html(reple_url[[i]][k])
    #'p'로 들어갔었는데 2번째 줄까지 밖에 안나와서 'onclick'으로 들어갔다.
    m_reple=reple_html_2 %>% html_nodes('.score_reple') %>% 
      html_nodes('a') %>% html_attr('onclick')
    
    #댓글이 짝수행에만 있으므로 짝수행만 추출
    m_reple_1=m_reple[seq(2,length(m_reple),2)]
    for (j in 1:length(m_reple_1)){
      #한글아닌것 전부 제거 str_replace_all를 이용해서 제거하려했는데
      #띄어쓰기도 사라지고 안지워지는 문자도 있어서 인덱싱을 해준다.
      #reple_word=str_replace_all(m_reple_1[j],'[A-z]','')
      #reple_word=str_replace_all(reple_word,'[0-9]','')
      #reple_word=str_replace_all(reple_word,'[^[:alnum:]]','')
      #reple[j]=reple_word
      reple[j]=str_sub(m_reple_1[j],135,-52)
    }
    movie_reple[[k]]=reple
  }
  movie_reple_all[[i]]=unlist(movie_reple)
}
#각 리스트 이름을 영화 제목으로 바꾼다.
names(movie_reple_all)=movie_name
head(movie_reple_all[[1]])



#시각화
install.packages('KoNLP')
install.packages('wordcloud')
install.packages('RColorBrewer')

library(wordcloud)
library(RColorBrewer)
library(KoNLP)
useSejongDic()


word_1=movie_reple_all[1]
#댓글로부터 명사들만 추출
#USE.NAMES=T 이름 속성 반환 / 
#USE.NAMES=F 이름 속성 없이 반환
word_2=sapply(word_1,extractNoun,USE.NAMES=F)
word_2
head(unlist(word_2),30)

#벡터 형태로 저장
word_3=unlist(word_2)
length(word_3)
#2글자 이상 단어만 추출
word_3<-Filter(function(x){nchar(x)>=2},word_3)
length(word_3)
word_3
#텍스트 파일로 저장; 워드클라우드를 꾸미기 위해(tagxedo)
write(unlist(word_3),"word_1.txt")

word_4=read.table("word_1.txt")
word_4
nrow(word_4) 
wordcount=table(word_3)
wordcount
head(sort(wordcount, decreasing=T),20)

wordcount_1=sort(wordcount, decreasing=T)
wordcount_1=wordcount_1[c(-1,-7)]
head(wordcount_1)

#글자 색 지정
palete <- brewer.pal(9,"Set1") 
#워드클라우드작성
#freq : 빈도는 언급된 단어수
#scale : 폰트 사이즈 조정
#rot.per : 수직 텍스트의 비율 조정
#min.freq : 최소 몇번의 언급된 단어만 추출
#random.order : 순서 랜덤
#random.color : 컬러 랜덤
wordcloud(names(wordcount),freq=wordcount,scale=c(5,0.25),rot.per=0.25,min.freq=5,random.order=F,random.color=T,colors=palete)


palete <- brewer.pal(9,"Set1")
for (i in 1:10){
  word_1=movie_reple_all[i]
  word_2=sapply(word_1,extractNoun,USE.NAMES=F)
  word_3=unlist(word_2)
  word_3=Filter(function(x){nchar(x)>=2},word_3)
  write(unlist(word_3),"word_1.txt")
  word_4=read.table("word_1.txt")
  wordcount=table(word_4)
  wordcloud(names(wordcount),freq=wordcount,  scale=c(3,1.5),
            rot.per=0.1,max.words=10, min.freq=4,random.order=F,
            random.color=T,colors=palete)
}







#barplot
top<-head(sort(wordcount,decreasing=T),15)
top
#main : 제목
#col : 색깔; rainbow(15) -> 15가지의 무지개 색깔
#cex.naems : 축이름의 크기
#las : 축이름을 가로(1), 세로(2) 로 할건지
#ylim : y축 범위
bp<-barplot(top,main="Top 15",col=rainbow(15),cex.names=1.2,las=2,ylim=c(0,250))
#각 플롯에 대한 텍스트추가
text(x=bp,y=top*1.05,labels=paste(top,"건"),col="black",cex=1)
