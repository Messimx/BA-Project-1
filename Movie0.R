Moviedata0<-read.csv("C:/Users/Messi/Desktop/Classes/BusinessAnalytics/Project/movie_metadata.csv", header=TRUE, stringsAsFactors = FALSE)
datatype<-as.data.frame(sapply(Moviedata0,class))
Moviedata0$revenue<-Moviedata0$gross-Moviedata0$budget

#Filter dataset
#1.Delete data before 2005
Moviedata1<-subset(Moviedata0,title_year>=2001)

#2.Extract parameters data from original dataset
Moviedata2<-subset(Moviedata1,select = c(movie_title,
                                         gross,
                                         budget,
                                         revenue,
                                         movie_facebook_likes,
                                         cast_total_facebook_likes,
                                         director_facebook_likes,
                                         num_user_for_reviews,
                                         num_critic_for_reviews,
                                         imdb_score
                                         ))
Moviedata3<-Moviedata2[-1]

#Descriptive analysis of Xs
#mode function
getmode<-function(x){
  uniqv<-na.omit(unique(x))
  uniqv[which.max(tabulate(match(x,uniqv)))]
}
#99% confidence function
confidence99lower<-function(x){
  mean<-mean(x,na.rm=TRUE)
  sd<-sd(x,na.rm = TRUE)
  lower<-mean-3*sd
}
confidence99upper<-function(x){
  mean<-mean(x,na.rm=TRUE)
  sd<-sd(x,na.rm = TRUE)
  upper<-mean+3*sd
}

#Descriptive funciton
descriptive.summary<-function(x)list(mean = mean(x,na.rm=TRUE),
                                     median=median(x,na.rm=TRUE),
                                     mode=getmode(x),
                                     var = var(x,na.rm=TRUE), 
                                     sd=sd(x,na.rm=TRUE), 
                                     min=min(x,na.rm=TRUE),
                                     max=max(x,na.rm=TRUE),
                                     "25%"=quantile(x,0.25,na.rm=TRUE),
                                     "50%"=quantile(x,0.5,na.rm=TRUE),
                                     "75%"=quantile(x,0.75,na.rm=TRUE),
                                     cv=(sd(x,na.rm = TRUE))/(mean(x,na.rm=TRUE)),
                                     confidence99_from=confidence99lower(x),
                                     confidence99_to=confidence99upper(x))
descriptive.x<-sapply(Moviedata3,descriptive.summary)
descriptive.x
cordf1<-cor(Moviedata3,use = "complete.obs")
cordf1
