library(corrplot)

dataframe0<-read.csv("C:/Users/Messi/Downloads/metadata2.csv", header=TRUE, stringsAsFactors = FALSE)
datatype<-as.data.frame(sapply(dataframe0,class))

#Filter dataset
#1.Delete NA values
dataframe1<-dataframe0[complete.cases(dataframe0),]

#2.Extract parameters data from original dataset
dataframe2<-subset(dataframe1,select = c(movie_title,
                                         gross,
                                         budget,
                                         movie_facebook_likes,
                                         cast_total_facebook_likes,
                                         num_user_for_reviews,
                                         num_critic_for_reviews,
                                         imdb_score
                                         ))
dataframe3<-dataframe2[-1]

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
descriptive.x<-sapply(dataframe3,descriptive.summary)
descriptive.x
cordf1<-cor(dataframe3,use = "complete.obs")
cordf1

#Normalization
norm.fun<-function(x){
  norm.max<-max(x,na.rm=TRUE)
  norm.result<-x/norm.max*100
}
dataframe4<-as.data.frame(sapply(dataframe3,norm.fun))
boxplot(dataframe4,outline = FALSE,color="green")
summary.dataframe4<-sapply(dataframe4,descriptive.summary)
cordf2<-as.data.frame(cor(dataframe4,use = "complete.obs"))
corrplot(cordf2, method=c("number"))

#Assigning weights
cal_weight<-function(x){
  var.cv=(sd(x,na.rm = TRUE))/(mean(x,na.rm=TRUE))
  var.weight=cv/sum(summary.dataframe4$cv)
}
var_weight<-sapply(dataframe4,cal_weight)

#Calculate the index value
dataframe5$Name<-dataframe2[1]
dataframe5$Index<-rowSum(t(dataframe5)*var_weight)
