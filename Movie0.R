library(corrplot)
install.packages("corrgram")
library(corrgram)

dataframe0<-read.csv("https://raw.githubusercontent.com/Messimx/BA-Project-1/master/Final%20dataset%20(1).csv", header=TRUE, stringsAsFactors = FALSE)
datatype<-as.data.frame(sapply(dataframe0,class))

#Filter dataset
dataframe1<-dataframe0[-1]
dataframe1$Net.Trade..Exports...Imports..in....<-NULL


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
descriptive.x<-sapply(dataframe1,descriptive.summary)
descriptive.x
cordf1<-cor(dataframe1,use = "complete.obs")
cordf1

#Normalization
norm.fun<-function(x){
  norm.max<-max(x,na.rm=TRUE)
  norm.result<-x/norm.max*100
}
dataframe2<-as.data.frame(sapply(dataframe1,norm.fun))
boxplot(dataframe2,outline = FALSE,color="green")
summary.dataframe2<-sapply(dataframe2,descriptive.summary)
cordf2<-as.data.frame(cor(dataframe2,use = "complete.obs"))
corrgram(cordf2, order = TRUE, upper.panel = panel.cor,main = "cordf2",col.regions = colorRampPalette(c(
  "darkred", "blue")))
