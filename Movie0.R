library(corrplot)
install.packages("corrgram")
library(corrgram)

dataframe0<-read.csv("https://raw.githubusercontent.com/Messimx/BA-Project-1/master/Final%20Countries%20Dataset.csv", header=TRUE, stringsAsFactors = FALSE)
datatype<-as.data.frame(sapply(dataframe0,class))

#Filter dataset
dataframe1<-dataframe0[-1]

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
  norm.max<-max(abs(x),na.rm=TRUE)
  norm.result<-x/norm.max*100
}
dataframe2<-as.data.frame(sapply(dataframe1,norm.fun))
boxplot(dataframe2,outline = FALSE,color="green")
summary.dataframe2<-sapply(dataframe2,descriptive.summary)
cordf2<-as.data.frame(cor(dataframe2,use = "complete.obs"))
corrgram(cordf2, order = TRUE, upper.panel = panel.cor,main = "cordf2",col.regions = colorRampPalette(c(
  "darkred", "blue")))

#Assigning weights
weights_var<-dataframe2[1,]

summary_eco<-as.data.frame(summary.dataframe2["cv",c("GDP","Net.Imports","Net.Exports","Foreign.Direct.Investment")])
cv_eco<-sum(summary_eco)
weights_var$GDP<-(0.4*as.numeric(summary.dataframe2["cv","GDP"]))/cv_eco
weights_var$Net.Imports<-(0.4*as.numeric(summary.dataframe2["cv","Net.Imports"]))/cv_eco
weights_var$Net.Exports<-(0.4*as.numeric(summary.dataframe2["cv","Net.Exports"]))/cv_eco
weights_var$Foreign.Direct.Investment<-(0.4*as.numeric(summary.dataframe2["cv","Foreign.Direct.Investment"]))/cv_eco

summary_env<-as.data.frame(summary.dataframe2["cv",c("CO2.emissions...population","Renewable.Energy.Consumption")])
cv_env<-sum(summary_env)
weights_var$CO2.emissions...population<-(0.2*as.numeric(summary.dataframe2["cv","CO2.emissions...population"]))/cv_env
weights_var$Renewable.Energy.Consumption<-(0.2*as.numeric(summary.dataframe2["cv","Renewable.Energy.Consumption"]))/cv_env

weights_var$HDI<-0.2
weights_var$Central.Government.Debt<-0.2
weights_var[2:27,]<-weights_var

#Calculate the index value
dataframe2$Index<-rowSums(dataframe2*weights_var)
dataframe2$Name<-dataframe0$Country.Name

rank_Index<-dataframe2[order(dataframe2$Index,decreasing=TRUE),]
barplot(rank_Index$Index, col = "blue",names.arg=rank_Index$Name,las=2,
        main="Rank of Countries",ylab="Index Values")
