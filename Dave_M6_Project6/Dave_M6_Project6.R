#Install packages

install.packages('FSA')
install.packages('FSAdata')
install.packages('magrittr')
install.packages('dplyr')
install.packages('tidyr')
install.packages('plyr')
install.packages('tidyverse')
install.packages("janitor")

#Calling libraries
library(FSA)
library(FSAdata)
library(magrittr)
library(dplyr)
library(tidyr)
library(plyr)
library(tidyverse)
library(janitor)
library(ggplot2)

#Importing dataset
bank<-read.csv("banks.csv", na.strings = ".")

#data cleaning
def<-na.omit(bank)
View(def)

#Aanalysis
summary(def)

new<-data.frame(sapply(def,unclass))
headtail(new)
str(new)
colnames(new)[colnames(new) == "Institution.Type"]<-"iType"     #chaninging column name
colnames(new)[colnames(new) == "Institution.Name"]<-"iName"     #changing column name
colnames(new)[colnames(new) == "Charter.Type"]<-"ct"            #changing column name
colnames(new)[colnames(new) == "Failure.Date"]<-"fd"            #changing column name
colnames(new)[colnames(new) == "Estimated.Loss..2015."]<-"loss" #changing column name

#Creating objects of the coulmn which is grouped
ctype<- table(new$iType)
cname<-table(new$iName)
cct<-table(new$ct)


#subsetting the useful data columns only
mydata<- subset(new,select=c("iName","iType","ct","fd","loss"))

#Analysis
cor(mydata)
summary(mydata)
cor(mydata[c("iType","fd")])


library(dplyr)
library(purr)

#Creating Visulaisation

lloss <- summarise_at(group_by(mydata,iType),vars(loss),funs(sum(.,na.rm=TRUE)))
lloss
ggplot(lloss, aes(iType, loss, fill = loss)) + geom_col(position = "dodge")+         #Plotting Bar graph
  ggtitle("Institution Type making Loss") +
      xlab("Institution Type")+ylab("Loss")
  
subset1<- subset(mydata, ct==1 )                                                    #Subsetting data with respect to chartertype
lloss2 <- summarise_at(group_by(subset1,iName),vars(loss),funs(sum(.,na.rm=TRUE)))  #Summarizing
d <- lloss2[order(-lloss2$loss),]                                                   #Sorting the dataframe of lloss2 in descending order
headd<-head(d,10)                                                                   #Top 10 loss making banks
headd

ggplot(headd, aes(iName, loss, fill = loss,xlab(InsttituteName),ylab(Loss))) + geom_col(position = "dodge")   #plotting bar graph

piepercent<-round(100*(headd$loss)/sum(headd$loss),1)                               #Creating an object for future reference to use in Pie chart to depict percentage in Graph

pie(headd$loss, labels=piepercent,main = "Top 10 Loss making  Banks")                #Plotting pie chart

    