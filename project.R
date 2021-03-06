#install.packages("readr")
library('readr')
setwd("C:/workshop/workshop1")
autos_m<-read_csv("Data/autos_m.csv")
makes_m<-read_csv("Data/makes_m.csv")
data <- merge(x=autos_m, y=makes_m, by='make-id', all.x=TRUE) #left join

data$`engine-type`[data$`engine-type`=='l'] <- ""
#data$`engine-type`==''
#View(data[data$`engine-type`=="",])
#View(data[data$`engine-type`!="","engine-type"])

unique(data$`num-of-doors`)
#View(data[is.na(data$`num-of-doors`), "num-of-doors"])
data$`num-of-doors` <- ifelse(data$`num-of-doors`=="four", 4, ifelse(data$`num-of-doors`=="two", 2, NA) )

#data<-na.omit(data) - delete all NA's
data$`normalized-losses`[is.na(data$`normalized-losses`)] <- round(mean(data$`normalized-losses`, na.rm = TRUE))
#View(data$`normalized-losses`)

v1<-c(101.2, 320.1, 323.0, 412.2, 23.23, 253.9)



#--------------------------------------------------------------------------------------
#outliers

Chauvenet <- function(datapoints, loop=FALSE){
  numdatapoints <- nrow(datapoints)
  # calculate normalized distance to mean
  dist <- abs(datapoints - colMeans(datapoints))/sapply(datapoints,sd)
  # calculate probability to have seen such a point assuming a normal
  # distribution
  prob <- apply(dist,c(1,2),function(x) numdatapoints*dnorm(x))
  # select only those points which have a probability >= 0.5
  sel <-  apply(prob,c(1,2),function(x) x>=0.5)
  idx <- rowSums(sel) == ncol(datapoints)
  datapoints <- datapoints[idx,]
  if(loop == TRUE){
    while(FALSE %in% idx){
      numdatapoints <- nrow(datapoints)
      dist <- abs(datapoints - colMeans(datapoints))/sapply(datapoints,sd)
      prob <- apply(dist,c(1,2),function(x) numdatapoints*dnorm(x))
      sel <-  apply(prob,c(1,2),function(x) x>=0.5)
      idx <- rowSums(sel) == ncol(datapoints)
      datapoints <- datapoints[idx,]
    }
  }
  
  return(datapoints)
}

#View(Chauvenet(t(data$`normalized-losses`)))

#
up<-median(v1)+1.5*IQR(v1); low<-median(v1)-1.5*IQR(v1)
filter<-(v1<low) | (v1>up)
outliers <- v1[filter]


#--------------------------------------------------------------------------------------
#data normalization

donormit <-function(x) {(x-min(x,na.rm = TRUE))/(max(x,na.rm = TRUE)-min(x, na.rm = TRUE))}

data$price_norm <- donormit(data$price)


#--------------------------------------------------------------------------------------
#work with SQL querys
library("sqldf")
View(sqldf('SELECT DISTINCT * FROM data'))

#equivalent of the above
View(unique(data))

data <- unique(data)


#--------------------------------------------------------------------------------------
library(readxl)
date_conv <- read_excel("Data/date_conv.xlsx")
head(date_conv)

library(lubridate)
date_conv$StartDate <- mdy(date_conv$StartDate, tz = Sys.timezone())
date_conv$StartDate <- format(date_conv$StartDate, format="%B-%Y")



#--------------------------------------------------------------------------------------
##filter data

#-------------------
##choose subsets
date_conv_f <- subset.data.frame(date_conv, date_conv$StartDate %in% c("May-2016", "June-2016"))
unique(date_conv_f$StartDate)

#-------------------
##aggregation (==group by) "sum(Sales) group by LIST"
date_conv_f_aggregate <-aggregate(date_conv_f$Sales, list(Date=date_conv_f$StartDate), sum)
date_conv_f_aggregate_mean <-aggregate(date_conv_f$Sales, list(Date=date_conv_f$StartDate), mean)
colnames(date_conv_f_aggregate_mean)[2] <- "Mean"

#-------------------
##derivation
plan <- read_excel("C:/workshop/workshop1/Data/plan.xlsx")
#summary(plan)
plan$Date<-format(plan$Date, format="%B-%Y")
plan_fact<-merge(x = date_conv_f_aggregate, y = plan, by = "Date", all.x = TRUE)

#-------------------
##metadata
colnames(plan_fact)[2] <- "Fact"


#--------------------------------------------------------------------------------------
library(dplyr)
#adds new variables and preserves existing
plan_fact<-mutate(plan_fact, performance = plan_fact$Fact/plan_fact$Plan)

library(scales)
#tranfromation of "performance" columnt to percent
plan_fact$performance<-percent(plan_fact$performance)

#--------------------------------------------------------------------------------------
#Splitting
str <- read_excel("C:/workshop/workshop1/Data/string.xlsx")
str$Number<-NULL
#extract number - positions 6 & 7 in the String column (or by using reg exp)
str$Number<-substring(str$String,6,7)

library(reshape)
str$Number<-NULL
#split our String into String.City & String.Number
str = transform(str, String = colsplit(String, split = "-", names = c('City', 'Number')))
summary(str)

#--------------------------------------------------------------------------------------
#Sampling
library(base)
x <- c(1:100)
x_samp<-sample(x,20,replace = TRUE)
x_samp_int<-sample.int(x,20,replace = TRUE)
unique(x_samp)

#--------------------------------------------------------------------------------------
#Exploring data
#Shapiro-Wilks normality test
#p-value <= 0.05
library(stats)

results<-shapiro.test(date_conv_f$Sales)
View(results)
results$p.value<0.05

#--------------------------------------------------------------------------------------
#Correlation check
View(data)
cor(data$`curb-weight`, data$`engine-size`,
    method = "pearson")

cov(data$`curb-weight`, data$'engine-size',
    method = "pearson")

#--------------------------------------------------------------------------------------
summary(date_conv_f)

library(pastecs)
#Descriptive statistics on a data frame or time series
stat.desc(date_conv_f$Sales)

library(psych)
describe.by(plan_fact, plan_fact$Date)

#--------------------------------------------------------------------------------------
#Vizualisation
library(ggplot2)
#https://www.rstudio.com/wp-content/uploads/2016/11/ggplot2-cheatsheet-2.1.pdf

hist(data$price_norm)

head(iris)
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width)) + 
  geom_point()


ggplot(iris, aes(x = Sepal.Length)) +
  geom_histogram(bins=15)


a=c(1:5)
b=c(5,3,4,5,5)
c=c(4,5,4,3,1)

plot( b~a, type="h" , bty="l" , xlab="value of a" , ylab="value of b" , col=rgb(0.2,0.4,0.1,0.7) , lwd=3 , pch=17 , ylim=c(1,5) )
plot( a , b, type="b" , bty="l" , xlab="value of a" , ylab="value of b" , col=rgb(0.2,0.4,0.1,0.7) , lwd=3 , pch=17 , ylim=c(1,5) )
lines(c ~a , col=rgb(0.8,0.4,0.1,0.7) , lwd=3 , pch=19 , type="b" )

#trying to define outliers
data_gr_by_price <- sqldf('select price, count(*) from data group by price;')
x = t(data_gr_by_price$price)[1,]
y = t(data_gr_by_price$`count(*)`)[1,]
plot( y~x, type="h" , bty="l" , xlab="value of a" , ylab="value of b" , col=rgb(0.2,0.4,0.1,0.7) , lwd=3 , pch=17)
#


# Add a legend
legend("topright", 
       legend = c("Group 1", "Group 2"), 
       col = c(rgb(0.2,0.4,0.1,0.7), 
               rgb(0.8,0.4,0.1,0.7)), 
       pch = c(17,19), 
       bty = "n", 
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))

#--------------------------------------------------------------------------------------
#Save to file

#csv
write.csv(plan_fact, file="plan_fact.csv")

#sql
toSQL = data.frame(data);
write.table(toSQL,"C:/workshop/workshop1/filename.txt",quote=FALSE,sep=",",row.names=FALSE,col.names=FALSE,append=FALSE);

library(RODBC)

#sqlQuery(channel,"BULK
#         INSERT Yada.dbo.yada
#         FROM '\\\\<server-that-SQL-server-can-see>\\export\\filename.txt'
#         WITH
#         (
#         FIELDTERMINATOR = ',',
#         ROWTERMINATOR = '\\n'
#         )")

                                  
