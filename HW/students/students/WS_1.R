#DBO
#mycon<-odbcDriverConnect("Driver={ODBC Driver 13 for SQL Server};
#                         Server=tcp:smart-isv-server.database.windows.net,1433;
#                         Database=DBNAME;Uid=SERVERNAME; Pwd={PAsSWORD};
#                        Encrypt=yes;TrustServerCertificate=no;Connection Timeout=30;")
#t1<-sqlFetch(mycon, 'dbo.TableName')


#Goodle API
library(googleway)
#adr<-google_geocode(address,key=api_key,simplify = TRUE )

#File
library(readr)
setwd("C:/Users/anshc/OneDrive - SMART business/Academy/Lec_2/Lab/Lab02A")
admissions_mapping <- read_csv("C:/Users/anshc/OneDrive - SMART business/Academy/part_2/Lab/Lab02A!!!!/admissions_mapping.csv")
diabetic_data <- read_csv("C:/Users/anshc/OneDrive - SMART business/Academy/part_2/Lab/Lab02A!!!!/diabetic_data.csv")


data<-merge(x = diabetic_data, y = admissions_mapping, by = "admission_type_id", all.x = TRUE)



#Replace ? " "
data$weight[data$weight == "?"] <- ""
#Replace Male Female M F
data$gender<-ifelse(data$gender=='Male','M',
                   ifelse(data$gender=='Female','F',""))

#NA missing
data<-na.omit(data)
Column1[is.na(Column1)] <- round(mean(Column1, na.rm = TRUE))

#Outliers in the data 
v1 <- c(101.2, 90.0, 99.0, 102.0, 103.0, 100.2, 89.0, 98.1, 101.5, 102.0)

v1<-Chauvenet(v1, loop = FALSE)
#v vector of numeric values
#loop loop logical value; if TRUE, process will be repeated until no more values can be removed
up<- median(v1) + 1.5*IQR(v1); l<- median(v1) - 1.5*IQR(v1)
filter <- (v1<l)|(v1>up)
outliers <- v1[filter]

aggr$Sales=ifelse(aggr$x>(aggr$AVG+aggr$SD),aggr$AVG,aggr$x)
#Scaling of Data (normalization)
donormit <- function(x) {(x - min(x, na.rm=TRUE))/(max(x,na.rm=TRUE) -
                                                 min(x, na.rm=TRUE))}

diabetic_data_norm<-donormit(diabetic_data$num_lab_procedures)

#Deduplication
library(sqldf)
sqldf('SELECT DISTINCT * FROM df')

#Format revision 
library(readxl)
date_conv <- read_excel("C:/Users/anshc/OneDrive - SMART business/Academy/Lec_2/Lab/Lab02A/date_conv.xlsx")
library(lubridate)
date_conv$StartDate<-mdy(date_conv$StartDate,tz=Sys.timezone())
date_conv$StartDate<-format(date_conv$StartDate, format="%B-%Y")


#Filtering
date_conv_f<-subset.data.frame(date_conv,date_conv$StartDate %in% c('May-2016','June-2016'))


#Aggregation
date_conv_f_aggr<-aggregate(date_conv_f$Sales,list(Date=date_conv_f$StartDate),sum)

#Derivation
plan <- read_excel("C:/Users/anshc/OneDrive - SMART business/Academy/Lec_2/Lab/Lab02A/plan.xlsx")
plan$Date<-format(plan$Date, format="%B-%Y")

plan_fact<-merge(x = date_conv_f_aggr, y = plan, by = "Date", all.x = TRUE)

#Metadata
colnames(plan_fact)[2] <- "Fact"

library(dplyr)
plan_fact<-mutate(plan_fact, performance = plan_fact$Fact/plan_fact$Plan)

library(scales)

plan_fact$performance<-percent


#Splitting
str <- read_excel("C:/Users/anshc/OneDrive - SMART business/Academy/Lec_2/Lab/Lab02A/string.xlsx")
str$Number<-NULL
str$Number<-substring(str$String,6,7)
library(reshape)
str$Number2<-NULL
str = transform(str, String = colsplit(String, split = "-", names = c('City', 'Number')))


#sampling
library(base)

x <- c(1:100)
x_samp<-sample.int(x,20,replace = TRUE)
unique(x_samp)


#Exploring data
#Shapiro-Wilks normality test
#p-value <= 0.05
library(stats)

results<-shapiro.test(date_conv_f$Sales)
results$p.value<0.05

# check correl

cor(data$time_in_hospital, data$num_procedures,
    method = "pearson")

cov(data$time_in_hospital, data$num_procedures,
    method = "pearson")

summary(date_conv_f)

library(pastecs)

stat.desc(date_conv_f$Sales)

library(psych)
describe.by(mydata, group,...)


#Viz
library(ggplot2)
#https://www.rstudio.com/wp-content/uploads/2016/11/ggplot2-cheatsheet-2.1.pdf

hist(data$time_in_hospital)

head(iris)
ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width)) + 
  geom_point()


ggplot(plan_fact$performance, aes(x = plan_fact$Date)) +
  geom_histogram()


a=c(1:5)
b=c(5,3,4,5,5)
c=c(4,5,4,3,1)

plot( b~a , type="b" , bty="l" , xlab="value of a" , ylab="value of b" , col=rgb(0.2,0.4,0.1,0.7) , lwd=3 , pch=17 , ylim=c(1,5) )
lines(c ~a , col=rgb(0.8,0.4,0.1,0.7) , lwd=3 , pch=19 , type="b" )


# Add a legend
legend("bottomleft", 
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




#csv
write.csv(plan_fact, file="plan_fact.csv")

#sql
toSQL = data.frame(...);
write.table(toSQL,"C:\\export\\filename.txt",quote=FALSE,sep=",",row.names=FALSE,col.names=FALSE,append=FALSE);
sqlQuery(channel,"BULK
         INSERT Yada.dbo.yada
         FROM '\\\\<server-that-SQL-server-can-see>\\export\\filename.txt'
         WITH
         (
         FIELDTERMINATOR = ',',
         ROWTERMINATOR = '\\n'
         )")

