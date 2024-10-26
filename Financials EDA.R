#Company Financials Dataset - to obtain a cleaned dataset with EDA insights
#Drag data
setwd("/Users/cuiying/Desktop/Data")
data <- read.csv('Financials.csv')
data.backup <- data
str(data)
summary(data)

#check for missing value
sum(is.na(data))  #0 missing

#replace characters
data$Units.Sold <- gsub("[\\$,]","",data$Units.Sold)
data$Manufacturing.Price <- gsub("[\\$,]","",data$Manufacturing.Price)
data$Sale.Price <- gsub("[\\$,]","",data$Sale.Price)
data$Gross.Sales <- gsub("[\\$,]","",data$Gross.Sales)
data$Discounts <- gsub("[\\$-,]","",data$Discounts)
data$Discounts <- gsub("-","0",data$Discounts)
data$Sales <- gsub("[\\$,]","",data$Sales)
data$COGS <- gsub("[\\$,]","",data$COGS)
data$Profit <- gsub("[\\$,]","",data$Profit)

#converting data type
data$Manufacturing.Price <- as.numeric(data$Manufacturing.Price)
data$Sale.Price <- as.numeric(data$Sale.Price)
data$Gross.Sales <- as.numeric(data$Gross.Sales)
data$Discounts <- as.numeric(data$Discounts)
data$Sales <- as.numeric(data$Sales)
data$COGS <- as.numeric(data$COGS)
data$Profit <- as.numeric(data$Profit)
data$Date <- as.Date(data$Date, format = "%d/%m/%Y")
data$Discount.Band <- factor(data$Discount.Band)
data$Units.Sold <- as.numeric(data$Units.Sold)

#date checking
months <- format(data$Date,"%m")

str(data)
sum(is.na(data))  #63 missing

#extract records with NAs
data[!complete.cases(data),]

#deriving Profit
data[is.na(data$Profit),"Profit"] <- data[is.na(data$Profit),"Sales"] - data[is.na(data$Profit),"COGS"]

sum(is.na(data))  #0 missing

#cleaned output
write.csv(data, file = "CleanedFinancials.csv", row.names = FALSE)

#visualisation 
library(ggplot2)
library(dplyr)
library(scales)
#filter the data
Data2014 <- data[data$Date >= '2014-01-01',]
Data2013 <- data[data$Date < '2014-01-01',]
sum(is.na(Data2014)) #0 missing
str(Data2014)

ggplot(data=Data2014, aes(x=Date,y=Profit)) +
  geom_bar(stat="identity")

ggplot(data=Data2014, aes(x=Date, y=mean(Profit))) +
  geom_bar(stat="identity")

ggplot(data=Data2014, aes(x=Date, y=Sales)) +
  geom_bar(stat="identity")

ggplot(data=Data2014, aes(x=Date, y=mean(Sales))) +
  geom_bar(stat="identity")
# 6,10,12 high profits and sales

#####################factors that may effect profit and sales

#H0: there is no relationship between discounts and profit
#H1: there is a relationship between discounts and profit
a <- Data2014$Discounts
b <- Data2014$Profit
t_test_result1 <- t.test(a,b)
t_test_result1
p_value1 <- t_test_result1$p.value
#reject H0

#H0: there is no relationship between discounts and sales
#H1: there is a relationship between discounts and sales
c <- Data2014$Sales
t_test_result2 <- t.test(a,c)
t_test_result2
p_value2 <- t_test_result2$p.value
#reject H0

rm(a, b, c, t_test_result1, t_test_result2)
rm(p_value, p_value1, p_value2)

#Segment > Profit
char_vector1 <- Data2014$Segment
dummyvar1 <- as.numeric(factor(char_vector1))
a <- dummyvar1
b <- Data2014$Profit
t_test_result1 <- t.test(a,b)
t_test_result1
p_value1 <- t_test_result1$p.value

#Segment > Sales
t_test_result2 <- t.test(a,c)
t_test_result2
p_value2 <- t_test_result2$p.value

#Product > Profit
char_vector2 <- Data2014$Product
dummyvar2 <- as.numeric(factor(char_vector2))
d <- dummyvar2
t_test_result3 <- t.test(d,b)







