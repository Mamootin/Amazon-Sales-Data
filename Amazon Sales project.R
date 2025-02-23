#Amazon Sales data 
# Sales Project, Mahmood Nejati 
#this is my work upon R program class

#---------------Necessary Libraries---------------
library(ggplot2, dplyr)

#---------------Input Data Frame---------------
sales_data<-read.csv(choose.files())

str(sales_data)
summary(sales_data)
head(sales_data)

#---------------Data Cleaning---------------
sales_data==""                                    # Check for empty cells
sales_data[sales_data==""]<-NA                   #replace empty cells with "NA"
colSums(is.na(sales_data))                       #Check for  "NA" cells


#---------------Frequency Tables---------------

ft_ProductLine<-table(sales_data$PRODUCTLINE)    # R base package => table{base}
ft_ProductCode<-table(sales_data$PRODUCTCODE)    # R base package => table{base}

ft_ProductLine                                   #Product line frequency table
ft_ProductCode                                   #Product frequency table

#---------------Products Cumulative Percentage Frequency Tables---------------
ProductLine<-as.data.frame(ft_ProductLine)      #Make a Data Frame from Table

SumTotal_ProductLine<-sum(ProductLine$Freq)     #Calculate Total Number of Sales

ProductLine$Percent<-((ProductLine$Freq)/SumTotal_ProductLine)*100

OrderedProductLine<-ProductLine[order(-ProductLine$Percent),]

OrderedProductLine$CumPrc<-cumsum(OrderedProductLine$Freq)/(SumTotal_ProductLine)*100

names(OrderedProductLine)[1]<-"PrClass"         #change first row name

OrderedProductLine                             # Result


#(---------------Sales Frequency Tables---------------

productLine_sales<-group_by(sales_data, PRODUCTLINE)%>%
                    summarise(total_sales=sum(PRICEEACH)) 
  
  
productCode_sales<-group_by(sales_data, PRODUCTCODE) %>%
                   summarise(total_sales=sum(PRICEEACH))
  
  
print(productLine_sales, n=50)                    #Sales by Product Line
print(productCode_sales, n=50)                    #Sales by Product code

total_sum<-sum(productLine_sales$total_sales)
total_sum                                         #Total sales price


#(---------------Average Sales By Year and Month---------------

monthly_sales <- sales_data %>%
                  group_by(PRODUCTLINE, YEAR_ID, MONTH_ID) %>%
                  summarise(TotalSales=sum(SALES, na.rm=TRUE), .groups="drop") %>%
                  arrange(PRODUCTLINE, YEAR_ID, MONTH_ID)

print(average_sales, n=200)

as.data.frame(monthly_sales)

Products_sales<- monthly_sales %>%
                group_by(PRODUCTLINE) %>%
                summarise(AVG=mean(TotalSales))
Products_sales

mean(monthly_sales$TotalSales)

#---------------Hypotheses Test---------------

monthly_sales_test<-c(monthly_sales$TotalSales)
t_test<-t.test(monthly_sales_test, mu=5000, altenative="less")

t_test

#---------------Binomial dist---------------
calls<-15
prb<-0.3

dbinom(5, size=calls, prob=prb)  
more_than_7<- 1-dbinom(7, size=calls, prob=prb) 

print(more_than_7)

#---------------Normal Distribution---------------

normal_dist<-as.data.frame(rnorm(200, mean=100, sd=20))
normal_dist

pnorm(110, mean=100, sd=20) - pnorm(90, mean=100, sd=20)


#---------------Packed_Data---------------

packed_data<-read.table(choose.files())
hist(packed_data$Weight, breaks=8, main="weight frequency", xlab="weight", col="lightblue", border="black")






