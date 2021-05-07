library(dplyr)
library(ggplot2)
library(tidyr)
library(shiny)
library(plotly)
library(corrr)
library(treemap)


superstore <- read.csv("C:\\Users\\ADMIN\\Downloads\\SampleSuperstore.csv")
head(superstore)
tail(superstore)

summary(superstore)

#To check if there are any null values
is.null(superstore)

#To check if there is any duplicacy and remove them too along with removing two columns (postal codes and country)
#as I do not require them for further analysis.
data <- superstore %>% 
         distinct() %>%
         select(-c(Country, Postal.Code))
data

x <- data %>%select(Sales, Quantity, Discount, Profit)
corr_var <- correlate(x, method = 'pearson',use = "pairwise.complete.obs", diagonal = 1)
corr_var

y<- data %>%select(Sales, Quantity, Discount, Profit)
cov_var <- cov(y)
cov_var

summary(data$Sales)

 statewise_sales <- data %>% 
                  group_by(State) %>%
                  summarise(total_sales = sum(Sales)) %>%
                  arrange(desc(total_sales))
statewise_sales

ggplot(data, aes( x= State, y= Sales, fill= State),options(scipen=999)) + 
          geom_col()+
          ggtitle("Statewise Sales Analysis") + 
          coord_flip() + 
        theme(legend.position = "None", axis.text.y = element_text(size=6))

regionwise_sales = data %>% 
        group_by(Region) %>%
       summarize(totalS= sum(Sales)) %>%
       arrange(desc(totalS))
regionwise_sales

ggplot(data, aes( x= Region, y= Sales, fill= Region),options(scipen=99)) + 
          geom_col()+
          ggtitle("Regionwise Sales Analysis") + 
        theme(legend.position = "None", axis.text.y = element_text(size=6))

Statewise_profit = data%>% 
          group_by(State)%>% 
          summarise(totalP= sum(Profit))%>% 
           arrange(desc(totalP))
Statewise_profit

ggplot(data, aes( x= State, y= Profit, fill= State),options(scipen=99)) + 
          geom_col()+
          ggtitle("Statewise Profit Analysis") + 
          coord_flip() +
          theme(legend.position = "None", axis.text.y = element_text(size=6))

regionwise_profit = data %>% 
        group_by(Region) %>%
       summarize(totalP= sum(Profit)) %>%
       arrange(desc(totalP))
regionwise_sales

ggplot(data, aes( x= Region, y= Profit, fill= Region),options(scipen=99)) + 
          geom_col()+
          ggtitle("Regionwise Profit Analysis") + 
        theme(legend.position = "None", axis.text.y = element_text(size=6))

BarPlot = data %>% 
        group_by(State) %>%
       summarize(sales_profit_ratio= sum(Profit)/sum(Sales)) %>%
       arrange(desc(sales_profit_ratio))
BarPlot

ggplot(BarPlot, aes( x= sales_profit_ratio, y= State, fill= State),options(scipen=99)) + 
          geom_col()+
          ggtitle("Statewise Sales-Profit Ratio Analysis ") + 
        theme(legend.position = "None", axis.text.y = element_text(size=6))

Segment_analysis = data %>% 
        group_by(Segment) %>%
        summarize(ratio= sum(Profit)/sum(Sales)) %>%
        arrange(desc(ratio)) %>%
        ggplot( aes( x= Segment, y= ratio, fill= Segment),options(scipen=99)) + 
        geom_col()+
        ggtitle("Profit-Sales Ratio analysis for each Segment ")
Segment_analysis

category_s = data %>% 
                  group_by(Category) %>%
                  summarize(Sales=sum(Sales))
pct <- round((category_s$Sales/sum(category_s$Sales))*100)
lbls <- paste(category_s$Category , pct)
lbls <- paste(lbls, "%", sep = " ")
pie(category_s$Sales, labels = lbls, main =" Percentage sales by Category ", col= c('darkmagenta','plum','plum4'))

treemap(data, index = c("Category","Sub.Category"),title='Sales treemap for categories', vSize = "Sales",vColor ="Profit", type= "value",palette = "RdYlGn", range=c(-20000,60000),mapping= c(-20000,10000,60000))

Price_per_product = ggplot(data, aes( x= Sub.Category, y=sum(Sales)/sum(Quantity), fill= Sub.Category),options(scipen=99)) + 
          geom_col()+
          ggtitle("Price per product") + 
          coord_flip() + 
        theme(legend.position = "None", axis.text.x = element_text(size=6))
Price_per_product

profit_per_product = ggplot(data, aes( x= Sub.Category, y=sum(Profit)/sum(Quantity), fill= Sub.Category),options(scipen=99)) + 
          geom_col()+
          ggtitle("Profit per product") +  
          coord_flip()+
        theme(legend.position = "None", axis.text.x = element_text(size=6))
profit_per_product

ggplot(data, aes(x = Quantity, y = Sales, fill = Ship.Mode),options(scipen=99) )+ geom_bar(stat = "identity")

ggplot(data, aes( x=Ship.Mode, y= Profit, fill= Ship.Mode),options(scipen=99)) + 
          geom_col()+
          ggtitle("Profit by Shipment mode and Segment") + 
        theme(legend.position = "None", axis.text.x = element_text(angle = 70 ,size=6)) +
         facet_wrap(~Segment)

Sales_with_discount = data %>%
                      filter(Discount != 0) %>%
                      summarize(totals=sum(Sales))
Sales_with_discount

Sales_without_discount = data %>%
                      filter(Discount == 0) %>%
                      summarize(totals=sum(Sales))
Sales_without_discount

profit_with_discount = data %>%
                      filter(Discount != 0) %>%
                      summarize(totalp=sum(Profit))
profit_with_discount

profit_without_discount = data %>%
                      filter(Discount == 0) %>%
                      summarize(totalp=sum(Profit))
profit_without_discount
