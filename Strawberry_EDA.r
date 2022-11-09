library(ggplot2)
library(dplyr)
library(tidyr)
library(data.table)
library("readxl")
library(magrittr)

strawb <- read.csv("/Users/priamvyas/Desktop/MSSP/615 Data Science in R/MidTerm Project/EDA/stawb_clean.csv")

## EDA Work
strawb$Value <- as.numeric(as.character(strawb$Value))

# Sum of All Sales
aggregate(strawb$Value, by=list(strawb$Domain, strawb$units), FUN=sum)

# Sum of All Organic Sales meaasure in $
organic_strawb_sales <- strawb[(strawb$items == ' FRESH MARKET - SALES') & (strawb$units == ' MEASURED IN $'), ]
strawb_sales <- organic_strawb_sales[complete.cases(organic_strawb_sales), ]
total_organic_strawb_sales_dollar <- sum(strawb_sales$Value)
total_organic_strawb_sales_dollar

# Sum of All Organic Sales meaasure in CWT
organic_strawb_sales <- strawb[(strawb$items == ' FRESH MARKET - SALES') & (strawb$units == ' MEASURED IN CWT'), ]
strawb_sales <- organic_strawb_sales[complete.cases(organic_strawb_sales), ]
total_organic_strawb_sales_CWT <- sum(strawb_sales$Value)
total_organic_strawb_sales_CWT



tibble(total_organic_strawb_sales_dollar, total_organic_strawb_sales_CWT) %>% 
   pivot_longer(everything()) %>%
  ggplot(aes(x = name, y = value)) + 
      geom_col()

# Statewise, Domain, units sales sum
t1 <- aggregate(strawb$Value, by=list(strawb$State, strawb$Domain, strawb$units), FUN=sum) 
names(t1)[names(t1) == "Group.1"] <- "State"
names(t1)[names(t1) == "Group.2"] <- "Domain"
names(t1)[names(t1) == "Group.3"] <- "Units"
names(t1)[names(t1) == "x"] <- "Value"
t1[order(-t1$Value),]

#Organic strawberries have the higher sales compared to inorganic strawberries

t2 <- t1[(t1$Domain == 'ORGANIC STATUS'), ]
t2

ggplot(t2, aes(x = State, y = Value))+
     geom_col(aes(fill = Units))

#California overpowers the rest of the states in terms of organic strawberry production sales in $

View(head(strawb, 5))

strawb_non_organic_lbs <- strawb[(strawb['items'] == " MEASURED IN LB"), ]
names(strawb_non_organic_lbs)[names(strawb_non_organic_lbs) == 'CV....'] <- "CV"
strawb_non_organic_lbs <- strawb_non_organic_lbs[ , -which(names(strawb_non_organic_lbs) %in% c("units","CV"))]
strawb_non_organic_lbs <- strawb_non_organic_lbs[complete.cases(strawb_non_organic_lbs), ]

k1 <- aggregate(strawb_non_organic_lbs$Value, by=list(strawb_non_organic_lbs$Domain), FUN=sum)
names(k1)[names(k1) == "Group.1"] <- "Chemical"
names(k1)[names(k1) == "x"] <- "Value"
k1


ggplot(k1, aes(x = Chemical, y = Value))+
     geom_col(aes(fill = Value)) +
    theme(axis.text.x=element_text(size=rel(2), angle=90))

#California overpowers the rest of the states in terms of organic strawberry production sales in $






