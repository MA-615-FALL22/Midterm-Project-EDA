library(ggplot2)
library(dplyr)
library(tidyr)
library(data.table)
library("readxl")
library(magrittr)

#Data Cleaning
strawb <- read_xlsx("/Users/priamvyas/Desktop/MSSP/615 Data Science in R/MidTerm Project/EDA/strawberries-2022oct30-a.xlsx", col_names = TRUE)
df_2 <-  strawb[!grepl("(D)", strawb$Value),]
df_3 <-  df_2[!grepl("(NA)", df_2$Value),]
df_4 <-  df_3[!grepl(" (NA)", df_3$Value),]
names(df_4)[13] <- "CV"

## Get the column names and index them
cnames <- colnames(df_4)
x <- 1:dim(df_4)[2]

#Data Explore
## Set T as an indicator
T <- NULL

## Collect number of unique rows in each column
for(i in x){T <- c(T, dim(unique(df_4[i]))[1])}

drop_cols <- cnames[which(T == 1)]

strawb <- df_4 %>% select(!all_of(drop_cols))

strawb %<>% arrange(Year, State)

strawb %<>% separate(col=`Data Item`,
                     into = c("Strawberries", "type", "items", "units"),
                     sep = ",",
                     fill = "right")

r_thiram <- grep("THIRAM", strawb$Domain.Category)

r_thiram_1 <- grep("Thiram", 
                   strawb$Domain.Category, 
                   ignore.case = T)

## Chemicals mentioned in 
## the "Shoppers Guide to Pesticides in Produce"
## Carbendazim, Bifenthrin, methyl bromide, 1,3-dichloropropene,
## chloropicrin, Telone

df_carbendazim <- grep("carbendazim", 
                       strawb$`Domain Category`, ignore.case = T)

## Bifenthrin found 27
df_Bifenthrin <- grep("Bifenthrin", 
                      strawb$`Domain Category`, ignore.case = T)

## methyl bromide found 3
df_methyl_bromide <- grep("methyl bromide", 
                          strawb$`Domain Category`, ignore.case = T)

## 1,3-dichloropropene empty
df_1_3_dichloropropene <- grep("1,3-dichloropropene", 
                               strawb$`Domain Category`, 
                               ignore.case = T)

## chloropicrin found 18
df_chloropicrin <- grep("chloropicrin", 
                        strawb$`Domain Category`, 
                        ignore.case = T)

## Telone empty
df_Telone <- grep("Telone", 
                  strawb$`Domain Category`, 
                  ignore.case = T)

## We'll come back to chemicals after they 
## have their own column

## Let's continue cleaning up the columns

temp1 <- strawb %>% select(Strawberries) %>% 
  distinct()

pr_rec <- grep("STRAWBERRIES - PRICE RECEIVED", 
               strawb$Strawberries, 
               ignore.case = T)

type_organic <- grep("organic", 
                     strawb$type, 
                     ignore.case = T)

Domain_organic <- grep("organic", 
                       strawb$Domain, 
                       ignore.case = T)


Domain_Category_organic <- grep("organic", 
                                strawb$`Domain Category`, 
                                ignore.case = T)

same <- (intersect(type_organic, Domain_organic)==
           intersect(type_organic, Domain_organic))
length(same)==length(type_organic)


org_rows <- intersect(type_organic, Domain_organic)

strawb_organic <- strawb %>% slice(org_rows, preserve = FALSE)

strawb_non_organic <- strawb %>% filter(!row_number() %in% org_rows)


cali_2016_org <- strawb %>% filter(Year==2016 & State=="CALIFORNIA")
cali_2016_org_1 <- cali_2016_org[cali_2016_org$Domain %like% "ORGANIC", ]
cali_2016_org_2 <- cali_2016_org_1[cali_2016_org_1$units == "MEASURED IN $", ]

margin <- qt(0.975, 1)*(sd(c(1221571, 224886))/sqrt(2))
mean(c(1221571, 224886)) + margin
mean(c(1221571, 224886)) - margin


cali_2016_nonorg <- strawb_non_organic %>%
  filter(Year==2016 & State=="CALIFORNIA")

cali_2016_nonorg_1 <- cali_2016_nonorg[ -c(13) ]

cali_2016_nonorg_2 <- cali_2016_nonorg_1[complete.cases(cali_2016_nonorg_1), ]

cali_2016_nonorg_3 <- cali_2016_nonorg_2[cali_2016_nonorg_2$items ==" MEASURED IN LB / ACRE / YEAR", ]

cali_2016_nonorg_3$Value <- as.numeric(as.character(cali_2016_nonorg_3$Value))

n <- dim(cali_2016_nonorg_3)[1]

margin <- qt(0.975, df = n-1)*(sd(cali_2016_nonorg_3$Value)/sqrt(n))
margin

mean(cali_2016_nonorg_3$Value) + margin
mean(cali_2016_nonorg_3$Value) - margin

#write.csv(strawb,"/Users/priamvyas/Desktop/MSSP/615 Data Science in R/MidTerm Project/EDA/stawb_clean.csv", row.names = FALSE)
