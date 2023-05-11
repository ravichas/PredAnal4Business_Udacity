library(readxl)
p1_customers <- read_excel("Documents/Udemy/Nanodegree-SQL-Python-VC/PredAnalyticsForBusiness/PredCatalogDemand/p1-customers.xlsx")
View(p1_customers)  

p1_mailinglist <- read_excel("Documents/Udemy/Nanodegree-SQL-Python-VC/PredAnalyticsForBusiness/PredCatalogDemand/p1-mailinglist.xlsx")


colnames(p1_customers)
colnames(p1_mailinglist)

#clean the colnames 
library(tidyverse)
p1_customers <- p1_customers %>% 
  rename(Num_Years_as_Customer = "#_Years_as_Customer")

p1_mailinglist <- p1_mailinglist %>% 
  rename(Num_Years_as_Customer = "#_Years_as_Customer")
library(fastDummies)

p1_customers <- dummy_cols(p1_customers, select_columns = "Customer_Segment", remove_first_dummy = TRUE,
                 remove_selected_columns = TRUE)

p1_mailinglist <- dummy_cols(p1_mailinglist, select_columns = "Customer_Segment", remove_first_dummy = TRUE,
                            remove_selected_columns = TRUE)

# p1_customers <- dummy_cols(p1_customers, select_columns = "Customer_Segment", 
#                            remove_selected_columns = TRUE)
# 
# p1_mailinglist <- dummy_cols(p1_mailinglist, select_columns = "Customer_Segment", 
#                              remove_selected_columns = TRUE)

p1_customers <- p1_customers %>% 
  rename(CS_LC_and_CC = "Customer_Segment_Loyalty Club and Credit Card",
         CS_LC_Only = "Customer_Segment_Loyalty Club Only",
         CS_SML = "Customer_Segment_Store Mailing List")
#         CS_CC_Only = "Customer_Segment_Credit Card Only")

p1_mailinglist <- p1_mailinglist %>% 
  rename(CS_LC_and_CC = "Customer_Segment_Loyalty Club and Credit Card",
         CS_LC_Only = "Customer_Segment_Loyalty Club Only",
         CS_SML = "Customer_Segment_Store Mailing List")
#         CS_CC_Only = "Customer_Segment_Credit Card Only")


model <- lm(data = p1_customers, Avg_Sale_Amount ~ Avg_Num_Products_Purchased + 
#              Num_Years_as_Customer +
              CS_LC_and_CC + 
              CS_LC_Only + 
              CS_SML)
summary(model)
p1_mailinglist$Predicted_Average_Sale <- predict(model, p1_mailinglist)
p1_mailing_list <- p1_mailinglist %>% mutate(Avg_Sale = Predicted_Average_Sale * Score_Yes)

p1_mailing_list %>% select(Avg_Sale) %>% select(Avg_Sale) %>% summarise(Avg_Sale_Sum = (sum(Avg_Sale) * 0.5 - 1625.0))

b <- ggplot(p1_customers, aes(Num_Years_as_Customer, Avg_Sale_Amount)) 
b + geom_point() + 
  geom_smooth(method="lm") +
  ggthemes::theme_economist() +
  ggtitle("Num. of Years as Customer vs Avg Sale Amount")

b <- ggplot(p1_customers, aes(Avg_Num_Products_Purchased, Avg_Sale_Amount)) 
b + geom_point() + 
  geom_smooth(method="lm") +
  ggthemes::theme_economist() +
  ggtitle("<Num Products purchased> vs Avg Sale Amount")

ggplot(p1_customers, aes(CS_LC_and_CC, Avg_Sale_Amount)) +
  geom_point() + 
  geom_smooth(method="lm") +
  ggthemes::theme_economist() +
  ggtitle("CS_LC_and_CC = Customer_Segment_Loyalty\n Club and Credit Card")


ggplot(p1_customers, aes(CS_LC_Only, Avg_Sale_Amount)) +
  geom_point() + 
  geom_smooth(method="lm") +
  ggthemes::theme_economist() +
  ggtitle("CS_LC_Only = Customer_Segment_Loyalty Club Only")


ggplot(p1_customers, aes(CS_SML, Avg_Sale_Amount)) +
  geom_point() + 
  geom_smooth(method="lm") +
  ggthemes::theme_economist() +
  ggtitle("CS_SML = Customer_Segment_Store Mailing List")
