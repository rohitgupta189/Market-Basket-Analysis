#Market Basket Analysis
#Publicly available dataset - grocery

#Loading the required packages
library(ggplot2) 
install.packages("arules")
library(arules)
install.packages("arulesViz")
library(arulesViz)
install.packages("plyr")
library(plyr) 


#Set working directory
setwd("C:/Users/rohit/Desktop/Spring16/Market Basket Analysis")

# Import data
data_raw <- read.csv(file="groceries.csv",sep=",")
View(data_raw)


# Extracting the various types of products in the data
product_names <- levels(unlist(data_raw[,1:15])) # Identify all unique products


# Identify the products asked inside the product_names list
citrus <- which(product_names == "citrus fruit")
tropical <- which(product_names == "tropical fruit")  
milk <- which(product_names == "whole milk") 
other_vegetables <- which(product_names == "other vegetables") 
rolls_buns <- which(product_names == "rolls/buns")  
chocolate <- which(product_names == "chocolate")  
water <- which(product_names == "bottled water") 
yogurt <- which(product_names == "yogurt")  
sausage <- which(product_names == "sausage") 
root_vegetables <- which(product_names == "root vegetables")  
pastry <- which(product_names == "pastry")  
soda <- which(product_names == "soda") 
cream <- which(product_names == "cream") 


# Define products vector for the asked products ( products under consideration)
product_vector <- c(citrus,tropical, milk, other_vegetables, rolls_buns, chocolate, water, yogurt, sausage, root_vegetables, pastry, soda, cream)

# Keep only the product names needed
product_names <- product_names[product_vector]

#c(1,2,3) %in% c(3,4,5,8)
# Create the binary table without headers or extra information
products <- as.data.frame(t(apply(data_raw[,1:15],1,  function(x) # For each transcation line of data_raw
  (product_names) %in% as.character(unlist(x))))) 
# %in% returns a vector as long as product_names (left) with TRUE or FALSE 
# depending on the existance of the product in the specified transaction line

write.csv(products,'C:/Users/rohit/Desktop/Spring16/Market Basket Analysis/products.csv')

names(products) <- product_names # Define the headers

data_transactional = as(products, "transactions")

# APRIORI - To determine association rules


#Generating association rules
rules <- apriori(data_transactional,parameter = list(minlen=3, maxlen=5, supp=0.02, conf=0.2))

#Sorting the rules as per their support
# Support - How often a pattern occurs in the data

rules.sorted <- sort(rules, by="support")
inspect(rules.sorted)

#Plotting parallel co-ordinates plot 
plot(rules.sorted, method="paracoord", control=list(reorder=TRUE))


rules.sorted <- sort(rules, by="lift")
inspect(rules.sorted)

#Graph for 6 rules
plot(rules.sorted, method="graph", control=list(type="items"))


rules.sorted <- sort(rules, by="support")
#Group Matrix
plot(rules.sorted, method="grouped")





