rm(list=ls())
setwd("C:/Users/mdsha/OneDrive/Documents/R/win-library/3.3/readr/instacart_data")
#load libraries
library(data.table)
library(dplyr)
library(tidyr)


# Load Data 

aisles = read.csv("aisles.csv",header=T)
departments = read.csv("departments.csv",header=T)

order_prior = read.csv("order_products__prior.csv",header=T)
order_prior = order_prior[sample(nrow(order_prior),10000000,replace=F),]

order_train = read.csv("order_products__train.csv",header=T)
order_train = order_train[sample(nrow(order_train),1000000,replace=F),]

orders = read.csv("orders.csv",header=T)
orders = orders[sample(nrow(orders),2000000,replace=F),]

products = read.csv("products.csv",header=T)
#products=products[sample(nrow(products),5000,replace=F),]


#joining products and aisles and then  department with the result 
products_new = inner_join(inner_join(products,aisles),departments) %>% select(-aisle_id, -department_id)
rm(aisles, departments)

#fetching only those order_id's which are common to both order_train and orders dataset
order_train$user_id = orders$user_id[match(order_train$order_id, orders$order_id)]

#merging orders and order_prior by "order_id" into order_detail
order_detail = inner_join(orders,order_prior, by = "order_id")

#removing to get some memory free
rm(order_prior)



# Products dataset
# summarizing the data of order_detail into new table product_detail
product_detail = arrange(order_detail,user_id, order_number, product_id) %>%
  group_by(user_id, product_id) %>%
  mutate(product_occurance= n()) %>%
  ungroup() %>%
  group_by(product_id) %>%
  summarise(
          product_1st_order = sum(product_occurance == 1),
          product_2nd_order = sum(product_occurance == 2),
          tot_prod_orders = n(),
          tot_prod_reorders = sum(reordered)
  )
# calculating probablity and the ratio for reordered
product_detail$prod_reorder_probability = product_detail$product_2nd_order / product_detail$product_1st_order
product_detail$prod_reorder_count = 1 + product_detail$tot_prod_reorders / product_detail$product_1st_order
product_detail$prod_reorder_ratio = product_detail$tot_prod_reorders / product_detail$ tot_prod_orders

product_detail = select(product_detail, -tot_prod_reorders, -product_1st_order, -product_2nd_order)

rm(products)


# Users dataset
# summarizing orders dataset(by evaluating maximum,summation and mean for the mentioned columns)
users_summary = filter(orders,eval_set == "prior") %>%
  group_by(user_id) %>%
  summarise(
    max_user_orders = max(order_number),
    user_period_of_order = sum(days_since_prior_order, na.rm = T),
    user_mean_days_since_prior = mean(days_since_prior_order, na.rm = T)
  )

user_group = group_by(order_detail,user_id) %>%
  summarise(
    user_total_products = n(),
    user_reordered_ratio = sum(reordered == 1) / sum(order_number > 1),
    users_products_types = n_distinct(product_id)
  )

#joining users_summary and user_group
users_summary =   inner_join(users_summary,user_group)
#creating average column of user's cart
users_summary$user_average_bucket = users_summary$user_total_products / users_summary$max_user_orders

user_group =  filter(orders,eval_set != "prior") %>%
  select(user_id, order_id, eval_set,
         duration_since_last_order = days_since_prior_order)
#joining users_summary and user_group
users_summary = inner_join(users_summary,user_group)

rm(user_group)



# final dataset
final_data = group_by(order_detail,user_id, product_id) %>% 
  summarise(
    last_order_grow = max(order_number), #maximun order for a particular user-id and product-id
    first_order_grow = min(order_number),#minimum order value
    orders_grow = n(),                   #total order place for a product
    average_cart_position_grow = mean(add_to_cart_order)) #average size of a user's cart

rm(order_detail, orders)

#first join final data with product_detail then then obtained result with users_summary
final_data = inner_join(final_data,product_detail, by = "product_id") %>%
  inner_join(users_summary, by = "user_id")

#growth in order 
final_data$order_rate_grow = final_data$orders_grow / final_data$max_user_orders
final_data$orders_since_last_order_grow = final_data$max_user_orders - final_data$last_order_grow
final_data$order_rate_since_first_order_grow = final_data$orders_grow / (final_data$max_user_orders - final_data$first_order_grow + 1)

#joining final_data with order_train first then few columns user and product id's
final_data = left_join(final_data,order_train %>% select(user_id, product_id, reordered), 
            by = c("user_id", "product_id"))

rm(order_train, product_detail, users_summary,products_new)


# Train / Test datasets
#train dataset
train_data = as.data.frame(final_data[final_data$eval_set == "train",])
train_data$reordered[is.na(train_data$reordered)] = 0
train_data$user_id = NULL
train_data$product_id = NULL
train_data$order_id = NULL
train_data$eval_set = NULL


#test dataset
test_data = as.data.frame(final_data[final_data$eval_set == "test",])
test_data$user_id = NULL
test_data$reordered = NULL
test_data$eval_set = NULL

rm(final_data)



# Model building using XGBoost algo.....!!!!!!!!!
library(xgboost)
#parameter list
parameters = list(
  "booster"             = "gbtree",
  "objective"           = "reg:logistic",
  "eta"                 = 0.1,
  "max_depth"           = 6, #no change
  "min_child_weight"    = 1, #find by cv
  "gamma"               = 0,
  "subsample"           = 1,
  "colsample_bytree"    = 1,
  "alpha"               = 0,
  "lambda"              = 0,
  "silent"              = 0
)
model_data = as.matrix (select(train_data,-reordered))
model_data = xgb.DMatrix(model_data, label = train_data$reordered)

#calculate best nrounds
#xgbcv = xgb.cv( params = parameters,data = model_data,label = NULL,
#               nfold = 5,nrounds = 100,verbose=1,showsd = T, stratified = T,
#               print_every_n = 10, early_stopping_rounds= 20)


#model building
insta_model = xgboost(data = model_data, params = parameters, nrounds = 76)

#calculate the importance of variables
importance = xgb.importance(colnames(model_data), model = insta_model)

#print the importance of variables
importance

##tree building inside the model
#xgb.dump(model,with_stats = T)

#visualize the importance of the variables
xgb.ggplot.importance(importance)



##prediction using above model
Th = 0.25
test = select(test_data,-order_id, -product_id)
Test = xgb.DMatrix(as.matrix(test))
test_data$reordered = predict(insta_model, Test)
test_data$reordered = ifelse (test_data$reordered > Th,1,0)
#test_data$reordered


#confusion matrix
#library(caret)
#train_lab=train_data$reordered[1:length(test_data$reordered)]
#confusionMatrix(test_data$reordered, train_lab)


#producing final result
result = filter(test_data,reordered == 1)
  result = group_by(result,order_id) 
  result = summarise(result,products = paste(product_id, collapse = ",") #To concatenate string
  )
#result
##show missing entry
missing_entries = data.frame(
  order_id = unique(test_data$order_id[!test_data$order_id %in% result$order_id]),
  products = "None"
)

#desired result
show_result = arrange(rbind(result,missing_entries),order_id)
