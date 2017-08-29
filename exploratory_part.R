rm(list=ls())
setwd("C:/Users/mdsha/OneDrive/Documents/R/win-library/3.3/readr/instacart_data")
getwd()
library(dplyr)
library(ggplot2)
library(knitr)
#loading data 

aisles <- read.csv("aisles.csv",header=T)
departments <- read.csv("departments.csv",header=T)

order_prior <-read.csv("order_products__prior.csv",header=T)
order_prior=order_prior[sample(nrow(order_prior),1000000,replace=F),]

order_train <- read.csv("order_products__train.csv",header=T)
order_train=order_train[sample(nrow(order_train),100000,replace=F),]

orders <- read.csv("orders.csv",header=T)
orders=orders[sample(nrow(orders),300000,replace=F),]


products <-read.csv("products.csv",header=T)
products=products[sample(nrow(products),5000,replace=F),]

kable(head(orders,3))
str(orders)

kable(head(aisles,5))
str(aisles)

kable(head(products,5))
str(departments)

#day of week
#library(stringr)
  
  ggplot(orders,aes(x=order_dow)) + geom_histogram(stat="count",fill="blue")

#hour of day

    ggplot(orders ,aes(x=order_hour_of_day)) + geom_histogram(stat="count",fill="red")
    
#day_since_prior_order
    
    
      ggplot(orders,aes(x=days_since_prior_order)) + geom_histogram(stat="count",fill="red")
    
#eval set
      library(scales)
      ggplot(orders,aes_string(x=orders$eval_set,y=orders$user_id)) +
        geom_bar(stat="identity",fill="DarkSlateBlue")+theme_bw() + scale_y_continuous(breaks=pretty_breaks(n=10))

      ##orders_prior
      x=summarize(group_by(order_train,order_id),
                  n_items = last(add_to_cart_order))
                  ggplot(x,aes(x=n_items))+
                  geom_histogram(stat="count",fill="orange") + 
                  coord_cartesian(xlim=c(0,80))
     
    ##top buyers
                   
                    x=group_by(order_train,product_id) 
                    y=summarize(x,count = n()) 
                    top_products = top_n(y,5, wt = count) %>%
                    left_join(select(products,product_id,product_name),by="product_id") %>%
                    arrange(desc(count)) 
                  kable(top_products)
 
#bar plot for top 10 products
ggplot(top_products,aes(x=reorder(product_name,-count), y=count))+
geom_bar(stat="identity",fill="orange")+
theme(axis.text.x=element_text(angle=90, hjust=1),axis.title.x = element_blank())

#which item do people select into the cart first
#first selection into the cart

  x=group_by(order_train ,product_id, add_to_cart_order)  
  y=summarize(x,count = n()) 
  z=mutate(y,count_rate=count/sum(count)) 
  p=filter(z,add_to_cart_order == 1, count>10) 
  q=arrange(p,desc(count_rate))  
  cart_first =left_join(q,products,by="product_id") %>% 
  select(product_name, count_rate, count) %>% 
  ungroup() %>% 
  top_n(10, wt=count_rate)

kable(cart_first)

##bar plot for the same

  ggplot(cart_first,aes(x=reorder(product_name,-count_rate), y=count_rate))+
  geom_bar(stat="identity",fill="brown")+
  theme(axis.text.x=element_text(angle=90, hjust=1),axis.title.x = element_blank())+coord_cartesian(ylim=c(0.4,0.7))

  ##how often user go for the same product
    
    x=group_by(order_train,reordered)  
    y=summarize(x,count = n())  
    same_product <- mutate(y,reordered = as.factor(reordered)) %>%
    mutate(proportion = count/sum(count))
  kable(same_product) 
  
  ###bar plot for the same
 
    ggplot(same_product,aes(x=reordered,y=count,fill=reordered))+
    geom_bar(stat="identity")
  
  ##most frequently reordered
    
      x=group_by(order_train,product_id) 
      y=summarize(x,reordered_ratio = mean(reordered), n=n()) 
      
        most_reordered <- filter(y,n>60) %>% 
      top_n(5,wt=reordered_ratio) %>% 
      arrange(desc(reordered_ratio)) %>% 
      left_join(products,by="product_id")
    
    kable(most_reordered) 
  
    ##bar plot for the same
     
      ggplot(most_reordered,aes(x=reorder(product_name,-reordered_ratio), y=reordered_ratio))+
      geom_bar(stat="identity",fill="orange")  +
      theme(axis.text.x=element_text(angle=90, hjust=1),axis.title.x = element_blank())
  
      
    #association between time of last order and probability of reorder
      order_train %>% 
        left_join(orders,by="order_id") %>% 
        group_by(days_since_prior_order) %>%
        summarize(avg_reorder = mean(reordered)) %>%
        ggplot(aes(x=days_since_prior_order,y=avg_reorder))+
        geom_bar(stat="identity",fill="orange")
   
      
    #Association between number of orders and probability of reordering     
      order_train %>% 
        group_by(product_id) %>% 
        summarize(reordered_ratio = mean(reordered), n_orders=n()) %>%
        ggplot(aes(x=n_orders,y=reordered_ratio))+
        geom_point()+
        geom_smooth(color="green")+
        coord_cartesian(xlim=c(0,1000)) 
    #organic & non-organic
      prod_cat <- products %>% 
        mutate(organic=ifelse(str_detect(str_to_lower(products$product_name),
                                         'organic'),"organic","not organic"), organic= as.factor(organic))
      
      or_nor <- order_train %>% 
        left_join(prod_cat, by="product_id") %>% 
        group_by(organic) %>% 
        summarize(count = n()) %>% 
        mutate(proportion = count/sum(count))
      kable(or_nor)
      
      ##bar plot
       
        ggplot(or_nor,aes(x=organic,y=count, fill=organic))+
        geom_bar(stat="identity")
     
        #Reordering Organic vs Non-Organic
        r_or_nor <- order_train %>% left_join(products,by="product_id") %>% group_by(organic) %>% summarize(avg_reordered = mean(reordered))
        kable(r_or_nor)
        
        #bar plot
        
          ggplot(r_or_nor,aes(x=organic,fill=organic,y=avg_reordered))+geom_bar(stat="identity")
        
        