setwd("D:\\Documents\\GitHub\\Mercari")
library(dplyr)

training <- read.delim(file="train.tsv", encoding="UTF-8", header=TRUE)

attach(training)
plot(item_condition_id, price)

plot(item_condition_id[shipping==0], price[shipping==0], xlab="item condition", ylab="price", pch="o", col=5, ylim=c(0, 2000))
points(item_condition_id[shipping==1], price[shipping==1], pch='1', col=1)

train.lm <- lm(price ~ item_condition_id + shipping)

plot(train.lm$residuals, train.lm$fitted.values)

training <- training %>%
  mutate( category = as.factor( ifelse(category_name == "", "No category", category_name) ),
          brand = as.factor( ifelse(brand_name == "", "No brand", brand_name))
  ) %>%
  
  filter (price != 0) %>%
  select( condition = item_condition_id, category, brand, shipping, price)

aggregate(price ~ brand, training, FUN="mean")
