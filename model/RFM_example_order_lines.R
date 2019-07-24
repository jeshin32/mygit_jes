
library(ggplot2)
library(lubridate)   
library(dplyr)


# data cleaning -----------------------------------------------------------

# read raw
raw <- read.csv("data/order_lines.csv")
head(raw)
sum(is.na(raw))  # 0, 결측치 없음

raw %>% filter(customer_id == "c10763")
length(raw$customer_id)
table(raw$category)
table(raw$order_status)

# date
raw$date <- ymd(raw$date)
max(raw$date)
min(raw$date)

# delete useless and make new X
df <- raw[,-c(6,9)]
head(df)

df$totalprice <- df$price * df$quantity
df$netprice <- ifelse(df$order_status=="Delivered",df$totalprice,-df$totalprice)
summary(df)

# 기준일자
analysis_date <-today()

df %>% filter(totalprice==max(totalprice))

# make RFM ----------------------------------------------------------------

# rfm_raw
rfm_raw <- df %>% filter(date <= analysis_date) %>%
  group_by(customer_id) %>%
  summarise(recent_date = max(date),
            duration = analysis_date - max(date),
            total_revenue = sum(totalprice),
            tr_per_order = sum(totalprice)/n_distinct(order_id),
            revenue = sum(ifelse(order_status=="Delivered",totalprice,-totalprice)),
            r_per_order = sum(ifelse(order_status=="Delivered",totalprice,-totalprice))/sum(ifelse(order_status=="Delivered",1,0)),
            order_n = n_distinct(order_id),
            line_n = n_distinct(order_line_id),
            buy_n = sum(ifelse(order_status=="Delivered",1,0)))
head(rfm_raw)


rfm_df <- rfm_raw %>% 
  mutate( recency = cut(as.numeric(duration), 
                        breaks = quantile(duration, seq(0,1, by=0.2)),
                        labels = 5:1,
                        include.lowest = T),
          freq = cut(as.numeric(buy_n), 
                     breaks = quantile(buy_n, c(0, seq(0.4,1, by=0.2))), # 0~0.6 / 0.6~0.8 / 0.8~1.0
                     labels = 1:4,
                     include.lowest = T),
          moneytary = cut(as.numeric(revenue), 
                          breaks = quantile(revenue, seq(0,1, by=0.2)),
                          labels = 1:5,
                          include.lowest = T)) %>% 
  mutate(RFM = paste0(recency, freq, moneytary))

rfm <- rfm_df[,-c(2:10)]
rfm



# fv ----------------------------------------------------------------------

fv <- df %>% filter(date > analysis_date ) %>% 
  group_by(customer_id) %>% 
  summarise(fv = sum(netprice))
fv              

rfm_fv <- rfm %>% 
  merge(fv, by="customer_id", all.x = T)
rfm_fv              

# NA 를 0처리 (구매를 안 했으니까)
rfm_fv <- rfm_fv %>% 
  mutate(fv = ifelse(is.na(fv), 0, fv))
rfm_fv


# 영향력 (미래수익 설명력) -----------------------------------------------------

# Recency
temp <- 
  rfm_fv %>% group_by(recency) %>% 
  summarise(N= n(),
            fv = mean(fv)) %>% 
  arrange(recency) 
temp

ggplot(temp, aes(recency, fv))+ geom_bar(stat = "identity", fill = "orange", color="red")


# Frequency
temp <- 
  rfm_fv %>% group_by(freq) %>% 
  summarise(N= n(),
            fv = mean(fv)) %>% 
  arrange(desc(freq))
temp

ggplot(temp, aes(freq, fv)) + 
  geom_bar(stat = "identity", fill = "orange", color="red")

# Moneytary
temp <- 
  rfm_fv %>% group_by(moneytary) %>% 
  summarise(N= n(),
            fv = mean(fv)) %>% 
  arrange(desc(moneytary))
temp

ggplot(temp, aes(moneytary, fv))+ geom_bar(stat = "identity", fill = "orange", color="red")

# RFM
temp <- 
  rfm_fv %>% group_by(RFM) %>% 
  summarise(N= n(),
            fv = mean(fv)) %>% 
  arrange(-fv)
temp

ggplot(temp, aes(reorder(RFM,-fv), fv)) + 
  geom_bar(stat = "identity", fill = "orange", color="red") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))


# Retension Effect -----------------------------------------------------

churn <- df %>% filter(date > analysis_date ) %>% 
  group_by(customer_id) %>% 
  summarise(churn = ifelse(sum(netprice)>0, 0, 1))
churn               

rfm_churn <- rfm %>% 
  merge(churn, by="customer_id", all.x = T)
rfm_churn     

# NA면 실적이 없으므로 이탈
rfm_churn <- rfm_churn %>% 
  mutate(churn = ifelse(is.na(churn), 1, churn))
rfm_churn

# 이탈 모델링
dim(rfm_churn)
table(rfm_churn$churn)

library(randomForest)
md <- randomForest(factor(churn) ~ recency + freq + moneytary,
                   data = rfm_churn, 
                   ntree = 200)
md

md$importance

pred <- predict(md, rfm_churn, type = "prob")
head(pred)

df <- data.frame(
  rfm_churn[, c("recency","freq","moneytary")],
  p = pred[, 2]
)
df

# 모델의 prob값이 각각의 LEVEL에서 확률이 얼마나 나오나 (기여도)
df %>% group_by(recency) %>% 
  summarise(p = mean(p))

df %>% group_by(freq) %>% 
  summarise(p = mean(p))

df %>% group_by(moneytary) %>% 
  summarise(p = mean(p))
