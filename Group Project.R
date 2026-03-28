install.packages("tidyverse")
library(tidyverse)
data <- read_csv("c:/Users/제품유형/besktop/Project 1.csv")

glimpse(data)

coffee_data <- data %>%
  select(hour, product_type)

summary(coffee_data)

hourly_popular <- coffee_data %>%
  group_by(Hour, product_type) %>%
  summarise(count = n()) %>%
  arrange(hour, desc(count))

top3_by_hour <- hourly_popular %>%
  group_by(Hour) %>%
  slice_max(count, n = 3)

print(top3_by_hour)

ggplot(top3_by_hour, aes(x = factor(hour), y = count, fill = product_type)) +
  geom_col(position = "dodge") +
  labs(title = "各时间段热销咖啡类型TOP3",
       x = "小时",
       y = "销售量",
       fill = "产品类型") +
  theme_minimal()

#以上内容为第一部分(郑儒铉)，主要采用的数据集是未经处理的原始数据集1
--------------------------------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(car)         
library(ggpubr)      
library(broom)       

data <- read.csv("coffee_behavior_data.csv")

data$q19 <- as.factor(data$q19)
data$q20 <- as.factor(data$q20)      
data$q2_1 <- as.factor(data$q2_1)    
data$q18 <- as.factor(data$q18)      

str(data)

model <- lm(q30 ~ q19 + q20 + q2_1 + q18, data = data)
summary(model)

par(mfrow=c(2,2))
plot(model)

vif(model)

anova(model)

tidy_model <- tidy(model)
print(tidy_model)

p1 <- ggplot(data, aes(x=q19, y=q30)) +
  geom_boxplot(fill="#a1d99b") +
  theme_minimal() +
  labs(title="Willingness to Pay by Time of Day", x="Time of Day", y="Willingness to Pay")

p2 <- ggplot(data, aes(x=q20, y=q30)) +
  geom_boxplot(fill="#9ecae1") +
  theme_minimal() +
  labs(title="Willingness to Pay by Coffee Type", x="Coffee Type", y="Willingness to Pay")

p3 <- ggplot(data, aes(x=q18, y=q30)) +
  geom_boxplot(fill="#fdae6b") +
  theme_minimal() +
  labs(title="Willingness to Pay by Frequency", x="Frequency", y="Willingness to Pay")

ggarrange(p1, p2, p3, ncol = 1, nrow = 3)

interaction_model <- lm(q30 ~ q19 * q20 + q2_1 + q18, data = data)
summary(interaction_model)

anova(model, interaction_model)

##以上内容为第二部分(徐梓翔)，主要采用的数据是数据集2
--------------------------------------------------------------------------------
setwd("D:/Texas/第四学期勉强过/統計軟件")
#0原始數據導入以及初步處理
myRawData <- read.csv(file="CoffeeSpecifiedProject.csv")
df <- subset(myRawData,select=c(-1,-2,-3,-4,-6,-7,-11,-15,-16))
df$store_id <- as.factor(df$store_id)
df$Month <- as.factor(df$Month)
df$Day.of.Week <- as.factor(df$Day.of.Week)

#1.1 查看各類別的頻率分佈
table(df$store_id) 
prop.table(table(df$store_id))
#各門店的數據相對較爲平均
table(df$transaction_qty)
prop.table(table(df$transaction_qty))
median(df$transaction_qty)
#絕大部分人并不會購買三杯及以上的咖啡，更多的人願意只購買一杯咖啡；購買的中位數是2杯
table(df$unit_price)
prop.table(table(df$unit_price))
#最多的顧客購買了價格為3美元的咖啡（但要注意，這可能是由於店鋪本身給出了更多的3美元產品）
table(df$Total_Bill)
prop.table(table(df$Total_Bill))
#絕大部分的縂交易額聚集在5美元以下
table(df$product_type)
prop.table(table(df$product_type))
#最受人喜愛的咖啡大類是Gourmet brewed coffee，其次是Barista Espresso
table(df$product_detail)
prop.table(table(df$product_detail))
#最受歡迎的咖啡細分是Our Old Time Diner Blend，見可視化圖片
table(df$Size)
prop.table(table(df$Size))
#賣出最多的Regular型的，其次是Large型
table(df$Hour)
prop.table(table(df$Hour))
#8點至11點的顧客最多
table(df$Month)
prop.table(table(df$Month))
#隨著月份逐漸傾向於夏季，來購買咖啡的人總體增多
table(df$Day.of.Week)
prop.table(table(df$Day.of.Week))
#星期幾并不起到太關鍵的作用，并不是工作日來享用的人更多

#1.2 交叉表
table(df$Month, df$product_type)
table(df$Month, df$product_detail)
table(df$Month, df$Size)
table(df$Month, df$Hour)
table(df$Month, df$Day.of.Week)
table(df$Month, df$Total_Bill)
table(df$Month, df$unit_price)
table(df$Month, df$transaction_qty)
table(df$Month, df$store_id)
#Month下其他變量基本呈現相近的增加幅度
table(df$Day.of.Week, df$product_type)
table(df$Day.of.Week, df$product_detail)
table(df$Day.of.Week, df$Size)
table(df$Day.of.Week, df$Hour)
table(df$Day.of.Week, df$Total_Bill)
table(df$Day.of.Week, df$unit_price)
table(df$Day.of.Week, df$transaction_qty)
table(df$Day.of.Week, df$store_id)
#Day.of.Week的作用似乎並不顯著，其與其他變量之間的關係非常薄弱
table(df$Hour, df$product_type)
table(df$Hour, df$product_detail)
table(df$Hour, df$Size)
table(df$Hour, df$Total_Bill)
table(df$Hour, df$unit_price)
table(df$Hour, df$transaction_qty)
table(df$Hour, df$store_id)
#Hour的作用顯見於store_id，3號在整個白天吸引的客流較多，相較之下5號在早上更能吸引客流，8號則在晚上吸引更多客流
#Hour的作用同樣可見於transaction_qty，早上的顧客更傾向於購買更多份額的咖啡
#Hour各個時段的最佳商品：Columbian Medium Roast,Our Old Time Diner Blend,Jamaican Coffee River,Ethiopia,Ethiopia,Columbian Medium Roast,Brazilian,Columbian Medium Roast,Ethiopia,Ethiopia,Columbian Medium Roast,Brazilian,Our Old Time Diner Blend,Columbian Medium Roast,Our Old Time Diner Blend
table(df$store_id, df$unit_price)
table(df$store_id, df$Size)
table(df$store_id, df$transaction_qty)
table(df$store_id, df$product_detail)
table(df$store_id, df$Total_Bill)
#store_id的作用可見於transaction_qty，來5號消費的顧客更愿意購買更多份額的咖啡
#store_id的作用可見於product——detail，最爲顯著的是3號不提供Ouro Brasileiro shot
#store_id的作用可見於Total_Bill，5號的收入相較之下高於其餘二者
table(df$product_type, df$transaction_qty)
table(df$product_type, df$unit_price)
table(df$product_type, df$product_detail)
table(df$product_type, df$Size)
table(df$product_type, df$Total_Bill)
#product_type對於unit_price有明顯的作用，這是由於商店決定不同大類咖啡的不同單價，注意到Barista Espresso的單價明顯更高
#product_type對於Size有明顯的作用，這是由於商店決定不同大類咖啡的不同規格，注意到僅Barista Espresso中存在Not Defined規格
#product_detail各類隸屬於product_type
#product_type對於Total_Bill有明顯的作用
table(df$product_detail, df$transaction_qty)
table(df$product_detail, df$unit_price)
table(df$product_detail, df$Size)
table(df$product_detail, df$Total_Bill)
#unit_price和Size都依product_detail決定，注意到Cappuccino和Latte價格較高
#product_detail對於Total_Bill有明顯的作用
table(df$Size, df$unit_price)
table(df$Size, df$transaction_qty)
table(df$Size, df$Total_Bill)
#Size顯然對unit_price有重要影響，Size越大，unit_price越高；值得注意的是，Not Defined的價格也相對較高
#Size中的Not Defined可以合理推測為介於Regular和Large之間難以直接測量的狀態，Not Defined均值為3.34，Large均值約爲3.51
#Size對於Total_Bill有明顯的作用
table(df$unit_price, df$transaction_qty)
table(df$unit_price, df$Total_Bill)
#unit_price並未影響到transaction_qty，甚至數量最多的購買還是來自於最貴的單價
table(df$transaction_qty, df$Total_Bill)
#unit_price和transaction_qty直接決定了Total_Bill

#1.3 可視化及具體分析
library(dplyr)
library(ggplot2)

ggplot(df, aes(x = transaction_qty)) +
  geom_bar(fill = "skyblue") +
  labs(title = "每次交易購買杯數分佈", x = "購買數量", y = "交易筆數") +
  theme_minimal()

ggplot(df, aes(x = unit_price)) +
  geom_histogram(binwidth = 0.25, fill = "yellow") +
  labs(title = "單價分佈直方圖", x = "單價（美元）", y = "產品數量") +
  theme_minimal()

ggplot(df, aes(x = product_type)) +
  geom_bar(fill = "purple") +
  labs(title = "各類咖啡類型交易次數", x = "產品類型", y = "交易筆數") +
  theme_minimal()

ggplot(df, aes(x = product_detail)) +
  geom_bar(fill = "pink") +
  labs(title = "各類咖啡類型交易次數", x = "產品細節", y = "交易筆數") +
  theme_minimal()

ggplot(df, aes(x = Size)) +
  geom_bar(fill = "red") +
  labs(title = "不同容量銷售分佈", x = "Size", y = "交易次數") +
  theme_minimal()

ggplot(df, aes(x = Hour)) +
  geom_bar(fill = "grey") +
  labs(title = "小時（Hour）分佈", x = "小時", y = "交易次數") +
  theme_minimal()
df %>%
  group_by(Hour) %>%
  summarise(count = n(), avg_qty = mean(transaction_qty), total_bill = sum(Total_Bill))

ggplot(df, aes(x = Day.of.Week)) +
  geom_bar(fill = "orange") +
  labs(title = "日期（Day.of.Week）分佈", x = "小時", y = "交易次數") +
  theme_minimal()
df %>%
  group_by(Day.of.Week) %>%
  summarise(sales_volume = sum(transaction_qty)) %>%
  ggplot(aes(x = Day.of.Week, y = sales_volume)) +
  geom_bar(stat = "identity", fill = "orange") +
  labs(title = "每星期的總銷售量", y = "交易總量")

df %>%
  group_by(Hour, store_id) %>%
  summarise(total_qty = sum(transaction_qty), .groups = "drop") %>%
  ggplot(aes(x = Hour, y = total_qty, color = store_id)) +
  geom_line(size = 1) +
  labs(title = "不同門市在一天內的交易量變化", y = "交易總量") +
  theme_minimal()

df %>%
  group_by(Hour) %>%
  summarise(avg_qty = mean(transaction_qty)) %>%
  ggplot(aes(x = Hour, y = avg_qty)) +
  geom_line(color = "black", size = 1) +
  labs(title = "不同時間平均購買數量", y = "平均購買杯數") +
  theme_minimal()

df %>%
  mutate(hour = as.numeric(Hour),
         time_bin = case_when(
           hour >= 6 & hour < 11 ~ "Morning",
           hour >= 11 & hour < 14 ~ "Lunch",
           hour >= 14 & hour < 18 ~ "Afternoon",
           hour >= 18 & hour < 22 ~ "Evening",
           TRUE ~ "Off-hours"
         )) %>%
  group_by(time_bin, product_type) %>%
  summarise(sales = sum(transaction_qty)) %>%
  ggplot(aes(x = time_bin, y = sales, fill = product_type)) +
  geom_bar(stat = "identity") +
  labs(title = "各時段不同產品的銷售量") +
  theme_minimal()

df %>%
  mutate(Hour = as.factor(Hour)) %>%
  group_by(Hour, product_detail) %>%
  summarise(sales = sum(Total_Bill), .groups = "drop") %>%
  ggplot(aes(x = Hour, y = sales, fill = product_detail)) +
  geom_bar(stat = "identity") +
  labs(title = "不同時段產品類型的銷售量分布",
       x = "時段",
       y = "縂銷售額") +
  theme_minimal()

df %>%
  group_by(store_id) %>%
  summarise(avg_qty = mean(transaction_qty)) %>%
  ggplot(aes(x = store_id, y = avg_qty)) +
  geom_col(fill = "brown") +
  labs(title = "不同門市平均購買數量", y = "平均購買數量") +
  theme_minimal()
store_bill_summary <- df %>%
  group_by(store_id) %>%
  summarise(
    total_revenue = sum(Total_Bill, na.rm = TRUE),
    avg_revenue_per_order = mean(Total_Bill, na.rm = TRUE),
    n_transactions = n()
  )

library(reshape2)

product_matrix <- table(df$store_id, df$product_detail) %>% as.data.frame()
colnames(product_matrix) <- c("store_id", "product_detail", "Freq")
ggplot(product_matrix, aes(x = product_detail, y = store_id, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "darkgreen") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "不同門市供應品項熱力圖",
       x = "產品細節",
       y = "門市 ID")

df %>%
  group_by(Size) %>%
  summarise(avg_price = mean(unit_price), avg_bill = mean(Total_Bill))
df %>%
  group_by(Size) %>%
  summarise(avg_qty = mean(transaction_qty),
            total_qty = sum(transaction_qty),
            n_orders = n())

ggplot(df, aes(x = Size, y = unit_price)) +
  geom_boxplot(fill = "blue") +
  labs(title = "不同容量的單價分佈")

df %>%
  group_by(product_detail) %>%
  summarise(avg_bill = mean(Total_Bill)) %>%
  arrange(desc(avg_bill)) #Latte和Cappuccino的平均單次售賣總價超過了6美元，其餘的都小於5美元

ggplot(df, aes(x = unit_price, y = transaction_qty)) +
  geom_jitter(alpha = 0.3, color = "violet") +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "單價與購買數量的關係圖")  #增加了隨機抖動以可視化取各個真值的數量

ggplot(df, aes(x = unit_price, y = transaction_qty)) +
  geom_jitter(alpha = 0.3, color = "cyan") +
  geom_smooth(method = "lm", se = FALSE, color = "darkorange") +
  facet_wrap(~ product_type) +
  labs(
    title = "不同咖啡類型中價格與購買數量的關係",
    x = "單價（unit_price）",
    y = "交易數量（transaction_qty）"
  ) +
  theme_minimal()
#觀察到無論是對於全部的咖啡還是各種品類的咖啡，價格對於每一筆交易的購買數量并無明顯作用
#以上的全部過程是對描述統計的内容進行基本的可視化，但并未將所有圖片在ppt中顯示


#2.1 聚類分析

library(klaR)

df_transformed <- df %>%
  mutate(
    time_bin = case_when(
      Hour >= 6 & Hour < 11 ~ "Morning",
      Hour >= 11 & Hour < 14 ~ "Lunch",
      Hour >= 14 & Hour < 18 ~ "Afternoon",
      Hour >= 18 & Hour < 22 ~ "Evening",
      TRUE ~ "Off-hours"
    ),
    weekday_type = ifelse(Day.of.Week %in% c(0, 6), "Weekend", "Weekday"),
    season = case_when(
      Month %in% c(1, 2) ~ "Winter",
      Month %in% c(3, 4, 5) ~ "Spring",
      Month == 6 ~ "Summer",
      TRUE ~ NA_character_
    )
  )
df_kmodes_ready <- df_transformed %>%
  dplyr::select(weekday_type, time_bin, season, store_id, product_type, product_detail, Size) %>%
  mutate(across(everything(), as.factor)) %>%
  na.omit()


set.seed(123)

best_model <- NULL
best_cost <- Inf  

n_runs <- 20

for (i in 1:n_runs) {
  current_model <- kmodes(df_kmodes_ready, modes = 3, iter.max = 20, fast = T)
  if (sum(current_model$withindiff) < best_cost) {
    best_cost <- sum(current_model$withindiff)
    best_model <- current_model
  }
}

best_cost3<-best_cost
best_model$modes


set.seed(123)

for (i in 1:n_runs) {
  current_model <- kmodes(df_kmodes_ready, modes = 4, iter.max = 20, fast = T)
  if (sum(current_model$withindiff) < best_cost) {
    best_cost <- sum(current_model$withindiff)
    best_model <- current_model
  }
}

best_cost4<-best_cost
best_model$modes


set.seed(123)

for (i in 1:n_runs) {
  current_model <- kmodes(df_kmodes_ready, modes = 5, iter.max = 20, fast = T)
  if (sum(current_model$withindiff) < best_cost) {
    best_cost <- sum(current_model$withindiff)
    best_model <- current_model
  }
}

best_cost5<-best_cost
best_model$modes


set.seed(123)

for (i in 1:n_runs) {
  current_model <- kmodes(df_kmodes_ready, modes = 6, iter.max = 20, fast = T)
  if (sum(current_model$withindiff) < best_cost) {
    best_cost <- sum(current_model$withindiff)
    best_model <- current_model
  }
}

best_cost6<-best_cost
best_model$modes

k_values <- c(3, 4, 5, 6)
costs <- c(best_cost3, best_cost4, best_cost5, best_cost6)

plot(k_values, costs, type = "b", 
     xlab = "Number of Clusters (K)", 
     ylab = "Total Within-Cluster Difference",
     main = "Elbow Method for Optimal K")
#根據肘部法則，認爲分成4組最優
#此處有大量重複内容，可能造成了審閲時的不便，非常抱歉！

set.seed(123)
best_model <- NULL
best_cost <- Inf  

for (i in 1:n_runs) {
  current_model <- kmodes(df_kmodes_ready, modes = 4, iter.max = 20, fast = T)
  if (sum(current_model$withindiff) < best_cost) {
    best_cost <- sum(current_model$withindiff)
    best_model <- current_model
  }
}

best_cost4<-best_cost
best_model$modes


vars <- c("weekday_type", "time_bin", "season", "store_id", "product_detail", "product_type", "Size")

chi_results <- lapply(vars, function(v) {
  tab <- table(df_kmodes_ready[[v]], best_model$cluster)
  test <- chisq.test(tab)
  data.frame(
    variable = v,
    p_value = test$p.value
  )
})

chi_summary <- do.call(rbind, chi_results)
chi_summary <- chi_summary[order(chi_summary$p_value), ]
print(chi_summary)
#在這一步後，刪去p值大於0.05的weekday_type和season
#這一步利用卡方檢驗，將聚類所得的標簽本身作爲一個因變量，考慮各個自變量對因變量的影響，即各個變量對聚類結果是否起到作用

df_kmodes_ready <- df_kmodes_ready[,-c(1,3)]
set.seed(123)

best_model <- NULL
best_cost <- Inf  

n_runs <- 20

for (i in 1:n_runs) {
  current_model <- kmodes(df_kmodes_ready, modes = 3, iter.max = 20, fast = T)
  if (sum(current_model$withindiff) < best_cost) {
    best_cost <- sum(current_model$withindiff)
    best_model <- current_model
  }
}

best_cost3 <- best_cost
best_model$modes


set.seed(123)
for (i in 1:n_runs) {
  current_model <- kmodes(df_kmodes_ready, modes = 4, iter.max = 20, fast = T)
  if (sum(current_model$withindiff) < best_cost) {
    best_cost <- sum(current_model$withindiff)
    best_model <- current_model
  }
}

best_cost4<-best_cost
best_model$modes


set.seed(123)
for (i in 1:n_runs) {
  current_model <- kmodes(df_kmodes_ready, modes = 5, iter.max = 20, fast = T)
  if (sum(current_model$withindiff) < best_cost) {
    best_cost <- sum(current_model$withindiff)
    best_model <- current_model
  }
}

best_cost5 <- best_cost
best_model$modes


set.seed(123)
for (i in 1:n_runs) {
  current_model <- kmodes(df_kmodes_ready, modes = 6, iter.max = 20, fast = T)
  if (sum(current_model$withindiff) < best_cost) {
    best_cost <- sum(current_model$withindiff)
    best_model <- current_model
  }
}

best_cost6 <- best_cost
best_model$modes


set.seed(123)
for (i in 1:n_runs) {
  current_model <- kmodes(df_kmodes_ready, modes = 7, iter.max = 20, fast = T)
  if (sum(current_model$withindiff) < best_cost) {
    best_cost <- sum(current_model$withindiff)
    best_model <- current_model
  }
}

best_cost7 <- best_cost
best_model$modes


k_values <- c(3, 4, 5, 6, 7)
costs <- c(best_cost3, best_cost4, best_cost5, best_cost6, best_cost7)

plot(k_values, costs, type = "b", 
     xlab = "Number of Clusters (K)", 
     ylab = "Total Within-Cluster Difference",
     main = "Elbow Method for Optimal K")
#根據肘部法則，認爲分成5組最優

set.seed(123)
best_model <- NULL
best_cost <- Inf  

for (i in 1:n_runs) {
  current_model <- kmodes(df_kmodes_ready, modes = 5, iter.max = 20, fast = T)
  if (sum(current_model$withindiff) < best_cost) {
    best_cost <- sum(current_model$withindiff)
    best_model <- current_model
  }
}

best_cost5 <- best_cost
best_model$modes

vars <- c("time_bin", "store_id", "product_detail", "product_type", "Size")

chi_results <- lapply(vars, function(v) {
  tab <- table(df_kmodes_ready[[v]], best_model$cluster)
  test <- chisq.test(tab)
  data.frame(
    variable = v,
    p_value = test$p.value
  )
})

chi_summary <- do.call(rbind, chi_results)
chi_summary <- chi_summary[order(chi_summary$p_value), ]
print(chi_summary)
df_kmodes_ready$cluster <- best_model$cluster
#同樣通過卡方分析，認爲此處所有的變量都對聚類結果有較大影響，換言之，在這種情況下可以歸納每個類別的基本情況，如某一個類別有其集中的消費時段、商品大類

library(clustMixType)

df_kproto <- df %>%
  mutate(
    weekday_type = ifelse(Day.of.Week %in% c(0, 6), "Weekend", "Weekday"),
    season = case_when(
      Month %in% c(1, 2) ~ "Winter",
      Month %in% c(3, 4, 5) ~ "Spring",
      Month == 6 ~ "Summer",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::select(weekday_type, season, store_id, product_detail, Size, Hour, unit_price, transaction_qty) %>%
  mutate(across(where(is.character), as.factor)) %>%
  mutate(across(c(Hour, unit_price, transaction_qty), ~ scale(.)[,1])) %>%
  na.omit()

wss_result <- data.frame(k = 2:10, wss = NA)

for (k in 2:10) {
  set.seed(123)
  model <- kproto(df_kproto[, -ncol(df_kproto)], k = k)
  wss_result$wss[wss_result$k == k] <- model$tot.withinss
}

ggplot(wss_result, aes(x = k, y = wss)) +
  geom_point() +
  geom_line() +
  labs(title = "K-Prototypes Elbow Plot", x = "Cluster 數", y = "總群內差異") +
  theme_minimal()
#觀察圖像，根據肘部法則，生成3組最優
#這裏采用了簡略的寫法

set.seed(123)
kproto_model <- kproto(df_kproto, k = 8)
df_kproto$cluster <- as.factor(kproto_model$cluster)

kproto_model$centers

chisq.test(df_kproto$cluster, df_kproto$weekday_type)
chisq.test(df_kproto$cluster, df_kproto$season)
chisq.test(df_kproto$cluster, df_kproto$store_id)
chisq.test(df_kproto$cluster, df_kproto$product_detail)
chisq.test(df_kproto$cluster, df_kproto$Size)
summary(aov(Hour ~ cluster, data = df_kproto))
summary(aov(unit_price ~ cluster, data = df_kproto))
summary(aov(transaction_qty ~ cluster, data = df_kproto))
#同樣以標簽為因變量，將定量變量進行ANOVA檢驗，定性變量進行卡方檢驗

ggplot(df_kproto, aes(x = Hour, fill = cluster)) +
  geom_histogram(binwidth = 1, position = "dodge") +
  labs(title = "K-Prototypes 聚類結果在 Hour 上的分布", x = "Hour", fill = "Cluster") +
  theme_minimal()

ggplot(df_kproto, aes(x = product_detail, fill = cluster)) +
  geom_bar(position = "dodge") +
  labs(title = "K-Prototypes 聚類結果在 product_detail 上的分布",
       x = "product_detail",
       fill = "Cluster") +
  theme_minimal()

ggplot(df_kproto, aes(x = season, fill = cluster)) +
  geom_bar(position = "dodge") +
  labs(title = "K-Prototypes 聚類結果在 season 上的分布",
       x = "season",
       fill = "Cluster") +
  theme_minimal()
tab <- table(df_kproto$cluster, df_kproto$season)
test <- chisq.test(tab)
print(test$stdres)
#將一些重要的結果進行可視化（就kmodes中被刪去的season等變量在kproto中得到保留進行可視化驗證）

#2.2 比較分析聚類情況
table(kmodes_cluster = df_kmodes_ready$cluster, kproto_cluster = df_kproto$cluster)
#分析觀察是否kmodes和kproto有明確的優先或層級關係，根據表格的元素知並沒有
#這是由於kmodes中的聚類主要針對消費者的行爲模式，而kproto中的聚類則主要針對其實際消費行爲；二者並不是直接的因果聯係

df_cluster <- df %>%
  mutate(
    kmodes_cluster = df_kmodes_ready$cluster,
    kproto_cluster = df_kproto$cluster,
    cluster_pair = paste0("KM", kmodes_cluster, "_KP", kproto_cluster),
    
    weekday_type = ifelse(Day.of.Week %in% c(0, 6), "Weekend", "Weekday"),
    
    season = case_when(
      Month %in% c(1, 2) ~ "Winter",
      Month %in% c(3, 4, 5) ~ "Spring",
      Month == 6 ~ "Summer",
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::select(cluster_pair, weekday_type, season, store_id, product_detail, Size, Hour, unit_price, transaction_qty) %>%
  na.omit()
write.csv(df_cluster, "cluster_pair_analysis.csv", row.names = FALSE)

table(df_cluster$cluster_pair)
cluster_freq <- df_cluster %>%
  group_by(cluster_pair) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  mutate(proportion = count / sum(count))
head(cluster_freq, 5)
df_cluster$kmodes_cluster <- df_kmodes_ready$cluster
df_cluster$kproto_cluster <- df_kproto$cluster
heat_data <- table(df_cluster$kmodes_cluster, df_cluster$kproto_cluster) %>% as.data.frame()
colnames(heat_data) <- c("KM", "KP", "Freq")

ggplot(heat_data, aes(x = factor(KP), y = factor(KM), fill = Freq)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Freq), size = 3) +
  scale_fill_gradient(low = "white", high = "maroon") +
  labs(title = "交叉聚类频率热力图", x = "kproto 聚类编号", y = "kmodes 聚类编号") +
  theme_minimal()
library(tidyr)
top5_combinations <- df_cluster %>%
  count(cluster_pair) %>%
  arrange(desc(n)) %>%
  head(5) %>%
  pull(cluster_pair)
df_cluster %>%
  filter(cluster_pair %in% top5_combinations) %>%
  pivot_longer(cols = c(Hour, unit_price), names_to = "Variable", values_to = "Value") %>%
  ggplot(aes(x = factor(cluster_pair), y = Value)) +
  geom_boxplot(fill = "lightgreen") +
  facet_wrap(~ Variable, scales = "free") +
  labs(title = "前五聚类组合的关键变量分布", x = "聚类组合", y = "数值") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
df_cluster %>%
  filter(cluster_pair %in% top5_combinations) %>%
  ggplot(aes(x = factor(transaction_qty), fill = factor(cluster_pair))) +
  geom_bar(position = "dodge")
  facet_wrap(~ Variable, scales = "free") +
  labs(x = "Variable Value", y = "Count")

summary_table <- df_cluster %>%
  group_by(cluster_pair) %>%
  summarise(
    count = n(),
    avg_hour = mean(Hour, na.rm = TRUE),
    avg_price = mean(unit_price, na.rm = TRUE),
    avg_qty = mean(transaction_qty, na.rm = TRUE),
    top_product = names(sort(table(product_detail), decreasing = TRUE)[1]),
    top_store = names(sort(table(store_id), decreasing = TRUE)[1]),
    top_size = names(sort(table(Size), decreasing = TRUE)[1]),
    weekend_ratio = mean(weekday_type == "Weekend"),
    spring_ratio = mean(season == "Spring"),
    summer_ratio = mean(season == "Summer"),
    winter_ratio = mean(season == "Winter")
  ) %>%
  arrange(desc(count))

write.csv(summary_table, file = "Cluster_Pair_Summary.csv", row.names = T)
#進行可視化體現以及生成文件以展示基本各個組別情況，歸納得出各個組別的類型

#3.1 回歸分析
library(car)

df_agg <- df %>%
  group_by(Month, Hour, product_detail, Size, store_id, unit_price) %>%
  summarise(
    total_qty = sum(transaction_qty),
    total_bill = sum(Total_Bill),
    avg_qty = mean(transaction_qty),
  ) %>%
  ungroup()
#對數據進行聚合，生成了一個新的total_qty以供後續回歸與隨機森林建模

train_number <- round(length(df_agg$total_qty) * 2 / 3)
set.seed(123)
train_rows <- sample(1:length(df_agg$total_qty), train_number)
df_agg_train <- df_agg[train_rows,]
df_agg_test <- df_agg[-train_rows,]
#對數據進行訓練集和測試集的切割，訓練集被用於訓練各類回歸模型以及隨機森林模型

table(df_agg_train$Hour)
ggplot(df_agg_train, aes(x = Hour, y = total_qty)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "darkred", size = 2) +
  labs(
    title = "每小時總銷量（total_qty by Hour）",
    x = "小時（Hour）",
    y = "總銷量（Total Quantity）"
  ) +
  theme_minimal()
#解釋分析爲什麽把hour作爲一個重要的變量進行處理：作爲定量變量的hour在此處綫性關係顯然不成立

model1 <- lm(total_qty ~ Hour + I(Hour^2) + product_detail + Size + store_id + unit_price, data = df_agg_train)
summary(model1)
vif(model1)
model1 <- lm(total_qty ~ Hour + I(Hour^2) + Size + store_id + unit_price, data = df_agg_train)
summary(model1)
vif(model1)
op=par(mfrow=c(2,2))
plot(model1)
par(op)
#這裏model1中Hour和Hour^2的多重共綫性很强，但這是出於特殊處理的目的
#這裏是構造多項式回歸，根據圖片中回歸診斷認爲殘差略微偏離正態分佈，且偏離真值的量隨著真值增大有較明顯的增大趨勢

df_agg_train <- df_agg_train %>%
  mutate(hour_group = case_when(
    Hour == 6 ~ "Early Bird",
    Hour >= 7 & Hour < 11 ~ "Mid-Morning",
    Hour >= 11 & Hour < 20 ~ "Mid-Day",
    Hour == 20 ~ "Evening Low",
    TRUE ~ "Other"
  ))

model2 <- lm(total_qty ~ hour_group + product_detail + Size + store_id + unit_price, data = df_agg_train)
summary(model2)
vif(model2)
model2 <- lm(total_qty ~ hour_group + Size + store_id + unit_price, data = df_agg_train)
summary(model2)
vif(model2)
op=par(mfrow=c(2,2))
plot(model2)
par(op)
#根據圖像特徵人工將hour轉化爲定性變量進行綫性回歸，R^2得到了提升，殘差也更偏進正態，但是殘差明顯隨著真值增大而偏離量和方差增加

library(splines2)

model3 <- lm(total_qty ~ splines::ns(Hour, df = 3) + product_detail + Size + store_id + unit_price, data = df_agg_train)
summary(model3)
vif(model3)
model3 <- lm(total_qty ~ splines::ns(Hour, df = 3) + Size + store_id + unit_price, data = df_agg_train)
summary(model3)
vif(model3)
AIC(model3)
BIC(model3)

model3 <- lm(total_qty ~ splines::ns(Hour, df = 4) + product_detail + Size + store_id + unit_price, data = df_agg_train)
summary(model3)
vif(model3)
model3 <- lm(total_qty ~ splines::ns(Hour, df = 4) + Size + store_id + unit_price, data = df_agg_train)
summary(model3)
vif(model3)
AIC(model3)
BIC(model3)

model3 <- lm(total_qty ~ splines::ns(Hour, df = 5) + product_detail + Size + store_id + unit_price, data = df_agg_train)
summary(model3)
vif(model3)
model3 <- lm(total_qty ~ splines::ns(Hour, df = 5) + Size + store_id + unit_price, data = df_agg_train)
summary(model3)
vif(model3)
AIC(model3)
BIC(model3)

model3 <- lm(total_qty ~ splines::ns(Hour, df = 6) + product_detail + Size + store_id + unit_price, data = df_agg_train)
summary(model3)
vif(model3)
model3 <- lm(total_qty ~ splines::ns(Hour, df = 6) + Size + store_id + unit_price, data = df_agg_train)
summary(model3)
vif(model3)
AIC(model3)
BIC(model3)
#由Adjusted R^2得到：最優的spline擬合應當是4或5；但由AIC/BIC可知：最優的spline擬合應當是5

model3 <- lm(total_qty ~ splines::ns(Hour, df = 5) + Size + store_id + unit_price, data = df_agg_train)
knots <- attr(splines::ns(df_agg_train$Hour, df = 5), "knots")
model3 <- lm(total_qty ~ splines::ns(Hour, df = 5, knots = c(7,9,11,19)) + Size + store_id + unit_price, data = df_agg_train)
#更優的人工處理
summary(model3)
op=par(mfrow=c(2,2))
plot(model3)
par(op)
#根據殘差圖，認爲樣條回歸相較於前兩個更符合綫性回歸關於殘差的假定

data.frame(
  model = c("poly", "categorical", "spline"),
  adj_r2 = c(summary(model1)$adj.r.squared,
             summary(model2)$adj.r.squared,
             summary(model3)$adj.r.squared),
  AIC = c(AIC(model1), AIC(model2), AIC(model3)),
  BIC = c(BIC(model1), BIC(model2), BIC(model3))
)
#認爲spline擬合的效果最好。
#儘管本研究模型僅解釋了約 31.6% 的變異，但考慮到消費行為的主觀性與多重外部影響，該模型仍具有實務價值。模型變數皆達顯著水準，並且不存在明顯多重共線性，顯示所選特徵在統計上具備穩定性與預測力。

#3.2 建立隨機森林模型
library(randomForest)
df_agg_train <- subset(df_agg_train, select = -hour_group)
df_agg_train$product_detail <- as.factor(df_agg_train$product_detail)
df_agg_train$Size <- as.factor(df_agg_train$Size)
df_agg_test$product_detail <- as.factor(df_agg_test$product_detail)
df_agg_test$Size <- as.factor(df_agg_test$Size)

set.seed(123)
rf_model <- randomForest(
  total_qty ~ Hour + unit_price + product_detail + Size + store_id + Month,
  data = df_agg_train,
  ntree = 500,
  importance = TRUE
)

print(rf_model)

importance(rf_model)
varImpPlot(rf_model)

#3.3 對上述的回歸模型及隨機森林預測模型進行測試

pred_spline <- predict(model3, newdata = df_agg_test)
pred_rf <- predict(rf_model, newdata = df_agg_test)

R2 <- function(y_true, y_pred) {
  ss_res <- sum((y_true - y_pred)^2)
  ss_tot <- sum((y_true - mean(y_true))^2)
  1 - ss_res / ss_tot
}

eval_model <- function(true, pred) {
  data.frame(RMSE = sqrt(mean((df_agg_test$total_qty - pred) ^ 2)), R2 = R2(pred, true))
}
res_spline <- eval_model(df_agg_test$total_qty, pred_spline)
res_rf <- eval_model(df_agg_test$total_qty, pred_rf)
#隨機森林模型的預測效果很不錯(R2>0.3)，但spline的預測效果較爲糟糕

#以上内容为第三部分(沈新)，本人的代码中多有冗余部分，实在非常抱歉！以上的数据主要采用的是数据集3，数据集3是在数据集1的基础上由我人工挑选出Category为Coffee的部分进行加工而成的。