# Modeling Click-Through Rate and Conversion Probability in Online Advertising Using Regression Analysis

# data 출처 
# install.packages("summarytools")
# setting 

rm(list=ls())
getwd()


# library 
library(readr)
library(dplyr)
library(car)
library(summarytools)
library(boot)


# data download 
setwd('C:\\융융\\2025\\교환 준비\\수업 서류\\통계적 추론\\project')


# data Check 
df = read.csv('criteo_sample2.csv')

# train test split 
n <- nrow(df)
split_idx <- floor(0.7 * n)

df_train <- df[1:split_idx, ]
df_test  <- df[(split_idx + 1):n, ]

# data save 
write.csv(df_train, "df_train.csv", row.names = FALSE)
write.csv(df_test, "df_test.csv", row.names = FALSE)



# train data check 
sum(is.na(df_train)) # find missing value -> 0 
str(df_train)
dim(df_train) # 7000   22
head(df_train,10) 

# summary 
summary(df_train)
print(dfSummary(df_train, method='render'))

# data type conversion
table(df_train$conversion)
table(df_train$attribution)
table(df_train$click)
barplot(table(df_train$click_pos))
barplot(table(df_train$cat1))

# data type conversion to factor 
df_train$conversion<-as.factor(df_train$conversion)
df_train$attribution<-as.factor(df_train$attribution)
df_train$click<-as.factor(df_train$click)
df_train$campaign<-as.factor(df_train$campaign)

# data type conversion cat1-9 
cat_vars <- paste0("cat", 1:9)
df_train[cat_vars] <- lapply(df_train[cat_vars], as.factor)

# data type conversion to num 
df_train$timestamp <- as.numeric(df_train$timestamp)


str(df_train)


# 데이터 셋 가공 

# 의미 기반 변수 선택 

df_CTR <- df_train %>%
  select(-uid,-conversion_timestamp,-conversion_id,-cpo ,-conversion, -attribution,-click_pos,-click_nb)


str(df_CTR)

# 변수 변환 tslc ,has~ 생성 
df_CTR$has_prev_click <- as.factor(df_CTR$time_since_last_click != -1)
df_CTR$tslc <- ifelse(df_CTR$time_since_last_click == -1,
                      0,
                      df_CTR$time_since_last_click)
df_CTR<-df_CTR%>%select(-time_since_last_click)


write.csv(df_CTR, "df_CTR1.csv", row.names = FALSE)







############# EDA


# 종속 변수의 비율 
barplot(table(df_CTR$click))

# 독립 변수의 분포 
pairs(df_CTR[, c( "cost", "tslc","timestamp")])

barplot(table(df_CTR$has_prev_click))
barplot(table(df_CTR$campaign))

# 변수들 간의 상관관계 확인 


# 범주형 변수 처리 

# campaign -> campaign_cumcnt로 누적 노출량 변수 만들기 
df_CTR$campaign_cumcnt <- ave(
  rep(1L, nrow(df_CTR)),
  df_CTR$campaign,
  FUN = cumsum
) - 1L

summary(df_CTR)

# 특정 campaign 하나 확인
subset(df_CTR, campaign == levels(df_CTR$campaign)[1])[, 
                                          c("timestamp", "campaign", "campaign_cumcnt")][1:5, ]


# cat1-9 변수 count encoding 하기 

# cat 변수 이름
cat_cols <- paste0("cat", 1:9)

# level 개수 계산
cat_levels <- data.frame(
  variable = cat_cols,
  n_levels = sapply(df_CTR[cat_cols], nlevels)
)

cat_levels


# cat3,5,7 빈도 인코딩 
count_encode <- function(x) {
  tab <- table(x)
  as.numeric(tab[x])
}
df_CTR$cat1_cnt<- count_encode(df_CTR$cat1)
df_CTR$cat2_cnt<- count_encode(df_CTR$cat2)
df_CTR$cat3_cnt <- count_encode(df_CTR$cat3)
df_CTR$cat4_cnt <- count_encode(df_CTR$cat4)
df_CTR$cat5_cnt <- count_encode(df_CTR$cat5)
df_CTR$cat6_cnt <- count_encode(df_CTR$cat6)
df_CTR$cat7_cnt <- count_encode(df_CTR$cat7)
df_CTR$cat8_cnt <- count_encode(df_CTR$cat8)
df_CTR$cat9_cnt <- count_encode(df_CTR$cat9)

str(df_CTR)

######### test data 적용 

### 0) df_test에서도 train과 똑같이 dtype 맞추기
df_test$conversion    <- as.factor(df_test$conversion)
df_test$attribution   <- as.factor(df_test$attribution)
df_test$click         <- as.factor(df_test$click)
df_test$campaign      <- as.factor(df_test$campaign)
cat_vars <- paste0("cat", 1:9)
df_test[cat_vars] <- lapply(df_test[cat_vars], as.factor)
df_test$timestamp <- as.numeric(df_test$timestamp)

### 1) CTR용 컬럼만 뽑기 (train에서 한 것과 동일)
df_CTR_test <- df_test %>%
  select(-uid,-conversion_timestamp,-conversion_id,-cpo,-conversion,-attribution,-click_pos,-click_nb)

### 2) has_prev_click, tslc 만들기 (train과 동일)
df_CTR_test$has_prev_click <- as.factor(df_CTR_test$time_since_last_click != -1)
df_CTR_test$tslc <- ifelse(df_CTR_test$time_since_last_click == -1, 0, df_CTR_test$time_since_last_click)
df_CTR_test <- df_CTR_test %>% select(-time_since_last_click)

### 3) factor level 정렬: 반드시 train 기준으로 맞추기
# (특히 stepwise 모델에 cat factor가 남아있을 수 있으면 필수)
df_CTR_test$has_prev_click <- factor(df_CTR_test$has_prev_click, levels = levels(df_CTR$has_prev_click))
df_CTR_test$campaign <- factor(df_CTR_test$campaign, levels = levels(df_CTR$campaign))

for (cc in cat_vars) {
  df_CTR_test[[cc]] <- factor(df_CTR_test[[cc]], levels = levels(df_CTR[[cc]]))
}

### 4) campaign_cumcnt: test에서도 "시간 순서 기준 누적 노출량" 만들기
# 주의: 반드시 timestamp 정렬이 보장되어야 함
df_CTR_test <- df_CTR_test %>%
  arrange(timestamp) %>%
  mutate(campaign_cumcnt = ave(rep(1L, n()), campaign, FUN = cumsum) - 1L)

### 5) count encoding: train에서 만든 빈도 맵으로 lookup
make_count_map <- function(x_factor_train) {
  tab <- table(x_factor_train)
  tab
}
lookup_count <- function(x_factor_test, tab) {
  out <- as.numeric(tab[as.character(x_factor_test)])
  out[is.na(out)] <- 0
  out
}

# train에서 만든 map
cat_tabs <- lapply(df_CTR[cat_vars], make_count_map)
names(cat_tabs) <- cat_vars

# test에 적용
for (cc in cat_vars) {
  df_CTR_test[[paste0(cc, "_cnt")]] <- lookup_count(df_CTR_test[[cc]], cat_tabs[[cc]])
}

### 6) 로그 변환: train과 같은 방식(같은 함수/같은 상수)
df_CTR_test$tslc_log <- log1p(df_CTR_test$tslc)
df_CTR_test$log_cost <- log1p(df_CTR_test$cost)
df_CTR_test$campaign_cumcnt_log <- log1p(df_CTR_test$campaign_cumcnt)

str(df_CTR_test)


########## 모델링 
str(df_CTR)

# 로지스틱 회귀 분석 

mod_ctr1 <- glm(
  click ~
    cost +
    has_prev_click +
    tslc +
    campaign_cumcnt+
    cat1 + cat2 + cat3_cnt + cat4 + cat5_cnt +
    cat6 + cat7_cnt + cat8 + cat9,
  data = df_CTR,
  family = binomial
)

summary(mod_ctr1)

## 모델 2
mod_ctr2 <- glm(
  click ~
    cost +
    has_prev_click +
    tslc +
    campaign_cumcnt+
    cat1_cnt + cat2_cnt + cat3_cnt + cat4_cnt + cat5_cnt +
    cat6_cnt + cat7_cnt + cat8_cnt+ cat9_cnt,
  data = df_CTR,
  family = binomial
)

summary(mod_ctr2)




# campaign_cumcnt 변수 로그 변환하기 

hist(df_CTR$campaign_cumcnt)
boxplot(df_CTR$campaign_cumcnt)
df_CTR$campaign_cumcnt_log <- log1p(df_CTR$campaign_cumcnt)

hist(df_CTR$campaign_cumcnt_log)
boxplot(df_CTR$campaign_cumcnt_log)


## tslc 변수 로그 변환하기 
hist(df_CTR$tslc)
df_CTR$tslc_log <- log1p(df_CTR$tslc)
hist(df_CTR$tslc_log)


# cost 변수 로그 변환하기 
hist(df_CTR$cost)
df_CTR$log_cost <- log1p(df_CTR$cost)
hist(df_CTR$cost_log)


## baseline 모델 3
mod_ctr3 <- glm(
  click ~
    log_cost+
    has_prev_click +
    tslc_log +
    campaign_cumcnt_log+
    cat1_cnt + cat2_cnt + cat3_cnt + cat4_cnt + cat5_cnt +
    cat6_cnt + cat7_cnt + cat8_cnt+ cat9_cnt,
  data = df_CTR,
  family = binomial
)

summary(mod_ctr3)


# step wise 방법 쓰기 

null.model <- glm(click ~ 1, data = df_CTR, family = binomial) 
step.model <- step(null.model, scope = list(upper = formula(mod_ctr3)), direction = "both")

summary(step.model) 

# 상호작용 term 추가하기 
step2<-update(step.model, . ~ .  + log_cost*tslc_log)
summary(step2)

# 상호작용 term 추가하기
step3<-update(step2, . ~ .  + log_cost*has_prev_click)
summary(step3)

# 상호작용 term 추가하기-> step3 까지만 하자 
step4<-update(step3, . ~ .  + tslc_log*has_prev_click)
summary(step4)

# 모델 평가 

# cross-validation 
cv.glm(df_CTR, mod_ctr3, K = 5)$delta
cv.glm(df_CTR, step.model, K = 5)$delta
cv.glm(df_CTR, step2, K = 5)$delta
cv.glm(df_CTR, step3, K = 5)$delta
cv.glm(df_CTR, step4, K = 5)$delta

# 10- cross -validation 
model.list <- list(mod_ctr3, step.model, step2, step3)
model.list[[4]]

all.cv10 <- double(4)
for(i in 1:4) {
  all.cv10[i] <- cv.glm(df_CTR, model.list[[i]], K = 10)$delta[1]
}

all.cv10

### 7) 최종 모델(step3) 예측 및 AUC(테스트에서)
prob_test <- predict(step3, newdata = df_CTR_test, type = "response")

preds50 <- prob_test > 0.5
table(preds = preds50, true = df_CTR_test$click)
mean(preds50 == df_CTR_test$click)
# (선택) AUC
# library(pROC)
roc_obj <- roc(df_CTR_test$click, prob_test)
auc(roc_obj)



library(pROC)
glm.roc <- roc(df_CTR_test$click ~ prob_test, plot = TRUE, print.auc = TRUE)


# 모델 5 
mod_ctr5<-update(mod_ctr4, . ~ .  + log_cost*has_prev_click)
summary(mod_ctr5)

#




# 5-cross-validation 
cv.glm(df_CTR, mod_ctr3, K = 5)$delta
cv.glm(df_CTR, mod_ctr2, K = 5)$delta
cv.glm(df_CTR, mod_ctr4, K = 5)$delta
cv.glm(df_CTR, mod_ctr5, K = 5)$delta

# 10- cross -validation 
model.list <- list(mod1, mod2, mod3, mod4, mod5)






# 로그 변환 관련 그래프 
plot(ecdf(df_CTR$log_cost),
     main = "ECDF of cost",
     xlab = "cost",
     ylab = "F(cost)")

boxplot(df_CTR$log_cost,
        horizontal = TRUE,
        main = "Boxplot of cost (original scale)")

hist(log1p(df_CTR$cost),
     main = "Histogram of log(1 + cost)",
     xlab = "log(1 + cost)")

str(df_CTR)













# click 범주형 변수 처리 
barplot(table(df_CTR$click))

  # 클래스 별 X 분포 
  df_CTR_1 <- subset(df_CTR, click == 1)
  table(df_CTR$time_since_last_click)

# conversion 범주형 변수 처리 
barplot(table(df$conversion)) # 구매한 사람과 안 한 사람 9430: 570

  ## 전환이 발생한 행들을 따로 데이터 프레임을 만들어서 분석
  df_conversion <- subset(df, conversion == 1)
  df_conversion
  dim(df_conversion)
  
  ## 구매한 사람들 중 광고가 영향을 끼친 행들 분석 
   table(df_conversion$attribution)   ## 328명 
  
  ## 328명에 대해서 기여도 계산하기 
   sum(table(df_conversion$click_pos))%>%barplot()

# 전환이 발생한 행들을 좀 더 분석해야할듯 
hist(table(df$conversion_timestamp))

# 기여가 발생한 행 분석 해야함 
table(df$attribution) # 9672: 328 

# 상품 구매 전 광고 클릭 수 분석 
barplot(table(df$click_pos))
df_click <- subset(df, click_pos != -1)
tab <- table(df_click$click_pos)
barplot(tab)





