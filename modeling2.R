####### CVR Modeling 
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
df_train=read.csv('df_train.csv')
df_test=read.csv('df_test.csv')

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
df_train$uid<-as.factor(df_train$uid)
df_train$conversion_id<-as.factor(df_train$conversion_id)

# data type conversion cat1-9 
cat_vars <- paste0("cat", 1:9)
df_train[cat_vars] <- lapply(df_train[cat_vars], as.factor)

# data type conversion to num 
df_train$timestamp <- as.numeric(df_train$timestamp)

# 변수 변환 tslc ,has~ 생성 
df_train$has_prev_click <- as.factor(df_train$time_since_last_click != -1)
df_train$tslc <- ifelse(df_train$time_since_last_click == -1,
                        0,
                        df_train$time_since_last_click)


str(df_train)

################# CVR 데이터 셋 가공 

# df_CVR 만들기 
df_CVR <- subset(df_train, click == 1)
str(df_CVR)
dim(df_CVR) # 2587* 22 

summary(df_CVR)


############# 모델링 전 변수선택 

df_CVR <- df_CVR %>%
  select(-conversion_timestamp,-conversion_id,-cpo , -attribution,-click_pos,-click_nb,-time_since_last_click)

str(df_CVR)
summary(df_CVR)


############### df_CVR Train data 모델링 전 변수 변환 

# campaign -> campaign_cumcnt로 누적 노출량 변수 만들기 
df_CVR$campaign_cumcnt <- ave(
  rep(1L, nrow(df_CVR)),
  df_CVR$campaign,
  FUN = cumsum
) - 1L

# 특정 campaign 하나 확인
subset(df_CVR, campaign == levels(df_CVR$campaign)[1])[, 
                                                       c("timestamp", "campaign", "campaign_cumcnt")][1:5, ]


# uid -> uid_cumcnt로 누적 노출량 변수 만들기 
df_CVR$uid_cumcnt <- ave(
  rep(1L, nrow(df_CVR)),
  df_CVR$uid,
  FUN = cumsum
) - 1L
summary(df_CVR)

# 특정 uid 하나 확인
subset(df_CVR, uid == unique(df_CVR$uid)[10])[,c("timestamp", "uid", "uid_cumcnt")][1:5, ]



# cat1-9 변수 count encoding 하기 

# cat 변수 이름
cat_cols <- paste0("cat", 1:9)

# level 개수 계산
cat_levels <- data.frame(
  variable = cat_cols,
  n_levels = sapply(df_CVR[cat_cols], function(x) length(unique(x)))
)

cat_levels

length(unique(df_CVR$cat7))





# cat 1-9 빈도 인코딩 
count_encode <- function(x) {
  tab <- table(x)
  as.numeric(tab[x])
}
df_CVR$cat1_cnt<- count_encode(df_CVR$cat1)
df_CVR$cat2_cnt<- count_encode(df_CVR$cat2)
df_CVR$cat3_cnt <- count_encode(df_CVR$cat3)
df_CVR$cat4_cnt <- count_encode(df_CVR$cat4)
df_CVR$cat5_cnt <- count_encode(df_CVR$cat5)
df_CVR$cat6_cnt <- count_encode(df_CVR$cat6)
df_CVR$cat7_cnt <- count_encode(df_CVR$cat7)

df_CVR$cat8_cnt <- count_encode(df_CVR$cat8)
df_CVR$cat9_cnt <- count_encode(df_CVR$cat9)

str(df_CVR)



# campaign_cumcnt 변수 로그 변환하기 

hist(df_CVR$campaign_cumcnt)
boxplot(df_CVR$campaign_cumcnt)
df_CVR$campaign_cumcnt_log <- log1p(df_CVR$campaign_cumcnt)

hist(df_CVR$campaign_cumcnt_log)
boxplot(df_CVR$campaign_cumcnt_log)


## tslc 변수 로그 변환하기 
hist(df_CVR$tslc)
df_CVR$tslc_log <- log1p(df_CVR$tslc)
hist(df_CVR$tslc_log)


# cost 변수 로그 변환하기 
hist(df_CVR$cost)
df_CVR$cost_log<- log1p(df_CVR$cost)
hist(df_CVR$cost_log)

# uid_cum 변수 로그  변환하기 
hist(df_CVR$uid_cumcnt)
df_CVR$uid_cumcnt_log<- log1p(df_CVR$uid_cumcnt)
hist(df_CVR$uid_cumcnt_log)

############# test data 전처리 
## =========================
## 0) test 데이터 dtype 맞추기 (train과 동일)
## =========================
df_test$conversion    <- as.factor(df_test$conversion)
df_test$attribution   <- as.factor(df_test$attribution)
df_test$click         <- as.factor(df_test$click)
df_test$campaign      <- as.factor(df_test$campaign)
df_test$uid           <- as.factor(df_test$uid)
df_test$conversion_id <- as.factor(df_test$conversion_id)

cat_vars <- paste0("cat", 1:9)
df_test[cat_vars] <- lapply(df_test[cat_vars], as.factor)

df_test$timestamp <- as.numeric(df_test$timestamp)

## has_prev_click, tslc 생성 (train과 동일 로직)
df_test$has_prev_click <- as.factor(df_test$time_since_last_click != -1)
df_test$tslc <- ifelse(df_test$time_since_last_click == -1, 0, df_test$time_since_last_click)


## =========================
## 1) CVR용 데이터셋 만들기: click==1만
## =========================
df_CVR_test <- subset(df_test, click == 1)

## (선택) timestamp 기준 정렬: 누적카운트 의미 보장
df_CVR_test <- df_CVR_test %>% arrange(timestamp)


## =========================
## 2) train과 동일하게 컬럼 제거
## =========================
df_CVR_test <- df_CVR_test %>%
  select(-conversion_timestamp, -conversion_id, -cpo,
         -attribution, -click_pos, -click_nb, -time_since_last_click)


## =========================
## 3) factor level을 train 기준으로 맞추기 (매우 중요)
##    - 새로운 레벨은 NA가 될 수 있음 → 아래에서 처리
## =========================
df_CVR_test$campaign       <- factor(df_CVR_test$campaign, levels = levels(df_CVR$campaign))
df_CVR_test$uid            <- factor(df_CVR_test$uid,      levels = levels(df_CVR$uid))
df_CVR_test$has_prev_click <- factor(df_CVR_test$has_prev_click, levels = levels(df_CVR$has_prev_click))

for (cc in cat_vars) {
  df_CVR_test[[cc]] <- factor(df_CVR_test[[cc]], levels = levels(df_CVR[[cc]]))
}

## 만약 factor 맞추면서 NA가 생겼다면(=test에만 있던 새 레벨),
## 간단히 NA로 두고 count encoding에서 0 처리하도록 할 거야.
## (혹은 명시적으로 "Unknown" 레벨을 만들 수도 있는데, 여기서는 0처리 방식)


## =========================
## 4) 누적 카운트 변수 생성 (train과 동일)
##    - timestamp 정렬된 상태에서 cumsum
## =========================
df_CVR_test$campaign_cumcnt <- ave(rep(1L, nrow(df_CVR_test)),
                                   df_CVR_test$campaign,
                                   FUN = cumsum) - 1L

df_CVR_test$uid_cumcnt <- ave(rep(1L, nrow(df_CVR_test)),
                              df_CVR_test$uid,
                              FUN = cumsum) - 1L


## =========================
## 5) count encoding: train 빈도표로 lookup (누수/불일치 방지)
## =========================
make_count_tab <- function(x_factor_train) table(x_factor_train)

lookup_count <- function(x_factor_test, tab) {
  out <- as.numeric(tab[as.character(x_factor_test)])
  out[is.na(out)] <- 0  # test에만 있는 새 레벨/NA는 0으로
  out
}

# train에서 만든 빈도표
cat_tabs <- lapply(df_CVR[cat_vars], make_count_tab)
names(cat_tabs) <- cat_vars

# test에 적용
for (cc in cat_vars) {
  df_CVR_test[[paste0(cc, "_cnt")]] <- lookup_count(df_CVR_test[[cc]], cat_tabs[[cc]])
}


## =========================
## 6) log 변환 (train과 동일)
## =========================
df_CVR_test$campaign_cumcnt_log <- log1p(df_CVR_test$campaign_cumcnt)
df_CVR_test$uid_cumcnt_log      <- log1p(df_CVR_test$uid_cumcnt)
df_CVR_test$tslc_log            <- log1p(df_CVR_test$tslc)
df_CVR_test$cost_log            <- log1p(df_CVR_test$cost)


## =========================
## 7) (선택) 원본 cat 변수는 모델에 안 쓰면 제거해도 됨
##    - 네 모델이 cat*_cnt만 쓰니까 정리 가능
## =========================
# df_CVR_test <- df_CVR_test %>% select(-all_of(cat_vars))

str(df_CVR_test)
summary(df_CVR_test)

############# 모델링 
pCVR_mod <- glm(
  conversion ~ cost_log + tslc_log + has_prev_click +
    uid_cumcnt_log + campaign_cumcnt_log +
    cat1_cnt + cat2_cnt + cat3_cnt + cat4_cnt + cat5_cnt + cat6_cnt + cat7_cnt + cat8_cnt + cat9_cnt,
  data = df_CVR,
  family = binomial
)

summary(pCVR_mod)



# step wise 
null.model <- glm(conversion ~ 1, data = df_CVR, family = binomial) 
step.model <- step(null.model, scope = list(upper = formula(pCVR_mod)), direction = "both")


summary(step.model)

# 상호작용 추가 
step2<-update(step.model, . ~ .  + has_prev_click*cost_log)
summary(step2)

# 상호작용 추가 -> back 
step3<-update(step2, . ~ .  + has_prev_click*uid_cumcnt_log)
summary(step3)

# 상호작용 추가 
step4<-update(step2, . ~ .  + cost_log*uid_cumcnt_log)
summary(step4)



################ 모델 평가 

# cross-validation 
cv.glm(df_CVR, pCVR_mod, K = 5)$delta
cv.glm(df_CVR, step.model, K = 5)$delta
cv.glm(df_CVR, step2, K = 5)$delta
cv.glm(df_CVR, step3, K = 5)$delta
cv.glm(df_CVR, step4, K = 5)$delta

# 10- cross -validation 
model.list <- list(pCVR_mod, step.model, step2, step3,step4)
model.list[[5]]

all.cv10 <- double(5)
for(i in 1:5) {
  all.cv10[i] <- cv.glm(df_CVR, model.list[[i]], K = 10)$delta[1]
}

all.cv10

### 7) 최종 모델(step3) 예측 및 AUC(테스트에서)
prob_test <- predict(step3, newdata = df_CVR_test, type = "response")

preds50 <- prob_test > 0.5
table(preds = preds50, true = df_CVR_test$conversion)
mean(preds50 == df_CVR_test$conversion)

# (선택) AUC
library(pROC)
roc_obj <- roc(df_CVR_test$conversion, prob_test)
auc(roc_obj)



library(pROC)
glm.roc <- roc(df_CVR_test$conversion ~ prob_test, plot = TRUE, print.auc = TRUE)






# 5-cross-validation 
cv.glm(df_CTR, mod_ctr3, K = 5)$delta
cv.glm(df_CTR, mod_ctr2, K = 5)$delta
cv.glm(df_CTR, mod_ctr4, K = 5)$delta
cv.glm(df_CTR, mod_ctr5, K = 5)$delta

# 10- cross -validation 
model.list <- list(mod1, mod2, mod3, mod4, mod5)






################ conversion delay model
library(survival)
### 생존 분석 변수 만들기 

df_CVR_Delay<-subset(df_train,click==1)
head(df_CVR_Delay)

str(df_CVR_Delay) # 2587 *32 
hist(df_CVR_Delay$timestamp)



# delay_time 계산

t_end <- max(df_CVR_Delay$timestamp, na.rm=TRUE)
t_end

df_CVR_Delay$delay_time <- ifelse(
  df_CVR_Delay$conversion == 1 &
    !is.na(df_CVR_Delay$conversion_timestamp) &
    df_CVR_Delay$conversion_timestamp >= 0,
  df_CVR_Delay$conversion_timestamp - df_CVR_Delay$timestamp,
  t_end - df_CVR_Delay$timestamp
)


# sanity check
summary(df_CVR_Delay$delay_time)

######### 변수 변환 

# campaign -> campaign_cumcnt로 누적 노출량 변수 만들기 
df_CVR_Delay$campaign_cumcnt <- ave(
  rep(1L, nrow(df_CVR_Delay)),
  df_CVR_Delay$campaign,
  FUN = cumsum
) - 1L

# 특정 campaign 하나 확인
subset(df_CVR_Delay, campaign == levels(df_CVR_Delay$campaign)[1])[, 
                                                       c("timestamp", "campaign", "campaign_cumcnt")][1:5, ]


# uid -> uid_cumcnt로 누적 노출량 변수 만들기 
df_CVR_Delay$uid_cumcnt <- ave(
  rep(1L, nrow(df_CVR_Delay)),
  df_CVR_Delay$uid,
  FUN = cumsum
) - 1L
summary(df_CVR_Delay)

# 특정 uid 하나 확인
subset(df_CVR_Delay, uid == unique(df_CVR_Delay$uid)[10])[,c("timestamp", "uid", "uid_cumcnt")][1:5, ]



# cat1-9 변수 count encoding 하기 

# cat 변수 이름
cat_cols <- paste0("cat", 1:9)


# level 개수 계산
cat_levels <- data.frame(
  variable = cat_cols,
  n_levels = sapply(df_CVR_Delay[cat_cols], function(x) length(unique(x)))
)

cat_levels

length(unique(df_CVR_Delay$cat7))





# cat 1-9 빈도 인코딩 
count_encode <- function(x) {
  tab <- table(x)
  as.numeric(tab[x])
}
df_CVR_Delay$cat1_cnt<- count_encode(df_CVR_Delay$cat1)
df_CVR_Delay$cat2_cnt<- count_encode(df_CVR_Delay$cat2)
df_CVR_Delay$cat3_cnt <- count_encode(df_CVR_Delay$cat3)
df_CVR_Delay$cat4_cnt <- count_encode(df_CVR_Delay$cat4)
df_CVR_Delay$cat5_cnt <- count_encode(df_CVR_Delay$cat5)
df_CVR_Delay$cat6_cnt <- count_encode(df_CVR_Delay$cat6)
df_CVR_Delay$cat7_cnt <- count_encode(df_CVR_Delay$cat7)
df_CVR_Delay$cat8_cnt <- count_encode(df_CVR_Delay$cat8)
df_CVR_Delay$cat9_cnt <- count_encode(df_CVR_Delay$cat9)

str(df_CVR_Delay)



# campaign_cumcnt 변수 로그 변환하기 

hist(df_CVR_Delay$campaign_cumcnt)
boxplot(df_CVR_Delay$campaign_cumcnt)
df_CVR_Delay$campaign_cumcnt_log <- log1p(df_CVR_Delay$campaign_cumcnt)

hist(df_CVR_Delay$campaign_cumcnt_log)
boxplot(df_CVR_Delay$campaign_cumcnt_log)


## tslc 변수 로그 변환하기 
hist(df_CVR_Delay$tslc)
df_CVR_Delay$tslc_log <- log1p(df_CVR_Delay$tslc)
hist(df_CVR_Delay$tslc_log)


# cost 변수 로그 변환하기 
hist(df_CVR_Delay$cost)
df_CVR_Delay$cost_log<- log1p(df_CVR_Delay$cost)
hist(df_CVR_Delay$cost_log)

# uid_cum 변수 로그  변환하기 
hist(df_CVR_Delay$uid_cumcnt)
df_CVR_Delay$uid_cumcnt_log<- log1p(df_CVR_Delay$uid_cumcnt)
hist(df_CVR_Delay$uid_cumcnt_log)


######### KM plot 확인하기 

fit_has_prev_click <- survfit(Surv(delay_time, conversion)~ ., df_CVR_Delay)
plot(fit_has_prev_click, conf.int=F,ylab="survival probability", main="cat1", col=1:9)


fit_click_nb <- survfit(Surv(delay_time, conversion)~click_nb, df_CVR_Delay)

plot(fit_click_nb, conf.int=F,ylab="survival probability", main="cat1", col=1:7)


######## 변수 선택하기 
## 전환을 빠르게 만드는데 유의한 변수가 뭘까? 
fit_cox_final <- coxph(
  Surv(delay_time, conversion) ~ 
    campaign_cumcnt_log +
    uid_cumcnt_log +
    cat1_cnt +
    has_prev_click,
  data = df_CVR_Delay,
  ties = "efron"
)
summary(fit_cox_final)


########## 모델 비교하기 
df_CVR_Delay$conversion <- as.numeric(as.character(df_CVR_Delay$conversion)) == 1

fit_cox <- coxph(Surv(delay_time, conversion)~ has_prev_click + cost_log + uid_cumcnt_log+campaign_cumcnt_log, ties="efron",data=df_CVR_Delay)

#And check whether the proportional hazard assumption holds.
#H_0: The proportional assumption holds vs H_1: not H_0 
cox.zph(fit_cox)

# Interpret the parameter estimate of the treatment. 
summary(fit_cox)
coxph(Surv(delay_time, conversion) ~ has_prev_click + cost_log + uid_cumcnt_log+ campaign_cumcnt_log+cat1_cnt, data=df_CVR_Delay)




unique(df_CVR_Delay$campaign)%>%length
summary(df_CVR_Delay)

# conversion-> event df_CVR_Delay$delay_time

## 네 step3에 맞춘 변수(원하면 더 줄이거나 늘려도 됨)
form_delay <- Surv(delay_time, conversion) ~
  cat4_cnt + has_prev_click +
  cat1_cnt + cat2_cnt + cat7_cnt + cat5_cnt +
  cost_log + uid_cumcnt_log +
  has_prev_click:cost_log +
  has_prev_click:uid_cumcnt_log

aft_exp  <- survreg(df_CVR_Delay, dist="exponential", data=df_delay)
aft_wei  <- survreg(df_CVR_Delay, dist="weibull",     data=df_delay)
aft_lnor <- survreg(df_CVR_Delay, dist="lognormal",   data=df_delay)
aft_llog <- survreg(df_CVR_Delay, dist="loglogistic", data=df_delay)

AIC(aft_exp); AIC(aft_wei); AIC(aft_lnor); AIC(aft_llog)

## 예를 들어 loglogistic가 제일 낮다고 치면:
summary(aft_llog)


str(df_CVR_Delay)

library(survival)

cox_cvr <- coxph(
  Surv(delay_time, conversion) ~
    cost_log +
    has_prev_click +
    campaign_cumcnt_log +
    cat1_cnt + cat2_cnt + cat3_cnt,
  data = df_click
)

summary(cox_cvr)

















################# EDA 


# 상품을 구매한 사람이라면 몇 번째의 클릭만에 구매했는가
df_conv<-subset(df_CVR, conversion  == 1)
barplot(table(df_conv$click_nb))

# 구매까지 걸린 시간 

# 변수 선택 및 변수 확인 

## conversion_id
table(df_CVR$conversion) # 구매한 사람 400명 
table(df_CVR$conversion_id)

subset(df_CVR,conversion_id==19903738)
subset(df,uid ==26592070)

# 데이터 확인하기 
head(df_CVR)
unique(df_CVR$click_nb) #2310 

# conversion 범주형 변수 처리 
barplot(table(df_CVR$conversion)) # 클릭한 사람 중 구매한 사람과 안 한 사람 2187: 400 

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
barplot(tab)0
