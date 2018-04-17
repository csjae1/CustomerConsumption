# Package
install.packages("nparcomp")
install.packages("PMCMR")
install.packages("caret")

# 성별, 연령대, 거주지역, 구매 빈도수, 멤버십 가입 기간, 앱 이용 횟수 등을 통해
# 고객이 한번 방문 할때 얼마나 많은 금액을 쓸지 예측하는 것이 저희의 주제입니다.

library(caret)
library(data.table)
library(nortest) # ad.test 정규성검정정
library(nparcomp)
library(PMCMR)
library(psych)
library(readxl)
library(car)
library(gvlma)
library(ggplot2)
rm(list = ls())
gc()

# Data import
CustDF <- fread(input = "data/Cust.txt",
                sep = ",",
                header = TRUE)
VisitDF <- fread(input = "data/visit.txt",
                 sep = ",",
                 header = TRUE)
ItemDF <- fread(input = "data/Item.txt",
                sep = ",",
                header = TRUE)
MembershipDF <- fread(input =  "data/membership.txt",
                      sep = ",",
                      header = TRUE)
# post1 <- fread("projectData/우편.txt", header = TRUE, sep = "|", integer64 = "character")
# post2 <- fread("projectData/우편2.txt", header = TRUE, sep = "|", integer64 = "character")
# # 우편번호, 시도, 시군구
# post3 <- readxl::read_excel(path = "projectData/zipcode.xlsx", col_names = TRUE)
# post3 <- unique(post3[ , c(1,2,4)])
# 
# # 주소 변환
# l = list(post1, post2)
# post <- rbindlist(l, use.names = T, fill = T)
# post <- post[ ,c(1,3,5)]
# colnames(post) <- c("거주지역", "시도", "시군구")
# str(post)
# 
# post1 <- post[post$거주지역<10000, ]
# post2 <- post[post$거주지역>=10000, ]
# 
# post1 <- post1[ ,list(시도=substr(post1$거주지역,1,2), 주소)]
# post2 <- post2[ ,list(거주지역=substr(post2$거주지역,1,3), 주소)]
# l = list(post1, post2)
# 
# post <- rbindlist(l, use.names = T, fill = T)
# rm(post1, post2, l)
# 
# post <- unique(post)
# post$거주지역 <- as.integer(post$거주지역)
# 
# 
# CustDF <- merge(CustDF, post, by = "거주지역", all.x = TRUE)
# A계열사만 분석
ItemDF <- ItemDF[제휴사 == "A",]
# AllDF <- merge(ItemDF, CustDF, by = "고객번호", all.x = TRUE)
# 평균구매금액
price.mean <- ItemDF[ , .(총구매금액 = sum(구매금액)), by = "고객번호,영수증번호"]
price.mean <- price.mean[ , .(평균구매금액 = mean(총구매금액), 구매횟수=.N), by = "고객번호"]
# 총구매금액
price.sum <- ItemDF[ , .(총구매금액 = sum(구매금액)), by = "고객번호"]
CustDF <- merge(CustDF, price.mean, by = "고객번호", all.x = TRUE)
CustDF <- merge(CustDF, price.sum, by = "고객번호", all.x = TRUE)
# 총구매물품수
Item.sum <- ItemDF[ , .(총구매물품 = .N), by = "고객번호"]
CustDF <- merge(CustDF, Item.sum, by = "고객번호", all.x = TRUE)
rm(price.mean, Item.sum)
# log 평균금액
CustDF$log.mean.price <- log(CustDF$평균구매금액)
CustDF
# 로그인 횟수
head(VisitDF)
VisitDF <- VisitDF[제휴사 == "A_MOBILE/APP"]
VisitDF$제휴사 <- NULL
CustDF <- merge(CustDF, VisitDF, by = "고객번호", all.x = TRUE)
CustDF$이용횟수[is.na(CustDF$이용횟수)] <- 0
CustDF
# 맴버쉽 가입
year <- MembershipDF$가입년월 %/% 100
year <- 2015 - year
month <- MembershipDF$가입년월 %% 100
month <- 12-month
time <- year*12 + month
MembershipDF$time <- time

MembershipDF$멤버십명 <- NULL
MembershipDF$가입년월 <- NULL
CustDF <- merge(CustDF, MembershipDF, by = "고객번호", all.x = TRUE)
CustDF$time[is.na(CustDF$time)] <- 0
CustDF
rm(MembershipDF, time, month, year)
# 이용점포갯수 N
head(ItemDF)
stores <- ItemDF[ , .N, by = "고객번호,점포코드"]
stores <- stores[ , .N, by = "고객번호"]
stores
CustDF = merge(CustDF, stores, by = "고객번호", all.x = TRUE)
CustDF[is.na(CustDF)] <- 0
CustDF$거주지역 <- as.factor(CustDF$거주지역)
CustDF$성별 <- as.factor(CustDF$성별)
CustDF$연령대 <- as.factor(CustDF$연령대)
corr.test(총구매금액~., data = CustDF)
## Y : log(총구매금액)
CustDF$time <- NULL
CustDF <- CustDF[CustDF$총구매금액>0, ]
sum.lm <- lm(log(총구매금액) ~ ., data = CustDF)
summary(sum.lm) 
step(sum.lm, direction = "both")
sum.fit <- lm(formula = log(총구매금액) ~ 성별 + 연령대 + 
                거주지역 + 구매횟수 + 총구매물품 + 이용횟수 + N, data = CustDF)
summary(sum.fit) # R : 0.7444
box.result <- boxplot(log(CustDF$총구매금액))
str(box.result)
outlier.up <- 1.5*IQR(log(CustDF$총구매금액)) + summary(log(CustDF$총구매금액))[5]
outlier.down <- summary(log(CustDF$총구매금액))[2] - 1.5*IQR(log(CustDF$총구매금액)) 
# 총구매금액 이상치제거
CustDF1 <- CustDF[log(CustDF$총구매금액) > 7.542842, ]
# 이상치 제거한 회귀분석
sum.newfit <- lm(log(총구매금액) ~ ., data = CustDF1)
summary(sum.newfit) # R : 0.8614
# 연령대 범주 
CustDF1$연령대 <- ifelse(CustDF1$연령대 == "20세~24세" | CustDF1$연령대 == "25세~29세", "20대",
                      ifelse(CustDF1$연령대 == "30세~34세" | CustDF1$연령대 == "35세~39세", "30대",
                             ifelse(CustDF1$연령대 == "40세~44세" | CustDF1$연령대 == "45세~49세", "40대",
                                    ifelse(CustDF1$연령대 == "50세~54세" | CustDF1$연령대 == "55세~59세", "50대",
                                           ifelse(CustDF1$연령대 == "19세이하", "19세이하", "60세이상")))))
CustDF1$연령대 <- as.factor(CustDF1$연령대)
sum.newfit1 <- lm(log(총구매금액) ~ ., data = CustDF1)
summary(sum.newfit1) # R : 0.8615
# 
# # 주소 분류
# unique(CustDF1$주소)
# CustDF1 <- CustDF1[!(CustDF1$주소 == 0), ]
### Y : log(평균금액)
avg.fit <- lm(평균구매금액 ~ ., data = CustDF1)
summary(avg.fit) # R : 0.2977
box.result <- boxplot(CustDF$평균구매금액)
qqPlot(avg.fit, labels=row.names(고객번호), id.method="identify", simulate=TRUE, main="Q-Q_ plot")

CustDF2 <- CustDF[-c(14876,18022), ]
# outlier.up <- 1.5*IQR(CustDF$평균구매금액) + summary(CustDF$평균구매금액)[5]
# outlier.down <- summary(CustDF$평균구매금액)[2] - 1.5*IQR(CustDF$평균구매금액)
# 
# CustDF2 <- CustDF[CustDF$평균구매금액 > 71852.84 | CustDF$평균구매금액 < 223388.1, ]
box.result <- boxplot(CustDF2$평균구매금액)
avg.newfit <- lm(평균구매금액 ~ 성별 + 연령대 + 거주지역 + 이용횟수, data = CustDF2)
CustDF2$거주지역
summary(avg.newfit) # R : 0.015
avg.newfit <- lm(평균구매금액 ~ 구매횟수 + 총구매금액, data = CustDF2)