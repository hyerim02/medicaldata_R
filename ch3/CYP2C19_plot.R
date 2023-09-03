# Chapter 3. 데이터 분석 및 시각화  
# 3.1 데이터 그룹핑하기

dat24 <- dat23
table(dat24$CYP2C19)


dat24$gCYP2C19 <- ifelse(dat24$CYP2C19=="*1/*1", "Control",
                  ifelse(dat24$CYP2C19=="*1/*17", "GOF",
                  ifelse(dat24$CYP2C19=="*2/*17", "Mixed",
                  ifelse(dat24$CYP2C19=="*3/*17", "Mixed",
                  ifelse(dat24$CYP2C19=="*1/*2", "LOF",
                  ifelse(dat24$CYP2C19=="*1/*3", "LOF",
                  ifelse(dat24$CYP2C19=="*2/*2", "LOF",
                  ifelse(dat24$CYP2C19=="*2/*3", "LOF",
                  ifelse(dat24$CYP2C19=="*3/*3", "LOF", dat24$CYP2C19)))))))))

table(dat24$gCYP2C19, useNA = "always")

dat24$gCYP2C19 <- ""
dat24$gCYP2C19 <- ifelse(dat24$CYP2C19=="*1/*1", "Control",dat24$gCYP2C19)
dat24$gCYP2C19 <- ifelse(dat24$CYP2C19=="*1/*17", "GOF",dat24$gCYP2C19)
dat24$gCYP2C19 <- ifelse(dat24$CYP2C19=="*2/*17", "Mixed",dat24$gCYP2C19)
dat24$gCYP2C19 <- ifelse(dat24$CYP2C19=="*3/*17", "Mixed",dat24$gCYP2C19)
dat24$gCYP2C19 <- ifelse(dat24$CYP2C19=="*1/*2", "LOF",dat24$gCYP2C19)
dat24$gCYP2C19 <- ifelse(dat24$CYP2C19=="*1/*3", "LOF",dat24$gCYP2C19)
dat24$gCYP2C19 <- ifelse(dat24$CYP2C19=="*2/*2", "LOF",dat24$gCYP2C19)
dat24$gCYP2C19 <- ifelse(dat24$CYP2C19=="*2/*3", "LOF",dat24$gCYP2C19)
dat24$gCYP2C19 <- ifelse(dat24$CYP2C19=="*3/*3", "LOF",dat24$gCYP2C19)

dat24 <- within(dat24,{
  CYP2C19 <- as.character(CYP2C19)
  gCYP2C19 <- ""
  gCYP2C19 <- ifelse(CYP2C19=="*1/*1", "Control",gCYP2C19)
  gCYP2C19 <- ifelse(CYP2C19=="*1/*17", "GOF",gCYP2C19)
  gCYP2C19 <- ifelse(CYP2C19=="*2/*17", "Mixed",gCYP2C19)
  gCYP2C19 <- ifelse(CYP2C19=="*3/*17", "Mixed",gCYP2C19)
  gCYP2C19 <- ifelse(CYP2C19=="*1/*2", "LOF",gCYP2C19)
  gCYP2C19 <- ifelse(CYP2C19=="*1/*3", "LOF",gCYP2C19)
  gCYP2C19 <- ifelse(CYP2C19=="*2/*2", "LOF",gCYP2C19)
  gCYP2C19 <- ifelse(CYP2C19=="*2/*3", "LOF",gCYP2C19)
  gCYP2C19 <- ifelse(CYP2C19=="*3/*3", "LOF",gCYP2C19)
})

dat24 <- within(dat24, {
  CYP2C19 <- as.character(CYP2C19)
  gCYP2C19 <- case_when(
    CYP2C19=="*1/*1" ~ "Control",
    CYP2C19=="*1/*17" ~ "GOF",
    CYP2C19=="*2/*17" ~ "Mixed",
    CYP2C19=="*3/*17" ~ "Mixed",
    CYP2C19=="*1/*2" ~ "LOF",
    CYP2C19=="*1/*3" ~ "LOF",
    CYP2C19=="*2/*2" ~ "LOF",
    CYP2C19=="*2/*3" ~ "LOF",
    CYP2C19=="*3/*3" ~ "LOF",
    TRUE ~ CYP2C19
  )})

dat24 <- within(dat24, {
  gCYP2C19 <- ""
  gCYP2C19[CYP2C19=="*1/*1"] <- "Control"
  gCYP2C19[CYP2C19=="*1/*17"] <- "GOF"
  gCYP2C19[CYP2C19=="*2/*17"] <- "Mixed"
  gCYP2C19[CYP2C19=="*3/*17"] <- "Mixed"
  gCYP2C19[CYP2C19=="*1/*2"] <- "LOF"
  gCYP2C19[CYP2C19=="*1/*3"] <- "LOF"
  gCYP2C19[CYP2C19=="*2/*2"] <- "LOF"
  gCYP2C19[CYP2C19=="*2/*3"] <- "LOF"
  gCYP2C19[CYP2C19=="*3/*3"] <- "LOF"
})

# 3.2. 데이터 분석 및 시각화 
# 3.2.1 그룹별 단면 분석

require(moonBook)
A2 <- mytable(gCYP2C19~ Age+Sex+Dx+Smoking+HTN+CYP2C19+HB+CLOP+PCI_DATE, data=dat24)
mycsv(A2, file="data\\A2.csv")  # A2를 CSV파일로 저장

mytable(gCYP2C19~VERI_PRU, data=dat24, method = 3)

library(nparcomp)
result = mctp(VERI_PRU~factor(gCYP2C19), data = dat24)
summary(result)

# [그림 3-1] 그래프
boxplot(VERI_PRU~gCYP2C19, data=dat24)

# [그림 3-2] 그래프
boxplot(VERI_PRU~gCYP2C19, data=dat24)  
stripchart(VERI_PRU~gCYP2C19, data=dat24, method = "jitter", col=c("black", "red", "blue", "green"), pch = 19, vertical = T, add = T)

# [그림 3-3] 그래프
boxplot(VERI_PRU~gCYP2C19, notch=TRUE, data=dat24)  
dat24.1 <- dat24[ complete.cases(dat24[,c("gCYP2C19")]),]
m <- sampling::strata(data=dat24.1, stratanames = "gCYP2C19", size = c(46,46,46,46), method = "srswor")
dat24.2 <- sampling::getdata(dat24.1, m)
stripchart(VERI_PRU~gCYP2C19, data=dat24.2, method = "jitter", col=c("black", "red", "blue", "green"), pch = 19, vertical = T, add = T)

# [그림 3-4] 그래프
densityplot(VERI_PRU~gCYP2C19, data = dat24)

# 예제 Boxplot의 코드
# 무작위 샘플링이기 때문에 실행할때마다 다르게 추출될 수 있다. 
dat24s <- dat24[ complete.cases(dat24[,c("VERI_PRU")]),]  # VERI_PRU값이 없는 행 삭제
m <- sample(1:nrow(dat24s), 100)  # 행 100개를 무작위 샘플링하여 m에 할당 
dat24s100 <- dat24s[m,]  # 무작위 샘플링한 행 추출 
boxplot(dat24s100$VERI_PRU, notch = TRUE)  # notch 표시

# Boxplot 설명 
quantile(dat24s100$VERI_PRU)  # 사분위값 확인 

IQR <- 272.98771 - 174.23771  # IQR값 계산 (Q3 - Q1) 
IQR1.5 <- 1.5*IQR
IQR1.5 

upper.whisker <- 272.98771 + IQR1.5 
upper.whisker  
# 가장 큰 값이 407인데 이는 421보다 작다
# 이경우에는 upper whisker가 407이 된다 

lower.whisker <- 174.23771 - IQR1.5 
lower.whisker
# 가장 작은 값이 13인데 이는 26보다 작다
# 이경우에는 lower whisker가 26이 된며, 26보다 작은 값은 outlier가 된다.

# 3.2.2 연구 종점 및 기간 변수 만들기 



dat14_RR <- dat14[,c("Index","TOTAL_RR")]
dat14_join <- left_join(dat24,dat14_RR,by='Index')
dat14_join <- subset(dat14_join,select=-TOTAL_RR.x)
colnames(dat14_join)[87] <- 'TOTAL_RR'
dat25<-dat14_join

dat25$gCYP2C19 <- ifelse(dat24$CYP2C19=="*1/*1", "Control",
                         ifelse(dat24$CYP2C19=="*1/*17", "GOF",
                                ifelse(dat24$CYP2C19=="*2/*17", "Mixed",
                                       ifelse(dat24$CYP2C19=="*3/*17", "Mixed",
                                              ifelse(dat24$CYP2C19=="*1/*2", "LOF",
                                                     ifelse(dat24$CYP2C19=="*1/*3", "LOF",
                                                            ifelse(dat24$CYP2C19=="*2/*2", "LOF",
                                                                   ifelse(dat24$CYP2C19=="*2/*3", "LOF",
                                                                          ifelse(dat24$CYP2C19=="*3/*3", "LOF", dat24$CYP2C19)))))))))

## 복합연구종료점 MACCE 변수 생성
# Outcome_names: 결과(Outcome)변수들
Outcome_names <- c("CV_DEATH", "MI_YN", "CVA_YN", "ST_YN", "TOTAL_RR") 

# repNA : NA를 0으로 바꿔주는 함수
repNA=function(x){x[is.na(x)]=0;x}
# Outcome변수의 NA를 0처리
dat25[, Outcome_names] = lapply(dat25[, Outcome_names], repNA) 

# 복합연구종료점 MACCE 변수 생성 
dat25[, Outcome_names] = lapply(dat25[, Outcome_names], as.numeric) # numeric으로 변환
dat25$MACCE = ifelse(with(dat25, CV_DEATH+MI_YN+CVA_YN+ST_YN+TOTAL_RR)>0,1,0) # 하나라도 1이면 1

head(dat25[,c(Outcome_names, "MACCE")], n=10)  # 결과 확인 

# Date 변수에 대해 day 변수 생성 
OutcomeDate_names <- c("DEATH_DATE", "MI_DATE", "CVA_DATE", "ST_DATE", "RR_DATE") 
OutcomeDay_names <- c("DEATH_Day", "MI_Day", "CVA_Day", "ST_Day", "RR_Day") 

# 방법1. Date – Date 사용
for (i in 1:length(OutcomeDate_names)){
  dat25[, OutcomeDay_names[i]] <- ifelse(!is.na(dat25[, OutcomeDate_names[i]]),
                                         dat25[, OutcomeDate_names[i]] - dat25$PCI_DATE,
                                         dat25$FINAL_FU_DATE - dat25$PCI_DATE)    
}


# 방법2. difftime() 사용
for (i in 1:length(OutcomeDate_names)){
  dat25[, OutcomeDay_names[i]] <- ifelse(!is.na(dat25[, OutcomeDate_names[i]]),
                                         difftime(dat25[, OutcomeDate_names[i]], dat25$PCI_DATE, units="datys"),
                                         difftime(dat25$FINAL_FU_DATE, dat25$PCI_DATE, units="days"))    
}

# MACCE Day 변수 생성 
dat25.Day <- dat25[,OutcomeDay_names]
# Outcome Day(DEATH_Day, MI_Day, CVA_Day, ST_Day, RR_Day) 중에서 가장 작은 일수
# 필요한 변수만 추출하여 apply()를 적용하여 연산
dat25$MACCE_Day <- apply(dat25.Day, 1, min, na.rm = T)
head(dat25[,c(OutcomeDate_names, OutcomeDay_names, "MACCE_Day")], n=10)  # Day변수 결과 확인
class(dat25$DEATH_Day); class(dat25$MACCE_Day)

# 결과(Outcome)변수 및 관련 날짜, 기간에 대한 벡터 만들기
Outcome_names.new <- c("CV_DEATH", "MI_YN", "CVA_YN", "ST_YN", "TOTAL_RR", "MACCE") 
OutcomeDay_names.new <- c("DEATH_Day", "MI_Day", "CVA_Day", "ST_Day", "RR_Day", "MACCE_Day") 
Outcome1y_names <- c("CV_DEATH.1y", "MI.1y", "CVA.1y", "ST.1y", "RR.1y", "MACCE.1y") 
OutcomeDay1y_names <- c("DEATH.1y_Day", "MI.1y_Day", "CVA.1y_Day", "ST.1y_Day", "RR.1y_Day", "MACCE.1y.Day")

for (i in 1:length(OutcomeDay1y_names)){
  # 일수가 365일 이하면 그대로, 이상이면 365으로 할당
  dat25[, OutcomeDay1y_names[i]] <- ifelse(dat25[, OutcomeDay_names.new[i]]<= 365, dat25[, OutcomeDay_names.new[i]], 365)
  # 일수가 365일 이하이면서 Outcome이 1이면 사건발생을 1로 할당 나머지 경우는 0.
  dat25[, Outcome1y_names[i]] <- ifelse(dat25[, OutcomeDay_names.new[i]]<= 365 & dat25[, Outcome_names.new[i]]==1, 
                                        1, 0)
}

# Day 변수 결과 확인
temp <- dat25[,c(Outcome1y_names, OutcomeDay1y_names, Outcome_names.new, OutcomeDay_names.new)]
head(temp[temp$MACCE ==1 & temp$MACCE.1y ==0, c(OutcomeDay1y_names, OutcomeDay_names.new) ], n=10)

# 3.2.3 연구 결과 분석 

dat26 <- dat25
mytable(gCYP2C19 ~ CV_DEATH+MI_YN+CVA_YN+ST_YN+TOTAL_RR+MACCE, data=dat26)

dat27 <- dat26[dat26$gCYP2C19=="Control" | dat26$gCYP2C19=="LOF", ] # Control, LOF 그룹선별 
dat27 <- dat27[!is.na(dat27$MACCE.1y), ] # 추적조사결과(MACCE.1y)가 있는 그룹 선별 

mytable(gCYP2C19 ~ CV_DEATH+MI_YN+CVA_YN+ST_YN+TOTAL_RR+MACCE, data=dat27)

mytable(gCYP2C19 ~ MACCE_Day, method=2, data=dat27)

mytable(gCYP2C19 ~ CV_DEATH.1y+MI.1y+CVA.1y+ST.1y+RR.1y+MACCE.1y, data=dat27)

# survival 패키지 설치 후 실행
require(survival)
fit = survdiff(Surv(DEATH.1y_Day, CV_DEATH.1y==1) ~ gCYP2C19, dat27)
fit 

# survminer 패키지 설치 후 실행
require(survminer)
ggsurv = ggsurvplot(
  fit,
  data = dat27,
  risk.table = TRUE,  # risk table 표시여부 
  conf.int = T,   # confidence interval 표시여부 
  xlim = c(0,360),  # X축 범위
  ylim = c(0,0.02),  # Y축 범위
  xlab = "Time (Day)", # X축 이름 
  break.time.by = 30,  # X축 간격 
  risk.table.y.text = F, # risk table의 Y축 라벨표시 여부 
  font.tickslab = c(14, "plain", "black"), # 축 값의 font 설정 
  tables.theme = theme_cleantable(),  #테이블 테마설정
  fun = "event"  # "event" - cumulative event로 설정 
) 
ggsurv

# Cox proportional hazard regression model
require(survival)
result <- coxph(Surv(DEATH.1y_Day, CV_DEATH.1y==1) ~ factor(gCYP2C19), data = dat27) 
summary(result)

# 75세를 기준으로 이상이면 1, 이하면 0으로 나눈 새로운변수 Age75 생성 
dat27$Age75 <- ifelse(dat27$Age >= 75, 1, 0) 
# 분석하고자 하는 요인들을 포함한 Cox 모델 생성 
result <- coxph(Surv(DEATH.1y_Day, CV_DEATH.1y==1) ~ factor(gCYP2C19) + factor(Age75) + Sex + HTN + DM, data = dat27) 
# Step() 함수로 AIC를 기준으로 backward elimination방식으로 모형선별 
CoxStep <- step(result, direction="backward")   
# 결과확인 
summary(CoxStep)

dat28 <- dat27
mytable(CV_DEATH ~ VERI_PRU, data=dat28)

quantile(dat28$VERI_PRU, na.rm=T)   # na.rm = T: NA를 제외하고 연산 

dat28$gPRU <- rank2group(dat28$VERI_PRU, 4)
mytable(gPRU ~ CV_DEATH, data = dat28)

table (dat28$CV_DEATH, dat28$gPRU)

# prop.trend.test에 적용하기 위한 테이블(res) 생성
res = table (dat28$CV_DEATH, dat28$gPRU)
result = prop.trend.test(res[2,], colSums(res), score = c(1,2,3,4))
result

# pROC 패키지 설치 후 실행
require(pROC)
res1=roc(dat28$CV_DEATH, dat28$VERI_PRU, ci=TRUE, plot=TRUE, legacy.axes=TRUE)
res1

coords(res1, "best", transpose = F)