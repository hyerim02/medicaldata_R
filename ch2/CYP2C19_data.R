setwd("C:\\Users\\phl02\\Desktop\\P\\medicaldata_R")  

library(readxl)
dat.excel <- read_xlsx("data\\Raw Data.xlsx", col_names = TRUE)
dat.excel.del <- read_xlsx("data\\Raw Data Del.xlsx", col_names = TRUE)
dat.csv.del <- read.csv("data\\Raw Data Del.csv")

dim(dat.csv.del)
# na.strings 옵션을 사용하여 #DIV/0!, #NULL!, ND, NA, -, N/A, n/a, UK, ""(빈칸)을 NA로 처리
dat.csv <- read.csv("data\\Raw Data Del.csv", na.strings=c("#DIV/0!", ".", "#NULL!", "ND", "NA", "", "-", " ", "N/A", "n/a", "UK")) 

# 2.2.2 데이터 삭제하기 

# 불러들일 때 na.strings 처리 (dat1)
dat1 <- read.csv("data\\Raw Data Del.csv", na.strings=c("#DIV/0!", ".", "#NULL!", "ND", "NA", "", "-", " ", "N/A", "n/a", "UK"))

# 불러들일때 na.strings처리를 하지않은 것 (dat2)
dat2 <- read.csv("data\\Raw Data Del.csv")

dat3 <- dat1[is.na(dat1$Index), c(1:9) ] # dat1은 Index가 NA인 행이 2개가 있게 확인됨. 
dat3

dat4 <- dat2[is.na(dat2$Index), c(1:9) ] # 불러들일때 na.strings처리를 하지 않은 dat2는 파악이 안됨.
dat4

dat5 <- dat2[dat2$Index =="", c(1:9) ]   # dat2는 Index가 ""(공란)인 행을 파악하여 확인가능.
dat5

dat6 <- dat1[complete.cases(dat1[ , c("Index")]), ] # dat1은 Index가 NA인 행전체삭제하여 정리
nrow(dat6) # 행개수가 12,310개로 수정됨

dat7 <- dat2[dat2$Index != "", ] # dat2는 Index가 ""(공란)인 행전체를 삭제하여 정리 
nrow(dat7) # 행개수가 12,310개로 수정됨

# (참고) 특정행 삭제**
dat7 <- dat7[!(dat7$Index == "Z-0001"),]  # Index가 'Z-0001'인 행 삭제

# PCI_DATE와 VERI_PRU가 NA인 행전체삭제하여 정리
dat8 <- dat6[complete.cases(dat6[ , c("PCI_DATE", "VERI_PRU")]), ] 
nrow(dat8) # 행 개수가 11,182개로 수정됨

dat9 <- unique(dat8)
nrow(dat9)  #행개수는11,182개로동일함 

# x[which(duplicated(x)|duplicated(x,fromLast=T)),]
dat10 <- dat8[which(duplicated(dat8[,c("Age", "Sex", "CR", "VERI_PRU", "CYP2C19")])|duplicated(dat8[,c("Age", "Sex", "CR", "VERI_PRU", "CYP2C19")], fromLast = T)),] # 중복된항 확인 
dat11 <- dat10[,c("Index","Hosp", "Enroll_DATE", "Age", "Sex", "CR", "VERI_PRU", "CYP2C19")]
head(dat11[order(dat11$VERI_PRU, decreasing = T),], n=10) # 중복행 10개 확인 

nrow(dat11) # 중복행 갯수 36개 

dat12 <- dat8[!duplicated(dat8[,c("Age", "Sex", "CR", "VERI_PRU", "CYP2C19")]),] # 중복된 행 제거
nrow(dat12) # 행 개수는 11,164개 남음  

# 2.2.3 날짜 변수 정리하기

class(dat12$Enroll_DATE) # Enroll_DATE의 데이터 타입 확인 

dat12$datetest1 <- as.Date(dat12$Enroll_DATE) # as.Date로 변환 
summary(dat12$datetest1) # 변환된 데이터 확인 

dat12$datetest1 <- NULL # datetest1 변수 삭제 

dat12$TEMP_DATE <- as.character(dat12$Enroll_DATE)  # 문자형으로 변환
dat12$TEMP_DATE1 <- as.Date (dat12$TEMP_DATE, format='%Y/%m/%d') # 새로운 변수에 날짜형으로 변환 
dat12$TEMP_DATE2 <- as.Date (dat12$TEMP_DATE, format='%Y-%m-%d')
dat12$TEMP_DATE3 <- as.Date (dat12$TEMP_DATE, format='%Y.%m.%d')
dat12$TEMP_DATE4 <- as.Date (dat12$TEMP_DATE, format='%Y%m%d')
dat12$TEMP_DATE5 <- as.Date (dat12$TEMP_DATE, format='%d-%b-%y')
dat12$TEMP_DATE6 <- as.Date (dat12$TEMP_DATE, format='%Y-%m.%d')

# ifelse 구문을 활용해서 새로운 Enroll_DATEf 변수에 합침 
dat12$TEMP_DATEf <- ifelse( is.na(dat12$TEMP_DATE1)==FALSE, as.character(dat12$TEMP_DATE1), 
                            ifelse( is.na(dat12$TEMP_DATE2)==FALSE, as.character(dat12$TEMP_DATE2),
                            ifelse( is.na(dat12$TEMP_DATE3)==FALSE, as.character(dat12$TEMP_DATE3),
                            ifelse( is.na(dat12$TEMP_DATE4)==FALSE, as.character(dat12$TEMP_DATE4),
                            ifelse( is.na(dat12$TEMP_DATE5)==FALSE, as.character(dat12$TEMP_DATE5),
                            ifelse( is.na(dat12$TEMP_DATE6)==FALSE, as.character(dat12$TEMP_DATE6),NA))))))
dat12$TEMP_DATEf <- as.Date(dat12$TEMP_DATEf) # Enroll_DATEfinal에 날짜형으로 변환 

# 날짜형으로 변환되지 못한 데이터 유무 확인 
if(nrow(dat12[is.na(dat12$TEMP_DATEf),]) != 0 )
{table(as.data.frame.character(dat12[is.na(dat12$TEMP_DATEf) & !is.na(dat12$Enroll_DATE), "Enroll_DATE"]))}else
{print("0개 입니다")}

# Enroll_DATE를 TEMP_DATEf로 업데이트
dat12$Enroll_DATE <- dat12$TEMP_DATEf

# 필요없는 파생변수들 삭제 
dat12$TEMP_DATE <- NULL; dat12$TEMP_DATE1 <- NULL; dat12$TEMP_DATE2 <- NULL;
dat12$TEMP_DATE3 <- NULL; dat12$TEMP_DATE4 <- NULL; dat12$TEMP_DATE5 <- NULL; 
dat12$TEMP_DATE6 <- NULL; dat12$TEMP_DATEf <- NULL

names(dat12) # dataframe 컬럼 이름 확인 
# 날짜 변수 확인 
# Enroll_DATE, VERI_P_DATE, PCI_DATE, DIS_DATE, FINAL_FU_DATE, DEATH_DATE, MI_DATE, ST_DATE, CVA_DATE, RR_DATE

## VERI_P_DATE
dat12$TEMP_DATE <- as.character(dat12$VERI_P_DATE)  # 문자형으로 변환
dat12$TEMP_DATE1 <- as.Date (dat12$TEMP_DATE, format='%Y/%m/%d') # 새로운 변수에 날짜형으로 변환 
dat12$TEMP_DATE2 <- as.Date (dat12$TEMP_DATE, format='%Y-%m-%d')
dat12$TEMP_DATE3 <- as.Date (dat12$TEMP_DATE, format='%Y.%m.%d')
dat12$TEMP_DATE4 <- as.Date (dat12$TEMP_DATE, format='%Y%m%d')
dat12$TEMP_DATE5 <- as.Date (dat12$TEMP_DATE, format='%d-%b-%y')
dat12$TEMP_DATE6 <- as.Date (dat12$TEMP_DATE, format='%Y-%m.%d')

# ifelse 구문을 활용해서 새로운 Enroll_DATEf 변수에 합침 
dat12$TEMP_DATEf <- ifelse( is.na(dat12$TEMP_DATE1) == FALSE, as.character(dat12$TEMP_DATE1), 
                            ifelse( is.na(dat12$TEMP_DATE2)== FALSE, as.character(dat12$TEMP_DATE2),
                                    ifelse( is.na(dat12$TEMP_DATE3)== FALSE, as.character(dat12$TEMP_DATE3),
                                            ifelse( is.na(dat12$TEMP_DATE4)== FALSE, as.character(dat12$TEMP_DATE4),
                                                    ifelse( is.na(dat12$TEMP_DATE5)== FALSE, as.character(dat12$TEMP_DATE5),
                                                            ifelse( is.na(dat12$TEMP_DATE6)== FALSE, as.character(dat12$TEMP_DATE6),NA))))))
dat12$TEMP_DATEf <- as.Date(dat12$TEMP_DATEf) # Enroll_DATEfinal에 날짜형으로 변환 

# 날짜형으로 변환되지 못한 데이터 유무 확인 
if(nrow(dat12[is.na(dat12$TEMP_DATEf),]) != 0 )
{table(as.data.frame.character(dat12[is.na(dat12$TEMP_DATEf) & !is.na(dat12$VERI_P_DATE), "VERI_P_DATE"]))}else
{print("0개 입니다")}

dat12$VERI_P_DATE <- dat12$TEMP_DATEf
dat12$TEMP_DATE <- NULL; dat12$TEMP_DATE1 <- NULL; dat12$TEMP_DATE2 <- NULL; 
dat12$TEMP_DATE3 <- NULL; dat12$TEMP_DATE4 <- NULL; dat12$TEMP_DATE5 <- NULL; 
dat12$TEMP_DATE6 <- NULL; dat12$TEMP_DATEf <- NULL

## PCI_DATE
dat12$TEMP_DATE <- as.character(dat12$PCI_DATE)  # 문자형으로 변환
dat12$TEMP_DATE1 <- as.Date (dat12$TEMP_DATE, format='%Y/%m/%d') # 새로운 변수에 날짜형으로 변환 
dat12$TEMP_DATE2 <- as.Date (dat12$TEMP_DATE, format='%Y-%m-%d')
dat12$TEMP_DATE3 <- as.Date (dat12$TEMP_DATE, format='%Y.%m.%d')
dat12$TEMP_DATE4 <- as.Date (dat12$TEMP_DATE, format='%Y%m%d')
dat12$TEMP_DATE5 <- as.Date (dat12$TEMP_DATE, format='%d-%b-%y')
dat12$TEMP_DATE6 <- as.Date (dat12$TEMP_DATE, format='%Y-%m.%d')

# ifelse 구문을 활용해서 새로운 Enroll_DATEf 변수에 합침 
dat12$TEMP_DATEf <- ifelse( is.na(dat12$TEMP_DATE1) == FALSE, as.character(dat12$TEMP_DATE1), 
                            ifelse( is.na(dat12$TEMP_DATE2)== FALSE, as.character(dat12$TEMP_DATE2),
                                    ifelse( is.na(dat12$TEMP_DATE3)== FALSE, as.character(dat12$TEMP_DATE3),
                                            ifelse( is.na(dat12$TEMP_DATE4)== FALSE, as.character(dat12$TEMP_DATE4),
                                                    ifelse( is.na(dat12$TEMP_DATE5)== FALSE, as.character(dat12$TEMP_DATE5),
                                                            ifelse( is.na(dat12$TEMP_DATE6)== FALSE, as.character(dat12$TEMP_DATE6),NA))))))
dat12$TEMP_DATEf <- as.Date(dat12$TEMP_DATEf) # Enroll_DATEfinal에 날짜형으로 변환 

# 날짜형으로 변환되지 못한 데이터 유무 확인 
if(nrow(dat12[is.na(dat12$TEMP_DATEf),]) != 0 )
{table(as.data.frame.character(dat12[is.na(dat12$TEMP_DATEf) & !is.na(dat12$PCI_DATE), "PCI_DATE"]))}else
{print("0개 입니다")}

dat12$PCI_DATE <- dat12$TEMP_DATEf
dat12$TEMP_DATE <- NULL; dat12$TEMP_DATE1 <- NULL; dat12$TEMP_DATE2 <- NULL; 
dat12$TEMP_DATE3 <- NULL; dat12$TEMP_DATE4 <- NULL; dat12$TEMP_DATE5 <- NULL; 
dat12$TEMP_DATE6 <- NULL; dat12$TEMP_DATEf <- NULL

## DIS_DATE
dat12$TEMP_DATE <- as.character(dat12$DIS_DATE)  # 문자형으로 변환
dat12$TEMP_DATE1 <- as.Date (dat12$TEMP_DATE, format='%Y/%m/%d') # 새로운 변수에 날짜형으로 변환 
dat12$TEMP_DATE2 <- as.Date (dat12$TEMP_DATE, format='%Y-%m-%d')
dat12$TEMP_DATE3 <- as.Date (dat12$TEMP_DATE, format='%Y.%m.%d')
dat12$TEMP_DATE4 <- as.Date (dat12$TEMP_DATE, format='%Y%m%d')
dat12$TEMP_DATE5 <- as.Date (dat12$TEMP_DATE, format='%d-%b-%y')
dat12$TEMP_DATE6 <- as.Date (dat12$TEMP_DATE, format='%Y-%m.%d')

# ifelse 구문을 활용해서 새로운 Enroll_DATEf 변수에 합침 
dat12$TEMP_DATEf <- ifelse( is.na(dat12$TEMP_DATE1) == FALSE, as.character(dat12$TEMP_DATE1), 
                            ifelse( is.na(dat12$TEMP_DATE2)== FALSE, as.character(dat12$TEMP_DATE2),
                                    ifelse( is.na(dat12$TEMP_DATE3)== FALSE, as.character(dat12$TEMP_DATE3),
                                            ifelse( is.na(dat12$TEMP_DATE4)== FALSE, as.character(dat12$TEMP_DATE4),
                                                    ifelse( is.na(dat12$TEMP_DATE5)== FALSE, as.character(dat12$TEMP_DATE5),
                                                            ifelse( is.na(dat12$TEMP_DATE6)== FALSE, as.character(dat12$TEMP_DATE6),NA))))))
dat12$TEMP_DATEf <- as.Date(dat12$TEMP_DATEf) # Enroll_DATEfinal에 날짜형으로 변환 

# 날짜형으로 변환되지 못한 데이터 유무 확인 
if(nrow(dat12[is.na(dat12$TEMP_DATEf),]) != 0 )
{table(as.data.frame.character(dat12[is.na(dat12$TEMP_DATEf) & !is.na(dat12$DIS_DATE), "DIS_DATE"]))}else
{print("0개 입니다")}

dat12$DIS_DATE <- dat12$TEMP_DATEf
dat12$TEMP_DATE <- NULL; dat12$TEMP_DATE1 <- NULL; dat12$TEMP_DATE2 <- NULL; 
dat12$TEMP_DATE3 <- NULL; dat12$TEMP_DATE4 <- NULL; dat12$TEMP_DATE5 <- NULL; 
dat12$TEMP_DATE6 <- NULL; dat12$TEMP_DATEf <- NULL

## FINAL_FU_DATE
dat12$TEMP_DATE <- as.character(dat12$FINAL_FU_DATE)  # 문자형으로 변환
dat12$TEMP_DATE1 <- as.Date (dat12$TEMP_DATE, format='%Y/%m/%d') # 새로운 변수에 날짜형으로 변환 
dat12$TEMP_DATE2 <- as.Date (dat12$TEMP_DATE, format='%Y-%m-%d')
dat12$TEMP_DATE3 <- as.Date (dat12$TEMP_DATE, format='%Y.%m.%d')
dat12$TEMP_DATE4 <- as.Date (dat12$TEMP_DATE, format='%Y%m%d')
dat12$TEMP_DATE5 <- as.Date (dat12$TEMP_DATE, format='%d-%b-%y')
dat12$TEMP_DATE6 <- as.Date (dat12$TEMP_DATE, format='%Y-%m.%d')

# ifelse 구문을 활용해서 새로운 Enroll_DATEf 변수에 합침 
dat12$TEMP_DATEf <- ifelse( is.na(dat12$TEMP_DATE1) == FALSE, as.character(dat12$TEMP_DATE1), 
                    ifelse( is.na(dat12$TEMP_DATE2)== FALSE, as.character(dat12$TEMP_DATE2),
                    ifelse( is.na(dat12$TEMP_DATE3)== FALSE, as.character(dat12$TEMP_DATE3),
                    ifelse( is.na(dat12$TEMP_DATE4)== FALSE, as.character(dat12$TEMP_DATE4),
                    ifelse( is.na(dat12$TEMP_DATE5)== FALSE, as.character(dat12$TEMP_DATE5),
                    ifelse( is.na(dat12$TEMP_DATE6)== FALSE, as.character(dat12$TEMP_DATE6),NA))))))
dat12$TEMP_DATEf <- as.Date(dat12$TEMP_DATEf) # Enroll_DATEfinal에 날짜형으로 변환 

# 날짜형으로 변환되지 못한 데이터 유무 확인 
if(nrow(dat12[is.na(dat12$TEMP_DATEf),]) != 0 )
{table(as.data.frame.character(dat12[is.na(dat12$TEMP_DATEf) & !is.na(dat12$FINAL_FU_DATE), "FINAL_FU_DATE"]))}else
{print("0개 입니다")}

dat12$FINAL_FU_DATE <- dat12$TEMP_DATEf
dat12$TEMP_DATE <- NULL; dat12$TEMP_DATE1 <- NULL; dat12$TEMP_DATE2 <- NULL; 
dat12$TEMP_DATE3 <- NULL; dat12$TEMP_DATE4 <- NULL; dat12$TEMP_DATE5 <- NULL; 
dat12$TEMP_DATE6 <- NULL; dat12$TEMP_DATE7 <- NULL; dat12$TEMP_DATEf <- NULL

## DEATH_DATE
dat12$TEMP_DATE <- as.character(dat12$DEATH_DATE)  # 문자형으로 변환
dat12$TEMP_DATE1 <- as.Date (dat12$TEMP_DATE, format='%Y/%m/%d') # 새로운 변수에 날짜형으로 변환 
dat12$TEMP_DATE2 <- as.Date (dat12$TEMP_DATE, format='%Y-%m-%d')
dat12$TEMP_DATE3 <- as.Date (dat12$TEMP_DATE, format='%Y.%m.%d')
dat12$TEMP_DATE4 <- as.Date (dat12$TEMP_DATE, format='%Y%m%d')
dat12$TEMP_DATE5 <- as.Date (dat12$TEMP_DATE, format='%d-%b-%y')
dat12$TEMP_DATE6 <- as.Date (dat12$TEMP_DATE, format='%Y-%m.%d')

# ifelse 구문을 활용해서 새로운 Enroll_DATEf 변수에 합침 
dat12$TEMP_DATEf <- ifelse( is.na(dat12$TEMP_DATE1) == FALSE, as.character(dat12$TEMP_DATE1), 
                            ifelse( is.na(dat12$TEMP_DATE2)== FALSE, as.character(dat12$TEMP_DATE2),
                                    ifelse( is.na(dat12$TEMP_DATE3)== FALSE, as.character(dat12$TEMP_DATE3),
                                            ifelse( is.na(dat12$TEMP_DATE4)== FALSE, as.character(dat12$TEMP_DATE4),
                                                    ifelse( is.na(dat12$TEMP_DATE5)== FALSE, as.character(dat12$TEMP_DATE5),
                                                            ifelse( is.na(dat12$TEMP_DATE6)== FALSE, as.character(dat12$TEMP_DATE6),NA))))))
dat12$TEMP_DATEf <- as.Date(dat12$TEMP_DATEf) # Enroll_DATEfinal에 날짜형으로 변환 

# 날짜형으로 변환되지 못한 데이터 유무 확인 
if(nrow(dat12[is.na(dat12$TEMP_DATEf),]) != 0 )
{table(as.data.frame.character(dat12[is.na(dat12$TEMP_DATEf) & !is.na(dat12$DEATH_DATE), "DEATH_DATE"]))}else
{print("0개 입니다")}

dat12$DEATH_DATE <- dat12$TEMP_DATEf
dat12$TEMP_DATE <- NULL; dat12$TEMP_DATE1 <- NULL; dat12$TEMP_DATE2 <- NULL; 
dat12$TEMP_DATE3 <- NULL; dat12$TEMP_DATE4 <- NULL; dat12$TEMP_DATE5 <- NULL; 
dat12$TEMP_DATE6 <- NULL; dat12$TEMP_DATEf <- NULL

## MI_DATE
dat12$TEMP_DATE <- as.character(dat12$MI_DATE)  # 문자형으로 변환
dat12$TEMP_DATE1 <- as.Date (dat12$TEMP_DATE, format='%Y/%m/%d') # 새로운 변수에 날짜형으로 변환 
dat12$TEMP_DATE2 <- as.Date (dat12$TEMP_DATE, format='%Y-%m-%d')
dat12$TEMP_DATE3 <- as.Date (dat12$TEMP_DATE, format='%Y.%m.%d')
dat12$TEMP_DATE4 <- as.Date (dat12$TEMP_DATE, format='%Y%m%d')
dat12$TEMP_DATE5 <- as.Date (dat12$TEMP_DATE, format='%d-%b-%y')
dat12$TEMP_DATE6 <- as.Date (dat12$TEMP_DATE, format='%Y-%m.%d')

# ifelse 구문을 활용해서 새로운 Enroll_DATEf 변수에 합침 
dat12$TEMP_DATEf <- ifelse( is.na(dat12$TEMP_DATE1) == FALSE, as.character(dat12$TEMP_DATE1), 
                            ifelse( is.na(dat12$TEMP_DATE2)== FALSE, as.character(dat12$TEMP_DATE2),
                                    ifelse( is.na(dat12$TEMP_DATE3)== FALSE, as.character(dat12$TEMP_DATE3),
                                            ifelse( is.na(dat12$TEMP_DATE4)== FALSE, as.character(dat12$TEMP_DATE4),
                                                    ifelse( is.na(dat12$TEMP_DATE5)== FALSE, as.character(dat12$TEMP_DATE5),
                                                            ifelse( is.na(dat12$TEMP_DATE6)== FALSE, as.character(dat12$TEMP_DATE6),NA))))))
dat12$TEMP_DATEf <- as.Date(dat12$TEMP_DATEf) # Enroll_DATEfinal에 날짜형으로 변환 

# 날짜형으로 변환되지 못한 데이터 유무 확인 
if(nrow(dat12[is.na(dat12$TEMP_DATEf),]) != 0 )
{table(as.data.frame.character(dat12[is.na(dat12$TEMP_DATEf) & !is.na(dat12$MI_DATE), "MI_DATE"]))}else
{print("0개 입니다")}

dat12$MI_DATE <- dat12$TEMP_DATEf
dat12$TEMP_DATE <- NULL; dat12$TEMP_DATE1 <- NULL; dat12$TEMP_DATE2 <- NULL; 
dat12$TEMP_DATE3 <- NULL; dat12$TEMP_DATE4 <- NULL; dat12$TEMP_DATE5 <- NULL; 
dat12$TEMP_DATE6 <- NULL; dat12$TEMP_DATEf <- NULL

## ST_DATE
dat12$TEMP_DATE <- as.character(dat12$ST_DATE)  # 문자형으로 변환
dat12$TEMP_DATE1 <- as.Date (dat12$TEMP_DATE, format='%Y/%m/%d') # 새로운 변수에 날짜형으로 변환 
dat12$TEMP_DATE2 <- as.Date (dat12$TEMP_DATE, format='%Y-%m-%d')
dat12$TEMP_DATE3 <- as.Date (dat12$TEMP_DATE, format='%Y.%m.%d')
dat12$TEMP_DATE4 <- as.Date (dat12$TEMP_DATE, format='%Y%m%d')
dat12$TEMP_DATE5 <- as.Date (dat12$TEMP_DATE, format='%d-%b-%y')
dat12$TEMP_DATE6 <- as.Date (dat12$TEMP_DATE, format='%Y-%m.%d')

# ifelse 구문을 활용해서 새로운 Enroll_DATEf 변수에 합침 
dat12$TEMP_DATEf <- ifelse( is.na(dat12$TEMP_DATE1) == FALSE, as.character(dat12$TEMP_DATE1), 
                            ifelse( is.na(dat12$TEMP_DATE2)== FALSE, as.character(dat12$TEMP_DATE2),
                                    ifelse( is.na(dat12$TEMP_DATE3)== FALSE, as.character(dat12$TEMP_DATE3),
                                            ifelse( is.na(dat12$TEMP_DATE4)== FALSE, as.character(dat12$TEMP_DATE4),
                                                    ifelse( is.na(dat12$TEMP_DATE5)== FALSE, as.character(dat12$TEMP_DATE5),
                                                            ifelse( is.na(dat12$TEMP_DATE6)== FALSE, as.character(dat12$TEMP_DATE6),NA))))))
dat12$TEMP_DATEf <- as.Date(dat12$TEMP_DATEf) # Enroll_DATEfinal에 날짜형으로 변환 

# 날짜형으로 변환되지 못한 데이터 유무 확인 
if(nrow(dat12[is.na(dat12$TEMP_DATEf),]) != 0 )
{table(as.data.frame.character(dat12[is.na(dat12$TEMP_DATEf) & !is.na(dat12$ST_DATE), "ST_DATE"]))}else
{print("0개 입니다")}

dat12$ST_DATE <- dat12$TEMP_DATEf
dat12$TEMP_DATE <- NULL; dat12$TEMP_DATE1 <- NULL; dat12$TEMP_DATE2 <- NULL; 
dat12$TEMP_DATE3 <- NULL; dat12$TEMP_DATE4 <- NULL; dat12$TEMP_DATE5 <- NULL; 
dat12$TEMP_DATE6 <- NULL; dat12$TEMP_DATEf <- NULL

## CVA_DATE 
dat12$TEMP_DATE <- as.character(dat12$CVA_DATE)  # 문자형으로 변환
dat12$TEMP_DATE1 <- as.Date (dat12$TEMP_DATE, format='%Y/%m/%d') # 새로운 변수에 날짜형으로 변환 
dat12$TEMP_DATE2 <- as.Date (dat12$TEMP_DATE, format='%Y-%m-%d')
dat12$TEMP_DATE3 <- as.Date (dat12$TEMP_DATE, format='%Y.%m.%d')
dat12$TEMP_DATE4 <- as.Date (dat12$TEMP_DATE, format='%Y%m%d')
dat12$TEMP_DATE5 <- as.Date (dat12$TEMP_DATE, format='%d-%b-%y')
dat12$TEMP_DATE6 <- as.Date (dat12$TEMP_DATE, format='%Y-%m.%d')

# ifelse 구문을 활용해서 새로운 Enroll_DATEf 변수에 합침 
dat12$TEMP_DATEf <- ifelse( is.na(dat12$TEMP_DATE1) == FALSE, as.character(dat12$TEMP_DATE1), 
                            ifelse( is.na(dat12$TEMP_DATE2)== FALSE, as.character(dat12$TEMP_DATE2),
                                    ifelse( is.na(dat12$TEMP_DATE3)== FALSE, as.character(dat12$TEMP_DATE3),
                                            ifelse( is.na(dat12$TEMP_DATE4)== FALSE, as.character(dat12$TEMP_DATE4),
                                                    ifelse( is.na(dat12$TEMP_DATE5)== FALSE, as.character(dat12$TEMP_DATE5),
                                                            ifelse( is.na(dat12$TEMP_DATE6)== FALSE, as.character(dat12$TEMP_DATE6),NA))))))
dat12$TEMP_DATEf <- as.Date(dat12$TEMP_DATEf) # Enroll_DATEfinal에 날짜형으로 변환 

# 날짜형으로 변환되지 못한 데이터 유무 확인 
if(nrow(dat12[is.na(dat12$TEMP_DATEf),]) != 0 )
{table(as.data.frame.character(dat12[is.na(dat12$TEMP_DATEf) & !is.na(dat12$CVA_DATE), "CVA_DATE"]))}else
{print("0개 입니다")}

dat12$CVA_DATE <- dat12$TEMP_DATEf
dat12$TEMP_DATE <- NULL; dat12$TEMP_DATE1 <- NULL; dat12$TEMP_DATE2 <- NULL; 
dat12$TEMP_DATE3 <- NULL; dat12$TEMP_DATE4 <- NULL; dat12$TEMP_DATE5 <- NULL; 
dat12$TEMP_DATE6 <- NULL; dat12$TEMP_DATEf <- NULL

## RR_DATE
dat12$TEMP_DATE <- as.character(dat12$RR_DATE)  # 문자형으로 변환
dat12$TEMP_DATE1 <- as.Date (dat12$TEMP_DATE, format='%Y/%m/%d') # 새로운 변수에 날짜형으로 변환 
dat12$TEMP_DATE2 <- as.Date (dat12$TEMP_DATE, format='%Y-%m-%d')
dat12$TEMP_DATE3 <- as.Date (dat12$TEMP_DATE, format='%Y.%m.%d')
dat12$TEMP_DATE4 <- as.Date (dat12$TEMP_DATE, format='%Y%m%d')
dat12$TEMP_DATE5 <- as.Date (dat12$TEMP_DATE, format='%d-%b-%y')
dat12$TEMP_DATE6 <- as.Date (dat12$TEMP_DATE, format='%Y-%m.%d')

# ifelse 구문을 활용해서 새로운 Enroll_DATEf 변수에 합침 
dat12$TEMP_DATEf <- ifelse( is.na(dat12$TEMP_DATE1) == FALSE, as.character(dat12$TEMP_DATE1), 
                            ifelse( is.na(dat12$TEMP_DATE2)== FALSE, as.character(dat12$TEMP_DATE2),
                                    ifelse( is.na(dat12$TEMP_DATE3)== FALSE, as.character(dat12$TEMP_DATE3),
                                            ifelse( is.na(dat12$TEMP_DATE4)== FALSE, as.character(dat12$TEMP_DATE4),
                                                    ifelse( is.na(dat12$TEMP_DATE5)== FALSE, as.character(dat12$TEMP_DATE5),
                                                            ifelse( is.na(dat12$TEMP_DATE6)== FALSE, as.character(dat12$TEMP_DATE6),NA))))))
dat12$TEMP_DATEf <- as.Date(dat12$TEMP_DATEf) # Enroll_DATEfinal에 날짜형으로 변환 

# 날짜형으로 변환되지 못한 데이터 유무 확인 
if(nrow(dat12[is.na(dat12$TEMP_DATEf),]) != 0 )
{table(as.data.frame.character(dat12[is.na(dat12$TEMP_DATEf) & !is.na(dat12$RR_DATE), "RR_DATE"]))}else
{print("0개 입니다")}

dat12$RR_DATE <- dat12$TEMP_DATEf
dat12$TEMP_DATE <- NULL; dat12$TEMP_DATE1 <- NULL; dat12$TEMP_DATE2 <- NULL; 
dat12$TEMP_DATE3 <- NULL; dat12$TEMP_DATE4 <- NULL; dat12$TEMP_DATE5 <- NULL; 
dat12$TEMP_DATE6 <- NULL; dat12$TEMP_DATEf <- NULL

dat14 <- dat12  # dat14로 version upgrade 
write.csv(dat14, file="dat14.csv", fileEncoding = "utf8") # 데이터의 저장

head(dat14$PCI_DATE)  # 데이터 확인 

# 년-월 추출 
dat14$PCI_yrmo <- format(as.Date(dat14$PCI_DATE), "%Y-%m")
head(dat14$PCI_yrmo)

# 년 추출 
dat14$PCI_year <- format(as.Date(dat14$PCI_DATE), "%Y")
head(dat14$PCI_year)

class(dat10$PCI_DATE); 

library(lubridate)
head(dat10$PCI_DATE) # 예제 데이터 확인 
dat10$PCI_DATE.1 <- ymd(dat10$PCI_DATE)  # 년월일 순으로된 데이터 변환 
head(dat10$PCI_DATE.1) #데이터확인

testdate <- "31-Jul-11"
result <- ymd(testdate)
result  # 2031년으로 잘못 변경하였다.

dat14$PCI_year <- year(dat14$PCI_DATE)  # 년도를 추출 
sum.year <- table(dat14$PCI_year)  # table()함수로 건수 계산 
barplot(sum.year)  # barplot()로 시각화 

dat.date <- read.csv("data\\Date_example2.csv")
head(dat.date)  # 데이터  확인 
class(dat.date$FINAL_FU_DATE)
class(dat.date$DEATH_DATE)

# as.Date() 함수로 데이터 타입을 날짜형으로 변환
dat.date$Enroll_DATE <- as.Date(dat.date$Enroll_DATE) 
dat.date$PCI_DATE <- as.Date(dat.date$PCI_DATE) 
dat.date$FINAL_FU_DATE <- as.Date(dat.date$FINAL_FU_DATE) 

# as.Date() 함수에 'origin =' 옵션을 사용하여 날짜형 데이터로 변환
dat.date$DEATH_DATE <- as.Date(dat.date$DEATH_DATE, origin = "1900-01-01") 
dat.date$MI_DATE <- as.Date(dat.date$MI_DATE, origin = "1900-01-01")
dat.date$ST_DATE <- as.Date(dat.date$ST_DATE, origin = "1900-01-01")
dat.date$CVA_DATE <- as.Date(dat.date$CVA_DATE, origin = "1900-01-01")
dat.date$RR_DATE <- as.Date(dat.date$RR_DATE, origin = "1900-01-01")

head(dat.date) # dat.date의 1~6행의 데이터를 출력한다. 

# 2.2.4 그 밖의 변수 정리하기

class(dat14$CV_DEATH)   # 데이터 타입 확인 
table(dat14$CV_DEATH)   # 데이터 내용 확인

table(dat14$CV_DEATH, dat14$Hosp)  # 기관별 데이터 출력  

dat14$CV_DEATH <- as.character(dat14$CV_DEATH) # 문자형으로 변환 

# 1, Cardiac, Vascular로 입력된 사건만 CV Death로 정리  
dat14$CV_DEATH <- ifelse(dat14$CV_DEATH=="Cardiac", "1",
                         ifelse(dat14$CV_DEATH=="Vascular", "1",
                                ifelse(dat14$CV_DEATH=="1", "1","0")))

dat14$CV_DEATH <- as.factor(dat14$CV_DEATH) # 다시 요인형으로 변환 
nrow(dat14) # 데이터의 총 행수  

table(dat14$CV_DEATH) # 데이터 확인 

table(dat14$CV_DEATH, useNA = "always")

summary(dat14$CV_DEATH)

# 카테고리형 변수의 경우, 일관된 형식으로 수정
# factor형으로 되어 있는 경우에는 이상하게 변환될 수 있으므로 반드시 미리 character타입으로 설정한 다음에 변환
dat14$CYP2C19 <- as.character(dat14$CYP2C19)
dat14$CYP2C19 <- ifelse(dat14$CYP2C19=='*17 homozygous', '*1/*17',dat14$CYP2C19)
dat14$CYP2C19 <- ifelse(dat14$CYP2C19=='*2 heterozygous', '*1/*2',dat14$CYP2C19)
dat14$CYP2C19 <- ifelse(dat14$CYP2C19=='*2 homozygous', '*1/*2',dat14$CYP2C19)
dat14$CYP2C19 <- ifelse(dat14$CYP2C19=='*2/*3 heterozygous', '*2/*3',dat14$CYP2C19)
dat14$CYP2C19 <- ifelse(dat14$CYP2C19=='*3 heterozygous', '*1/*3',dat14$CYP2C19)
dat14$CYP2C19 <- ifelse(dat14$CYP2C19=='1*/1*', '*1/*1',dat14$CYP2C19)
dat14$CYP2C19 <- ifelse(dat14$CYP2C19=='1*/17', '*1/*17',dat14$CYP2C19)
dat14$CYP2C19 <- ifelse(dat14$CYP2C19=='1*/2*', '*1/*2',dat14$CYP2C19)
dat14$CYP2C19 <- ifelse(dat14$CYP2C19=='1*/3*', '*1/*3',dat14$CYP2C19)
dat14$CYP2C19 <- ifelse(dat14$CYP2C19=='*2/*1', '*1/*2',dat14$CYP2C19)
dat14$CYP2C19 <- ifelse(dat14$CYP2C19=='2*/3*', '*2/*3',dat14$CYP2C19)
dat14$CYP2C19 <- ifelse(dat14$CYP2C19=='2*/2*', '*2/*2',dat14$CYP2C19)
dat14$CYP2C19 <- ifelse(dat14$CYP2C19=='3*/3*', '*3/*3',dat14$CYP2C19)
dat14$CYP2C19 <- ifelse(dat14$CYP2C19=='wild type', '*1/*1',dat14$CYP2C19)

dat14$NoDzVV <- as.character(dat14$NoDzVV)
dat14$NoDzVV <- ifelse(dat14$NoDzVV=='1', '1VD',dat14$NoDzVV)
dat14$NoDzVV <- ifelse(dat14$NoDzVV=='2', '2VD',dat14$NoDzVV)
dat14$NoDzVV <- ifelse(dat14$NoDzVV=='3', '3VD',dat14$NoDzVV)
dat14$NoDzVV <- ifelse(dat14$NoDzVV=='4', 'insignificant',dat14$NoDzVV)
dat14$NoDzVV <- ifelse(dat14$NoDzVV=='NULL', NA,dat14$NoDzVV)

# 인덱스 벡터 활용예
dat14$NoDzVV <- as.character(dat14$NoDzVV) #항상 잊지말고 character로 바꾼다.
dat14$NoDzVV[dat14$NoDzVV == "1"] <- "1VD"   
dat14$NoDzVV[dat14$NoDzVV == "2"] <- "2VD"  
dat14$NoDzVV[dat14$NoDzVV == "3"] <- "3VD"  
dat14$NoDzVV[dat14$NoDzVV == "4"] <- "insignificant"  
dat14$NoDzVV[dat14$NoDzVV == "NULL"] <- NA  

# dplyr 패키지의 case_when() 활용예
## 문자형 -> 문자형 
dat14$NoDzVV <- as.character(dat14$NoDzVV)
library(dplyr)
dat14$NoDzVV <- case_when(
  dat14$NoDzVV == "1" ~ "1VD",
  dat14$NoDzVV == "2" ~ "2VD",
  dat14$NoDzVV == "3" ~ "3VD",
  dat14$NoDzVV == "4" ~ "insignificant",
  TRUE ~ NA_character_  
)
### TRUE는 ifelse의 else에 해당
### 문자형으로 변환하는 경우의 NA를 NA_character_로 표시

## 문자형 -> 수치형 
dat14$NoDzVV <- case_when(
  dat14$NoDzVV == "1VD" ~ 1,
  dat14$NoDzVV == "2VD" ~ 2,
  dat14$NoDzVV == "3VD" ~ 3,
  dat14$NoDzVV == "insignificant" ~ 4,
  TRUE ~ NA_real_  #수치형으로 변환하는 경우의 NA
)
### 수치형으로 변환하는 경우의 NA를 NA_real_로 표시

summary(dat14$Height)
table(dat14$Height, useNA = "always")

## 아래 코드들은 책에서는 예시 코드로 작성하였습니다. 흐름 위주로 이해하시면 됩니다.
# 수치형 변수의 unknown은 NA로 처리하고, 수치형 변수로 변환

dat14$Height <- ifelse (dat14$Height=="unknown", NA, 
                        ifelse (dat14$Height=="Unknown", NA, dat14$Height)) 
dat14$Height <- as.numeric(dat14$Height)

dat14$Weight <- ifelse(dat14$Weight=="unknown", NA,
                       ifelse(dat14$Weight=="Unknown", NA, dat14$Weight))
dat14$Weight <- as.numeric(dat14$Weight)

dat14$BMI <- ifelse(dat14$BMI=="unknown", NA,dat14$BMI)
dat14$BMI <- as.numeric(dat14$BMI)

# 수치형 변수의 입력값의 범위를 확인하여 이상한 outlier를 확인하고 Raw 데이터를 확인 
# 아래의 경우는 나올 수 없는 값(음수)을 NA 처리 
dat14$VERI_PRU <- ifelse(dat14$VERI_PRU <= 0, NA, dat14$VERI_PRU)

dat14$TG <- as.numeric(dat14$TG) # TG변수를 수치형 변수로 변환 

table(dat14$CV_DEATH, dat14$Hosp, useNA = "always") # 상황에 따라 다르게 출력 될 수 있습니다.

dat15 <- dat14
dat15$Hosp <- droplevels(factor(dat15$Hosp)) # 유효하지않은 레벨을 삭제한다.
length(levels(dat15$Hosp)) # level을 추출하여 개수를 구한다.

# VIM 패키지를 이전에 인스톨하지 않았다면 install.packages()를 사용하여 먼저 인스톨
require(VIM)
dat15.1 <- dat15[, c(2:20)]   # 데이터세트 dat15의 2~20열을 추출하여 dat15.1로 할당
VIM::aggr(dat15.1, prop=F, numbers=T)  # ::는 VIM패키지에 있는 aggr()함수를 사용한다는 의미

# apply() 함수를 사용하여 sum(is.na(x))를 열별로 적용
na.count=apply(dat15,2,function(x) sum(is.na(x)))
# na.count가 10000개 초과인 변수 출력
na.count[na.count>10000]

table(dat15$TOTAL_RR, dat15$Hosp, useNA = "always")

dat15$TOTAL_RR <- ifelse(!is.na(dat15$TOTAL_RR), dat15$TOTAL_RR, 0) # 0으로 대체

# NA를 0으로 바꿔주는 함수
repNA=function(x){x[is.na(x)]=0;x}

# 결측값 0 처리 
dat15[, "TOTAL_RR"] = lapply(dat15[, "TOTAL_RR"], repNA) 

# 2.2.5 변수 새로 만들기 

# Data 확인 (실행결과는 생략함)

class(dat15$LAD_YN);class(dat15$LCX_YN);class(dat15$RCA_YN);class(dat15$LM_YN);class(dat15$RAMUS_YN);class(dat15$NoDzVV) 
table(dat15$LAD_YN, useNA = "always");table(dat15$LCX_YN, useNA = "always");table(dat15$RCA_YN, useNA = "always");
table(dat15$LM_YN, useNA = "always");table(dat15$RAMUS_YN, useNA = "always");table(dat15$NoDzVV, useNA = "always")

# 수치형 변환 
dat15$LAD_YN <- as.numeric(dat15$LAD_YN)
dat15$LCX_YN <- as.numeric(dat15$LCX_YN)
dat15$RCA_YN <- as.numeric(dat15$RCA_YN)
dat15$LM_YN <- as.numeric(dat15$LM_YN)
dat15$RAMUS_YN <- as.numeric(dat15$RAMUS_YN)

# VV.NA : LAD_YN, LCX_YN, RCA_YN, LM_YN, RAMUS_YN 모두 NA면 1
dat15$VV.NA <- ifelse(is.na(dat15$LAD_YN)&is.na(dat15$LCX_YN)&is.na(dat15$RCA_YN)&is.na(dat15$LM_YN)&is.na(dat15$RAMUS_YN), 1,0)

# NA를 0으로 치환 
dat15$LAD_YN <- ifelse(!is.na(dat15$LAD_YN), dat15$LAD_YN, 0)
dat15$LCX_YN <- ifelse(!is.na(dat15$LCX_YN), dat15$LCX_YN, 0)
dat15$RCA_YN <- ifelse(!is.na(dat15$RCA_YN), dat15$RCA_YN, 0)
dat15$LM_YN <- ifelse(!is.na(dat15$LM_YN), dat15$LM_YN, 0)
dat15$RAMUS_YN <- ifelse(!is.na(dat15$RAMUS_YN), dat15$RAMUS_YN, 0)

# VV.NA가 1(모두 NA)이면, NA
dat15$LAD_YN <- ifelse(dat15$VV.NA == 1, NA, dat15$LAD_YN)
dat15$LCX_YN <- ifelse(dat15$VV.NA == 1, NA, dat15$LCX_YN)
dat15$RCA_YN <- ifelse(dat15$VV.NA == 1, NA, dat15$RCA_YN)
dat15$LM_YN <- ifelse(dat15$VV.NA == 1, NA, dat15$LM_YN)
dat15$RAMUS_YN <- ifelse(dat15$VV.NA == 1, NA, dat15$RAMUS_YN)

dat15$NoDzVV1 <- dat15$LAD_YN + dat15$LCX_YN + dat15$RCA_YN + dat15$LM_YN + dat15$RAMUS_YN

table(dat15$NoDzVV1, useNA = "always") # 실행결과 아래에 출력 

dat15$NoDzVV <- dat15$NoDzVV1   # NoDzVV 업데이트, 새로 만든 변수 삭제 
dat15$NoDzVV1 <- NULL; dat15$VV.NA <- NULL  # 더이상 불필요한 파생변수 삭제

dat16 <- dat15

# 데이터 타입을 문자형으로 변환
STENT_Names <- c("STENT1", "STENT2", "STENT3", "STENT4", "STENT1.1", "STENT2.1", "STENT3.1", "STENT4.1", "STENT1.2", "STENT2.2", "STENT3.2", "STENT4.2", "STENT1.3", "STENT2.3", "STENT3.3", "STENT4.3")
dat16[, STENT_Names] = lapply(dat16[, STENT_Names], as.character) 

# 여러열(변수)을 새로운 변수에 한꺼번에 붙여넣기 
dat16$STENT_Total <- do.call(paste, c(dat16[, STENT_Names], sep=",")) 

# 데이터 정리 
dat16$STENT_Total <- gsub("NA|\\.|\\,\\s|\\s\\,|\\/|\\s{2,}|\\s$", ",", dat16$STENT_Total) 
# NA, ".", ",", "/" -> ","
dat16$STENT_Total <- gsub("\\,+|\\,\\s", ",", dat16$STENT_Total) # 1개이상 "," -> "," 
dat16$STENT_Total <- gsub("^\\,|\\,$", "", dat16$STENT_Total) # 앞 뒤 "," 제거
dat16$STENT_Total <- tolower(as.character(dat16$STENT_Total)) # 소문자로 변경

dat16$STENT_Total <- gsub("기타", "other", dat16$STENT_Total) # "기타" -> "other" 
dat16$STENT_Total <- gsub("1", "biomatrix", dat16$STENT_Total) # "1" -> "biomatrix" 
dat16$STENT_Total <- gsub("2", "xience prime", dat16$STENT_Total) # "2" -> "xience prime" 
dat16$STENT_Total <- gsub("3", "xience", dat16$STENT_Total) # "3" -> "xience" 
dat16$STENT_Total <- gsub("4", "nobori", dat16$STENT_Total) # "4" -> "nobori" 
dat16$STENT_Total <- gsub("5", "endeavor resolute", dat16$STENT_Total) # "5" -> "endeavor resolute" 
dat16$STENT_Total <- gsub("6", "resolute integrity", dat16$STENT_Total) # "6" -> "resolute integrity" 
dat16$STENT_Total <- gsub("7", "promus element", dat16$STENT_Total) # "7" -> "promus element" 
dat16$STENT_Total <- gsub("8", "bms", dat16$STENT_Total) # "8" -> "bms" 
dat16$STENT_Total <- gsub("9", "cilotax", dat16$STENT_Total) # "9" -> "cilotax" 

table(dat16$STENT_Total)

# stringr 패키지가 없으면 install.package()로 먼저 install해야 함
library(stringr)

# TotalNoST1 : STENT_Total 변수에 대해 “," 개수 반환
dat16$TotalNoST1 <- str_count(dat16$STENT_Total, ",") 

# TotalNoST1을 통해 2개 이상의 스텐트 삽입한 케이스 처리
# !is.na(str_extract())를 통해 입력사항이 있으면 1개로 처리, 없으면 0개로 처리
dat16$TotalNoST2 <- ifelse(dat16$TotalNoST1 >= 1, dat16$TotalNoST1+1,
                           ifelse(!is.na(str_extract(dat16$STENT_Total, "[:graph:]+")), 1, 0 ))

dat16$TotalNoST1 <- NULL  # 필요없는 파생변수 삭제 

# 변환데이터 검토
table(dat16$TotalNoST, useNA = "always") # [결과] 0 -130건, NA – 6891건

table(dat16$TotalNoST2, useNA = "always") # [결과] 0 -3442건, NA – 0건

test <- dat16[, c("STENT_Total","TotalNoST", "TotalNoST2")]  # STENT_Total, TotalNoST, TotalNoST2 변수(열) 추출

# TotalNoST2 == 0인데, TotalNoST가 NA가 아닌 경우 출력
# TotalNoST 변수만 입력하고, 세부 STENT 변수들은 입력하지 않은 경우
head(test[(!is.na(test$TotalNoST))&(test$TotalNoST2==0),], n=10)

# TotalNoST가 NA인데, TotalNoST2 >= 1인 경우 출력
# TotalNoST 변수를 입력하지 않고, 세부 스텐트 변수들만 입력한 경우
head(test[(is.na(test$TotalNoST))&(test$TotalNoST2>=1),], n=10)

# 정규표현식 
## 날짜추출 
temp <- c("2020-07-01", "2012.12,12", "1997-12.31", "5/25/2007")
# 20으로 시작하는 4자리 숫자-1,2자리 숫자-1,2자리 숫자로 끝  
result <- str_extract_all(temp, "^20[:digit:]{2}\\-\\d{1,2}\\-\\d{1,2}$")
result

# 1,2자리 숫자/1,2자리 숫자/4자리 숫자로 끝   
result <- str_extract_all(temp, "^\\d{1,2}\\/\\d{1,2}\\/\\d{4}$")
result

## 한글2자 연속 (이름 등) 데이터 추출
temp <- c("김 이박", "원빈", "바나나", "헨리 G 김", "robert 김")
result <- str_extract_all(temp, "^[가-힣]{2,}")
result

## jpg,gif,png 파일 이름 추출 
temp <- c("abfdf.jpg", "dfaer.pn", "dpfjdfas.jpg", "abfdf .jpg")
result <- str_extract_all(temp, "\\S+\\.(jpg|gif|png)$")
result

## 전화번호 추출
temp <- c("010-3434-5656", "1234567890", "017.234.2345", "05/25/2007")
result <- str_extract_all(temp, "(\\d{3})(.|-)(\\d{3,4})(.|-)(\\d{4})$")
result

## 이메일 추출
temp <- c("dffdf89@adf@a-d.dif", "dfajdiofa@gma/il.com", "doRlf@korea.ac.kr","dfjds./foa@df65.fo.kr")
result <- str_extract_all(temp, "^[A-z0-9_+.-]+@([a-z0-9-]+\\.)+[a-z0-9]{2,4}$")
result

## 1부터 31까지의 번호
temp <- c("30", "31", "32","100", "2" ,"001", "03", "301", "0331")
result <- str_extract_all(temp, "(^0*[1-9]$)|(^(1|2)[0-9]$)|^30$|^31$")
result

### Total Stent Length 계산하기 
dat17 <- dat16

# 스텐트길이 변수명 리스트 만들기 
STENT_LEN_Names <- c("STENT1_LEN", "STENT2_LEN", "STENT3_LEN", "STENT4_LEN", 
                     "STENT1_LEN.1", "STENT2_LEN.1", "STENT3_LEN.1", "STENT4_LEN.1", 
                     "STENT1_LEN.2", "STENT2_LEN.2", "STENT3_LEN.2", "STENT4_LEN.2",
                     "STENT1_LEN.3", "STENT2_LEN.3", "STENT3_LEN.3", "STENT4_LEN.3")

# 데이터 타입을 수치형으로 변환 
dat17[, STENT_LEN_Names] = lapply(dat17[, STENT_LEN_Names], as.numeric) 

# 결측값 0 처리함수 정의 
repNA=function(x){x[is.na(x)]=0;x}

# 결측값 0 처리 
dat17[, STENT_LEN_Names] = lapply(dat17[, STENT_LEN_Names], repNA) 

# 총 스텐트 길이 합 계산 
dat17$STENT.total.length <- dat17$STENT1_LEN + dat17$STENT2_LEN + dat17$STENT3_LEN + dat17$STENT4_LEN +
  dat17$STENT1_LEN.1 + dat17$STENT2_LEN.1 + dat17$STENT3_LEN.1 + dat17$STENT4_LEN.1 +
  dat17$STENT1_LEN.2 + dat17$STENT2_LEN.2 + dat17$STENT3_LEN.2 + dat17$STENT4_LEN.2 +
  dat17$STENT1_LEN.3 + dat17$STENT2_LEN.3 + dat17$STENT3_LEN.3 + dat17$STENT4_LEN.3 

# 총 스텐트 길이가 0인 경우 NA로 처리
rep0 <- function(x){x[x==0]=NA;x}  # repNA 함수를 응용하여, 값이 0이면 NA처리하는 함수 정의
dat17$STENT.total.length <- rep0(dat17$STENT.total.length)
# 데이터 확인
table(dat17$STENT.total.length, useNA = "always")

# 데이터 검증을 위한 test 데이터 생성 및 확인
STENT_LEN_Names_final <- c("STENT1_LEN", "STENT2_LEN", "STENT3_LEN", "STENT4_LEN", 
                           "STENT1_LEN.1", "STENT2_LEN.1", "STENT3_LEN.1", "STENT4_LEN.1", 
                           "STENT1_LEN.2", "STENT2_LEN.2", "STENT3_LEN.2", "STENT4_LEN.2",
                           "STENT1_LEN.3", "STENT2_LEN.3", "STENT3_LEN.3", "STENT4_LEN.3",
                           "STENT.total.length")
test <- dat17[, STENT_LEN_Names_final]
head(test)

### Minial Stent Diameter 변수 새로 만들기
STENT_DIA_Names <- c("STENT1_DIA", "STENT2_DIA", "STENT3_DIA", "STENT4_DIA", 
                     "STENT1_DIA.1", "STENT2_DIA.1", "STENT3_DIA.1", "STENT4_DIA.1", 
                     "STENT1_DIA.2", "STENT2_DIA.2", "STENT3_DIA.2", "STENT4_DIA.2",
                     "STENT1_DIA.3", "STENT2_DIA.3", "STENT3_DIA.3", "STENT4_DIA.3")

# 데이터 타입을 수치형으로 변환 
dat17[, STENT_DIA_Names] = lapply(dat17[, STENT_DIA_Names], as.numeric) 

# 스텐트직경만 있는 데이터프레임 만들어서 최소값 계산하기
dat17.stent.dia <- dat17[,STENT_DIA_Names]
dat17.stent.dia$DIA.MIN <- apply(dat17.stent.dia, 1, min, na.rm=T) 

# 데이터 합치기 
dat17 <- cbind(dat17, dat17.stent.dia$DIA.MIN)

# 변수이름 정리하기 
require(dplyr)
dat17 <- rename(dat17, "STENT.DIA.MIN" = "dat17.stent.dia$DIA.MIN")

# 데이터 확인하고 최종 수정하기
table(dat17$STENT.DIA.MIN, useNA = "always")

# inf값을 NA로 치환하고 최종 확인하기
dat17$STENT.DIA.MIN <- ifelse(dat17$STENT.DIA.MIN==Inf, NA, dat17$STENT.DIA.MIN)
table(dat17$STENT.DIA.MIN, useNA = "always")

### Lesion Vessel 정리 ###
# "Targetlesion", LESION_VESSEL", "LESION_VESSEL.1", "LESION_VESSEL.2", "LESION_VESSEL.3"
dat18 <- dat17

# 데이터 타입을 문자형으로 변환 
LESION_VESSEL_Names <- c("Targetlesion", "LESION_VESSEL", "LESION_VESSEL.1", "LESION_VESSEL.2", "LESION_VESSEL.3")
dat18[, LESION_VESSEL_Names] = lapply(dat18[, LESION_VESSEL_Names], as.character) 

# 카테고리데이터를 다시 문자형데이터로 변환 
dat18$Targetlesion <- str_replace_all(dat18$Targetlesion, "1", "lm")  
dat18$Targetlesion <- str_replace_all(dat18$Targetlesion, "2", "lad")  
dat18$Targetlesion <- str_replace_all(dat18$Targetlesion, "3", "lcx")  
dat18$Targetlesion <- str_replace_all(dat18$Targetlesion, "4", "rca")  
dat18$Targetlesion <- str_replace_all(dat18$Targetlesion, "5", "lmladlcx")  
dat18$Targetlesion <- str_replace_all(dat18$Targetlesion, "6", "lmladrca")  
dat18$Targetlesion <- str_replace_all(dat18$Targetlesion, "7", "lmlcxrca")  
dat18$Targetlesion <- str_replace_all(dat18$Targetlesion, "8", "lmladlcxrca")  

# 변수합치기  
dat18$LESION_VESSEL.total.raw  <- paste(dat18$Targetlesion, dat18$LESION_VESSEL, dat18$LESION_VESSEL.1, dat18$LESION_VESSEL.2, dat18$LESION_VESSEL.3)
dat18$LESION_VESSEL.total <- dat18$LESION_VESSEL.total.raw

# 데이터 전처리
# 'NA','미체크', 빈칸 삭제
dat18$LESION_VESSEL.total <- str_replace_all(dat18$LESION_VESSEL.total, "NA", "")          
dat18$LESION_VESSEL.total <- str_replace_all(dat18$LESION_VESSEL.total, "미체크", "")    
dat18$LESION_VESSEL.total <- str_replace_all(dat18$LESION_VESSEL.total, "\\s*", "")   

# 데이터 타입을 소문자로 변환 
dat18$LESION_VESSEL.total <- tolower(dat18$LESION_VESSEL.total)

# 특정 문자열 추출 
dat18$TEMP1 <- ifelse(!is.na(str_extract(dat18$LESION_VESSEL.total, "lad")), 1, 0)  # lad
dat18$LESION_VESSEL.total <- str_replace_all(dat18$LESION_VESSEL.total, "lad", "")  
dat18$TEMP2 <- ifelse(!is.na(str_extract(dat18$LESION_VESSEL.total, "lda")), 1, 0)  # lad
dat18$LESION_VESSEL.total <- str_replace_all(dat18$LESION_VESSEL.total, "lda", "")  
dat18$TEMP3 <- ifelse(!is.na(str_extract(dat18$LESION_VESSEL.total, "lca")), 1, 0)  # lad
dat18$LESION_VESSEL.total <- str_replace_all(dat18$LESION_VESSEL.total, "lca", "")  
dat18$TEMP4 <- ifelse(!is.na(str_extract(dat18$LESION_VESSEL.total, "d1")), 1, 0)  # d1
dat18$LESION_VESSEL.total <- str_replace_all(dat18$LESION_VESSEL.total, "d1", "")
dat18$TEMP5 <- ifelse(!is.na(str_extract(dat18$LESION_VESSEL.total, "dia")), 1, 0)  # dia
dat18$LESION_VESSEL.total <- str_replace_all(dat18$LESION_VESSEL.total, "dia", "")
dat18$TEMP6 <- ifelse(!is.na(str_extract(dat18$LESION_VESSEL.total, "lcx")), 1, 0)  # lcx
dat18$LESION_VESSEL.total <- str_replace_all(dat18$LESION_VESSEL.total, "lcx", "")   
dat18$TEMP7 <- ifelse(!is.na(str_extract(dat18$LESION_VESSEL.total, "om1")), 1, 0)  # om1
dat18$LESION_VESSEL.total <- str_replace_all(dat18$LESION_VESSEL.total, "om1", "")
dat18$TEMP8 <- ifelse(!is.na(str_extract(dat18$LESION_VESSEL.total, "om2")), 1, 0)  # om2
dat18$LESION_VESSEL.total <- str_replace_all(dat18$LESION_VESSEL.total, "om2", "")
dat18$TEMP9 <- ifelse(!is.na(str_extract(dat18$LESION_VESSEL.total, "om")), 1, 0)  # om
dat18$LESION_VESSEL.total <- str_replace_all(dat18$LESION_VESSEL.total, "om", "")
dat18$TEMP10 <- ifelse(!is.na(str_extract(dat18$LESION_VESSEL.total, "rca")), 1, 0)  # rca
dat18$LESION_VESSEL.total <- str_replace_all(dat18$LESION_VESSEL.total, "rca", "")    
dat18$TEMP11 <- ifelse(!is.na(str_extract(dat18$LESION_VESSEL.total, "pl")), 1, 0)  # pl
dat18$LESION_VESSEL.total <- str_replace_all(dat18$LESION_VESSEL.total, "pl", "")   
dat18$TEMP12 <- ifelse(!is.na(str_extract(dat18$LESION_VESSEL.total, "pda")), 1, 0)  # pda
dat18$LESION_VESSEL.total <- str_replace_all(dat18$LESION_VESSEL.total, "pda", "")   
dat18$TEMP13 <- ifelse(!is.na(str_extract(dat18$LESION_VESSEL.total, "ramus")), 1, 0)  # ramus
dat18$LESION_VESSEL.total <- str_replace_all(dat18$LESION_VESSEL.total, "ramus", "") 
dat18$TEMP14 <- ifelse(!is.na(str_extract(dat18$LESION_VESSEL.total, "ri")), 1, 0)  # ri
dat18$LESION_VESSEL.total <- str_replace_all(dat18$LESION_VESSEL.total, "ri", "") 
dat18$TEMP15 <- ifelse(!is.na(str_extract(dat18$LESION_VESSEL.total, "intermedius")), 1, 0)  # intermedius
dat18$LESION_VESSEL.total <- str_replace_all(dat18$LESION_VESSEL.total, "intermedius", "") 
dat18$TEMP16 <- ifelse(!is.na(str_extract(dat18$LESION_VESSEL.total, "lm")), 1, 0)  # lm
dat18$LESION_VESSEL.total <- str_replace_all(dat18$LESION_VESSEL.total, "lm", "")   
dat18$TEMP17 <- ifelse(!is.na(str_extract(dat18$LESION_VESSEL.total, "leftmain")), 1, 0)  # lm
dat18$LESION_VESSEL.total <- str_replace_all(dat18$LESION_VESSEL.total, "leftmain", "")  
dat18$TEMP18 <- ifelse(!is.na(str_extract(dat18$LESION_VESSEL.total, "lima")), 1, 0)  # lima
dat18$LESION_VESSEL.total <- str_replace_all(dat18$LESION_VESSEL.total, "lima", "")   
dat18$TEMP19 <- ifelse(!is.na(str_extract(dat18$LESION_VESSEL.total, "svg")), 1, 0)  # svg
dat18$LESION_VESSEL.total <- str_replace_all(dat18$LESION_VESSEL.total, "svg", "")
dat18$TEMP20 <- ifelse(!is.na(str_extract(dat18$LESION_VESSEL.total, "graft")), 1, 0)  # graft
dat18$LESION_VESSEL.total <- str_replace_all(dat18$LESION_VESSEL.total, "graft", "")  

# 새로운 변수들 생성 (ex. LESION_VESSEL.lad : 시술병변(LAD) )
dat18$LESION_VESSEL.lad <- ifelse(dat18$TEMP1 + dat18$TEMP2 + dat18$TEMP3 + dat18$TEMP4 + dat18$TEMP5 >= 1, 1,0) # LESION_VESSEL.lad : 시술병변(LAD)
dat18$LESION_VESSEL.lcx <- ifelse(dat18$TEMP6 + dat18$TEMP7 + dat18$TEMP8 + dat18$TEMP9 >= 1, 1,0) # LESION_VESSEL.lcx : 시술병변(LCX)
dat18$LESION_VESSEL.rca <- ifelse(dat18$TEMP10 + dat18$TEMP11 + dat18$TEMP12 >= 1, 1,0) # LESION_VESSEL.rca : 시술병변(RCA)
dat18$LESION_VESSEL.ramus <- ifelse(dat18$TEMP13 + dat18$TEMP14 + dat18$TEMP15 >= 1, 1,0) # LESION_VESSEL.ramus : 시술병변(RI)
dat18$LESION_VESSEL.lm <- ifelse(dat18$TEMP16 + dat18$TEMP17 >= 1, 1,0) # LESION_VESSEL.lm : 시술병변(LM)
dat18$LESION_VESSEL.other <- ifelse(dat18$TEMP18 + dat18$TEMP19 + dat18$TEMP20 >= 1, 1,0) # LESION_VESSEL.other : 시술병변(Other)

# 필요없는 문자열 삭제 
dat18$LESION_VESSEL.total <- ifelse(dat18$LESION_VESSEL.total == "os", "", dat18$LESION_VESSEL.total)
dat18$LESION_VESSEL.total <- ifelse(dat18$LESION_VESSEL.total == "p", "", dat18$LESION_VESSEL.total)
dat18$LESION_VESSEL.total <- ifelse(dat18$LESION_VESSEL.total == "p-", "", dat18$LESION_VESSEL.total)
dat18$LESION_VESSEL.total <- ifelse(dat18$LESION_VESSEL.total == "pp", "", dat18$LESION_VESSEL.total)
dat18$LESION_VESSEL.total <- ifelse(dat18$LESION_VESSEL.total == "m", "", dat18$LESION_VESSEL.total)
dat18$LESION_VESSEL.total <- ifelse(dat18$LESION_VESSEL.total == "m-", "", dat18$LESION_VESSEL.total)
dat18$LESION_VESSEL.total <- ifelse(dat18$LESION_VESSEL.total == "mm", "", dat18$LESION_VESSEL.total)
dat18$LESION_VESSEL.total <- ifelse(dat18$LESION_VESSEL.total == "pm", "", dat18$LESION_VESSEL.total)
dat18$LESION_VESSEL.total <- ifelse(dat18$LESION_VESSEL.total == "d", "", dat18$LESION_VESSEL.total)
dat18$LESION_VESSEL.total <- ifelse(dat18$LESION_VESSEL.total == "dd", "", dat18$LESION_VESSEL.total)
dat18$LESION_VESSEL.total <- ifelse(dat18$LESION_VESSEL.total == "fail", "", dat18$LESION_VESSEL.total)

# 데이터 확인 
# LESION_VESSEL.total.raw, LESION_VESSEL.total 변수만 추출 (test)
test <- dat18[,c("LESION_VESSEL.total.raw", "LESION_VESSEL.total")]
# LESION_VESSEL.total이 “"(빈칸)이 아닌 항목만 추출 (test1)
test1 <- test[test$LESION_VESSEL.total != "",]
head(test1, n=10) #맨 위 10개 행 출력

# 우선순위대로 Target lesion 정하기 
# 이 내용은 책 본문에서는 생략했습니다
# 목표혈관(Target lesion)변수를 정의할때, 여러 혈관을 치료한 경우에는 각 혈관에 대해 우선 순위를 미리 정하여 하나의 혈관만 선별하였습니다.  
# 우선 순위: LM > LAD > LCX > RCA > Ramus > Others
dat18$Targetlesion_final <- ifelse(dat18$LESION_VESSEL.lm == 1, "LM",
                                   ifelse(dat18$LESION_VESSEL.lad == 1, "LAD",
                                          ifelse(dat18$LESION_VESSEL.lcx == 1, "LCX",
                                                 ifelse(dat18$LESION_VESSEL.rca == 1, "RCA",
                                                        ifelse(dat18$LESION_VESSEL.ramus == 1, "RI", "Others")))))
table(dat18$Targetlesion_final, useNA = "always")

## 새로운 변수 최종 확인
# 데이터 확인용 변수 추출
test2 <- dat18[, c("LESION_VESSEL.total.raw", "LESION_VESSEL.lad","LESION_VESSEL.lcx","LESION_VESSEL.rca","LESION_VESSEL.ramus", "LESION_VESSEL.lm", "LESION_VESSEL.other")]
head(test2, n=10)

# 데이터세트 복사
dat19 <- dat18
# 최종 데이터세트의 변수이름 확인
names(dat19)
# 불필요한 변수 삭제
dat19$TEMP1 <- NULL;dat19$TEMP2 <- NULL;dat19$TEMP3 <- NULL;dat19$TEMP4 <- NULL;dat19$TEMP5 <- NULL;
dat19$TEMP6 <- NULL;dat19$TEMP7 <- NULL;dat19$TEMP8 <- NULL;dat19$TEMP9 <- NULL;dat19$TEMP10 <- NULL;
dat19$TEMP11 <- NULL;dat19$TEMP12 <- NULL;dat19$TEMP13 <- NULL;dat19$TEMP14 <- NULL;dat19$TEMP15 <- NULL;
dat19$TEMP16 <- NULL;dat19$TEMP17 <- NULL;dat19$TEMP18 <- NULL;dat19$TEMP19 <- NULL;dat19$TEMP20 <- NULL;

dat19$LAD_YN <- NULL;dat19$LCX_YN <- NULL;dat19$RCA_YN <- NULL;dat19$LM_YN <- NULL;dat19$RAMUS_YN <- NULL;
dat19$Targetlesion <- NULL;dat19$NoDzVV <- NULL;dat19$MultivvPCI <- NULL;dat19$TotalNoST <- NULL;

dat19$LESION_0 <- NULL;dat19$LESION_VESSEL <- NULL;
dat19$STENT1 <- NULL;dat19$STENT1_DIA <- NULL;dat19$STENT1_LEN <- NULL;
dat19$STENT2 <- NULL;dat19$STENT2_DIA <- NULL;dat19$STENT2_LEN <- NULL;
dat19$STENT3 <- NULL;dat19$STENT3_DIA <- NULL;dat19$STENT3_LEN <- NULL;
dat19$STENT4 <- NULL;dat19$STENT4_DIA <- NULL;dat19$STENT4_LEN <- NULL;

dat19$LESION_0.1 <- NULL;dat19$LESION_VESSEL.1 <- NULL;
dat19$STENT1.1 <- NULL;dat19$STENT1_DIA.1 <- NULL;dat19$STENT1_LEN.1 <- NULL;
dat19$STENT2.1 <- NULL;dat19$STENT2_DIA.1 <- NULL;dat19$STENT2_LEN.1 <- NULL;
dat19$STENT3.1 <- NULL;dat19$STENT3_DIA.1 <- NULL;dat19$STENT3_LEN.1 <- NULL;
dat19$STENT4.1 <- NULL;dat19$STENT4_DIA.1 <- NULL;dat19$STENT4_LEN.1 <- NULL;

dat19$LESION_0.2 <- NULL;dat19$LESION_VESSEL.2 <- NULL;
dat19$STENT1.2 <- NULL;dat19$STENT1_DIA.2 <- NULL;dat19$STENT1_LEN.2 <- NULL;
dat19$STENT2.2 <- NULL;dat19$STENT2_DIA.2 <- NULL;dat19$STENT2_LEN.2 <- NULL;
dat19$STENT3.2 <- NULL;dat19$STENT3_DIA.2 <- NULL;dat19$STENT3_LEN.2 <- NULL;
dat19$STENT4.2 <- NULL;dat19$STENT4_DIA.2 <- NULL;dat19$STENT4_LEN.2 <- NULL;

dat19$LESION_0.3 <- NULL;dat19$LESION_VESSEL.3 <- NULL;
dat19$STENT1.3 <- NULL;dat19$STENT1_DIA.3 <- NULL;dat19$STENT1_LEN.3 <- NULL;
dat19$STENT2.3 <- NULL;dat19$STENT2_DIA.3 <- NULL;dat19$STENT2_LEN.3 <- NULL;
dat19$STENT3.3 <- NULL;dat19$STENT3_DIA.3 <- NULL;dat19$STENT3_LEN.3 <- NULL;
dat19$STENT4.3 <- NULL;dat19$STENT4_DIA.3 <- NULL;dat19$STENT4_LEN.3 <- NULL

# 최종 데이터세트 저장
write.csv(dat19, file="data\\dat19.csv", fileEncoding = "utf8")

# Ch.2.3 탐색적 데이터 분석
# 2.3.1 카테고리형 변수 분석

dat19 <-  read.csv("data\\dat19.csv")  # 최종 데이터세트 불러오기
dat19 <- dat19[,-1]              # 맨 왼쪽(첫번째) 인덱스 컬럼 삭제
dat20 <- dat19                   # dat20에 복사

# moonBook 패키지가 없으면 install.package()로 먼저 인스톨해야 함
require(moonBook)

A1 <- mytable (Hosp ~ Dx+Age+Sex+Smoking+DM+HTN+Dyslipid+CYP2C19+TotalNoST2+STENT.DIA.MIN+Targetlesion_final, data = dat20)
mycsv(A1, file="data\\A1.csv") #분석결과를CSV파일로저장

# 2.3.2 수치형 변수 분석

par(mfrow = c(2, 1))
plot(dat20$Height)
plot(dat20$BMI)

par(mfrow = c(1, 1))

# BMI변수의 기관별 NA 개수
table(dat20$Hosp[is.na(dat20$BMI)])

plot(dat20$HB)

table(dat20$Hosp[is.na(dat20$HB)]) # NA 개수
table(dat20$Hosp[!is.na(dat20$HB)]) # NA 아닌 개수

plot(dat20$CR)

# 2.3.3 날짜 오류 확인하기 

class(dat20$Enroll_DATE)

# 데이터 타입을 날짜로 변환 
DATE_Names <- c("Enroll_DATE", "PCI_DATE", "VERI_P_DATE", "DIS_DATE", 
                "FINAL_FU_DATE", "DEATH_DATE", "MI_DATE", "ST_DATE", 
                "CVA_DATE", "RR_DATE")
dat20[, DATE_Names] = lapply(dat20[, DATE_Names], as.Date) 
class(dat20$Enroll_DATE)

dat21 <- dat20

# 날짜 데이터만 있는 데이터프레임 만들어서 계산하기
DATE_Names1 <- c("PCI_DATE", "VERI_P_DATE", "DIS_DATE", 
                 "FINAL_FU_DATE", "DEATH_DATE", "MI_DATE", "ST_DATE", 
                 "CVA_DATE", "RR_DATE")
dat21.DATE <- dat21[,DATE_Names1]
dat21.DATE$MIN <- apply(dat21.DATE, 1, min, na.rm = T)  # 최소 날짜 추출 
dat21.DATE$MAX <- apply(dat21.DATE, 1, max, na.rm = T)  # 최소 날짜 추출 

# 데이터 합치기 
dat21 <- cbind(dat21, dat21.DATE$MIN, dat21.DATE$MAX) # 원래 데이터세트와 다시 합치기
dat21$DATE.MIN <- as.Date(dat21$`dat21.DATE$MIN`)     # 날짜형으로 변환하여 새로운 변수에 할당
dat21$DATE.MAX <- as.Date(dat21$`dat21.DATE$MAX`)
dat21$`dat21.DATE$MIN` <- NULL; dat21$`dat21.DATE$MAX`<- NULL  # 불필요한 파생변수 삭제

# 데이터 검토 
## 검토할 날짜 변수만 추출
dat21.DATE.Summary <- dat21.DATE[,c('PCI_DATE', 'MIN', 'DIS_DATE', 'DEATH_DATE', 'FINAL_FU_DATE', 'MAX')]
## DEATH_DATE와 FINAL_FU_DATE가 다른 경우 출력
head(filter(dat21.DATE.Summary,DEATH_DATE != FINAL_FU_DATE), head=10)

# 변수 정리하기 
dat21$FINAL_FU_DATE <- dat21$DATE.MAX
dat21$FINAL_FU_DATE <- ifelse(is.na(dat21$DEATH_DATE), dat21$FINAL_FU_DATE, dat21$DEATH_DATE)
dat21$FINAL_FU_DATE <- as.Date(dat21$FINAL_FU_DATE, origin = "1970-01-01")
# R에서는 내부적으로 1970년 1월 1일 이후 경과된 일수를 저장한다. 

# PCI_DATE와 MIN이 다른 경우 10개 출력
head(filter(dat21.DATE.Summary,PCI_DATE != MIN), head=10)

# PCI_DATE, FINAL_FU_DATE 논리 검증 
# FINAL_FU_DATE가 PCI_DATE보다 더 빠른 날짜인 경우 10개 출력
test <- filter(dat21,FINAL_FU_DATE < PCI_DATE)
test[,c("PCI_DATE", "DIS_DATE", "FINAL_FU_DATE", "DEATH_DATE")]
# FINAL_FU_DATE가 PCI_DATE보다 더 빠른 날짜인 경우의 행 개수 출력
nrow(test[,c("PCI_DATE", "DIS_DATE", "FINAL_FU_DATE", "DEATH_DATE")])

# 데이터 삭제 
dat22 <- dat21

# test에 있는 56건 케이스의 Index를 저장
IndexNames <- factor(test$Index)
# for문을 이용하여 56건에 해당하는 index가 아닌 행만 dat22로 할당
for (i in IndexNames){
  dat22 <- dat22[!(dat22$Index == c(i)), ]
}
# 56건 케이스가 삭제된 것 확인
dim(dat21);dim(dat22)

# 2.4 데이터 분할 및 관리
# 2.4.1 데이터 나누기

dat23 <- dat22
Hospital_A <- dat23[dat23$Hosp=="Hospital_A",]
Hospital_B <- dat23[dat23$Hosp=="Hospital_B",]
Hospital_C <- dat23[dat23$Hosp=="Hospital_C",]

write.csv(Hospital_B, file="data\\Hospital_B.csv", fileEncoding = "utf8")
Hospital_B <- read.csv("data\\Hospital_B.csv", encoding="UTF-8")

library(writexl)
writexl::write_xlsx(Hospital_B, path = "data\\Hospital_B.xlsx")

library(readxl)
Hospital_B_guess <- read_excel("data\\Hospital_B.xlsx", col_type="guess") 
Hospital_B_text <- read_excel("data\\Hospital_B.xlsx", col_type="text") 
str(Hospital_B_guess)
str(Hospital_B_text)

library(xlsx)
write.xlsx(Hospital_B, file="data\\Hospital_B.xlsx")
Hospital_B <- read.xlsx(file="data\\Hospital_B.xlsx", 1, encoding="UTF-8")

# 파일 인코딩 
이름 <- c("A", "B", "C")
Name <- c("A", "B", "C")
Name2 <- c("가", "나", "다")
df <- data.frame(이름, Name, Name2)

write.csv(df, file="data\\df_utf8.csv", fileEncoding = "utf8", row.names = FALSE)
write.csv(df, file="data\\df_euc.csv", fileEncoding = "euc-kr", row.names = FALSE)
write.csv(df, file="data\\df_cp949.csv", fileEncoding = "cp949", row.names = FALSE)
writexl::write_xlsx(df, path = "df.xlsx")

df_utf8 <- read.csv("data\\df_utf8.csv")
df_euc <- read.csv("data\\df_euc.csv", fileEncoding="euc-kr", encoding="utf-8")
df_cp949 <- read.csv("data\\df_cp949.csv", fileEncoding="cp949", encoding="utf-8")
df_xlsx <- read_xlsx("data\\df.xlsx", col_types = "text")

# 2.4.2 데이터 합치기 

## 데이터 합치기 (세부기관)

# dat23에서 Hospital_A 데이터 분리하기
Hospital_A <- dat23[dat23$Hosp=="Hospital_A",]

# Index만 분리하기
Index <- Hospital_A[,c("Index")]

# 병원등록번호(ID) 생성하기 
## stri_rand_strings() 함수를 사용하여 무작위로 생성 
## 0~9를 사용하여 8자리의 코드를 700개 생성
library(stringi)
ID <- stri_rand_strings(700, 8, '[0-9]')

# ID와 Index로 이루어진 메타테이블 생성
Hospital_A_Meta <- data.frame(ID, Index)
summary(Hospital_A_Meta)

# merge()를 사용하여 2개의 데이터세트를 합침
Hospital_A.final <- merge(x=Hospital_A_Meta, y=Hospital_A, by="Index", all.y = TRUE)
head(Hospital_A.final[,c("Index", "ID", "Hosp", "Dx", "Age", "Sex", "PCI_DATE" )], n=10)

## 데이터 합치기 (주관기관)

dim(Hospital_A)
head(names(Hospital_A), n=10)

# ComparisonNames() 코드 
ComparisonNames <- function(x,y){
  Result = NULL
  Col_names1 = names(x); Col_names3 = names(x)
  Col_names2 = names(y); Col_names4 = names(y)
  for (i in 1:length(Col_names1)){
    for (m in 1:length(Col_names2)){
      ifelse(Col_names1[i] == Col_names2[m], Col_names1 <- Col_names1[-i], Col_names1[i])
    }
  }
  for (i in 1:length(Col_names4)){
    for (m in 1:length(Col_names3)){
      ifelse(Col_names4[i] == Col_names3[m], Col_names4 <- Col_names4[-i], Col_names4[i])
    }
  }
  Result[[1]] = Col_names1
  Result[[2]] = Col_names4
  return(
    Result
  )
}

ComparisonNames(Hospital_A, dat1)

str(Hospital_A)

# 전체 데이터세트에서 실제 입력값이 없는 인자의 level 삭제 (droplevels())
# for()문을 사용하여 Hospital_A 데이터세트에 있는 factor형 변수들에 대해 droplevel() 함수를 적용
for (i in 1:length(names(Hospital_A))){
  ifelse(class(Hospital_A[[i]]) == "factor",
         Hospital_A[[i]] <- droplevels(Hospital_A[[i]]), Hospital_A[[i]])
}

# dfstr() 코드 (dataframe structure)
dfstr <- function(x){
  Var_name = names(x)
  Var_class = NULL;Var_example = NULL;Var_width = NULL
  Value_min = NULL;Value_name = NULL;Var_examples = NULL
  for (i in 1:ncol(x)){
    Var_class[i] = class(x[,c(i)])
    Value_name = names(sort(table(x[,c(i)]), decreasing = F))
    Var_width[i] = paste0("[",length(Value_name),"]")
    for(m in 1:length(Value_name)){
      Var_example = paste(Var_example, Value_name[m])
    }
    Var_examples[i] = Var_example
    Var_example = ""
  }
  Var_examples = ifelse(nchar(Var_examples)<30, Var_examples, paste0(substring(Var_examples, 1, 27),"..."))
  Data.summary = data.frame(Var_name, Var_class, Var_width, Var_examples)
  return(Data.summary)
}

dfstr(Hospital_A)

# 데이터 합치기 

library(data.table)

lst <- list(Hospital_A, Hospital_B, Hospital_C)
Hospital_ABC <- rbindlist(lst)

nrow_A <- nrow(Hospital_A);nrow_B <- nrow(Hospital_B)
nrow_C <- nrow(Hospital_C);nrow_ABC <- nrow(Hospital_ABC)

cat(nrow_A, nrow_B, nrow_C, nrow_ABC, sum(nrow_A, nrow_B, nrow_C))
