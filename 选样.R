# import data -------------------------------------------------------------

#数据导入并作初步准备
setwd("D:/R/Database")  #设置读取文件的路径
data<-read.csv("2012begin.csv",colClasses = "character")  #读取数据并将各变量的初始类型设置为字符型
data$CHANGE_DATE<-as.Date(data$CHANGE_DATE,"%Y/%m/%d")  #将CHANGE_DATE的类型改为日期型
data$ISSUED_VOL_AFTER<-as.numeric(data$ISSUED_VOL_AFTER)  #将发行量的类型改为数值型
data<-data[order(data$CHANGE_DATE),]  #按照发行日期排序

# data prepared -----------------------------------------------------------

#生成新的变量new，当该券为新发时，new=1
data$new<-1
for(t in 1:length(data$ISSUED_VOL_AFTER)) {
  list<-data[1:t,]$SECURITY_CODE
  if(data$SECURITY_CODE[t+1] %in% list) data$new[t+1]<-0
}

#设置选样起始日期
#Original_Date<-as.Date(c("2012-08-20"))

#设置空数据框
result<-data.frame(DATE=NA,SECURITY_CODE=NA,VOL=NA,CHANGE_DATE=NA,WEIGHT=NA,new=NA)

#设置阈值，注：此处导入的源数据的发行量的单位是“百”
x1<-1e8
x2<-5e8
x3<-2e9

#设置各券占比
r1<-c(.6,.3,.1)
r2<-c(.75,.25,0)

# select bond -------------------------------------------------------------

date_list<-data[!duplicated(data$CHANGE_DATE),]$CHANGE_DATE
#date_list<-date_list[order(date_list)]  #排序，import data步骤已经排序，此处不再重复
i<-1
j<-0
while(i<=length(date_list)){
  sample_data<-subset(data,CHANGE_DATE<=date_list[i])
  # L<-length(sample_data$CITY)
  # if(sample_data$new[L]==1) sample_data<-sample_data[1:L-1,]  #新发的券第二天纳入指数
  sample_data<-sample_data[order(sample_data$CHANGE_DATE,decreasing = TRUE),]
  if(length(sample_data[!duplicated(sample_data$SECURITY_CODE),]$CITY)>2){
  k<-0
  for(j in (j+1):(j+3)){
    for(k in (k+1):length(sample_data$CITY)){
      if(sample_data[k,7]>=x1){
        if(j %% 3==1){    #本组第一支券无需判断CODE是否重复    
          result[j,1]<-date_list[i]+sample_data[k,9] #若券为新发，则日期+new(1)
          result[j,2]<-sample_data[k,2]
          result[j,3]<-min(sample_data[k,7],x3)
          result[j,4]<-sample_data[k,8]
          result[j,6]<-sample_data[k,9]
          #if(sample_data[k,7]>=x2) result[j,4]<-r2[j%%3+1]
          #else result[j,4]<-r1[j%%3+1]
          break
        }else if(j %% 3==2){    #本组第二/三支券需要判断CODE是否重复
          if(sample_data[k,2]==result[j-1,2]) next() else {
              result[j,1]<-result[j-1,1]
              result[j,2]<-sample_data[k,2]
              result[j,3]<-min(sample_data[k,7],x3)
              result[j,4]<-sample_data[k,8]
              result[j,6]<-sample_data[k,9]
              #if(sample_data[k,7]>=x2) result[j,4]<-r2[j%%3+1]
              #else result[j,4]<-r1[j%%3+1]
              break
              }
            }else {
            if(sample_data[k,2]==result[j-1,2] | sample_data[k,2]==result[j-2,2]) next() else {
              result[j,1]<-result[j-2,1]
              result[j,2]<-sample_data[k,2]
              result[j,3]<-min(sample_data[k,7],x3)
              result[j,4]<-sample_data[k,8]
              result[j,6]<-sample_data[k,9]
              #if(sample_data[k,7]>=x2) result[j,4]<-r2[j%%3+1]
              #else result[j,4]<-r1[j%%3+1]
              break
              }
            }
        }else next()
    }
  }
  if(result[j-2,3]>=x2){
    result[j-2,5]<-r2[1]
    result[j-1,5]<-r2[2]
    result[j,5]<-r2[3]
    }else {
    result[j-2,5]<-r1[1]
    result[j-1,5]<-r1[2]
    result[j,5]<-r1[3]
    }
  i<-i+1
  }else i<-i+1
}
#result$DATE<-result$DATE+result$new
result$DATE<-as.Date(result$DATE,origin = "1970-01-01")
result$CHANGE_DATE<-as.Date(result$CHANGE_DATE,origin = "1970-01-01")

# export result -----------------------------------------------------------

name<-paste("指数选样_",Sys.Date(),".csv")
write.csv(result,name)
