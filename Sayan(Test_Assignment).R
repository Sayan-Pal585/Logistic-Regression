#-----------------------------------------------------------------------------------------------------------
#Test_Assignment
#-----------------------------------------------------------------------------------------------------------

#Loading Data
account<-read.csv(file.choose(),header=TRUE)
View(account)

enquiry<-read.csv(file.choose(),header=TRUE)
View(enquiry)

data<-read.csv(file.choose(),header=TRUE)
View(data)

data_account=merge(x = data[ , c("customer_no","entry_time")],y = account[,c(-1)], by = c("customer_no"))

data_account[is.na(data_account)] <- 0

data_enquiry=merge(x = data[ , c("customer_no","entry_time")],y = enquiry[,c(-1)], by = c("customer_no"))

data_enquiry[is.na(data_enquiry)] <- 0

#sum avg
data_account$total_diff_lastpaymt_opened_dt <-(as.numeric(substr(strptime(data_account$last_paymt_dt, format = "%d-%b-%y"),1,4))*12 + as.numeric(substr(strptime(data_account$last_paymt_dt, format = "%d-%b-%y"),6,7))) - (as.numeric(substr(strptime(data_account$opened_dt, format = "%d-%b-%y"),1,4))*12 + as.numeric(substr(strptime(data_account$opened_dt, format = "%d-%b-%y"),6,7)))

#avg
data_account$utilisation <-ifelse(data_account$creditlimit>0,data_account$cur_balance_amt/data_account$creditlimit,0)           

#avg
data_account$payment_history_mean_length=nchar(gsub('^.{3}', '',gsub('.{3}$', '', data_account$paymenthistory1)))/3
 
v <- vector(mode="numeric", length=nrow(data_account))

for (i in 1:nrow(data_account))
    {
	v[i]=ifelse(max(as.numeric(
	substring(gsub('^.{3}', '',gsub('.{3}$', '', data_account[i,]$paymenthistory1)),
	seq(1, nchar(gsub('^.{3}', '',gsub('.{3}$', '', data_account[i,]$paymenthistory1))),3),    
	seq(3, nchar(gsub('^.{3}', '',gsub('.{3}$', '', data_account[i,]$paymenthistory1))),3))),na.rm=T)>29,1,0)
	print (i)
	}

#avg
data_account$payment_history_avg_dpd_0_29_bucket<-as.data.frame(v)

v <- vector(mode="numeric", length=nrow(data_account))

for (i in 1:nrow(data_account))
   {
   	v[i]=9999
   	if (max(as.numeric(
	substring(gsub('^.{3}', '',gsub('.{3}$', '', data_account[i,]$paymenthistory1)),
	seq(1, nchar(gsub('^.{3}', '',gsub('.{3}$', '', data_account[i,]$paymenthistory1))),3),    
	seq(3, nchar(gsub('^.{3}', '',gsub('.{3}$', '', data_account[i,]$paymenthistory1))),3))),na.rm=T)>=30)
	{
   	j=1
    while((as.numeric(substring(gsub('^.{3}', '',gsub('.{3}$', '', data_account[i,]$paymenthistory1)),
	seq(1, nchar(gsub('^.{3}', '',gsub('.{3}$', '', data_account[i,]$paymenthistory1))),3),    
	seq(3, nchar(gsub('^.{3}', '',gsub('.{3}$', '', data_account[i,]$paymenthistory1))),3))))[j]<30 |
	is.na((as.numeric(substring(gsub('^.{3}', '',gsub('.{3}$', '', data_account[i,]$paymenthistory1)),
	seq(1, nchar(gsub('^.{3}', '',gsub('.{3}$', '', data_account[i,]$paymenthistory1))),3),    
	seq(3, nchar(gsub('^.{3}', '',gsub('.{3}$', '', data_account[i,]$paymenthistory1))),3))))[j])==TRUE)
	{
		j=j+1
	}
	v[i]=j
	
    }
    print (v[i])
    }

#min
data_account$min_months_last_30_plus <-as.data.frame(v)$v
v1<-cbind.data.frame(data_account$customer_no,as.data.frame(v)$v)
colnames(v1)[1]="customer_no"
colnames(v1)[2]="min_months_last_30_plus"
v2<-cbind.data.frame(data_account$customer_no,data_account$payment_history_avg_dpd_0_29_bucket)
colnames(v2)[1]="customer_no"
colnames(v2)[2]="payment_history_avg_dpd_0_29_bucket"

data_account1<-aggregate(formula = cbind(total_diff_lastpaymt_opened_dt)~customer_no, 
           data = data_account,
           FUN = sum,na.rm=TRUE,na.action="na.pass")
           
data_account2<-aggregate(formula = cbind(total_diff_lastpaymt_opened_dt,utilisation,payment_history_mean_length)~customer_no, 
           data = data_account,
           FUN = mean,na.rm=TRUE,na.action="na.pass")

colnames(data_account2)[2]="mean_diff_lastpaymt_opened_dt"

data_account3<-aggregate(formula = cbind(payment_history_avg_dpd_0_29_bucket)~customer_no, 
           data = v2,
           FUN = mean,na.rm=TRUE,na.action="na.pass")

data_account4<-aggregate(formula = cbind(min_months_last_30_plus)~customer_no, 
           data = v1,
           FUN = min,na.rm=TRUE,na.action="na.pass")
           
data_account4$rec<-ifelse(data_account4$min_months_last_30_plus==9999,0,data_account4$min_months_last_30_plus) 
data_account5<- data_account4[c(-2)]   
colnames(data_account5)[2]="min_months_last_30_plus"
           
data_account12=merge(x = data_account1,y = data_account2, by = c("customer_no"))
data_account34=merge(x = data_account3,y = data_account5, by = c("customer_no"))
data_account_final=merge(x = data_account12,y = data_account34, by = c("customer_no"))

data_enquiry$count_enquiry_recency_365 <- ifelse(as.Date(substr(strptime(data_enquiry$entry_time, format = "%d-%b-%y"),1,10)) -
as.Date(substr(strptime(data_enquiry$enquiry_dt, format = "%d-%b-%y"),1,10))>365,0,1)

data_enquiry$count_enquiry_recency_90 <- ifelse(as.Date(substr(strptime(data_enquiry$entry_time, format = "%d-%b-%y"),1,10)) -
as.Date(substr(strptime(data_enquiry$enquiry_dt, format = "%d-%b-%y"),1,10))>90,0,1)

data_enquiry$mean_diff_entry_time_enquiry_dt <-
(as.numeric(substr(strptime(data_enquiry$entry_time, format = "%d-%b-%y"),1,4))*12 + as.numeric(substr(strptime(data_enquiry$entry_time, format = "%d-%b-%y"),6,7))) - (as.numeric(substr(strptime(data_enquiry$enquiry_dt, format = "%d-%b-%y"),1,4))*12 + as.numeric(substr(strptime(data_enquiry$enquiry_dt, format = "%d-%b-%y"),6,7)))

vect=c(1, 2, 3, 4, 7, 11, 13, 14, 15, 17, 31, 32, 33, 34, 42, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60)

data_enquiry$perc_unsecured_others <- ifelse(is.na(match(data_enquiry$enq_purpose, vect))==TRUE,0,1)

library("plyr", lib.loc="~/R/win-library/3.2")
df<-count(data_enquiry, c("customer_no", "enq_purpose"))

df1<-aggregate(formula = cbind(freq)~customer_no, 
           data = df,
           FUN = max,na.rm=TRUE,na.action="na.pass")
 
colnames(df1)[2]="max_freq_enquiry"
           
data_enquiry1<-aggregate(formula = cbind(count_enquiry_recency_365,count_enquiry_recency_90)~customer_no, 
           data = data_enquiry,
           FUN = sum,na.rm=TRUE,na.action="na.pass")
           
data_enquiry2<-aggregate(formula = cbind(mean_diff_entry_time_enquiry_dt,perc_unsecured_others)~customer_no, 
           data = data_enquiry,
           FUN = mean,na.rm=TRUE,na.action="na.pass")

data_enquiry12=merge(x = data_enquiry1,y = data_enquiry2, by = c("customer_no"))
data_enquiry_final=merge(x = data_enquiry12,y = df1, by = c("customer_no"))

data_account_enquiry=merge(x = data_enquiry_final,y = data_account_final, by = c("customer_no"))
data_final=merge(x = data_account_enquiry,y = data, by = c("customer_no"))
View(data_final)

write.csv(data_final, file = "data_final_output.csv")

#Modeling with data_final

str(data_final)
names(data_final)
attributes(data_final)

#missing value check
apply(data_final, 2, function(x){sum(is.na(x))})

data_final1=data_final[,c(-22,-23,-24,-27,-32,-34,-35,-36,-37,-38,-41,-42,-43,-44,-46,-47,-50,-51,-52,-57,-58,-59,-60,-61,-62,-63,-64,-65,-67,-68,-71,-72,-73,-74,-75,-76,-77,-80,-84,-86,-87,-88,-89,-91,-93)]
View(data_final1)

#missing value check
apply(data_final1, 2, function(x){sum(is.na(x))})
data_final2=na.omit(data_final1)
View(data_final2)
data_final3=data_final2[,c(-13,-14,-15,-16,-19,-22,-23,-26,-27)]
View(data_final3)
data_final4=data_final3[,c(-1,-18)]
View(data_final4)

glm.mod=glm(Bad_label~.,data = data_final4,family = binomial(logit))
summary(glm.mod)

alias(lm(Bad_label~.,data = data_final4))

data_final5=data_final4[,c(-14,-33,-34)]
View(data_final5)

glm.mod1=glm(Bad_label~.,data = data_final5,family = binomial(logit))
summary(glm.mod1)

#For multicollinearity check
library("VIF", lib.loc="~/R/win-library/3.2")
vif(glm.mod)

#variables with vif >3 removed

data_final6=data_final5[,c(-1,-6,-16,-20,-22,-27,-28,-29)]
View(data_final6)
glm.mod3=glm(Bad_label~.,data = data_final6,family = binomial(logit))
summary(glm.mod3)

#Variable selection
m=step(glm.mod3,direction = "both")

data_final7=data_final6[,c(1,3,4,7,8,10,11,12,13,14,19,20,21,22,24,27)]
View(data_final7)
attach(data_final7)
glm.mod4=glm(Bad_label~.,data = data_final7,family = binomial(logit))
summary(glm.mod4)

library("MLmetrics", lib.loc="~/R/win-library/3.2")
Gini(y_pred = glm.mod4$fitted.values, y_true = data_final7$Bad_label)

p=exp(-1.194e+00+(1.287e-01*count_enquiry_recency_90)+(8.965e-01*perc_unsecured_others)+(-2.329e-02*max_freq_enquiry)+(-4.697e-02*payment_history_mean_length)+(1.666e+00*payment_history_avg_dpd_0_29_bucket)+(-2.551e-03*feature_3)+(1.754e-01*feature_4)+(-3.678e-06*feature_7)+(-1.209e-02*feature_14)+( 1.852e-01*feature_25)+(-1.838e-02*feature_41)+(-8.965e-02*feature_42)+(-2.501e-06*feature_52)+(2.824e-04*feature_65)+(7.702e-03*feature_71))/(1+exp((-1.194e+00+(1.287e-01*count_enquiry_recency_90)+(8.965e-01*perc_unsecured_others)+(-2.329e-02*max_freq_enquiry)+(-4.697e-02*payment_history_mean_length)+(1.666e+00*payment_history_avg_dpd_0_29_bucket)+(-2.551e-03*feature_3)+(1.754e-01*feature_4)+(-3.678e-06*feature_7)+(-1.209e-02*feature_14)+( 1.852e-01*feature_25)+(-1.838e-02*feature_41)+(-8.965e-02*feature_42)+(-2.501e-06*feature_52)+(2.824e-04*feature_65)+(7.702e-03*feature_71))))
p1=1000*p
score_data=data.frame(p1,data_final7$Bad_label)
View(score_data)

score_data$decile<-with(score_data, cut(p1, breaks=quantile(p1, probs=seq(0,1, by=0.1), na.rm=TRUE),include.lowest=TRUE))

score_data<-within(score_data, decile<- as.integer(cut(p1, quantile(p1, probs=0:10/10), include.lowest=TRUE)))

y<-cbind(decile=aggregate(score_data$bad_label, by=list(decile=score_data$decile), FUN=sum)[1],
         
         freq=aggregate(score_data$bad_label, by=list(decile=score_data$decile), FUN=sum)[2],
         
         total_freq=as.data.frame(table(score_data$decile))[2])

table(score_data$data_final7.Bad_label,score_data$decile)
rate_of_bad_value=c((22/(1382+22)),(17/(1386+17)),(17/(1386+17)),(24/(1379+24)),(30/(1373+30)),(36/(1367+36)),(48/(1355+48)),(65/(1338+65)),(103/(1300+103)),(131/(1272+131)))

decile=quantile(p, prob = seq(0, 1, length = 11), type = 5)
table(data_final7$Bad_label)
bad_value_freq=493

#Rank ordering
rank_ordering=decile/bad_value_freq
as.data.frame(rank_ordering)

#Loading test-data files

test_account<-read.csv(file.choose(),header=TRUE)
View(test_account)

test_enquiry<-read.csv(file.choose(),header=TRUE)
View(test_enquiry)

test_data<-read.csv(file.choose(),header=TRUE)
View(test_data)

data_test_account=merge(x = data[ , c("customer_no","entry_time")],y = account[,c(-1)], by = c("customer_no"))

data_test_account[is.na(data_test_account)] <- 0

data_test_enquiry=merge(x = data[ , c("customer_no","entry_time")],y = enquiry[,c(-1)], by = c("customer_no"))

data_test_enquiry[is.na(data_test_enquiry)] <- 0

#sum avg
data_test_account$total_diff_lastpaymt_opened_dt <-(as.numeric(substr(strptime(data_test_account$last_paymt_dt, format = "%d-%b-%y"),1,4))*12 + as.numeric(substr(strptime(data_test_account$last_paymt_dt, format = "%d-%b-%y"),6,7))) - (as.numeric(substr(strptime(data_test_account$opened_dt, format = "%d-%b-%y"),1,4))*12 + as.numeric(substr(strptime(data_test_account$opened_dt, format = "%d-%b-%y"),6,7)))

#avg
data_test_account$utilisation <-ifelse(data_test_account$creditlimit>0,data_test_account$cur_balance_amt/data_test_account$creditlimit,0)           

#avg
data_test_account$payment_history_mean_length=nchar(gsub('^.{3}', '',gsub('.{3}$', '', data_test_account$paymenthistory1)))/3

v <- vector(mode="numeric", length=nrow(data_test_account))

for (i in 1:nrow(data_test_account))
{
  v[i]=ifelse(max(as.numeric(
    substring(gsub('^.{3}', '',gsub('.{3}$', '', data_test_account[i,]$paymenthistory1)),
              seq(1, nchar(gsub('^.{3}', '',gsub('.{3}$', '', data_test_account[i,]$paymenthistory1))),3),    
              seq(3, nchar(gsub('^.{3}', '',gsub('.{3}$', '', data_test_account[i,]$paymenthistory1))),3))),na.rm=T)>29,1,0)
  print (i)
}

#avg
data_test_account$payment_history_avg_dpd_0_29_bucket<-as.data.frame(v)

v <- vector(mode="numeric", length=nrow(data_test_account))

for (i in 1:nrow(data_test_account))
{
  v[i]=9999
  if (max(as.numeric(
    substring(gsub('^.{3}', '',gsub('.{3}$', '', data_test_account[i,]$paymenthistory1)),
              seq(1, nchar(gsub('^.{3}', '',gsub('.{3}$', '', data_test_account[i,]$paymenthistory1))),3),    
              seq(3, nchar(gsub('^.{3}', '',gsub('.{3}$', '', data_test_account[i,]$paymenthistory1))),3))),na.rm=T)>=30)
  {
    j=1
    while((as.numeric(substring(gsub('^.{3}', '',gsub('.{3}$', '', data_test_account[i,]$paymenthistory1)),
                                seq(1, nchar(gsub('^.{3}', '',gsub('.{3}$', '', data_test_account[i,]$paymenthistory1))),3),    
                                seq(3, nchar(gsub('^.{3}', '',gsub('.{3}$', '', data_test_account[i,]$paymenthistory1))),3))))[j]<30 |
          is.na((as.numeric(substring(gsub('^.{3}', '',gsub('.{3}$', '', data_test_account[i,]$paymenthistory1)),
                                      seq(1, nchar(gsub('^.{3}', '',gsub('.{3}$', '', data_test_account[i,]$paymenthistory1))),3),    
                                      seq(3, nchar(gsub('^.{3}', '',gsub('.{3}$', '', data_test_account[i,]$paymenthistory1))),3))))[j])==TRUE)
    {
      j=j+1
    }
    v[i]=j
    
  }
  print (v[i])
}

#min
data_test_account$min_months_last_30_plus <-as.data.frame(v)$v
v1<-cbind.data.frame(data_test_account$customer_no,as.data.frame(v)$v)
colnames(v1)[1]="customer_no"
colnames(v1)[2]="min_months_last_30_plus"
v2<-cbind.data.frame(data_test_account$customer_no,data_test_account$payment_history_avg_dpd_0_29_bucket)
colnames(v2)[1]="customer_no"
colnames(v2)[2]="payment_history_avg_dpd_0_29_bucket"

data_test_account1<-aggregate(formula = cbind(total_diff_lastpaymt_opened_dt)~customer_no, 
                         data = data_test_account,
                         FUN = sum,na.rm=TRUE,na.action="na.pass")

data_test_account2<-aggregate(formula = cbind(total_diff_lastpaymt_opened_dt,utilisation,payment_history_mean_length)~customer_no, 
                         data = data_test_account,
                         FUN = mean,na.rm=TRUE,na.action="na.pass")

colnames(data_test_account2)[2]="mean_diff_lastpaymt_opened_dt"

data_test_account3<-aggregate(formula = cbind(payment_history_avg_dpd_0_29_bucket)~customer_no, 
                         data = v2,
                         FUN = mean,na.rm=TRUE,na.action="na.pass")

data_test_account4<-aggregate(formula = cbind(min_months_last_30_plus)~customer_no, 
                         data = v1,
                         FUN = min,na.rm=TRUE,na.action="na.pass")

data_test_account4$rec<-ifelse(data_test_account4$min_months_last_30_plus==9999,0,data_test_account4$min_months_last_30_plus) 
data_test_account5<- data_test_account4[c(-2)]   
colnames(data_test_account5)[2]="min_months_last_30_plus"

data_test_account12=merge(x = data_test_account1,y = data_test_account2, by = c("customer_no"))
data_test_account34=merge(x = data_test_account3,y = data_test_account5, by = c("customer_no"))
data_test_account_final=merge(x = data_test_account12,y = data_test_account34, by = c("customer_no"))

data_test_enquiry$count_enquiry_recency_365 <- ifelse(as.Date(substr(strptime(data_test_enquiry$entry_time, format = "%d-%b-%y"),1,10)) -
                                                   as.Date(substr(strptime(data_test_enquiry$enquiry_dt, format = "%d-%b-%y"),1,10))>365,0,1)

data_test_enquiry$count_enquiry_recency_90 <- ifelse(as.Date(substr(strptime(data_test_enquiry$entry_time, format = "%d-%b-%y"),1,10)) -
                                                  as.Date(substr(strptime(data_test_enquiry$enquiry_dt, format = "%d-%b-%y"),1,10))>90,0,1)

data_test_enquiry$mean_diff_entry_time_enquiry_dt <-
  (as.numeric(substr(strptime(data_test_enquiry$entry_time, format = "%d-%b-%y"),1,4))*12 + as.numeric(substr(strptime(data_test_enquiry$entry_time, format = "%d-%b-%y"),6,7))) - (as.numeric(substr(strptime(data_test_enquiry$enquiry_dt, format = "%d-%b-%y"),1,4))*12 + as.numeric(substr(strptime(data_test_enquiry$enquiry_dt, format = "%d-%b-%y"),6,7)))

vect=c(1, 2, 3, 4, 7, 11, 13, 14, 15, 17, 31, 32, 33, 34, 42, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60)

data_test_enquiry$perc_unsecured_others <- ifelse(is.na(match(data_test_enquiry$enq_purpose, vect))==TRUE,0,1)

library("plyr", lib.loc="~/R/win-library/3.2")
df2<-count(data_test_enquiry, c("customer_no", "enq_purpose"))

df3<-aggregate(formula = cbind(freq)~customer_no, 
               data = df2,
               FUN = max,na.rm=TRUE,na.action="na.pass")

colnames(df3)[2]="max_freq_enquiry"

data_test_enquiry1<-aggregate(formula = cbind(count_enquiry_recency_365,count_enquiry_recency_90)~customer_no, 
                         data = data_test_enquiry,
                         FUN = sum,na.rm=TRUE,na.action="na.pass")

data_test_enquiry2<-aggregate(formula = cbind(mean_diff_entry_time_enquiry_dt,perc_unsecured_others)~customer_no, 
                         data = data_test_enquiry,
                         FUN = mean,na.rm=TRUE,na.action="na.pass")

data_test_enquiry12=merge(x = data_test_enquiry1,y = data_test_enquiry2, by = c("customer_no"))
data_test_enquiry_final=merge(x = data_test_enquiry12,y = df3, by = c("customer_no"))

data_test_account_enquiry=merge(x = data_test_enquiry_final,y = data_test_account_final, by = c("customer_no"))
data_test_final=merge(x = data_test_account_enquiry,y = test_data, by = c("customer_no"))
View(data_test_final)

write.csv(data_test_final, file = "data_test_final_output.csv")

#prediction

str(data_test_final)
names(data_test_final)
attributes(data_test_final)

#missing value check
apply(data_test_final, 2, function(x){sum(is.na(x))})

data_test_final1=data_test_final[,c(-22,-23,-24,-27,-32,-34,-35,-36,-37,-38,-41,-42,-43,-44,-46,-47,-50,-51,-52,-57,-58,-59,-60,-61,-62,-63,-64,-65,-67,-68,-71,-72,-73,-74,-75,-76,-77,-80,-84,-86,-87,-88,-89,-91,-93)]
View(data_test_final1)

#missing value check
apply(data_test_final1, 2, function(x){sum(is.na(x))})
data_test_final2=na.omit(data_test_final1)
View(data_test_final2)
data_test_final3=data_test_final2[,c(-13,-14,-15,-16,-19,-22,-23,-26,-27)]
View(data_test_final3)
data_test_final4=data_test_final3[,c(-1,-18)]
View(data_test_final4)
data_test_final5=data_test_final4[,c(-14,-33,-34)]
View(data_test_final5)
data_test_final6=data_test_final5[,c(-1,-6,-16,-20,-22,-27,-28,-29)]
View(data_test_final6)
data_test_final7=data_test_final6[,c(1,3,4,7,8,10,11,12,13,14,19,20,21,22,24,27)]
View(data_test_final7)

glm.mod5=glm(Bad_label~.,data = data_test_final7,family = binomial(logit))
summary(glm.mod5)

p2=exp(-1.452e+00+(-3.912e-02*count_enquiry_recency_90)+(3.796e-01*perc_unsecured_others)+(2.785e-03*max_freq_enquiry)+(-3.543e-02*payment_history_mean_length)+(1.516e-01*payment_history_avg_dpd_0_29_bucket)+(-5.398e-04*feature_3)+(3.774e-02*feature_4)+(-3.014e-06*feature_7)+(-1.749e-02*feature_14)+(-3.898e-02*feature_25)+(-4.897e-02*feature_41)+(-8.754e-02*feature_42)+(-3.589e-06*feature_52)+(3.562e-04*feature_65)+(-1.465e-02*feature_71))/(1+exp((-1.452e+00+(-3.912e-02*count_enquiry_recency_90)+(3.796e-01*perc_unsecured_others)+(2.785e-03*max_freq_enquiry)+(-3.543e-02*payment_history_mean_length)+(1.516e-01*payment_history_avg_dpd_0_29_bucket)+(-5.398e-04*feature_3)+(3.774e-02*feature_4)+(-3.014e-06*feature_7)+(-1.749e-02*feature_14)+(-3.898e-02*feature_25)+(-4.897e-02*feature_41)+(-8.754e-02*feature_42)+(-3.589e-06*feature_52)+(3.562e-04*feature_65)+(-1.465e-02*feature_71))))
p3=1000*p
score_test_data=data.frame(p3,data_final7$Bad_label)
View(score_test_data)

score_test_data$decile<-with(score_test_data, cut(p1, breaks=quantile(p3, probs=seq(0,1, by=0.1), na.rm=TRUE),include.lowest=TRUE))

score_test_data<-within(score_test_data, decile<- as.integer(cut(p3, quantile(p3, probs=0:10/10), include.lowest=TRUE)))

y_test<-cbind(decile=aggregate(score_test_data$bad_label, by=list(decile=score_test_data$decile), FUN=sum)[1],
         
         freq=aggregate(score_test_data$bad_label, by=list(decile=score_test_data$decile), FUN=sum)[2],
         
         total_freq=as.data.frame(table(score_test_data$decile))[2])

table(score_test_data$data_final7.Bad_label,score_test_data$decile)


library("gains", lib.loc="~/R/win-library/3.2")
g=gains(data_test_final7$Bad_label,preds,groups = 10)
plot.gains(g)