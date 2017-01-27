library(tidyverse)
library(broom)

datas = read.csv("final_investigated.csv")
dim(data)

datas = (datas[sort(names(datas))])
datastbl = as_tibble(datas)
names(data %>% select(starts_with("trt")))

?select

table(datas$claimcatastrophecode)
table(datas$claimcatastrophedesc)

table(datas$claimlodgemethod)
table(datas$DefenceBarrister)
table(datas$Cyclist)
table(datas$Broker)
table(datas$Assessor)

library(jsonlite)
install.packages("jsonlite")



sapply(datas, function(x)length(unique(x))) == 1 


tidy(summary(datas))

names(datas[,!(sapply(datas, function(x)length(unique(x))) ==1)  
               ])

sapply(datas %>% select(starts_with("flag")), table)

table(datas$POLICYNBRCLAIMS )

datastbl = datas[,(sapply(datas, function(x)length(unique(x))) >1)]

View(data.frame(datas$POLICYNBRCLAIMS , datas$claimdecision))

hist(datas$claim_lag_lossalteration )
hist(datas$claim_lag_lossinception )
hist(datas$claim_lag_losslodge[datas$claim_lag_losslodge >1000] , breaks =1000 )

table(datas$policytype)



names(datas)


table(grepl("fire",datas$claimincidentdescription), datas$claimdecision)
1668/221
45/3

View(datas[,!(sapply(datas, function(x)length(unique(x))) >50)])

library(caret)

sum(sapply(datastbl , anyNA))
sum(sapply(datastbl , function()is.na(x)))

datastbl = datastbl[!sapply(datastbl , anyNA) ]

View(datastbl %>% select(starts_with("trt"  ) ))

cor(datas$Other,datastbl$OtherInsurer)

table(cut(datas$claim_lag_losslodge,breaks = 10) , datas$claimdecision)

table(datas$ActivityOwner)


64/2000





name = data.frame(a = rep(NA,181))
name[1] = names(datas)
hist(datas$ivehicleyear, breaks = 100)
table(datas$alternate_fraud_class , datas$fraud_class )


datastbl = datastbl[-4]

summary(glm(data = datas , claimdecision~a,family = "binomial"))


a = paste0(names(datastbl[
  sapply(datastbl ,function(x) length(unique(x) ))>2] ) , collapse = "+")


table(datas$ActivityOwner )
table(datas$fraud_class, datas$claimdecision)
length(unique(datas$claimid))

?as.formula



a = paste("fraud_class~" , paste(names(datastbl[   sapply(
  datastbl ,function(x) length(unique(x) ))>2] ),collapse = "+"))

summary(glm(a,data=datastbl , family = "binomial"))


a
for (i in 1:length(datastbl)){
  print(length(unique(datastbl[,i])))
  if (length(unique(datastbl[,i])) == 2){
    print(names(datastbl[i]))
    datastbl[,i] <- as.factor(datastbl[,i])
  }
}

str(datastbl)

length(which(is.na(datas$claimpolicenotified)))


sapply(datastbl, anyNA)

datastbl$riskstate[1:10]
datastbl$claimlossstate[1:10]


table(datas$claimexcesswaived)  ###not to be used in model

table(datastbl$policyproducttype)

View(data.frame(datas$tpvehicledriverage))

range(datas$claimexcessapplicable)

anyNA(datastbl$policyproducttype)

table(datas$Mortgagee)


summary(glm(data = datastbl , a))


datastbl$fraud_class = as.integer(datastbl$fraud_class)

range(datas$ivehicledriverage)
table(datas$fraud_class)

sapply(datastbl[1:10] , )

unique(datastbl$POLICYEXPIRYDATE )

table(cut(datastbl$claim_lag_lossinception/365, breaks   = 400), datastbl$fraud_class)
table(datastbl$fraud_class)
range(datastbl$claim_lag_lossinception)

ggplot(datastbl) + 
  geom_bar(aes(x =cut(datastbl$claim_lag_lossalteration/365 , breaks   = 400),
               fill = as.factor(fraud_class) ))


ggplot(datastbl) + 
  geom_bar(aes(x =cut(datastbl$claim_lag_lossinception/365 , breaks   = 400),
               fill = as.factor(fraud_class) ))



ggplot(datastbl[datastbl$ivehicleyear>0,]) + 
  geom_bar(aes(x = cut(ivehicleyear,breaks = 10) ,
               fill = as.factor(fraud_class ) ))


ggplot(datastbl) + 
  geom_bar(aes(x =cut(datastbl$claim_lag_losslodge/(60*24) , breaks   = 1000),
               fill = as.factor(fraud_class) )) +
  coord_flip()


table(cut(datastbl$claim_lag_losslodge/(60*24) , breaks   = 1000), datastbl$fraud_class)

range(datastbl$claim_lag_lossinception)

hist(datastbl$claim_lag_losslodge, breaks  = 100)/(60*24)


table(cut(datastbl$claim_lag_losslodge/(60*24) , breaks   = 1000), datastbl$fraud_class)

table(datastbl$ivehicleyear , datastbl$fraud_class)

length(unique(datastbl$ivehicleyear))

plot(datas$ivehicleyear[datas$ivehicleyear >0],
     as.Date(datas$POLICYINCEPTIONDATE[datas$ivehicleyear >0]) ,
     col = as.factor(datas$fraud_class[datas$ivehicleyear >0]))

datas$POLICYINCEPTIONDATE[1:10]

ggplot(datastbl) + 
  geom_bar(aes(cut(tpvehicledriverage,breaks = 4), fill = as.factor(fraud_class)))

range(datas$tpvehicledriverage)

datas$claimlodgemethod[1:5]

table(datastbl$claimlodgemethod, datastbl$fraud_class)


table(cut(datastbl$claim_lag_lossinception/365, breaks   = 400),
      datastbl$fraud_class)


1247/68

68+5+6+2+3
1247+213+134+98+54+41+21

1808/84
84/2001
68/(1247+68)
43/(668+43)


table(cut(datas$ivehicledriverage, breaks = 10) , datastbl$fraud_class)

31/(586+31)
28/444
(84-(31+28))/(2001-(586+444))



table(datas$claimlosscause)

ggplot(datastbl) + 
  geom_bar(aes(claimlosscause, fill = as.factor(fraud_class))) +
  coord_flip()


quantile(datas$claim_lag_lossinception , c(.06))


ggplot(datastbl) +
  geom_bar(aes(grepl("park",cls, ignore.case = T),
               fill = as.factor(fraud_class)))

table(grepl("arson",(datas$cls), ignore.case = T),datas$fraud_class)

83/(1868+83)

ggplot(datastbl) +
  geom_bar(aes(riskstate ,
               fill = as.factor(fraud_class)))
table(datas$riskstate,datas$claimlossstate)

table(datas$fraud_class[!(as.character(datas$riskstate) == as.character(datas$claimlossstate))])

(2/69 - 82/1850)/(2/69)

length(unique(datas$claimlosssuburb))
29/670
datas$ris


table(datas$flag_towingagency, datas$fraud_class)

ggplot(datas) + geom_bar(aes(as.factor(flag_salvageservice),
                             fill = as.factor(fraud_class)))

71/1300
13/700

ggplot(datas) + geom_bar(aes(as.factor(flag_motorrepairshop),
                             fill = as.factor(fraud_class)))

66/1400
18/600

table(datas$ivehicledamage, datas$fraud_class)

82/1900
2/92

table(cut(datas$claimexcesstotal, breaks=10), datas$fraud_class)

ggplot(datas) + geom_bar(aes(flag_claimexcesswaived ,
                             fill = as.factor(fraud_class))) +
  coord_flip()

table(datas$flag_claimexcesswaived , datas$fraud_class)

table(datastbl$AdditionalInsured)
table(datastbl$AlternateContact , datastbl$fraud_class)

datastbl$claim_lag_losslodge



library(tidyr)

datas$new[1:10]


table(datas$Repairer)

mu = read.delim("clipboard")

View(mu[ mu$claim_lag_losslodge > quantile(mu$claim_lag_losslodge , .99) ,c("ivehicleyear","claim_lag_losslodge","POLICYINCEPTIONDATE","POLICYEXPIRYDATE","fraud_class","new__claimdecision") ])

sapply(mu[1:10], table)

table(mu$Agent)

summary(mu$claim_lag_lossalteration)

plot(density(mu$AdditionalInsured))


hist(mu$ivehicledriverage, breaks = 100)
abline(v = mean(mu$ivehicledriverage), col = "red")
length(mu$ivehicledriverage[mu$ivehicledriverage == 0])
range(mu$ivehicledriverage)

mu$ivehicledriverage[order(mu$ivehicledriverage)]

mu = read.delim("clipboard")
hist(mu$ivehicleyear)

names(mu[sapply(mu , function(x)length(unique(x)))>2])


table(mu$ivehicleyear)

mu$POLICYEXPIRYDATE = as.Date(mu$POLICYEXPIRYDATE, format = "%m/%d/%Y")
mu$POLICYINCEPTIONDATE = as.Date(mu$POLICYINCEPTIONDATE, format = "%m/%d/%Y")

range(mu$POLICYINCEPTIONDATE)

View(data.frame(mu$Driver , mu$Claimant , mu$Owner))

mu$tpvehicleyear = NULL

hist(mu$claim_lag_losslodge , breaks = 1000)
head(mu$claim_lag_losslodge[order(mu$claim_lag_losslodge)] , 200)


mu = read.delim("clipboard")
mu$tp_parties = NULL

View(data.frame())
sort(mu$claim_lag_losslodge[mu$fraud_class == 1])

hist(mu$claimexcessapplicable, breaks = 1000)
table(mu$policyalterations , mu$fraud_class)

View(data.frame(mu$claimexcesstotal , mu$claimexcessapplicable))

table(mu$claimlossstate)

mu$claimlossstate[ mu$claimlossstate == ""] = "NSW"


sum((mu$claimlosssuburb == ""))

length(unique(mu$claimlosssuburb))

hist(mu$claimreserve)

sum(is.na(mu$cls ))

table(mu)


mu$riskmotoragreedvalue = NULL
mu$riskmotormarketvalue = NULL
mus = mu[ , sort(names(mu))]


length(unique(mu$claimlosssuburb))
a = read.delim("clipboard" , header = F)



b = mu[a[,1]]

q = glm(fraud_class~ * , data = dataset  , family = "binomial" )

predict(q, type="response")>.5
predict(q, type="response", newdata = b[c("AlternateContact")])


str(b$fraud_class)
b$fraud_class = as.factor(b$fraud_class)
table(b$fraud_class)

a1 = sort(a[,1])[2:68]
a1 = as.character(a1)

b = mu[c((a1))]
a1 = a1[-c(44,61)]
length(b)

table(mu$TPSettlementParty, mu$tp_parties)
b$claimlosssuburb[1:10]

grep("suburb", a[-c(61,73),])
a[c(10,12,64,47),1]

install.packages("glmnet")
rm(q)


g = glmnet::glmnet(y =as.factor( b$fraud_class) ,x =  as.factor(b[,1]) , family = "binomial")




table(mu$policystatus)
table(mu$tp_parties , mu$TPSettlementParty)
table(datas$policyproducttype)
table(datastb )

rm(q,len,i)


table(datastbl$claimlosscause)
table(datastbl$flag_aggclaimlosscause_theft )




library(tidyverse)

ggplot(datas) + geom_histogram(aes(x = datas$ivehicledriverage 
                                   , fill = as.factor(fraud_class) ), binwidth = 5)



range(as.Date(datas$POLICYEXPIRYDATE, "%m/%d/%Y"))
datas$policy

table(datastbl$riskstate)
datastbl$fraud_class = as.factor(datastbl$fraud_class)
str(datastbl$fraud_class)
summary(lm(data=datastbl, riskmotorpurchaseprice~riskstate + fraud_class - 1))

datas$inve
library(tidyverse)


class(datas)
var  = datas %>% select(-contains("new") ) %>% filter(contains("trt"))
View(data.frame(names(var)))


table(datas$policypremiumbilling, datas$fraud_class)



table(datas$tpvehicledamage)
table(datas$flag_trt_tpvehicledamage)
table(datas$Interestedparty)
table(datas$ListedDriver)

install.packages("class")

library(twitteR)
library(stringr)



str(datastbl)


class(fraud)
library(rjson)

writeLines("\"")


library(tidyverse)

class(datastbl[,1])

x <- toJSON(unname(split(datastbl[1:5, c("claimid","fraud_class")], 1:5)))
cat(x)



View(data.frame(sort(predict(mod , newdata = dataset[,vdetails[which(vdetails[,6] == 1),1]] ,type = "response"))))

plot(x = seq(0,100,10), y = seq(0,100,10), type = "b")


gain = c(0,cumsum(tapply(as.numeric( dataset$fraud_class)-1, cut(1:nrow(dataset),breaks = 10),  sum)*100/84))

plot(x = seq(0,100,10),gain, type = "b" )
abline(a = 0, b=1)

cut(as.numeric(dataset$fraud_class),breaks = 10)

cat(jsonlite::toJSON(
  list(
     polacq = list(polAcqChann = tapply(dataset$pred, dataset$policychannel , sum)),
     polBrand = list(policyBrand = tapply(dataset$pred, dataset$policybrand , sum)),
      policyPrem = list(policyPrem = tapply(dataset$pred, dataset$policypremiumbilling , sum))
     )
), file = "sun.json" , sep = "\n" )

cat(jsonlite::toJSON( jsonlite::toJSON(dataset[1:2,1:2])))
cat(toJSON(dataset[1:2,1:2]))


a = toJSON(list("name " = "year" ,"children" =  
                  unname(split(
                    data.frame("name" = "q1","children"= 
                          unname(split
                                 (data.frame
                                 ("name" = letters[1:3], size = 1:3), f = 1:3))), f = 1))))

cat(a)
fromJSON(a)

tbl = tibble(a = 1 , b = list(a = 1:2, b = 1:2))

toJSON(
  list(
    list(
      list(a = "asdad" ,"children"= unname(split(data.frame(
                                                            list(a = 1:2, b = 1:2)),1:2))),
      list(a = "asdad" ,"children" =  unname(split(data.frame(
                                                            list(a = 1:2, b = 1:2)),1:2)))
    )
))


cat(toJSON(list(
  list(list(name = "policychannel",children = unname(split(data.frame(table(dataset$policychannel)),f = 1:4))),
       list(name = "policyprem",children =unname(split(data.frame(table(dataset$policypremiumbilling)),f = 1:4)))
       ))
))

a = data.frame(table( dataset$policychannel[dataset$pred == 1]))
names(a) = c("name", "size")
b = data.frame(table( dataset$policypremiumbilling[dataset$pred == 1]))
names(b) = c("name", "size")
c = data.frame(table( dataset$policybrand[dataset$pred == 1]))
names(c) = c("name", "size")

cat(toJSON(list(name = "All" , children =
  list(list(name = "policy Channel",children = unname(split(a,f = 1:4))),
       list(name = "policy Prem",children =unname(split(b,f = 1:4))),
       list(name = "policy Brand",children =unname(split(c,f = 1:12)))
       
  ))
))

ggplot(dataset) + 
  geom_point(aes
             (x = 1:2001, y = sort(dataset$prob) , col = dataset$fraud_class), alpha = .1)




plot(x = dataset$ivehicledriverage, y = mod$fitted.values, col = dataset$fraud_class)



mod$fitted.values






