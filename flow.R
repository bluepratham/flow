###fetch data
vdetails = read.csv("details.csv", skip = 2 ,header = F, stringsAsFactors = F)
dname = read.csv("details.csv" , nrows = 1 , header = F, stringsAsFactors = F )[1,1]
vnames = vdetails[,1]


dataset = read.csv(file = dname)
dataset = dataset[vnames]
#dataset = dataset[sort(names(dataset))]
######

##converting numerics to factors, if uniques less than 20
for (i in seq_len(length(dataset))){
  if (length(unique(dataset[,i])) < 20 & is.numeric(dataset[,i])){
    dataset[,i] <- as.factor(dataset[,i])
  }
}


##performing treatment on factors
for (i in seq_len(length(dataset))){
  if(is.factor(dataset[,i]) & length(dataset[(dataset[,i]) == "",i ] ) >0){
#    print(names(dataset[i]))
    dataset[dataset[,i] == "",i ] = vdetails[ i, 3]
    dataset[,i] = droplevels(dataset[,i])
  }
}

##performing treatment on numerics
for (i in seq_len(length(dataset))){
  if(is.numeric(dataset[,i]) ){
    if(!(vdetails[i,3] == "")) dataset[dataset[,i] == 0,i] = as.numeric(vdetails[i,3])
##performing treatment on numerics
    if(!is.na(vdetails[i,4]))  dataset[dataset[,i]<vdetails[i,4]  ,i] = vdetails[i,4]
    if(!is.na(vdetails[i,5]))  dataset[dataset[,i]>vdetails[i,5]  ,i] = vdetails[i,5]
    # if (anyNA(dataset[,i]) | sum(dataset[,i] == 0)>0){
    # dataset[dataset[,i] == 0,i] = as.numeric(vdetails[i,3])
    # }
    # dataset[dataset[,i]<vdetails[i,4]  ,i] = vdetails[i,4]
    # dataset[dataset[,i]>vdetails[i,5]  ,i] = vdetails[i,5]
 }
}

##########
##creating model
####
# ind_mod_var = vdetails[which(vdetails[,6] == 1),1]
# dep_mod_car = vdetails[which(vdetails[,6] == 2),1]
# 
# form = paste0(dep_mod_car, "~" , paste0(ind_mod_var, collapse = "+"))
# 
# mod = glm(form, data = dataset , family = "binomial")

mod = readRDS("model.RDS")

##############################################


########
##predicting

pred = predict(mod , newdata = dataset[,vdetails[which(vdetails[,6] == 1),1]] ,type = "response")
pred1 = unname(pred[order(pred, decreasing = T )])
dataset$prob = pred
dataset$pred = (as.numeric(pred>.103))
# datasetorder = dataset[order(pred),]
# rownames(datasetorder) = NULL
write.csv(dataset, "prediction.csv", row.names = F)
#######
#######
#######
##subset the frauds

fraud = dataset[dataset$pred == 1 , ]
genuine = dataset[dataset$pred == 0,]
#######



######
###batchwise fraud rate graph
library("rjson")
x = toJSON(
  data.frame(
    y = round(mean(dataset$pred),2),
    y = round(mean(dataset$pred),2),
    y = round(mean(dataset$pred),2),
                      x = paste("Batch",ifelse(length(nchar(readLines("xyChart.json"))) == 0,
                                               1, 1+nchar(readLines("xyChart.json")) - 
                                                 nchar(gsub("\\{","" ,readLines("xyChart.json")))))))

js <- readLines("xyChart.json")

if(length(nchar(js)) == 0){
  cat(paste("[", x , "]", sep="") , file = "xyChart.json", sep = "\n")
} else {
  cat(gsub(pattern = "\\}]",
           replacement = paste("\\},", x,"]", sep = "") , x = js),file="xyChart.json", sep="\n")
}

xychart = readLines("xyChart.json")
########


##batchwise policy_channel, policy_brand n policy prem billing number and proportion of frauds


batchwise = function(filename , varName){
  
  x = data.frame(setNames(replicate(length(unique(dataset[,c(varName)])),numeric(0)), paste0(sort(unique(dataset[,c(varName)])) , "_Number")))
#  y = data.frame(setNames(replicate(length(unique(dataset[,c(varName)])),numeric(0)), paste0(sort(unique(dataset[,c(varName)])) , "_Propor")))
  #print(y)
  x[1,] = table(fraud[,c(varName)])
  print(x)
#  y[1,] = round(table(fraud[,c(varName)])/table(dataset[,c(varName)]),4)
  #print(table(varName))
  
  js <- readLines(filename)
  
  x = toJSON(
    data.frame(
      x,
      x = paste("Batch",ifelse(length(nchar(js)) == 0,
                               1, 1+nchar(js) - 
                                 nchar(gsub("\\{","" ,readLines(filename)))))))
  
  if(length(nchar(js)) == 0){
    cat(paste("[", x , "]", sep="") , file = filename, sep = "\n")
  } else {
    cat(gsub(pattern = "\\}]",
             replacement = paste("\\},", x,"]", sep = "") , x = js),file=filename, sep="\n")
  }
  
}

batchwise(filename = "prem_bill.json",varName =  "policypremiumbilling")
batchwise(filename = "policy_brand.json",varName =  "policybrand")
batchwise(filename = "policy_channel.json",varName =  "policychannel")




###########
###gains chart

gain = c(0,cumsum(
  tapply(
    as.numeric(dataset$fraud_class[order(dataset$prob, decreasing = T)])-1,
    cut(1:nrow(dataset),breaks = 10),
    sum)*100/sum(as.numeric(dataset$fraud_class)-1))
  )
rand = seq(0,100,10)
xgain = seq(0,100,10)


gains = rjson::toJSON(unname(split(data.frame(gain,xgain,rand), f = 0:10)))

###########
###########


#plot(x = xgain, y = rand, type = "b")


###lift

lift = toJSON(unname(split(data.frame(liftMod  = gain/rand,liftRand = rand/rand , perc = seq(0,100,10)  )[-1,], f = 2:length(gain))))



####sensitivity n specificity
sens = c()
spec = c()
for ( i in (0:100)/100){
  sens = append(sens , round(sum(as.numeric(dataset$prob>i) == 1 & dataset$fraud_class == 1)/
                  (sum(as.numeric(dataset$prob>i) == 1 & dataset$fraud_class == 1) +
                     sum(as.numeric(dataset$prob>i) == 0 & dataset$fraud_class == 1)),3))
  spec = append(spec , round(sum(as.numeric(dataset$prob>i) == 0 & dataset$fraud_class == 0)/
                  (sum(as.numeric(dataset$prob>i) == 0 & dataset$fraud_class == 0) +
                     sum(as.numeric(dataset$prob>i) == 1 & dataset$fraud_class == 0)),3))
 

}

sensespec = toJSON(unname(split(data.frame(sens,spec,prob = 0:100/100), f = 1:length(sens))))
#cat(toJSON(unname(split(data.frame(spec,prob = 0:100/100), f = 1:length(spec)))), file= "spec.json", sep="\n")

 # plot(x = 0:100,y = sens, type = "l")
 # par(fig=c(0,1,0,1), new=TRUE)
 # plot(x = 0:100,y = spec, type = "l")

#################


##beta coefficients
##in normal json form, not jsonlite type


beta = toJSON(
 unname( split(data.frame(
    variable = names(mod$coefficients), 
                      coefficient = unname(mod$coefficients))[!grepl("Inter",names(mod$coefficients) )&!is.na(mod$coefficients),]  , f = seq_along(mod$coefficients)
) ))


#############################

##############
##for type of incident doughnut

table(dataset$claimlosscause)

#THIS IS for normal json
cat(toJSON(aggregate(cbind(no.OfClaims = rep(1,NROW(dataset$claimlosscause[dataset$pred == 1]))) ,
          by = list(Incident.Type = dataset$claimlosscause[dataset$pred == 1]) , sum)) , file = "incidentType.json" , sep="\n")

#this is for jsonlite type json
incdnt = 
  toJSON(
    unname(split(
      aggregate(
        cbind(no.OfClaims = rep(1,NROW(dataset$claimlosscause[dataset$pred == 1]))) ,
                     by = list(Incident.Type = dataset$claimlosscause[dataset$pred == 1]) , sum),
                  f = 1:length(unique(as.character(dataset$claimlosscause[dataset$pred == 1]))))))
   

#can also be done using table function (use as.chracter(dataset$claimlosscause))

######################################################

####
###driverage grouped  json (bar chart)
dataset$age_grp = cut(dataset$ivehicledriverage, c(0,18,38,60,100), labels = c("under 18", "18-38", "38-60","60-100"))

age_grp = toJSON(unname(
  split(
    data.frame(
      table(grp = 
        dataset$age_grp[(dataset$pred == 1)])/sum(dataset$pred == 1)), f = 1:4)))

#################


#### premiumpolicy (yearly quarterly etc ) json


# cat(
#   toJSON(
#     unname(
#       split(
#         data.frame(
#           table(PolicyRenewalType = dataset$policypremiumbilling[dataset$pred == 1]))
#  , f = 1:length(unique(dataset$policypremiumbilling)) ))), file = "premiumPolicy.json" , sep="\n" )

##################


####
###for top fraudulent claims

topfraud = toJSON(
  unname(
    split(
      fraud[order(fraud$prob,decreasing = T),
            c("claimid", "claimlossstate",
              "claimlosscause","risksuminsured",
              "ivehicledriverage","claim_lag_lossinception")],
      f = seq_len(nrow(fraud)))))


########################

######
###Sunburst 

# a = data.frame(table( dataset$policychannel[dataset$pred == 1]))
# names(a) = c("name", "size")
# b = data.frame(table( dataset$policypremiumbilling[dataset$pred == 1]))
# names(b) = c("name", "size")
# c = data.frame(table( dataset$policybrand[dataset$pred == 1]))
# names(c) = c("name", "size")
# 
# snbrst = toJSON(list(name = "All" , children =
#                        list(list(name = "policy Channel",children = unname(split(a,f = 1:4))),
#                             list(name = "policy Prem",children =unname(split(b,f = 1:4))),
#                             list(name = "policy Brand",children =unname(split(c,f = 1:12)))
#                             
#                        ))
# )

#####throttle of fraud and non frauds percent

frd_prcnt = toJSON(c(fraud = sum(dataset$pred == 1)/nrow(dataset), genuine = sum(!dataset$pred == 1)/nrow(dataset)))


#write.csv(c(frd_prcnt,topfraudjson,snbrst,premPolicyjson,incdnt,age_grp,beta,sensspec,gains,xychart,lift), "jsons.csv")


concordance<-function(model){
  # Get all actual observations and their fitted values into a frame
  fitted<-data.frame(cbind(model$y,model$fitted.values))
  colnames(fitted)<-c('respvar','score')
  # Subset only ones
  ones<-fitted[fitted[,1]==1,]
  # Subset only zeros
  zeros<-fitted[fitted[,1]==0,]
  
  # Initialise all the values
  pairs_tested<-0
  conc<-0
  disc<-0
  ties<-0
  
  # Get the values in a for-loop
  for(i in 1:nrow(ones))
  {
    for(j in 1:nrow(zeros))
    {
      pairs_tested<-pairs_tested+1
      if(ones[i,2]>zeros[j,2]) {conc<-conc+1}
      else if(ones[i,2]==zeros[j,2]){ties<-ties+1}
      else {disc<-disc+1}
    }
  }
  # Calculate concordance, discordance and ties
  concordance<-conc/pairs_tested
  discordance<-disc/pairs_tested
  #ties_perc<-ties/pairs_tested
  return(data.frame("Concordance"=concordance,
                    "Discordance"=discordance,
                    #"Tied"=ties_perc,
                    "Pairs"=pairs_tested,
                    "AIC" = mod$aic))
}


rjson::toJSON(concordance(mod))
#updating git
