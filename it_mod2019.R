##
#author: Steve Keller
#This program reads data from FPDS and
#the IT dashboard to identify which 
#major, IT projects are involving 
#GSA contract vehicles. 
## 

#pre-work 
#download one data set from CASE/SAP FPDS Universe
#Custom SQL - REF PIID and Contract Vehicle 
#save file as fpds.csv

#libraries and working directory 
setwd("~/GitHub/IT Modernization/data")
library(dplyr)

########## READ DATA ################
#Read FPDS Files 
fpds<-read.csv("fpds.csv",header = TRUE, 
               stringsAsFactors = FALSE,strip.white=TRUE)
#Fix FPDS Names
names(fpds)<-c("vehicle","piid")

#Read Business Case File 
business<-read.csv("bus_case.csv",header = TRUE, 
                   stringsAsFactors = FALSE)
#https://itdashboard.gov/api/v1/ITDB2/dataFeeds/businessCase?csv=true
#business<-
#  read.csv("https://myit-2019.itdashboard.gov/api/v1/ITDB2/dataFeeds/businessCase?csv=true",
#           stringsAsFactors = FALSE,strip.white=TRUE)

#Read Contracts File 
#Current API isn't working because they're waiting for new data
#temporary measure: read from file. 
contracts<-read.csv("contracts.csv",header = TRUE, 
                    stringsAsFactors = FALSE)
#https://myit-2019.itdashboard.gov/api/v1/ITDB2/dataFeeds/contractsReport?csv=true
#contracts<-
#  read.csv("https://myit-2019.itdashboard.gov/api/v1/ITDB2/dataFeeds/contractsReport?csv=true",
#           stringsAsFactors = FALSE,strip.white=TRUE)

#Read Portfolio File 
portfolio<-read.csv("portfolio.csv",header = TRUE, 
                    stringsAsFactors = FALSE)
#https://itdashboard.gov/api/v1/ITDB2/dataFeeds/portfolio?csv=true
#portfolio<-
#  read.csv("https://myit-2019.itdashboard.gov/api/v1/ITDB2/dataFeeds/portfolio?csv=true",
#           stringsAsFactors = FALSE,strip.white=TRUE)

#create a test for contracts 
#contracts<-head(contracts,50)

######## SEARCH FUNCTION #######
#Bisection Search
#Linear Search 
######## BISECTION SEARCH ######
#bisection search
#takes a code to match and a data set
#and finds the code using bisection
#search
#input:character code, dataframe data
#output:character vehicle name 
bisection_search<-function(code,data){
  #determine length of the array store as n
  n<-nrow(data)
  #set L and R 
  L<-0
  R<-n-1 
  
  #main while loop 
  while(L<=R){
    
    M<-floor((L+R)/2)
    if(data[M,2]<code){
      L<-M+1
    }else if(data[M,2]>code){
      R<-M-1
    }else{
      if(!is.na(data[M,2]==code)){
        if(data[M,2]==code){
          #print(fpds[find,1])
          #return(fpds[find,1])
          return(data[M,1])
          #break  
        }#end of conditional for term
      }#end filter for NA
      
    }
  }#end of main loop
  
  #return answer 
  return("Unsuccessful")
}#end of bisection search 

#test search 
#bisection_search("zzz",testArray)

#Search FPDS
#function returns the vehicle of the PIID
#input: character PIID and a dataframe object
#output:character vehicle name
fpds_search<-function(term,file){
  #set variables 
  searchTerm<-term
  fpds<-file
  
  #loop through fpds for number
  for(find in 1:nrow(fpds)){
    #print(fpds[find,2])
    
    #filter NA 
    if(!is.na(fpds[find,2]==searchTerm)){
      if(fpds[find,2]==searchTerm){
        #print(fpds[find,1])
        return(fpds[find,1])
        #break  
      }#end of conditional for term
    }#end filter for NA 
    
  }#end of for loop for search
  
}#end of fpds_search function 

######### CLEAN DATA ################
#use grep to remove dashes for 
#standardization of contracts.piid fpds.piid
contracts$PIID.Ref.PIID<-gsub("-", "", contracts$PIID.Ref.PIID)
fpds$Referenced.PIID<-gsub("-", "", fpds$piid)

#sort fpds 
fpds<-fpds[order(fpds$piid),]

#add a column to the dataframe contracts for each vehicle
#first examine all the available vehicles 
#fpds%>%
#  group_by(vehicle)%>%
#  summarize(n=n())

#create column for each vehicle 
contracts$ccv<-0
contracts$gsa_idiq<-0
contracts$gwac<-0
contracts$mac<-0
contracts$networks<-0
contracts$pbs_idiq<-0
contracts$primary<-0
contracts$schedule<-0
contracts$undefined<-0
contracts$va_schedule<-0
contracts$generic_gsa<-0

#The files were read in to remove trailing spaces
#this line removes the blank fpds contract numbers 
#fpds<-fpds%>%filter(Referenced.PIID!="")

##
#this function takes a character and removes trailing 
#or leading spaces
##
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

#testing trim function 
#testChar<-" Steve   "
#print(testChar)
#trim(testChar)

######### DETECT ITC ###############
#Return Column Number 
#Takes valid contract vehicle name 
#and returns a column number 
#input: character vehicle name
#return: numeric column number 
vehicle_to_column<-function(v_name){
  if(v_name=="(CCV)"){
    return(11) 
  }else if(v_name=="(GSA-IDIQ)"){
    return(12) 
  }else if(v_name=="(GWAC)"){
    return(13) 
  }else if(v_name=="(MAC)"){
    return(14) 
  }else if(v_name=="(NETWORKS)"){
    return(15) 
  }else if(v_name=="(PBS-IDIQ)"){
    return(16) 
  }else if(v_name=="(PRIMARY)"){
    return(17) 
  }else if(v_name=="(SCHEDULE)"){
    return(18) 
  }else if(v_name=="(UNDEFINED)"){
    return(19) 
  }else if(v_name=="(VA-SCHED)"){
    return(20)
  }else if(v_name=="GS_Contract"){
    return(21)}
  else{
    #return undefined 
    return(19)
  }
  
}#end vehicle to column 

#Generic GS Finder
#this function accepts a character
#looks at first two for 'GS' and returns
#True
#input: character string
#output:boolean TRUE or FALSE
gs_finder<-function(contract_piid){
  first_two<-substr(contract_piid,1,2)
  if(first_two=='GS'){
    return(TRUE)
  }else{
    return(FALSE)
  }
}#end gs_finder function

#This section of code will run through
#the PIID's in the contract data and 
#compare to FPDS to find ITC PIIDs 
#for each row in contracts
for(count in 1:nrow(contracts)){
  #save first set of PIID in contracts
  testRow<-contracts$PIID.Ref.PIID[count]
  #split the character into a list
  testList<-as.list(strsplit(testRow,"[|]"))
  #for each item in the list 
  for(k in testList){
    for(j in k){
      #print the PIID 
      print(j)
      #trim the contract number 
      j<-trim(j)
      #search for the PIID in fpds and store vehicle
      vehicle_name<-bisection_search(j,fpds)
      #save vehicle to the right spot in the table
      if(vehicle_name!="Unsuccessful"){
        #get column number 
        v_column<-vehicle_to_column(vehicle_name)
        contracts[count,v_column]<-1
        print(v_column)
      }#end null checker for writing out answer 
      else{
        #this is a null vehicle name 
        #before end of function check for generic GSA contracts
        if(gs_finder(j)){
          #get column number 
          vehicle_name<-'GS_Contract'
          v_column<-vehicle_to_column(vehicle_name)
          contracts[count,v_column]<-1
          
        }#end check for generic finder 
      }
    } #end loop through PIID array 
  } #end loop through PIID array
}# End main vehicle tagging loop

#test output 
#write.csv(contracts,"testContractsFile.csv")

#match contract file with IT dashboard spending file
#potentially the business case file
portfolio_contracts<-portfolio%>%
  left_join(contracts,by = 
              c("Unique.Investment.Identifier" = "ï..Unique.Investment.Identifier"))

#remove unavailable  records from file 
#check the file for records incomplete and structure
#head(portfolio_contracts)
#summary(portfolio_contracts)

#remove NA's from the file 
#portfolio_contracts<-portfolio_contracts%>%
#  filter(!is.na(ccv))

#checker<-portfolio_contracts%>%select(PIID.Ref.PIID,ccv,gsa_idiq,gwac,mac,networks,
#                             pbs_idiq,primary,schedule,undefined,
#                             va_schedule,generic_gsa)

######## IT MODERNIZATION TAGGING ###
#this section of code builds business rules and machine learning
#for identifying IT modernization within the IT dashboard data
#read in data for model building (hand tagged 300+ rows)
itmod<-read.csv("Machine_Master_List.csv",
                header = TRUE, stringsAsFactors = FALSE)

#correct the IT modernization column in itmod
itmod$IT_Modernization<-
  ifelse(itmod$IT_Modernization=="TRUE"|itmod$IT_Modernization=="1",1,0)

#add features related to the language in description
#specifically we're interested in "to-be" or unique language 
#suggesting transformative projects 
#data system provides initiative represents participation management investment 
#presidential
#also perhaps a search for "ing" 

#create_text_features
#this function takes a data frame 
#and a column name(string) and returns
#a dataframe with new columns 
#input:dataframe and column name
#output:dataframe with new columns 
create_text_features<-function(df,columnName){
  #create major IT 
  df$majorIT<-ifelse(df$Type.of.Investment=="01 - Major IT",1,0)
  
  #create text based feature columns 
  df$transforming<-as.numeric(
    grepl('*ing',tolower(df[,columnName])))
  df$data<-as.numeric(
    grepl('data',tolower(df[,columnName])))
  df$system<-as.numeric(
    grepl('system',tolower(df[,columnName])))
  df$provides<-as.numeric(
    grepl('provide',tolower(df[,columnName])))
  df$initiative<-as.numeric(
    grepl('initiative',tolower(df[,columnName])))
  df$represents<-as.numeric(
    grepl('represent',tolower(df[,columnName])))
  df$participation<-as.numeric(
    grepl('participation',tolower(df[,columnName])))
  df$management<-as.numeric(
    grepl('management',tolower(df[,columnName])))
  df$investment<-as.numeric(
    grepl('investment',tolower(df[,columnName])))
  df$presidential<-as.numeric(
    grepl('presidential',tolower(df[,columnName])))
  df$will<-as.numeric(
    grepl('will',tolower(df[,columnName])))
  
  #return dataframe
  return(df)
}#end of create_text_features

#run featuring creation on it modeling dataset 
itmod<-create_text_features(itmod,"Investment.Description")
portfolio_contracts<-create_text_features(portfolio_contracts,
                                          "Investment.Description")

############### MODEL RECESSION PROBABILITY ########
#This section trains a model for categorizing IT modernization 
####################################################
library(caret)

#turn it_modernization into a factor 
itmod$IT_Modernization<-as.factor(itmod$IT_Modernization)
train<-itmod

#create partition for testing 
index <- createDataPartition(train$IT_Modernization, p=0.75, list=FALSE)
trainSet <- train[ index,]
testSet <- train[-index,]

#set outcome and predictors 
outcomeName<-'IT_Modernization'
predictors<-c("transforming" , 
              "data", 
              "system", 
              "provides", 
              "initiative",
              "represents",
              "participation",
              "management",
              "investment",
              "presidential",
              "will",
              "Total.IT.Spending.FY2018..PY.....M.",
              "Total.IT.Spending.FY2019..CY.....M.",
              "Total.IT.Spending.FY2020..BY.....M.",
              "DME.PY.Agency.Funding....M.",
              "DME.CY.Agency.Funding....M.",
              "DME.CY.Contributions....M.",
              "O.M.PY.Agency.Funding....M.",
              "O.M.CY.Agency.Funding....M.",
              "O.M.BY.Agency.Funding....M.")

#"Cloud.Computing.Alternatives.Evaluation",
#"Total.Cloud.PaaS.IaaS.Cost.CY",
#"Total.Cloud.SaaS.Cost.CY",
#"Total.Other.Managed.Services.Cost.CY",
#"Part.Of.IT.Portfolio",
#"Mission.Support.Investment.Category",
#"IT.Infrastructure.and.Management.Type",
#"Type.of.Investment",
#"Mission.Delivery.and.Management.Support.Area"


############## MODEL STOCK AND HOUSING IMPACT #####
fitControl <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 5)

#build random forest
model_rf<-train(trainSet[,predictors],trainSet[,outcomeName],
                method='rf',trControl=fitControl,tuneLength=10)
#build tree
model_tree<-train(trainSet[,predictors],trainSet[,outcomeName],
                  method='rpart',trControl=fitControl,tuneLength=10)

#Variable Importance
varImp(object=model_rf)
varImp(object=model_tree)

#Plotting Varianle importance
plot(varImp(object=model_tree),main="Decision Tree - Variable Importance")
plot(varImp(object=model_rf),main="Random Forest - Variable Importance")

#plot the decision tree
library(rattle)
fancyRpartPlot(model_tree$finalModel)

#plot the model objects 
plot(model_rf)
plot(model_tree)

#Predictions
pred_rf<-predict.train(object=model_rf,testSet[,predictors],type="raw")
#table(pred_rf)
confusionMatrix(pred_rf,testSet[,outcomeName])
#rf_test<-cbind(train,pred_rf)

pred_tree<-predict.train(object=model_tree,testSet[,predictors],type="raw")
#table(pred_tree)
confusionMatrix(pred_tree,testSet[,outcomeName])
#FullTest<-cbind(testSet,pred_xgb)

#run predictions on total test data set 
pred_all<-predict.train(object=model_rf,portfolio_contracts[,predictors],type="raw")
portfolio_contracts<-cbind(portfolio_contracts,pred_all)
#table(pred_all)
#confusionMatrix(pred_all,portfolio_contracts[,outcomeName])

######## OUTPUT CSV #################
#make an ITC variable 
rowSelections<-c("gwac","networks","schedule","generic_gsa")
portfolio_contracts$ITC<-ifelse(rowSums(portfolio_contracts[,rowSelections]>=1),1,0)

#read out file as a csv
write.csv(portfolio_contracts,file="ITC_portfolio2019.csv")

#open and explore file in tableau
portfolio_contracts<-read.csv("ITC_portfolio2019.csv",
                              stringsAsFactors = FALSE, 
                              header = TRUE)

####### Print ITC Modernization % ###
#check portfolio names 
names(portfolio_contracts)

#summarize portfolio data
portfolio_summary<-
  portfolio_contracts%>%
  group_by(ITC,Type.of.Investment,pred_all)%>%
  summarize(IT18=sum(Total.IT.Spending.FY2018..PY.....M.),
            IT19=sum(Total.IT.Spending.FY2019..CY.....M.),
            IT20=sum(Total.IT.Spending.FY2020..BY.....M.),
            DME_AGENCY_18=sum(DME.PY.Agency.Funding....M.),
            DME_AGENCY_19=sum(DME.CY.Agency.Funding....M.),
            DME_AGENCY_20=sum(DME.BY.Agency.Funding....M.),
            OM_AGENCY_18=sum(O.M.PY.Agency.Funding....M.),
            OM_AGENCY_19=sum(O.M.CY.Agency.Funding....M.),
            OM_AGENCY_20=sum(O.M.BY.Agency.Funding....M.))

#write out summary
write.csv(portfolio_summary,file="portfolio_summary2019.csv")

#reconfigure summary to be by year 
#weighted_summary<-portfolio_summary%>%
#  filter(Type.of.Investment=="01 - Major IT")

#calculate the total percentages per year
#weighted_total_py<-sum(weighted_summary$DME_AGENCY_18)
#weighted_per_py<-weighted_summary$DME_AGENCY_18[[2]]/weighted_total_py

#weighted_total_cy<-sum(weighted_summary$DME_AGENCY_19)
#weighted_per_cy<-weighted_summary$DME_AGENCY_19[[2]]/weighted_total_cy

#weighted_total_by<-sum(weighted_summary$DME_AGENCY_20)
#weighted_per_by<-weighted_summary$DME_AGENCY_20[[2]]/weighted_total_by 

#put into a single dataframe 
#weighted_df<-data.frame(weighted_per_py,weighted_per_cy,weighted_per_by)

#summarize data without weights 
#add column for DME for each year 
#portfolio_contracts$DME_PY<-ifelse(portfolio_contracts$DME.PY.Agency.Funding....M.>0,1,0)
#portfolio_contracts$DME_CY<-ifelse(portfolio_contracts$DME.CY.Agency.Funding....M.>0,1,0)
#portfolio_contracts$DME_BY<-ifelse(portfolio_contracts$DME.BY.Agency.Funding....M.>0,1,0)

#form counts for modernization
counts_mod<-portfolio_contracts%>%
  dplyr::group_by(ITC)%>%
  dplyr::filter(pred_all==1)%>%
  dplyr::summarize(n=n())%>%
  dplyr::mutate(freq=n/sum(n))%>%
  dplyr::filter(ITC==1)

#form counts for major IT 
counts_major<-portfolio_contracts%>%
  dplyr::group_by(ITC)%>%
  dplyr::filter(majorIT==1)%>%
  dplyr::summarize(n=n())%>%
  dplyr::mutate(freq=n/sum(n))%>%
  dplyr::filter(ITC==1)

#form counts per agency
agency_counts_mod<-portfolio_contracts%>%
  dplyr::group_by(Agency.Name.x,ITC)%>%
  dplyr::filter(pred_all==1)%>%
  dplyr::summarize(n=n())%>%
  dplyr::mutate(freq=n/sum(n))%>%
  dplyr::filter(ITC==1)

agency_counts_major<-portfolio_contracts%>%
  dplyr::group_by(Agency.Name.x,ITC)%>%
  dplyr::filter(majorIT==1)%>%
  dplyr::summarize(n=n())%>%
  dplyr::mutate(freq=n/sum(n))%>%
  dplyr::filter(ITC==1)

#read final files
write.csv(counts_mod,"counts_mod2019.csv")
write.csv(counts_major,"counts_major2019.csv")
write.csv(agency_counts_mod,"agency_counts_mod2019.csv")
write.csv(agency_counts_major,"agency_counts_major2019.csv")