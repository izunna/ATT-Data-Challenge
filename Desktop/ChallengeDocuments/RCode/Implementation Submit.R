gradData = read.csv("GraduationCensusNoNA.csv")
gData2 = na.omit(gradData)
str(gradData)

###############################################################
#Running the Boruta Algorithm
library(Boruta)
set.seed(777)
BorGdata = Boruta(ALL_RATE_1112~.,data=gData2, doTrace = 2,
                  ntree = 500,maxRuns = 20)
print(getSelectedAttributes(BorGdata))
testBor = BorGdata
TentativeRoughFix(testBor, averageOver = Inf)
print(getSelectedAttributes(testBor))
print(BorGdata)
###############################################################

#Splitting the Data
library(caTools)
set.seed(3000)
gData2$completeGrad = ifelse(gData2$ALL_RATE_1112 >= 90, 1,0)
str(gData2)
spl = sample.split(gData2$completeGrad, SplitRatio = 0.7)

gDataTrain = subset(gData2, spl==TRUE)
gDataTest = subset(gData2, spl==FALSE)

###############################################################
#logistic regression model:
gDataLogNoID = glm(completeGrad ~                         
                    ALL_COHORT_1112                 
                   +MWH_COHORT_1112                 
                   +CWD_COHORT_1112                 
                   +ECD_COHORT_1112                 
                   +LAND_AREA                       
                   +RURAL_POP_CEN_2010              
                   +Hispanic_CEN_2010               
                   +NH_White_alone_CEN_2010         
                   +NH_White_alone_ACS_08_12        
                   +NH_Blk_alone_CEN_2010           
                   +NH_Blk_alone_ACS_08_12          
                   +NH_Blk_alone_ACSMOE_08_12       
                   +NH_AIAN_alone_CEN_2010          
                   +Not_HS_Grad_ACS_08_12           
                   +College_ACS_08_12               
                   +College_ACSMOE_08_12            
                   +Prs_Blw_Pov_Lev_ACS_08_12       
                   +Prs_Blw_Pov_Lev_ACSMOE_08_12    
                   +US_Cit_Nat_ACS_08_12            
                   +MrdCple_Fmly_HHD_ACS_08_12      
                   +Female_No_HB_CEN_2010           
                   +Med_HHD_Inc_ACS_08_12           
                   +Tot_Vacant_Units_CEN_2010       
                   +Tot_Vacant_Units_ACS_08_12      
                   +Mobile_Homes_ACS_08_12          
                   +No_Plumb_ACS_08_12              
                   +FRST_FRMS_CEN_2010              
                   +Mail_Return_Rate_CEN_2010       
                   +Low_Response_Score              
                   +pct_Pop_5_17_CEN_2010           
                   +pct_Pop_18_24_CEN_2010          
                   +pct_Hispanic_ACS_08_12          
                   +pct_NH_White_alone_CEN_2010     
                   +pct_NH_White_alone_ACS_08_12    
                   +pct_NH_Blk_alone_CEN_2010       
                   +pct_NH_Blk_alone_ACS_08_12      
                   +pct_NH_Blk_alone_ACSMOE_08_12   
                   +pct_NH_AIAN_alone_CEN_2010      
                   +pct_NH_Asian_alone_CEN_2010     
                   +pct_Pop_5yrs_Over_ACSMOE_08_12  
                   +pct_Age5p_Only_Eng_ACS_08_12    
                   +pct_Age5p_Scandinav_ACSMOE_08_12
                   +pct_Pop_25yrs_Over_ACSMOE_08_12 
                   +pct_Not_HS_Grad_ACS_08_12       
                   +pct_Not_HS_Grad_ACSMOE_08_12    
                   +pct_College_ACS_08_12           
                   +pct_College_ACSMOE_08_12        
                   +pct_Prs_Blw_Pov_Lev_ACS_08_12   
                   +pct_Prs_Blw_Pov_Lev_ACSMOE_08_12
                   +pct_Civ_emp_16p_ACS_08_12       
                   +pct_Civ_unemp_16p_ACSMOE_08_12  
                   +pct_Pop_1yr_Over_ACSMOE_08_12   
                   +pct_Diff_HU_1yr_Ago_ACSMOE_08_12
                   +pct_Born_US_ACS_08_12           
                   +pct_MrdCple_HHD_ACS_08_12       
                   +pct_MrdCple_HHD_ACSMOE_08_12    
                   +pct_Not_MrdCple_HHD_ACS_08_12   
                   +pct_Female_No_HB_CEN_2010       
                   +pct_Female_No_HB_ACS_08_12      
                   +pct_HHD_PPL_Und_18_CEN_2010     
                   +pct_Tot_Occp_Units_CEN_2010     
                   +pct_Vacant_Units_CEN_2010       
                   +pct_Vacant_Units_ACSMOE_08_12   
                   +pct_Single_Unit_ACS_08_12       
                   +pct_MLT_U2_9_STRC_ACSMOE_08_12  
                   +pct_Mobile_Homes_ACS_08_12      
                   +pct_Mobile_Homes_ACSMOE_08_12   
                   +pct_No_Plumb_ACS_08_12          
                   +pct_Census_Mail_Returns_CEN_2010
                   +pct_Mailback_Count_CEN_2010     
                   +pct_FRST_FRMS_CEN_2010, data = gDataTrain, family=binomial)

summary(gDataLogNoID)

#Training Data Prediction 
PredictLogNoID = predict(gDataLogNoID, type = "response")
table(gDataTrain$completeGrad, PredictLogNoID > 0.5)

# Logistic Regression Testing Data
PredictLogTest = predict(gDataLogNoID, newdata = gDataTest, type = "response")

table(gDataTest$completeGrad, PredictLogTestNoID > 0.5)
((1287+600)/2682)*100

#Cart Testing Data
prp(gDataTree)
PredictCARTTest = predict(gDataTree, newdata=gDataTest, type = "class")
table(gDataTest$completeGrad, PredictCARTTest)
((1193+762)/2628)*100

###############################################################
#Using Cross Validation to obtain cp Value
# cv stands for cross validation
library(caret)
library(e1071)
folds = trainControl(method = "cv", number = 10)

# Pick possible values for our parameter cp

cpValues = expand.grid(.cp = seq(0.001,0.1,0.001)) 
gDataTrain$completeGrad = as.factor(gDataTrain$completeGrad)

# Perform the cross validation

train(completeGrad ~                          
        ALL_COHORT_1112                 
      +MWH_COHORT_1112                 
      +CWD_COHORT_1112                 
      +ECD_COHORT_1112                 
      +LAND_AREA                       
      +RURAL_POP_CEN_2010              
      +Hispanic_CEN_2010               
      +NH_White_alone_CEN_2010         
      +NH_White_alone_ACS_08_12        
      +NH_Blk_alone_CEN_2010           
      +NH_Blk_alone_ACS_08_12          
      +NH_Blk_alone_ACSMOE_08_12       
      +NH_AIAN_alone_CEN_2010          
      +Not_HS_Grad_ACS_08_12           
      +College_ACS_08_12               
      +College_ACSMOE_08_12            
      +Prs_Blw_Pov_Lev_ACS_08_12       
      +Prs_Blw_Pov_Lev_ACSMOE_08_12    
      +US_Cit_Nat_ACS_08_12            
      +MrdCple_Fmly_HHD_ACS_08_12      
      +Female_No_HB_CEN_2010           
      +Med_HHD_Inc_ACS_08_12           
      +Tot_Vacant_Units_CEN_2010       
      +Tot_Vacant_Units_ACS_08_12      
      +Mobile_Homes_ACS_08_12          
      +No_Plumb_ACS_08_12              
      +FRST_FRMS_CEN_2010              
      +Mail_Return_Rate_CEN_2010       
      +Low_Response_Score              
      +pct_Pop_5_17_CEN_2010           
      +pct_Pop_18_24_CEN_2010          
      +pct_Hispanic_ACS_08_12          
      +pct_NH_White_alone_CEN_2010     
      +pct_NH_White_alone_ACS_08_12    
      +pct_NH_Blk_alone_CEN_2010       
      +pct_NH_Blk_alone_ACS_08_12      
      +pct_NH_Blk_alone_ACSMOE_08_12   
      +pct_NH_AIAN_alone_CEN_2010      
      +pct_NH_Asian_alone_CEN_2010     
      +pct_Pop_5yrs_Over_ACSMOE_08_12  
      +pct_Age5p_Only_Eng_ACS_08_12    
      +pct_Age5p_Scandinav_ACSMOE_08_12
      +pct_Pop_25yrs_Over_ACSMOE_08_12 
      +pct_Not_HS_Grad_ACS_08_12       
      +pct_Not_HS_Grad_ACSMOE_08_12    
      +pct_College_ACS_08_12           
      +pct_College_ACSMOE_08_12        
      +pct_Prs_Blw_Pov_Lev_ACS_08_12   
      +pct_Prs_Blw_Pov_Lev_ACSMOE_08_12
      +pct_Civ_emp_16p_ACS_08_12       
      +pct_Civ_unemp_16p_ACSMOE_08_12  
      +pct_Pop_1yr_Over_ACSMOE_08_12   
      +pct_Diff_HU_1yr_Ago_ACSMOE_08_12
      +pct_Born_US_ACS_08_12           
      +pct_MrdCple_HHD_ACS_08_12       
      +pct_MrdCple_HHD_ACSMOE_08_12    
      +pct_Not_MrdCple_HHD_ACS_08_12   
      +pct_Female_No_HB_CEN_2010       
      +pct_Female_No_HB_ACS_08_12      
      +pct_HHD_PPL_Und_18_CEN_2010     
      +pct_Tot_Occp_Units_CEN_2010     
      +pct_Vacant_Units_CEN_2010       
      +pct_Vacant_Units_ACSMOE_08_12   
      +pct_Single_Unit_ACS_08_12       
      +pct_MLT_U2_9_STRC_ACSMOE_08_12  
      +pct_Mobile_Homes_ACS_08_12      
      +pct_Mobile_Homes_ACSMOE_08_12   
      +pct_No_Plumb_ACS_08_12          
      +pct_Census_Mail_Returns_CEN_2010
      +pct_Mailback_Count_CEN_2010     
      +pct_FRST_FRMS_CEN_2010, data = gDataTrain, method = "rpart", trControl = folds, tuneGrid = cpValues)

###############################################################
#Moving Onto the Cart Model
library(rpart)
library(rpart.plot)

# Create a CART tree:
gDataTree = rpart(completeGrad ~                            
                    ALL_COHORT_1112                 
                  +MWH_COHORT_1112                 
                  +CWD_COHORT_1112                 
                  +ECD_COHORT_1112                 
                  +LAND_AREA                       
                  +RURAL_POP_CEN_2010              
                  +Hispanic_CEN_2010               
                  +NH_White_alone_CEN_2010         
                  +NH_White_alone_ACS_08_12        
                  +NH_Blk_alone_CEN_2010           
                  +NH_Blk_alone_ACS_08_12          
                  +NH_Blk_alone_ACSMOE_08_12       
                  +NH_AIAN_alone_CEN_2010          
                  +Not_HS_Grad_ACS_08_12           
                  +College_ACS_08_12               
                  +College_ACSMOE_08_12            
                  +Prs_Blw_Pov_Lev_ACS_08_12       
                  +Prs_Blw_Pov_Lev_ACSMOE_08_12    
                  +US_Cit_Nat_ACS_08_12            
                  +MrdCple_Fmly_HHD_ACS_08_12      
                  +Female_No_HB_CEN_2010           
                  +Med_HHD_Inc_ACS_08_12           
                  +Tot_Vacant_Units_CEN_2010       
                  +Tot_Vacant_Units_ACS_08_12      
                  +Mobile_Homes_ACS_08_12          
                  +No_Plumb_ACS_08_12              
                  +FRST_FRMS_CEN_2010              
                  +Mail_Return_Rate_CEN_2010       
                  +Low_Response_Score              
                  +pct_Pop_5_17_CEN_2010           
                  +pct_Pop_18_24_CEN_2010          
                  +pct_Hispanic_ACS_08_12          
                  +pct_NH_White_alone_CEN_2010     
                  +pct_NH_White_alone_ACS_08_12    
                  +pct_NH_Blk_alone_CEN_2010       
                  +pct_NH_Blk_alone_ACS_08_12      
                  +pct_NH_Blk_alone_ACSMOE_08_12   
                  +pct_NH_AIAN_alone_CEN_2010      
                  +pct_NH_Asian_alone_CEN_2010     
                  +pct_Pop_5yrs_Over_ACSMOE_08_12  
                  +pct_Age5p_Only_Eng_ACS_08_12    
                  +pct_Age5p_Scandinav_ACSMOE_08_12
                  +pct_Pop_25yrs_Over_ACSMOE_08_12 
                  +pct_Not_HS_Grad_ACS_08_12       
                  +pct_Not_HS_Grad_ACSMOE_08_12    
                  +pct_College_ACS_08_12           
                  +pct_College_ACSMOE_08_12        
                  +pct_Prs_Blw_Pov_Lev_ACS_08_12   
                  +pct_Prs_Blw_Pov_Lev_ACSMOE_08_12
                  +pct_Civ_emp_16p_ACS_08_12       
                  +pct_Civ_unemp_16p_ACSMOE_08_12  
                  +pct_Pop_1yr_Over_ACSMOE_08_12   
                  +pct_Diff_HU_1yr_Ago_ACSMOE_08_12
                  +pct_Born_US_ACS_08_12           
                  +pct_MrdCple_HHD_ACS_08_12       
                  +pct_MrdCple_HHD_ACSMOE_08_12    
                  +pct_Not_MrdCple_HHD_ACS_08_12   
                  +pct_Female_No_HB_CEN_2010       
                  +pct_Female_No_HB_ACS_08_12      
                  +pct_HHD_PPL_Und_18_CEN_2010     
                  +pct_Tot_Occp_Units_CEN_2010     
                  +pct_Vacant_Units_CEN_2010       
                  +pct_Vacant_Units_ACSMOE_08_12   
                  +pct_Single_Unit_ACS_08_12       
                  +pct_MLT_U2_9_STRC_ACSMOE_08_12  
                  +pct_Mobile_Homes_ACS_08_12      
                  +pct_Mobile_Homes_ACSMOE_08_12   
                  +pct_No_Plumb_ACS_08_12          
                  +pct_Census_Mail_Returns_CEN_2010
                  +pct_Mailback_Count_CEN_2010     
                  +pct_FRST_FRMS_CEN_2010, data = gDataTrain, method="class", cp = 0.006)


prp(gDataTree)

# Predict on the training set:
PredictCART = predict(gDataTree, type = "class")
table(gDataTrain$completeGrad, PredictCART)

prp(gDataTree)
PredictCARTTest = predict(gDataTree, newdata=gDataTest, type = "class")
table(gDataTest$completeGrad, PredictCARTTest)

###############################################################
