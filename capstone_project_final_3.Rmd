---
title: "Prediction of California Hospital Quality Ratings"
author: "Maria P. Frushicheva"
date: "August 15, 2016"
output: html_document
---
## Introduction 

__Importance:__ Using hospital quality ratings, patients are able to make a better decision in what hospital they want to be treated and where the best care is available in state of California, based on overall hospital performance or based on particular medical condition or procedure. 

__Question:__ Can we predict hospital quality ratings based on risk adjusted mortality rates, number of deaths, number of cases, medical procedures performed and medical conditions treated for 2012-2013?

## Description of Data Set

__Dataset:__ is available from [California Hospital Inpatient Mortality Rates and Quality Ratings, 2012-2013](https://chhs.data.ca.gov/Healthcare/California-Hospital-Inpatient-Mortality-Rates-and-/rpkf-ugbp). 

__Description of dataset:__ The dataset contains risk-adjusted mortality rates, and number of deaths and cases for 6 medical conditions treated (Acute Stroke, Acute Myocardial Infarction, Heart Failure, Gastrointestinal Hemorrhage, Hip Fracture and Pneumonia) and 6 procedures performed (Abdominal Aortic Aneurysm Repair, Carotid Endarterectomy, Craniotomy, Esophageal Resection, Pancreatic Resection, Percutaneous Coronary Intervention) in California hospitals for 2012 and 2013. This dataset does not include conditions treated or procedures performed in outpatient settings. 

## Description, Analysis and Cleaning of Variables in the Data Set

```{r,warning=FALSE,echo=FALSE,message=FALSE}
require(dplyr)
require(tidyr)
require(ggplot2)
require(gridExtra)
require(ggmap)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(randomForest)
require(caTools)
library(foreign)
library(nnet)
library(reshape2)
```

__Load the data from csv file.__ 
```{r,warning=FALSE,message=FALSE}
setwd("C:/Users/postdoc/Dropbox (Personal)/SpringBoard Fund/Rprojects/")
data <- read.csv("California_Hospital_Inpatient_Mortality_Rates_and_Quality_Ratings__2012-2013.csv",sep=",",header=TRUE)
df <- tbl_df(data)
```

__Dataset:__ __`r nrow(df)`__ observations and __`r ncol(df)`__ variables. 

__Variables with missing values:__

- Risk Adjusted Mortality Rate: The Risk Adjusted Mortality Rates (RAMR) presented here adjusts the observed mortality rates. This statistical methodology takes into account pre-existing health problems that put some patients at greater risk of death to level the playing field and allow fair comparisons across hospitals; __`r length(df$Risk.Adjusted.Mortality.Rate[is.na(df$Risk.Adjusted.Mortality.Rate)])`__ missing values.
- Number of Deaths: Number of patients that died in this hospital; __`r length(df$X..of.Deaths[is.na(df$X..of.Deaths)])`__ missing values.
- Number of Cases: Number of patients that had this medical procedure or condition in this hospital; __`r length(df$X..of.Cases[is.na(df$X..of.Cases)])`__ missing values.

__Remove missing values, because number of missing values consists of half of dataset.__
```{r}
df_clean <- df[which(is.na(df$X..of.Cases)==F),]
```

__Clean Dataset:__ __`r nrow(df_clean)`__ observations and __`r ncol(df_clean)`__ variables. 

__Variables with no missing values:__

- Year: __`r length(df_clean$Year[df_clean$Year==2012])`__ values for 2012 year and __`r length(df_clean$Year[df_clean$Year==2013])`__ values for 2013 year. 
- County: __55__ counties.
- Hospital: __341__ hospitals. 
- OSHPDID: A unique number established by the Office of Statewide Health Planning and Development (OSHPD) for identifying facilities and used in the Licensed Facility Information System (LFIS). The first three numbers identify the type of facility, the next two represent the county number, and the last five are randomly assigned within each county. 570261 unique codes. 
- Longitude: __Longitude__ of hospital. 
- Latitude: __Latitude__ of hospital.
- location1: __333__ levels. 
- Hospital Ratings: Comparison rating based on a 95% Confidence Interval (CI). If a hospitals upper CI is less than the statewide observed rate, it is designated as performing better than the average hospital. If a hospitals lower CI is greater than the state rate, it is designated as performing worse than the average state hospital. __3 levels of Hospital Ratings:__ As Expected, Better and Worse.
```{r}
summary(df_clean$Hospital.Ratings)
```

- Procedure.Condition: Procedure that was performed or condition that was treated. __6__ medical __procedures__ performed: Abdominal Aortic Aneurysm (AAA) Repair, Carotid Endarterectomy, Craniotomy, Esophageal Resection, Pancreatic Resection, Percutaneous Coronary Intervention. __6__ medical __conditions__ treated: Acute Stroke, Acute Myocardial Infarction, Heart Failure, Gastrointestinal Hemorrhage, Hip Fracture and Pneumonia. Clean dataset contains __17 levels, instead of 12.__ 
```{r}
summary(df_clean$Procedure.Condition)
```

__Decoding Procedure.Condition variable.__

According to the American Stroke Association (ASA), strokes can be classified into 2 main categories: __87%__ are ischemic strokes, caused by blockage of an artery; __13%__ are hemorrhagic strokes, caused by bleeding. Ischemic strokes are further divided into 2 groups: thrombotic and embolic strokes. Hemorrhagic strokes are divided into 2 main categories: intracerebral and subarachnoid hemorrhages. 

Our clean dataset has four categories for Acute Stroke: 

- Acute Stroke: 617 observations;
- Acute Stroke Hemorrhagic: 466 observations;
- Acute Stroke Ischemic: 615 obervations;
- Acute Stroke Subarachnoid: 241 observations.

Within each hospital, there are different notations for Acute Stroke variable. It suggests that different doctor uses different notations for the condition. These four categories are combined in one: Acute Stroke. 
```{r}
df_clean$Procedure.Condition <- gsub("Acute Stroke .*","Acute Stroke",df_clean$Procedure.Condition)
df_clean$Procedure.Condition <- factor(df_clean$Procedure.Condition)
```

Two additional categories are present in Procedure.Condition variable:

- Pancreatic Cancer: 142 observations;
- Pancreatic Other: 130 observations. 

These categories are separate medical conditions and are not combined in one category. 

The Procedure.Condition variable contains 6 medical procedures and 8 medical conditions. To indicate what procedure was performed or what condition was treated, the Medical_Category variable was added to the clean dataset. 

```{r}
df_clean <- df_clean %>% 
  mutate(Medical_Category = ifelse(grepl("Repair",Procedure.Condition) | grepl("Endarterectomy",Procedure.Condition) | grepl("Craniotomy",Procedure.Condition) | grepl("Resection",Procedure.Condition) | grepl("PCI",Procedure.Condition),               "Procedure", "Condition"))
```

__Decoding Hospital.Ratings variable.__
```{r,warning=FALSE,message=FALSE}
df_clean <- df_clean %>% mutate(ratings =
           ifelse(grepl("As Expected",Hospital.Ratings),"0",                        
           ifelse(grepl("Better",Hospital.Ratings),"1",
           ifelse(grepl("Worse",Hospital.Ratings),"-1",NA))))
df_clean$ratings <- as.numeric(df_clean$ratings)
```

__Combine Acute Stroke repetitions for each hospital, so each hospital has one unique value for Procedure.Condition variable.__
```{r,warning=FALSE,message=FALSE}
df_clean_original <- df_clean
df_clean <- df_clean %>% group_by(Year,Hospital,Procedure.Condition,
                                   Longitude,Latitude,Medical_Category) %>% 
             summarise(ratings = sum(ratings), 
                       X..of.Deaths = sum(X..of.Deaths),
                       X..of.Cases = sum(X..of.Cases),
                       Risk.Adjusted.Mortality.Rate = sum(Risk.Adjusted.Mortality.Rate)) %>% 
             mutate(Hospital.Ratings =
                       ifelse(ratings > 0,"Better",
                       ifelse(ratings < 0, "Worse","As Expected")))
df_clean$Hospital.Ratings <- as.factor(df_clean$Hospital.Ratings)
```

## Explanatory Data Analysis

__Density Plots for # of Cases, # of Deaths and Risk Adjusted Mortality Rate by Hospital Ratings.__ 
```{r,warning=FALSE,message=FALSE}
p1 <- ggplot(df_clean,aes(log(X..of.Cases),fill=factor(Hospital.Ratings),colour=factor(Hospital.Ratings)))+
  geom_density(alpha = 0.1)

p2 <- ggplot(df_clean,aes(log(X..of.Deaths),fill=factor(Hospital.Ratings),colour=factor(Hospital.Ratings)))+
  geom_density(alpha = 0.1)

p3 <- ggplot(df_clean,aes(log(Risk.Adjusted.Mortality.Rate),fill=factor(Hospital.Ratings),colour=factor(Hospital.Ratings)))+
  geom_density(alpha = 0.1)

grid.arrange(p1, p2, p3, ncol=1)
```

__Conclusions 1:__ 

- Distributions between 2012 and 2013 years look simiar (graphs are not shown). 
- There are no associations between variables for number of deaths and number of cases.
- There is a possible __association__ between the risk adjusted mortality rate and hospital ratings.
- Lower the risk adjusted mortality rate, better the hospital ratings.
- Higher the risk adjusted mortality rate, worse the hospital ratings.

__Density Plots for Risk Adjusted Mortality Rate by Procedures Performed and Hospital Ratings.__ 
```{r,warning=FALSE,message=FALSE}
df_p <- df_clean[which(df_clean$Medical_Category=="Procedure"),]

p6 <- ggplot(df_p,aes(log(Risk.Adjusted.Mortality.Rate),fill=factor(Hospital.Ratings),colour=factor(Hospital.Ratings)))+
  geom_density(alpha = 0.1)+
  theme(legend.position='bottom')+
  facet_wrap(~ Procedure.Condition, ncol=2, scales="free_y")
p6
```

- The highest mortality rates are for Craniotomy and Pancreatic Resection procedures.  
- Better and worst ratings are for Craniotomy and PCI procedures. 
- There is __association__ between the risk adjusted mortality rate and hospital ratings.

__Density Plots for Risk Adjusted Mortality Rate by Conditions Treated and Hospital Ratings.__
```{r,warning=FALSE,message=FALSE}
df_c <- df_clean[which(df_clean$Medical_Category=="Condition"),]

p9 <- ggplot(df_c,aes(log(Risk.Adjusted.Mortality.Rate),fill=factor(Hospital.Ratings),colour=factor(Hospital.Ratings)))+
  geom_density(alpha = 0.1)+
  theme(legend.position='bottom')+
  facet_wrap(~ Procedure.Condition, ncol=2, scales="free_y")
p9
```

- The highest mortality rates are for Acute Stroke, AMI and Heart Failure conditions. 
- Better and worse ratings are for Acute Stroke, AMI, GI Hemorrhage and Heart Failure conditions.
- There is __association__ between the risk adjusted mortality rate and hospital ratings.

### __Associations between medical procedures or conditions with hospital ratings, number of cases, number of deaths and risk adjusted mortality rate.__

__Procedures.__
```{r,warning=FALSE,message=FALSE}
df_p_all <- df_p %>% 
  group_by(Procedure.Condition) %>% 
  summarise(all_cases = sum(X..of.Cases), 
            all_deaths = sum(X..of.Deaths),
            all_mortality_rate = sum(Risk.Adjusted.Mortality.Rate))
df_p_all 
```

__Conditions.__
```{r,warning=FALSE,message=FALSE}
df_c_all <- df_c %>% 
  group_by(Procedure.Condition) %>% 
  summarise(all_cases = sum(X..of.Cases), 
            all_deaths = sum(X..of.Deaths),
            all_mortality_rate = sum(Risk.Adjusted.Mortality.Rate))
df_c_all
```

- The highest number of cases is for PCI and Craniotomy procedures, Acute Stroke, Heart Failure, AMI and GI Hemorrhage conditions.
- The highest number of deaths is for Craniotomy and PCI procedures, Acute Stroke, AMI and Heart Failure conditions.
- The highest mortality rates are for Craniotomy and Pancreatic Resection procedures, Acute Stroke, AMI and Heart Failure conditions. 

- The lowest number of cases is for Esophageal Resection procedure, Pancreatic Cancer and Pancreatic Other conditions. 
- The lowest number of deaths is for Esophageal Resection procedure, Pancreatic Cancer and Pancreatic Other conditions.
- The lowest mortality rates are for Carotid Endarterectomy procedure, Pancreatic Other and Pneumonia conditions. 

__Hospital Ratings.__
```{r,warning=FALSE,message=FALSE}
prop.table(table(df_clean$Procedure.Condition,df_clean$Hospital.Ratings))*100
```

- Better ratings are for Craniotomy procedure, Acute Stroke, AMI and Heart Failure conditions.
- Worse ragings are for Craniotomy and PCI procedures, Acute Stroke, AMI, GI Hemorrhage and Heart Failure conditions.  
- As Expected ratings are for Acute Stroke, AMI, GI Hemorrhage and Heart Failure conditions.

__Conclusions 2:__ 

- There is __association__ between the risk adjusted mortality rate and hospital ratings.
- Lower the risk adjusted mortality rate, better the hospital ratings.
- Higher the risk adjusted mortality rate, worse the hospital ratings.

- __Procedures__: 
    - with severe outcomes: PCI, Craniotomy and Pancreatic Resection.
    - with good outcomes: Esophageal Resection and Carotid Endarterectomy. 
  
- __Conditions__: 
    - with severe outcomes: Acute Stroke, AMI, Heart Failure and GI Hemorrhage.
    - with good outcomes: Pancreatic Cancer, Pancreatic Other and Pneumonia. 

### __Mapping and summary__ of overall hospital quality ratings and mean mortality rate among all conditions and procedures. 

__Summary of hospital ratings over all conditions and procedues.__ 
```{r,warning=FALSE,message=FALSE}
all_ratings <- df_clean %>% 
                group_by(Hospital,Latitude,Longitude) %>% 
                summarise(all_ratings = 0.5 * sum(ratings), # 0.5 to account for 2012 and 2013
                          mean_mortality_rate = mean(Risk.Adjusted.Mortality.Rate)) %>% 
                mutate(ratings =
                         ifelse(all_ratings > 0,"Better",
                         ifelse(all_ratings < 0, "Worse","As Expected")))
all_ratings$ratings <- as.factor(all_ratings$ratings)
all_ratings <- tbl_df(all_ratings)
```

__Mapping of overall hospital ratings and mean mortality rates.__ 
```{r,warning=FALSE,message=FALSE}
CAmap <- get_map(location="California",source="google",maptype="roadmap",crop=FALSE,zoom=6) 
ggmap (CAmap) +
  geom_point(aes(x=Longitude,y=Latitude,size=mean_mortality_rate,colour=ratings),data=all_ratings,alpha=0.5)+
  scale_colour_manual(values=c("Worse" = "darkred","Better" = "darkblue","As Expected" = "darkgrey"))+
  scale_size(range = c(0, 10))
```

__Overall Hospital Ratings:__
```{r,warning=FALSE,message=FALSE}
summary(all_ratings$ratings)
```

- Top 5 hospitals with the __best__ quality ratings:
```{r,warning=FALSE,message=FALSE}
all_ratings %>% arrange(desc(all_ratings)) %>% select(Hospital) %>% slice(1:5)
```

- Top 5 hospitals with the __lowest__ mean mortality rate:
```{r,warning=FALSE,message=FALSE}
all_ratings %>% arrange(mean_mortality_rate) %>% select(Hospital) %>% slice(1:5)
```

- Top 5 hospitals with the __worst__ quality ratings:
```{r,warning=FALSE,message=FALSE}
all_ratings %>% arrange(all_ratings) %>% select(Hospital) %>% slice(1:5)
```

- Top 5 hospitals with the __highest__ mean mortality rate:
```{r,warning=FALSE,message=FALSE}
all_ratings %>% arrange(desc(mean_mortality_rate)) %>% select(Hospital)  %>% slice(1:5)
```

### __Summary__ of hospital quality ratings and mortality rates for Acute Stroke, AMI and Heart Failure __conditions__, PCI, Craniotomy and Pancreatic Resection __procedures__. 

```{r,warning=FALSE,message=FALSE,eval=FALSE,echo=FALSE}
for (cond in c("Acute Stroke","AMI","Heart Failure","PCI","Craniotomy","Pancreatic Resection")){
     var <- gsub(" ","_",paste("df",cond))
     var <- df_c[which(df_c$Procedure.Condition==cond),]
     print(paste("The year is", cond))
     #print(var)
}
```

```{r,warning=FALSE,message=FALSE}
df_as <- df_c[which(df_c$Procedure.Condition=="Acute Stroke"),]

df_as_all <- df_as %>% 
                group_by(Hospital,Latitude,Longitude) %>% 
                summarise(all_ratings = 0.5 * sum(ratings), # to account for 2012 and 2013 
                          mean_mortality_rate = mean(Risk.Adjusted.Mortality.Rate)) %>% 
                mutate(ratings =
                         ifelse(all_ratings > 0,"Better",
                         ifelse(all_ratings < 0, "Worse","As Expected")),
                       Procedure.Condition="Acute Stroke")

df_as_all$ratings <- as.factor(df_as_all$ratings)
df_as_all <- tbl_df(df_as_all)

df_as_best_rat <- df_as_all %>% arrange(desc(all_ratings)) %>% slice(1:50)
df_as_best <- df_as_best_rat %>% arrange(mean_mortality_rate) %>% slice(1:25)
```

__Top 5 hospitals for treatment of Acute Stroke condition.__
```{r,warning=FALSE,message=FALSE}
df_as_best %>% slice(1:5) %>% select(Hospital)
```

__Top 5 hospitals for treatment of AMI condition.__
```{r,warning=FALSE,message=FALSE,echo=FALSE}
df_ami <- df_c[which(df_c$Procedure.Condition=="AMI"),]

df_ami_all <- df_ami %>% 
                group_by(Hospital,Latitude,Longitude) %>% 
                summarise(all_ratings = 0.5 * sum(ratings), 
                          mean_mortality_rate = mean(Risk.Adjusted.Mortality.Rate)) %>% 
                mutate(ratings =
                         ifelse(all_ratings > 0,"Better",
                         ifelse(all_ratings < 0, "Worse","As Expected")),
                       Procedure.Condition="AMI")
df_ami_all$ratings <- as.factor(df_ami_all$ratings)
df_ami_all <- tbl_df(df_ami_all)

df_ami_best_rat <- df_ami_all %>% arrange(desc(all_ratings)) %>% slice(1:50)
df_ami_best <- df_ami_best_rat %>% arrange(mean_mortality_rate) %>% slice(1:25)
df_ami_best %>% slice(1:5) %>% select(Hospital)
```

__Top 5 hospitals for treatment of Heart Failure condition.__
```{r,warning=FALSE,message=FALSE,echo=FALSE}
df_hf <- df_c[which(df_c$Procedure.Condition=="Heart Failure"),]

df_hf_all <- df_hf %>% 
                group_by(Hospital,Latitude,Longitude) %>% 
                summarise(all_ratings = 0.5 * sum(ratings), 
                          mean_mortality_rate = mean(Risk.Adjusted.Mortality.Rate)) %>% 
                mutate(ratings =
                         ifelse(all_ratings > 0,"Better",
                         ifelse(all_ratings < 0, "Worse","As Expected")),
                       Procedure.Condition="Heart Failure")
df_hf_all$ratings <- as.factor(df_hf_all$ratings)
df_hf_all <- tbl_df(df_hf_all)

df_hf_best_rat <- df_hf_all %>% arrange(desc(all_ratings)) %>% slice(1:50)
df_hf_best <- df_hf_best_rat %>% arrange(mean_mortality_rate) %>% slice(1:25)
df_hf_best %>% slice(1:5) %>% select(Hospital)
```

__Top 5 hospitals to perform the PCI procedure.__
```{r,warning=FALSE,message=FALSE,echo=FALSE}
df_pci <- df_p[which(df_p$Procedure.Condition=="PCI"),]

df_pci_all <- df_pci %>% 
                group_by(Hospital,Latitude,Longitude) %>% 
                summarise(all_ratings = 0.5 * sum(ratings), 
                          mean_mortality_rate = mean(Risk.Adjusted.Mortality.Rate)) %>% 
                mutate(ratings =
                         ifelse(all_ratings > 0,"Better",
                         ifelse(all_ratings < 0, "Worse","As Expected")),
                       Procedure.Condition="PCI")
df_pci_all$ratings <- as.factor(df_pci_all$ratings)
df_pci_all <- tbl_df(df_pci_all)

df_pci_best_rat <- df_pci_all %>% arrange(desc(all_ratings)) %>% slice(1:50)
df_pci_best <- df_pci_best_rat %>% arrange(mean_mortality_rate) %>% slice(1:25)
df_pci_best %>% slice(1:5) %>% select(Hospital)
```

__Top 5 hospitals to perform the Craniotomy procedure.__
```{r,warning=FALSE,message=FALSE,echo=FALSE}
df_cr <- df_p[which(df_p$Procedure.Condition=="Craniotomy"),]

df_cr_all <- df_cr %>% 
                group_by(Hospital,Latitude,Longitude) %>% 
                summarise(all_ratings = 0.5 * sum(ratings), 
                          mean_mortality_rate = mean(Risk.Adjusted.Mortality.Rate)) %>% 
                mutate(ratings =
                         ifelse(all_ratings > 0,"Better",
                         ifelse(all_ratings < 0, "Worse","As Expected")),
                       Procedure.Condition="Craniotomy")
df_cr_all$ratings <- as.factor(df_cr_all$ratings)
df_cr_all <- tbl_df(df_cr_all)

df_cr_best_rat <- df_cr_all %>% arrange(desc(all_ratings)) %>% slice(1:50)
df_cr_best <- df_cr_best_rat %>% arrange(mean_mortality_rate) %>% slice(1:25)
df_cr_best %>% slice(1:5) %>% select(Hospital)
```

__Top 5 hospitals to perform the Pancreatic Resection procedure.__
```{r,warning=FALSE,message=FALSE,echo=FALSE}
df_pr <- df_p[which(df_p$Procedure.Condition=="Pancreatic Resection"),]

df_pr_all <- df_pr %>% 
                group_by(Hospital,Latitude,Longitude) %>% 
                summarise(all_ratings = 0.5 * sum(ratings), 
                          mean_mortality_rate = mean(Risk.Adjusted.Mortality.Rate)) %>% 
                mutate(ratings =
                         ifelse(all_ratings > 0,"Better",
                         ifelse(all_ratings < 0, "Worse","As Expected")),
                       Procedure.Condition="Pancreatic Resection")
df_pr_all$ratings <- as.factor(df_pr_all$ratings)
df_pr_all <- tbl_df(df_pr_all)

df_pr_best_rat <- df_pr_all %>% arrange(desc(all_ratings)) %>% slice(1:50)
df_pr_best <- df_pr_best_rat %>% arrange(mean_mortality_rate) %>% slice(1:25)
df_pr_best %>% slice(1:5) %>% select(Hospital)
```

### __Predictions__

__Approach__

- Predict hospital quality ratings using __random forests and classification decision trees__. 
- Train the models and evaluate the model performances on 2012 training data.
- Test the model performances on 2013 test data. 

#### Hospital Ratings Prediction Using Random Forests for Dataset in __Wide__ Format.

__Cleanning the Data Set and converting to the wide format based on Procedure.Condition and Risk.Adjusted.Mortality.Rate variables.__
```{r,warning=FALSE,message=FALSE}
# convert data to the wide format
df_wide <- df_clean %>% select(Year,Hospital,Latitude,Longitude,Procedure.Condition,Hospital.Ratings,Risk.Adjusted.Mortality.Rate) %>% spread(Procedure.Condition,Risk.Adjusted.Mortality.Rate) 
# remove white spaces from column names
colnames(df_wide) <- gsub(" ","",colnames(df_wide))
# replace NA with 0, because some hospitals does not treat these conditions, thus mortality rate is zero. 
df_wide[is.na(df_wide)] <- 0
```

__Split the Data Set into 2012 training and 2013 test sets.__
```{r,warning=FALSE,message=FALSE}
train_wide <- df_wide[which(df_wide$Year==2012),]
test_wide_original <- df_wide[which(df_wide$Year==2013),]
test_wide <- subset(test_wide_original, select = -Hospital.Ratings)
```

__Feature Enginering with Random Forests__
```{r,warning=FALSE,message=FALSE}
fit <- randomForest(Hospital.Ratings ~ AAARepair + AcuteStroke + AMI + CarotidEndarterectomy + Craniotomy + EsophagealResection + GIHemorrhage + HeartFailure + HipFracture + PancreaticCancer + PancreaticOther + PancreaticResection + PCI + Pneumonia, data=train_wide,importance=TRUE,ntree=1000)
varImpPlot(fit)
```

- __The most important variables are__ Heart Failure, GI Hemorrhage, AMI and Acute Stroke, Hip Fracture __conditions__;
- __Procedure__ variables are less important and thus are not included in classification. 

__Model performance__ on __train_wide__ dataset using all variables.
```{r,warning=FALSE,message=FALSE}
# confusion matrix on train data
fit$confusion 
```

- Accuracy (how often is the classifier correct): `r sum(diag(fit$confusion))/sum(fit$confusion)`
- Error Rate (how often is the classifier wrong): `r (1 - sum(diag(fit$confusion))/sum(fit$confusion))`

__Predictions__ on __test_wide__ dataset using all variables. 
```{r,warning=FALSE,message=FALSE}
prediction <- predict(fit, test_wide)
# confusion matrix on test data
cm <- as.matrix(table(Actual = test_wide_original$Hospital.Ratings,Predicted = prediction))
cm 
rf_a <- sum(diag(cm))/sum(cm)
rf_e <- 1 - sum(diag(cm))/sum(cm)
```

- Accuracy: `r rf_a`
- Error Rate: `r rf_e`

#### Hospital Ratings Prediction Using Classification Decision Trees (CART).

__Model 1: All variables are included in tree construction.__
```{r,warning=FALSE,message=FALSE}
set.seed(34)
tree0 <- rpart(Hospital.Ratings ~ AAARepair + AcuteStroke + AMI + CarotidEndarterectomy + Craniotomy + EsophagealResection + GIHemorrhage + HeartFailure + HipFracture + PancreaticCancer + PancreaticOther + PancreaticResection + PCI + Pneumonia, data = train_wide, method = "class",control=rpart.control(cp=0.001)) 
printcp(tree0)
num <- which.min(tree0$cptable[,"xerror"])
tree0$cptable[num,]
cp.choice<-tree0$cptable[num,"CP"]
pruned.tree<-prune(tree0, cp=cp.choice)
fancyRpartPlot(pruned.tree)
```

__Predictions__ on __test_wide__ dataset.
```{r}
# Make predictions on the test set
prediction <- predict(pruned.tree, test_wide, type = "class")
# confusion matrix
cm <- as.matrix(table(Actual = test_wide_original$Hospital.Ratings,Predicted = prediction))
cm

call_a <- sum(diag(cm))/sum(cm)
call_e <- 1 - sum(diag(cm))/sum(cm)
```

- Accuracy: `r call_a` 
- Error Rate: `r call_e`


__Model 2: AMI, GIHemorrhage and HeartFailure variables are included in tree construction.__
```{r,warning=FALSE,message=FALSE}
tree1 <- rpart(Hospital.Ratings ~ AMI + GIHemorrhage + HeartFailure, data = train_wide, method = "class",control=rpart.control(cp=0.001)) # cp determines when the splitting up of the decision tree stops
printcp(tree1)
num <- which.min(tree1$cptable[,"xerror"])
tree1$cptable[num,]
cp.choice<-tree1$cptable[num,"CP"]
pruned.tree1<-prune(tree1, cp=cp.choice)
fancyRpartPlot(pruned.tree1)
```

__Predictions__ on __test_wide__ dataset.
```{r}
# Make predictions on the test set
prediction <- predict(pruned.tree1, test_wide, type = "class")
# confusion matrix
cm <- as.matrix(table(Actual = test_wide_original$Hospital.Ratings,Predicted = prediction))
cm

c3_a <- sum(diag(cm))/sum(cm)
c3_e <- 1 - sum(diag(cm))/sum(cm)
```

- Accuracy: `r c3_a` 
- Error Rate: `r c3_e`


__Conclusions 3:__ 

- __Accuracy on the test data set using__
    - Random Forests with all variables: `r round(rf_a,4)`
    - CART with all variables: `r round(call_a,4)`
    - CART with three variables: `r round(c3_a,4)`
- __Random forests__ gives the best performance, however is not good enough to predict hospitals with the best care in future. 
- __Random forests__ predicts that classification of __hospital ratings__ depend on __conditions and not procedures__ with the most severe patient outcomes.

### __Future Work__

- Predict hospital quality ratings using __multinomial logistic regression__. 
    - Train the model and evaluate the model performance on 2012 training data.
    - Test the model performance on 2013 test data. 
- Compare three models: random forests, classification decision trees and multinomial logistic regression.
    - Summarize which model gives the best performance on 2012 training data and on 2013 test data. 
    - Choose the best model and test its performance on [2014 test data](http://oshpd.ca.gov/HID/Products/PatDischargeData/AHRQ/iqi-imi_overview.html). 
- Recommend which hospitals will have the best care in future using predicted hospital ratings. 

### __Recommendations to Patients__

### Top 25 hospitals with the best overall ratings and the lowest mean mortality rate in state of California.
```{r,warning=FALSE,message=FALSE}
best_ratings <- all_ratings %>% arrange(desc(all_ratings)) %>% slice(1:50)
best_lowest <- best_ratings %>% arrange(mean_mortality_rate) %>% slice(1:25)
# best_lowest$Hospital[duplicated(best_lowest$Hospital)]

CAmap <- get_map(location="California",source="google",maptype="roadmap",crop=FALSE,zoom=6) 
ggmap (CAmap) +
  geom_point(aes(x=Longitude,y=Latitude,colour=mean_mortality_rate),data=best_lowest,size=5,alpha=0.6)+
  scale_colour_gradient(limits=c(1, 5), high="red", low="blue")
```

__Top 5 hospitals with the best overall ratings and the lowest mean mortality rate in state of California.__
```{r,warning=FALSE,message=FALSE}
best_lowest$Hospital[1:5]
```

### Top hospitals with the best ratings and the lowest mean mortality rate for Acute Stroke, AMI and Heart Failure __conditions__. 
```{r,warning=FALSE,message=FALSE}
best_cond <- bind_rows(df_as_best[1:10,],df_ami_best[1:10,],df_hf_best[1:10,])

CAmap <- get_map(location="California",source="google",maptype="roadmap",crop=FALSE,zoom=6) 
ggmap (CAmap) +
  geom_point(aes(x=Longitude,y=Latitude,size=mean_mortality_rate,colour=Procedure.Condition),data=best_cond,alpha=0.8)+
  scale_colour_manual(values=c("Acute Stroke"="darkred", "AMI"="darkgrey", "Heart Failure"="darkblue"))+
  scale_size(range = c(0, 10))
```

__There are `r length(best_cond$Hospital[duplicated(best_cond$Hospital)])` hospitals that have the best ratings and the lowest mortality rate for the most severe conditions.__ 
```{r,warning=FALSE,message=FALSE}
best_cond$Hospital[duplicated(best_cond$Hospital)]
```

### Top hospitals with the best ratings and the lowest mean mortality rate for PCI, Craniotomy and Pancreatic Resection __procedures__. 
```{r,warning=FALSE,message=FALSE}
best_proc <- bind_rows(df_pci_best[1:10,],df_cr_best[1:10,],df_pr_best[1:10,])

CAmap <- get_map(location="California",source="google",maptype="roadmap",crop=FALSE,zoom=6) 
ggmap (CAmap) +
  geom_point(aes(x=Longitude,y=Latitude,size=mean_mortality_rate,colour=Procedure.Condition),data=best_proc,alpha=0.6)+
  scale_colour_manual(values=c("PCI"="darkblue", "Craniotomy"="darkred", "Pancreatic Resection"="darkgrey"))+
  scale_size(range = c(0, 10))
```

__There are `r length(best_proc$Hospital[duplicated(best_proc$Hospital)])` hospitals that have the best ratings and the lowest mortality rate for the most severe procedures.__
```{r,warning=FALSE,message=FALSE}
best_proc$Hospital[duplicated(best_proc$Hospital)]
```