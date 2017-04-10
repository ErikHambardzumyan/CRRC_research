
################################################################### 
# THIS FILE CONTAINS FULL COMPOSITION OF DECISION TREE            #
#                 OF PERSONAL INCOME LAST MONTH                   #
#                                                                 #
# E-mail : e.hambardzumyan@crrc.am                                #
#                                                                 #
# Dataset reference:                                              #
#                                                                 #
#     Caucasus Research Resource Centers. (2015) "Caucasus        #
#                                                                 #
# Barometer". Retrieved from                                      #
#                                                                 #
# http://www.crrc.am/caucasusbarometer/documentation?lang=en      # 
#                                                                 #
#                                                                 #                                     
###################################################################
# Author : Erik Hambardzumyan 2016 under CRRC Armenia Fellowship  #
#                                                                 #
# License: MIT                                                    #
###################################################################

# ===================================================================================
# REFERENCES:
#     A. Liaw and M. Wiener (2002). Classification and Regression by 
# randomForest. R News 2(3), 18--22.
#     
#     Pruning (decision trees). (n.d.). 
# Retrieved from https://en.wikipedia.org/wiki/Pruning_(decision_trees)
#
#     H. Wickham. ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York, 2009.
#
#     Max Kuhn. Contributions from Jed Wing, Steve Weston, Andre Williams, Chris Keefer, Allan
# Engelhardt, Tony Cooper, Zachary Mayer, Brenton Kenkel, the R Core Team, Michael
# Benesty, Reynald Lescarbeau, Andrew Ziem, Luca Scrucca, Yuan Tang and Can Candan.
# (2016). caret: Classification and Regression Training. R package version 6.0-71.
# https://CRAN.R-project.org/package=caret
#      
#   Receiver operating characteristic. (n.d.). 
# Retrieved from https://en.wikipedia.org/wiki/Receiver_operating_characteristic
#
#      Sing T, Sander O, Beerenwinkel N and Lengauer T (2005). "ROCR: visualizing classifier performance in R."
# _Bioinformatics_, *21*(20), pp. 7881. <URL: http://rocr.bioinf.mpi-sb.mpg.de>.
#      
#     Terry Therneau, Beth Atkinson and Brian Ripley (2015). rpart: Recursive Partitioning and
# Regression Trees. R package version 4.1-10. https://CRAN.R-project.org/package=rpart
# ===================================================================================

###Calling all the necessary packages
library(foreign)
library(rpart)
library(rpart.plot)
library(rattle)
library(ROCR)
library(class)
library(caret)
library(e1071)
library(randomForest)
library(ggplot2)

###reading from SPSS file

dataset<-read.spss("CB_2015_Regional_Public_14.04.16_Databases.sav", to.data.frame=T)

## when you run this line you will get a warning messages 

#the code below is to fix the problem

indx <- sapply(dataset, is.factor)

dataset[indx] <- lapply(dataset[indx], function(x) {
  
  levels(x) <- make.unique(levels(x))
  
  x})

#removing the columns having missing values more than 20% of observations

dataset <- dataset[, colMeans(is.na(dataset)) <= .2]

## splitting into Armenian and Georgian observations

Armenia<-dataset[ which(dataset$COUNTRY=="Armenia"),]

Georgia<-dataset[ which(dataset$COUNTRY=="Georgia"),]

Armenia<-Armenia[Armenia$WORKYRS !="1914",]
#############################################

##picking certain reasonable indipendent variables for "PERSINC" as dependent

fm<-formula(PERSINC~AGE+EDUYRS+RESPSEX+STRATUM+WORKSEC+JOBSATFN+
              
              WORKYRS+GETJOBF+TRUBANK+TRUEXEC+IMPISS1+INTACEM+JOBDESC+
              
              EMIGRAT+FUTRUNG+FUTRUNG+KNOWENG+COMPABL+WORKTYP+EMPLSIT)

#   **Check the variable names in the documentation reference above**

###############################################

###cleaning the data from unnecessary observations

##i.e. removing the specefic rows,e.g remove all "Refuse to answer"-s

Armenia<-Armenia[Armenia$PERSINC !="Don't know",]

Armenia<-Armenia[Armenia$PERSINC !="Refuse to answer",]

Armenia<-Armenia[Armenia$PERSINC !="Interviewer error",]

##Converting the variable with several levels into binary

high<-c("More than USD 1200","USD 801 - 1200", "USD 401 - 800", "USD 251 - 400")

##the following ranges of income will be considered  "High"

levels(Armenia$PERSINC) <- list(High = high,
                                
                                Low = levels(Armenia$PERSINC)[!levels(Armenia$PERSINC) %in% high])

###other than "High" assign "Low"
qplot(PERSINC, data=Armenia)
#########

####### variable selection

#####Set the random seed to 123 (applies everywhere)

## we create a randomForest model to check for variable importance

### nodesize - Minimum size of terminal nodes. The default is 1 I prefer to set 7

####ntree - Number of trees to grow (large numbers are preffered)

#####do.trace - If set to TRUE, give a more verbose output as randomForest is run

####### importance - Should importance of predictors be assessed

set.seed(123)

model<-randomForest(fm, data=Armenia,nodesize=7,
                    
                    do.trace=T,importance=T,ntree=2000)


varImpPlot(model)

###checking for important variables considering mean decrease in accuracy

# The more is the mean decrease in accuracy, the better is the model

table(Armenia$WORKYRS)

####Years one's been working at the workplace variable has

##a lot of legal skips which idicates the pseron has been jobless

table(Armenia$WORKSEC)

##Legal skips both in working year and working sector are the same

table(Armenia$EMPLSIT)

table(Armenia$RESPSEX)

table(Armenia$AGE)

table(Armenia$JOBSATFN)

##Legal skips are also the same for Job satisfaction as were above

##############################################

Armenia<-Armenia[Armenia$WORKSEC !="Legal skip",]

set.seed(123)

model_rf<-randomForest(fm, data=Armenia,nodesize=7,
                       
                       do.trace=T,importance=T,ntree=2000)

varImpPlot(model_rf, type=1)

table(Armenia$COMPABL)

table(Armenia$EDUYRS)

### -3,-2,-1 stand for "Interviewer error", "Refuse to answer","Don't know" respectively

Armenia<-Armenia[Armenia$WORKSEC !="Refuse to answer",]

Armenia<-Armenia[Armenia$WORKSEC !="Don't know",]

Armenia<-Armenia[Armenia$WORKYRS !="-3",]

Armenia<-Armenia[Armenia$WORKYRS !="-2",]

Armenia<-Armenia[Armenia$WORKYRS !="-1",]

Armenia<-Armenia[Armenia$COMPABL !="Don't know",]

Armenia<-Armenia[Armenia$JOBDESC !="Refuse to answer",]

Armenia<-Armenia[Armenia$JOBDESC !="Interviewer error",]

Armenia<-Armenia[Armenia$JOBDESC !="Don't know",]

Armenia<-Armenia[Armenia$EDUYRS !="-3",]

fm_filtered<-formula(PERSINC~WORKSEC+EDUYRS+
                       
                       RESPSEX+JOBDESC+COMPABL+WORKYRS)

###we filtered out the most important variables

############################################

#Then split into Train (80% of Armenian observations) and Test (the remaining 20%)

set.seed(123)

trainindex<- createDataPartition(Armenia$PERSINC, p=0.8, list=F)

Train<-Armenia[trainindex,]

Test<-Armenia[-trainindex,]

############################################

#before we move forward it is worth looking at the distribution of the dependent variable

ggplot(data=Train, aes(x=PERSINC,fill=PERSINC)) + geom_bar()

#############################################
#####Tuning the parameters of the decision tree 
### See more here https://cran.r-project.org/web/packages/rpart/rpart.pdf

set.seed(123)

minsplit <- tune.rpart(fm_filtered, data=Train, minsplit=seq(10,100,10))

##minsplit - the minimum number of observations that must exist in a node in order for a split to be attempted

plot(minsplit, main="Tune rpart on minsplit")

minsplit$best.parameters

##minsplit=40

# maxdepth
set.seed(123)

maxdepth <- tune.rpart(fm_filtered, data = Train, maxdepth = 1:5)

###maxdepth - Set the maximum depth of any node of the final tree, with the root node counted

#as depth 0. Values greater than 30 rpart will give nonsense results on 32-bit machines

plot(maxdepth,main="Performance of rpart vs. maxdepth")

maxdepth$best.parameters

##maxdepth=1

###########################################################

###Running corss validation to get the best Complexity parameter

## See more here ftp://cran.r-project.org/pub/R/web/packages/caret/caret.pdf 

ctrl <- trainControl(method="repeatedcv", number=10, repeats=5)

###resampling method is repeated cross validation

## repeats -  the number of complete sets of folds to compute

#number - number resampling iterations

############

###cp - complexity parameter. Any split that does not decrease the overall lack of fit by

##a factor of cp is not attempted.The main role of this parameter is to save computing time 

#by pruning off splits that are obviously not worthwhile

grid <-  expand.grid(cp = c(0.002,0.005,0.01,0.015,0.02,0.03))

##specifying the complexity parameters for cross validation

set.seed(123)

cp_cv<-train(fm_filtered, 
             
             data = Train, 
             
             method = "rpart",
             
             trControl = ctrl,
             
             tuneGrid = grid)

plot(cp_cv$results[,1:2])
##method - a string specifying which classification or regression model to use

# in our case it is "rpart" which stands for "Recursive partitioning for classification,

#regression and survival trees"

##trControl - a list of values that define how this function acts. 

# we specified above in our control funciton

## tuneGrid - a data frame with possible tuning values (specified as well)

cp_cv$bestTune

### cp = 0.02

## getting the best tune for given complexity parameters

###############################################
##

### we set the tuned parameters and method="class" for rpart to solve a classification problem
set.seed(123)

fit_filtered<-rpart(fm_filtered, data=Train, method="class",
                    
                    minsplit= 100, cp= 0.02, maxdepth=1)

pred_class_filtered<-predict(fit_filtered, Test, type="class")

confusionMatrix(pred_class_filtered, Test$PERSINC)

##Accuracy : 0.7611

#### it automatically took "High" as the positive case

####The model has a Specificity of 0.94186 

#####that is it does a good job at predicting "Low" income

###The model is doing bad job at predicting "High" income (Sensitivity of)

#it might be caused my maxdepth (we had better change it)

set.seed(123)

fit_filtered_best<-rpart(fm_filtered, data=Train, method="class",
                         
                         minsplit= 40, cp= 0.02, maxdepth=5)

pred_class_filtered_best<-predict(fit_filtered_best, Test, type="class")

confusionMatrix(pred_class_filtered_best, Test$PERSINC)

##Accuracy : 0.7611

##Accuracy did not change; however now the model is way useful 

### Sensitivity went up by 0.18519

#######################

##Pruning is a technique in machine learning that reduces the size of decision trees by 

#removing sections of the tree that provide little power to classify instances. 

#Pruning reduces the complexity of the final classifier, and hence improves predictive accuracy 

#by the reduction of overfitting.

set.seed(123)

pfit<- prune(fit_filtered_best, cp=0.02)

pred_prune<-predict(pfit, Test, type="class")

confusionMatrix(pred_prune, Test$PERSINC)

asRules(pfit)
### calling the decision rules

###e.g. according to the rules if one's work sector is Agriculture; Hunting; Forestry,Manufacturing etc.
##then there is a 85% probability for one to have a low income

prp(pfit, tweak=1.2, extra=6, box.palette=c("#F8766D", "#00BFC4"),gap=0, type = 1, space = 0.5)
##plotting the decision tree with certain features
View(pred_prune_prob)
##############################################
pred_prune_prob<-predict(pfit, Test, type="prob")

table(Armenia$PERSINC)[2]/(table(Armenia$PERSINC)[1]+table(Armenia$PERSINC)[2])

### we may also want to look at ROC curve

#receiver operating characteristic (ROC), or ROC curve, is a graphical plot that

#illustrates the performance of a binary classifier system 

#as its discrimination threshold is varied

###we must build an ROC prediction object based on predicted probabilities 

### we take "Low" as the class of interest

roc_pred<-prediction(pred_prune_prob[,2],Test$PERSINC)

perf<-performance(roc_pred, "tpr","fpr")

### we plot it based on the true positive rate and false positive rate

### the color spectrum helps us in deteremining the effects for different tresholds

####the alternative measure for accuracy is Area Under the Curve

##### The closer is AUC to 1, the better is our model doing 

plot(perf, colorize=T)

### when auc is 0.5, the model is useless 

### We can see that is does a good job in predicting the "Low" income

performance(roc_pred,"auc")@y.values

###our auc is 0.6511628

###### Finally let's compute the mean of probabilities of "High" class

mean(pred_prune_prob[,1])

### 0.2312364

###################################################################
#                   **END OF THE CODE**                           #
###################################################################