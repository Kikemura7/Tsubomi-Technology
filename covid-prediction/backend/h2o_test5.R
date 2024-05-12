library(tidyverse)
library(readxl)
library(h2o)
library(plyr)
library(ggplot2)
library(ggpubr)

# start H2O cluster
h2o.init()
################################################################################
####################Load data###########################################################
# data <- read.csv("/Users/kenjiikemura/OneDrive - Montefiore Medicine/Research/COVID_Projects/COVID_ML/NewData/FinalData/8_3_2020/Downloaded_8_3_2020_organized_race_age_copy_admin_to_death.csv", header = TRUE)
data <- read.csv("/Users/kenjiikemura/Desktop/Downloaded_8_3_2020_organized_race_age_copy_admin_to_death.csv", header = TRUE)
dim(data) #how many data and how many parameters including one to classify.
str(data)
table(data$death)  #number of 1 and 0 from death
# head(data) #Display first 6 rows of data.
data1<-data
#############################################################
##Test set from 4/12-4/26(not included)
# data <- read.csv("/Users/kenjiikemura/OneDrive - Montefiore Medicine/Research/COVID_Reyes/NewData/CovidPosInpatientAfter4_12_2020(2).csv", header = TRUE)
# data1<-data
# test_future <- read.csv("/Users/kenjiikemura/OneDrive - Montefiore Medicine/Research/COVID_Projects/COVID_ML/NewData/CovidPosInpatientAfter4_12_2020_LimitedParam.csv", header = TRUE)
# test_future <- read.csv("/Users/kenjiikemura/OneDrive - Montefiore Medicine/Research/COVID_Projects/COVID_ML/NewData/FinalData/Group5.csv", header = TRUE)
# test_future <- read.csv("/Users/kenjiikemura/OneDrive - Montefiore Medicine/Research/COVID_Projects/COVID_ML/NewData/FinalData/7_31_2020/Downloaded_7_31_2020_May_July.csv", header = TRUE)
# dim(test_future)
# test_future$death<- as.factor(test_future$death)
# test_future_h<-as.h2o(test_future)
################################################################################
########delete certain column we are not going to use (CLG)#######. 
##Biomarkers
## albumin, cr, egfr, eosinophil, fibrinogen, hgb, inr, lymphocyte, 
# neutrophil, NLratio, platelet, protein, ptt, interleukin6, 
# ferritin, ddimer, ALT, AST, wbc, calcium, ldh, mcv, 
# procalcitonin, rdw, troponin, crp, CT_value. 

##Top 10 parameters to use###
# data1<-subset(data1,select=-diastolicBP)
# data1<-subset(data1,select=-systolicBP)
# data1<-subset(data1,select=-age)
# data1<-subset(data1,select=-bun)
# data1<-subset(data1,select=-ldh)
# data1<-subset(data1,select=-pulseOx)
# data1<-subset(data1,select=-ddimer)
# data1<-subset(data1,select=-troponin)
# data1<-subset(data1,select=-rr)
# data1<-subset(data1,select=-charlson_score)

# data1<-subset(data1,select=-albumin)
# data1<-subset(data1,select=-egfr)
# data1<-subset(data1,select=-pro_bnp)
# data1<-subset(data1,select=-glucose)
# 
# ###############
# #### Parameters to delete when you want to only use the top 10 parameters###
# data1<-subset(data1,select=-creatine_kinase)
# data1<-subset(data1,select=-cr)
# data1<-subset(data1,select=-platelet)
# data1<-subset(data1,select=-alt)
# data1<-subset(data1,select=-ast)
# data1<-subset(data1,select=-monocyte)
# data1<-subset(data1,select=-mpv)
# data1<-subset(data1,select=-temperature)
# data1<-subset(data1,select=-hgb)
# data1<-subset(data1,select=-calcium)
# data1<-subset(data1,select=-mcv)
# data1<-subset(data1,select=-ct_value)
# data1<-subset(data1,select=-eosinophil)
# data1<-subset(data1,select=-fibrinogen)
# data1<-subset(data1,select=-inr)
# data1<-subset(data1,select=-lymphocyte)
# data1<-subset(data1,select=-neutrophil)
# data1<-subset(data1,select=-protein)
# data1<-subset(data1,select=-ptt)
# data1<-subset(data1,select=-interleukin6)
# data1<-subset(data1,select=-wbc)
# data1<-subset(data1,select=-procalcitonin)
# data1<-subset(data1,select=-rdw)
# data1<-subset(data1,select=-crp)
# data1<-subset(data1,select=-chloride)
# data1<-subset(data1,select=-ferritin)
# data1<-subset(data1,select=-NLratio)
# data1<-subset(data1,select=-direct_bili)
# data1<-subset(data1,select=-total_bili)
# data1<-subset(data1,select=-potassium)
# 
# # #
# # # # #
# # # # #####Clinical findings parameter.
# data1<-subset(data1,select=-pulse)
# data1<-subset(data1,select=-bmi)
# data1<-subset(data1,select=-gender)
# data1<-subset(data1,select=-race)

###################################################################
############Parameters to always delete##########
data1<-subset(data1,select=-GroupNo)
data1<-subset(data1,select=-PatientID)
data1<-subset(data1,select=-MRN)
data1<-subset(data1,select=-admin_to_death)
data1<-subset(data1,select=-death_time)
# data1<-subset(data1,select=-Patient.Name)
# data1<-subset(data1,select=-Date.Of.Birth )
# data1<-subset(data1,select=-ethnicity)
# data1<-subset(data1,select=-city)
# data1<-subset(data1,select=-Patient.Address)
# data1<-subset(data1,select=-Zip.Code.Set)
# data1<-subset(data1,select=-Ventilator.Count)
# data1<-subset(data1,select=-Charlson_with_age)


#########################################################

##Others (Outcomes you are looking for)
data1<-subset(data1,select=-ventilator)
data1<-subset(data1,select=-EventDateTime)
# data1<-subset(data1,select=-EventDateTime_value)
# data1<-subset(data1,select=-event_to_death)
# data1<-subset(data1,select=-death_time)
# data1<-subset(data1,select=-death_or_vent)
# data1<-subset(data1,select=-death)

data1<-subset(data1,select=-tnf)
data1<-subset(data1,select=-indirect_bili)
############################################################
dim(data1) #how many data and how many parameters including one to classify.

############################How many NAs###############################################
p <- function(x){sum(is.na(x))/length(x)*100} #Percent NA in each column
k<-apply(data1,2,p)
cbind(k)
#################################################################
### If you want to Omit NA values, run this code ###
# data_no_na<- na.omit(data1) # Omit cases with NA.
# data1<-data_no_na
# dim(data_no_na)
################################################################################
#############State which parameters are factorial###############################

data1$death<- as.factor(data1$death) ## Necessary.

# data1$ventilator<- as.factor(data1$ventilator)

# data1$death_or_vent<- as.factor(data1$death_or_vent)

# data1$gender_M0F1<-as.factor(data1$gender_M0F1) #R seem to automatically found gender and race to be factorial.

# is.na(data1)<-sapply(data1, is.infinite) ## Replace infinite value to NA. 

data1_h<-as.h2o(data1) #Convert data1 to H2O format.

################################################################################
#####Let's checkout what our data looks like (average, median, std, missing values)#####
h2o.describe(data1_h)
format(x, scientific = FALSE) #take out exponential expression.
cbind(k) #%missing from each value
################################################################################

data1_h["death"]<-h2o.relevel(data1_h["death"],y="0") #Define the reference level.
##Reference level in this case is those who servived. You want to see the difference between dead people in reference to survivers. 
# data1_h["ventilator"]<-h2o.relevel(data1_h["ventilator"],y="0")
# data1_h["death_or_vent"]<-h2o.relevel(data1_h["death_or_vent"],y="0")
# data1_h["Expired"]<-h2o.relevel(data1_h["Expired"],y="Yes") #Relevel so the confusion matrix is corretly made.
################################################################################

splits <- h2o.splitFrame(data = data1_h, 
                         ratios = c(0.8),  #partition data into 70%, 15%, 15% chunks
                         seed =3)  #setting a seed will guarantee reproducibility
#For publication seed 3

train <- splits[[1]]
# valid <- splits[[2]]
test <- splits[[2]]
################################################################################
##If you need to bring h2o frame back to R frame##
train_R<-as.data.frame (train)
test_R<-as.data.frame(test)
################################################################################
####checkout what our data looks like (average, median, std, missing values)###
h2o.describe(train)
h2o.describe(test)
################################################################################

folds<-10

# identify the response column
y <- "death"
# y<- "ventilator"
# y<- "death_or_vent"
# y <- "Expired"

# identify the predictor columns (remove columns you do not want)
# x <- setdiff(names(train), c(y))
x <- setdiff(names(train), c(y, "admin_to_death"))
#x <- setdiff(names(train), c(y, "ferritinIni")) #remove "y" and  "ferritinIni" from calculation.

########################Execute AutoML#################################
aml <- h2o.automl(
  y=y,
  x=x,
  training_frame = train,
  # validation_frame = test,
  # sort_metric = c("AUTO"),
  # exclude_algos=c("DRF", "GLM", "XGBoost", "GBM", "StackedEnsemble"),
  exclude_algos=c("DeepLearning"),
  # include_algos = c("DeepLearning"),
  # max_model = 3,
  max_model = 20, ##Number of models you like to generate
  nfolds=folds,
  # max_runtime_secs = 180,
  # sort_metric=c("AUCPR"),
  sort_metric=c("AUC"),
  leaderboard_frame=test, # Choose which set for the leaderboard to evaluate on.
  seed =3)
#For publication seed 3
########################################################################

# The leader model is stored at 'aml@leader' and the leaderboard is stored at 'aml@leaderboard'.
lb <- h2o.get_leaderboard(object = aml)
# lb <- h2o.get_leaderboard(object = aml, extra_columns = 'ALL')
print(lb, n=50)

exa<-h2o.explain(aml, train) #explainability (SHAP, PDP, etc)
exa

# Ensemble Exploration ---
# To understand how the ensemble works, let's take a peek inside the Stakced Ensemble "ALL Models"
# The "All Models" ensemble is an ensemble of all of the individual models in the AutoML run
# This is often the top peroforming model on the leaderboard.

#Get model ids for all models in the AutoML leaderboard
model_ids<-as.data.frame(aml@leaderboard$model_id)[,1] #all rows and first column
## Get the "All Models" Stacked Ensemble model
# se <- h2o.getModel(grep("StackedEnsemble_AllModels", model_ids, value = TRUE)[1])
se <- h2o.getModel(grep("StackedEnsemble_All", model_ids, value = TRUE)[1])

###Get the Stacked Ensemble metalearner model
metalearner <- h2o.getModel(se@model$metalearner$name)

## Examine the variable importance of the metalearner (combiner) algorithm in the ensemble.
## THis shows us how much each learner is contributing to the ensemble. The AutoML stacked Ensembles
## Use the default metalearner algorithm (GLM with non-negative weights), so the variable importance of the
## metalearner is actually the standardized coefficient magnitude of the GLM.

h2o.varimp(metalearner)

## We can also plot the base learner contribution to the ensemble.
# h2o.varimp_plot(metalearner)

#######Variable importance#################
best_model_var10<- h2o.getModel(grep("StackedEnsemble", model_ids, value= TRUE)[1]) #Put the model you are interested in seeing.
best_model1<- h2o.getModel(grep("GLM", model_ids, value= TRUE)[1])
best_model2<- h2o.getModel(grep("XGB", model_ids, value= TRUE)[1])

h2o.varimp(best_model1)
h2o.varimp_plot(best_model1)
df <- as.data.frame(h2o.varimp(best_model1))
print(df)
###########Test on Test set###########################
perf_10 <- h2o.performance(best_model_var10,test)
perf_10
plot(perf_48, type="roc",col = "red") #Plot ROC
abline(a=0, b=1)

perf1 <- h2o.performance(best_model1,test)
perf1

h2o.confusionMatrix(object = best_model_var10, test, threshold = 0.20)

############compare model_var_48 and model_var_10: You must run both version and save model###################

best_model_48<- h2o.getModel(grep("StackedEnsemble", model_ids, value= TRUE)[1])
perf_48 <- h2o.performance(best_model_48,test)

best_model_var10<- h2o.getModel(grep("StackedEnsemble", model_ids, value= TRUE)[1])
perf_10 <- h2o.performance(best_model_var10,test)

metrics <- as.data.frame(h2o.metric(perf_48)) # Plot PRROC
metrics2 <- as.data.frame(h2o.metric(perf_10))

ggplot(metrics,aes(recall,precision))+geom_line(color = "blue")+
  geom_line(data = metrics2,aes(recall,precision), color = "red") #Overlap two plots

##############Test future data#######################
perf3 <- h2o.performance(best_model,test_future_h)
perf3
plot(perf2, type="roc")
abline(a=0, b=1)
h2o.confusionMatrix(object = best_model, test_future_h, threshold = 0.12)

#### Build the standardized coefficient magnitudes plot:#####Warning: model must be a GLM########
GLM_model<- h2o.getModel(grep("GLM", model_ids, value= TRUE)[1])
h2o.std_coef_plot(GLM_model,num_of_features=50) #Warning: model must be a GLM
ken(GLM_model,num_of_features=50)
########################Test single Data###################
test_k<-data.frame(systolicBP = 90,
                   diastolicBP=60,
                   age=50,
                   bun=7,
                   ldh=140,
                   pulseOx=98,
                   glucose=100,
                   rr=10,
                   troponin=0.5,
                   ddimer=1.5
                   )
test_kh<-as.h2o(test_k)
perf3 <- h2o.predict(best_model,test_kh)
perf3

########## Save your model of choice######################
aml1 <- h2o.getModel(aml@leaderboard[1, 1]) #Chose your model of choice.
modelfile <- h2o.download_mojo(aml1, path="/Users/kenjiikemura/downloads", get_genmodel_jar=TRUE)

# modelfile <- h2o.download_pojo(aml1, path="/Users/kenjiikemura/Downloads/MOJO_models/")

#################Import MOJO file################
mojo_model_stack_47 <- h2o.upload_mojo( "/Users/kenjiikemura/OneDrive - Montefiore Medicine/Research/COVID_Projects/COVID_ML/NewData/FinalData/8_3_2020/MOJO_8_3_2020/8_15_2020/47_var/StackedEnsemble_AllModels_AutoML_20200815_172950.zip")
mojo_model_GBM <- h2o.upload_mojo( "/Users/kenjiikemura/GBM.zip")

h2o.predict(mojo_model_GBM, test_kh)
# h2o.partialPlot(object = best_model, data = test, cols = c("age"))
# h2o.partialPlot(object = best_model, data = test, cols = c("bpsystolic"))

######Print all variable and its means (study of variables) #######
##If you need to bring h2o fram back to R frame##
train_R<-as.data.frame(train)
test_R<-as.data.frame(test)

sink("varibales.txt") #save the following output.
d.summary.extended <- data1 %>%
  psych::describe(quant=c(.25,.75)) %>%
  as_tibble(rownames="rowname")  %>%
  print(d.summary.extended, n=50)
sink()

p <- function(x){sum(is.na(x))/length(x)*100} #Percent NA in each column
k<-apply(test_R,2,p)
cbind(k)

table(data1$death)  #number of 1 and 0 from death_or_vent
table(train_R$death)
table(test_R$death)
