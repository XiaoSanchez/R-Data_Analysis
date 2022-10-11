suppressPackageStartupMessages(c(library(caret),library(corrplot),library(smotefamily)))
#A. Load the dataset (a proportion of the original data)
creditcardFraud <- read.csv("creditcardFraud.csv")
#B. Change class to factor the as.factor function encodes the vector as a factor or category
creditcardFraud$class <- as.factor(creditcardFraud$class)

#A. Structure of the dataset
str(creditcardFraud)
#B. Missing data?
sum(is.na(creditcardFraud)) #no missing values found
#C. Check the imbalance in the dataset
summary(creditcardFraud$class)
prop.table(table(creditcardFraud$class)) #The dataset is extremely imbalanced
#D. Compile histograms for each variable
par(mfrow = c(3,5))
i <- 1
for (i in 1:30)
{hist((creditcardFraud[,i]), main = paste("Distibution of ", colnames(creditcardFraud[i])), xlab = colnames(creditcardFraud[i]), col = "light blue")
}
#E. Compute the correlations among the variables
r <- cor(creditcardFraud[,1:30])
corrplot(r, type = "lower", tl.col = 'black',  tl.srt = 15)

#A. Split data into training and testing dataset used for model building (training dataset)
set.seed(1337)
train<-createDataPartition(creditcardFraud$class,
                           p = .70, #% of data going to training
                           times = 1,#of partitions to create
                           list = F)

train.orig<-creditcardFraud[train,]
test<-creditcardFraud[-train,]

#B. Check the proportion of observations allocated to each group
dim(train.orig) / dim(creditcardFraud)
dim(test) / dim(creditcardFraud)
#C. Class balance for training dataset
prop.table(table(train.orig$class))
#D. Class balance for test dataset
prop.table(table(test$class))

#SMOTE Balanced
train.smote<-SMOTE(train.orig[,-31],train.orig[,31], K=5)
names(train.smote)
train.smote<-train.smote$data # Extract only the balanced dataset
train.smote$class<-as.factor(train.smote$class)

#ADASYN Balanced
train.adas<-ADAS(train.orig[,-31],train.orig[,31], K=5)
train.adas<-train.adas$data # Extract only the balanced dataset
train.adas$class<-as.factor(train.adas$class)

#Density based SMOTE
train.dbsmote<-DBSMOTE(train.orig[,-31],train.orig[,31])
train.dbsmote<-train.dbsmote$data # Extract only the balanced dataset
train.dbsmote$class<-as.factor(train.dbsmote$class)

#Class Distribution of SMOTE Balanced Dataset
prop.table(table(train.smote$class))
#Class Distribution of ADASYN Balanced Dataset
prop.table(table(train.adas$class))
#Class Distribution of DB SMOTE Balanced Dataset
prop.table(table(train.dbsmote$class))

#A. Global options that we will use across all of our trained models
ctrl <- trainControl(method = "cv",
                     number = 10,
                     classProbs = TRUE,
                     summaryFunction = twoClassSummary)

#B. Decision Tree: original data
dt_orig <- train(class ~ .,
                 data=train.orig,
                 method = "rpart",
                 trControl = ctrl,
                 metric="ROC")

#C. Naive Bayes regression: original data
nb_orig <- train(class ~ .,
                data =train.orig,
                method = "naive_bayes",
                trControl = ctrl,
                metric="ROC")

#D. Linear Discriminant Analysis: original data
lda_orig <- train(class ~ ., data =train.orig,
            method = "lda",
            trControl = ctrl,
            metric="ROC")

###################################################
#Decision Tree Model - Trained on original dataset#
###################################################
#A. Decision Tree Model predictions
dt_orig_pred<-predict(dt_orig, test, type="prob")

#B. Decision Tree - Assign class to probabilities
dt_orig_test<- factor(ifelse(dt_orig_pred$yes> 0.50, "yes", "no") )


#C. Decision Tree Save Precision/Recall/F
precision_dtOrig <- posPredValue(dt_orig_test, test$class, positive="yes")
recall_dtOrig <- sensitivity(dt_orig_test, test$class, positive="yes")
F1_dtOrig <- (2*precision_dtOrig * recall_dtOrig) / (precision_dtOrig + recall_dtOrig)

#################################################
#Naive Bayes Model - Trained on original dataset#
#################################################
#A. NB Model predictions
nb_orig_pred<-predict(nb_orig, test, type="prob")

#B. NB - Assign class to probabilities
nb_orig_test<- factor(ifelse(nb_orig_pred$yes> 0.50, "yes", "no") )


#C. NB Save Precision/Recall/F
precision_nbOrig <- posPredValue(nb_orig_test, test$class, positive="yes")
recall_nbOrig <- sensitivity(nb_orig_test, test$class, positive="yes")
F1_nbOrig <- (2 * precision_nbOrig * recall_nbOrig) / (precision_nbOrig + recall_nbOrig)

#########################################
#LDA Model - Trained on original dataset#
#########################################
#A. LDA Model predictions
lda_orig_pred<-predict(lda_orig,test, type="prob")

#B. LDA - Assign class to probabilities
lda_orig_test<- factor(ifelse(lda_orig_pred$yes> 0.50, "yes", "no") )


#C. LDA Save Precision/Recall/F
precision_ldaOrig <- posPredValue(lda_orig_test, test$class, positive="yes")
recall_ldaOrig <- sensitivity(lda_orig_test, test$class, positive="yes")
F1_ldaOrig <- (2 * precision_ldaOrig * recall_ldaOrig) / (precision_ldaOrig + recall_ldaOrig)

#A. Decision Tree: SMOTE data
dt_smote <- train(class ~ .,
                 data=train.smote,
                 method = "rpart",
                 trControl = ctrl,
                 metric="ROC" )

#B. Naive Bayes regression: SMOTE data
nb_smote <- train(class ~ ., data =train.smote,
            method = "naive_bayes",
            trControl = ctrl,
            metric = "ROC")

#C. Linear Discriminant Analysis: SMOTE data
lda_smote <- train(class ~ ., data =train.smote,
             method = "lda",
             trControl = ctrl,
             metric = "ROC")

################################################
#Decision Tree Model - Trained on SMOTE dataset#
################################################
#A. Decision Tree Model predictions
dt_smote_pred<-predict(dt_smote, test, type="prob")

#B. Decision Tree - Assign class to probabilities
dt_smote_test<- factor(ifelse(dt_smote_pred$yes> 0.50, "yes", "no") )


#C. Decision Save Precision/Recall/F
precision_dtsmote <- posPredValue(dt_smote_test, test$class, positive="yes")
recall_dtsmote <- sensitivity(dt_smote_test, test$class, positive="yes")
F1_dtsmote <- (2 * precision_dtsmote * recall_dtsmote) / (precision_dtsmote + recall_dtsmote)

##############################################
#Naive Bayes Model - Trained on SMOTE dataset#
##############################################
#A. NB Model predictions
nb_smote_pred<-predict(nb_smote,test, type="prob")

#B. NB - Assign class to probabilities
nb_smote_test<- factor( ifelse(nb_smote_pred$yes> 0.50, "yes", "no") )


#C. NB Save Precision/Recall/F
precision_nbsmote <- posPredValue(nb_smote_test, test$class, positive="yes")
recall_nbsmote <- sensitivity(nb_smote_test, test$class, positive="yes")
F1_nbsmote <- (2 * precision_nbsmote * recall_nbsmote) / (precision_nbsmote + recall_nbsmote)

######################################
#LDA Model - Trained on SMOTE dataset#
######################################
#A. LDA Model predictions
lda_smote_pred<-predict(lda_smote,test, type="prob")

#B. LDA - Assign class to probabilities
lda_smote_test<- factor(ifelse(lda_smote_pred$yes> 0.50, "yes", "no") )


#C. LDA Save Precision/Recall/F
precision_ldasmote <- posPredValue(lda_smote_test, test$class, positive="yes")
recall_ldasmote <- sensitivity(lda_smote_test, test$class, positive="yes")
F1_ldasmote <- (2 * precision_ldasmote * recall_ldasmote) / (precision_ldasmote + recall_ldasmote)

#A. Decision Tree: ADASYN data
dt_adas <- train(class ~ .,
                  data=train.adas,
                  method = "rpart",
                  trControl = ctrl,
                  metric="ROC" )

#B. Naive Bayes regression: ADASYN data
nb_adas <- train(class ~ ., data =train.adas,
            method = "naive_bayes",
            trControl = ctrl,
            metric = "ROC")

#C. Linear Discriminant Analysis: ADASYN data
lda_adas <- train(class ~ ., data =train.adas,
             method = "lda",
             trControl = ctrl,
             metric = "ROC")

#################################################
#Decision Tree Model - Trained on ADASYN dataset#
#################################################
#A. Decision Tree Model predictions
dt_adas_pred<-predict(dt_adas, test, type="prob")

#B. Decision Tree - Assign class to probabilities
dt_adas_test<- factor(ifelse(dt_adas_pred$yes> 0.50, "yes", "no") )


#C. Decision Save Precision/Recall/F
precision_dtadas <- posPredValue(dt_adas_test, test$class, positive="yes")
recall_dtadas <- sensitivity(dt_adas_test, test$class, positive="yes")
F1_dtadas <- (2 * precision_dtadas * recall_dtadas) / (precision_dtadas + recall_dtadas)

###############################################
#Naive Bayes Model - Trained on ADASYN dataset#
###############################################
#A. NB Model predictions
nb_adas_pred<-predict(nb_adas,test, type="prob")

#B. NB - Assign class to probabilities
nb_adas_test<- factor(ifelse(nb_adas_pred$yes> 0.50, "yes", "no") )


#C. NB Save Precision/Recall/F
precision_nbadas <- posPredValue(nb_adas_test, test$class, positive="yes")
recall_nbadas <- sensitivity(nb_adas_test, test$class, positive="yes")
F1_nbadas <- (2 * precision_nbadas * recall_nbadas) / (precision_nbadas + recall_nbadas)

#######################################
#LDA Model - Trained on ADASYN dataset#
#######################################
#A. LDA Model predictions
lda_adas_pred<-predict(lda_adas,test, type="prob")

#B. LDA - Assign class to probabilities
lda_adas_test<- factor(ifelse(lda_adas_pred$yes> 0.50, "yes", "no") )


#C. LDA Save Precision/Recall/F
precision_ldaadas <- posPredValue(lda_adas_test, test$class, positive="yes")
recall_ldaadas <- sensitivity(lda_adas_test, test$class, positive="yes")
F1_ldaadas <- (2 * precision_ldaadas * recall_ldaadas) / (precision_ldaadas + recall_ldaadas)

#A. Decision Tree: dbsmote data
dt_dbsmote <- train(class ~ .,
                 data=train.dbsmote,
                 method = "rpart",
                 trControl = ctrl,
                 metric="ROC" )

#B. Naive Bayes regression: dbsmote data
nb_dbsmote <- train(class ~ ., data =train.dbsmote,
            method = "naive_bayes",
            trControl = ctrl,
            metric = "ROC")

#C. Linear Discriminant Analysis: dbsmote data
lda_dbsmote <- train(class ~ ., data =train.dbsmote,
             method = "lda",
             trControl = ctrl,
             metric = "ROC")

###################################################
#Decision Tree Model - Trained on DB SMOTE dataset#
###################################################
#A. Decision Tree Model predictions
dt_dbsmote_pred<-predict(dt_dbsmote, test, type="prob")

#B. Decision Tree - Assign class to probabilities
dt_dbsmote_test<- factor(ifelse(dt_dbsmote_pred$yes> 0.50, "yes", "no") )


#C. Decision Save Precision/Recall/F
precision_dtdbsmote <- posPredValue(dt_dbsmote_test, test$class, positive="yes")
recall_dtdbsmote <- sensitivity(dt_dbsmote_test, test$class, positive="yes")
F1_dtdbsmote <- (2 * precision_dtdbsmote * recall_dtdbsmote) / (precision_dtdbsmote + recall_dtdbsmote)

#################################################
#Naive Bayes Model - Trained on DB SMOTE dataset#
#################################################
#A. NB Model predictions
nb_dbsmote_pred<-predict(nb_dbsmote,test, type="prob")

#B. NB - Assign class to probabilities
nb_dbsmote_test<- factor( ifelse(nb_dbsmote_pred$yes> 0.50, "yes", "no") )


#C. NB Save Precision/Recall/F
precision_nbdbsmote <- posPredValue(nb_dbsmote_test, test$class, positive="yes")
recall_nbdbsmote <- sensitivity(nb_dbsmote_test, test$class, positive="yes")
F1_nbdbsmote <- (2 * precision_nbdbsmote * recall_nbdbsmote) / (precision_nbdbsmote + recall_nbdbsmote)

#########################################
#LDA Model - Trained on DB SMOTE dataset#
#########################################
#A. LDA Model predictions
lda_dbsmote_pred<-predict(lda_dbsmote,test, type="prob")

#B. LDA - Assign class to probabilities
lda_dbsmote_test<- factor(ifelse(lda_dbsmote_pred$yes> 0.50, "yes", "no") )


#C. LDA Save Precision/Recall/F
precision_ldadbsmote <- posPredValue(lda_dbsmote_test, test$class, positive="yes")
recall_ldadbsmote <- sensitivity(lda_dbsmote_test, test$class, positive="yes")
F1_ldadbsmote <- (2 * precision_ldadbsmote * recall_ldadbsmote) / (precision_ldadbsmote + recall_ldadbsmote)

#Lets reset the chart settings so we see one chart at a time
par(mfrow = c(1,1))

#Compare the Recall of the models: TP / TP + FN. To do that, we'll need to combine our results into a dataframe [note update the names of the recall object if you used different names]
model_compare_recall <- data.frame(Model = c('DT-Orig',
                                      'NB-Orig',
                                      'LDA-Orig',
                                      'DT-SMOTE',
                                      'NB-SMOTE',
                                      'LDA-SMOTE',
                                      'DT-ADASYN',
                                      'NB-ADASYN',
                                      'LDA-ADASYN',
                                      'DT-DBSMOTE',
                                      'NB-DBSMOTE',
                                      'LDA-DBSMOTE' ),
                            Recall = c(recall_dtOrig,
                                   recall_nbOrig,
                                   recall_ldaOrig,
                                   recall_dtsmote,
                                   recall_nbsmote,
                                   recall_ldasmote,
                                   recall_dtadas,
                                   recall_nbadas,
                                   recall_ldaadas,
                                   recall_dtdbsmote,
                                   recall_nbdbsmote,
                                   recall_ldadbsmote))

ggplot(aes(x=reorder(Model,-Recall) , y=Recall), data=model_compare_recall) +
  geom_bar(stat='identity', fill = 'light blue') +
  ggtitle('Comparative Recall of Models on Test Data') +
  xlab('Models')  +
  ylab('Recall Measure')+
  geom_text(aes(label=round(Recall,2)))+
  theme(axis.text.x = element_text(angle = 40))

  #Compare the Precision of the models: TP/TP+FP [note update the names of the precision object if you used different names]
model_compare_precision <- data.frame(Model = c('DT-Orig',
                                      'NB-Orig',
                                      'LDA-Orig',
                                      'DT-SMOTE',
                                      'NB-SMOTE',
                                      'LDA-SMOTE',
                                      'DT-ADASYN',
                                      'NB-ADASYN',
                                      'LDA-ADASYN',
                                      'DT-DBSMOTE',
                                      'NB-DBSMOTE',
                                      'LDA-DBSMOTE' ),
                              Precision = c(precision_dtOrig,
                                         precision_nbOrig,
                                         precision_ldaOrig,
                                         precision_dtsmote,
                                         precision_nbsmote,
                                         precision_ldasmote,
                                         precision_dtadas,
                                         precision_nbadas,
                                         precision_ldaadas,
                                         precision_dtdbsmote,
                                         precision_nbdbsmote,
                                         precision_ldadbsmote))

ggplot(aes(x=reorder(Model,-Precision) , y=Precision), data=model_compare_precision) +
  geom_bar(stat='identity', fill = 'light green') +
  ggtitle('Comparative Precision of Models on Test Data') +
  xlab('Models')  +
  ylab('Precision Measure')+
  geom_text(aes(label=round(Precision,2)))+
  theme(axis.text.x = element_text(angle = 40))

  #Compare the F1 of the models: 2*((Precision*Recall) / (Precision + Recall)) [note update the names of the F1 object if you used different names]
model_compare_f1 <- data.frame(Model = c('DT-Orig',
                                      'NB-Orig',
                                      'LDA-Orig',
                                      'DT-SMOTE',
                                      'NB-SMOTE',
                                      'LDA-SMOTE',
                                      'DT-ADASYN',
                                      'NB-ADASYN',
                                      'LDA-ADASYN',
                                      'DT-DBSMOTE',
                                      'NB-DBSMOTE',
                                      'LDA-DBSMOTE' ),
                              F1 = c(F1_dtOrig,
                                         F1_nbOrig,
                                         F1_ldaOrig,
                                         F1_dtsmote,
                                         F1_nbsmote,
                                         F1_ldasmote,
                                         F1_dtadas,
                                         F1_nbadas,
                                         F1_ldaadas,
                                         F1_dtdbsmote,
                                         F1_nbdbsmote,
                                         F1_ldadbsmote))

ggplot(aes(x=reorder(Model,-F1) , y=F1), data=model_compare_f1) +
  geom_bar(stat='identity', fill = 'light grey') +
  ggtitle('Comparative F1 of Models on Test Data') +
  xlab('Models')  +
  ylab('F1 Measure')+
  geom_text(aes(label=round(F1,2)))+
  theme(axis.text.x = element_text(angle = 40))
