#'install needed packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(BSDA)) install.packages("BSDA", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")

#'load needed libraries
library(tidyverse)
library(caret)
library(ggplot2)
library(dbplyr)
library(readr)
library(gridExtra)
library(BSDA)
library(randomForest)

#'download file
dl <- "cardio_train.csv"
download.file("https://raw.githubusercontent.com/nyrvelli/CVD/main/cardio_train.csv", dl)
cardio <- read_csv("cardio_train.csv")

#' https://www.kaggle.com/datasets/sulianova/cardiovascular-disease-dataset/code

#'exploring the dataset
head(cardio) %>% knitr::kable()
nrow(cardio) %>% knitr::kable()

#'convert age from days to years
cardio$age<- round(cardio$age/365.25, digits=2) 
summary(cardio$age)

#'remove weird values
summary(cardio$height) 
hist(cardio$height)
cardio<- cardio %>% filter(height >=140 & height <= 210)

#'remove weird values
summary(cardio$weight) 
hist(cardio$weight)
cardio<- cardio %>% filter(weight >=40)

#' remove weird values, incompatible with life
summary(cardio$ap_hi)  
hist(cardio$ap_hi)
cardio<- cardio %>% filter(ap_hi >=50 & ap_hi <= 240)

#' remove weird values, incompatible with life
summary(cardio$ap_lo) 
hist(cardio$ap_lo)
cardio<- cardio %>% filter(ap_lo >=40 & ap_lo <= 140)

nrow(cardio) %>% knitr::kable()
#'we have 68555 rows left


#'create an exploration copy of the dataset
cardio_plot <- cardio

#' gender_map={1:'female',2:'male'}; convert gender to factors
cardio_plot$gender <- factor(cardio_plot$gender, 
                        levels=c(1,2), 
                        labels=c("female","male"))

#' cholesterol_map = {1: 'normal', 2: 'above normal', 3: 'well above normal'}, convert ro factors
cardio_plot$cholesterol <- factor(cardio_plot$cholesterol, 
                            levels=c(1,2,3), 
                            labels=c("normal","high", "very high"))

#' glucose_map={1: 'normal', 2: 'above average', 3: 'well above normal'}, convert ro factors
cardio_plot$gluc <- factor(cardio_plot$gluc, 
                      levels=c(1,2,3), 
                      labels=c("normal","high", "very high"))

#' df.smoke.unique() ['no', 'yes'] 0 = no, 1= yes, convert ro factors
cardio_plot$smoke <- factor(cardio_plot$smoke, 
                      levels=c(0,1), 
                      labels=c("non-smoker","smoker"))


#'df.alco.unique() ['no', 'yes'], convert ro factors
cardio_plot$alco <- factor(cardio_plot$alco, 
                      levels=c(0,1), 
                      labels=c("non-drinker","drinker"))


#' active_map={0:'no',1:'yes'}, convert ro factors
cardio_plot$active <- factor(cardio_plot$active, 
                       levels=c(0,1), 
                       labels=c("inactive","active"))

#'cvd_map={0:'no',1:'yes'}, convert ro factors
cardio_plot$cardio <- factor(cardio_plot$cardio, 
                        levels=c(0,1), 
                        labels=c("no CVD","CVD"))

#'view exploration dataset
head(cardio_plot) %>% knitr::kable()

#' EXPLORING THE DATASET

#' make boxplot of age vs CVD state
age_box <- cardio_plot %>% ggplot(aes(cardio, age, fill = cardio)) + geom_boxplot() +
  scale_fill_manual(values = c("#00BFC4", "#F8766D")) +
  labs(y = "Age")+
  theme(axis.title.x = element_blank()) +
  theme(legend.position="none")

#' test for significant differences in age between CVD states, using z-score
nonCVD <- cardio_plot %>% filter(cardio == "no CVD") 
CVD <- cardio_plot %>% filter(cardio == "CVD") 
sd_nonCVD_age <- sd(nonCVD$age)
sd_CVD_age <- sd(CVD$age)
nonCVD_age <- nonCVD %>% select(age)
CVD_age <- CVD %>% select(age)
ztest_age<- z.test(x= nonCVD_age, y= CVD_age, sigma.x= sd_nonCVD_age, sigma.y= sd_CVD_age)
ztest_results <- tibble(variable = "Age", p = ztest_age$p.value)

#' make boxplot of height vs CVD state
height_box <- cardio_plot %>% ggplot(aes(cardio, height, fill = cardio)) + geom_boxplot() +
  scale_fill_manual(values = c("#00BFC4", "#F8766D")) +
  labs(y = "Height")+
  theme(axis.title.x = element_blank())+
  theme(legend.position="none")

#' test for significant differences in height between CVD states, using z-score
sd_nonCVD_height <- sd(nonCVD$height)
sd_CVD_height <- sd(CVD$height)
nonCVD_height <- nonCVD %>% select(height)
CVD_height <- CVD %>% select(height)
ztest_height<- z.test(x= nonCVD_height, y= CVD_height, sigma.x= sd_nonCVD_height, sigma.y= sd_CVD_height)
ztest_results <- bind_rows(ztest_results,
                          tibble(variable = "Height", p = ztest_height$p.value))

#' make boxplot of weight vs CVD state
weight_box <- cardio_plot %>% ggplot(aes(cardio, weight, fill = cardio)) + geom_boxplot() +
  scale_fill_manual(values = c("#00BFC4", "#F8766D")) +
  labs(y = "Weight")+
  theme(axis.title.x = element_blank())+
  theme(legend.position="none")

#' test for significant differences in weight between CVD states, using z-score
sd_nonCVD_weight <- sd(nonCVD$weight)
sd_CVD_weight <- sd(CVD$weight)
nonCVD_weight <- nonCVD %>% select(weight)
CVD_weight <- CVD %>% select(weight)
ztest_weight<- z.test(x= nonCVD_weight, y= CVD_weight, sigma.x= sd_nonCVD_weight, sigma.y= sd_CVD_weight)
ztest_results <- bind_rows(ztest_results,
                           tibble(variable = "Weight", p = ztest_weight$p.value))

#' make boxplot of systolic blood pressure vs CVD state  
syst_box <- cardio_plot %>% ggplot(aes(cardio, ap_hi, fill = cardio)) + geom_boxplot() +
  scale_fill_manual(values = c("#00BFC4", "#F8766D")) +
  labs(y = "Blood Pressure (Systolic)")+
  theme(axis.title.x = element_blank())+
  theme(legend.position="none")

#' test for significant differences in systolic blood pressure between CVD states, using z-score
sd_nonCVD_sys <- sd(nonCVD$ap_hi)
sd_CVD_sys <- sd(CVD$ap_hi)
nonCVD_sys <- nonCVD %>% select(ap_hi)
CVD_sys <- CVD %>% select(ap_hi)
ztest_sys<- z.test(x= nonCVD_sys, y= CVD_sys, sigma.x= sd_nonCVD_sys, sigma.y= sd_CVD_sys)
ztest_results <- bind_rows(ztest_results,
                           tibble(variable = "Systolic Blood Pressure", p = ztest_sys$p.value))

#' make boxplot of diastolic blood pressure vs CVD state
diast_box<- cardio_plot %>% ggplot(aes(cardio, ap_lo, fill = cardio)) + geom_boxplot() +
  scale_fill_manual(values = c("#00BFC4", "#F8766D")) +
  labs(y = "Blood Pressure (Diastolic)")+
  theme(axis.title.x = element_blank())+
  theme(legend.position="none")

#' test for significant differences in diatolic blood pressure between CVD states, using z-score
sd_nonCVD_dia <- sd(nonCVD$ap_lo)
sd_CVD_dia <- sd(CVD$ap_lo)
nonCVD_dia <- nonCVD %>% select(ap_lo)
CVD_dia <- CVD %>% select(ap_lo)
ztest_dia<- z.test(x= nonCVD_dia, y= CVD_dia, sigma.x= sd_nonCVD_dia, sigma.y= sd_CVD_dia)
ztest_results <- bind_rows(ztest_results,
                           tibble(variable = "Diastolic Blood Pressure", p = ztest_dia$p.value))

#'create plot
grid.arrange(age_box, height_box, weight_box, syst_box, diast_box,nrow = 2)

#'print z-test results
ztest_results %>% knitr::kable()

#'arrange the order of factors
cardio_plot$cardio <- factor(cardio_plot$cardio, levels = c("no CVD", "CVD")) 

#' make a barplot for gender vs CVD state
gend <- ggplot(cardio_plot,aes(x=factor(cardio),fill=factor(gender))) +
  geom_bar(position = position_fill(reverse = TRUE)) +
  scale_fill_manual(values = c("#00BFC4", "#F8766D")) +
  ylab("Gender") +
  scale_y_continuous(labels=scales::percent_format()) +
  theme(axis.title.x = element_blank())

#' test for significant differences in gender between CVD states, using chi-squared test
test_gender <- chisq.test(table(cardio_plot$gender, cardio_plot$cardio))
test_gender #p-value = 0.0634
chi_squared_results <- tibble(variable = "Gender", p = test_gender$p.value)

#' make a barplot for cholesterol vs CVD state
chole <- ggplot(cardio_plot,aes(x=factor(cardio),fill=factor(cholesterol))) +
  geom_bar(position = position_fill(reverse = TRUE)) +
  scale_fill_manual(values = c("#00BFC4", "lightgoldenrod2", "#F8766D")) +
  ylab("Cholesterol") +
  scale_y_continuous(labels=scales::percent_format()) +
  theme(axis.title.x = element_blank())

#' test for significant differences in cholesterol between CVD states, using chi-squared test
test_chole <- chisq.test(table(cardio_plot$cholesterol, cardio_plot$cardio))
test_chole #p-value < 2.2e-16
chi_squared_results <-  bind_rows(chi_squared_results,
                         tibble(variable = "Cholesterol", p = test_chole$p.value))

#' make a barplot for blood sugar vs CVD state
glucose <- ggplot(cardio_plot,aes(x=factor(cardio),fill=factor(gluc))) +
  geom_bar(position = position_fill(reverse = TRUE)) +
  scale_fill_manual(values = c("#00BFC4", "lightgoldenrod2", "#F8766D")) +
  ylab("Blood Sugar") +
  scale_y_continuous(labels=scales::percent_format()) +
  theme(axis.title.x = element_blank())

#' test for significant differences in blood sugar between CVD states, using chi-squared test
test_glucose <- chisq.test(table(cardio_plot$gluc, cardio_plot$cardio))
test_glucose #p-value < 2.2e-16
chi_squared_results <-  bind_rows(chi_squared_results,
                                  tibble(variable = "Blood Sugar", p = test_glucose$p.value))

#' make a barplot for smoking vs CVD state
smoking <- ggplot(cardio_plot,aes(x=factor(cardio),fill=factor(smoke))) +
  geom_bar(position = position_fill(reverse = TRUE)) +
  scale_fill_manual(values = c("#00BFC4", "#F8766D")) +
  ylab("Smoking") +
  scale_y_continuous(labels=scales::percent_format()) +
  theme(axis.title.x = element_blank())

#' test for significant differences in smoking between CVD states, using chi-squared test
test_smoking <- chisq.test(table(cardio_plot$smoke, cardio_plot$cardio))
test_smoking #p-value = 2.036e-05
chi_squared_results <-  bind_rows(chi_squared_results,
                                  tibble(variable = "Smoking", p = test_smoking$p.value))

#'make a barplot for alcohol vs CVD state
alcohol <- ggplot(cardio_plot,aes(x=factor(cardio),fill=factor(alco))) +
  geom_bar(position = position_fill(reverse = TRUE)) +
  scale_fill_manual(values = c("#00BFC4", "#F8766D")) +
  ylab("Alcohol") +
  scale_y_continuous(labels=scales::percent_format()) +
  theme(axis.title.x = element_blank())

#' test for significant differences in alcohol between CVD states, using chi-squared test
test_alcohol <- chisq.test(table(cardio_plot$alco, cardio_plot$cardio))
test_alcohol #p-value = 0.02943
chi_squared_results <-  bind_rows(chi_squared_results,
                                  tibble(variable = "Alcohol", p = test_alcohol$p.value))

#'make a barplot for physical activity vs CVD state
active <- ggplot(cardio_plot,aes(x=factor(cardio),fill=factor(active))) +
  geom_bar(position="fill") +
  scale_fill_manual(values = c("#F8766D", "#00BFC4")) +
  ylab("Physical Activity") +
  scale_y_continuous(labels=scales::percent_format()) +
  theme(axis.title.x = element_blank())

#' test for significant differences in physical activity between CVD states, using chi-squared test
test_active <- chisq.test(table(cardio_plot$active, cardio_plot$cardio))
test_active #p-value < 2.2e-16
chi_squared_results <-  bind_rows(chi_squared_results,
                                  tibble(variable = "Physical Activity", p = test_active$p.value))

#'create plot
grid.arrange(gend, chole, glucose, smoking, alcohol, active, nrow = 2)

#'print chi-squared test results
chi_squared_results %>% knitr::kable()

#'check smoking plot again
smoking

#'remove smoking from dataset
cardio <- cardio %>% select(-smoke)

#' MODELS

#'partition dataset into train and test set 80:20
set.seed(2, sample.kind = "Rounding")
test_index <- createDataPartition(cardio$id, times = 1, p = 0.2, list = FALSE)
train_set <- cardio %>% slice(-test_index)
test_set <- cardio %>% slice(test_index)

#'separate predictor and variables
y<- train_set$cardio
x<- select(train_set, -cardio, -id)
y_test<- test_set$cardio
x_test <- select(test_set, -cardio, -id)

#'GLM

#'train a glm
train_glm <- train(x,as.factor(y), method = "glm")
glm_accuracy <- confusionMatrix(predict(train_glm,x_test, type = "raw"), as.factor(y_test))$overall["Accuracy"]

#'save and print results
cvd_results <- tibble(method = "GLM", Accuracy = glm_accuracy)
cvd_results %>% knitr::kable()

#'KNN

#'train a knn
fit<- knn3(x,as.factor(y))
y_hat_knn <- predict(fit, x_test, type = "class")
knn_accuracy <- mean(y_hat_knn == y_test)

#'save and print results
cvd_results <- bind_rows(cvd_results,
                          tibble(method="KNN",  
                                 Accuracy = knn_accuracy))
cvd_results %>% knitr::kable()

#' The accuracy of the k-NN algorithm can be severely degraded by the presence of noisy or irrelevant features, or if the feature scales are not consistent with their importance.

#'train knn without alcohol, height
x2 <- x%>% select(-gender, alco, -height)
x2_test <- x_test %>% select(-gender, -alco, -height)
fit<- knn3(x2,as.factor(y))
y_hat_knn <- predict(fit, x2_test, type = "class")
knn_2_accuracy <- mean(y_hat_knn == y_test)

#'save and print results
cvd_results <- bind_rows(cvd_results,
                         tibble(method="KNN-2",  
                                Accuracy = knn_2_accuracy))
cvd_results %>% knitr::kable()

#'Random Forest

#'build a random forest model
train_rf<-randomForest(as.factor(y)~., data = x)
rf_accuracy<- confusionMatrix(predict(train_rf,x_test), as.factor(y_test))$overall["Accuracy"]

#'save and print results
cvd_results <- bind_rows(cvd_results,
                         tibble(method="RF",  
                                Accuracy = rf_accuracy))
cvd_results %>% knitr::kable()





