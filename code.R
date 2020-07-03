library(ISLR) #data source
library(ggplot2)
library(tidyverse)
library(missForest)
library(hydroGOF)

set.seed(42)

#Assigning the dataset to the variable data
data <- Credit%>%
  select(-ID)
str(data)
summary(data)
names(data)

#Lists of numerical and categorical variables
numeric_vars <- c("Income", "Limit", "Rating", "Cards", "Age", "Education", "Balance"  )
cat_vars <- c("Gender", "Student", "Married", "Ethnicity")

#Analysis - numerical variables
data_num <- data %>%
  select(numeric_vars)%>%
  gather(id, value)

#Boxplot of all numeric variables at once
data_num %>%
  ggplot(aes(y=value))+
  facet_grid(. ~ id) +
  geom_boxplot(width=0.7, fill="forestgreen")+
  labs(y="Value")+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


#Individual boxplots
data_t <- data
data_t$title <- "Income"
data_t %>%
  ggplot(aes(y=Income))+
  geom_boxplot(width=0.7, fill="forestgreen")+
  labs(y="")+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  facet_grid(. ~ title)

data_cat <- data %>%
  select(cat_vars)#%>%
#gather(id, value)

data_cat$title <- "Ethnicity"
data_cat %>%
  ggplot(aes(x=Ethnicity))+
  geom_bar(fill="forestgreen")+
  labs(y="", x="")+
  theme_bw()+
  facet_grid(. ~ title)

#Creating the dataset where the missing values will be imputed
data_nas_imp <- data

#Generate 10% missing values at Random 
#using function prodNA of library missForest
set.seed(42)
data_nas <- prodNA(data, noNA = 0.1)
summary(data_nas)

#Producing missing data manually
data_man_nas <- data
for (i in 1:ncol(data)){
  na_indexes <- sample(1:nrow(data),0.1*nrow(data),replace=F)
  data_man_nas[na_indexes,i] <- NA
}
summary(data_man_nas)

####Imputation with MICE package####
library(mice)
imputed_Data <- mice(data_nas)
summary(imputed_Data)
completeData <- mice::complete(imputed_Data,2)
(mice_err <- missForest::mixError(completeData, data_nas, data))

mice_sim_results <- NA
mice_sim_res <- data.frame(matrix(0, ncol = 2, nrow = 3))


#MRSE for the numeric variables 
differences <- completeData[ , numeric_vars]-data[ , numeric_vars]
sqrt(sum(sapply(differences, function(x) sum(x^2)/sum(x!=0))))

#NRMSE - this one will be used in the simulations
missForest::nrmse(completeData[ , numeric_vars], data_nas[ , numeric_vars], data[ , numeric_vars])

#Wrong matches for the categorical variables
sum(completeData[ , cat_vars]!=data[ , cat_vars])

#Individual boxplots for MICE
data_t_mice <- completeData
data_t_mice$title <- "Income - MICE"
data_t_mice %>%
  ggplot(aes(y=Income))+
  geom_boxplot(width=0.7, fill="orange1")+
  labs(y="")+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  facet_grid(. ~ title)

#Simulation
for (i in 1:50){
  data_nas_temp <- prodNA(data, noNA = 0.1)
  imputed_Data <- mice(data_nas_temp)
  completeData <- mice::complete(imputed_Data,2)
  mice_sim_results[i] <- missForest::nrmse(completeData[ , numeric_vars], data_nas_temp[ , numeric_vars], data[ , numeric_vars])
}

for (i in 1:5){
  imputed_Data <- mice(data_nas)
  completeData <- mice::complete(imputed_Data,2)
  mice_sim_results[i] <- missForest::mixError(completeData[ , cat_vars], data_nas[ , cat_vars], data[ , cat_vars])
}

#all errors together
for (i in 1:50){
  data_nas_temp <- prodNA(data, noNA = 0.1)
  imputed_Data <- mice(data_nas)
  completeData <- mice::complete(imputed_Data,2)
  mice_sim_res[i,1] <- missForest::mixError(completeData, data_nas, data)[1]
  mice_sim_res[i,2] <- missForest::mixError(completeData, data_nas, data)[2]
}
names(mice_sim_res) <- c("NRMSE","PFC")

write.csv(mice_sim_res, "mice_sim_res.csv")

####Imputation with missForest####

#Imputation with missForest
data_imp_mf <- missForest(data_nas)

#check imputed values
data_imp_mf$ximp

#comparing actual data accuracy
me <- mixError(data_imp_mf$ximp, data_nas, data)
me

#Individual boxplots for MICE
data_t_mf <- data_imp_mf$ximp
data_t_mf$title <- "Income - MF"
data_t_mf %>%
  ggplot(aes(y=Income))+
  geom_boxplot(width=0.7, fill="springgreen4")+
  labs(y="")+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  facet_grid(. ~ title)

#simulation
mf_sim_res <- data.frame(matrix(0, ncol = 2, nrow = 3))
for (i in 1:3){
  data_imp_mf <- missForest(data_nas)
  mf_sim_res[i,1] <- missForest::mixError(data_imp_mf$ximp, data_nas, data)[1]
  mf_sim_res[i,2] <- missForest::mixError(data_imp_mf$ximp, data_nas, data)[2]
}

eth_graphs <- data.frame(Original = data$Ethnicity,
                         MICE = completeData$Ethnicity,
                         MF = data_imp_mf$ximp$Ethnicity)

eth<-eth_graphs %>%
  gather(var, value)%>%
  group_by(var, value) %>%
  summarise(number = n())

cols <- c("forestgreen", "orange1", "darkgoldenrod1")

ggplot(eth,aes(x=value, y=number, fill=var))+
  geom_bar(stat="identity", position="dodge")+
  scale_fill_manual(values=cols)+
  labs(y="",x="", fill="Data")+
  theme_bw()

#Final Simulation
#Creating empty data frame for the results
final_results <- data.frame(matrix(0, ncol = 4, nrow = 50))
names(final_results) <- c("NRMSE_MF", "PFC_MF", "NRMSE_MICE","PFC_MICE")
final_results$id <- seq_len(nrow(final_results))
#Loop for simulation
for (i in 1:50){
  #Creating dataset with missing values
  data_nas <- prodNA(data, noNA = 0.1)
  #Imputation with missForest
  data_imp_mf <- missForest(data_nas)
  #Imputation with MICE
  imputed_data_mice <- mice(data_nas)
  data_imp_mice <- mice::complete(imputed_data_mice,2)
  #Filling the results matrix
  final_results[i,1] <- missForest::mixError(data_imp_mf$ximp, data_nas, data)[1]
  final_results[i,2] <- missForest::mixError(data_imp_mf$ximp, data_nas, data)[2]
  final_results[i,3] <- missForest::mixError(data_imp_mice, data_nas, data)[1]
  final_results[i,4] <- missForest::mixError(data_imp_mice, data_nas, data)[2]
}

write.csv(final_results, "final_results.csv")


cols <- c("forestgreen", "orange1", "darkgoldenrod1")

ggplot(final_results, aes(id)) + 
  geom_line(aes(y = NRMSE_MF,  col = "NRMSE_MF")) + 
  geom_line(aes(y = NRMSE_MICE, col = "NRMSE_MICE"))+
  scale_color_manual(values=cols)+
  labs(col="Variable", y = "Accuracy measurement", x="ID", title="Normalized Root Mean Square Error")+
  theme_bw()

#Graph for final results
ggplot(final_results, aes(id)) + 
  geom_line(aes(y = PFC_MF,  col = "PFC_MF")) + 
  geom_line(aes(y = PFC_MICE, col = "PFC_MICE"))+
  scale_color_manual(values=cols)+
  labs(col="Variable", y = "Accuracy measurement", x="ID", title="Proportion of Falsely Classified entries")+
  theme_bw()

final_results <- read.csv("final_results.csv")

final_results$title <- "NRMSE - MICE"
final_results %>%
  ggplot(aes(y=NRMSE_MICE))+
  geom_boxplot(width=0.7, fill="forestgreen")+
  labs(y="")+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  facet_grid(. ~ title)
