setwd("/users/ayan/Desktop/BU/Spring 2023/CS555_project")

df <- read.csv("project_data.csv")

fraud_subset <- subset(df, isFraud == 1)
not_fraud_subset <- subset(df, isFraud == 0)

library(dplyr)
set.seed(17)
srs_fraud <- sample_n(fraud_subset, 500)
srs_not_fraud <- sample_n(not_fraud_subset, 500)

srs_df <- rbind(srs_fraud, srs_not_fraud)

relevant_cols <- c("type", "amount", "oldbalanceOrg", "newbalanceOrig", "oldbalanceDest", "newbalanceDest", "isFraud")
srs_df_filtered <- srs_df[relevant_cols]

srs_df_filtered$type <- as.factor(srs_df_filtered$type)

table(srs_fraud$type)
table(srs_not_fraud$type)

#log reg models
log_reg_model <- glm(srs_df_filtered$isFraud ~ srs_df_filtered$type, family = binomial)
summary(log_reg_model)

log_reg_model <- glm(srs_df_filtered$isFraud ~ srs_df_filtered$amount, family = binomial)
summary(log_reg_model)

predictions <- predict(log_reg_model, type = "response")
library(pROC)
roc(srs_df_filtered$isFraud, predictions)

library(Metrics)
Metrics::recall(actual = srs_df_filtered$isFraud, predicted = predictions)
Metrics::precision(actual = srs_df_filtered$isFraud, predicted = predictions)

log_reg_model <- glm(srs_df_filtered$isFraud ~ srs_df_filtered$amount, family = binomial)
summary(log_reg_model)

#t test of means
t.test(srs_fraud$amount, srs_not_fraud$amount, alternative = "two.sided", var.equal = FALSE)

#
table(srs_fraud$MFA.enabled)
table(srs_not_fraud$MFA.enabled)

table(srs_fraud$isFraudPrevented)
table(srs_not_fraud$isFraudPrevented)

table(srs_fraud$isFlaggedFraud)
table(srs_not_fraud$isFlaggedFraud)

# anova (of types)
summary(aov(amount ~ type, data = srs_df_filtered))


