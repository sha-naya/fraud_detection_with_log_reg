setwd("/users/ayan/Desktop/BU/Spring 2023/CS555_project")

df <- read.csv("project_data.csv")

fraud_subset <- subset(df, isFraud == 1)
not_fraud_subset <- subset(df, isFraud == 0)

library(dplyr)
set.seed(17)
srs_fraud <- sample_n(fraud_subset, 500)
srs_not_fraud <- sample_n(not_fraud_subset, 500)

srs_df <- rbind(srs_fraud, srs_not_fraud)

