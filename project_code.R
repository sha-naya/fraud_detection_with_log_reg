setwd("/users/ayan/Desktop/BU/Spring 2023/CS555_project")

df <- read.csv("project_data.csv")

fraud_subset <- subset(df, isFraud == 1)
not_fraud_subset <- subset(df, isFraud == 0)

library(dplyr)
set.seed(17)
srs_fraud <- sample_n(fraud_subset, 500)
srs_not_fraud <- sample_n(not_fraud_subset, 500)

srs_df <- rbind(srs_fraud, srs_not_fraud)

relevant_cols <- c("type", "amount", "isFraud")
srs_df_filtered <- srs_df[relevant_cols]

srs_df_filtered$type <- as.factor(srs_df_filtered$type)
srs_df_filtered$isFraud <- as.factor(srs_df_filtered$isFraud)

table(srs_fraud$type)
table(srs_not_fraud$type)

#log reg models
log_reg_model <- glm(srs_df_filtered$isFraud ~ srs_df_filtered$amount, family = binomial)
summary(log_reg_model)

log_reg_model <- glm(srs_df_filtered$isFraud ~ srs_df_filtered$amount + srs_df_filtered$type, family = binomial)
summary(log_reg_model)

predictions <- predict(log_reg_model, type = "response")
library(pROC)
roc(srs_df_filtered$isFraud, predictions)

srs_df_filtered$predictions <- predictions
srs_df_filtered$predictions <- ifelse(predictions>=0.5, 1,0)
srs_df_filtered$predictions <- as.factor(srs_df_filtered$predictions)

confusionMatrix(srs_df_filtered$predictions, srs_df_filtered$isFraud, mode = "everything")

### ANOVA (types column)
# normal distribution test
fit <- aov(amount ~ type, data = srs_df_filtered)
resid <- residuals(fit)
hist(resid, main="Residuals Distribution of ANOVA", xlab = "Amounts", col="#FFA500")
qqnorm(resid)
shapiro.test(resid)

## homogeneity of variance test
leveneTest(amount ~ type, data = srs_df_filtered)


# boxplot
boxplot(srs_fraud$amount,
        main = "Transaction amount (fraud) boxplot",
        xlab = "Unknown Currency(?)",
        ylab = "Transaction amount",
        col = "#6A0DAD",
        border = "black",
        horizontal = TRUE,
        notch = TRUE
)
boxplot(srs_not_fraud$amount,
        main = "Transaction amount (not fraud) boxplot",
        xlab = "Unknown Currency(?)",
        ylab = "Transaction amount",
        col = "#56A0D3",
        border = "black",
        horizontal = TRUE,
        notch = TRUE
)

library(pROC)
library(ggplot2)

ggroc(roc(srs_df_filtered$isFraud, predictions)) +
  theme_minimal() + 
  ggtitle("ROC curve") + 
  geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), color="#6495ED", linetype="dashed")

################################################################################
hist(srs_fraud$amount, main="Distribution of Fraudulent Transactions Amounts", xlab = "Amounts", col="#800000")
shapiro.test(srs_fraud$amount)
hist(srs_not_fraud$amount, main="Distribution of Normal Transactions Amounts", xlab = "Amounts", col="#DC143C")
shapiro.test(srs_not_fraud$amount)

log_fraud_amount <- log(srs_fraud$amount)
hist(log_fraud_amount, main="Log Distribution of Fraudulent Transactions Amounts", xlab = "Amounts", col="#008000")
mean(log_fraud_amount)
shapiro.test(log_fraud_amount)

log_not_fraud_amount <- log(srs_not_fraud$amount)
hist(log_not_fraud_amount, main="Log Distribution of Normal Transactions Amounts", xlab = "Amounts", col="#7FFF00")
mean(log_not_fraud_amount)
shapiro.test(log_not_fraud_amount)

################################################################################
library(caret)
ctrl <- trainControl(method = "repeatedcv", 
                     number = 10, 
                     repeats = 5, 
                     verboseIter = FALSE)
ada_grid <- expand.grid(iter = 5, maxdepth = 1:5, nu = seq(0.1, 0.5, by=0.1))
ada_model <- train(isFraud ~ amount,
                      data = srs_df_filtered,
                      method = "ada",
                      preProcess = c("scale", "center"),
                      trControl = ctrl,
                      tuneGrid = ada_grid
                      )
summary(ada_model)
ada_predictions <- predict(ada_model)
confusionMatrix(ada_predictions, srs_df_filtered$isFraud, mode = "everything")
