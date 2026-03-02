rm(list =ls())
setwd("D:/MACS_30100/MLproject")
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggcorrplot)
library(randomForest)
library(glmnet)
library(pROC)
library(caret)

df <- read.csv("df.csv")
df <- df %>% select(-X)

# EDA
summary(df)

## MID by year
mid_by_year <- df %>%
  group_by(year) %>%
  summarise(mid_count = sum(mid, na.rm = TRUE))

ggplot(mid_by_year, aes(x = year, y = mid_count)) +
  geom_area(fill = "#E69F00", alpha = 0.2) +
  geom_line(color = "#F0E442", size = 1) +
  geom_point(color = "#D55E00", size = 1.5, alpha = 0.6) +
  scale_x_continuous(breaks = seq(1940, 2020, by = 10),
                     minor_breaks = seq(1946, 2014, by = 1),
                     limits = c(1946, 2014),
                     expand = c(0.02, 0)) +
  theme_minimal(base_size = 14) +
  labs(title = "Events of MID by year",
    x = "Year", y = "MID count",) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"))

## MID by conttype
mid_by_con <- df %>%
  mutate(distance_grp = case_when(
    conttype == 1 ~ "1: Land Border",
    conttype == 2 ~ "2: 12mi Sea",
    conttype == 3 ~ "3: 24mi Sea",
    conttype == 4 ~ "4: 150mi Sea",
    conttype == 5 ~ "5: 400mi Sea",
    is.na(conttype) ~ "6: Non-Contiguous",
    TRUE ~ as.character(conttype)
  )) %>%
  group_by(year, distance_grp) %>%
  summarise(mid_count = sum(mid, na.rm = TRUE), .groups = "drop")

ggplot(mid_by_con, aes(x = year, y = mid_count, color = distance_grp, fill = distance_grp)) +
  geom_area(alpha = 0.3, show.legend = FALSE) +
  geom_line(size = 0.8) +
  facet_wrap(~distance_grp, scales = "free_y", ncol = 2) + 
  scale_x_continuous(breaks = seq(1940, 2020, by = 10)) +
  scale_color_viridis_d(option = "plasma") +
  scale_fill_viridis_d(option = "plasma") +
  theme_minimal(base_size = 12) +
  labs(title = "Events of MID by contiguity types",
       x = "Year", y = "MID count") +
  theme(
    strip.background = element_rect(fill = "#999999", color = NA),
    strip.text = element_text(face = "bold"),
    panel.spacing = unit(1, "lines"),
    legend.position = "none"
  )

## MID by joint democracy
mid_by_dem <- df %>%
  filter(!is.na(joint_dem)) %>%
  mutate(Democracy = ifelse(joint_dem == 1, "Joint Democracy", "Non-Joint Democracy")) %>%
  group_by(year, Democracy) %>%
  summarise(mid_rate = mean(mid, na.rm = TRUE), .groups = "drop")

ggplot(mid_by_dem, aes(x = year, y = mid_rate, color = Democracy, fill = Democracy)) +
  geom_area(alpha = 0.15, position = "identity") +
  geom_line(size = 1.2) +
  scale_x_continuous(breaks = seq(1940, 2020, by = 10), limits = c(1946, 2014)) +
  scale_color_manual(values = c("Joint Democracy" = "#0072B2", "Non-Joint Democracy" = "#E69F00")) +
  scale_fill_manual(values = c("Joint Democracy" = "#0072B2", "Non-Joint Democracy" = "#E69F00")) +
  theme_minimal(base_size = 14) +
  labs(title = "MID by Joint Democracy",
    x = "Year", y = "MID Frequency") +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 16),
        panel.grid.minor = element_blank())

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
               "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


## correlation matrix
corr_matrix <- cor(df %>% select(mid, conttype, joint_dem, ally, nmc_ratio, smoothtotrade, trdspike, tradedip), 
                   use = "complete.obs")

ggcorrplot(corr_matrix, 
           hc.order = TRUE,
           type = "lower",
           lab = TRUE,
           lab_size = 3,
           colors = c("#E15759", "white", "#56B4E9"), 
           title = "Correlation Matrix",
           ggtheme = theme_minimal())



## hierarchical clustering
df_cluster <- df %>%
  select(mid, conttype, joint_dem, ally, nmc_ratio, smoothtotrade, trdspike, tradedip) %>%
  mutate(conttype = ifelse(is.na(conttype), 6, conttype)) %>%
  na.omit()

df_scaled <- scale(df_cluster)
d_feat <- dist(t(df_scaled))
hc_feat <- hclust(d_feat, method = "complete")
plot(hc_feat)


# Models
df$logtrade <- log(df$smoothtotrade + 1)
train <- df %>% filter(year <= 1990)
test  <- df %>% filter(year > 1990)
xvar <- c("mid", "conttype", "joint_dem", "ally", "nmc_ratio", "logtrade", "trdspike", "tradedip")
train <- train[complete.cases(train[, xvar]), xvar]
test <- test[complete.cases(test[, xvar]), ]

## 1) baseline: logit
model1 <- glm(mid ~ conttype + joint_dem + ally + nmc_ratio + logtrade + trdspike + tradedip,
              data = train, family = binomial(link = "logit"))
prob1 <- predict(model1, newdata = test, type = "response")

## 2) random forest
model2 <-randomForest(as.factor(mid) ~ conttype + joint_dem + ally + nmc_ratio + logtrade + trdspike + tradedip,
                      importance = T, proximity = F, ntree = 1000, mtry = 3, data = train)
prob2 <- predict(model2, newdata = test, type = "prob")[, 2]
importance(model2)
varImpPlot(model2)

## 3) lasso
y <- train$mid
x <- model.matrix(mid ~ conttype + joint_dem + ally +nmc_ratio + logtrade + trdspike + tradedip, data = train)[, -1]
model3 <- cv.glmnet(x, y, family = "binomial", alpha = 1, nfolds = 10)
plot(model3)
x_test <- model.matrix(mid ~ conttype + joint_dem + ally + nmc_ratio + logtrade + trdspike + tradedip, data = test)[, -1]
prob3 <- predict(model3, newx = x_test, s = "lambda.min", type = "response")


# Model Evaluation and Validation
## ROC + AUC
roc1 <- roc(test$mid, prob1)
roc2 <- roc(test$mid, prob2)
roc3 <- roc(test$mid, as.numeric(prob3))
auc1 <- auc(roc1)
auc2 <- auc(roc2)
auc3 <- auc(roc3)

# comparison
plot(roc1, col = "#E15759", lwd = 2, lty = 1, main = "ROC comparison on test data")
lines(roc2, col = "#E69F00", lwd = 2, lty = 3)
lines(roc3, col = "#56B4E9", lwd = 2, lty = 2)
legend("bottomright", 
       legend = c(paste0("Logit (AUC=", round(auc1,3),")"),
                  paste0("Random Forest (AUC=", round(auc2,3),")"),
                  paste0("Lasso (AUC=", round(auc3,3),")")),
       col = c("#E15759", "#E69F00", "#56B4E9"), lwd = 2)


# validation
# 10-fold cross-validation
ctrl <- trainControl(method = "cv", number = 10, 
                     classProbs = TRUE, 
                     summaryFunction = twoClassSummary)

train_cv <- train
train_cv$mid_factor <- factor(ifelse(train_cv$mid == 1, "Yes", "No"))

# 1) Logit CV
cv_logit <- train(mid_factor ~ conttype + joint_dem + ally + nmc_ratio + logtrade + trdspike + tradedip, 
                  data = train_cv, method = "glm", family = "binomial",
                  trControl = ctrl, metric = "ROC")

# 2) Random Forest CV
cv_rf <- train(mid_factor ~ conttype + joint_dem + ally + nmc_ratio + logtrade + trdspike + tradedip, 
               data = train_cv, method = "rf", 
               ntree = 500, tuneGrid = expand.grid(mtry = c(2,3,4)),
               trControl = ctrl, metric = "ROC")

results <- resamples(list(Logit = cv_logit, RandomForest = cv_rf))
summary(results)
dotplot(results)


# Preliminary Findings
## Variable Importance (RF)
importance_rf <- varImp(cv_rf, scale = FALSE)
plot_importance <- function(importance_data, title_name) {
  df_imp <- data.frame(Variable = row.names(importance_data$importance),
                       Importance = importance_data$importance$Overall)
  
  ggplot(df_imp, aes(x = reorder(Variable, Importance), y = Importance)) +
    geom_bar(stat = "identity", fill = "#56B4E9", width = 0.7) +
    coord_flip() +
    theme_minimal() +
    labs(title = title_name, x = "Variables", y = "Importance Score")
}

plot_importance(importance_rf, "Variable Importance (Random Forest)")

## Confusion Matrix
threshold <- 0.5
pred1_class <- factor(ifelse(prob1 > threshold, "Yes", "No"), levels = c("Yes", "No"))
pred2_class <- factor(ifelse(prob2 > threshold, "Yes", "No"), levels = c("Yes", "No"))
true_class <- factor(ifelse(test$mid == 1, "Yes", "No"), levels = c("Yes", "No"))
cm1 <- confusionMatrix(pred1_class, true_class)
cm2 <- confusionMatrix(pred2_class, true_class)

cm_comparison <- data.frame(
  Metric = c("Accuracy", "Sensitivity (Recall)", "Specificity", "Precision"),
  Logit = c(cm1$overall['Accuracy'], cm1$byClass['Sensitivity'], cm1$byClass['Specificity'], cm1$byClass['Precision']),
  RandomForest = c(cm2$overall['Accuracy'], cm2$byClass['Sensitivity'], cm2$byClass['Specificity'], cm2$byClass['Precision'])
)
print(cm_comparison)

## Probability Distribution
dist_df <- data.frame(
  Actual = factor(test$mid, labels = c("No Conflict", "Conflict")),
  Prob_RF = prob2,
  Prob_Logit = prob1
)

ggplot(dist_df, aes(x = Prob_RF, fill = Actual)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("#999999", "#E69F00")) +
  theme_classic() +
  labs(title = "Distribution of Predicted Probabilities (Random Forest)",
       x = "Predicted Probability of MID",
       y = "Density")


## Lasso Coefficient
lasso_coefs <- as.matrix(coef(model3, s = "lambda.min"))
lasso_df <- data.frame(Variable = rownames(lasso_coefs), 
                       Coefficient = lasso_coefs[,1]) %>%
  filter(Variable != "(Intercept)" & Coefficient != 0)

ggplot(lasso_df, aes(x = reorder(Variable, Coefficient), y = Coefficient)) +
  geom_segment(aes(xend = Variable, yend = 0), color = "grey") +
  geom_point(size = 4, color = "#E15759") +
  coord_flip() +
  theme_bw() +
  labs(title = "Selected Features & Coefficients (Lasso)", x = "", y = "Coefficient Value")


































