rm(list =ls())
setwd("D:/MACS_30100/MLproject")
library(dplyr)
library(tidymodels)
library(glmnet)
library(parsnip)
library(ggrepel)

df <- read.csv("df.csv")
set.seed(30100)

# separate data
df$logtrade <- log(df$smoothtotrade + 1)
train <- df %>% filter(year <= 2000)
test  <- df %>% filter(year > 2000)
xvar <- c("ccode1", "ccode2", "year", "mid", "conttype", "joint_dem", 
          "ally", "nmc_ratio", "logtrade", "trdspike", "tradedip",
          "gdp_ratio", "gdppp_ratio", "pop_ratio", "area_ratio", "fdi_ratio")
train <- train[complete.cases(train[, xvar]), xvar]
test <- test[complete.cases(test[, xvar]), xvar]
train$mid <- factor(train$mid, levels = c(0,1))
test$mid  <- factor(test$mid, levels = c(0,1))


################## Model ###################
### 1. Selection and Justification: Identify the models you've used, preferably those covered in class. 
### Explain the choice of models based on their relevance to your task.
### 2. Parameter Tuning: Discuss any parameters you've tuned and the rationale behind your choices. 
### Show the tuning process and its impact on model performance.
### 3. Performance Evaluation: Use appropriate metrics to assess model performance. 
### After covering evaluation techniques in class, provide a multi-perspective analysis and explain the results.
################## Result Analysis ########################
### 1. Performance Interpretation: Offer interpretations of the model's performance, 
### such as visualizations for tree models or significant features for linear models.
### 2. Error Analysis: Present examples where the model fails and conduct an error analysis to hypothesize why these errors occur. 
### Suggest possible improvements.
### 3. Comparative Analysis: If using multiple models, thoroughly explore and compare their performance, 
### highlighting each model's strengths and weaknesses.

# lasso
y <- train$mid
x <- model.matrix(mid ~ conttype + joint_dem + ally + nmc_ratio + 
                    logtrade + trdspike + tradedip + gdp_ratio + 
                    gdppp_ratio + pop_ratio + area_ratio + fdi_ratio,
                  data = train)[, -1]
lasso <- cv.glmnet(x, y, family = "binomial", alpha = 1, nfolds = 10)
plot(lasso)
lasso.coef <- coef(lasso, s = "lambda.min")
lasso.coef

selected_features <- rownames(lasso.coef)[lasso.coef[,1] != 0]
selected_features <- selected_features[selected_features != "(Intercept)"]



# recipe
data_recipe <- recipe(mid ~ conttype + joint_dem + ally + nmc_ratio + 
                        logtrade + trdspike + tradedip + gdp_ratio + 
                        gdppp_ratio + pop_ratio + area_ratio + fdi_ratio, 
                      data = train) %>%
  step_scale(all_predictors()) %>%  # make sd=1
  step_center(all_predictors())     # make mean=0

# Model Specfications
# logit
model1 <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification")

# random forest
model2 <- rand_forest(trees = 100) %>% 
  set_engine("randomForest") %>%
  set_mode("classification")

# svm
model3 <- svm_rbf() %>%
  set_engine("kernlab") %>%
  set_mode("classification")

# xgboost
model4 <- boost_tree(trees = 100, mtry = min(3, length(selected_features))) %>%
  set_mode("classification") %>%
  set_engine("xgboost")


# workflow
wf <- workflow_set(preproc = list(recipe = data_recipe),
                   models = list(logit = model1,
                                 rf = model2,
                                 svm = model3,
                                 xgboost = model4))


cv_folds <- vfold_cv(train, v = 10)

wf_results <- wf %>% 
  workflow_map(resamples = cv_folds,
               metrics = metric_set(roc_auc, accuracy))


autoplot(wf_results,
         rank_metric = "accuracy",
         metric = c("roc_auc","accuracy") ) +
  theme(legend.position = "none") +
  geom_text_repel(aes(y = mean +1.95*std_err, label = model), hjust=0, vjust=1) 


## collect metrics
results <- wf_results %>% collect_metrics()

# 1. Select the best model
best_model <- results %>% 
  filter(.metric == "accuracy") %>%
  arrange(desc(mean)) %>%
  slice(1) %>%
  pull(wflow_id)

cat('best model is: ',best_model, '\n') 

# extract the best model
best_workflow <- wf_results %>% extract_workflow(id = best_model)

# 2. Refit the best model on the full training data
wf_best_fit <- best_workflow %>% fit(data = train)
summary(wf_best_fit)

# 3. Make predictions on the test data
pred <- wf_best_fit %>% 
  predict(new_data = test, type = "class") %>% 
  bind_cols(test)

# 4. Visualize the predictions
conf_mat <- pred %>% 
  conf_mat(truth = mid, estimate = .pred_class)

print(conf_mat)

autoplot(conf_mat)
plot(table(pred$mid, pred$.pred_class),
     col = 'steelblue3',
     main = 'Confusion Matrix',
     xlab = 'True Class',
     ylab = 'Predicted Class')


pred.prob <- wf_best_fit %>%
  predict(new_data = test , type = "prob") %>%
  bind_cols(test)

# AUC
the_roc_auc <- roc_auc(pred.prob, truth = mid, .pred_1)

# ROC curve
pred.prob %>% roc_curve(mid, .pred_1) %>% 
  autoplot()+ ggtitle(paste(the_roc_auc, collapse = ", ") )


