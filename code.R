rm(list =ls())

## set your exact directory
setwd("D:/MACS_30100/MLproject")

## library required packages 
library(countrycode)
library(dplyr)
library(factoextra)
library(ggcorrplot)
library(ggplot2)
library(ggrepel)
library(glmnet)
library(haven)
library(parsnip)
library(pdp)
library(ranger)
library(readxl)
library(tidymodels)
library(vip)
library(WDI)

#############################################
############ 1. Data Setup ##################
#############################################
mid <- read_dta("dyadic_mid_4.03.dta")
dyad <- read.csv("dyads.csv")
con <- read_dta("contdird.dta")
polity <- read_xls("p5v2018.xls")
alliance <- read_dta("alliance_v4.1_by_dyad_yearly.dta")
nmc <- read_dta("NMC-60-abridged.dta")
trade <- read.csv("Dyadic_COW_4.0.csv")

# construct dyad-year for every pair in 1946-2014
dyad <- dyad[dyad$year >= 1946, ]
dyad_pairs <- unique(dyad[, c("ccode1", "ccode2")])
extension <- 2009:2014
dyad_extend <- merge(dyad_pairs, data.frame(year = extension))
dyad_all <- rbind(dyad, dyad_extend)
df <- dyad_all

# MID
mid <- mid %>% rename(ccode1 = statea, ccode2 = stateb)
mid <- mid[mid$year >= 1946, ]
mid$mid <- 1
df <- df %>% 
  left_join(mid %>% select(year, ccode1, ccode2, mid), by = c("year", "ccode1", "ccode2"))
df$mid[is.na(df$mid)] <- 0

# contiguity
con <- con[con$year >= 1946 & con$year <= 2014, ]
con <- con %>% rename(ccode1 = state1no, ccode2 = state2no)
df <- df %>% 
  left_join(con %>% select(year, ccode1, ccode2, conttype), by = c("year", "ccode1", "ccode2"))
df$con_dummy <- ifelse(is.na(df$conttype), 0, 1)

# joint democracy
polity <- polity[polity$year >= 1946 & polity$year <= 2014, ]
df <- df %>%
  left_join(polity %>% select(ccode, year, polity2), by = c("year", "ccode1" = "ccode"))
df <- df %>% rename(polity2a = polity2)
df <- df %>%
  left_join(polity %>% select(ccode, year, polity2), by = c("year", "ccode2" = "ccode"))
df <- df %>% rename(polity2b = polity2)
df$joint_dem <- ifelse(df$polity2a >= 6 & df$polity2b >=6, 1, 0)

# alliance
alliance <- alliance[alliance$year >= 1946, ]
alliance$ally <- 1
df <- df %>%
  left_join(alliance %>% select(year, ccode1, ccode2, ally), by = c("year", "ccode1", "ccode2"))
df$ally[df$year <= 2012 & is.na(df$ally)] <- 0

# National Material Capabilities (nmc)
nmc <- nmc[nmc$year >= 1946 & nmc$year <= 2014, ]
df <- df %>%
  left_join(nmc %>% select(ccode, year, cinc), by = c("year", "ccode1" = "ccode"))
df <- df %>% rename(cinc1 = cinc)
df <- df %>%
  left_join(nmc %>% select(ccode, year, cinc), by = c("year", "ccode2" = "ccode"))
df <- df %>% rename(cinc2 = cinc)
df$nmc_ratio <- pmin(df$cinc1, df$cinc2) / pmax(df$cinc1, df$cinc2)

# trade
df <- df %>% left_join(
  trade %>% 
    mutate(smoothtotrade = ifelse(smoothtotrade < 0, NA, smoothtotrade)) %>%
    select(year, ccode1, ccode2, smoothtotrade, trdspike, tradedip), 
  by = c("year", "ccode1", "ccode2"))
df$logtrade <- log(df$smoothtotrade + 1)

# WDI
wdi <- as.data.frame(WDI(country = "all", 
                         indicator = c("gdp" = "NY.GDP.MKTP.KD", 
                                       "gdppp" = "NY.GDP.PCAP.KD", 
                                       "pop" = "SP.POP.TOTL", 
                                       "area" = "AG.LND.TOTL.K2",
                                       "fdi" = "BX.KLT.DINV.CD.WD"),
                         start = 1960, end = 2014))
wdi$ccode <- countrycode(wdi$iso3c, origin = "iso3c", destination = "cown")
df <- df %>%
  left_join(wdi %>% select(ccode, year, gdp, gdppp, pop, area, fdi),
            by = c("year", "ccode1" = "ccode")) %>%
  rename(gdp1 = gdp, gdppp1 = gdppp, pop1 = pop, area1 = area, fdi1 = fdi)
df <- df %>%
  left_join(wdi %>% select(ccode, year, gdp, gdppp, pop, area, fdi),
            by = c("year", "ccode2" = "ccode")) %>%
  rename(gdp2 = gdp, gdppp2 = gdppp, pop2 = pop, area2 = area, fdi2 = fdi)

df$gdp_ratio <- log(df$gdp1 / df$gdp2)
df$gdppp_ratio <- log(df$gdppp1 / df$gdppp2)
df$pop_ratio <- log(df$pop1 / df$pop2)
df$area_ratio <- log(df$area1 / df$area2)
df$fdi_ratio <- log((df$fdi1+1) / (df$fdi2+1))


#############################################
################# 2. EDA ####################
#############################################

# basic description
summary(df)
table(df$mid)
table(df$conttype)
table(df$joint_dem)
table(df$ally)
hist(df$nmc_ratio)
hist(df$logtrade)
df %>% group_by(mid) %>%
  summarise(joint_dem = mean(joint_dem, na.rm = TRUE),
            ally = mean(ally, na.rm = TRUE),
            nmc_ratio = mean(nmc_ratio, na.rm = TRUE),
            trade = mean(logtrade, na.rm = TRUE))

# rare event comparison
ggplot(df, aes(mid)) +
  geom_bar(fill = "#CC79A7") +
  labs(title = "Distribution of MID Onset",
       x = "MID Onset",
       y = "Count") +
  theme_minimal()

## visualization: MID by year
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

## visualization: MID by conttype
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

## visualization: MID by joint democracy
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
corr_matrix <- cor(df %>% select(mid, conttype, joint_dem, ally, 
                                 nmc_ratio, logtrade, trdspike, 
                                 tradedip, gdp_ratio, gdppp_ratio, 
                                 area_ratio, pop_ratio, fdi_ratio), 
                   use = "complete.obs")

ggcorrplot(corr_matrix, 
           hc.order = TRUE,
           type = "lower",
           lab = TRUE,
           lab_size = 3,
           colors = c("#E15759", "white", "#56B4E9"), 
           title = "Correlation Matrix",
           ggtheme = theme_minimal())

## PCA
df_pca <- df[, 5:ncol(df)]
df_pca <- na.omit(df_pca)
dummy <- sapply(df_pca, function(x) length(unique(x)) <= 2)
df_pca <- df_pca[, !dummy]
pca <- prcomp(df_pca, center = TRUE, scale. = TRUE)

summary(pca)
pca$center  
pca$scale    
pca$rotation 

fviz_pca_var(pca, col.var = "contrib", repel = TRUE,
             gradient.cols = c("#0072B2", "#F0E442", "#009E73"))


#############################################
############# 3. Modelling ##################
#############################################

# split train and test
train <- df %>% filter(year <= 2000)
test  <- df %>% filter(year > 2000)
xvar <- c("ccode1", "ccode2", "year", "mid", "conttype", "joint_dem", 
          "ally", "nmc_ratio", "logtrade", "trdspike", "tradedip",
          "gdp_ratio", "gdppp_ratio", "pop_ratio", "area_ratio", "fdi_ratio")
train <- train[complete.cases(train[, xvar]), xvar]
test <- test[complete.cases(test[, xvar]), xvar]
train$mid <- factor(train$mid, levels = c(1,0))
test$mid  <- factor(test$mid, levels = c(1,0))

# tidymodel workflow
data_recipe <- recipe(mid ~ conttype + joint_dem + ally + nmc_ratio + 
                        logtrade + trdspike + tradedip + gdp_ratio + 
                        gdppp_ratio + pop_ratio + area_ratio + fdi_ratio, 
                      data = train) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_scale(all_predictors()) %>%
  step_center(all_predictors())

# 1. logit
model1 <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification")

# 2. lasso
model2 <- logistic_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet") %>%
  set_mode("classification")

# 3. random forest
model3 <- rand_forest(mtry = tune(), trees = 1000, min_n = tune()) %>% 
  set_engine("ranger", 
             importance = "permutation",
             probability = TRUE) %>%
  set_mode("classification")

# 4. SVM
model4 <- svm_rbf(cost = tune(), rbf_sigma = tune()) %>%
  set_engine("kernlab") %>%
  set_mode("classification")

# 5. xgboost
model5 <- boost_tree(trees = tune(), tree_depth = tune(), learn_rate = tune()) %>%
  set_mode("classification") %>%
  set_engine("xgboost")

# workflow
wf <- workflow_set(preproc = list(recipe = data_recipe),
                   models = list(logit = model1,
                                 lasso = model2,
                                 rf = model3,
                                 svm = model4,
                                 xgboost = model5))

# 10-fold cross-validation
cv_folds <- vfold_cv(train, v = 10)

# hyper parameter tuning
wf_results <- wf %>% 
  workflow_map(fn = "tune_grid", 
               resamples = cv_folds, 
               grid = 5,
               metrics = metric_set(roc_auc, accuracy),
               verbose = TRUE)

# ROC across models
autoplot(wf_results, 
         rank_metric = "roc_auc", 
         metric = "roc_auc") +
  geom_text_repel(aes(label = wflow_id, 
                      y = mean +1.95*std_err), 
                  max.overlaps = 10) +
  theme_minimal()


#############################################
############# 4. Evaluation #################
#############################################

## collect metrics
results <- wf_results %>% collect_metrics()

# 1. Select the best model
best_model <- results %>% 
  filter(.metric == "roc_auc") %>%
  arrange(desc(mean)) %>%
  slice(1) %>%
  pull(wflow_id)

cat('best model is: ',best_model, '\n') 

# extract the optimization results of the model
best_tune_results <- wf_results %>% 
  extract_workflow_set_result(id = best_model)

# choose the optimal parameter combination
best_params <- best_tune_results %>% 
  select_best(metric = "roc_auc")

# return the optimal parameters
best_workflow <- wf_results %>% 
  extract_workflow(id = best_model) %>% 
  finalize_workflow(best_params)

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

plot(table(pred$mid, pred$.pred_class),
     col = "#56B4E9",
     main = "Confusion Matrix",
     xlab = "True Class",
     ylab = "Predicted Class")

# 5. ROC curve for best model
pred.prob <- wf_best_fit %>%
  predict(new_data = test , type = "prob") %>%
  bind_cols(test)

the_roc_auc <- roc_auc(pred.prob, truth = mid, .pred_1)

pred.prob %>% roc_curve(mid, .pred_1) %>% 
  autoplot()+ ggtitle(paste(the_roc_auc, collapse = ", ") )

# 6. VIP plot
rf_fit <- wf_best_fit %>%
  extract_fit_parsnip() %>%
  .$fit
vip(rf_fit, num_features = 10)


# 7. PDP for top 3 vip variables
prep_rec <- prep(data_recipe)
train_processed <- bake(prep_rec, new_data = train)

pdp1 <- pdp::partial(rf_fit, pred.var = "gdp_ratio",
                     train = train_processed, prob = TRUE)
plotPartial(pdp1)

pdp2 <- pdp::partial(rf_fit, pred.var = "pop_ratio",
                     train = train_processed, prob = TRUE)
plotPartial(pdp2)

pdp3 <- pdp::partial(rf_fit, pred.var = "area_ratio",
                     train = train_processed, prob = TRUE)
plotPartial(pdp3)

