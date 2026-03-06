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

## pca也放进来 EDA
pca <- prcomp(df[, 5:ncol(df)], scale = TRUE)
summary(pca)
pca$center
pca$scale
pca$rotation

library(factoextra)
fviz_pca_var(pca)






















