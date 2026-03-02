rm(list =ls())
setwd("D:/MACS_30100/MLproject")
library(haven)
library(readxl)
library(dplyr)

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
summary(df)

mid <- mid %>% rename(ccode1 = statea, ccode2 = stateb)
mid <- mid[mid$year >= 1946, ]
mid$mid <- 1
df <- df %>% 
  left_join(mid %>% select(year, ccode1, ccode2, mid), by = c("year", "ccode1", "ccode2"))
df$mid[is.na(df$mid)] <- 0



######### COVARIATES ###########

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


# National Material Capabilities
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
  by = c("year", "ccode1", "ccode2")
  )

write.csv(df, file = "df.csv")

