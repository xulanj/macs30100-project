# macs30100-project

# Project Description

- **Overview:** 
This project examines whether structural political and economic factors are associated with the onset of militarized interstate disputes (MIDs). The analysis uses dyad–year data covering the period from 1946 to 2014, where each observation represents a pair of states in a given year. The project incorporates commonly used predictors from the international conflict literature, including geographic contiguity, joint democracy, alliance relationships, relative military capabilities, trade dependence, and economic indicators. Using these variables, the project applies a bunch of machine learning approaches to evaluate how well structural features can predict the onset of interstate disputes. The analysis also compares the predictive performance of traditional statistical models with more flexible machine learning methods in the context of rare-event conflict prediction.

- **Research Question:** 
This project asks whether structural political and economic variables can meaningfully predict the onset of militarized interstate disputes between states. In particular, it examines whether machine learning models improve predictive performance relative to traditional logistic regression when forecasting MID onset using dyadic data.

- **Why this matters:**
Understanding the conditions under which interstate disputes occur is central to the study of international conflict. Improving predictive models of MID onset can help scholars evaluate the informational value of commonly used structural variables and assess whether more flexible modeling approaches provide meaningful forecasting gains. More broadly, the findings contribute to debates on the predictability of rare political events and the potential role of machine learning methods in conflict forecasting and early warning systems.

## Project owner 
- **Xulan Jiang**

**Course**: MACSS 30100 (Winter 2026)


## Project Structure

```
macs30100-project/
├── README.md                      # this file
├── code.r                         # R script for the entire project
└── data/                          # repository of downloaded raw data
    ├── dyads.csv                           # cross-sectional data containg all possible dyads of ccode
    ├── dyadic_mid_4.03.dta                 # Dyadic MID Data (v4.0)
    ├── contdird.dta                        # Direct Contiguity Data (v3.2)
    ├── alliance_v4.1_by_dyad_yearly.dta    # Formal Alliances Data (v4.1)
    ├── NMC-60-abridged.dta                 # National Material Capabilities Data (v6.0)
    ├── Dyadic_COW_4.0.csv                  # Trade Data (v4.0)
    ├── p5v2018.xls                         # PolityIV data
```
