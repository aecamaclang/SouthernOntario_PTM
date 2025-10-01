Calculate cost-effectiveness
================
Abbey Camaclang
2025-09-24

This calculates cost-effectiveness (CE) scores and ranks strategies by
Benefit, Cost, and CE.

Requires the following .csv files as inputs

1)  **Estimates_avg_benefits.csv**: table of mean benefit estimates  
2)  **EcolGroupsList.csv**: table with lists of species in each
    species/ecological group, and  
3)  **CostFeas.csv**: table of costs and feasibility of each strategy

``` r
library(tidyverse)
library(here)

# Set scaling factor to get cost and benefits in roughly the same order of magnitude
a <- 10^6

# Specify paths to subfolders within current working directory
input <- here("data", "raw") 
results <- here("results") 

# Read in and prep data
costfeas <- read.csv(paste0(input, "/CostFeas.csv"))
costfeas <- costfeas[-1,] # Remove baseline values
costfeas$Strategy <- as_factor(costfeas$Strategy)
names(costfeas)[str_which(names(costfeas), "3")] <- "Cost" # PV with 3% discount rate 
names(costfeas)[str_which(names(costfeas), "Feas")] <- "Avg.Feas" 
costfeas <- select(costfeas, c("Strategy", "Cost", "Avg.Feas"))

grplist <- read_csv(paste0(input, "/EcolGroupsList.csv")) 
numspp <- apply(grplist, MARGIN = 2, # number of species in each group
                FUN = function(x) length(x[!is.na(x)]) )

benefit.avg <- read.csv(paste0(results, "/Mean_benefits.csv"))

# Weight benefits by number of species in group (multiply)
ben.grpwtd <- benefit.avg[,2:ncol(benefit.avg)]*numspp
ben.grpwtd <- cbind(benefit.avg[,1], ben.grpwtd)
names(ben.grpwtd)[1] <- "Ecological.Group"

# write_csv(ben.grpwtd, paste0(results, "/Estimates_avg_benefits_groupwtd.csv"))

# Calculate total benefit of each strategy (sum across all species groups) 
bestben <- ben.grpwtd[,-1] %>%
  t() %>%
  data.frame() %>%
  setNames(ben.grpwtd[,1]) %>%
  filter(grepl("Best", rownames(.))) 

bestben.sum <- data.frame(rowSums(bestben)) %>%
  mutate(Est.type = rownames(.)) %>%
  separate(Est.type, c("Estimate", "Strategy"), sep = "[_]", remove = TRUE) %>% 
  mutate(Estimate = NULL) %>%
  relocate(Strategy) %>%
  setNames(c("Strategy", "Benefit")) %>%
  mutate(Strategy = as_factor(Strategy))

# Calculate cost-effectiveness and rank strategies
CE_table <- full_join(bestben.sum, costfeas, by="Strategy")%>%
  mutate(Exp.Benefit = Benefit * Avg.Feas) %>% 
  mutate(CE = (Exp.Benefit/Cost)*a) %>% # re-scale using scaling factor
  mutate(CE_rank = rank(-CE), 
         ExpBenefit_rank = rank(-Exp.Benefit),  
         Cost_rank = rank(Cost)) 

write.csv(CE_table, paste0(results, "/CE_Scores.csv"), row.names = FALSE)
```

| Strategy | Benefit | Cost | Avg.Feas | Exp.Benefit | CE | CE_rank | ExpBenefit_rank | Cost_rank |
|:---|---:|---:|---:|---:|---:|---:|---:|---:|
| S1 | 1029.6805 | 28630505 | 0.55 | 566.3242 | 19.7804488 | 2 | 15 | 2 |
| S2 | 1742.5507 | 100966624 | 0.65 | 1132.6579 | 11.2181420 | 4 | 9 | 6 |
| S3 | 2262.8871 | 27475428 | 0.42 | 950.4126 | 34.5913650 | 1 | 11 | 1 |
| S4 | 1962.8165 | 1461944349 | 0.51 | 1001.0364 | 0.6847295 | 15 | 10 | 11 |
| S5 | 996.6757 | 88506158 | 0.66 | 657.8060 | 7.4323186 | 6 | 14 | 4 |
| S6 | 1546.2704 | 96336616 | 0.44 | 680.3590 | 7.0623093 | 7 | 13 | 5 |
| S7 | 2212.4001 | 216582867 | 0.62 | 1371.6881 | 6.3333176 | 8 | 6 | 8 |
| S8 | 1562.4652 | 57628764 | 0.60 | 937.4791 | 16.2675551 | 3 | 12 | 3 |
| S9 | 2331.2430 | 305089025 | 0.64 | 1491.9955 | 4.8903612 | 10 | 5 | 10 |
| S10 | 2797.1639 | 1678527217 | 0.56 | 1566.4118 | 0.9332061 | 13 | 4 | 13 |
| S11 | 3206.1389 | 1779493841 | 0.59 | 1891.6220 | 1.0630113 | 11 | 2 | 14 |
| S12 | 2774.0948 | 1489419778 | 0.47 | 1303.8246 | 0.8753909 | 14 | 8 | 12 |
| S13 | 2274.4344 | 245213373 | 0.58 | 1319.1719 | 5.3796900 | 9 | 7 | 9 |
| S14 | 2818.1245 | 186070816 | 0.56 | 1578.1497 | 8.4814467 | 5 | 3 | 7 |
| S15 | 3838.5715 | 2078071312 | 0.56 | 2149.6001 | 1.0344207 | 12 | 1 | 15 |
