Compile, aggregate, and plot estimates
================
Abbey Camaclang and Aranya Iyer
2025-09-24

### Compile and format data

Reads individual expert estimates from multiple .csv files, compiles
them into a single table, and reorganizes the table into a tidy version.

It requires that the individual expert tables contain the same number of
rows and columns, are saved as .csv file in a subfolder within the
working directory, and no other .csv files are in the same subfolder.

``` r
# Load packages
library(here)
library(tidyverse)
library(naniar)

# Specify paths to subfolders within current working directory
input <- here("data", "raw") # where raw data files are stored
derived <- here("data") # where compiled data tables will be saved
results <- here("results") # where results of analysis will be saved

# Read in the individual tables of expert estimates (there should be one .csv file per expert) and combine.
nstrat <- 15 # number of strategies (including combinations, but excluding baseline)
ngroups <- 16 # number of ecological groups
numexp <- 13 # number of experts
numcols <- (nstrat+1+2) # number of cols to read in (one per strategy + 1 for Baseline + 2 for row header names)
numrows <- (ngroups+1)*3 # number of rows to read in (3 rows [Best, Low, High] per Ecol group and the example)
skiplines <- 20 # number of header rows to skip

# Combine .csv files into single data frame
# NOTE that these files are not included in the archive to maintain expert confidentiality; instead an empty template file is provided to show the table structure
files <- list.files(path = paste(input, "/benefits/", sep=""), pattern = "*.csv", full.names = T)
listcsv <- lapply(files,
                  function(x) read.csv(x, skip = skiplines, header = F, nrows = numrows, as.is = T)
                  )
rawdata <- do.call("rbind", listcsv)

# Check to make sure all rows have been read
checkrows <- nrow(rawdata)
if (checkrows != numrows*numexp) {
  warning("Unexpected number of rows in the combined table")
  }

rawdata <- rawdata[,1:numcols] # exclude 'Notes' column
names(rawdata) <- c("Ecological.Group", "Estimate", "Baseline",
                    paste(rep("S", times = nstrat),1:nstrat, sep = ""))

# Create vector of Expert ID and add to table
tempvec <- c()
for (i in 1:numexp) {
  tempvec <- c(tempvec, rep(i, times = (ngroups+1)*3))
  }
rawdata <- rawdata %>%
  add_column(Expert = tempvec, .before = "Ecological.Group")

# Format table
data <- rawdata %>%
  mutate(Ecological.Group = ifelse(Ecological.Group == "", NA, Ecological.Group)) %>%
  fill(Ecological.Group) %>%
  filter(!grepl("example", Ecological.Group)) # remove 'example' rows

# Standardize column/row names; must match the names in SpecialCases.csv and EcolGroupsList.csv
data$Ecological.Group[str_which(data$Ecological.Group, "Alvar")] <- "Alvar species"
data$Ecological.Group[str_which(data$Ecological.Group, "Artificial")] <- "Artificial structure dependent spp"
data$Ecological.Group[str_which(data$Ecological.Group, "Mixed Forest")] <- "Mixed" # change so "Forest" can be renamed
data$Ecological.Group[str_which(data$Ecological.Group, "Forest")] <- "Forest species"
data$Ecological.Group[str_which(data$Ecological.Group, "Mixed")] <- "Mixed forest species" # change to correct name
data$Ecological.Group[str_which(data$Ecological.Group, "Lake")]  <- "Ciscos"
data$Ecological.Group[str_which(data$Ecological.Group, "Naturalized")] <- "Naturalized open habitat spp"
data$Ecological.Group[str_which(data$Ecological.Group, "Oak Savannah")] <- "Oak savannah species"
data$Ecological.Group[str_which(data$Ecological.Group, "Riparian")] <- "Riparian species"
data$Ecological.Group[str_which(data$Ecological.Group, "Riverine")] <- "Riverine species"
data$Ecological.Group[str_which(data$Ecological.Group, "Sandy")] <- "Sandy species"
data$Ecological.Group[str_which(data$Ecological.Group, "Snakes and Lizard")] <- "Snakes and lizard"
data$Ecological.Group[str_which(data$Ecological.Group, "Wetland")] <- "Wetland species"
data$Ecological.Group[str_which(data$Ecological.Group, "Working")] <- "Working landscapes species"

data$Estimate[str_which(data$Estimate, "High")] <- "High"
data$Estimate[str_which(data$Estimate, "Low")] <- "Low"

# Replace X's and blanks with NA
na_strings <- c("X", "X ", "x", "x ")
clean <- data %>%
  replace_with_na_all(condition = ~.x %in% na_strings) %>%
  replace_with_na_all(condition = ~.x == "")

# Replace B's with value for baseline estimates from same row
for (i in 1:(numexp*ngroups*3)) {
  temp <- which(data[i,] == "B" | data[i,] == "b")
  clean[i,temp] <- data[i,4]
  }

write.csv(clean, file = paste(derived, "/Estimates_clean.csv", sep = ""), row.names = FALSE) # file excluded from archive

# Convert to tidy version for plotting
long <- gather(clean, key = Strategy, value = Value, Baseline:S15)

write_csv(long, file = paste(derived, "/Estimates_tidy.csv", sep = "")) # file excluded from archive

# Re-format for aggregating
grp.levels <- unique(long$Ecological.Group)

tempvec <- paste(long$Strategy, long$Estimate, sep = ".")
long <- add_column(long, Strat.Est = tempvec, .before = "Value")
long <- na.omit(long)
long$Value <- as.numeric(long$Value)

est.levels <- unique(long$Strat.Est)

wide <- select(long, -c("Estimate", "Strategy")) %>%
  spread(key = Strat.Est, value = Value)
wide$Ecological.Group <- factor(wide$Ecological.Group, levels = grp.levels)
wide <- wide[, c("Expert", "Ecological.Group", est.levels)] # Reorder columns by Strategy
wide <- with(wide, wide[order(Expert, Ecological.Group),]) # Reorder rows by Ecological Group

write_csv(wide, file = paste(derived, "/Estimates_wide.csv", sep = "")) # file excluded from archive

# Summarize the number of expert estimates
temp <- subset(long, Estimate == "Best")
temp$Strategy <- factor(temp$Strategy,levels = unique(temp$Strategy))

stgy.count <- table(temp$Ecological.Group, temp$Strategy)

write.csv(stgy.count, file = paste(results, "/estimatecounts.csv", sep = ""))
```

### Plot individual expert estimates

Creates plots of initial estimates for expert review and revision.

This script creates two plots for each Ecological Group:

1)  boxplots of the best guess, lowest, and highest estimates for each
    Strategy from all Experts;  
2)  pointrange plots showing the best guess, lowest, and highest
    estimates of each Expert for each Strategy.  
    It requires a table of all individual expert estimates
    (**Estimates_tidy.csv**; excluded from the archive).

NOTE that plots showing individual expert estimates have been excluded
from the archived version.

``` r
library(tidyverse)
library(cowplot)
library(gridExtra)
library(here)

# Specify paths to subfolders within current working (R project) directory
derived <- here("data") 
results <- here("results") 
figures <- here("figures") # where plots will be saved
explots <- here("figures/expertplots") # where individual files for expert review will be saved 

# Read in data
long <- read_csv(paste(derived,"/Estimates_tidy.csv", sep="")) # file excluded from archived version

strat.levels <- unique(long$Strategy)
grp.levels <- unique(long$Ecological.Group)
expcode <- unique(long$Expert) 
est.levels <- c("Low", "Best", "High")

long$Strategy <- factor(long$Strategy, levels = strat.levels)
long$Estimate <- factor(long$Estimate, levels = est.levels)

long <- na.omit(long) 

# Rearrange table so estimates for each ecol group-strategy are on the same row
wide <- spread(long, key = Estimate, value = Value)
wide$Expert <- as.factor(wide$Expert)
```

**Boxplots**

``` r
for (j in seq_along(expcode)) {
  
  # If you do NOT need to highlight each expert's response, comment out the relevant lines (indicated below) and run the inside loop (i.e., from the line below) only to get a single set of plots
  grp.list <- list()
  
  for (i in seq_along(grp.levels)) { 
    
    temp.grpdata <- subset(long, Ecological.Group == grp.levels[i])
    
    temp.plot <-
      ggplot(temp.grpdata, aes(x = Estimate, 
                               y = Value, 
                               fill = Estimate) 
             ) + 
      geom_boxplot(width=0.4) + 
      geom_point(data = subset(temp.grpdata, Expert == expcode[j]), # plot expert [j]'s estimates as blue points
                 aes(x = Estimate, y = Value),
                 color = 'blue'
                 ) + # include the geom_point option only if you want to highlight individual expert responses
      theme_cowplot() +   
      theme(plot.margin = unit(c(1.5, 1, 1.5, 1), "cm"), 
            panel.spacing = unit(1, "lines"), 
            axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)), 
            plot.caption = element_text(size = 10, hjust = 0)
            ) + 
      facet_wrap( ~ Strategy, nrow = 3) +  # one panel per management strategy
      scale_x_discrete(name = "",
                       breaks = c("Low", "Best", "High"),
                       labels = c("L", "B", "H") # Give the x-axis variables shortened labels
                       ) + 
      scale_fill_manual(values = c("white", "gray80", "white"), 
                        guide = "none" # remove legend
                        ) + 
      labs(x = "", 
           y = "Probability of persistence (%)", 
           title = paste(grp.levels[i]),
           caption = str_wrap(paste0(
             "Figure ", i, ". Boxplots summarizing the distribution of the lowest (L), best guess (B), and highest (H) expert estimates of the probability of persistence of ", grp.levels[i], " under the Baseline scenario and each of the management strategies (S1 - S15). The thick horizontal lines indicate the median estimate, while the surrounding box shows the interquartile range. Any outliers are shown as points beyond the plot whiskers. Your individual estimates are shown in blue."), 150)
           ) +
      ylim(0, 100)
    
    grp.list[[i]] <- temp.plot
    
  }
  
  # Save all plots as a single .pdf: 
  plot1 <- marrangeGrob(grp.list, nrow = 1, ncol = 1, top = NULL) # one plot (species group) per page
  ggsave(
    # filename = "All_boxplot.pdf", # use instead if generating a single file with no highlighting
    filename = paste0("exp", expcode[j], "_boxplot.pdf", sep=''),
    plot1, 
    path = explots, 
    width = 11, height = 8.5, units = "in")
  
}
```

**Pointrange plots**

``` r
for (j in seq_along(expcode)) {
  
  # If you do NOT need to highlight each individual response, comment out the relevant lines (indicated below) and run the inside loop (i.e., from the line below) only to get a single set of plots
  grp.list <- list()
  
  for (i in seq_along(grp.levels)) {
    
    temp.expdata <- subset(wide, Ecological.Group == grp.levels[i])
    temp.expdata <- mutate(temp.expdata, expi = ifelse(Expert == expcode[j], T, F)) # use if highlighting individual expert responses
    
    temp.plot2 <-
      ggplot(temp.expdata, aes(x = Expert, 
                               y = Best 
                               , color = expi, # use this only if highlighting individual expert responses
                               )
             ) +  
      geom_pointrange(aes(ymin = Low, ymax = High)) +
      theme_cowplot() +  
      theme(plot.margin = unit(c(1.5, 1, 1.5, 1), "cm"), 
            panel.spacing = unit(1, "lines"), 
            axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)), 
            axis.text.x = element_text(size=10),
            # axis.text.x = element_blank(), # use this instead to remove x-axis labels of expert code (for archiving)
            plot.caption = element_text(size = 10, hjust = 0)
            ) +  
      scale_color_manual(values = c("grey", "blue"), guide = "none") + # needed if highlighting individual expert responses
      facet_wrap( ~ Strategy, nrow = 3) +  # one panel per management strategy
      labs(x = "Experts",
           y = "Probability of persistence (%)",
           title = paste(grp.levels[i]),
           caption = str_wrap(paste0(
             "Figure ", i, ". Plots of each expert estimate of the probability of persistence of ", grp.levels[i], 
             " under the Baseline scenario and each of the management strategies (S1 - S15). Each point indicates 
             the best guess of one expert. Your individual estimates are plotted in blue."), 150)
           ) +
      ylim(0, 100) 
    
    grp.list[[i]] <- temp.plot2
  }
  
  # Save all plots as a single .pdf: 
  plot2 <- marrangeGrob(grp.list, nrow = 1, ncol = 1, top = NULL) # one plot per page
  ggsave(
    # filename = "All_pointrange.pdf", # use instead if plotting all estimates without highlighting
    filename = paste0("exp", expcode[j], "_pointrange.pdf", sep=''), # if highlighting individual expert estimates
    plot2, 
    path = explots,
    width = 11, height = 8.5, units = "in"
  )
  
}
```

### Aggregate estimates

This code:

1)  calculates benefits of each strategy (strategy - baseline) for each
    ecological group,  
2)  aggregates (averages) benefit across expert estimates, and  
3)  calculates mean probability of persistence under each strategy based
    on the aggregated estimates.

This script uses **Estimates_wide.csv**.

For the Southern Ontario PTM, some of the expert estimates for a given
ecological group are based on only a subset of species in that group. To
make sure these estimates are weighted accordingly, the following files
are required:

1)  a .csv file **EcolGroupsList.csv** with ecological groups in columns
    and the list of species for each group in rows, and  
2)  a .csv file **SpecialCases.csv** listing only the estimates (from
    which expert, and for which ecol group and strategy) that require
    different weighting, along with the number of species in that group
    that the estimate is based on. Information for this table can be
    derived from experts’ comments on the elicitation worksheet.

The names of ecological groups in both files must match the names used
in the compiled benefit estimates tables. Store both files in the
*~/data/raw/* folder.

``` r
library(tidyverse)
library(here)

# Specify paths to subfolders within current working directory
input <- here("data", "raw") 
derived <- here("data") 
results <- here("results") 

# Read in and prepare data
long <- read_csv(paste0(derived, "/Estimates_tidy.csv"))  
grp.levels <- unique(long$Ecological.Group)

wide <- read_csv(paste0(derived, "/Estimates_wide.csv"))
wide$Expert <- as_factor(wide$Expert)
wide$Ecological.Group <- factor(wide$Ecological.Group, levels = grp.levels)

# Get number of species in each group 
grplist <- read_csv(paste0(input, "/EcolGroupsList.csv")) 
numspp <- apply(grplist, MARGIN = 2, FUN = function(x) length(x[!is.na(x)]) )
grpwts <- data.frame(Ecological.Group=names(numspp), numspp) 
grpwts$Ecological.Group <- factor(grpwts$Ecological.Group, levels = grp.levels)

# Get number of species scored by each expert for each strategy
spcases <- read_csv(paste0(input, "/SpecialCases.csv")) 
spcases$Strategy <- factor(spcases$Strategy, levels = unique(long$Strategy))
spcases$Expert <- factor(spcases$Expert, levels = levels(wide$Expert))
spcases$`Ecological Group`<- factor(spcases$`Ecological Group`, levels = grp.levels)
names(spcases)[which(str_detect(names(spcases), "Ecological Group")==1)] <- "Ecological.Group"  

# Calculate benefit
baseline <- wide[3:5]
strategies <- wide[6:ncol(wide)]  
benefit <- strategies - as.matrix(baseline)

# Format benefit table
benefit <- cbind(wide[,1:2], benefit) 
benefit.long <- gather(benefit, key = Est.type, value = Value, -c(1:2)) %>%
  separate(., Est.type, c("Strategy", "Estimate"), sep = "[.]", remove = TRUE)

strat.levels <- c("Baseline", unique(benefit.long$Strategy))
benefit.long$Strategy <- factor(benefit.long$Strategy, levels = strat.levels)
benefit.long$Estimate <- as_factor(benefit.long$Estimate) 

benefit.wide <- spread(benefit.long, key=Estimate, value = Value) 

# Combine tables for weighting and aggregating
benefit.joined <- left_join(benefit.wide, spcases, 
                            by=c("Expert", "Ecological.Group", "Strategy")) %>% 
  left_join(., grpwts, by = "Ecological.Group") 

fullwts.idx <- which(is.na(benefit.joined$NumSppScored)) # NOTE NAs indicate that estimate was based on all species in the group
benefit.joined$NumSppScored[fullwts.idx] <- benefit.joined$numspp[fullwts.idx] 

# Calculate the total number of spp scored across all experts and strategies
fullwts <- aggregate(benefit.joined$NumSppScored,  
                     by = list(Ecological.Group = benefit.joined$Ecological.Group, 
                               Strategy = benefit.joined$Strategy), 
                     FUN = sum, na.rm = TRUE)

# Calculate the weights to assign to each estimate and apply the weights to the benefit values
benefit.joined <- benefit.joined %>%
  left_join(., fullwts, by = c("Ecological.Group", "Strategy")) %>% 
  mutate(Wts = NumSppScored/x) %>% # NOTE: x is the sum from above
  mutate(Wt.Best = Best*Wts,  
         Wt.Low = Low*Wts, 
         Wt.High = High*Wts)

# Aggregate (sum) the weighted benefit estimates and re-organize table for calculating performance 
benefit.avg <- aggregate(benefit.joined[,11:13], 
                         by = list(Ecological.Group = benefit.joined$Ecological.Group, 
                                   Strategy = benefit.joined$Strategy), 
                         FUN = sum, na.rm = TRUE) %>%
  gather(., key = "Est.Type", value = "Wt.Avg", Wt.Best, Wt.Low, Wt.High)

benefit.avg$Est.Type <- as_factor(benefit.avg$Est.Type)

benefit.avg <- benefit.avg %>%
  arrange(Ecological.Group, Strategy, Est.Type) %>%
  unite(., col = "Estimate", c("Est.Type", "Strategy"), sep = "_", remove = TRUE)

benefit.avg$Estimate <- as_factor(benefit.avg$Estimate)

benefit.avg <- benefit.avg %>%
  spread(., Estimate, Wt.Avg)

write_csv(benefit.avg, paste0(results, "/Mean_benefits.csv"))

# Prepare the baseline estimates table
baseline <- cbind(wide[,1:2], baseline)
baseline <- baseline %>%
  rename(Best = Baseline.Best, Low = Baseline.Low, High = Baseline.High) %>%
  add_column(Strategy = rep("Baseline", nrow(baseline)), .before = "Best")
baseline$Strategy <- factor(baseline$Strategy, levels = strat.levels)

# Calculate weights as above and aggregate baseline estimates
baseline.joined <- left_join(baseline, spcases, 
                             by = c("Expert", "Ecological.Group", "Strategy")) %>%
  left_join(., grpwts, by = "Ecological.Group")

base.fullwts.idx <- which(is.na(baseline.joined$NumSppScored))
baseline.joined$NumSppScored[base.fullwts.idx] <- baseline.joined$numspp[base.fullwts.idx]

base.fullwts <- aggregate(baseline.joined$NumSppScored, 
                          by = list(Ecological.Group = baseline.joined$Ecological.Group, 
                                    Strategy = baseline.joined$Strategy), 
                          FUN = sum, na.rm = TRUE)

baseline.joined <- baseline.joined %>%
  left_join(., base.fullwts, by = c("Ecological.Group", "Strategy")) %>%
  mutate(Wts = NumSppScored/x) %>%
  mutate(Wt.Best_Baseline = Best*Wts,
         Wt.Low_Baseline = Low*Wts,
         Wt.High_Baseline = High*Wts)

baseline.avg <- aggregate(baseline.joined[,11:13], 
                          by = list(Ecological.Group = baseline.joined$Ecological.Group, 
                                    Strategy = baseline.joined$Strategy), 
                          FUN = sum, na.rm = TRUE) %>%
  select(., -Strategy)

write_csv(baseline.avg, paste0(results, "/Mean_baseline.csv"))

# Calculate mean performance (probability of persistence) by adding mean benefit to the mean baseline
persistence <- benefit.avg[,2:ncol(benefit.avg)] + as.matrix(baseline.avg[,2:ncol(baseline.avg)]) 
persistence <- cbind(baseline.avg, persistence)

write_csv(persistence, paste0(results, "/Mean_persistence.csv"))
```

### Plot aggregated estimates

Creates pointrange plots of the aggregated (mean) best guess, lowest,
and highest estimates of probability of persistence given a table of
mean estimates (**Mean_persistence.csv**).

Provided to experts as part of the review/revision stage of expert
elicitation. This is also the same figure used as Fig. B-2 in the
manuscript appendix.

``` r
library(tidyverse)
library(cowplot)
library(gridExtra)
library(here)

# Specify paths to subfolders within current working (R project) directory
results <- here("results") 
figures <- here("figures") # where plots will be saved
explots <- here("figures/expertplots") # where individual files for expert review will be saved 

# Read in and format data
persistence <- read_csv(paste0(results, "/Mean_persistence.csv"))

grp.levels <- unique(persistence$Ecological.Group)
persistence$Ecological.Group <- factor(persistence$Ecological.Group, levels = grp.levels)

persistence.long <- persistence %>%
  gather(., key = "Estimate", value = "Value", -Ecological.Group) %>%
  separate(., Estimate, c("Est.Type", "Strategy"), sep = "[_]", remove = TRUE) %>%
  mutate(Strategy = factor(Strategy, levels = unique(Strategy)))

plot.data <- persistence.long %>%
  spread(., 
         key = Est.Type, 
         value = Value)
# write_csv(plot.data, paste0(results, "/Estimates_avg_persistence_tidy.csv"))

strat.levels <- levels(plot.data$Strategy)

baseline.data <- plot.data[which(plot.data$Strategy=="Baseline"),]
strategy.data <- plot.data[which(plot.data$Strategy!="Baseline"),]

# Create plot
temp.plot3 <- 
  ggplot(strategy.data, aes(x = Strategy, y = Wt.Best) ) +
  geom_pointrange(aes(ymin = Wt.Low, ymax = Wt.High)) +
  geom_hline(aes(yintercept = Wt.Best), baseline.data, colour = "blue") +
  geom_hline(aes(yintercept = Wt.Low), baseline.data, colour = "blue", lty = "dashed") +
  geom_hline(aes(yintercept = Wt.High), baseline.data, colour = "blue", lty = "dashed") +
  theme_cowplot() +  
  theme(plot.margin = unit(c(1.5, 1, 1.5, 1), "cm"), # t, r, b and l margins around the plot area
        panel.spacing = unit(1, "lines"), # adjust margins and between panels of the plot 
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)), 
        axis.text.x = element_text(size = 8, angle = 65, hjust = 1, vjust = 1)
        , plot.caption = element_text(size =10, hjust = 0)
        ) +
  facet_wrap( ~ Ecological.Group, nrow = 4, ncol = 4) +  # one panel per ecological group
  theme(strip.text = element_text(size=8))+
  labs(x = "Strategies",
       y = "Probability of persistence (%)"
       , caption = str_wrap("Estimated probability of persistence of each ecological group under each of the management strategies (Best Guess = solid black dots, Lowest and- Highest estimates = black vertical lines), compared to the probability under the Baseline scenario (blue horizontal lines: Best Guess = solid lines, Lowest and Highest Estimates = dashed lines). Values are based on expert estimates averaged over the number of experts that provided estimates for the strategy and ecological group.", 100)
       ) +  
  ylim(0, 100) 

# Save as .pdf for expert review
ggsave(filename=paste0(explots, "/Mean_persistence_plot.pdf"), 
       temp.plot3, 
       width = 8.5, height = 11, 
       units = "in")

# Save plot of unweighted mean estimates as .png for use as Fig. B-2 of manuscript
temp.plot3 <- temp.plot3 + labs(caption = "")
ggsave(filename=paste0(figures, "/FigureB-2.png"), 
       temp.plot3,
       width = 7.5, height = 10, units = "in")
```
