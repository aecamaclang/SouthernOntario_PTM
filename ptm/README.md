# PTM Analysis

R project folder for the cost-effectiveness and complementarity analyses of the PTM. Data processing and analyses were performed using R version 4.5.1 (2025-06-13 ucrt) in RStudio 2025.05.1+513 for windows.

**/code/**
- *01-Compile-Aggregate-Plot.Rmd* compiles individual expert estimates of probabilities of persistence into a single table, and aggregates (averages) them to obtain a single value. Also creates summary plots of individual estimates as well as aggregated (mean) estimates of probabilities of persistence used for expert review and revision.
- *02-ExpBenefit-and-Persistence.Rmd* calculates expected benefits and expected probabilities of persistence by weighting benefit estimates by the feasibility of each strategy. Also creates summary plots of the the expected probabilities of persistence.
- *03-Calculate-CE.Rmd* calculates cost-effectiveness of candidate management strategies.
- *04_Complementarity.Rmd* identifies Pareto-optimal strategies based on maximizing the total number of species groups that achieve a specified threshold probability of persistence (or benefit achieved) for the least total cost. Also performs uncertainty analyses.
- *05_Manuscript-figures.Rmd* creates the figures used in the manuscript and supporting information.

**/data/raw/**
- contains the raw and summary data files used as inputs for the analysis. Also includes a blank copy of the Benefits worksheet provided to experts for eliciting estimates of probabilities of persistence.
- The *benefits/* subfolder is where .csv data files of individual expert estimates (*exp**XX**.csv*, where XX are expert ID numbers) are typically stored; note, however, that these files have been excluded from this archive to maintain expert confidentiality. A blank table (*exp00.csv*) has been provided instead for reference.

**/docs/**
- contains *.md* versions of the R code for viewing in GitHub. 

**/figures/**
- contains plots included in the manuscript and appendices.
- The *expertplots/* subfolder contains anonymized versions of the summary plots of expert estimates of probabilities of persistence, used for the review and revision stage of the expert elicitation, as well as the plots of the aggregated (averaged) estimates.

**/results/**
- contains intermediate and final results of the analysis, including tables of average benefit and probability of persistence, expected benefit and performance (i.e. estimates weighted by feasibility), and results of the cost-effectiveness and complementarity analyses.
- the *uncertainty/* subfolder contains the results of the uncertainty analyses.
