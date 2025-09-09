# Priority Threat Management - Southern Ontario

This archive contains the summarized and anonymized data, code, and results of the Priority Threat Management analysis for Southern Ontario, as presented in the following manuscript:

Camaclang, A. E., Iyer, A., Liang, C., Giles, E., Frank, B., Alambo, K. I., Lamoureux, J., Matchett, S., Miller, T., Norris, D. R., Perron, M. A. C., Rumney, R. H. M., Schueler, F. W., Timms, L., Paquette, C., Hemming, V., Snider, J., & Martin, T. (2025). Nature requires investment: Applying Priority Threat Management to support biodiversity and climate targets.

A pre-print version of the manuscript is available from EcoEvoRxiv at <https://doi.org/10.32942/X2NW6R>.

This archive is organized as follows:

**code/**
- *01-Compile-and-Aggregate.Rmd* compiles individual expert estimates of probabilities of persistence into a single table, and aggregates (averages) them to obtain a single value. Also calculates expected benefit and expected probability of persistence by weighting benefit estimates by the feasibility of each strategy.
- *02-Expert-plots.Rmd* creates summary plots of individual estimates of probabilities of persistence used for expert review and revision. Also creates plots of the aggregated estimates.
- *03-CE-test.Rmd* calculates cost-effectiveness of candidate management strategies.
- *04_Complementarity.Rmd* identifies Pareto-optimal strategies based on maximizing the total number of species groups that achieve a specified threshold probability of persistence (or benefit achieved) for the least total cost. Also performs uncertainty analyses.
- *05_Manuscript-figures.Rmd* creates the figures used in the manuscript and appendices.

**data/raw/**
- contains the raw and summary data files used as inputs for the analysis. Also includes a blank copy of the Benefits worksheet provided to experts for eliciting estimates of probabilities of persistence.
- The *benefits/* subfolder is where .csv data files of individual expert estimates (*exp**XX**.csv*, where XX are expert ID numbers) are typically stored; note, however, that these files have been excluded from this archive to maintain expert confidentiality. A blank table (*exp00.csv*) has been provided instead for reference.

**docs/**
- contains .html versions of the R code for viewing. 

**figures/**
- contains plots included in the manuscript and appendices.
- The *expertplots/* subfolder contains anonymized versions of the summary plots of expert estimates of probabilities of persistence, used for the review and revision stage of the expert elicitation, as well as the plots of the aggregated (averaged) estimates.

**results/**
- contains intermediate and final results of the analysis, including tables of average benefit and probability of persistence, expected benefit and performance (i.e. estimates weighted by feasibility), and results of the cost-effectiveness and complementarity analyses and the uncertainty analysis.
