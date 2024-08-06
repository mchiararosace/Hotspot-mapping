# `R` code for hotspot mapping with Bayesian hierarchical spatial models

This is the R code associated with the paper titled "Hotspot mapping of pest introductions in the EU: A regional analysis of environmental, anthropogenic and spatial effects".

Authors: Maria Chiara Rosace, David V. Conesa, Antonio López-Quílez, Lorenzo Marini, Miguel A. Martinez-Beneito, Davide Nardi, Vittorio Rossi, Antonio Vicent, Martina Cendoya

The paper describes the use of a Bayesian hierarchical spatial model to assess areas of higher risk of pests' introductions by including covariates and random effects to borrow information from neighbouring areas. 

Files included are:

* `models_run.R`: data preparation, neighborhood structure creation, model selection, and Bayesian hierarchical modeling, for studying plant pest introductions across various regions (NUTS2) in the EU. Please note that INLA is not available on CRAN, so you will need to install it manually in R when using the 'models_run' file. You can download it from: https://www.r-inla.org/download-install

* `mode_select.R`: Function for model selection based on DIC and WAIC criteria

* `Correlation and maps.R`: correlation matrix and maps
