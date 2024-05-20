# `R` code for Hotspot-analysis with Bayesian hierarchical spatial models

This is the R code associated with our paper:
[_"Spatial modelling of hotspots for plant pests’ introductions in the EU using regionalized data" #add correct reference_]  

The paper describes the use of a Bayesian hierarchical spatial model to assess areas of higher risk by including covariates and random effects to borrow information from neighbouring areas. 

Files included are:

* `models_run.R`: data preparation, neighborhood structure creation, model selection, and Bayesian hierarchical modeling, for studying plant pest introductions across various regions (NUTS2) in the EU

* `mode_select.R`: Function for model selection based on DIC and WAIC criteria

* `Correlation and maps.R`: correlation matrix and maps
