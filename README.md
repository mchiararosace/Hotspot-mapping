# `R` code for hotspot mapping with Bayesian hierarchical spatial models

This is the R code associated with our paper titled "Hotspot mapping of pest introductions in the EU: A regional analysis of environmental, anthropogenic and spatial effects", currently submitted to _Biological Invasions_.

Authors: Maria Chiara Rosace, David V. Conesa, Antonio López-Quílez, Lorenzo Marini, Miguel A. Martinez-Beneito, Davide Nardi, Vittorio Rossi, Antonio Vicent, Martina Cendoya

The paper describes the use of a Bayesian hierarchical spatial model to assess areas of higher risk by including covariates and random effects to borrow information from neighbouring areas. 

Files included are:

* `models_run.R`: data preparation, neighborhood structure creation, model selection, and Bayesian hierarchical modeling, for studying plant pest introductions across various regions (NUTS2) in the EU

* `mode_select.R`: Function for model selection based on DIC and WAIC criteria

* `Correlation and maps.R`: correlation matrix and maps

You will also need to download an `.RData` file before running the above-mentioned files in R. 
This large data file, named `data.RData`, is available for download from Dropbox at the following link:

[Download data.RData](https://www.dropbox.com/scl/fi/4fbtgnhnpycou97t3do77/data.RData?rlkey=x1gstn0sg6g4nxhsdrmg9s4vm&st=dqw4la47&dl=0)
