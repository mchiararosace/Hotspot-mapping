# `R` code for hotspot mapping with Bayesian hierarchical spatial models

This is the R code associated with the paper titled "Hotspot mapping of pest introductions in the EU: A regional analysis of environmental, anthropogenic and spatial effects".

Authors: Maria Chiara Rosace, David V. Conesa, Antonio López-Quílez, Lorenzo Marini, Miguel A. Martinez-Beneito, Davide Nardi, Vittorio Rossi, Antonio Vicent, Martina Cendoya

The paper describes the use of a Bayesian hierarchical spatial model to assess areas of higher risk of pests' introductions by including covariates and random effects to borrow information from neighbouring areas. 

Files included are:

* `models_run.R`: data preparation, neighbourhood structure creation, model selection, and Bayesian hierarchical modelling, for studying plant pest introductions across various regions (NUTS2) in the EU.

* `mode_select.R`: Function for model selection based on DIC and WAIC criteria.

* `Correlation and maps.R`: correlation matrix and maps.

Cite the code using: [![DOI](https://zenodo.org/badge/803205533.svg)](https://zenodo.org/doi/10.5281/zenodo.13283845)
