### ReadME

This repo contains the `The Analysis of Serve Decisions in Tennis using Bayesian Hierarchical Models` project.

* The model is fit only on Roland Garros tracking data, however similar tracking data from the Australian Open is available too.
* The JSON data (from RG's Court Vision feature) was saved in early December 2020. 

`modelling`
-----------
* Fit all Bayesian models of interest 
* Obtain Posterior predictions
* Plot model results (eg: compare player-varying intercepts)

`prototypes`
------------
* Legacy code that may contain some useful - albeit completely scattered - bits of code


`collect_data`
--------------
* Scrape Roland Garros tracking data
* Scrape AO tracking data
* Process dataframes into a useable format with all covariates of interest

`eda`
------
* Plots for the EDA section
* Eg: Federer's point importance, serve speeds vs serve number, player-specific heatmaps

`src`
-----
* Contains some functions that I repeatedly use throughout the project

