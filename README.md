# Data Analysis of 2013 New York City Flights

Math 151: Fundamentals of Data Science
Authors: Lindsay Gross, Siyu Li, Bryan Alfonso Llobrera
Date: May 6, 2026

## Overview

This project analyzes the `flights` dataset from the R package `nycflights13`
(336,776 flights departing from New York City in 2013, 19 variables). The goal is to explore
the factors that influence arrival delay (`arr_delay`) and to build models to predict it.

After removing flights with missing departure or arrival delay (mostly cancelled or incomplete
flights), 327,346 observations (about 97.2%) remain.

## Contents

| File | Description |
|---|---|
| `flights_analysis.R` | Full R script that reproduces the analysis |
| Project report | Written report with results and interpretation (PDF/Word) |
| `README.md` | This file |

The data are not included in this repository: they come with the `nycflights13` package.

## Analyses in the script

1. **Background and cleaning**: missing values, variable types
2. **Univariate analyses**: distributions of arrival and departure delay
3. **Bivariate analyses**: delay vs. origin, destination, airline, time of day, season, and distance
4. **Multivariate analyses**: airline × time of day, season × time of day
5. **Correlation analysis**: correlation matrix and heatmap of numerical variables
6. **Regression**: simple linear regression, multiple linear regression (with VIF), and logistic regression
7. **Principal component analysis**: `dep_delay`, `arr_delay`, `air_time`, `distance`
8. **Clustering**: Hopkins statistic, VAT, silhouette method, agglomerative clustering, K-means, and K-medoids

## Requirements

R (the script was run with R 4.6.0) and the following packages:

```r
install.packages(c("nycflights13", "tidyverse", "lubridate", "corrplot",
                   "car", "GGally", "factoextra", "cluster", "hopkins",
                   "gridExtra", "mclust"))
```

## How to run

1. Install the packages above (only needed once).
2. Open `flights_analysis.R` in R or RStudio.
3. Run the script from the top. Sections follow the order of the report.

Notes:

- Some plots draw all 327,346 points and can be slow. If the plotting device throws an error,
  restart R or plot a random sample of the points instead.
- The clustering section works on a random sample of 500 flights. Seeds are set in the script,
  but results can change with a different seed or R version, so cluster sizes and shapes may
  not match exactly across runs.
