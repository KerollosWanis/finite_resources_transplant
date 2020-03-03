# Causal inference with limited resources: proportionally-representative interventions
# Introduction
Here we provide the code to reproduce the analysis described in: 

### Citation

> 

# Abstract
Investigators often evaluate treatment effects by considering settings in which all individuals
are assigned a treatment of interest, assuming that an unlimited number of treatment units
are available. However, many real-life treatments are of limited supply and cannot be
provided to all individuals in the population. For example, patients on the liver transplant
waiting list cannot be assigned a liver transplant immediately at the time they reach highest
priority because a suitable organ is not likely to be immediately available. In these cases,
investigators may still be interested in the effects of treatment strategies in which a finite
number of organs are available at a given time, that is, treatment regimes that satisfy
resource constraints. Here, we describe an estimand that can be used to define causal
effects of treatment strategies that satisfy resource constraints: proportionally-representative
interventions for limited resources. We derive a simple class of inverse probability weighted
estimators, and apply one such estimator to evaluate the effect of restricting or expanding
utilization of `increased risk' liver organs to treat patients with end-stage liver disease. Our
method is designed to evaluate policy-relevant interventions in the setting of finite treatment
resources.

# Organization
- `main.R` — R file which contains further details describing the analysis, and which allows users to reproduce the main analyses by running the code contained in `scripts`.
- `scripts`  — Scripts to reproduce the main analyses in the manuscript.
- `data`  — Raw data should be inserted in this folder. Data are available from the Scientific Registry of Transplant Recipients by application

# Correspondence
If you have any questions, comments, or discover an error, please contact Kerollos Wanis at knwanis@g.harvard.edu
