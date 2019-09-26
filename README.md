# Emulating a trial of a policy intervention in the setting of finite treatment resources: an application to increased risk organ transplantation
# Introduction
Here we provide the code to reproduce the analysis described in: 

### Citation

> 

# Abstract

Investigators are often interested in studying the effects on a particular outcome of treatment strategies in which all subjects are assigned the treatment of interest. Specifically, in these deterministic treatment strategies, the particular intervention assigned at each time is fully specified, and may depend on an individual’s measured covariates. However, sometimes the treatment assignment cannot be fully specified by the decision maker. For example, patients on the liver transplant waiting list cannot be assigned a liver transplant immediately at the time they reach highest priority because a suitable graft is not likely to be immediately available. In these cases, investigators may still be able to study the effects of stochastic dynamic treatment strategies, which assign individuals to treatment with a probability that depends on measured covariates. However, when treatment resources are finite, estimands which appropriately satisfy resource constraints should be chosen. Here, we describe an approach to emulate target trials of stochastic treatment strategies using observational data, and we apply this approach to estimate the effect of restricting or expanding utilization of ‘increased risk’ liver grafts to treat patients with end-stage liver disease. Our target trials are designed to evaluate plausible and policy-relevant interventions in the setting of finite treatment resources. 

# Organization
- `main.R` — R file which contains further details describing the analysis, and which reproduces the main analyses by running the code contained in `scripts`.
- `scripts`  — Scripts to reproduce the main analyses in the manuscript.
- `data`  — Raw data should be inserted in this folder. Data are available from the Scientific Registry of Transplant Recipients by application

# Correspondence
If you have any questions, comments, or discover an error, please contact Kerollos Wanis at knwanis@g.harvard.edu
