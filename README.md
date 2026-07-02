# CTR/CVR Modeling with Survival Analysis for Delayed Feedback

> **Ca' Foscari University of Venice — Statistical Inference and Learning Project**  
> Yunseo Chang · 2026.01

---

## Overview

This project models **Click-Through Rate (CTR)** and **Conversion Rate (CVR)** in online advertising using Criteo's ad impression dataset, with a focus on the **delayed feedback problem** — the phenomenon where conversions are not immediately observed after a click.

| Item | Detail |
|------|--------|
| **Data** | Criteo Ad Impression Dataset (10,000 observations, 22 variables) |
| **Language** | R |
| **Course** | Statistical Inference and Learning, Ca' Foscari University |

---

## Analysis Pipeline

```
Data (Criteo)
  ↓ EDA — CTR/CVR distribution, feature exploration
  ↓ Train/Test Split — chronological 70/30 split (time-series structure)
  ↓ CTR Modeling — Logistic Regression (click prediction)
  ↓ CVR Modeling — Logistic Regression (conversion prediction)
  ↓ Delayed Feedback Analysis
      ├── Kaplan-Meier curves — conversion delay time visualization
      ├── Cox Proportional Hazards model — factors affecting conversion timing
      └── AFT models — Exponential / Weibull / Log-normal / Log-logistic comparison
```

---

## File Structure

```
├── Yunseo Chang_Project.Rmd   # Main analysis report (R Markdown)
├── project.R                  # Exploratory analysis script
├── modeling1.R                # CTR / CVR logistic regression
├── modeling2.R                # Survival analysis (KM, Cox PH, AFT)
├── images/                    # Visualizations
│   ├── hypothesis1.png
│   ├── hypothesis2_km.png
│   ├── km_curve.png
│   ├── km_behavior.png
│   ├── delayed_feedback_concept.png
│   ├── observation_window.png
│   └── ...
└── README.md
```

---

## Key Methods

### 1. CTR & CVR Modeling
- Logistic Regression with marginal effects interpretation
- Bootstrap confidence intervals
- ROC / AUC evaluation

### 2. Delayed Feedback — Survival Analysis
- **Kaplan-Meier**: non-parametric estimation of conversion delay distribution
- **Cox PH**: semi-parametric model identifying covariates affecting conversion hazard
- **AFT models**: parametric comparison across 4 distributions to model time-to-conversion

---

## Key Visualizations

<table>
  <tr>
    <td><img src="images/delayed_feedback_concept.png" width="320"/></td>
    <td><img src="images/km_curve.png" width="320"/></td>
  </tr>
  <tr>
    <td><img src="images/hypothesis2_km.png" width="320"/></td>
    <td><img src="images/observation_window.png" width="320"/></td>
  </tr>
</table>

---

## Data

The dataset is sampled from the [Criteo Sponsored Search Conversion Log Dataset](http://labs.criteo.com/2014/08/criteo-sponsored-search-conversion-log-dataset/).  
Place the CSV files in a `data/` folder to reproduce the analysis.

---

## Libraries

```r
library(readr); library(dplyr); library(ggplot2)
library(boot); library(pROC); library(car)
library(margins); library(effects)
library(survival)   # KM, Cox PH, AFT
```
