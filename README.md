# 📊 Bayesian Interim Monitoring Dashboard  
### *Adaptive Decision-Making for Phase II Clinical Trials Using Bayesian Updating*

[![R](https://img.shields.io/badge/R-4.0%2B-blue?logo=r)](https://www.r-project.org/)
[![Shiny](https://img.shields.io/badge/Shiny-1.7%2B-purple?logo=r)](https://shiny.rstudio.com/)
[![License: MIT](https://img.shields.io/badge/License-MIT-green.svg)](https://opensource.org/licenses/MIT)
[![Reproducible](https://img.shields.io/badge/Reproducible-✅-blue)](https://www.r-project.org/nosvn/pandoc/R-reproducibility.html)
[![Regulatory-Compliant](https://img.shields.io/badge/ICH_E9(R1)-Compliant-orange)](https://database.ich.org/sites/default/files/E9-R1_Step4_Guideline_2020_0604.pdf)

> **Interactive dashboard for real-time, Bayesian-based interim monitoring of Phase II clinical trials — designed for regulatory-grade decision support.**

![Dashboard Screenshot](screenshot.png)

---

## 🔍 Overview

This Shiny app implements a **Bayesian adaptive monitoring framework** for early-phase clinical trials, following modern statistical and regulatory best practices (ICH E9(R1), FDA Adaptive Designs Guidance). It enables data-driven decisions at interim analyses — whether to **continue**, **stop for efficacy**, or **stop for futility** — based on posterior probabilities and predefined decision thresholds.

Ideal for:
- Biostatisticians supporting DSMBs (Data Safety Monitoring Boards)  
- Regulatory Affairs professionals submitting adaptive trial protocols  
- Clinical development teams optimizing resource allocation  
- Portfolio projects demonstrating advanced statistical engineering

---

## 🧠 Key Features

| Feature | Description | Regulatory Value |
|--------|-------------|------------------|
| **Real-Time Bayesian Updating** | Posterior response rate updated as each patient is enrolled (Beta-Binomial conjugacy) | Ensures decisions reflect accumulating evidence |
| **Predefined Decision Rules** | Efficacy (P(θ > 0.3 \| data) > 0.95) / Futility (P(θ > 0.3 \| data) < 0.10) | Objective, protocol-specified stopping criteria |
| **Interactive Enrollment Slider** | Simulate analyses at different sample sizes (n = 10–60) | Supports sample size re-estimation & go/no-go planning |
| **All-Patient Audit Table** | Full list of enrolled patients with outcomes — no aggregation | Enables source data verification (SDV) & audit trail |
| **Data Quality Check** | Automatic validation for missing dates/responses | Compliant with GCP (Good Clinical Practice) |
| **PDF Report Export** | One-click generation of interim monitoring report (via R Markdown) | Ready for DSMB submissions or sponsor review |
| **Regulatory Note Panel** | Explicit reference to ICH E9(R1) adaptivity principles | Demonstrates methodological rigor |

---

## 📈 How It Works: Bayesian Decision Logic

1. **Prior**: Weakly informative Beta(α=2, β=8) → mean = 20% response (common for novel dental/oncology therapies)  
2. **Data**: Binary outcome (1 = success, 0 = failure)  
3. **Posterior**: Beta(α + y, β + n − y), where *y* = successes, *n* = enrolled  
4. **Decision Metric**:  
   \[
   P(\theta > 0.3 \mid \text{data}) = 1 - \text{pbeta}(0.3, \alpha + y, \beta + n - y)
   \]
5. **Rules**:  
   - ✅ **STOP: Efficacy** if > 0.95  
   - 🛑 **STOP: Futility** if < 0.10  
   - ▶️ **CONTINUE** otherwise  

> *Example*: With 9/20 successes → posterior = Beta(11,19) →  
> posterior mean = 0.367, P(θ > 0.3 \| data) = 0.771 → **CONTINUE**.

---

## 🚀 Try It Locally

### Prerequisites
- R ≥ 4.0  
- RStudio (recommended)  
- Internet connection (for package installation)

### Installation
```r
# Install required packages (run once)
install.packages(c(
  "shiny", "shinydashboard", "shinyWidgets", "dplyr", "ggplot2",
  "plotly", "rmarkdown", "officer", "bslib", "extraDistr"
))

# Clone or download this repo
# Then run:
shiny::runApp("app.R")
