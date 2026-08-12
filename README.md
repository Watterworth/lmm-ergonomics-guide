# Mixed-Effects Modelling for Ergonomics Research

## Authors

- [Michael W. B. Watterworth, MHSc](https://orcid.org/0000-0001-5227-4554), Ontario Tech University
- [Nicholas J. La Delfa, PhD](https://orcid.org/0000-0002-6928-5441), Ontario Tech University
- [Michael A. Rotondi, PhD](https://discover.academics.yorku.ca/Michael.Rotondi), York University

Correspondence regarding the tutorial may be directed to [Nicholas J. La Delfa](mailto:nicholas.ladelfa@ontariotechu.ca).
This folder contains beginner-oriented R code for the worked examples in *Moving Beyond Repeated Measures ANOVA: A Practical Guide to Mixed-Effects Modeling for Ergonomics Research*.

## View the tutorial

The fully rendered tutorial is available online at [watterworth.github.io/lmm-ergonomics-guide](https://watterworth.github.io/lmm-ergonomics-guide/). This is the recommended way to read the walkthrough and does not require R or Quarto to be installed.

## Run the analyses locally

1. Download or clone this repository.
2. Open `lmm-ergonomics-guide.Rproj` in RStudio.
3. Open `LMM_Ergonomics_Tutorial.qmd` to work through the annotated code, or open `R/00_Complete_Analysis.R` and run the complete analysis from top to bottom.

The illustrative dataset is supplied in `data/`, so readers do not need to simulate or generate the data before beginning. The data-generation code is also provided in `optional/` for readers who wish to examine it.

## Install the packages once

Run this command in the R console:

```r
install.packages(c(
  "lme4",
  "lmerTest",
  "emmeans",
  "ez",
  "performance",
  "ggplot2",
  "patchwork",
  "sjPlot"
))
```

The `performance` package produces the model-diagnostic figures that should be reviewed before interpreting or reporting results. `ggplot2` and `patchwork` reproduce the opening two-panel figure, while `sjPlot` plots the continuous model's population-level predictions over the raw observations.

## Files

```text
LMM_Ergonomics_Tutorial.qmd           Annotated, reader-facing tutorial
R/00_Complete_Analysis.R              All examples in one script
R/01_Factor_Models.R                  Example 1A only
R/02_Continuous_Model.R               Example 1B only
R/03_Missing_Data.R                   Example 2 only
R/04_Continuous_and_Factor_Figure.R   Opening two-panel figure
data/                                 Supplied illustrative dataset
optional/                             Data-generation code, not required
```

The three analysis example scripts repeat the package-loading and data-preparation steps deliberately. This makes each script self-contained and easier for a new R user to follow.

Running `R/00_Complete_Analysis.R` or `R/04_Continuous_and_Factor_Figure.R` saves `continuous_vs_factor_analysis.png` in the project folder.

## Analysis conventions

- `Reach_c10 = 0` represents the average observed reach distance.
- One unit of `Reach_c10` represents a 10-cm increase in reach.
- Global fixed-effect tests use Satterthwaite-approximated denominator degrees of freedom.
- Follow-up means, trends, and contrasts use the same approximation.
- Tukey adjustment is used for the reach-bin pairwise comparisons within each sex.
- Holm adjustment is used for the two tests of sex-specific slopes against zero.
