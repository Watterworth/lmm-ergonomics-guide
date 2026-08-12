# Mixed-Effects Modelling for Ergonomics Research

This repository contains a beginner-oriented tutorial, illustrative dataset, and R code for the worked examples in *Moving Beyond Repeated Measures ANOVA: A Practical Guide to Mixed-Effects Modeling for Ergonomics Research*.

## Authors

* [Michael W. B. Watterworth, MHSc](https://orcid.org/0000-0001-5227-4554), Ontario Tech University
* [Nicholas J. La Delfa, PhD](https://orcid.org/0000-0002-6928-5441), Ontario Tech University
* [Michael A. Rotondi, PhD](https://discover.academics.yorku.ca/Michael.Rotondi), York University

Correspondence regarding the tutorial may be directed to [Nicholas J. La Delfa](mailto:nicholas.ladelfa@ontariotechu.ca).

## Where to start

The browser-based tutorial is available at:

https://watterworth.github.io/lmm-ergonomics-guide/

To run the analyses locally:

1. Download or clone this repository.
2. Open `lmm-ergonomics-guide.Rproj` in RStudio.
3. Open `LMM_Ergonomics_Tutorial.qmd` for the annotated walkthrough or run `Complete_Analysis_Script.R` from top to bottom.

The illustrative dataset is supplied in `data/`, so readers do not need to generate the data before beginning. The data-generation code is provided in `optional/` for readers who wish to examine how the dataset was created.

## Install the packages once

Run the following command in the R console:

```r
install.packages(c(
  "lme4",
  "lmerTest",
  "emmeans",
  "ez",
  "performance",
  "see",
  "ggplot2",
  "patchwork",
  "sjPlot"
))
```

The `performance` and `see` packages produce the model-diagnostic figures that should be reviewed before interpreting or reporting results. `ggplot2` and `patchwork` reproduce the opening two-panel figure, while `sjPlot` plots the continuous model’s population-level predictions over the raw observations.

## Repository contents

```text
LMM_Ergonomics_Tutorial.qmd    Annotated, reader-facing tutorial
Complete_Analysis_Script.R     Complete analysis in one R script
data/                          Supplied illustrative dataset
optional/                      Data-generation code
CITATION.cff                   Repository citation metadata
LICENSE                        MIT License
```

## Analysis conventions

* `Reach_c10 = 0` represents the average observed reach distance.
* One unit of `Reach_c10` represents a 10-cm increase in reach.
* Global fixed-effect tests use Satterthwaite-approximated denominator degrees of freedom.
* Follow-up means, trends, and contrasts use the same approximation.
* Tukey adjustment is used for the reach-bin pairwise comparisons within each sex.
* Holm adjustment is used for the two tests of sex-specific slopes against zero.

## Citation

Citation information is available through the **Cite this repository** link on GitHub. The archived version corresponding to the published article will also be made available through Zenodo.

## License

The contents of this repository are distributed under the [MIT License](LICENSE).
