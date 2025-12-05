# shinyFORC
A Shiny App for Bayesian Probabilistic Forecasting Under Model Uncertainty



## Overview

ShinyFORC conducts Bayesian probabilistic forecasting (BPF) with longitudinal data. The app implements our BPF framework, which utilizes methodologies designed to obtain optimally predictive measures of rate of change as the foundation for projecting future trends. ShinyFORC can be applied to a variety of longitudinal data, including country-level large-scale assessments, repeated measures for individuals, and more.

## Modeling Framework

ShinyFORC integrates latent growth estimation, Bayesian model averaging (BMA), and Bayesian stacking into one interactive and accessible Shiny application.

The app uses the blavaan package (which runs Stan in the background) to provide latent growth estimates for each unit of analysis:

https://cran.r-project.org/package=blavaan

These estimates serve as inputs to:

BMS: https://cran.r-project.org/package=BMS

rstanarm: https://cran.r-project.org/package=rstanarm

loo: https://cran.r-project.org/package=loo

This allows users to conduct and compare BMA and Bayesian stacking under various parameter and model prior specifications.

## Forecasting Capabilities

ShinyFORC supports multiple weighting approaches, including ELPD-LOO, pseudo-BMA, and pseudo-BMA+ weighting. The app generates in-sample and pseudo–out-of-sample predictions, performance measures, and interactive visualizations.

It can also conduct true out-of-sample forecasts and produce forecasting plots.

## Audience

ShinyFORC brings advanced Bayesian methods into a single user-friendly application for both technical and non-technical researchers working with longitudinal data.

## Contact

For questions or support, contact Kjorte Harra at:
harra@wisc.edu or kjorteh@gmail.com


GitHub: https://github.com/kjorteh
Citation

Harra, K., & Kaplan, D. (in preparation). ShinyFORC: A Shiny App for Bayesian Probabilistic Forecasting Under Model Uncertainty.
