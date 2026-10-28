# mlr3pseudovalue

Pseudo-value reduction for the
[mlr3proba](https://github.com/mlr-org/mlr3proba) survival ecosystem.

A pseudo-observation is the jackknife contribution of a non-parametric
estimator — Kaplan-Meier for the survival probability, or the area under it for
the restricted mean survival time — at a fixed time point. Replacing the
censored outcome by these pseudo-values turns a survival task into a regression
task, so that any regression learner can be applied to time-to-event data. The
package provides the `mlr3pipelines` operators for that reduction and for
mapping the regression predictions back to a survival prediction.

The pseudo-value reduction is one of the five reductions discussed in Piller et
al. (2026), [*Reduction techniques for survival
analysis*](https://doi.org/10.1007/s10985-026-09714-0), Lifetime Data Analysis
32. `mlr3proba` ships the piecewise-exponential (`survtoregr_pem`),
discrete-time (`survtoclassif_disctime`) and IPCW (`survtoclassif_IPCW`)
reductions; this package adds the pseudo-value one.

## Status

Skeleton only. The package infrastructure is in place — `mlr3proba` as a
dependency, registration in the `mlr3pipelines` dictionaries, tests and CI —
the operators themselves are under development and interfaces will change.

## Installation

```r
# install.packages("pak")
pak::pak("survival-org/mlr3pseudovalue")
```

## Development

The pipeline operators, their verification against the
[pseudo](https://CRAN.R-project.org/package=pseudo) package and the benchmark
against other reductions and survival learners are being developed by
**Bela Hassan** as part of his Bachelor's thesis at the Department of
Statistics, LMU Munich.

## License

LGPL-3, following `mlr3proba`.
