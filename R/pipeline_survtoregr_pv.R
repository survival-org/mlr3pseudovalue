#' @template pipeline
#' @templateVar pipeop [PipeOpTaskSurvRegrPV] and [PipeOpPredRegrSurvPV]
#' @templateVar id survtoregr_pv
#' @templateVar title Survival to Regression reduction using PV
#' 
#' @param learner [LearnerRegr][mlr3::LearnerRegr]\cr
#' Regression learner to fit the transformed [TaskRegr][mlr3::TaskRegr].
#' 
#' @param tau (`numeric()`)\cr
#' Predefined time point for PV. Observations with time larger than \eqn{\tau} are censored.
#' Must be less or equal to the maximum event time.
#' 
#' @param predict_type (`character`)\cr
#' Determinate the target variable. The parameter is directly used in the infinitesimal jackknife
#' calculation of *survival::pseudo()*. It has to be one of these three options:
#' "`surv`", "`rmst`", "`pstate`". Is set to "`surv`" per default.
#' 
#' @param pv_test (`character`)\cr
#' Determinates wheter the pseudo-values gets calculated for the testset or not.
#' Has two options: "`none`" or "`influence`".
#' `"influence"`: Builds the target from the stored influence function from .train().
#' `"none"`: No additional calculation for the testset is done. The target "pv" is an all - NA target, with which
#' a *TaskRegr* can still work. 
#' Per default set to `"none"`.
#' 
#' 
#' @param graph_learner (`logical(1)`)\cr
#' If `TRUE` returns wraps the [Graph][mlr3pipelines::Graph] as a
#' [GraphLearner][mlr3pipelines::GraphLearner] otherwise (default) returns as a `Graph`.
#' 
#' @details
#' The pipeline consists of the following steps:
#'
#' 1. [PipeOpTaskSurvRegrPV] Converts [TaskSurv] to a [TaskRegr][mlr3::TaskRegr].
#' 2. A [LearnerRegr][mlr3::LearnerRegr] is fit and predicted on the new `TaskRegr`.
#' 
#' 3. [PipeOpPredRegrSurvPV] transforms the resulting [PredictionRegr][mlr3::PredictionRegr]
#' to [PredictionSurv].(NOTWENDIG ?)
#' 
#' @references
#' `r format_bib("piller_2026")`
#' 
#' @examplesIf condition
#' # example code
#' 
#' 
pipeline_survtoregr_pv <- function(learner, tau, predict_type = "surv", pv_test = "none", graph_learner = FALSE) {
  # Checking arguments:
  assert_learner(learner, task_type = "regr")
  checkmate::assertNumber(tau, lower = 0)
  checkmate::assertSubset(predict_type, c("surv", "rmst", "pstate"))
  checkmate::assertSubset(pv_test, c("none", "influence"))
  
  # Create graph instance:
  gr = Graph$new()
  
  # Add nodes to graph:
  gr$add_pipeop(po("trafotask_survregr_pv", tau, predict_type, pv_test))
  gr$add_pipeop(po(learner$id, learner))
  gr$add_pipeop(po("nop"))
  gr$add_pipeop(po("trafopred_regrsurv_pv"))
  
  # Add edges between the nodes:
  gr$add_edge(src_id = "trafotask_survregr_pv", dst_id = learner$id, src_channel = "ouput", dst_channel = "input")
  gr$add_edge(src_id = learner$id, dst_id = "trafopred_regrsurv_pv", src_channel = "output", dst_channel = "input")
  gr$add_edge(src_id = "trafotask_survregr_pv", dst_id = "nop", src_channel = "data", dst_channel = "input")
  gr$add_edge(src_id = "nop", dst_id = "trafopred_regrsurv_pv", src_channel = "input", dst_channel = "data")
  
  create_grlrn(gr, graph_learner)
}
