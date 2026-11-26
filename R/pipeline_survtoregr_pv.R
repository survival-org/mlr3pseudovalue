#' @template pipeline
#' @templateVar pipeop [PipeOpTaskSurvRegrPV] and [PipeOpPredRegrSurvPV]
#' @templateVar id survtoregr_pv
#' @templateVar title Survival to Regression reduction using PV
#' 
#' @param learner [LearnerRegr][mlr3::LearnerRegr]\cr
#' Regression learner to fit the transformed [TaskRegr][mlr3::TaskRegr].
#' 
#' @param tau (`numeric()`)\cr
#' Predefined time point for the *PV* reduction method. Observations with time larger than \eqn{\tau} are censored.
#' Must be less or equal to the maximum event time.
#' 
#' @param predict_type (`character`)\cr
#' Determines the target variable \theta. The parameter is directly used in the infinitesimal jackknife
#' calculation of *survival::pseudo()*. It has to be one of these three options:
#' "`surv`" (for the survival function), "`rmst`" (for the restricted mean survival time), "`pstate`"
#' (for the cumulative incidence function) . Is set to "`surv`" per default.
#' 
#' @param pv_test (`character`)\cr
#' Determines wheter the pseudo-values gets calculated for the testset or not.
#' Has two options: "`none`" or "`influence`".
#' `"none"`: No additional calculation for the testset is done. The target *pv* is an all-NA target, with which
#' a *TaskRegr* can still work. 
#' `"influence"`: Builds the target from the stored influence function from .train().
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
#' 3. [PipeOpPredRegrSurvPV] transforms the resulting [PredictionRegr][mlr3::PredictionRegr]
#' to [PredictionSurv].
#' 
#' @references
#' `r format_bib("piller_2026")`
#' 
#' @examplesIf condition
#' # example code
#' 
#' 
pipeline_survtoregr_pv <- function(learner, tau, predict_type = "survival", pv_test = "none", graph_learner = FALSE) {
  # Checking arguments:
  assert_learner(learner, task_type = "regr")
  checkmate::assertNumber(tau, lower = 0)
  checkmate::assertSubset(predict_type, c("survival", "rmst", "pstate"))
  checkmate::assertSubset(pv_test, c("none", "influence"))
  
  # Create graph instance:
  gr = Graph$new()
  
  # Add nodes to graph:
  gr$add_pipeop(po("trafotask_survregr_pv", tau = tau, predict_type = predict_type, pv_test = pv_test))
  gr$add_pipeop(po("learner", learner))
  gr$add_pipeop(po("nop"))
  gr$add_pipeop(po("trafopred_regrsurv_pv"))
  
  # Add edges between the nodes:
  gr$add_edge(src_id = "trafotask_survregr_pv", dst_id = learner$id, src_channel = "output", dst_channel = "input")
  gr$add_edge(src_id = learner$id, dst_id = "trafopred_regrsurv_pv", src_channel = "output", dst_channel = "input")
  gr$add_edge(src_id = "trafotask_survregr_pv", dst_id = "nop", src_channel = "data", dst_channel = "input")
  gr$add_edge(src_id = "nop", dst_id = "trafopred_regrsurv_pv", src_channel = "output", dst_channel = "data")
  
  # create_grlrn(gr, graph_learner)
}

# mlr_graphs$add("survtoregr_pv", pipeline_survtoregr_pv)
# register_graph("survtoregr_pv", pipeline_survtoregr_pv)


run_test = FALSE

if(run_test) {

# Example (will be added to description later):


task = tsk("rats")
part = partition(task)
# task_train = task$clone()$filter(part$train)
# task_test = task$clone()$filter(part$test)
task_train = task$filter(part$train)
task_test = task$filter(part$test)
# learner = lrn("regr.rpart")

grlrn = ppl(
        "survtoregr_pv",
        learner = lrn("regr.gam"),
        tau = 50, 
        graph_learner = TRUE
)


grlrn$train(task_train)
pred = grlrn$predict(task_test)
pred = pred$trafopred_regrsurv_pv.output

grlrn
pred

grlrn$plot()

tau = grlrn$state$trafotask_survregr_pv$tau

pred$score(msr("surv.cindex"))
pred$score(msr("surv.graf", times = tau, integrated = FALSE), task = task, train_set = part$train)



# Random Forest Regression model: 

library("randomForestSRC")

grlrn = ppl(
  "survtoregr_pv",
  learner = lrn("regr.rfsrc"),
  tau = 50, 
  graph_learner = TRUE
)


grlrn$train(task_train)
pred = grlrn$predict(task_test)
pred = pred$trafopred_regrsurv_pv.output

grlrn
pred

grlrn$plot()

tau = grlrn$state$trafotask_survregr_pv$tau

pred$score(msr("surv.cindex"))
pred$score(msr("surv.graf", times = tau, integrated = FALSE), task = task, train_set = part$train)


# Test on 25th november:


task = tsk("rats")

part = partition(task)
# task_train = task$clone()$filter(part$train)
# task_test = task$clone()$filter(part$test)
task_train = task$filter(part$train)
task_test = task$filter(part$test)


## LM:

pv_lm = ppl(
  "survtoregr_pv",
  learner = lrn("regr.lm"),
  tau = 40,
  graph_learner = TRUE
)

pv_lm

pv_lm$train(task_train)

lm_pred = pv_lm$predict(task_test)
lm_pred

lm_pred_values = lm_pred$trafopred_regrsurv_pv.output

plot(lm_pred_values$crank)

lm_pred_values$score(msr("surv.cindex"))


## RSFRC:

pv_rfsrc = ppl(
  "survtoregr_pv",
  learner = lrn("regr.rfsrc"),
  tau = 40,
  graph_learner = TRUE
)

pv_rfsrc$train(task_train)

rfsrc_pred = pv_rfsrc$predict(task_test)
rfsrc_pred

rfsrc_pred_values = rfsrc_pred$trafopred_regrsurv_pv.output

plot(rfsrc_pred_values$crank)


rfsrc_pred_values$score(msr("surv.cindex"))


# Comparison per plot:

par(mfrow = c(1, 2))

plot(lm_pred_values$crank)
plot(rfsrc_pred_values$crank)

# Comparison between crank & truth:

par(mfrow = c(1, 2))

plot(lm_pred_values$crank, ylim = c(0, 2))
plot(lm_pred_values$truth)


par(mfrow = c(1, 2))

plot(rfsrc_pred_values$crank, ylim = c(0, 2))
plot(rfsrc_pred_values$truth)



## GAM:

pv_gam = ppl(
  "survtoregr_pv",
  learner = lrn("regr.gam"),
  tau = 100,
  graph_learner = TRUE
)

pv_gam

pv_gam$train(task_train)
gam_pred = pv_gam$predict(task_test)

gam_pred

gam_pred_values = gam_pred$trafopred_regrsurv_pv.output

gam_pred_values$score(msr("surv.cindex"))


par(mfrow = c(1, 2))

plot(gam_pred_values$crank, ylim = c(0, 2))
plot(gam_pred_values$truth)



## LM with pv_test = "influence":

pv_lm_inf = ppl(
  "survtoregr_pv",
  learner = lrn("regr.lm"),
  tau = 40,
  pv_test = "influence",
  graph_learner = TRUE
)

pv_lm_inf

pv_lm_inf$train(task_train)

lm_pred_inf = pv_lm_inf$predict(task_test)
lm_pred_inf

lm_pred_inf_values = lm_pred_inf$trafopred_regrsurv_pv.output

plot(lm_pred_inf_values$crank)

lm_pred_inf_values$score(msr("surv.cindex"))

# lm_pred_values$score(msr("surv.cindex)) --> 0.32


## RFSRC with pv_test = "influence":

pv_rfsrc_inf = ppl(
  "survtoregr_pv",
  learner = lrn("regr.rfsrc"),
  tau = 40,
  pv_test = "influence",
  graph_learner = TRUE
)

pv_rfsrc_inf
pv_rfsrc_inf$train(task_train)

rfsrc_inf_pred = pv_rfsrc_inf$predict(task_test)
rfsrc_inf_pred_values = rfsrc_inf_pred$trafopred_regrsurv_pv.output

rfsrc_inf_pred_values$score(msr("surv.cindex"))


par(mfrow = c(1, 2))

plot(rfsrc_pred_values$crank)
plot(rfsrc_inf_pred_values$crank)


## GAM with pv_test = "influence":

pv_gam_inf = ppl(
  "survtoregr_pv",
  learner = lrn("regr.gam"),
  tau = 40,
  graph_learner = TRUE
)

pv_gam_inf

pv_gam_inf$train(task_train)

gam_inf_pred = pv_gam_inf$predict(task_test)
gam_inf_pred_values = gam_inf_pred$trafopred_regrsurv_pv.output

par(mfrow = c(1, 2))

plot(gam_pred_values$crank, ylim = c(0, 2))
plot(gam_inf_pred_values$crank, ylim = c(0, 2))

gam_inf_pred_values$score(msr("surv.cindex"))

}