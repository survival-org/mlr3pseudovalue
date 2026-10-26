#' @title PipeOpTaskSurvRegrPV
#' @name mlr_pipeops_trafotask_survregr_pv
#' @template param_pipelines
#'
#' @description
#' Transform [TaskSurv] to [TaskRegr][mlr3::TaskRegr] using the **P**seudo
#' **V**alue (PV) method.
#'
#' The pseudo values give a new target variable which can be used for classical
#' regression learners. For the calculation of these pseudo values, this formular is
#' used (Piller et al., 2026):
#'
#' \eqn{\theta_i} = n \times \eqn{\theta} - (n - 1) \times \eqn{\theta}^(-i)
#'
#' In order to calculate the pseudo values, the class uses the build-in functions from
#' the package *pseudo*. Different functions for the different target variables exist
#' in this package and are called within this Pipeoperation class.
#' For more in-detail information on the *pseudo* package see:
#' \url{https://CRAN.R-project.org/package=pseudo}.
#'
#' @section Dictionary:
#' This [PipeOP][mlr3pipelines::PipeOP] can be instantiated via the
#' [dictionary][mlr3misc::Dictionary] [mlr3pipelines::mlr_pipeops]
#' or with the associated sugar function [mlr3pipelines::po()]:
#' ```
#' PipeOpTaskSurvRegrPV$new()
#' mlr_pipeops$get("trafotask_survregr_pv")
#' po("trafotask_survregr_pv")
#' ```
#'
#' @section Input and Output Channels:
#' [PipeOpTaskSurvRegrPV] has one input channel named "input", and two
#' output channels, one named "output" and "data".
#'
#' Training transforms the "input" [TaskSurv] to a [TaskRegr][mlr3::TaskRegr],
#' which is the output.
#' The target column is called `"pv"` and contain the pseudo-values for
#' all observations i. The time and censoring variable are removed as
#' targets.
#'
#' During prediction the "input" [TaskSurv] is transformed to a [TaskRegr][mlr3::TaskRegr]
#' with `"pv"` as the target. The "data" is a data.table[data.table::data.table], which contains the
#' `row_ids`, the time `times` \eqn{t_i} and the status variable `censoring` \eqn{\delta_i}.
#' This "data" is only meant to be used with the [PipeOpPredRegrSurvPV].
#'
#' @section Parameters:
#' The parameters are:
#'
#' * `tau` :: `numeric()`\cr
#' Predefined time point for PV. Observations with time larger than \eqn{\tau} are (right-)censored.
#' Must be less or equal to the maximum event time.
#'
#' * `prediction_type` :: `factor()`\cr
#' This variable let define the quantity which should be used as a target. It has to be a character string
#' out of this set: {"survival", "rmst", ...}.
#' ...
#'
#' @references
#' `r format_bib("piller_2026")`
#'
#' @seealso
#' @family Transformation PipeOps
#' @examplesIf condition
#' # example code
#'
#' @export
PipeOpTaskSurvRegrPV = R6Class("PipeOpTaskSurvRegrPV",
  inherit = mlr3pipelines::PipeOp,

  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "trafotask_survregr_pv") {
      param_set = paradox::ps(
        tau = paradox::p_dbl(0),
        # option: prediction_type as Hp to determine the type of target ?
        predict_type = paradox::p_fct(levels = c("survival", "rmst")) # etc.))
      )

      super$initialize(
        id = id,
        param_set = param_set,
        input = data.table(
          name = "input",
          train = "TaskSurv",
          predict = "TaskSurv"
        ),
        output = data.table(
          name = c("output", "data"),
          train = c("TaskRegr", "NULL"),
          predict = c("TaskRegr", "list")
        )
      )
    }
  ),

  private = list(
    .train = function(input) {
      task = input[[1L]]
      data = task$data()
      times = task$times()
      status = task$status()
      time_var = task$target_names[1]
      cens_var = task$target_names[2]
      psv = self$param_set$values
      tau = psv$tau
      predict_type = psv$predict_type

      # checks:
      # checking if tau is really part of [0, t_max]:
      check_tau = checkmate::assertNumber(tau, null.ok = FALSE)
      max_event_time = max(task$unique_event_times())
      stopifnot(tau <= max_event_time)

      # checking if predict_type is part of set:
      check_predict_type = checkmate::assertChoice(predict_type, c("survival", "rmst")) #, #etc.))


      # calculate the censoring variable:
      cut_times = times
      cut_times[cut_times > tau] = tau

      # calculate pv:

      # idea: determine type of target through prediction_type:
      if (identical(predict_type, "survival")) {
        pv <- pseudo::pseudosurv(times, status, tau)$pseudo
      }
      if (identical(predict_type, "rmst")) {
        pv <- pseudo::pseudomean(times, status, tau)
      }
      # etc.

      # add pseudo-values to original data:
      data[["pv"]] = pv
      # remove time and status target:
      data[[time_var]] = NULL
      data[[cens_var]] = NULL

      # create regression task:
      task_pv = TaskRegr$new(id = paste0(task$id, "_PV"), backend = data,
                             target = "pv")

      # keep this in the state to show that pipeop was trained
      self$state = list()
      # pass on regrtask
      list(task_pv, NULL)
  },

    .predict = function(input) {
      task = input[[1L]]
      data = task$data()
      time_var = task$target_names[1]
      status_var = task$target_names[2]
      psv_predict = self$param_set$values
      tau_predict = psv_predict$tau
      predict_type_predict = psv_predict$predict_type

      checkmate::assertNumeric(tau_predict, null.ok = FALSE)

      # setting new target: pv:
      times = task$times()
      status = task$status()
      if (identical(predict_type_predict, "survival")) {
        pv <- pseudo::pseudosurv(times, status, tau)$pseudo
      }
      if (identical(predict_type_predict, "rmst")) {
        pv <- pseudo::pseudomean(times, status, tau)
      }
      # remove time and status target:
      data[[time_var]] = NULL
      data[[status_var]] = NULL
      # add pseudo-values to original data:
      data[["pv"]] = pv

      # create regression task
      task_regr = TaskRegr$new(id = paste0(task$id, "_PV"), backend = data,
                               target = "pv")

      # keep original data
      data = list(
        row_ids = task$row_ids,
        times = task$times(),
        status = task$status(),
        tau = tau_predict
      )
      list(task_regr, data)
    }
  )
)

# register_pipeop("trafotask_survregr_pv", PipeOpTaskSurvRegrPV)
