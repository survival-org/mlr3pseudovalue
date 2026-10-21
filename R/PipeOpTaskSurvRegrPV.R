#' @title PipeOpTaskSurvRegrPV
#' @name mlr_pipeops_trafotask_survregr_pv
#' @template param_pipelines
#'
#' @description
#' Transform [TaskSurv] to [TaskRegr][mlr3::TaskRegr] using the **P**seudo
#' **V**alue (PV) methody.
#'
#' The pseudo values give a new target variable which can be used for normal
#' regression learners. For the calculation of these pseudo values, this fomular is
#' used (Bender et al. 2026):
#'
#' \eqn{\theta_i} = n \times \eqn{\theta} - (n - 1) \times \eqn{\theta}^(-i)
#'
#' For the two target values: S(t|x) and RMST(t), the nonparametric Kaplan-Meier
#' estimation for S(t) is used for \eqn{\theta}(t). The calculation takes two parts:
#' 1.) The global KM-estimator at time point \tau is calculated.
#' 2.) The LOO KM-estimator at time point \tau is calculated for each observation
#' i = 1,..., n.
#' Both of these steps are implemented in the training step. While for the global
#' KM-estimator the *survival* package is used, the LOO KM-estimator is calculated
#' seperated.
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
#' ouput channels, one named "output" and the other named "data".
#'
#' Training transforms the "input" [TaskSurv] to as [TaskRegr][mlr3::TaskRegr],
#' which is the output.
#' The target column is called `"pv"` and contain the single pseudo-values for
#' all observations i.
#'
#'
#'
#' @section Parameters:
#' The parameters are:
#'
#' * `tau` :: `numeric()`\cr
#' Predefined time point for PV. Observations with time larger than \eqn{\tau} are (right-)censored.
#' Must be less or equal to the maximum event time.
#'
#' (* `prediction_type` :: `factor()`\cr
#'
#'
#' )
#'
#' @references
#' `r format_bib(".....")`
#'
#' @seealso
#' @family Transformation PipeOps
#' @examplesIf condition
#' # example code
#'
#' @export
#'
#'
#'
#'
#'
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
      task = input[[1L]]     # task entspricht der zugeordneten Aufgabe !!!
                             # Also alles in dieser Funktion mit task bezieht sich auf die Aufgabe
                             # Bsp.: task = tsk("lung")
      data = task$data()
      times = task$times()
      status = task$status()
      time_var = task$target_names[1]
      cens_var = task$target_names[2]
      psv = self$param_set$values
      tau = psv$tau
      predict_type = psv$predict_type

      # checks:
      # checking if tau is really part of [0, T]:
      check_tau = checkmate::assertNumber(tau, null.ok = FALSE)
      max_event_time = max(task$unique_event_times())
      stopifnot(tau <= max_event_time)

      # checking if predict_type is part of set:
      check_predict_type = checkmate::assertChoice(predict_type, c("survival", "rmst")) #, #etc.))


      # calculate the censoring variable:
      cut_times = times
      cut_times[cut_times > tau] = tau

      # Calculate PV:

      # Output list with: 1.) ordered time points; 2.) PV values for each i
      # surv_prob_pv <- pseudo::pseudosurv(times, status, tau)$pseudo
      # rmst_pv <- pseudo::pseudomean(times, status, tau)
      # given better output:
      # rmst_data <- cbind(data, pseudo = rmst_pv, id = 1:nrow(data))
      # if (length(status > 2)) {
      # ci_pv <- pseudo::pseudoci(times, status, tau)
      # }

      # Idea: Determinating type of target through prediction_type:
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
      tau = self$paramet_set$values$tau

      tau = checkmate::assertNumeric(tau, null.ok = FALSE)

      # remove time and status target:
      data[[time_var]] = NULL
      data[[status_var]] = NULL
      # Create regression task
      task_regr = TaskRegr$new(id = paste0(task$id, "_PV"), backend = data,
                               target = "pv")

      # keep original data
      data = list(
        row_ids = task$row_ids,
        times = task$times(),
        status = task$status(),
        tau = tau
      )
      list(task_regr, data)
    }
  )
)
