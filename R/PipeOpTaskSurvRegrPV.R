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
#' @references
#' `r format_bib(".....")`
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
        tau = paradox::p_dbl(0)
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
      psv = self$param_set$values
      tau = psv$tau

      # checks: Checking if tau is really part of [0, T]:
      check_tau = checkmate::assertNumber(tau, null.ok = FALSE)
      max_event_time = max(task$unique_event_times())
      stopifnot(tau <= max_event_time)

      # Calculate PV:

      # for both S(t|x) and RMST(t|x) KM(t) is needed:
      times = task$times() # give the time-points t
      status = task$status() # give the censoring variables c
      global_KM_fit = survival::survfit(Surv(times, status) ~ 1) # calculate global KM (without LOO)
      # maybe transform to vector ?
      global_KM_surv = matrix(global_KM_fit$surv, ncol = 1)

      # apply the cutoff to ´times´:
      cut_times = times
      cut_times[cut_times > tau] = tau # All time points that are larger than tau (so if the observation
      # is right-censored) are replaced by tau itself.
      # Getting KM(t) at time tau:
      # KM_tau = summary(global_KM_fit, times = tau)$surv
      KM_tau = summary(global_KM_fit, time = tau)[6]$surv

      # Calculating PV:
      data = task$data()
      n = nrow(data)
      # 1.) Calculating the Risk-set:
      # risk_set = sum(ifelse(times >= tau, 1, 0))

      # 1.) Setting risk-sets:
      risk_set = vector("numeric", length = tau)
      # 2.) Calculating the number of event times:
      # event_times = sum(ifelse(times == tau & status == 1, 1, 0))

      # 2.) Setting number of event times:
      event_times = vector("numeric", length = tau)

      # 3.) Calculating leave-one-out numbers:
      # Creating matrices with: rows = ids(i) and cols = times(s)
      risk_set_loo = matrix(0, nrow = n, ncol = tau)
      event_times_loo = matrix(0, nrow = n, ncol = tau)
      factors = matrix(0, nrow = n, ncol = tau)
      km = vector("numeric", length = n)
      for(i in seq_along(1:n)) {
        for(s in seq_along(1:tau)) {
          browser()
          # Calculating Risk-set and number of events per time-point s:
          risk_set[s] = sum(as.integer(times >= s))
          event_times[s] = sum(as.integer(times == s & status == 1))

          # Risk-sets and event-times loo:
          risk_set_loo[i, s] = risk_set[s] - ifelse(times[i] >= s, 1, 0)
          event_times_loo[i, s] = event_times[s] - ifelse(times[i] == s & status[i] == 1, 1, 0)

          # KM LOO estimator:
          factors[i, s] = event_times_loo[i, s] / risk_set_loo[i, s]
          km[i] = prod(1 - factors, na.rm = TRUE)



          # risk_set_loo[s] = risk_set - sum(ifelse(times[i] >= s, 1, 0))
          # risk_set_loo[i] = risk_set - ifelse(times[i] >= tau, 1, 0)
          # event_times_loo[s] = event_times - sum(ifelse(times[i] == s & status[i] == 1, 1, 0))
          # event_times_loo[i] = event_times - ifelse(times[i] == tau & status[i] == 1, 1, 0)
          # km[i] = prod((risk_set_loo[i] - event_times_loo[i]) / risk_set_loo[i])
          # TODO: RECHNUNG NOCHMAL DURCHGEHEN !!!
        }
      }

      pv = vector("numeric", length = n)
      for(i in seq_along(1:n)) {
         pv[i] = n * KM_tau[[1]] - (n - 1) * km[i]
      }

      # If target is RMST(x|t) -> Andersen 2010:
      # TODO: WIE KANN MAN RMST ALS TARGET IDENTIFIZIEREN ?
      for(i in seq_along(1:n)) {
          pv[i] = integrate(function(x) {
                      n * KM_tau[[1]] - (n - 1) * km[i]
                    }, lower = 0, upper = tau)
      }


      # add pseudo-values to original data:
      data[["pv"]] = pv
      # ...
      # ...

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
      times = task$times()
      status = task$status()
      data = task$data()
      time_var = task$target_names[1]
      status_var = task$target_names[2]
      tau = self$paramet_set$values$tau
      # pv = task$pv
      # survival_var = task$target_names[1]
      # rmst_var = task$target_names[2]
      # ci_var = task$target_names[3]
      # psv = self$param_set$values
      # tau = psv$tau

      tau = checkmate::assertNumeric(tau, null.ok = FALSE)

      # TODO: Cutoff (defining censoring after tau)

      # Create regression task
      task_regr = TaskRegr$new(id = paste0(task$id, "_PV"), backend = data,
                               target = "pv")
      # keep original data
      # TODO: NACHSCHAUEN, OB DIES SO STIMMT
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

