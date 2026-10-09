#' @title PipeOpTaskSurvRegrPV
#' @name mlr_pipeops_trafotask_survregr_pv
#' @template param_pipelines
#'
#' @description
#' Transform [TaskSurv] to [TaskRegr][mlr3::TaskRegr] using the **P**seudo
#' **V**alue (PV) methody by ----
#'
#' THEORIE
#'
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
#' TODO: OUTPUT BESCHREIBEN
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
  inherit = mlr3pipelines::PipeOP,

  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "trafotask_survregr_pv") {
      param_set = ps(
        tau = p_dbl(0)
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
      psv = self$param_set

      # checks: Checking if tau is really part of [0, T]:
      check_tau = assertNumber(psv$tau, null.ok = FALSE)
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

      # Calculating the LOO:
      data = task$data()
      n = nrow(data)
      # 1.) Calculating the Risk-set:
      risk_set = sum(ifelse(times >= tau, 1, 0))
      # 2.) Calculating the number of event times:
      event_times = sum(ifelse(times == tau & status == 1, 1, 0))
      # 3.) Calculating leave-one-out numbers:
      risk_set_loo = vector("numeric", length = n)
      event_times_loo = vector("numeric", length = n)
      km = vector("numeric", length = n)
      for(i in seq_along(1:n)) {
         risk_set_loo[i] = risk_set - ifelse(times[i] >= tau, 1, 0)
         event_times_loo[i] = event_times - ifelse(times[i] == tau & status[i] == 1, 1, 0)
         km[i] = prod((risk_set_loo[i] - event_times_loo[i]) / risk_set_loo[i])
         # TODO: RECHNUNG NOCHMAL DURCHGEHEN !!!
      }
      pv = vector("numeric", length = n)
      for(i in seq_along(1:n)) {
         pv[i] = n * global_KM_surv - (n - 1) * km[i]
      }

      # add pseudo-values to original data:
      data[["pv"]] = pv
      # ...
      # ...

      # create regression task:
      task_pv = TaskRegr$new(id = paste0(task$id, "_PV"), backend = data,
                             target = pv)

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
      survival_var = task$target_names()[1]
      rmst_var = task$target_names()[2]
      ci_var = task$target_names()[3]
      # -> Hier kommt es auf die Definition der target-names an --> gibt es nur ein oder mehrere
      # Vermutung: 1.) Survival Probability; 2.) RMST; 3.) Cumulative-incidenc (CI)
      tau = assertNumeric(self$param_set$tau, null.ok = FALSE)

      # ...
      # ...
      # Create regression task
      task_regr = TaskRegr$new(id = paste0(task$id, "_PV"), backend = data,
                               target = "pv", positive = "1")
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
