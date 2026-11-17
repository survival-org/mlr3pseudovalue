#' @title PipeOpTaskSurvRegrPV
#' @name mlr_pipeops_trafotask_survregr_pv
#' @template param_pipelines
#'
#' @description
#' Transform [TaskSurv] to [TaskRegr][mlr3::TaskRegr] using the **P**seudo
#' **V**alue (PV) method.
#'
#' The pseudo values give a new target variable which can be used for classical
#' regression learners. For the calculation of these pseudo values, this fomular is
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
#' * `predict_type` :: `factor()`\cr
#' This variable let define the quantity which should be used as a target. It has to be a character string
#' out of this set: {"survival", "rmst", ...}. Per default it is "survival", which takes the
#' survival function as the target variable.
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
        # predict_type as Hp to determine the type of target ?
        # predict_type = paradox::p_fct(levels = c("survival", "rmst")) # etc.))
        pv_test = paradox::p_fct(levels = c("influence", "none"), default = "influence")
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
      # predict_type = psv$predict_type

      # checks:
      # checks for tau (if tau is in set [0, T]):
      check_tau = checkmate::assertNumber(tau, null.ok = FALSE)
      max_event_time = max(task$unique_event_times())
      stopifnot(tau <= max_event_time)

      # checks for predict_type:
      # check_predict_type = checkmate::assertChoice(predict_type, c("survival", "rmst")) #, #etc.))

      
      # calculating pseudo-values:
      
      fit <- survival::survfit(Surv(times, status) ~ 1, data = data)
      sum_fit <- summary(fit)
      
      km <- summary(fit, time = tau)$surv
      time_before_tau <- which(sum_fit$time <= tau)
      na <- sum_fit$cumhaz[time_before_tau]
      risk_ind_mean <- sum_fit$n.risk[time_before_tau] / sum_fit$n
      
      # count_proc <- matrx(0, ncol = length(time_before_tau), nrow = nrow(data))
      # for (j in seq(length(time_before_tau))) {
      #  for (i in seq(nrow(data))) {
      #      count_proc[[ij]] <- ifelse(times[i] == time_before_tau[j] & status[i] == 1, 1, 0)
      #  }
      # }
      
      # count_proc <- outer(times, summary(fit)$time[time_before_tau], "==") *
      #  as.numeric(status == c(rep(1, length(status))))
      # checks for each time before tau if times is equal for each observation
      # ncol = length(time_before_tau): For each time point 
      # nrow = nrow(data): For each observation
      
      # Influence function (IF):
      # -> necessary to store it and apply it later on unseen D_test
      
      pv_influence <- function(t_i, status_i) {
        d_na <- diff(c(0, na))
        risk_ind <- ifelse(summary(fit)$time[time_before_tau] <= t_i, 1, 0)
        count_process <- ifelse(summary(fit)$time[time_before_tau] == t_i & status_i == 1, 1, 0)
        1 + sum((risk_ind * d_na - count_process) / risk_ind_mean) 
      }
      
      # Trying to get pv_influence operate on vector level:
      
      pv_influence <- function(times, status) {
        d_na <- diff(c(0, na))
        
        vapply(seq(times), function(i) {
          risk_ind <- ifelse(summary(fit)$time[time_before_tau] <= times[i], 1, 0)
          count_process <- ifelse(summary(fit)$time[time_before_tau] == times[i] & status[i] == 1, 1, 0)
          km * (1 + sum((risk_ind * d_na - count_process)/risk_ind_mean))
        }, numeric(1))
      }

      # idea: determine type of target through predict_type:
      # if (identical(predict_type, "survival")) {
      #   pv <- pseudo::pseudosurv(times, status, tau)$pseudo
      # }
      # if (identical(predict_type, "rmst")) {
      #   pv <- pseudo::pseudomean(times, status, tau)
      # }
      # etc.

      # Take IJ instead of Jackknife: 
      
      # PV for D_train with IJ: 
      pv <- eventglm::pseudo_infjack(Surv(times, status) ~ 1, time = tau, data = data,
                                     # type = ...
          )
      
      # add pseudo-values to original data:
      data[["pv"]] = pv
      # remove time and status target:
      data[[time_var]] = NULL
      data[[cens_var]] = NULL
      
      # setting self$state:
      self$state$tau = tau
      self$state$km = km
      self$state$na = na
      self$state$risk_ind_mean = risk_ind_mean
      # self$state$pv_influence = pv_influence()
      
      # create regression task:
      task_pv = TaskRegr$new(id = paste0(task$id, "_PV"), backend = data,
                             target = "pv")

      # pass on regrtask
      list(task_pv, NULL)
  },

    .predict = function(input) {
      task = input[[1L]]
      state = self$state
      # TODO: GET pv_test !
      
      # pv = pv_influence(state, task$times(), task$status())
      pv_influence = state$pv_influence(task$times(), task$status())
      # TODO: Fix pv_influence so that it is directly applyiable to time and status !
      km = state$km
      na = state$na
      risk_ind_mean = state$risk_ind_mean
      tau = state$tau
      
      data = task$data()
      time_var = task$target_names[1]
      status_var = task$target_names[2]
      # psv_predict = self$param_set$values
      # predict_type_predict = psv_predict$predict_type

      # checkmate::assertNumeric(tau, null.ok = FALSE)
      
      times = task$times()
      status = task$status()
      
      # setting pv as the new target
      # if (identical(predict_type_predict, "survival")) {
      #  pv <- pseudo::pseudosurv(times, status, tau)$pseudo
      # }
      # if (identical(predict_type_predict, "rmst")) {
      #  pv <- pseudo::pseudomean(times, status, tau)
      # }
      
      
      # remove time and status target:
      
      data[[time_var]] = NULL
      data[[status_var]] = NULL
      
      # calculate pv using the IF (influence function): 
      
      # pv <- km * mapply(IF, times, status)
      # IF is applied to times[1], status[1] than: times[2], status[2] etc. 
      
      
      # add pv to the original data:
      data[["pv"]] = pv

      if(identical(pv_test, "none")) {
        data[["pv"]] = NA_real_
      }
      
      # create regression task
      task_regr = TaskRegr$new(id = paste0(task$id, "_PV"), backend = data,
                               target = "pv")

      # keep original data
      data = list(
        row_ids = task$row_ids,
        times = task$times(),
        status = task$status(),
        # tau = tau
      )
      list(task_regr, data)
    }
  )
)

# register_pipeop("trafotask_survregr_pv", PipeOpTaskSurvRegrPV)
