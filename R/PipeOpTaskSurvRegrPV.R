#' @title PipeOpTaskSurvRegrPV
#' @name mlr_pipeops_trafotask_survregr_pv
#' @template param_pipelines
#'
#' @description
#' Transform [TaskSurv] to [TaskRegr][mlr3::TaskRegr] using the **P**seudo
#' **V**alue (PV) method.
#'
#' The pseudo values give a new target variable which can be used for classical
#' regression learners. For the calculation of these pseudo values, this fomular can be
#' used (Piller et al., 2026):
#'
#' \eqn{\theta_i} = n \times \eqn{\theta} - (n - 1) \times \eqn{\theta}^(-i)
#'
#' This is the classical jackknife approach. But, in order to enable the classical ml-workflow, the
#' infinitesimal jackknife pseudo-values are used in this implementation.
#' 
#' During training, the class uses the build-in function *pseudo* from the package *survival*. 
#' The functions work for different target variables, which is controlled through an argument.
#' For more in-detail information on the *survival* package see:
#' \url{https://cran.r-project.org/web/packages/survival/index.html}.
#' Furthermore the influence function *pv_influence* and its necessary properties gets
#' defined. 
#' 
#' In the prediction phase on the testset, two options exist, which are both controlled through
#' the parameter *pv_test*. If *pv_test = none*, no *pv* for the testset gets calculated and instead
#' a all-NA target gets put into the *RegrTask*.  If *pv_test = influence*, than *pv_influence* gets
#' called during prediction and calculates the target *pv* which is put into the *RegrTask*. 
#' The properties that are used for this calculation are the one calculated during training.
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
#' out of this set: {"surv", "rmst", "pstate"}. Per default it is "surv", which takes the
#' survival function as the target variable.
#' This argumented is put inside the "`pseudo`" function from the packet "`survival`". 
#'
#' * `pv_test` :: `factor()`\cr
#' Determines wheter the testset gets a pseudo-value target or not.
#' Can take two values:
#' `"influence"`: Builds the target from the stored influence function from .train().
#' `"none"`: No additional calculation for the testset. The target "pv" is an all - NA target, with which
#' a *TaskRegr* can still work. 
#' Per default set to `"none"`.
#'
#'
#' @references
#' `r format_bib("piller_2026")`
#' `r frormat_bib("overgaard_2017")`
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
        predict_type = paradox::p_fct(levels = c("survival", "rmst", "pstate"), init = "survival"),
        pv_test = paradox::p_fct(levels = c("influence", "none"), init = "none")
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
      # data = task$data()
      # times = task$times()
      # status = task$status()
      
      
      cat("\n--- PIPEOP TRAIN START ---\n")
      
      print(class(task))
      print(task$target_names)
      print(task$feature_names)
      print(task$nrow)
      
      
      
      # surv_data = data.frame(times = times, status = status)
      # time_var = task$target_names[1]
      # cens_var = task$target_names[2]
      psv = self$param_set$values
      tau = psv$tau
      predict_type = psv$predict_type
      pv_test = psv$pv_test

      # checks:
      # checks for tau (if tau is in set [0, T]):
      check_tau = checkmate::assertNumber(tau, null.ok = FALSE)
      max_event_time = max(task$unique_event_times())
      stopifnot(tau <= max_event_time)
      
      # calculating pseudo-values:
      
      cat("Times length:", length(task$times()), "\n")
      cat("Status length:", length(task$status()), "\n")
      
      fit <- survival::survfit(Surv(task$times(), task$status()) ~ 1)
      sum_fit <- summary(fit)
      
      cat("Task:", task$nrow, "\n")
      cat("Fit n:", fit$n, "\n")
      
      
      km <- summary(fit, time = tau)$surv
      time_before_tau <- which(sum_fit$time <= tau)
      na <- sum_fit$cumhaz[time_before_tau]
      risk_ind_mean <- sum_fit$n.risk[time_before_tau] / sum_fit$n
      
      
      # Influence function (IF):
      # -> necessary to store it and apply it later on unseen D_test if one want "pv" on D_test
      
      # pv_influence <- function(t_i, status_i) {
      #  d_na <- diff(c(0, na))
      #  risk_ind <- ifelse(summary(fit)$time[time_before_tau] <= t_i, 1, 0)
      #  count_process <- ifelse(summary(fit)$time[time_before_tau] == t_i & status_i == 1, 1, 0)
      #  1 + sum((risk_ind * d_na - count_process) / risk_ind_mean) 
      #}
      
      # Trying to get pv_influence operate on vector level:
      
      pv_influence <- function(t, s) {
        d_na <- diff(c(0, na))
        
        vapply(seq(t), function(i) {
          risk_ind <- ifelse(summary(fit)$time[time_before_tau] <= t[i], 1, 0)
          count_process <- ifelse(summary(fit)$time[time_before_tau] == t[i] & s[i] == 1, 1, 0)
          km * (1 + sum((risk_ind * d_na - count_process)/risk_ind_mean))
        }, numeric(1))
      }
      
      # Try to get faster version: 
      
      # pv_influence2 <- function(times, status) {
      #  d_na <- diff(c(0, na))
      #  risk_ind <- vector("numeric", length = length(d_na))
      #  count_process <- vector("numeric", length = length(d_na))
        
      #  for(i in seq(times)) {
      #    risk_ind[i] <- ifelse(summary(fit)$time[time_before_tau] <= times[i], 1, 0)
      #    count_process[i] <- ifelse(summary(fit)$time[time_before_tau] == times[i] & status[i] == 1, 1, 0)
      #  }
          
      #  km * (1 + sum((risk_ind * d_na - count_process)/risk_ind_mean))
      #}

      # Wouldnt be really faster due to two for loops 
      
      # Take IJ instead of Jackknife: 
      
      # PV for D_train with IJ: 
      pv <- survival::pseudo(fit, times = tau, type = predict_type)
      names(pv) <- NULL
      
      data = task$data()
      
      # add pseudo-values to original data:
      data[["pv"]] = pv
      
      
      time_var = task$target_names[1]
      cens_var = task$target_names[2]
      
      # remove time and status target:
      data[[time_var]] = NULL
      data[[cens_var]] = NULL
      
      # setting self$state:
      self$state$tau = tau
      self$state$km = km
      self$state$na = na
      self$state$risk_ind_mean = risk_ind_mean
      self$state$pv_influence = pv_influence
      self$state$pv_test = pv_test
      
      # create regression task:
      task_pv = TaskRegr$new(id = paste0(task$id, "_PV"), backend = data,
                             target = "pv")

      # pass on regrtask
      list(task_pv, NULL)
  },

    .predict = function(input) {
      task = input[[1L]]
      state = self$state
      tau = state$tau
      pv_test = state$pv_test
      
      if (identical(pv_test, "influence")) {
        km = state$km
        na = state$na
        risk_ind_mean = state$risk_ind_mean
        pv_influence = state$pv_influence
        
        # pv = pv_influence(state, task$times(), task$status())
        pv = pv_influence(task$times(), task$status())
      } else {
        pv = NA_real_
      }
    
      data = task$data()
      time_var = task$target_names[1]
      status_var = task$target_names[2]
      
      # remove time and status target:
      
      data[[time_var]] = NULL
      data[[status_var]] = NULL
      
      # add pv to the original data:
      
      data[["pv"]] = pv
      
      # create regression task
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

# mlr_pipeops$add("trafotask_survregr_pv", PipeOpTaskSurvRegrPV)
# register_pipeop("trafotask_survregr_pv", PipeOpTaskSurvRegrPV)
