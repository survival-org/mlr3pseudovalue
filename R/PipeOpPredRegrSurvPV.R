#' @title PipeOpPredRegrSurvPV
#' @name mlr_pipeops_trafopred_regrsurv_pv
#'
#' @description
#' Transform [PredictionRegr][mlr3::PredictionRegr] to [PredictionSurv].
#' As the PV-reduction directly uses the survival quantity for a regression task,
#' the prediction out of the regression model can be used directly.
#' See as reference: Piller et al. 2026. 
#'
#' @section Dictionary:
#' This [PipeOp][mlr3pipelines::PipeOp] can be instantiated via the
#' [dictionary][mlr3misc::Dictionary] [mlr3pipelines::mlr_pipeops]
#' or with the associated sugar function [mlr3pipelines::po()]:
#' ```
#' PipeOpPredRegrSurvPV$new()
#' mlr_pipeops$get("trafopred_regrsurv_pv")
#' po("trafopred_regrsurv_pv")
#' ```
#'
#' @section Input and Output Channels:
#' The input is a [PredictionRegr][mlr3::PredictionRegr] and a
#' [data.table][data.table::data.table] containing the transformed data.
#' The output is the input [PredictionRegr][mlr3::PredictionRegr]
#' transformed to a [PredictionSurv].
#' Only works during prediction phase.
#'
#' @references
#' `r format_bib("piller_2026")`
#' @seealso
#' @family Transformation PipeOps
#' EVTL EXAMPLE CODE
#' @export
PipeOpPredRegrSurvPV <- R6Class("PipeOpPredRegrSurvPV",
  inherit = mlr3pipelines::PipeOp,

  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' EVTL id parameter explain
    initialize = function(id = "trafopred_regrsurv_pv") {
      super$initialize(
      id = id,
      input = data.table(
        name = c("input", "data"),
        train = c("NULL", "NULL"),
        predict = c("PredictionRegr", "data.table")
    #   TODO: NACHSCHAUEN, ob Prediction wirklich Liste ist oder doch ein Data Table !
        ),
      output = data.table(
        name = "output",
        train = "NULL",
        predict = "PredictionSurv"
        )
      )
    }
  ),

   active = list(
     #' @field predict_type (`character(1)`)\cr
     #' Returns the active predict type of this PipeOp, which is `"crank"`
     predict_type = function(rhs) {
       if (!missing(rhs) && !identical(rhs, "crank")) stop("predict_type is read-only!")
       "crank"
     }
   ),

    private = list(
      .predict = function(input) {
        # Input = list with PredRegr and D_test
        pred = input[[1]] # regression prediction
        data = input[[2]] # testData

        # Überlegung: Pred gibt enviroment von PredictionRegr wieder
        # -> Field pv (da reponse variable) sollte exisiteren -->
        # damit lässt sich Matrix bilden, mit Spalte = t, Zeilen = pv.
        # PredictionSurv - help-side,
        surv = matrix(data = pred$pv, ncol = 1)
        colnames(surv) = data$tau

        # Prediction of Regression Task gives directly the prediction for
        # Survival Task
        p = PredictionSurv$new(
          row_ids = data$row_ids(),
          true = Surv(data$times, data$status),
          distr = surv,
        )

        list(p)
      },

      .train = function(input) {
        self$state = list()
        list(input)
      }
  )
)

# register_pipeop("trafopred_regrsurv_pv", PipeOpPredRegrSurvPV)
