#' @title PipeOpPredRegrSurvPV
#' @name mlr_pipeops_trafopred_classifsurv_disctime
#'
#' @description
#' Transform [PredictionRegr][mlr3::PredictionRegr] to [PredictionSurv] by
#'
#'
#'
#' @section Dictionary:
#' This [PipeOp][mlr3pipelines::PipeOp] can be instantiated via the
#' [dictionary][mlr3misc::Dictionary] [mlr3pipelines::mlr_pipeops]
#' or with the associated sugar function [mlr3pipelines::po()]:
#' ```
#' PipeOpPredRegrSurvPV$new()
#' mlr_pipeops$get("---")
#' po("---")
#' ```
#'
#' @section Input and Output Channels:
#' The input is a [PredictionRegr][mlr3::PredictionRegr] and a
#' ---- .
#' The output is the input [PredictionRegr][mlr3::PredictionRegr]
#' transformed to a [PredictionSurv].
#' Only works during prediction phase.
#'
#' @references
#' `r format_bib("----")`
#' @seealso
#' @family Transformation PipeOps
#' EVTL EXAMPLE CODE
#' @export
PipeOpPredRegrSurvPV <- R6Class(
  "PipeOpPredRegrSurvPV",
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
        train = c("NULL", "data.table"),
        predict = c("PredictionRegr", "data.table")
    #   TODO: NACHSCHAUEN, ob Prediction wirklich Liste ist oder doch ein Data Table !
        ),
      ouput = data.table(
        name = "output",
        train = "NULL",
        predict = "PredictionSurv"
        )
      )
    },

    # Überlegen, ob hier active function wirklich gebraucht wird (ist Predictiontype "crank" ? )
    # active = function(
    #
    # ),

    private = list(
      .predict = function(input) {
        # TODO: MEHR NACHVOLLZIEHEN
        pred = input[[1]] # Regression Prediction
        data = input[[2]] # Data - for prediction (?)

        # TODO: NACHSCHAUEN; WIE PREDICTION ABGEBILDET WERDEN
        p = PredictionSurv$new(
          row_ids = data$row_ids(),
          true = Surv(data$times, data$status)
        )

      },

      .train = function(input) {
        self$state = list()
        list(input)
      }
    )
  )
)

