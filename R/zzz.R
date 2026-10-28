#' @import data.table
#' @import checkmate
#' @import mlr3misc
#' @import mlr3
#' @import mlr3proba
#' @importFrom R6 R6Class
#' @importFrom stats predict
"_PACKAGE"

# Registration machinery for mlr3 extension packages.
# Follows the pattern used in mlr3proba (R/aaa.R, R/zzz.R), so that the PipeOps
# and graphs defined here appear in the mlr3pipelines dictionaries.

.onLoad = function(libname, pkgname) { # nolint
  if (requireNamespace("mlr3pipelines", quietly = TRUE)) {
    register_mlr3pipelines()
  }
  setHook(packageEvent("mlr3pipelines", "onLoad"), function(...) register_mlr3pipelines(),
    action = "append")
}

.onUnload = function(libpath) { # nolint
  event = packageEvent("mlr3pipelines", "onLoad")
  hooks = getHook(event)
  pkgname = mlr3misc::map_chr(hooks[-1L], function(x) environment(x)$pkgname)
  setHook(event, hooks[pkgname != "mlr3pseudovalue"], action = "replace")

  mlr_pipeops = utils::getFromNamespace("mlr_pipeops", ns = "mlr3pipelines")
  mlr3misc::walk(names(mlr3pseudovalue_pipeops), function(nm) mlr_pipeops$remove(nm))
  mlr_graphs = utils::getFromNamespace("mlr_graphs", ns = "mlr3pipelines")
  mlr3misc::walk(names(mlr3pseudovalue_graphs), function(nm) mlr_graphs$remove(nm))
}
