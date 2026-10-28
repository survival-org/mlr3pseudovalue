# Registration machinery for this mlr3 extension package.
# Sourced first (file name) so that the PipeOp files below can call
# register_pipeop() / register_graph() at load time.
# Follows the pattern used in mlr3proba (R/aaa.R, R/zzz.R).

mlr3pseudovalue_pipeops = new.env()
mlr3pseudovalue_graphs = new.env()

register_pipeop = function(name, constructor) {
  if (name %in% names(mlr3pseudovalue_pipeops)) {
    mlr3misc::stopf("pipeop %s registered twice", name)
  }
  mlr3pseudovalue_pipeops[[name]] = constructor
}

register_graph = function(name, constructor) {
  if (name %in% names(mlr3pseudovalue_graphs)) {
    mlr3misc::stopf("graph %s registered twice", name)
  }
  mlr3pseudovalue_graphs[[name]] = constructor
}

register_mlr3pipelines = function() {
  mlr_pipeops = utils::getFromNamespace("mlr_pipeops", ns = "mlr3pipelines")
  mlr3misc::iwalk(as.list(mlr3pseudovalue_pipeops), function(obj, name) mlr_pipeops$add(name, obj)) # nolint

  mlr_graphs = utils::getFromNamespace("mlr_graphs", ns = "mlr3pipelines")
  mlr3misc::iwalk(as.list(mlr3pseudovalue_graphs), function(obj, name) mlr_graphs$add(name, obj)) # nolint
}

