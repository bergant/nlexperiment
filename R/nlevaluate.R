
nl_eval_init <- function(experiment) {
  nl_run_init(FALSE, nl_netlogo_path(), experiment$model_file)
}

nl_eval_run <- function(experiment, param_set) {
  nl_single_run(experiment, param_set)
}

nl_eval_close <- function() {
  nl_run_end()
}
