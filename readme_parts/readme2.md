






## Temporal measures
Set measures for each simulation step:

```r
experiment <- nl_experiment(
  model_file = file.path(nl_netlogo_path(), 
                         "models/Sample Models/Earth Science/Fire.nlogo"), 
  while_condition = "any? turtles",
  param_values = list(density = c(57, 59, 61)),
  random_seed = 1,
  step_measures = measures(
    percent_burned = "(burned-trees / initial-trees) * 100"
  )
)
```

Run the experiment:

```r
result <- nl_run(experiment)
```

To analyse the results from observations 
use `nl_get_step_result`:


```r
# get the observation data for step measures
dat <- nl_get_step_result(result)
head(dat)
#>   density param_set_id percent_burned step_id run_id
#> 1      57            1      0.4030340       1      1
#> 2      57            1      0.6437348       2      1
#> 3      57            1      0.8788379       3      1
#> 4      57            1      1.0859526       4      1
#> 5      57            1      1.3070615       5      1
#> 6      57            1      1.5113773       6      1
```

Or use `nl_show_step` to plot 
observations based on temporal measures:


```r
# get the observation data for step measures
nl_show_step(result, x_param = "density")
```

![](img/README-model_step_plot-1.png) 



*Note: values `run_id` and `step_id` are included in the `result$step` by default.
Parameter values are included only by reference to `parameter_set_id`. The 
function `nl_get_result` joins parameter sets to observation data.*

