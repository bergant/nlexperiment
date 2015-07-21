






## Observations per each simulation step
From statistical point of view, the interesting part of experiment is 
getting some quantitative information. 
This sample demonstrates how to set measures
for each simulation step.

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

Plot of burned forest as a function of time for different forest densities:

```r
# get the observation data for step measures
dat <- nl_get_step_result(result, add_parameters = TRUE) 
# plot the observations
library(ggplot2)
ggplot(dat, mapping = aes(x = tick, y = percent_burned)) + 
  geom_step() +
  facet_grid(. ~ density) +
  ylab("Percent burned")
```

![](img/README-model_step_plot-1.png) 

*Note that `run_id` and `tick` values are included in the `results$step` by default.
Parameter values are included only by reference to `parameter_space_id`. The 
function `nl_get_step_result` joins parameter space to observation data.*

