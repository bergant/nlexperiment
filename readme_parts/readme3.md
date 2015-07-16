






## Observations per each simulation run
Create NetLogo experiment object with defined *run* measure (percent burned)
and parameter values - parameter density goes from 55 to 62.

```r
experiment <- nl_experiment(
  model_file = fire_model, 
  while_condition = "any? turtles",
  repetitions = 4,
  run_measures = measures(
    percent_burned = "(burned-trees / initial-trees) * 100",
    progress = "max [pxcor] of patches with [pcolor > 0 and pcolor < 55]"
  ),
  param_values = list(
    density = seq(from = 55, to = 62, by = 1)
  )
)
```

Run the experiment:

```r
result <- nl_run(experiment)
# Join observations with parameter space values:
dat <- nl_get_run_result(result)
```

Plot the results - percent burned as a function of density:

```r
library(ggplot2)
# plot percent burned by density
ggplot(dat, mapping = aes(x = factor(density), y = percent_burned) ) + 
  geom_violin() +
  geom_jitter(position = position_jitter(width = .2)) 
```

![](img/README-plot_run_density-1.png) 

Fire progress from left (-125) to right (125) as a function of density:

```r
ggplot(dat, mapping = aes(x = factor(density), y = progress) ) + 
  geom_jitter(position = position_jitter(width = .2)) +
  theme_minimal()
```

![](img/README-plot_run_rightmost-1.png) 

