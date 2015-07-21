






## Parameter space
If parameter space is defined by list of values, it is interpreted as 
all possible combinations of parameters:

```r
experiment <- nl_experiment(
  model_file = fire_model, 
  while_condition = "any? turtles",
  repetitions = 30,
  run_measures = measures(
    percent_burned = "(burned-trees / initial-trees) * 100",
    progress = "max [pxcor] of patches with [pcolor > 0 and pcolor < 55]"
  ),
  param_values = list(
    world_size = c(100, 250),
    density = seq(from = 56, to = 61)
  )
)

experiment$param_space
#>    world_size density
#> 1         100      56
#> 2         250      56
#> 3         100      57
#> 4         250      57
#> 5         100      58
#> 6         250      58
#> 7         100      59
#> 8         250      59
#> 9         100      60
#> 10        250      60
#> 11        100      61
#> 12        250      61
```


*Note: `world_size` is a special parameter name. Instead of seting NetLogo variable 
it adjusts the NetLogo world dimensions.*

Sometimes we do not want all possible combinations of parameter values
(often because all combinations might produce a huge parameter space). 
In this case use **data frame instead of a list** in `nl_experiment` or 
`nl_set_param_space` function to set parameter values. 
Here is a definition of parameter space with different density values and
two world sizes but there are only some of the density values for the big world:


```r
experiment <- nl_set_param_space( experiment,
  param_values = rbind(
    expand.grid(
      world_size = 100,
      density = seq(from = 56, to = 61)
    ),
    expand.grid(
      world_size = 250,
      density = c(58, 59, 60) # only 3 densities for the big world
    )
  )
)

experiment$param_space
#>   world_size density
#> 1        100      56
#> 2        100      57
#> 3        100      58
#> 4        100      59
#> 5        100      60
#> 6        100      61
#> 7        250      58
#> 8        250      59
#> 9        250      60
```


Run the experiment with the `parallel` attribute:

```r
result <- nl_run(experiment, parallel = TRUE)
# Join observations with parameter space values:
dat <- nl_get_run_result(result, add_parameters = TRUE)
```

Plot the results - percent burned as a function of density:

```r
ggplot(dat, mapping = aes(x = factor(density), y=percent_burned, 
                          color = factor(density %in% c(58, 59, 60)))) + 
  geom_boxplot(outlier.shape=NA) +
  geom_jitter(position = position_jitter(width = .1), alpha = 0.3) +
  facet_grid(. ~ world_size, scales = "free_x", space = "free_x") +
  theme(legend.position="none") +
  scale_color_manual(values = c("gray", "black")) +
  ggtitle("Percent burned as a function of density and world size") +
  xlab("Forest density") +
  ylab("Percent burned")
```

![](img/README-p4plot-1.png) 


