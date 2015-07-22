


![nlexperiment](img/logo.png)
Define and run NetLogo experiments in R

## About
The goal of **nlexperiment** is to make
exploring agent based models in [NetLogo](http://ccl.northwestern.edu/netlogo/) (Wilensky 1999) as simple as possible (like NetLogo 
[Behavior Space](http://ccl.northwestern.edu/netlogo/docs/behaviorspace.html) tool)
while keeping complex functionalities available for
advanced users.
It uses [RNetLogo](https://cran.r-project.org/web/packages/RNetLogo/)
package (Thiele 2014) as an interface to NetLogo.

## Installation


```r
library(devtools)
install_github("bergant/nlexperiment")
```









## Simple experiment with fire
This sample experiment with NetLogo Fire model (Wilensky 1997) demonstrates
how to create and run minimal experiment. It runs the model with three parameter
values (forest density) and exports final NetLogo views to image files:


```r
library(nlexperiment)
# Set the path to your NetLogo instalation
nl_netlogo_path("c:/Program Files (x86)/NetLogo 5.1.0/") 

# Create NetLogo experiment object
experiment <- nl_experiment(
  model_file = file.path(nl_netlogo_path(), 
                         "models/Sample Models/Earth Science/Fire.nlogo"), 
  while_condition = "any? turtles",
  param_values = list(density = c(57, 59, 61)),
  random_seed = 1,
  export_view = TRUE
)
```

Run the experiment:

```r
result <- nl_run(experiment)
```

Find paths to the exported view image files in `result$export` or just display them by calling `nl_show_view` function:

```r
nl_show_view(result)
```

![](img/README-model_view-1.png) ![](img/README-model_view-2.png) ![](img/README-model_view-3.png) 










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

*Note: values `run_id` and `tick` are included in the `results$step` by default.
Parameter values are included only by reference to `parameter_space_id`. The 
function `nl_get_step_result` joins parameter space to observation data.*










## Observations per each simulation run
In this example

* two measures _per simulation run_ are defined (values are reported at the end 
of each simulation run),
* the model will be run repetedly `30` times for every parameter value
* and the model is running with `parallel` option (to save some time)


```r
experiment <- nl_experiment(
  model_file = file.path(nl_netlogo_path(), 
                         "models/Sample Models/Earth Science/Fire.nlogo"), 
  while_condition = "any? turtles",
  repetitions = 30,
  run_measures = measures(
    percent_burned = "(burned-trees / initial-trees) * 100",
    progress = "max [pxcor] of patches with [pcolor > 0 and pcolor < 55]"
  ),
  param_values = list(
    density = seq(from = 55, to = 62, by = 1)
  )
)
```

Run the experiment with `parallel` option:


```r
result <- nl_run(experiment, parallel = TRUE)
```

Plot the results - percent burned as a function of density:

```r
# Join observations with parameter space values:
dat <- nl_get_run_result(result, add_parameters = TRUE)
# plot percent burned by density
library(ggplot2)
ggplot(dat, mapping = aes(x = factor(density), y = percent_burned) ) + 
  geom_violin() +
  geom_jitter(position = position_jitter(width = .1), alpha = 0.3) +
  labs(x = "Forest density", y = "Percent burned")
```

![](img/README-plot_run_density-1.png) 

Fire progress from left to right as a function of density:

```r
ggplot(dat, mapping = aes(x = factor(density), y = progress/250 + 0.5) ) + 
  geom_jitter(position = position_jitter(width = .1), alpha = 0.3)  +
  theme_minimal() +
  labs(x = "Forest density", y = "Fire progress")
```

![](img/README-plot_run_rightmost-1.png) 










## Parameter space
When parameter space is defined by list of values, it is interpreted as 
all possible combinations of parameters. In this example, model is set
to run with all combination of `world_size` and `density`:

*Note: `world_size` is a special parameter name. Instead of setting NetLogo variable 
it changes the NetLogo world dimensions.*


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
```

This experiment has now 12 rows in the parameter space.
With 30 repetitions this means 
360 simulation runs.



Sometimes we do not want to run the model for all combinations of parameter values
(often because it would yield a huge parameter space and hours of waiting for
simulations to end). 
In this case use **data frame instead of a list** in `nl_experiment` or 
`nl_set_param_space` function to set parameter values. 
Here is an example of parameter space with different density values and
two world sizes where only some of the density values are used for the big sized world:


```r
# Instead of creating new experiment we can change the original experiment
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
# Print the parameter space values:
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

Plot the results - with world size as facets:

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











## Mapping parameters
NetLogo identifiers may include some ASCII characters `(?=*!<>:#+/%$^'&-)`
that makes the R part of data manipulation rather uncomfortable. 
The following example is using Ant model (Wilensky 1997) to show 
**how to use nice names in R and map them to NetLogo variables**.


```r
experiment <- nl_experiment(
  model_file = file.path(nl_netlogo_path(), 
                         "models/Sample Models/Biology/Ants.nlogo"), 
  max_ticks = 150,
  step_measures = measures(
    pile1 = "sum [food] of patches with [pcolor = cyan]",  
    pile2 = "sum [food] of patches with [pcolor = sky]",  
    pile3 = "sum [food] of patches with [pcolor = blue]"  
  ),
  param_values = list(
    population = 125,
    diffusion_rate = c(50, 60),
    evaporation_rate = c(5, 10, 15)
  ),
  mapping = c(
    diffusion_rate = "diffusion-rate",
    evaporation_rate = "evaporation-rate"
    ),
  random_seed = 1,
  export_view = TRUE
)
```

Run experiment

```r
results <- nl_run(experiment)
```

Show views

```r
nl_show_view(results)
```

![](img/README-p5ShowViews-1.png) ![](img/README-p5ShowViews-2.png) ![](img/README-p5ShowViews-3.png) ![](img/README-p5ShowViews-4.png) ![](img/README-p5ShowViews-5.png) ![](img/README-p5ShowViews-6.png) 

Show remaining food by parameter space and food piles

```r
library(tidyr)
dat <- nl_get_step_result(results)
dat <- tidyr::gather(dat, pile, value, pile1, pile2, pile3)

library(ggplot2)
ggplot(dat, aes(x = tick, y = value, color = pile) ) +
  geom_line() +
  facet_grid(diffusion_rate ~ evaporation_rate)
```

![](img/README-p5plot-1.png) 


## References

* Wilensky, U. (1999). NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

* Thiele, J. (2014) R Marries NetLogo: Introduction to the RNetLogo Package. Journal of Statistical Software 58(2) 1-41. http://www.jstatsoft.org/v58/i02/

* Wilensky, U. (1997). NetLogo Fire model. http://ccl.northwestern.edu/netlogo/models/Fire. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

* Wilensky, U. (1997). NetLogo Ants model. http://ccl.northwestern.edu/netlogo/models/Ants. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

* The ideas and principles of NetLogo experiment definition is taken from
the NetLogo's Behavior Space tool
http://ccl.northwestern.edu/netlogo/docs/behaviorspace.html

* The parallel implementation of `nl_run` function is based on the RNetLogo vignette
https://cran.r-project.org/web/packages/RNetLogo/vignettes/parallelProcessing.pdf

* Rickert, J. (2014) Agent Based Models and RNetLogo http://blog.revolutionanalytics.com/2014/07/agent-based-models-and-rnetlogo.html
