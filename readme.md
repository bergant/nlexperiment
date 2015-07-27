


![nlexperiment](img/logo.png)

####Define and run NetLogo experiments in R##

The goal of **nlexperiment** is to make
exploring agent based models in [NetLogo](http://ccl.northwestern.edu/netlogo/) (Wilensky 1999) as simple as possible (like NetLogo 
[Behavior Space](http://ccl.northwestern.edu/netlogo/docs/behaviorspace.html) tool)
while keeping complex functionalities available for
advanced users.
It uses [RNetLogo](https://cran.r-project.org/web/packages/RNetLogo/)
package (Thiele 2014) as an interface to NetLogo.

## Installation

`library(devtools)`   
`install_github("bergant/nlexperiment")`










## Simple experiment with fire
This sample experiment with NetLogo Fire model (Wilensky 1997) demonstrates
how to create and run minimal experiment. It runs the model with three parameter
values (forest density) and exports final NetLogo views to image files:


```r
library(nlexperiment)
# Set the path to your NetLogo instalation
nl_netlogo_path("c:/Program Files (x86)/NetLogo 5.2.0/") 

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

Find paths to the exported view image files in `result$export` or just display them by calling `nl_show_views_grid` function:


```r
library(ggplot2)
nl_show_views_grid(result, "density")
```

![](img/README-model_view-1.png) 










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
dat <- nl_get_result(result, type = "step") 
# plot the observations
library(ggplot2)
ggplot(dat, mapping = aes(x = step_id, y = percent_burned)) + 
  geom_step() +
  facet_grid(. ~ density) +
  labs(y = "Percent burned", x = "Iteration")
```

![](img/README-model_step_plot-1.png) 

*Note: values `run_id` and `step_id` are included in the `results$step` by default.
Parameter values are included only by reference to `parameter_space_id`. The 
function `nl_get_result` joins parameter space to observation data.*










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
# Join observations with parameter space values:
dat <- nl_get_run_result(result, add_parameters = TRUE)
```

Plot the results - percent burned as a function of density:

```r
# plot percent burned by density
library(ggplot2)
ggplot(dat, mapping = aes(x = factor(density), y = percent_burned) ) + 
  geom_violin() +
  #geom_jitter(position = position_jitter(width = .1), alpha = 0.3) +
  labs(x = "Forest density", y = "Percent burned")
```

![](img/README-p3plot1-1.png) 

Fire advances from left to right. It is interesting to observe
final fire position (left border = 0 and right = 1) as a function of density.

```r
ggplot(dat, mapping = aes(x = factor(density), y = progress/250 + 0.5) ) + 
  geom_jitter(position = position_jitter(width = .1), alpha = 0.3)  +
  theme_minimal() +
  labs(x = "Forest density", y = "Fire progress")
```

![](img/README-p3plot2-1.png) 










## Reading agent variables
While `run_measures` (see [Observation per run](#observations-per-each-simulation-run)) is good enough for aggregate measures, sometimes we need values for each agent individually.
Example shows usage of `agents_after` parameter to get vertices from Preferential attachment
model.


```r
experiment <- nl_experiment(
  model_file = file.path(nl_netlogo_path(), 
                         "models/Sample models/Networks/Preferential attachment.nlogo"), 
  iterations = 50,
  agents_after = list(
    vertices = agent_set(
      vars = c("who", "xcor", "ycor"), 
      agents = "turtles"),
    edges = agent_set(
      vars = c(e1 = "[who] of end1", e2 ="[who] of end2"), 
      agents = "links")
  ),
  repetitions = 2,
  random_seed = c(42, 69)
)
```

_Note:_

* _Here random seed is defined as a vector. It is applied to each repetition respectively._
* _Variables in agent set may or may not include variable names._
* _Element `agents_before` is analogous tp `agents_after` - it just gets the data before model runs.)_

Run experiment

```r
result <- nl_run(experiment)
#> Warning: Parameter space not defined. Using default parameters
```

Show graph by using **igraph** package:


```r
library(igraph, quietly = TRUE, warn.conflicts = FALSE)
par(mfrow=c(1,2), mai=c(0,0,0,0))
for(i in 1:experiment$run_options$repetitions) {
  g_edges <- subset(result$agents_after$edges, run_id == i)
  g1 <- graph.data.frame(g_edges, directed = FALSE)
  V(g1)$size <- sqrt(degree(g1))*6
  V(g1)$label <- ifelse(as.numeric(V(g1)$name) < 4, as.numeric(V(g1)$name), NA)
  plot.igraph(g1, margin = 0, 
              vertex.label.font = V(g1)$size * 0.07,
              vertex.label.color = "white",
              vertex.color="darkseagreen4",
              edge.color = "gray",
              vertex.frame.color="#ffffff",
              edge.curved=.1
  )
}
```

![](img/README-p6Sigraph-1.png) 

_Note:_

* _Data frame `result$agents_after$edges` includes variables from all simulation runs._
* _Use `param_space_id` and/or `run_id` columns to subset individual run or parameter combination_














## Simple parameter space definition
The following example is using Ant model (Wilensky 1997) to show 
simple parameter space definition and parameter mapping.


```r
experiment <- nl_experiment(
  model_file = file.path(nl_netlogo_path(), 
                         "models/Sample Models/Biology/Ants.nlogo"), 
  iterations = 150,
  step_measures = measures(
    pile1 = "sum [food] of patches with [pcolor = cyan]",  
    pile2 = "sum [food] of patches with [pcolor = sky]",  
    pile3 = "sum [food] of patches with [pcolor = blue]"  
  ),
  param_values = list(
    population = 125,
    diffusion_rate = c(50, 70),
    evaporation_rate = c(5, 10, 15)
  ),
  mapping = c(
    diffusion_rate = "diffusion-rate",
    evaporation_rate = "evaporation-rate"
    ),
  random_seed = 2,
  export_view = TRUE
)
```

_Note:_

* _When parameters are defined as a list of value vecors, parameter space is
  constructed as combination of all possible parameter sets
* _It is not required to include all parameters in mapping. Variable `population` 
is used as is._
* _NetLogo identifiers may include some ASCII characters `(?=*!<>:#+/%$^'&-)`
that makes the R part of data manipulation rather uncomfortable._
* _Element `mapping` maps `difussion_rate` and `evaporation_rate` names to NetLogo variables `diffusion-rate` and `evaporation-rate`._


Run experiment

```r
results <- nl_run(experiment)
```

Show views

```r
nl_show_views_grid(results, "evaporation_rate", "diffusion_rate")
```

![](img/README-p5ShowViews-1.png) 

Show remaining food by difussion rate and evaporation rate for all food piles

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










## Explicit definition of parameter sets (fur model)
The following example is using NetLogo Fur model (Wilensky 2003) to show 
explicit parameter space definition and parameter mapping.

There are 5 parameters in the NetLogo Fur model:

* ratio 
* outer-radius-y
* inner-radius-y
* outer-radius-x
* inner-radius-x

But considering constraints and model symmetry
we can reduce it only to ratio and ellipse aspect ratio. It is sufficient to
create parameter sets based on combinations of

* `radius_diff` (the difference between x and y radius) and
* `ratio` (the inhibitor concentration parameter)
* `gap` (distance between inner and outer circle)

For simplicity lets keep the `gap` between the circles constant:


```r
# prepare parameter space
param_sets <- 
  expand.grid(
    gap = 3, 
    radius_diff = seq(from = 0, to = 2, by = 0.5), 
    ratio = seq(from = 0.30, to = 0.65, by = 0.05)
)
# add NetLogo variables
param_sets <- 
  within(param_sets, {
    inner_radius_x <- 3
    outer_radius_x <- 3 + gap
    inner_radius_y <- inner_radius_x + radius_diff
    outer_radius_y <- inner_radius_y + gap
  })

mapping <- setNames(gsub("_", "-", names(param_sets)),names(param_sets))
mapping[c("gap", "radius_diff")] <- c("","")

#knitr::kable(as.data.frame(mapping), caption = "Mapping", format = "markdown")
cbind(mapping)
#>                mapping         
#> gap            ""              
#> radius_diff    ""              
#> ratio          "ratio"         
#> outer_radius_y "outer-radius-y"
#> inner_radius_y "inner-radius-y"
#> outer_radius_x "outer-radius-x"
#> inner_radius_x "inner-radius-x"
```

_Note:_ 

* _Mapping is here done by converting the `_` characters to `-`._ 
* _Variables `gap` and `radius_diff` should be mapped to empty string._

Define experiment:


```r
experiment <- nl_experiment( 
  model_file = file.path(nl_netlogo_path(), 
                         "models/Sample Models/Biology/Fur.nlogo"), 
  iterations = 20,
  repetitions = 1,
  param_values = param_sets,
  mapping = mapping,  
  export_view = TRUE,
  patches_after = list(
    fur = patch_set(vars = "pcolor", patches = "patches")
  ),
  random_seed = 3
)
```

_Note:_ 

* _Element `param_values` is now a data frame with explicit parameter sets_ 


Run experiment

```r
result <- nl_run(experiment, parallel = TRUE, max_cores = 3) 
```

Show resulting fur patterns:

```r
library(ggplot2)
nl_show_views_grid(
  result, x_param = "ratio", y_param = "radius_diff", img_gap = 0.01) + 
  ylab("radius_y - radius_x") +
  ggtitle("Fur patterns")
```

![](img/README-p7ShowViews-1.png) 










## Explore parameter space with criteria function
Getting all the observations per iterations steps in the memory may
exceed the memory capacity. Sometime it is better to store just
some aggregated values per each run.

This example uses Hoopoes model taken from Thiele, Kurth & Grimm (2014).
Here the approach is the same as in the article (chapters *Factorial design*
and *Latin hypercube sampling*) 
except using the **nlexperiment** package framework instead of original R scripts.
[Reading](http://jasss.soc.surrey.ac.uk/17/3/11.html) 
the article will improve understanding of the following tutorial.

Define an experiment:


```r
experiment <- nl_experiment( 
  model_file = 
    system.file("netlogo_models/SM2_Hoopoes.nlogo", package = "nlexperiment"),

  setup_commands = c("setup", "repeat 24 [go]"), # 2 years of warming-up
  go_command = "repeat 12 [go]",                 # iteration is per year
  iterations = 20,                               # run for 20 years
  repetitions = 10,                              # repeat simulation 10 times

  param_values = list(
    scout_prob = seq(from = 0.00, to = 0.50, by = 0.05),
    survival_prob = seq(from = 0.950, to = 1.000, by = 0.005)
  ),
  mapping = c(
    scout_prob = "scout-prob", 
    survival_prob = "survival-prob"
  ),
  step_measures = measures(
    year = "year",
    abund = "month-11-count",
    alpha = "month-11-alpha",
    patches_count = "count patches"
  ),
  criteria_function = function(result) {         # aggregate measures
    with(result$step, c(
      abundance = mean(abund),
      variation = sd(abund),
      vacancy   = mean(alpha / patches_count)
    ))
  }
)
```

_Note:_ 

* _The iteration step is set to 12 times calling `go` procedure (12 months of simulation)_
* _In this case we are using all combinations of parameter values_
* _Criteria function receives all the results from model run and returns numerical vector_ 

Run experiment:

```r
result <- nl_run(experiment, parallel = TRUE)
# get the data (criteria)
dat <- nl_get_result(result, type = "criteria")
```

The result includes the data per each run (10 values per parameter set).
We have to aggregate it.
Additionally the categorical criteria is added with boolean expressions
(see chapter *Preliminaries: Fitting criteria for the example model*):


```r
prepare_criteria <- function(dat) {
  # aggregate values for all simulation repetitions
  dat <- aggregate(
    formula = cbind(abundance, variation, vacancy) ~ scout_prob + survival_prob, 
    data = dat, FUN = mean)
  
  # categorical criteria
  dat <- within(dat, {
     c_abundance = abundance > 115 & abundance < 135
     c_variation = variation > 10 & variation < 15
     c_vacancy = vacancy > 0.15 & vacancy < 0.30
  })
}
dat <- prepare_criteria(dat)
```

Plot categorical criteria on parameter space:

```r
library(ggplot2)

ggplot(dat, aes(x = scout_prob, y = survival_prob)) +
  geom_point() +
  geom_point(data = subset(dat, c_abundance), color = "red", size = 7, shape = 2) +
  geom_point(data = subset(dat, c_variation), color = "steelblue", size = 7, shape = 3) +
  geom_point(data = subset(dat, c_vacancy), color = "darkgreen", size = 7, shape = 4) +
  theme_minimal()
```

![](img/README-p8SplotParameterSpace-1.png) 

Instead of exploring parameter space with (full) factorial design 
we can use sampling methods. Below is an example with **latin hypercube sampling** 
from **tgp** package:


```r
# use Latin Hypercube sampling to sample 50 parameter sets
library(tgp)
param_sets <- lhs(n=50, rect=matrix(c(0.0, 0.95, 0.5, 1.0), 2)) 
```

To change only parameter sets of experiment object use `nl_set_param_space`:

```r
# change parameters of existing experiment
experiment <- nl_set_param_space( experiment,
  param_values = 
    setNames(
      as.data.frame(param_sets), 
      c("scout_prob", "survival_prob")
    )
)
```

Run the model with new parameters:

```r
result2 <- nl_run(experiment, parallel = TRUE)
# get the data (criteria)
dat2 <- nl_get_result(result2, type = "criteria")
```

The rest of  data munging and plotting is same as before:

```r
# aggregate values for all simulation repetitions
dat2 <- prepare_criteria(dat2)

ggplot(dat2, aes(x = scout_prob, y = survival_prob)) +
  geom_point() +
  geom_point(data = subset(dat2, c_abundance), color = "red", size = 7, shape = 2) +
  geom_point(data = subset(dat2, c_variation), color = "steelblue", size = 7, shape = 3) +
  geom_point(data = subset(dat2, c_vacancy), color = "seagreen", size = 7, shape = 4) +
  theme_minimal()
```

![](img/README-p8Aggregate2-1.png) 





## References

* Wilensky, U. (1999). NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

* Thiele, J. (2014) R Marries NetLogo: Introduction to the RNetLogo Package. Journal of Statistical Software 58(2) 1-41. http://www.jstatsoft.org/v58/i02/

* Thiele, J. C., Kurth, W., & Grimm, V. (2014). Facilitating Parameter Estimation and Sensitivity Analysis of Agent-Based Models: A Cookbook Using NetLogo and R. Journal of Artificial Societies and Social Simulation, 17(3), 11. http://jasss.soc.surrey.ac.uk/17/3/11.html

* Wilensky, U. (1997). NetLogo Fire model. http://ccl.northwestern.edu/netlogo/models/Fire. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

* Wilensky, U. (1997). NetLogo Ants model. http://ccl.northwestern.edu/netlogo/models/Ants. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

* Wilensky, U. (2003). NetLogo Fur model. http://ccl.northwestern.edu/netlogo/models/Fur. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

* Wilensky, U. (2005). NetLogo Preferential Attachment model. http://ccl.northwestern.edu/netlogo/models/PreferentialAttachment. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

* Wilensky, U., & Shargel, B. (2002). BehaviorSpace. Center for Connected Learning and Computer Based Modeling, Northwestern University, Evanston, IL. http://ccl.northwestern.edu/netlogo/behaviorspace.html


## Resources

* nlexperiment is using RNetLogo package to connect to NetLogo
https://cran.r-project.org/web/packages/RNetLogo/index.html

* The parallel implementation of `nl_run` function is based on the RNetLogo vignette
https://cran.r-project.org/web/packages/RNetLogo/vignettes/parallelProcessing.pdf

* Basic ideas and principles of NetLogo experiment definition is taken from
the NetLogo's Behavior Space tool
http://ccl.northwestern.edu/netlogo/docs/behaviorspace.html

* This tutorial follows examples and methods from Thiele, Kurth & Grimm (2014)
article and its Supplementary Material
http://sourceforge.net/projects/calibrationsensitivityanalysis/

* This document is created with knitr package
https://cran.r-project.org/web/packages/knitr/index.html

* Graphics are created with ggplot2 package
https://cran.r-project.org/web/packages/ggplot2/index.html

