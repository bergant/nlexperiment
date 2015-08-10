






## Flocking example - Temporal (step) measures

This sample experiment is using NetLogo Flocking model (Wilensky, 1998).
Two measures of self-organization are defined:

* __converergence__ is based on variance of birds' orientations (Stonedahl, 2011)
* __mean crowding__ is average group size as experienced by individual (Reiczigel et al., 2008)

Define experiment with `nl_experiment` function:


```r
experiment <- nl_experiment( 
  model_file = 
    file.path(nl_netlogo_path(), "models/Sample Models/Biology/Flocking.nlogo"),
  setup_commands = 
    c("setup", "repeat 10 [go]"),
  iterations = 100,

  param_values = list(
    world_size = 50,
    population = 100,
    vision = c(2,6)
  ),

  step_measures = measures(
    converged = "1 - 
      (standard-deviation [dx] of turtles + 
       standard-deviation [dy] of turtles) / 2",
    mean_crowding = 
      "mean [count flockmates + 1] of turtles"
  ),
  repetitions = 2,
  random_seed = c(1, 4),
  export_view = TRUE
)
```

Run experiment


```r
result <- nl_run(experiment)      
```

Plot results


```r
# get observation of step measures
library(ggplot2) 
nl_show_views_grid(result, x_param = "vision", y_param = "run_id", img_gap = 0.01) +
  labs(title = "Simulation views")
nl_show_step(
  result, y =  "converged", x_param = "vision",  title = "Convergence")
nl_show_step(
  result, x = "mean_crowding", y =  "converged", 
  x_param = "vision", y_param = "run_id", title = "Two measures plot")
nl_show_step(
  result, y =  "mean_crowding", x_param = "vision", title = "Mean crowding")
```

![](img/README-pFlockPlot1-1.png) ![](img/README-pFlockPlot1-2.png) ![](img/README-pFlockPlot1-3.png) ![](img/README-pFlockPlot1-4.png) 


## Visualize temporal measures on parameter space 
It is obvious from the plots above that initial (random) positions influence the 
resulting measures. Below is an attempt to find the values 
for *min_separation* and *max_align_turn* parameters 
with best convergence:


```r
experiment <- nl_experiment( 
  model_file = 
    file.path(nl_netlogo_path(), "models/Sample Models/Biology/Flocking.nlogo"),
  setup_commands = c("setup", "repeat 10 [go]"),
  #go_command = c("repeat 5 [go]"),
  iterations = 50,

  param_values = list(
    world_size = 50,
    population = 100,
    min_separation = seq(from = 0, to = 3, by = 1),
    max_align_turn = c(10, 15, 20)
  ),
  mapping = c(
    min_separation = "minimum-separation",
    max_align_turn = "max-align-turn"),

  step_measures = measures(
    converged = "1 - 
      (standard-deviation [dx] of turtles + 
       standard-deviation [dy] of turtles) / 2"
  ),
  repetitions = 30, 
  random_seed = 1:30
)
```

Run experiment:

```r
result2 <- nl_run(experiment, parallel = TRUE)   
```

Plot temporal measure with 2 varying parameters:

```r
library(ggplot2)
nl_show_step(result2, y_param = "max_align_turn", x_param = "min_separation",
                  color = NA,  alpha = 0.3)
```

![](img/README-pFlockPlot2-1.png) 

## Searching parameter space - categorical criteria

Create experiment with evaluation criteria:

```r
experiment <- nl_experiment( 
  model_file = 
    file.path(nl_netlogo_path(), "models/Sample Models/Biology/Flocking.nlogo"),
  setup_commands = c("setup", "repeat 100 [go]"),
  iterations = 5,

  param_values = list(
    world_size = 50,
    population = 80,
    vision = c(3,6),
    min_separation = seq(from = 0, to = 4, by = 0.5),
    max_align_turn = seq(from = 0, to = 20, by = 2.5)
  ),
  mapping = c(
    min_separation = "minimum-separation",
    max_align_turn = "max-align-turn"),

  step_measures = measures(
    converged = "1 - 
      (standard-deviation [dx] of turtles + 
       standard-deviation [dy] of turtles) / 2",
    mean_crowding = 
      "mean [count flockmates + 1] of turtles"
  ),
  eval_criteria = criteria(
    c_converged = mean(step$converged),
    c_mcrowding = mean(step$mean_crowding)
  ),

  repetitions = 10,                              # repeat simulations 10 times
  random_seed = 1:10,
  
  eval_aggregate_fun = mean                     # aggregate over repetitions
)
```

_Note:_

* _Parameter sets are defined with values for each parameter which
  is interpreted as full factorial design (all parameter values combination)_
* _Criteria expression uses the result from step measures (`step$` prefix)_
* _Aggregate function (`eval_aggregate_fun`) specifies that the results
   from criteria function should be aggregated 
   (over all 10 repetitions for each parameter set)_

Run the experiment:

```r
result3 <- nl_run(experiment, parallel = TRUE) 
```

If we were interested in parameters where we get small sized flocks 
which fly in same direction
we could define criteria based on crowding and direction convergence
like this:


```r
dat <- nl_get_criteria_result( 
  result3,
  are_grouped = c_mcrowding >= 5 & c_mcrowding < 11,
  are_aligned = c_converged > 0.7
)

ggplot( dat, aes(x = min_separation, y = max_align_turn) ) +
  geom_point() +
  geom_point(data = subset(dat, are_aligned), color = "red", size = 5, shape = 2) +
  geom_point(data = subset(dat, are_grouped), color = "blue", size = 5, shape = 3) +
  coord_equal(ratio = 1/5) +
  facet_grid(. ~ vision, labeller = label_both) +
  theme_minimal() +
  theme(panel.margin = grid::unit(0.07,"npc"))
```

![](img/README-pFlockPlot3-1.png) 


## Categorical evaluation with LHS sampling
Instead of exploring parameter space with (full) factorial design we can use sampling methods. Below is an example with latin hypercube sampling from tgp package:


```r
# use Latin Hypercube sampling to sample 50 parameter sets
library(tgp)

experiment <- nl_set_param_values( experiment,
  param_values = nl_param_lhs(
    n = 100, 
    world_size = 50,
    population = 80,
    vision = c(6),
    min_separation = c(0, 4),
    max_align_turn = c(0, 20)
  )
)
```

_Note:_

* _To change only parameter sets use `nl_set_param_values`. The rest of
  experiment definition is not changed. If new parameters are added
  set the mapping accordingly.
* _Function `nl_param_lhs` is using `lhs` from **tgp** package to create
  random parameter sets_
* _Instead of using `nl_param_lhs` one can put any data frame in param_values.
  The columns of a data frame are interpreted as parameter names and rows should
  represent parameter sets._


```r
result4 <- nl_run(experiment, parallel = TRUE)
```


Add categorical criteria:

```r
dat <- nl_get_criteria_result( 
  result4,
  are_grouped = c_mcrowding >= 5 & c_mcrowding < 11,
  are_aligned = c_converged > 0.7
)

ggplot( dat, aes(x = min_separation, y = max_align_turn) ) +
  geom_point() +
  geom_point(data = subset(dat, are_aligned), color = "red", size = 5, shape = 2) +
  geom_point(data = subset(dat, are_grouped), color = "blue", size = 5, shape = 3) +
  coord_equal(ratio = 1/5) +
  theme_minimal()
```

![](img/README-pFlockPlot4-1.png) 

## Best-fit criterion

Create full factor design parameter sets:

```r
experiment <- nl_set_param_values( experiment,
  param_values = list(
    world_size = 50,
    population = 80,
    vision = c(6),
    min_separation = seq(from = 0, to = 4, by = 0.25),
    max_align_turn = seq(from = 0, to = 20, by = 1.25)
  )
)
```

Run experiment:

```r
result5 <- nl_run(experiment, parallel = TRUE)
```

Calculate single evaulation criterion (ideally mean crowding is around 8
and convergence is near 1):


```r
dat <- nl_get_criteria_result( 
  result5,
  eval_value = pmin(10, sqrt((c_mcrowding - 8)^2 + 100*(c_converged - 1)^2))
)
```


Plot the result:

```r
ggplot( dat, aes(x = min_separation, y = max_align_turn, fill = eval_value) ) +
  geom_tile() + coord_fixed(4/20) + theme_minimal() +
  theme(legend.position="none")

ggplot( dat, aes(x = min_separation, y = max_align_turn, z = eval_value) ) +
  stat_contour(bins =11, aes(color = ..level..)) +
  coord_fixed(4/20) + theme_minimal() + theme(legend.position="none")
```

![](img/README-pFlockPlot5-1.png) ![](img/README-pFlockPlot5-2.png) 

## Parameter fitting and optimization with L-BFGS-B
Optimization methods can't benefit from pre-defined parameter sets. Parameter 
values are selected as optimization runs. 
In this scenario the `nl_eval_run` function should be used instead of `nl_run`. 

There are two differences between `nl_eval_run` and `nl_run`:

* `nl_run_eval` accepts a single parameter set and returns a numeric value 
* It requires started NetLogo instance - user have to take care to 
  initialize NetLogo and load the model before optimization begins and 
  close NetLogo when it is no longer needed 
  (see `nl_eval_init` and `nl_eval_close` in package documentation).

Use `nl_eval_run` _parallel_ option when optimizing stochastic models
with more than a few repetitions needed to evaluate one parameter set. 

There are many R packages for solving optimization problems 
(see [CRAN Task View](https://cran.r-project.org/web/views/Optimization.html)).
This example use **L-BFGS-B method** with standard `stats::optim` function.
See also Thiele, Kurth & Grimm (2014) chapter 
[2.28 Gradient and quasi-Newton methods](http://jasss.soc.surrey.ac.uk/17/3/11.html#sectionGQNM).

Define experiment: 


```r
experiment <- nl_experiment( 
  model_file = 
    file.path(nl_netlogo_path(), "models/Sample Models/Biology/Flocking.nlogo"),
  setup_commands = c("setup", "repeat 100 [go]"),
  iterations = 5,

  param_values = list(
    world_size = 50,
    population = 80,
    vision = 6,
    min_separation = seq(from = 0, to = 4, by = 0.5),
    max_align_turn = seq(from = 0, to = 20, by = 2.5)
  ),
  mapping = c(
    min_separation = "minimum-separation",
    max_align_turn = "max-align-turn"),

  step_measures = measures(
    converged = "1 - 
      (standard-deviation [dx] of turtles + 
       standard-deviation [dy] of turtles) / 2",
    mean_crowding = 
      "mean [count flockmates + 1] of turtles"
  ),
  eval_criteria = criteria(
    c_converged = mean(step$converged),
    c_mcrowding = mean(step$mean_crowding)
  ),

  repetitions = 10,                              # repeat simulations 10 times

  eval_aggregate_fun = mean,                     # aggregate over repetitions

  eval_mutate = criteria(                        # avaluation criterium
    eval_value = 
      sqrt((c_mcrowding - 8)^2 + 100*(c_converged - 1)^2)
  )
)
```



```r
 
# initialize evaluation 
cl <- nl_eval_init(experiment, parallel = TRUE)
#> [1] "Creating sockets..."

# create callback container to spy what the optim function is doing
trace <- nl_eval_tracer(verbose = FALSE)

#get parameters' range (used for bounds on the variables for the "L-BFGS-B" method)
param_range <- nl_get_param_range(experiment)   

#call optimisation function with L-BFGS-B method:
set.seed(1) 
o_result <- optim(
  #par = param_range$upper,  
  par = (param_range$upper + param_range$lower)/2,
  nl_eval_run, 
    experiment = experiment, criteria = "eval_value", 
    call_back = trace$add, parallel = TRUE, cluster = cl,
  method = "L-BFGS-B",
  lower = param_range$lower, upper = param_range$upper, 
  control = list(maxit = 200, trace = 1))
#> final  value 3.171893 
#> converged

nl_eval_close(parallel = TRUE, cl)
o_result
#> $par
#> min_separation max_align_turn 
#>       2.423539      12.117697 
#> 
#> $value
#> [1] 3.171893
#> 
#> $counts
#> function gradient 
#>       12       12 
#> 
#> $convergence
#> [1] 0
#> 
#> $message
#> [1] "CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH"
```

The `trace$add` function colected every iteration of `optim`. 


```r
tr <- trace$get()

library(ggplot2)
ggplot(tr, aes(x = iter_id,  y = result)) +
  geom_line() +
  theme_minimal()
```

![](img/README-pFlockOptimPlot-1.png) 


Where did `optim` look for best value?


```r

ggplot(tr, aes(x = min_separation,  y = max_align_turn)) +
  geom_path(alpha = 0.1)+
  geom_point(shape = 4, size = 5, alpha = 0.3) +
  scale_x_continuous(limits = c(0, 4)) +
  scale_y_continuous(limits = c(0, 20)) +
  theme_minimal()
```

![](img/README-pFlockOptimPlot2-1.png) 

