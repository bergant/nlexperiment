





## Explore parameter space with criteria function
This example uses NetLogo Hoopoes model from Railsback & Grimm (2011).
Here the approach is the same as in the Thiele, Kurth & Grimm (2014)
except using the **nlexperiment** framework instead of original R scripts.
See Factorial design and Latin hypercube sampling chapters in the 
[article](http://jasss.soc.surrey.ac.uk/17/3/11.html). 

Define an experiment with evaluation criteria  
*abundance*, *variation* and *vacancy* (see `eval_criteria` element)
and categorical boolean criteria based on satisfactory ranges (see `eval_mutate`):


```r
experiment <- nl_experiment( 
  model_file = 
    system.file("netlogo_models/SM2_Hoopoes.nlogo", package = "nlexperiment"),
  
  setup_commands = c("setup", "repeat 24 [go]"), # 2 years of warming-up
  go_command = "repeat 12 [go]",                 # iteration is per year
  iterations = 20,                               # run for 20 years
  repetitions = 10,                              # repeat simulation 10 times
  
  param_values = list(                           # "full factor design"
    scout_prob = seq(from = 0.00, to = 0.50, by = 0.05),
    survival_prob = seq(from = 0.950, to = 1.000, by = 0.005)
  ),
  mapping = c(                                   # map NetLogo variables
    scout_prob = "scout-prob", 
    survival_prob = "survival-prob"
  ),
  
  step_measures = measures(                      # NetLogo reporters per step
    abund = "month-11-count",             
    alpha = "month-11-alpha",                 
    patches_count = "count patches"
  ),

  eval_criteria = criteria(                      # evaluation per each run
    abundance = mean(step$abund),
    variation = sd(step$abund),
    vacancy = mean(step$alpha / step$patches_count)
  ),
  
  eval_aggregate_fun = mean,                     # mean value (10 repetitions)
  
  eval_mutate = criteria(                        # get categorical values
    c_abundance = abundance > 115 & abundance < 135,
    c_variation = variation > 10 & variation < 15,
    c_vacancy = vacancy > 0.15 & vacancy < 0.30
  )  
)
```

_Note:_ 

* _The iteration is set to 12 times calling `go` procedure (1 iteration step = 12 NetLogo ticks)_
* _When using list of parameter values, full parameter set is evaluated_
* _Criteria function receives all the results from model run and returns numerical vector_ 
* _Results from criteria function are aggregated by parameter sets over 10 repetitions_
* _Additionally categorical criteria is defined with ranges_

Run experiment:

```r
result <- nl_run(experiment, parallel = TRUE) 
# get the data (criteria)
dat <- nl_get_result(result, type = "criteria") 
```

Returned data frame includes parameter sets with evaluation criteria:

```r
str(dat)
#> 'data.frame':	121 obs. of  9 variables:
#>  $ scout_prob   : num  0 0.05 0.1 0.15 0.2 0.25 0.3 0.35 0.4 0.45 ...
#>  $ survival_prob: num  0.95 0.95 0.95 0.95 0.95 0.95 0.95 0.95 0.95 0.95 ...
#>  $ param_set_id : int  1 2 3 4 5 6 7 8 9 10 ...
#>  $ abundance    : num  5.47 5.84 4.71 5.01 5.56 ...
#>  $ variation    : num  9.08 9.79 8.72 9.41 9.99 ...
#>  $ vacancy      : num  0.977 0.974 0.981 0.979 0.975 ...
#>  $ c_abundance  : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
#>  $ c_variation  : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
#>  $ c_vacancy    : logi  FALSE FALSE FALSE FALSE FALSE FALSE ...
```

Plot categorical criteria on the model parameter space:

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
param_sets <- setNames(as.data.frame(param_sets), c("scout_prob", "survival_prob")) 
```

To change only parameter sets one can use `nl_set_param_values`
instead of defining all experiment definitions with `nl_experiment`:

```r
# change parameters of existing experiment
experiment <- nl_set_param_values( experiment,
  param_values = param_sets
)
```

Run the model with new parameters:

```r
result2 <- nl_run(experiment, parallel = TRUE) 
# get the data (criteria)
dat2 <- nl_get_result(result2, type = "criteria")
```



```r
ggplot(dat2, aes(x = scout_prob, y = survival_prob)) +
  geom_point() +
  geom_point(data = subset(dat2, c_abundance), color = "red", size = 7, shape = 2) +
  geom_point(data = subset(dat2, c_variation), color = "steelblue", size = 7, shape = 3) +
  geom_point(data = subset(dat2, c_vacancy), color = "seagreen", size = 7, shape = 4) +
  theme_minimal()
```

![](img/README-p8Aggregate2-1.png) 


