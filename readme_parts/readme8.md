






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
* _Criteria function gets all the results from model run and returns numerical vector_ 

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


