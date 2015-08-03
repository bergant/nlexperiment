






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

* _When parameters are defined as a list of value vectors, parameter space is
  constructed as combination of all possible parameter sets
* _Element `mapping` maps `difussion_rate` and `evaporation_rate` names to NetLogo variables `diffusion-rate` and `evaporation-rate`._
* _It is not required to include all parameters in mapping. Variable `population` is used as is._


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
ggplot(dat, aes(x = step_id, y = value, color = pile) ) +
  geom_line() +
  facet_grid(diffusion_rate ~ evaporation_rate)
```

![](img/README-p5plot-1.png) 

