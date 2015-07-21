






## Mapping parameters
NetLogo identifiers may include any of the following ASCII characters:

`.?=*!<>:#+/%$_^'&-`

which makes variable names that are awkward to use in R data manipulation. 
To map R variables to NetLogo parameters use `mapping` option:


```r
experiment <- nl_experiment(
  model_file = file.path(nl_netlogo_path(), 
                         "models/Sample Models/Biology/Ants.nlogo"), 
  while_condition = "ticks < 150",
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

experiment$param_space
#>   population diffusion_rate evaporation_rate
#> 1        125             50                5
#> 2        125             60                5
#> 3        125             50               10
#> 4        125             60               10
#> 5        125             50               15
#> 6        125             60               15
```

Run experiment

```r
results <- nl_run(experiment)
```

Show views

```r
results <- nl_run(experiment)
nl_show_view(results)
```

![](img/README-p5ShowViews-1.png) ![](img/README-p5ShowViews-2.png) ![](img/README-p5ShowViews-3.png) ![](img/README-p5ShowViews-4.png) ![](img/README-p5ShowViews-5.png) ![](img/README-p5ShowViews-6.png) 

Show remaining food by parameter space and food pathces

```r
library(tidyr)
dat <- nl_get_step_result(results)
dat <- tidyr::gather(dat, pile, value, pile1, pile2, pile3)
ggplot(dat, aes(x = tick, y = value, color = pile) ) +
  geom_line() +
  facet_grid(diffusion_rate ~ evaporation_rate, margins = c(2,3))
```

![](img/README-p5plot-1.png) 

