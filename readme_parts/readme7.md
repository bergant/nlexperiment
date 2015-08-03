






## Parameter space mapping
The following example is using NetLogo Fur model (Wilensky 2003) to show 
explicit definition of parameter sets and parameter mapping.

There are 5 parameters in the NetLogo Fur model: 

* `ratio` 
* `outer-radius-y`
* `inner-radius-y`
* `outer-radius-x`
* `inner-radius-x`

But considering constraints and model symmetry
we can reduce the parameters to:

* `ratio` (the inhibitor concentration parameter)
* `radius_diff` (the difference between x and y radius) and
* `gap` (distance between inner and outer ellipse)

(for simplicity let's keep the `gap` between the circles constant):



```r
experiment <- nl_experiment( 
  model_file = file.path(nl_netlogo_path(), 
                         "models/Sample Models/Biology/Fur.nlogo"), 
  iterations = 20,                                     

  param_values = {                                  # Parameter sets:
    param_sets <- expand.grid(                      #   all combinations of
        gap = 3,                                    #   gap, ratio and ry- rx
        radius_diff = seq(0, 2, by = 0.5), 
        ratio = seq(0.30, 0.65, by = 0.05)
    )
    transform(param_sets,                           # Transform to NetLogo
      inner_radius_x = 3,                           #   variables
      outer_radius_x = 3 + gap,
      inner_radius_y = 3 + radius_diff,
      outer_radius_y = 3 + radius_diff + gap
    )
  },
  mapping = c(
    gap = "",
    radius_diff = "",
    inner_radius_x = "inner-radius-x",
    outer_radius_x = "outer-radius-x",
    inner_radius_y = "inner-radius-y",
    outer_radius_y = "outer-radius-y"
  ),
  patches_after = list(
    patches = patch_set(
      vars = c("pxcor", "pycor", "pcolor"),
      patches = "patches"
    )
  ),  
  random_seed = 3
)
```

_Note:_ 

* _Element `param_values` is set by a data frame with explicit parameter sets_ 
* _Variables `gap` and `radius_diff` must be mapped to empty string (not NetLogo variables)._


Run experiment

```r
result <- nl_run(experiment, parallel = TRUE, max_cores = 3)    
```

Show resulting fur patterns:

```r
library(ggplot2)
nl_show_patches(result, x_param = "ratio", y_param = "radius_diff") + 
  scale_fill_manual(values = c("black","white")) +
  labs(y=expression(radius[y] - radius[x]), title = "Fur patterns (gap = 3)")
```

![](img/README-p7ShowViews-1.png) 


