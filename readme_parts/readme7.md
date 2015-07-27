






## Parameter space 2
The following example is using NetLogo Fur model (Wilensky 2003) to show 
explicit parameter space definition and parameter mapping.

In NetLogo Fur model we can set the following variables:

* ratio 
* outer-radius-y
* inner-radius-y
* outer-radius-x
* inner-radius-x

But considering constraints and model symmetry
we can reduce it only to ratio and ellipse aspect ratio. We will
create parameter sets based on combinations of

* `radius_diff` (the difference between x and y radius) and
* `ratio` (the inhibitor concentration parameter)

For simplicity lets keep the `gap` between the circles constant:


```r
# prepare parameter space
param_space <- 
  expand.grid(
    gap = 3, 
    radius_diff = seq(from = 0, to = 2, by = 0.5), 
    ratio = seq(from = 0.30, to = 0.65, by = 0.05)
)
# add NetLogo variables
param_space <- 
  within(param_space, {
    inner_radius_x <- 3
    outer_radius_x <- 3 + gap
    inner_radius_y <- inner_radius_x + radius_diff
    outer_radius_y <- inner_radius_y + gap
  })

#knitr::kable(param_space, caption = "Parameter space")

mapping <- setNames(gsub("_", "-", names(param_space)),names(param_space))
mapping[c("gap", "radius_diff")] <- c("","")

knitr::kable(as.data.frame(mapping), caption = "Mapping", format = "markdown")
```



|               |mapping        |
|:--------------|:--------------|
|gap            |               |
|radius_diff    |               |
|ratio          |ratio          |
|outer_radius_y |outer-radius-y |
|inner_radius_y |inner-radius-y |
|outer_radius_x |outer-radius-x |
|inner_radius_x |inner-radius-x |

_Note:_ 

* _We created mapping simply by converting the `_` characters to `-`._ 
* _Variables `gap` and `radius_diff` are mapped to empty string._

Define experiment:


```r
experiment <- nl_experiment( 
  model_file = file.path(nl_netlogo_path(), 
                         "models/Sample Models/Biology/Fur.nlogo"), 
  max_ticks = 20,
  repetitions = 1,
  param_values = param_space,
  mapping = mapping,  
  export_view = TRUE,
  patches_after = list(
    fur = patch_set(vars = "pcolor", patches = "patches")
  ),
  random_seed = 3
)
```

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

