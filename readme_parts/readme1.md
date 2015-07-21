



## Simple experiment with fire
This sample experiment with NetLogo Fire model (Wilensky 1997) demonstrates
how to create and run minimal experiment. It runs the model with different parameter
values and exports final NetLogo views to image files:


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
  random_seed = 3,
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

