






## Simple experiment with fire
Create NetLogo experiment object

```r
library(nlexperiment)
# Set the path to your NetLogo instalation
nl_netlogo_path("c:/Program Files (x86)/NetLogo 5.1.0/") 
# Fire model is included in NetLogo sample models:
fire_model <- file.path(nl_netlogo_path(), "models/Sample Models/Earth Science/Fire.nlogo")

# Create simple NetLogo experiment object
experiment <- nl_experiment(
  model_file = fire_model, 
  while_condition = "any? turtles",
  repetitions = 3,
  export_view = TRUE
)
```

Run the experiment:

```r
result <- nl_run(experiment)
#> Warning: Parameter space not defined. Using default parameters
```

Find exported view image files paths in `result$export` or just display them by calling `nl_show_view` function:

```r
nl_show_view(result)
```

![](img/README-model_view-1.png) ![](img/README-model_view-2.png) ![](img/README-model_view-3.png) 

