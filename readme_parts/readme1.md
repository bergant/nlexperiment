






## Simple experiment with fire
Create NetLogo experiment object

```r
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
#> Warning: No parameter space defined. Using default parameters
```

Find exported view image files paths in `result$export` or just display them by calling `nl_show_view` function:

```r
nl_show_view(result)
```

![](img/README-model_view-1.png) ![](img/README-model_view-2.png) ![](img/README-model_view-3.png) 

