





## Parameter fitting and optimization / L-BFGS-B
Using experiment definition from 
[previous chapter: best-fit criteria function](#best-fit criteria function)
we could search the parameter space to optimize the cost function.









When using optimization methods we can't pre-define parameter sets because they
are selected as optimization runs. In this scenario the `nl_eval_run` function should be used instead of `nl_run`. 

There are two differences:

* `nl_run_eval` accepts a parameter set and returns a value 
* It requires NetLogo instance - user have to take care to initialize NetLogo and load the model before optimization begins and 
close NetLogo when it is no longer needed (see functions `nl_eval_init` and `nl_eval_close`).

Use `nl_eval_run` _parallel_ option when optimizing stochastic models
with more than a few repetitions needed to evaluate one parameter set. 

There are many R packages for solving optimization problems 
(see [CRAN Task View](https://cran.r-project.org/web/views/Optimization.html)).
This example use **L-BFGS-B method** with standard `stats::optim` function.
See also Thiele, Kurth & Grimm (2014) chapter 
[2.28 Gradient and quasi-Newton methods](http://jasss.soc.surrey.ac.uk/17/3/11.html#sectionGQNM).


```r
# parameter range from experiment
param_range <- nl_get_param_range(experiment)   
 
# initialize evaluation
cl <- nl_eval_init(experiment, parallel = TRUE)
#> [1] "Creating sockets..."

# create callback container to spy what the optim function is doing
trace <- nl_eval_tracer(verbose = FALSE)

#call optimisation function with L-BFGS-B method:
o_result <- optim(
  par = c(0.5, 1.0),  
  nl_eval_run, 
    experiment = experiment, criteria = "cost", 
    call_back = trace$add, parallel = TRUE, cluster = cl,
  method = "L-BFGS-B",
  lower = param_range$lower, upper = param_range$upper, 
  control = list(maxit = 200, trace = 1))
#> iter   10 value 0.037885
#> final  value 0.037885 
#> converged

nl_eval_close(parallel = TRUE, cl)
```


```r
#final result:
o_result
#> $par
#> [1] 0.4308075 0.9763381
#> 
#> $value
#> [1] 0.03788475
#> 
#> $counts
#> function gradient 
#>       41       41 
#> 
#> $convergence
#> [1] 0
#> 
#> $message
#> [1] "CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH"
```

The `trace$add` function colected every iteration of `optim`. 


```r
dat <- trace$get()

library(ggplot2)

ggplot(dat, aes(x=iter_id, y = result)) +
  geom_step() +
  labs(x = "Iteration", y = "Evaluation result")
```

![](img/README-p10plot-1.png) 

Optim iterations on parameter space:

```r


ggplot(dat, aes(x = scout_prob, y = survival_prob, color = pmin(20,result))) +
  #  geom_line(color = "gray", size = 1, ) +
  geom_point(size = 3) +
  geom_point(
    color = "red", 
    data = data.frame(scout_prob = o_result$par[1], survival_prob = o_result$par[2]),
    size = 20, shape = 4) +
  theme_minimal()
```

![](img/README-unnamed-chunk-5-1.png) 

