







```r
require(snowfall)
sfInit(cpu=16, parallel=T)
```

```
R Version:  R version 2.15.2 (2012-10-26) 

```

```r
sfLibrary(pdgControl)
```

```
Library pdgControl loaded.
```

```
Warning: 'keep.source' is deprecated and will be ignored
```

```r
sfExportAll()
```





```r
seed <- 123                 # Random seed (replicable results)
delta <- 0.01               # economic discounting rate
OptTime <- 100              # stopping time
gridsize <- 100              # grid size for fish stock and harvest rate (discretized population)
sigma_g <- 0.2              # Noise in population growth
reward <- 0                 # bonus for satisfying the boundary condition
z_g <- function() rlnorm(1,  0, sigma_g) # mean 1
z_m <- function() 1         # No measurement noise, 
z_i <- function() 1         # No implemenation noise
f <- BevHolt                # Select the state equation
pars <- c(1.5, 0.05)        # parameters for the state equation
K <- (pars[1] - 1)/pars[2]  # Carrying capacity (for reference 
xT <- 0                     # boundary conditions
x0 <- K
x_grid <- seq(0.01, 1.2 * K, length = gridsize)  
h_grid <- seq(0.01, 0.8 * K, length = gridsize)  
```




```r
SDP_Mat <- determine_SDP_matrix(f, pars, x_grid, h_grid, sigma_g )
opt <- find_dp_optim(SDP_Mat, x_grid, h_grid, OptTime, xT, 
                     profit, delta, reward=reward)
```





```r
L1 <- function(c2) function(h, h_prev)  c2 * abs(h - h_prev) 
free_increase <- function(c2) function(h, h_prev)  c2 * abs(min(h - h_prev, 0)) # increasing harvest is free
free_decrease <- function(c2) function(h, h_prev)  c2 * max(h - h_prev, 0) # decreasing harvest is free
fixed <-  function(c2) function(h, h_prev) c2 * as.numeric( !(h == h_prev) )
L2 <- function(c2) function(h, h_prev)  c2 * (h - h_prev) ^ 2
none <- function(h, h_prev)  0
penaltyfns <- list(L2=L2, L1=L1, free_decrease=free_decrease, fixed=fixed, free_increase=free_increase)
```


## Apples to Apples levels

### Loop over penalty functions and magnitudes


```r
policies <- 
sfSapply(penaltyfns, function(penalty){
  policies <- 
  sapply(c2, function(c2){
      policycost <- optim_policy(SDP_Mat, x_grid, h_grid, OptTime, xT, 
                        profit, delta, reward, penalty = penalty(c2))
      i <- which(x_grid > K)[1]
      max(policycost$penalty_free_V[i,]) 
  })
})
```


Note that `optim_policy` has been updated to return the equilibrium value of profits from fish harvests before the adjustment costs have been paid, `penalty_free_V`.  This containst the values for all possible states, we simply evaluate it at the carrying capacity (which is our initial condition.)  The index in `x_grid` that corresponds to the carrying capacity (initial condition) `i` indicates this.  



Quadratic costs on fishing effort have to be done separately,


```r
quad <- 
  sapply(c2, function(c2){
  effort_penalty = function(x,h) .1*c2*h/x
  policycost <- optim_policy(SDP_Mat, x_grid, h_grid, OptTime, xT, 
                        profit, delta, reward, penalty = fixed(0), 
                        effort_penalty)
      i <- which(x_grid > K)[1]
      max(policycost$penalty_free_V[i,]) # chooses the most sensible harvest in t=1
})
dat <- cbind(policies, quad)
```


Tidy up the data and plot the net present value (before the penalty has been paid) relative to that achieved when managed without a penalty.  


```r
npv0 <- dat[1,3] 
npv0
```

```
free_decrease 
         1197 
```

```r
dat <- data.frame(c2=c2,dat)
dat <- melt(dat, id="c2")
ggplot(dat, aes(c2, value, col=variable)) + geom_point() + geom_line()
```

![plot of chunk npv-plot](figure/npv-plot.png) 


Find the value of `c2` that brings each penalty closest to 75% of the cost-free adjustment value:


```r
ggplot(dat, aes(c2, (npv0-value)/npv0, col=variable)) + geom_point() + geom_line()
```

![plot of chunk apples_plot](figure/apples_plot.png) 



```r
closest <- function(x, v){
  which.min(abs(v-x))
}
dt <- data.table(dat)
index <- dt[,closest(.25, (npv0-value)/value), by=variable]
apples <- c2[index$V1]
names(apples) = index$variable
apples
```

```
           L2            L1 free_decrease         fixed free_increase 
        9.231         5.641         0.000        15.385         0.000 
         quad 
        4.103 
```




## Results

Solve the policy cost for the specified penalty function


```r
L2_policy <- optim_policy(SDP_Mat, x_grid, h_grid, OptTime, xT, 
                    profit, delta, reward, penalty = L2(apples["L2"]))
```



```r
fixed_policy <- optim_policy(SDP_Mat, x_grid, h_grid, OptTime, xT, 
    profit, delta, reward, penalty = fixed(apples["fixed"]))
```



```r
L1_policy <- optim_policy(SDP_Mat, x_grid, h_grid, OptTime, xT, profit, delta, reward, penalty = L1(apples["L1"]))
```



```r
free_increase_policy <- optim_policy(SDP_Mat, x_grid, h_grid, OptTime, xT, profit, delta, reward, penalty =  free_increase(apples["free_increase"]))
```



```r
free_decrease_policy <- optim_policy(SDP_Mat, x_grid, h_grid, OptTime, xT, profit, delta, reward, penalty = free_decrease(apples["free_decrease"]))
```


We also compare to the case in which costs of harvesting increase quadratically with effort; a common approach to create smoother policies.  


```r
quad_profit <- profit_harvest(price = 10, c0 = 30, c1 = apples["quad"]) 
quad_costs <- optim_policy(SDP_Mat, x_grid, h_grid, OptTime, xT, quad_profit, delta, reward, penalty =  none)
```




```r
sims <- list(
  L1 = simulate_optim(f, pars, x_grid, h_grid, x0, 
                      L1_policy$D, z_g, z_m, z_i, 
                      opt$D, profit=profit, penalty=L1(apples["L1"]), seed=seed), 
  L2 = simulate_optim(f, pars, x_grid, h_grid, x0, 
                      L2_policy$D, z_g, z_m, z_i, 
                      opt$D, profit=profit, penalty=L2(apples["L2"]), seed=seed),
  fixed = simulate_optim(f, pars, x_grid, h_grid, x0, 
                         fixed_policy$D, z_g, z_m, z_i, 
                         opt$D, profit=profit, penalty=fixed(apples["fixed"]), seed=seed),
  increase = simulate_optim(f, pars, x_grid, h_grid, x0, 
                            free_increase_policy$D, z_g, z_m, z_i, 
                            opt$D, profit=profit, penalty= free_increase(apples["increase"]), seed=seed),
  decrease = simulate_optim(f, pars, x_grid, h_grid, x0, 
                            free_decrease_policy$D, z_g, z_m, z_i, 
                            opt$D, profit=profit, penalty= free_decrease(apples["decrease"]), seed=seed),
  quad = simulate_optim(f, pars, x_grid, h_grid, x0, 
                            free_decrease_policy$D, z_g, z_m, z_i, 
                            opt$D, profit=quad_profit, penalty= none, seed=seed)
)
```




```r
#Make data tidy (melt), fast (data.tables), and nicely labeled.
dat <- melt(sims, id=names(sims[[1]]))  
dt <- data.table(dat)
setnames(dt, "L1", "penalty_fn") # names are nice
```


# Plots 




```r
p0 <- ggplot(dt) +
  geom_line(aes(time, harvest_alt), col="grey20", lwd=1) +
  geom_line(aes(time, harvest, col=penalty_fn, lty=penalty_fn))+ 
  labs(x="time", y="stock size", title = "Stock Dynamics")
```



```r
p1 <- ggplot(dt) +
  geom_line(aes(time, alternate), col="grey20", lwd=1) +
  geom_line(aes(time, fishstock), col=rgb(0,0,1,.8)) + facet_wrap(~penalty_fn) + 
  labs(x="time", y="stock size", title = "Stock Dynamics")
```



```r
p2 <- ggplot(dt) +
  geom_line(aes(time, harvest_alt), col="grey20", lwd=1)  +
  geom_line(aes(time, harvest), col=rgb(0,0,1,.8)) + 
  facet_wrap(~penalty_fn) + 
  labs(x="time", y="havest intensity (fish taken)", title = "Harvest Policy Dynamics")
```

