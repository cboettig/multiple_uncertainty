




# Critical transition 

 * Author [Carl Boettiger](http://carlboettiger.info), <cboettig@gmail.com>
 * License: [CC0](http://creativecommons.org/publicdomain/zero/1.0/)
 * Description:   





Chose the state equation / population dynamics function



```r
f <- May
```




With parameters 



```r
pars <- c(r = .75, k = 10, a=1.7, H=1, Q = 3)
K <- 8 # approx
```




Note that this bifurcates when a increases to 2.  


We consider a profits from fishing to be a function of harvest `h` and stock size `x`,  

<div> $$ \Pi(x,h) = h - \left( c_0  + c_1 \frac{h}{x} \right) \frac{h}{x}, $$ </div> 

conditioned on h > x and x > 0,



```r
price <- 1
c0 <- 0.0
c1 <- 0
profit <- profit_harvest(price=price, c0 = c0, c1=c1) 
```




with price = `1`, `c0` = `0` and `c1` = `0`. 




```r
xmin <- 0
xmax <- 1.5 * K
grid_n <- 100
```




We seek a harvest policy which maximizes the discounted profit from the fishery using a stochastic dynamic programming approach over a discrete grid of stock sizes from `0` to `12` on a grid of `100` points, and over an identical discrete grid of possible harvest values.  




```r
x_grid <- seq(xmin, xmax, length = grid_n)  
h_grid <- x_grid  
```







```r
delta <- 0.05
xT <- 0
OptTime <- 200
sigma_g <- .2
x0 <- 2
```




We will determine the optimal solution over a `200` time step window with boundary condition for stock at `0` and discounting rate of `0.05`.  The Reed model considers a stochastic growth model 

<div> $$ x_{t+1} = z_g f(x_t) $$ </div> 

for the random variable `z_g`, given by 



```r
z_g <- function() rlnorm(1, 0, sigma_g)
```




No other sources of noise enter into the dynamics.  



```r
z_m <- function() 1
z_i <- function() 1
```








```r
SDP_Mat <- determine_SDP_matrix(f, pars, x_grid, h_grid, sigma_g)
```





### Find the optimum by dynamic programming

Bellman's algorithm to compute the optimal solution for all possible trajectories.



```r
opt <- find_dp_optim(SDP_Mat, x_grid, h_grid, OptTime=OptTime, xT=xT, 
                     profit, delta=delta, reward=0)
```




Note that `SDP_Mat` is specified from the calculation above, as are our grids and our profit function. `OptTime` is the stopping time.  `xT` specifies a boundary condition at the stopping time. A reward for meeting this boundary must be specified for it to make any difference.  `delta` indicates the economic discount rate. Again, details are in the function documentation.   


Plot the policy function (in terms of escapement, `x-h`, rather than harvest `h`) at equilibrium (first time-step):



```r
q1 <- qplot(x_grid, x_grid - x_grid[opt$D[,1]], xlab="stock size", ylab="escapement") + 
geom_point(aes(x,y), data=data.frame(x=opt$S, y=opt$S), col="red")
q1
```

![plot of chunk policyfn_plot](http://farm8.staticflickr.com/7259/7663893314_2a94a34776_o.png) 


and the value function (at equilibrium):



```r
q2 <- qplot(x_grid, opt$V, xlab="stock size", ylab="value") + 
geom_vline(xintercept=opt$S)
q2
```

![plot of chunk valuefn_plot](http://farm9.staticflickr.com/8423/7663893784_6556e8a9e9_o.png) 


### Simulate 



```r
sims <- lapply(1:100, function(i){
  ForwardSimulate(f, pars, x_grid, h_grid, x0=x0, opt$D, z_g, z_m, z_i)
})
```





## Summarize and plot the results                                                   

R makes it easy to work with this big replicate data set.  We make data tidy (melt), fast (data.tables), and nicely labeled.



```r
dat <- melt(sims, id=names(sims[[1]]))  
dt <- data.table(dat)
setnames(dt, "L1", "reps") # names are nice
```




### Plots 



```r
p0 <- ggplot(subset(dt,reps==1)) +
  geom_line(aes(time, fishstock)) +
  geom_abline(intercept=opt$S, slope = 0) +
  geom_line(aes(time, harvest), col="darkgreen") 
p0
```

![plot of chunk p0](http://farm8.staticflickr.com/7122/7663894716_ae900b7252_o.png) 



This plot summarizes the stock dynamics by visualizing the replicates. Reed's S shown again, along with the dotted line showing the allee threshold, below which the stock will go to zero (unless rescued stochastically). 



```r
p1 <- ggplot(dt) + geom_abline(intercept=opt$S, slope = 0) + 
  geom_abline(intercept=xT, slope = 0, lty=2) 
p1 <- p1 + geom_line(aes(time, fishstock, group = reps), alpha = 0.2)
p1
```

![plot of chunk p1](http://farm9.staticflickr.com/8016/7663895200_0f45acb508_o.png) 




## Calculate warning signals on managed system 



```r
library(earlywarning)
```






```r
acor_tau <- dt[, 
               warningtrend(data.frame(time=time, value=fishstock),
                            window_autocorr),
               by=reps]

var_tau <- dt[, 
              warningtrend(data.frame(time=time, value=fishstock),
                          window_var),
              by=reps]
```






```r
m <- dt[, 
        stability_model(data.frame(time=time, value=fishstock),
                          "LSN")$pars["m"],
        by=reps]
```






```r
signals <- melt(data.frame(var=var_tau$V1, acor=acor_tau$V1, m=m$V1))
ggplot(signals) + geom_histogram(aes(value)) + facet_wrap(~variable, scales="free")
```

![plot of chunk summaryplot](http://farm8.staticflickr.com/7267/7665248434_2bd8e43978_o.png) 



## Unmanaged warning signals
Calculate warning signals on the same set of shocks in unfished data 



```r
acor_tau <- dt[, 
               warningtrend(data.frame(time=time, value=unharvested),
                            window_autocorr),
               by=reps]

var_tau <- dt[, 
              warningtrend(data.frame(time=time, value=unharvested),
                          window_var),
              by=reps]
```






```r
m <- dt[, 
        stability_model(data.frame(time=time, value=unharvested),
                          "LSN")$pars["m"],
        by=reps]
```






```r
signals <- melt(data.frame(var=var_tau$V1, acor=acor_tau$V1, m=m$V1))
ggplot(signals) + geom_histogram(aes(value)) + facet_wrap(~variable, scales="free")
```

![plot of chunk summaryplot2](http://farm9.staticflickr.com/8007/7665945074_642656676e_o.png) 


