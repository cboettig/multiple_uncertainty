




# Critical transition 

 * Author [Carl Boettiger](http://carlboettiger.info), <cboettig@gmail.com>
 * License: [CC0](http://creativecommons.org/publicdomain/zero/1.0/)
 * Description:  Implements a numerical version of the SDP described in Reed, (1979), in which the dynamics are slowly moving towards a tipping point.  





Chose the state equation / population dynamics function



```r
f <- May
```




With parameters 



```r
pars <- c(r = .75, k = 10, a=1, H=1, Q = 3)
K <- 8 # approx
```




Ask R to show us how this function is defined, and plot the transition point using these parameters.



```r
May
```

```
function (x, h, p) 
{
    sapply(x, function(x) {
        s <- x - h
        r <- as.numeric(p[1])
        K <- as.numeric(p[2])
        a <- as.numeric(p[3])
        H <- as.numeric(p[4])
        Q <- as.numeric(p[5])
        s * exp(r * (1 - s/K) - a * s^(Q - 1)/(s^Q + H^Q))
    })
}
<environment: namespace:pdgControl>
```

```r

curve(.75*(1-x/10), 0, 10)
curve(1*x^2/(x^3+1), 0, 10, add=T, col="blue")
curve(1.9*x^2/(x^3+1), 0, 10, add=T, col="red")
```

![plot of chunk showMay](http://farm9.staticflickr.com/8143/7415596848_fababcbb41_o.png) 






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
OptTime <- 25
sigma_g <- .5
```




We will determine the optimal solution over a `25` time step window with boundary condition for stock at `0` and discounting rate of `0.05`.  The Reed model considers a stochastic growth model 

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

![plot of chunk policyfn_plot](http://farm9.staticflickr.com/8160/7415598210_c9e1eaa4a8_o.png) 


and the value function (at equilibrium):



```r
q2 <- qplot(x_grid, opt$V, xlab="stock size", ylab="value") + 
geom_vline(xintercept=opt$S)
q2
```

![plot of chunk valuefn_plot](http://farm9.staticflickr.com/8153/7415598602_a70d1ba8a7_o.png) 






### Simulate 

Now we'll simulate 100 replicates of this stochastic process,


```r
sims <- lapply(1:100, function(i){
  ForwardSimulate(f, pars, x_grid, h_grid, x0=K, opt$D, z_g, z_m, z_i)
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

Let's begin by looking at the dynamics of a single replicate. The line shows Reed's S, the level above which the stock should be harvested (where catch should be the difference between stock and S).  To confirm that this policy is being followed, note that harvesting only occurs when the stock is above this line, and harvest is proportional to the amount by which it is above.  Change the replicate `reps==` to see the results from a different replicate.  



```r
p0 <- ggplot(subset(dt,reps==1)) +
  geom_line(aes(time, fishstock)) +
  geom_abline(intercept=opt$S, slope = 0) +
  geom_line(aes(time, harvest), col="darkgreen") 
p0
```

![plot of chunk p0](http://farm9.staticflickr.com/8010/7415599312_0e951aa9c9_o.png) 








This plot summarizes the stock dynamics by visualizing the replicates. Reed's S shown again, along with the dotted line showing the allee threshold, below which the stock will go to zero (unless rescued stochastically). 



```r
p1 <- ggplot(dt) + geom_abline(intercept=opt$S, slope = 0) + 
  geom_abline(intercept=xT, slope = 0, lty=2) 
p1 <- p1 + geom_line(aes(time, fishstock, group = reps), alpha = 0.2)
p1
```

![plot of chunk p1](http://farm8.staticflickr.com/7248/7415599950_fb3b9d6bcd_o.png) 






```r
profits <-dt[ , sum(profit), by="reps"] 
ggplot(profits) + geom_histogram(aes(V1)) 
```

![the distribution of profits by scenario](http://farm6.staticflickr.com/5040/7415601954_12219bc6a8_o.png) 



## Calculate warning signals 

# References

<p>Reed WJ (1979).
&ldquo;Optimal Escapement Levels in Stochastic And Deterministic Harvesting Models.&rdquo;
<EM>Journal of Environmental Economics And Management</EM>, <B>6</B>.
ISSN 00950696, <a href="http://dx.doi.org/10.1016/0095-0696(79)90014-7">http://dx.doi.org/10.1016/0095-0696(79)90014-7</a>.




