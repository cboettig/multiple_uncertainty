``` r
library("stringr")
library("dplyr")
library("tidyr")
library("ggplot2")
library("lazyeval")
library("readr")
library("multipleuncertainty")
knitr::opts_chunk$set(cache = TRUE)
```

Optimal control under multiple uncertainty by model:

``` r
mean_value <- function(sigma_g_belief, sigma_m_belief, sigma_i_belief,
                     sigma_g_true, sigma_m_true, sigma_i_true){
  
  delta <- 0.05
  grid <- seq(0,150,length=151) #seq(0, 200, length = 401)
  
  S <- multiple_uncertainty(f = "ricker", 
                            x_grid = grid, 
                            sigma_g = sigma_g_belief, 
                            sigma_m = sigma_m_belief, 
                            sigma_i = sigma_i_belief,
                            delta = delta,
                            noise_dist = "lognormal")

  ## Function to compute to do a replicate simulation and return NPV
  replicates <- function(){
    sim <- scenario(S, f = "ricker", 
                    x_grid = grid, 
                    sigma_g = sigma_g_true, 
                    sigma_m = sigma_m_true, 
                    sigma_i = sigma_i_true, 
                    noise_dist = "lognormal")
    sim %>% 
      mutate(value = h * (1-delta) ^ t) %>% 
      summarise(NPV = sum(value))
  }
  
  ## Simulate 100 replicates of the policy and return E(NPV)
  data.frame(rep = 1:100) %>% 
    group_by(rep) %>% 
    do(replicates()) %>% 
    ungroup() %>%
    summarise(ENPV = mean(NPV), sd = sd(NPV))


  }
```

``` r
low <- 0.01
high <- 0.5
cases <-
  expand.grid(
  sigma_g_belief = c(low, high),
  sigma_m_belief = c(low, high),
  sigma_i_belief = c(low, high),
  sigma_g_true = c(low, high),
  sigma_m_true = c(low, high),
  sigma_i_true = c(low, high)
  )

group_by_all <- function(x,...) x %>%  group_by_(.dots = lapply(names(x), as.name))  
cases %>%
  group_by_all() %>%
  do(mean_value(.$sigma_g_belief, .$sigma_m_belief, .$sigma_i_belief,
                 .$sigma_g_true, .$sigma_m_true, .$sigma_i_true)) %>% 
  ungroup() -> df
```

We want to collapse the columns that simply list the values of each true/belief sigma value into a single scenario key, where we'll use `L` for "Low" noise and `H` for high. We indicate the belief values of growth, measurement, and implementation error, respectively, followed by the true values used for the simulations. Thus `HLL | LHL` indicates that the optimal solution was computing assuming high growth error, but low measurement and implementation, and then this policy was applied to simulations in which growth noise and implementation noise were low, but measurement was high.

``` r
rekey <- function(str){
  str %>% 
  stringr::str_replace_all("0.01", "L") %>%
  stringr::str_replace_all("0.5", "H") %>%
  stringr::str_replace_all("_", "")
}

df %>% 
  unite_("true", names(df)[4:6]) %>% 
  mutate(true = rekey(true)) %>% 
  unite_("belief", names(df)[1:3]) %>% 
  mutate(belief = rekey(belief)) %>% 
  select(belief, true, ENPV, sd) -> 
  table

## Include unique id for binding
table <- cbind(id = 1:dim(df)[1], table)
```

In these 8 scenarios, the beliefs match the true values for each of the noise levels:

``` r
table %>% filter(belief == true) -> optimal
optimal
```

    ##   id belief true     ENPV         sd
    ## 1  1    LLL  LLL 602.7425   2.346842
    ## 2 10    LLH  LLH 444.6088  86.403558
    ## 3 19    LHL  LHL 285.6336 129.673301
    ## 4 28    LHH  LHH 211.8751  97.936354
    ## 5 37    HLL  HLL 762.2499 120.537733
    ## 6 46    HLH  HLH 524.6004 133.593980
    ## 7 55    HHL  HHL 294.4968 139.328054
    ## 8 64    HHH  HHH 251.7012 110.776794

Is it worse to believe noise is present when it is absent (e.g. conservative noise model), or ingore sources of noise when they are present?

First, we'd like to normalize each case by the expected value achieved by applying the optimal control to the simulated scenario:

``` r
tbls <- lapply(optimal$true, function(x){
  expr1 <- interp(~true == x)
  expr2 <- interp(~N, N = filter_(optimal, .dots = list(expr1))[["ENPV"]])
  table %>% filter_(~true == x) %>% mutate_(.dots = setNames(list(expr2), "N"))
})
tbl <- bind_rows(tbls) %>% arrange(id)
```

Now we can normalize:

``` r
tbl %>% mutate(ENPV / N) -> tbl
```

Ingore uncertaines that are present:

``` r
tbl %>% filter(belief == "HLL", true %in% c("HHL", "HLH", "HHH"))
```

    ## Source: local data frame [3 x 7]
    ## 
    ##      id belief  true     ENPV        sd        N    ENPV/N
    ##   (int)  (chr) (chr)    (dbl)     (dbl)    (dbl)     (dbl)
    ## 1    38    HLL   HLH 309.1881 166.66997 524.6004 0.5893783
    ## 2    39    HLL   HHL 197.5846 101.22018 294.4968 0.6709228
    ## 3    40    HLL   HHH 178.7917  99.22453 251.7012 0.7103331

Conservative: include uncertainty that doesn't exist

``` r
tbl %>% filter(true == "HLL", belief %in% c("HHL", "HLH", "HHH"))
```

    ## Source: local data frame [3 x 7]
    ## 
    ##      id belief  true     ENPV       sd        N    ENPV/N
    ##   (int)  (chr) (chr)    (dbl)    (dbl)    (dbl)     (dbl)
    ## 1    45    HLH   HLL 628.4697 125.0903 762.2499 0.8244931
    ## 2    53    HHL   HLL 610.0263 108.7246 762.2499 0.8002970
    ## 3    61    HHH   HLL 476.9531 102.0820 762.2499 0.6257175

``` r
write_csv(tbl, "data/table.csv")
```
