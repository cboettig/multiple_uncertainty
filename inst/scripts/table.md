``` r
library("stringr")
library("dplyr")
library("tidyr")
library("ggplot2")
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
  stringr::str_replace_all("_", "") %>%
  stringr::str_replace("(\\w{3})(.*)", "\\1 | \\2")
}

df %>% 
  unite_("key", names(df)[1:6]) %>% 
  mutate(key = rekey(key)) %>% 
  select(key, ENPV, sd) -> 
  table
```

In these 8 scenarios, the beliefs match the true values for each of the noise levels:

``` r
optimal <- c("LLL | LLL", "HLL | HLL", "LHL | LHL", "LLH | LLH", "HHL | HHL", "LHH | LHH", "HLH | HLH", "HHH | HHH")
table %>% filter(key %in% optimal)
```

    ## Source: local data frame [8 x 3]
    ## 
    ##         key     ENPV         sd
    ##       (chr)    (dbl)      (dbl)
    ## 1 LLL | LLL 602.7425   2.346842
    ## 2 LLH | LLH 444.6088  86.403558
    ## 3 LHL | LHL 285.6336 129.673301
    ## 4 LHH | LHH 211.8751  97.936354
    ## 5 HLL | HLL 762.2499 120.537733
    ## 6 HLH | HLH 524.6004 133.593980
    ## 7 HHL | HHL 294.4968 139.328054
    ## 8 HHH | HHH 251.7012 110.776794

What happens if we think a noise source is not present, but it is? (Note: curious that the scenario in which growth noise alone is present actually has a higher ENPV than the deterministic world. In general, noise reduces the NPV, even when accounted for optimally)

``` r
table %>% 
  mutate( N =  filter(table, key=="LLL | LLL")[["ENPV"]]) %>% 
  filter(key %in% c("LLL | HLL", "LLL | LHL", "LLL | LLH")) %>% 
  mutate(ENPV / N)
```

    ## Source: local data frame [3 x 5]
    ## 
    ##         key     ENPV        sd        N    ENPV/N
    ##       (chr)    (dbl)     (dbl)    (dbl)     (dbl)
    ## 1 LLL | LLH 288.8413 148.57233 602.7425 0.4792118
    ## 2 LLL | LHL 150.2830  67.56709 602.7425 0.2493320
    ## 3 LLL | HLL 758.8783 130.25866 602.7425 1.2590422

Note: Curious why the deterministic solution is actually doing better

Is it worse to believe noise is present when it is absent (e.g. conservative noise model), or ingore sources of noise when they are present?

``` r
#always assumes belief HLL
ignore <- c("HLL | HHL", "HLL | HLH", "HLL | HHH")
# reality is always HLL
conservative <-  c("HHL | HLL", "HLH | HLL", "HHH | HLL")

table %>% 
  mutate( N =  filter(table, key=="HLL | HLL")[["ENPV"]]) %>% 
  filter(key %in% c(ignore, conservative)) %>% 
  mutate(ENPV / N)
```

    ## Source: local data frame [6 x 5]
    ## 
    ##         key     ENPV        sd        N    ENPV/N
    ##       (chr)    (dbl)     (dbl)    (dbl)     (dbl)
    ## 1 HLL | HLH 309.1881 166.66997 762.2499 0.4056256
    ## 2 HLL | HHL 197.5846 101.22018 762.2499 0.2592124
    ## 3 HLL | HHH 178.7917  99.22453 762.2499 0.2345579
    ## 4 HLH | HLL 628.4697 125.09035 762.2499 0.8244931
    ## 5 HHL | HLL 610.0263 108.72459 762.2499 0.8002970
    ## 6 HHH | HLL 476.9531 102.08201 762.2499 0.6257175

Here's the same question, but fixing growth noise at low instead of high:

``` r
ignore <- c("LLL | HLL", "LLL | LHL", "LLL | LLH")
conservative <-  c("LHL | LLL", "LLH | LLL", "LHH | LLL")

table %>% 
  mutate( N =  filter(table, key=="LLL | LLL")[["ENPV"]]) %>% 
  filter(key %in% c(ignore, conservative)) %>% 
  mutate(ENPV / N)
```

    ## Source: local data frame [6 x 5]
    ## 
    ##         key     ENPV         sd        N    ENPV/N
    ##       (chr)    (dbl)      (dbl)    (dbl)     (dbl)
    ## 1 LLL | LLH 288.8413 148.572330 602.7425 0.4792118
    ## 2 LLL | LHL 150.2830  67.567089 602.7425 0.2493320
    ## 3 LLL | HLL 758.8783 130.258657 602.7425 1.2590422
    ## 4 LLH | LLL 484.3563   1.763013 602.7425 0.8035875
    ## 5 LHL | LLL 493.4585   1.713756 602.7425 0.8186886
    ## 6 LHH | LLL 378.8967   1.415383 602.7425 0.6286212
