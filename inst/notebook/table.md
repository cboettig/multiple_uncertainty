``` r
library("stringr")
library("dplyr")
library("tidyr")
library("ggplot2")
library("lazyeval")
library("readr")
library("multipleuncertainty")
knitr::opts_chunk$set(cache = TRUE, comment=NA)
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
optimal %>% knitr::kable("pandoc")
```

|   id| belief | true |      ENPV|          sd|
|----:|:-------|:-----|---------:|-----------:|
|    1| LLL    | LLL  |  603.0429|    2.134585|
|   10| LLH    | LLH  |  466.7537|   80.174116|
|   19| LHL    | LHL  |  292.8202|  124.046084|
|   28| LHH    | LHH  |  241.5899|  107.514183|
|   37| HLL    | HLL  |  774.3748|  136.523609|
|   46| HLH    | HLH  |  471.1784|  154.090984|
|   55| HHL    | HHL  |  288.1971|  130.285451|
|   64| HHH    | HHH  |  280.3641|  128.069568|

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
tbl %>% mutate(normalized_value = ENPV / N) -> tbl
```

Ignoring uncertaines that are present:

``` r
tbl %>% filter(belief == "HLL", true %in% c("HHL", "HLH", "HHH")) %>% knitr::kable("pandoc")
```

|   id| belief | true |      ENPV|         sd|         N|     ENPV/N|
|----:|:-------|:-----|---------:|----------:|---------:|----------:|
|   38| HLL    | HLH  |  296.5831|  139.03377|  471.1784|  0.6294498|
|   39| HLL    | HHL  |  192.8838|  111.81136|  288.1971|  0.6692773|
|   40| HLL    | HHH  |  162.3541|   78.81396|  280.3641|  0.5790831|

Conservative: include uncertainty that doesn't exist

``` r
tbl %>% filter(true == "HLL", belief %in% c("HHL", "HLH", "HHH")) %>% knitr::kable("pandoc")
```

|   id| belief | true |      ENPV|         sd|         N|     ENPV/N|
|----:|:-------|:-----|---------:|----------:|---------:|----------:|
|   45| HLH    | HLL  |  615.7262|  126.31208|  774.3748|  0.7951269|
|   53| HHL    | HLL  |  605.9144|  120.94954|  774.3748|  0.7824562|
|   61| HHH    | HLL  |  480.9155|   95.93188|  774.3748|  0.6210371|

We can visualize the whole table (rows are the true scenario, columns are believed scenario). First we just plot ENPV, which justifies our normalization routine, since we see the very strong influence of reality regardless what you believe. Note that we supress grid labels as the units are arbitrary and only relative differences are of interest.

``` r
ggplot(tbl) + geom_histogram(aes(ENPV), binwidth=50) + 
  facet_grid(belief ~ true) + 
  theme(axis.text=element_blank())
```

![](table_files/figure-markdown_github/unnamed-chunk-10-1.png)

After normalizing by the optimum that could be achieved for the given scenario, we have the following patterns.

``` r
ggplot(tbl) + geom_histogram(aes(normalized_value), binwidth=0.05) + 
  facet_grid(belief ~ true)  + 
  theme(axis.text.y=element_blank()) + 
  scale_x_continuous(breaks=scales::pretty_breaks(n=3)) + 
  coord_cartesian(xlim=c(0, 1.2))
```

![](table_files/figure-markdown_github/unnamed-chunk-11-1.png)

Note that believing that both measurement and implementation error are low (`LLL` and `HLL`) gives you consistently the worst outcomes unless both sources of noise truely are low.

``` r
ggplot(tbl) + 
  geom_histogram(aes(normalized_value, fill=belief), position="identity", binwidth=.05) + 
  facet_grid(~ true) + 
  theme(axis.text=element_blank())
```

![](table_files/figure-markdown_github/unnamed-chunk-12-1.png)

``` r
#write_csv(tbl, "table_files/table.csv")
```
