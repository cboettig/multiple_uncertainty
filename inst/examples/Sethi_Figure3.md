Compare multiple uncertainties

  

```
## Loading required package: multipleuncertainty
```

```
## Loading required package: reshape2
```

```
## Loading required package: ggplot2
```

```
## Use suppressPackageStartupMessages to eliminate package startup messages.
```

```
## Loading required package: data.table
```

```
## data.table 1.8.6 For help type: help("data.table")
```









```r
f <- function(x, h, p) {
    A <- p[1]
    B <- p[2]
    s <- pmax(x - h, 0)
    A * s/(1 + B * s)
}
```



```r
pars <- c(1.5, 0.05)
K <- (pars[1] - 1)/pars[2]
xmin <- 0
xmax <- 1.5 * K
n_x <- 300
n_h <- n_x
x_grid <- seq(xmin, xmax, length = n_x)
h_grid <- seq(xmin, xmax, length = n_h)
delta <- 0.05
xT <- 0
OptTime <- 5
sigma_g <- 0.3
profit <- function(x, h) pmin(x, h)
```





```r
g <- SDP_multiple_uncertainty(f, pars, x_grid, h_grid, OptTime, sigmas = c(sigma_g = sigma_g, 
    sigma_m = 0, sigma_i = 0), pdfn = pdfn)
```



```r
m <- SDP_multiple_uncertainty(f, pars, x_grid, h_grid, OptTime, sigmas = c(sigma_g = 0.03, 
    sigma_m = 0.3, sigma_i = 0), pdfn = pdfn)
```



```r
i <- SDP_multiple_uncertainty(f, pars, x_grid, h_grid, OptTime, sigmas = c(sigma_g = 0.03, 
    sigma_m = 0, sigma_i = 0.3), pdfn = pdfn)
```


Plot the policy function (in terms of escapement, `x-h`, rather than harvest `h`) at equilibrium (first time-step):


```r
require(reshape2)
policies <- melt(data.frame(stock = x_grid, g = x_grid[g$D[, 1]], 
    meas = x_grid[m$D[, 1]], imp = x_grid[i$D[, 1]]), id = "stock")
```



```r
q1 <- ggplot(policies, aes(stock, stock - value, color = variable)) + 
    geom_point() + xlab("stock size") + ylab("escapement")
q1
```

<img src="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAfgAAAH4CAMAAACR9g9NAAACzVBMVEUAAAAAujgCAgIDAwMEBAQFBQUGBgYHBwcICAgJCQkKCgoLCwsMDAwNDQ0ODg4PDw8QEBARERESEhITExMUFBQVFRUWFhYXFxcYGBgZGRkbGxscHBweHh4fHx8gICAhISEiIiIjIyMkJCQmJiYnJycqKiorKyssLCwtLS0uLi4vLy8wMDAyMjIzMzM0NDQ1NTU2NjY3Nzc4ODg5OTk6Ojo7Ozs8PDw9PT0+Pj4/Pz9AQEBBQUFDQ0NERERFRUVGRkZHR0dISEhJSUlKSkpLS0tMTExNTU1OTk5PT09QUFBRUVFSUlJTU1NVVVVWVlZXV1dYWFhaWlpbW1tcXFxeXl5fX19gYGBhYWFhnP9iYmJkZGRlZWVmZmZnZ2doaGhpaWlra2tsbGxtbW1ubm5wcHBycnJ0dHR1dXV2dnZ3d3d4eHh5eXl7e3t8fHx9fX1+fn5/f3+AgICBgYGCgoKDg4OEhISFhYWHh4eIiIiJiYmKioqLi4uMjIyNjY2Ojo6Pj4+QkJCRkZGSkpKTk5OUlJSVlZWWlpaXl5eYmJiZmZmampqbm5ucnJydnZ2enp6fn5+goKChoaGioqKjo6OkpKSlpaWmpqanp6eoqKipqamqqqqrq6usrKytra2urq6vr6+xsbGysrKzs7O0tLS1tbW2tra4uLi5ubm6urq7u7u8vLy9vb2+vr6/v7/AwMDBwcHCwsLDw8PExMTFxcXGxsbHx8fIyMjJycnKysrLy8vMzMzNzc3Ozs7Pz8/Q0NDR0dHS0tLT09PU1NTV1dXW1tbX19fY2NjZ2dna2trb29vc3Nzd3d3e3t7f39/g4ODh4eHi4uLj4+Pk5OTl5eXm5ubn5+fo6Ojp6enq6urr6+vs7Ozt7e3u7u7v7+/w8PDx8fHy8vLz8/P09PT19fX29vb39/f4dm34+Pj5+fn6+vr7+/v8/Pz9/f3+/v7////azfVRAAAACXBIWXMAAAsSAAALEgHS3X78AAAWF0lEQVR4nO3dj58U9X3H8duShhQCZ72mmkSsRmMCDQkaE40YQa0Ro4bUKNmmYg0xCjVtqKGVNqaQSoL8DAGEeEQjv5oq6gWkKg3FygmV3KUgy8Hd7d0B9wOOvdvv39DZHzM7893Pd3a+39357vx4vx4+7vZmd+6zx3Nn9vfawFAsa6j3GUD1CfAxDfAxDfAxDfAxDfAxDfAxDfAxDfAxDfAxDfAxDfAxDfAxTQZ+sMdZ73CPYr2K6w2d1jxw4KzmgWf4f+SeAMCfSTnrYCm1Tp5SXDHTpbhit+J6A6c1D+wd4pcAPgV4wMsGeMBLBXjA6xkIeDLAA14uwANeKsADXs9AwJMBHvByAR7wUgEe8HoGAp4M8ICXC/CAlwrwgNczEPBkgAe8XKGATyQSgKeLNHyiGLcY8KlowycCDH/CWQc7odapTsUVM92KK6quN3Ba10DLPcEdEQT4LmfdrEutdFpxxZE+xRV7Fdcb6tczMGGLOyobAHhuJ4Rdfa0G2thx444qovDWdXsigVv1ZNGEt9+mAzxZBOG52/KAJ4sePH8fDvBkkYMvu+sOeLJowVMP2QCeLFLw5CN1gCeLEjz9CC3gySIEL3hkHvBk0YEn1VOAFxQReMHWngvwZNGAd3EHPF0k4F3YAS8oAvBum3sK8ILCD+/ODnhBoYevwA54QWGHr+gOeLpww1fazecCPFmo4b24A54uzPCe3AFPF2J4T+yAFxQueLu0R3fA04UK3qKmXnAhCvBkYYK3vWbauzvg6UIEr+YOeLrQwFvWcuyAFxQWeGV3wNOFBJ7bzcusCniycMBX4Q54ujDA2/bt8u6ApwsBfNG92zwsuTrgyYIPb27v+NQrqujCW7t5wFNFFr509Q54qijCJ5xPvAKeKlrwzgdmE9UNBDxZEOETjqodCHiyAMLT7oAniw58QuAOeLLIwAvdAU8WFXiHufMxutDDZ55cvnwP4MmK5OQDs6GH73yhdBjw9ugn2pPJpPtA8wSCAgN/eOXm5j7je6qtre14t7M061arp1dxxZE+xRVPK643NCA6xnwyxlkyV+E7t5g7AZ+59Owwf0yd4I8dZO9sMr6/uGLFijcucDF+gdcyiutlVVdUXW90RHBE0b1sec7V8TVZrHR00nbaC/zikVH+N9YJ3ujC0uIB7OqthC+xKOzJk+WlSotLJy0eti0OzK5+91usfRPguYTuhQh350XA9gN5CbFXJ/jBzWvXpwFvWCeThnSSfzImlVPkVqrkTi8OGLy9uMLn76WbLkX1pM2dx/K8uYtO4Sj28Ll//rrAF6Ut+fx/xDW1VW5Jd3Erzv+Ysqu6b+/JoFzHBwW+sG+14GVfy2bBu96LLt/eBqyH5uhN03EwVbqtZgx0/C7KPZUiDgbmxl1A4IvXqia8600rqgI8vS+1yh+Z/8XJ/BW6/SFZciMlPM1fU76Lce4irMuNdURePzi36oMBb/37d5V+lPoFeQfb5klUPDb3mwsHHE/BmMeWXwDsW751WXC7bgnHI3eBgHc+AVY6xCm6XBy6U46Nsiw7ZII3NuHJDd/p7gXeNcCX4p75dLgXtlD7BcJcqyScKLzamXcvbckpEpSzLazA+Sbpe2epCDxJU394p7j9TrS1OTqPyH8pHFU8VQk+T2f7reavqARv6iftB4o/kJcTwFPJwDs28EyXfbN2wNvdiwAJ8/raWt95r8zaqVsXABG85Zm0X1Mk7ft561DhOMBTScCT23fxCMfGar8F4NC1XQIEm3bpYuOUtjbgEi1Z6UjAu+cdnnNP2t0dxMWdOL+U36QJ+fxCp3xR0Hk6uT8R8FRe4HPX2kWX3I+WrvFfqrhdO+E5ZMKdhE9Yu47ibiFVHJf/PnC6wp1AUYCnqgxvv+VVWOLc+IpaxC3qBL+wtHLhFiB/bOkCQJwPC172TwQ8VUV4237XXERzEfDmrYGyk1p3+ahjBQ8C5x6rV2AHPF0leMI9lw3e8Y1zz53Qun4orZaw/erir7P/UvqcaPrfiJeKNbzjZpYtjj1Zuv9k/Uj8tnLZBHU0LQ94ffDO29cO0GTGvnEXFhW+luDKCV036ZT1tCl5HOC1wZsPxHC78MKR+Wfn3CAFx7nCp1yuxgGvC75438p8xlIMT2oJ99v1//Rqj8UVvuh2rph9mz9nW2gdec5Z+YlDWBzhKWShpQu8zzQ+Vr63igG848EV27+EQFMAHHb3+MHzD6g6/znCzOm9OMIX7sQlyFto5o9BeUOFfwPj9/Jq5zNjZZf8uMDH7VZ9aS+fcrujDviIwduu3AsLBA+oAD5a8GXuogAfKfjS7fhk2cMXzgAfIXhray/coXFdEfDRgbd28+cAn4oRvHVz/hzgc8UFnnidjVuAjwg8+UIblwAfCXjHozaeAnwU4G1Px3hdEfARgLc9C+d5RcCHH97jY3XOAB92eCV2wIceXvLGvBXgww2vtLXnAnyo4ZXdAV8neO4DtdU+trzIrrKq+seWq35Ouvhjy30aWP6x5dkAwHOXRaUtXu1WXTFs8aGFr4Yd8OGFr2p7B3xo4YvPxYXs06t1DowmvLm1A15YFOFLe3nAC4sgfBLwHooevP1mHeCFRQ3e+ZILwAuLGDx3Jw7wwqIFz995B7ywSMGXPWoDeGFRgi9/tA7wwqIDTz1GC3hhkYEnH5sHvLCowNPPyQBeWETgBU/FAV5YJOCFT8ECXlgU4MVPvQNeWATgXV5yAXhh4Yd3e6UN4IWFHd79FVaAFxZy+AqvrAO8sHDDV3qHFOCFhRq+4gtpAS8szPCVX0ANeGHhhffyRljACwstvKd3TABeWFjhvb1RBvDCQgrvzR3w4kIJ72k3nwvwwsII79kd8OJCCO/dHfDiwgfvWT0FeJfCBu99a88FeGEhg5dzB7y4cMFLXL3nA7ywUMHLqacA71KY4KXdAS8uPPCSe/l8gBcWGngVd8CLCwu8kjvgxQUGPrt93cZ+IbwSO+BdCgx8ezPb/5IAXm1zTwHepcDAv7aXda41vuf+xTs6nRXcO+VLpxVWypXpVVyxT3G9oX7NA8+c55fUCX7nu2x4mfG9eenSpa+POMqzJ0Z0lh3VOq4OA0ez/JI6wb9ibPFrioeJXX1CaX+GXb2wwOzq27awA6Lr+Nr9/+O9Bnht8NmdGzcNAF7bwMDA2wO8/wMBTwZ4wMsFeMBLBXjA6xkIeDLAA14uwANeKsADXs9AwJMBHvByAR7wUgEe8HoGAp4M8ICXC/CAlwrwgNczEPBkgAe8XIAHvFSAB7yegYAnAzzg5QI84KUCPOD1DAQ8GeABLxfg3eH7bV8Bn4oLfCZzWcaodwLgzeIBP2ZMw5hcswFvFg94xm5xPS3g/R8oB79hTOH7kkvsPynAuwd4/wfKwZ85UiP4lz9/dS7AmwUO/qt3sH9rbP3s2Cv+fdW47zxuULflDi9pvLnx4VHjp1O3TbjxPQX4yxa8c8gI8GaBg3/+Q/23fmvdg8eu/cqqhvmHDOr84SUNq55v2Gr8NG/KqfumKcB/dNDttID3f2Al+PN//OyHXv2v6dddOXPV2JHczj1/eMkHR9gl/2T8dNNFV0+61NMHZTnhf7jYbSXA+z+w4nX83EkfH/3Gl97/1MxV4/PX6vnDSxpWP9+wzfjpwRvSP/sHL+4c/BfGT7wK1/G2gge/p+ExtuPyK2+4YkUBPn/4RxOumzAva/yUmj5h+m4F+EOFAG8WPPhaxd+dGzmRFZ4W8P4PrBP88ZvGXbTvi+2AN4sL/C3fHpqUefRmwJvFBf6Petkk1jUO8GbBg6/VZxw74SdvM+C3f0oE3+0szbrVSvcorjjSp7hir+J6QwOaB54d5pc4b3Nlz3CN1gT+taZ7Jtz9kV+L4LnLIrb42g+stMX7BM+6f7ZodYfotID3f2C94DuP5AK8WVzgv9Mw6UojwJvFBX78f7qdFvD+D6wT/NSjgHcUVPhzRhZ8+s5b582qDn7vn85daAR4s1DAf28Ze7pK+OunLvieEeDNQgF/1wH22yrhm3rcTgt4/wd6vY63w8//CVtWJfx3NwHeUWDh7TfuOm+9/a9FL4n3CH/9H3wML7a0Fwr4HbvZs49XB48XYnCFAv7/pt9zh/STeHghhmuhgFcKL8RwLS7weCEGV1zg8UIMrsDB1yypF2JwZwnwtR9YcYs/y4UXYrgWHXh/dvV4IQZXbOCz7716WPirAO//wDrBH5rS9Jk/mYwHcKziAn/dggvswoLrAW8WF/jGLuNL50TAmwUWPpGoKfx9zxhffnov4M2CCp9ImPIG/JZ77pzy/dtv6D//jc/fdOTMbTd/+c3/+crM+y/IwH/rA5NnTW64ffZs8lk+wPs/UAn+a+wf57GHdzzzGHtrxuEX2frH/+Wf2douGfgNVoDPFw74RWz5KrbwV9+e8cADdxx/6JF7F3TPn/nYgAx8ofWC0wLe/4EqN+5M+B//kB1d+bfr2cr5P93H5v5SBv7Qg8Ze/s5LAG8WKviBr98y642WmV+d/+mtn7xlttSu/nNzHr19w7TtgDcLBbxSTvix3f1fZie/CHizuMBP+g2b2jN4MeDN4gK/4oNH/+4zN84AvFlc4NmxocyzT58GvFng4FmWS9Gdgz+/uoVtWDIMeLPgwQ9yqco74edOPcD2fu6vAG8WOHifdvUXtxlf2hoBbxYX+CvfML7sxQcjWMUF/hdN8556pAkP2VrFBZ4dXjR34dui0wLe/4Eq8BuXVg9/ZHjw6TWiZ3IB7/9Ar/DJZE23+CfGdv1g2uS/AbxZUOGTSVPegN+wxHwpxqJv/sW13j61nL9V/9/ZS4+2NQHeLCzwxZdiLLqPHf60Cnzj79+cwjrs/8PBzJPLl+8BvLaBivDFJ2YXPcPY5d4e0uEewLn6ijXt195tW9L5Qukw4P0fqHIdb4P/S3b4Gk/uHHymuXnkvafO2pYcXrm5uc/4nvsX7+h01sU61UqnFVfM9Cqu2Ke43lC/5oFnzvNLaHj7rXob/J0zPtuiAl/+wQjHDrJ3ch+M07x06dLXyz4ym1/gd9nRqA8czfJLKsOXWrTFm3oZPPfBCPufy118Lph3E7Gr939gdQ/gKMOXfzDC7rdYu/lRWID3f2BgPhhhcPPa9WnAaxtYJ3h8MAJX4ODZea7aPB+PD0bgCh58rcIHI7gWG3jXAO//QMCTAR7wcgEe8FIBHvB6BgKeDPCAlwvwgJcK8IDXMxDwZIAHvFyAB7xUgAe8noGAJwM84OUCPOClAjzg9QwEPBngAS8X4AEvFeABr2cg4MkAD3i5AA94qQAPeD0DAU8GeMDLBXjASwV4wOsZCHgywANeLsADXirAA17PQMCTAR7wcgEe8FIBHvB6BgKeDPCAlwvwVcF3O0uzbrXSPYorjvQprtiruN7QgOaBZ4f5Jcr/E9EawnOXRWzxtR8YzC2eO0uAr/1AwJMBHvByAR7wUgEe8HoGAp4M8ICXC/CAlwrwgNczEPBkgAe8XIAHvFSAB7yegYAnAzzg5QI84KUCPOD1DAQ8GeABLxfgAS8V4AGvZyDgyQAPeLkAD3ipAA94PQMBTwZ4wMsFeMBLBXjA6xkIeDLAA14uwANeKsADXs9AwJMBHvByAR7wUgEe8HoGAp4M8ICXC/CAlwrwgNczMBjwLa0su33dxn7AaxsYBPjRtU+0svZmtv8lwGsbGAT47OjLrey1vaxzbQ69p6en66SzU+ykWl3diitm0oor9iquN3hW88DT5/kl+uEZ29XKdr7LhpcZBzcuXry4ZZSL8Qu8llVcT/vArPKKNRuoGX7/cy15+FeMLX5NcRl29f4PDMKuPg/ftoUdwHW8voGBgc/u3LhpAPDaBgYDngvw/g8EPBngAS8X4AEvFeABr2cg4MkAD3i5AA94qQAPeD0DAU8GeMDLBXjASwV4wOsZCHgywANeLsADXirAA17PQMCTAR7wcgEe8FIBHvB6BgKeDPCAlwvwgJcK8IDXMxDwZIAHvFyAB7xUgAe8noGAJwM84OUCPOClAjzg9QwEPBngAS8X4AEvFeABr2cg4MkAD3i5AA94qQAPeD0DAU8G+PrAdztLs2610j2KK470Ka7Yq7je0IDmgWeH+SXZAMBzl0Vs8bUfGMwtnjtLgK/9QMCTAR7wcgEe8FIBHvB6BgKeDPCAlwvwgJcK8IDXMxDwZIAHvFyAB7xUgAe8noGAJwM84OUCPOClAjzg9QwEPBngAS8X4AEvFeABr2cg4MkAD3i5AA94qQAPeD0DAU8GeMDLBXjASwV4wOsZCHgywANeLsADXirAA17PQMCTAR7wcgEe8FIBHvB6BoYDPplMKv11gBcWGnglecALAzwZ4AMDr/TXAV5YMOBbWlnmyeXL94jgceOu5gODAD+69olW1vlCaQHg/R8YBPjs6Mut7PDKzc19xg/nBwcHezqcnWQdanV2Kq6YSSuuqLrewBnNA/uG+CX64Rnb1cqOHWTvbDIOrlm4cOEun84DqkMi+P3PteThjS4sLS7Drt7/gUHY1efhd7/F2jcBXtvAwMAPbl67Pg14bQODAc8FeP8HAp4M8ICXC/CAlwrwgNczEPBkgAe8XIAHvFSAB7yegYAnAzzg5QJ8LeHf335c7a870aG2XmrXEcUVTyqu9+bbmgcefJ1fEgB4vqGFPv2fjoX95KjmgVv3VD5NTdv/C02DAO8a4KkAX/tCAT/8c93wL57SPHDvQc0Dj7yqaVA18CjEAT6mAT6mVQGf3b5uY3/tzknl7O/q0VJLq+Y/knvrkp9VAd/ezPa/VLtzUjn7u3o0lH8nkc4/suytS35WBfxre1nn2pqdEQ+Z7+rRVP6dRDr/SMdbl/yuCvid77LhZbU7J5Uz39WjrV2tmv9I21uX/K4K+FeMjWFN7c6Jp6x39WjJcND7R3JvXfKzKuDbtrADWq/j7e/q0ZLhoPeP5N665GfV3KrfuXHTQO3OSeXs7+rRkuGg94/k3rrkZ7gfH9MAH9MAH9OiCD8m4/hx2yzHj/um6jwvgS1+8N07dJ6XwBYp+MxDFzX9gN3ScFn/81dNvLuTsec+cfFD5w341o+9bh1tbPHPNjY2NvyK/ebPx808Xu/zXK8iBd989e/3j/2dscW3N/5Hes5s9r9Ne9umrt426/3Lt5aOLuzqt33idHfT1p6Hb6rvOa5f0YK/4s1s17ABv+QBxjr/cGTRPMYOtGy78Zr7bUfn4d//6D627h7GhsaN1Pcs161IwY+snnLpokEDfsFC46fxHXOfyi3d1vDdDx8vHZ2Dz9zwI8YWTZg0adJFJ+p6jutXpODfa2dHpz2d2+LnGFv8BzLff4Sxveu3zWBz5pSOzsH//W2jjK2+27gs7NP9ssGgFCn4p6492T5lNRvT+7uJL/fcfy97u+mNtmn/aty4O/7h31pHG/C7Pm7c8GMdH9nR9egX6n2e61Wk4M/eNf7ihy6wr03o/+VVE+46xdjP/2ziN4dzd+cWfilrHm3APzBm/Pjxi9mvrxk3va3e57leRQoeeQ/wMQ3wMQ3wMQ3wMQ3wMQ3wMQ3wMQ3wMQ3wMQ3wMe3/AUVTPJhyr41NAAAAAElFTkSuQmCC"> 


