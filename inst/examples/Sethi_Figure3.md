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
## Loading required package: data.table
```






```r
f <- function(x, h, p) {
    sapply(x, function(x) {
        S = max(x - h, 0)
        p[1] * S * (1 - S/p[2]) + S
    })
}
```



```r
# pars <- c(1.5, 0.05) K <- (pars[1] - 1)/pars[2]

pars <- c(1, 100)
K <- 100
xmin <- 0
xmax <- 1.5 * K
n_x <- 300
n_h <- n_x
x_grid <- seq(xmin, xmax, length = n_x)
h_grid <- seq(xmin, xmax, length = n_h)
delta <- 0.05
xT <- 0
OptTime <- 20
sigma_g <- 0.3
sigma_m <- 0.3
sigma_i <- 0.3
small_noise <- 0
profit <- function(x, h) pmin(x, h)
pdfn = pdfn_lnorm
```


The default pdfn is uniform


```r
g <- SDP_multiple_uncertainty(f, pars, x_grid, h_grid, OptTime, sigmas = c(sigma_g = sigma_g, 
    sigma_m = 0, sigma_i = 0), pdfn = pdfn, delta = delta)
```



```r
m <- SDP_multiple_uncertainty(f, pars, x_grid, h_grid, OptTime, sigmas = c(sigma_g = small_noise, 
    sigma_m = sigma_m, sigma_i = 0), pdfn = pdfn, delta = delta)
```



```r
i <- SDP_multiple_uncertainty(f, pars, x_grid, h_grid, OptTime, sigmas = c(sigma_g = small_noise, 
    sigma_m = 0, sigma_i = sigma_i), pdfn = pdfn, delta = delta)
```


Plot the policy function (in terms of escapement, `x-h`, rather than harvest `h`) at equilibrium (first time-step):


```r
require(reshape2)
policies <- melt(data.frame(stock = x_grid, g = x_grid[g$D[, 1]], meas = x_grid[m$D[, 
    1]], imp = x_grid[i$D[, 1]]), id = "stock")
```



```r
q1 <- ggplot(policies, aes(stock, stock - value, color = variable)) + geom_point() + 
    xlab("stock size") + ylab("escapement")
q1
```

![plot of chunk policyfunctions](data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAfgAAAH4CAMAAACR9g9NAAACzVBMVEUAAAAAujgCAgIDAwMEBAQFBQUGBgYHBwcICAgJCQkKCgoLCwsMDAwNDQ0ODg4PDw8QEBARERESEhITExMUFBQVFRUWFhYXFxcYGBgZGRkbGxscHBweHh4fHx8gICAhISEiIiIjIyMkJCQmJiYnJycqKiorKyssLCwtLS0uLi4vLy8wMDAyMjIzMzM0NDQ1NTU2NjY3Nzc4ODg5OTk6Ojo7Ozs8PDw9PT0+Pj4/Pz9AQEBBQUFDQ0NERERFRUVGRkZHR0dISEhJSUlKSkpLS0tMTExNTU1OTk5PT09QUFBRUVFSUlJTU1NVVVVWVlZXV1dYWFhaWlpbW1tcXFxeXl5fX19gYGBhYWFhnP9iYmJkZGRlZWVmZmZnZ2doaGhpaWlra2tsbGxtbW1ubm5wcHBycnJ0dHR1dXV2dnZ3d3d4eHh5eXl7e3t8fHx9fX1+fn5/f3+AgICBgYGCgoKDg4OEhISFhYWHh4eIiIiJiYmKioqLi4uMjIyNjY2Ojo6Pj4+QkJCRkZGSkpKTk5OUlJSVlZWWlpaXl5eYmJiZmZmampqbm5ucnJydnZ2enp6fn5+goKChoaGioqKjo6OkpKSlpaWmpqanp6eoqKipqamqqqqrq6usrKytra2urq6vr6+xsbGysrKzs7O0tLS1tbW2tra4uLi5ubm6urq7u7u8vLy9vb2+vr6/v7/AwMDBwcHCwsLDw8PExMTFxcXGxsbHx8fIyMjJycnKysrLy8vMzMzNzc3Ozs7Pz8/Q0NDR0dHS0tLT09PU1NTV1dXW1tbX19fY2NjZ2dna2trb29vc3Nzd3d3e3t7f39/g4ODh4eHi4uLj4+Pk5OTl5eXm5ubn5+fo6Ojp6enq6urr6+vs7Ozt7e3u7u7v7+/w8PDx8fHy8vLz8/P09PT19fX29vb39/f4dm34+Pj5+fn6+vr7+/v8/Pz9/f3+/v7////azfVRAAAACXBIWXMAAAsSAAALEgHS3X78AAAYSUlEQVR4nO3d/YNU1WGH8d2SxhQCWGmqScRqtCbSkKgx0YgR1Bo1amiNmm0q1hCjUtPGGtqk1RRSaXzhJRQQ6xJNAKn1JRAsVUkI1qBU3U1VVmB3lgXchci+zPkbenf27u7cM+fc83LPvfe8fJ8f1t2Z3Nwz82Hu3NeZJoKCrKnsAaByAnygAT7QAB9ogA80wAca4AMN8IEG+EADfKABPtAAH2iADzRl+A6qzsP0LXLt3ac3XU+33nT79Sbr6NUcp+b89vXRt+ShTgzAdx/Te4R7NZ+ZXs1/aF3v6E030Kk3XUVvsv1D9C15qBPACwN8HD0uwLMDPCfAMwM8L8CbCfCCAB9Hjwvw7ADPCfDMAM8L8GYCvCDAx9HjAjw7wHMCPDPA8wK8mQAvCPBx9LgAzw7wnADPzC746saVa3oBr5QX8O2tZMdTgFfKC/jN20jnCg58S9RvkamiZ9Mi+E2vkGNLov8+t379+l/0JWoBvMmGn83kE9xXJvyz0St+efTfl7Zu3fryoUSAN1nt2Uw+wYfKhG9bR3by3uMBb7AavEWL+uqmNWtHFzk0PFbu2Oms3LHcsR0f5zF8zd2qtfr66HEBnp06fAvg0/Ic3q7t+ProcQGenTL8yBs84Hn5Ch+v2AGeF+DNZACesQkiEeATjW7JOQTP3PgUB/j6xp5Eh+DximemBN8CeGH+wtd+cwi+uVnriQH8eHXvlu7ANzfryQN+rPq1JMDz8g++xU34bsAzU4Qf/cMheOzAYSYNn9wcBjwv3+Cp3SCA5wV4MwFeUM7w9H5Pp+Cxy5aRHHzD/m7X4NXlAT8c4KXzCr7xAJdr8OpPDOCZZ9U6Ba8V4JlnUwOelz/wzBMZAM8L8GYCvKDc4NlnLrkE39yMtfrGRPCcM9Ycgm+uwSvLA951+GOAZyWA552i6hZ8Mxb1DaXDc09Ndgle74kBPOCV8gKefy0C4Hn5AJ9yDQrgeXkAn3btEeB5uQ+fes0Z4HkB3kyG4LEdT8eFT7/I1DF4jT04ocILLi4GPC/AmwnwggzDiz5NwDF4vMc3xIYXfoqEa/DqBQnfAviA4VMnAzwvl+HF7m7Ba10oHSC8hLtz8OrygGcGeF7uwsu4Owevvj0XHLyUu1vwHTp7cEKDl3MHPDdH4cVb8CMBnpfL8BKTuQavXljwsu6A5+YkvLQ74LkB3kyAF2QCXt4d8NwchFdwdw0e++obCgJeZ59tOPAq7g7Cq27IBwOv5O4evPIenFDg1dwdg+8AfEMxvKK7a/Ajj1DpiQE8M/fgVQsDXtUd8Nycgld2Bzw3wJsJ8IKywKu7A56bQ/Aa7oDnBngzAV6QPryOO+C5OQOv5e4cvPoJ1r7D67m7Bq+xsx7wzADPyxF4TXeL4fdVkh0eiH7E8BWFug+o/K/HO9qnN12P3mSVwYM6U7WoPh2j9QzRt1TzYMfKnTCtV7zu693mVzw9LsAzAjw/n+H13QHPzQH4kTd4vdkBnpf98C2AT8tzeOmvEacCPC/r4eM3+IDg1VZofIUfXbELB15xTRbwzADPy3L4sS25cOCxqO+o34IPCF4tL+Hr9twAnhPgmQGel83w9btqAc/JQ/jELnrAc/IPPnloBvCcAM8M8LyshaeOxQKek2/w9DF4wHMCPDPA87IUvuGkm0Dgax97pbTP1i/4xpOtwoFXPLPeK/gWwEvnH3zypoDgw13Us86qDQRePY/gmWdTA56TP/Dss+iDgVf9NFvAM3MOXvlzjL2B51w2A3hOvsDzLpcCPCfAM3MOXjlP4LnXRwKekx/w/OtiA4OX34XjBTxjV+1oYcEr7LX1B559F+A5+QCf9gEIwcHLPkIP4FM/+CIseIXch0//wBPAcwI8M0fhQzosm+4eFrzKAXnX4QXuQcGLnotEjsOnbMGPFBJ8SK944T/yoODDeY8XL9zCgld4zTsNL/GmFh68pDzgmQGelw3wMmux4cFLPkKH4aW2XgKDl89deOGWXK1g4FXPvXIcXjgd4Dk5Cy+5mwrwnFyFl909GQy86hUVjsJL75YOB14xwDMDPK9y4eWPQ4UGL/3EOAmvcPwR8JwAzwzwvMqEVznhIDR46UsqHIRXOtEkJHjft+MBX1/ddrzSlrx78ErugOflHLyaO+B5uQav6B4QvN/v8aruQcErBXhmgOdVDryyO+B5OQWv7h4gvI977gAvs+dO7ilyCV7DHfC8HILXcQc8L3fg5c6qpQsOXjbH4JWnAzwnZ+D13J2HH/ju/fc/V924ck1voPCa7s7Ddz4e/WhvJTueChNe7w2+w3343Q890npw8zbSuSL6oysaR1tnsoP9nVpVuvWmO9KrN11Pl9ZkNXedCQ9qza6ze4i+pST4t35NXl676RVybEn0R+vixYu3DCYbqg4WWnWoyLnV3Iuc4SBpuKEE+B2Pbol+9i9+NnrFL49vo5dEXi/qdZfzHe4v6re+SNrXtq0jO0N8j9d+g+9wH/7IIytWdVc3rVnbJw3vzw6ckTd4vfm5Dt8QPS7GQRopeQfga+7y3x+fDPCc7IdvAXx99Lg8h5f+GnG68OB9eY+PV+wAH0ePy9e1+tEVesDH0eMCPDvAc7IcfmwLHvBx9Ljqz6tXuaLCbvjxPTeAj6PHNQ6vdimN1fB1e+wAH0ePKwkvL28zfP2eWsDH0eNKLurl5QHPzEl4pYW9xfCJQzOAj6PHRcFLP0J74ZOH5AAfR4/Lv825EOF7636GCk8dgw8BfmDg5IGonskZ4aWO0tgKT597EQL8hAlNE4abmw1e7vAc4JmVtKi/WDwBPS7P4BtOtrIafvWEkf8uOrH+Lw14iehx+bWobzzJzmr4w68bgn/6M2cMlxFeKivhGSdXWgb/pcvJv0zd9anjTv2PpRO/cWdE3Tb8+6KpF029ZSj6a/+lky94TQP+5AUvvxoVKjzrrFrL4B/7QO8lX1t501vnfHFp0x2vRtS13xc1LX2saX301/wZ+687WwP+w0eEE9Dj8g6eus0y+Pd+/+EP/OwXs849bc7S4waHF+613xe9f5Cc+A/RXxcef8b0k6SuwUjC3/M94UT0uDyCZ55Fbxk8mTf9o0Nf+fzbH5+zdFLtXb32+6KmZY81bYj+uun87h/9nYw7Bf/ZSVNOD/Y9nn31hG3wzzXdTp445bTzT31wBL72+/cnnzt5fjX6q2PW5FlbNeBfHSkzvMR6PeCZlbavfvCdavoE9Lh48CJ56+A5l0uFAb/nwonHb/9ce1Z4mUvObIPnjTkM+Iu/fnT6wG0XZYV3cFHPvT4yDPjf6yHTSdfEzPAS2QjPusM2eFOXzyfhz9oQwW/8uDa8s2fg8N+cLIOvHqYaMgK/edo1k6/+0JNZ4GXlrYJPWSkJA55UfrRw2d7UCehxAZ6dY/Cdrw+XBV72EdoEn7YVEgb8N5qmnxalDR/n2GHZ1K3PMOAn/bdwAnpc7OPxYnl74NP3OoQBP/NNU/AO7cBxEf63UWPw3VdcMv/KbPDb/nDeXVEZ4aXkrYEXjNUB+G8tIfdlhD9v5oJvRWWFl1na2wIv+jfqAPxVO8kvM8JPOyCcgB4XF17wCC2BFy6c7IQ/XA9/x7+SJRnhv7nWELw4m+DTprMUvn7lrvOSy/4y9ZR4Mfx5v/ORsE62FK+MOAD/xFby8J3Z4E2diCGRFfASK6EOwP/frGsuVz6Il8+JGBIBnllJ2/GGTsSQyQZ4mf0NYcBnPhHDqYM0Mu6BwGc+EUP+8Fz58FLutsEby+yJGM7Bi6ezDL76LpUdJ2LULeqt33Mn94K3Dj6fRX3WEzHGEz6rZcNLuocCX33tZ7vT/6/ocTkKL+seCPyrM6Z98g/OMrEDx/aDNIBP/HXugn7Sv+A8A/DCyoWXdg8EfmpX9KNzivfw8u62wjc3G4W/7oHoxw+vBfx4dsIPbzaPwa+75ooZ377s/N73vvKZC18/fOlFX3jhf7445/p+Ffivve+sK89qumzuXP5RPnpcLsIruDsB/2Xy9/PJLU88cDt5cfbun5JVd/7TP5IVXSrwq8fyGV7F3Qn4heT+peSun3x99g03XL7n5luvXVC5Y87tfWw/NvxIq9ImoMcFeHYFrtyNwv/gHvLmQ3+9ijx0xw+3k3k/VoF/9aZoKX/FiX7DK7m7BN/3Zxdf+fyWOV+64xPr//jiuUqL+k/feNtlq8/emAHe/n31au4uwGuVhD+u0vsFsu9z2eBjeUt34Ci6BwI//edk5oEjJ5iAFz2/JcGrugcC/+D73/ybT14wOxv8yC+A16qsgzRvHR14+L5DGeDHshNe2d02eFKl0nSn4N9btoWsXnTMBLzogHwp8Oru1sEfodKVT8LPm7mTbPv0XxiBFwR4ZiUt6k9oi360TfUVXsM9EPjTno9+bMv8wQgylQCv4x4I/L9Pm3/vrdM83WWr5R4IPNm9cN5dL6VOQI8L8OwKhF+zODv868eO3Lc89UguPS5n4PXcbYVvaTH6ir/7uK7vnH3WX/kIr+luKfzwgxmDX71o9FSMhV/903PkPrWcXqv/VfWkN9umeQiv6+4IfHwqxsLryO5P6MBP/c0LM8je1C8c3Et1oJ++Ra79XXrT9b2rNVkNXmfCwYrW/PZ2603WNUTfIgUfH5hd+AAhp8jt0qF24Jxx6vL2c65Ohe9MdrC/k1NLC++e4SrdaffyO9KrM1XNXWt+gwe0JuvUnKx7iL4l6ch6j6+D/3Oy+0wpdwp+oLV18LV7302bgF4SpeyyTV22Frqo117Q27qoT6zV18FfMftTW3TgTX4wgkXwLX7Dj7dwnZx6A7zJD0awDV76a8SThQFv6hsqJCoQvuYu//3xyZyCVyiAb6gYWc4DPg0+8wcjyAd4ZsLj8e9RmTken/mDEeQrDD5esfMF3lTmPxhB8gTrouBHV+gBnwovjh4XDS/7KTiAZwZ4XorwY1vwgE/mOfz4nhvAJzMOL1sh8HV77ACfzGv4+j21gE8GeEGAj6PHZTF84tAM4JN5DJ88JAf4ZHnCl3x0DvBp5Qifflw2d3jqGDzgk3kLT597AfhkgBcE+Dh6XJau3DWcbAX4ZJ7CN55kB/hkfsIzTq4EfDIv4Vkn1QI+GeAFAT6OHpeF8Myz6AGfzEN49tUTgE8GeEGAj6PHZR0853IpwCfLGZ6/8y4veN5lcoBPli98ysWKOcFzr48EfLJc4dMuUs0TnnUH4JN5Bs+fI+CT5QA/fnp12vG5XOBT/qUBPpl5+Ga5Ty7PAz7tAxAAnywfeInvKsgNnnMf4JPlCZ/6BbM5wKe5A54ql/d4mYuozMOnugOeKo+1+nLg097gOwBP588u23R3wFN5Ay9wBzxV3vBF7bIVLOgBT1cAPJsjD/i06QCfzBN4oTvgqfxY1IvdAU/lx8od4JXzAl7CHfBUPsDLuAOeygN4KXfAU7kPL+cOeCrACwJ8HD2usuEl3QFP5Tq8rDvgqQAvCPBx9LjKhZd2BzxV7vC57quXdwc8VRHwTBrAMwM8r3p4BXfAU7m8qFdxBzxVPvCFnGyp5A54qpzOshXLZ4YXnmyVDPDJ8oIXypuBl58O8MlyWdQX8YpXdAc8lavv8arugKdydc8d4DPmKLyyO+Cp3IRXdwc8Vf7wOey5U9ySqwX4ZIXAs4wywytOB/hkLsLruAOeysFFvZY74KkcXLkDvIlS4bfsItWNK9f01n7YAq/nDnh5+KEVd+8i7a1kx1O1H5bAa7oDXh6+OvT0LrJ5G+lcUfthB7yuO+Dl4Ql5ZhfZ9Ao5tqT2I/r7pw8++OBz/ckGqv16DWhNVYMvbnb9/VXNCXUnI/QtBcPveHRLDf7Z6MW+vPYjurGjra3t9UqywwMVrbp7dKaquWvN72C31mSVwYN60x3Sm6xniL6lYPhaEXzbOrLzqdqP+DZ6SVTool57QY9FvSp8ddOatX21HxbA6+yqHQ3wCvCs6HEVDi/9NeLJAJ/MJfiau/z3xycDfLKc4OtPvmIvntXhR5bzgDdTfvCCrysAPDPANxSv2AHeTM4s6kdX6AFvJldW7loAbzan4Id/AbyZHIEf33MDeDO5AV+3xw7wZnICvn5PLeDNBHhBgI+jx1UAfOLQDODN5AB88pAc4M0EeEGAj6PHlTs8dQwe8GayHp4+9wLwZioEPsu+enpPP+DNVAQ88yiNJHzDyVaAN5Pl8I0n2QHeTHbDM06uBLyZrF65Y51UC3gz5fSpV2Y+5w7wHe7Bi+Ql4Jln0QPeTBbDs6+eALyZ8oMXyAvhOZfNAN5Mea3cCeXl4BtvBryZrIXnXSYHeDPlCJ/+CAXw3MsjAW8mW7fjAT9aHurEWnj+9dCAN5Od8CnXwQPeTFbCp30AAuDNVAw8g1EIz7kP8GYq6EQMJfjUDzwBvJkshE91B7yhilrUN9zEhU97g+8AvKnsW7lLdwe8oayDF7gD3lC2wYvcAW8owAsCfBw9LrPwQnfAG8oueLE74A0FeEGAj6PHZRJewh3whioMniZlwMu4A95QRcInUBvhpdwBbyh74OXcAW8oexb1gB/OP3g6Gl7SHfCGKgi+EZWCl3UHvKEKOwMH8FKFBi/tDnhDFbaop29JwMu7A95QdqzcAX60sOAV3AFvKBvgVdwBbygL4JXcAW+o8uHV3AFvqNzgGz4Ug7evHvD1+Qlf7zsKr+gOeEOVDa/qDnhDlbyoF1w2wwjwZip55U7ZHfCGKhde3R3whioVXn1BD3hTlQ+vOB3gzVQmvI474A1VIryWO+ANBXhBgI+jxyUJ32C8V88d8IYq7mRLSlnTHfCGKusVr+sOeEMpw++rJDs8UJFpZJO9Jfmn1JRUR/t0pqpUevQmqwweLHR+PUP0LdU82Atduat/jWu/4PGKN1Q58PrugDdUkZtzSXf5749PBHgzlbIdD3huXsPX3OW/Pz4Z4M1UAvzIGzzgmfkL3wL4tHyFjzfnW+S/P54O8GYqB74D8Jw8gae/gmzcHfDsvIBv/NLBunOtAM8M8LwAb6ZC4YfVR28CPDMv4DtYL/jRmwDPzA94OsAL8xK+3h3w7HyEb04ciwU8M0/gE2t3gJfID/jEaj11+SzgmfkHT182DXhmfsDXLeobLpcHPDNP4McDvFy+wTe4A56dF/DD1MPezc2N7oBn5wN8czLqXsAzAzwvwJupMHj6XsAz8wG+Fs8d8Oy8gWev2XUAnpM/8LwAzwzwvABvJsALAnwcPS7AswM8J8AzAzwvwJsJ8IIAH0ePC/DsAM8J8MwAzwvwZgK8IMDH0eMCPDvAcwI8M8DzAryZAC8I8HH0uADPDvCcAM8M8LwAb6bM8Ls36z3Cd/bqTbf9V3rT7dOEf/oNzfnpTdb+JH1LHupEA57utWUmhiHfps3Fzu8HeT3z7LruKWhGgBcEeE6AN5o78Hs2mRiGfC+8VOz8Hq8UOrtDrQXNKDM8cjPABxrgAy0rfHXjyjW9RkYi0cB377//ueLmuGXXyMMrao7R/Ip7hFnh21vJjqeMjESizscLnOPQirt3jcysmDnW5lfcI8wKv3kb6VxhYiAy7X7okdaDRc2xOvT0rpGHV8wca/Mr7hFmhd/0Cjm2xMhIJHrr1+TltcXN8ZldIw+vqDlG8yvuEWaFfzb617ncyEjk6l9c3BwjiNrMippjND9S2CPMCt+2juws7D1+64ukfW1xc4wgajMrao7R/Ip7hJnX6jetWdtnZCQSHXlkxaru4uYYQdRmVtQco/kV9wixHR9ogA80wAean/ATBhJ/brgy8ef2mUWOxdJChK88UeRYLM0z+IGbj5/2HXJx08m9j50+5epOQh792Ak3vxfB7/rIf43dHb3iH546dWrTT8jP/2TinD1lj7mcPINvPeM3O457I3rFt0/9z+4b55L/nbatbeayDVe+fcr68btHFvUbPnaoMm39gVsuLHfEZeUb/KkvVLuORfCLbiCk83cHF84nZOeWDReceX3d3TX4tz+8nay8hpCjEwfLHXJJeQY/uGzGSQuPRPAL7or+mrR33r3Dt25o+uYH94zfPQw/cP73CVk4efr06ce/U+qIy8oz+NfayZtn3zf8ir8xesW/b+DbtxKybdWG2eTGG8fvHob/20uHCFl2dfRvYXu17EGXkmfw956zr33GMjKh540pTx+4/lry0rTn287+52jlbs8Hfzl2dwT/zEejFT+y90NPdN322bLHXE6ewb971aQTbu4nX57c++PTJ1+1n5B/+6MpXz02vDl31+ero3dH8DdMmDRp0vfIk2dOnNVW9pjLyTN4JBvgAw3wgQb4QAN8oAE+0AAfaIAPNMAHGuADDfCB9v+mBbKIalTcKgAAAABJRU5ErkJggg==) 


