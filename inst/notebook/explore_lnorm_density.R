library("ggplot2")

x <- seq(0, 150, 1)
m <- 80
s <- 0.1

## These match
ggplot( data.frame(x = x, y1 = dnorm(x, m, s), y2 = dnorm(x/m, 1, s/m) / m ), aes(x)) + geom_line(aes(y=y1)) + geom_line(aes(y=y2), col=2)
## Likewise these match
ggplot( data.frame(x = x, y1 = dnorm(x, m, s*m)*m, y2 = dnorm(x/m, 1, s) ), aes(x)) + geom_line(aes(y=y1)) + geom_line(aes(y=y2), col=2)

# These match
df <- data.frame(x = x, y1 = dlnorm(x, log(m), s) * m, y2 = dlnorm(x/m, 0, s))
ggplot(df, aes(x)) + geom_line(aes(y=y1)) + geom_line(aes(y=y2), col=2)


# These match
df <- data.frame(x = x, y1 = dlnorm(x, log(m), s) * m, y2 = dlnorm(x, log(m), log(m)*s), y3 = dlnorm(x/m, 0, s))
ggplot(df, aes(x)) + geom_line(aes(y=y1)) + geom_line(aes(y=y2), col=2) + geom_line(aes(y=y3), col=3)

qplot(x, dlnorm(x, log(m), log(m)*s))
