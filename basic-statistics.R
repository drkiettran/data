# Basic Statistics

## Variance ...
y <- c(13, 7, 5, 12, 9, 15, 6, 11, 9, 7, 12)
plot(y, ylim=c(0,20))
range(y)
plot(1:11, y, ylim=c(0,20), pch=16, col='blue')
lines(c(4.5, 4.5), c(5, 15), col='brown')
lines(c(4.5, 3.5), c(5, 5), col='brown', lty=2)
lines(c(4.5, 5.5), c(15, 15), col='brown', lty=2)

plot(1:11, y, ylim=c(0,20), pch=16, col='blue')
abline(h=mean(y), col='green')
for (i in 1:11) lines(c(i,i), c(mean(y), y[i]), col='red')

# If we simply do a difference, the sum will always be `zero`
y-mean(y)
round(sum(y-mean(y)),3)
sum((y-mean(y))^2)/(length(y)-1)

## Variance function:
variance <- function(x) sum((x - mean(x)) ^ 2)/(length(x)-1)
variance(y)
var(y)
      