x=seq(-5,5,0.1)
plot(x,dnorm(x,0,1),type = 'l',xlim=c(-4,4),ylim=c(0,0.4))
polygon(c(-4, x, 4), c(0,dnorm(x,0,1),0), col='gray')
