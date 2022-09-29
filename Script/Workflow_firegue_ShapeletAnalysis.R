library(dplyr)
library(ggplot2)
library(Hmisc)

dat <- data.frame(
  year = seq(2000,2015),
  cropprob = c(
    0.126091,0.131417,0.156946,0.162775,
    0.125633,0.135369,0.145379,0.185848,
    0.218122,0.299426,0.3484405,0.744354,
    0.874748,0.874748,0.837759,0.875404
  )
)

par(mar = rep(2.5,4),
    mgp=c(1,0.5,0))
plot(as.character(dat$year), dat$cropprob,
     type="o", pch=21, bg='white',
     tck = -0.03, # let y tick inside the plot
     cex=1,
     cex.axis=1,
     ylim = c(0,1),
     col = "purple",
     xlab = "",
     ylab="")
minor.tick(nx = 0, ny = 2,   # Ticks density
           tick.ratio = 0.5)

