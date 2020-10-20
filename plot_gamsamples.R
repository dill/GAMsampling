# Term plotting for GAM posterior samples
# library(mgcv)
# library(grDevices)
#
# set.seed(2)
# dat <- gamSim(1,n=400,dist="normal",scale=2)
# b <- gam(y~s(x0)+s(x1)+s(x2)+s(x3),data=dat)
#
# samples <- rmvn(200, coef(model), vcov(model))
# plot_gamsamples(b, samples, 3)

plot_gamsamples <- function(model, samples, select=NULL, ...){

  # lazy
  i <- select

  # get plot data
  plotdat <- plot(model, select=i, col="white", ...)
  # extract and setup prediction data
  pdat <- as.data.frame(lapply(plotdat, `[[`, name="x"))
  names(pdat) <- unlist(lapply(plotdat, `[[`, name="xlab"))
  # generate lp matrix
  lp <- predict(model, newdata=pdat, type="lpmatrix")

  ind <- model$smooth[[i]]$first.para:model$smooth[[i]]$last.para

  # zero columns not related to the term we want
  samples[, -ind] <- 0

  # linear predictor
  lp <- lp %*% t(samples)

  # get a transparent grey for samples
  grey_t <- adjustcolor("grey60", alpha.f=0.4)

  # plot all those samples
  apply(lp, 2, lines, x=pdat[[names(pdat)[i]]], col=grey_t, lwd=0.5)

  # overplot the old mean/se lines
  lines(plotdat[[i]]$x, plotdat[[i]]$fit[,1])
  lines(plotdat[[i]]$x,
        plotdat[[i]]$fit[,1] + plotdat[[i]]$se,
        lty=2)
  lines(plotdat[[i]]$x,
        plotdat[[i]]$fit[,1] - plotdat[[i]]$se,
        lty=2)

}
