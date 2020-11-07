# Term plotting for GAM posterior samples

# Note that only univariate terms can be plotted, bivariate terms
# raise an error. Also you can only plot one term at a time, unlike plot.gam

# Example:
#
 library(mgcv)
 library(grDevices)

 set.seed(2)
 dat <- gamSim(1,n=400,dist="normal",scale=2)
dat$o <- runif(400)
 #b <- gam(y~s(x0)+s(x1)+s(x2)+s(x3),data=dat)
 b <- gam(y~s(x0)+s(x1)+s(x2,x3) + o, data=dat)

 samples <- rmvn(200, coef(b), vcov(b))
 plot_gamsamples(b, samples, 2)

plot_gamsamples <- function(model, samples, select=NULL, ...){

  # lazy
  i <- select

  # get plot data
  plotdat <- plot(model, select=i, col="white", n=200, n2=200, ...)

  if(!is.null(dim(plotdat[[i]]$raw))){
    stop("Smooths with > 1 term are not supported")
  }

  # extract and setup prediction data
  pdat <- list()
  for(ii in seq_along(plotdat)){
    if(is.null(dim(plotdat[[ii]]$raw))){
      pdat[[ii]] <- as.data.frame(plotdat[[ii]]$x)
      names(pdat[[ii]]) <- plotdat[[ii]]$xlab
    }else{
      pdat[[ii]] <- data.frame(x1=plotdat[[ii]]$x, x2=plotdat[[ii]]$y)
      names(pdat[[ii]]) <- c(plotdat[[ii]]$xlab, plotdat[[ii]]$ylab)
    }
  }

  pdat <- do.call(cbind.data.frame, pdat)

  # generate nonsmooth terms
  nst <- setdiff(attr(model$terms, "term.labels"), names(pdat))
  for(ii in seq_along(nst)){
    pdat[[ nst[ii] ]] <- 0
  }

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
