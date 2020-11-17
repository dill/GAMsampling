# Term plotting for GAM posterior samples

# Note that only univariate terms can be plotted, bivariate terms
# raise an error. Also you can only plot one term at a time, unlike plot.gam

# Example:
#
# library(mgcv)
# library(grDevices)
#
# set.seed(2)
# dat <- gamSim(1,n=400,dist="normal",scale=2)
# b <- gam(y~s(x0)+s(x1)+s(x2)+s(x3),data=dat)
#
# samples <- rmvn(200, coef(b), vcov(b))
# plot_gamsamples(b, samples, 2)

# Contributions from Jason Roberts for transformations of the variables
# and fixed effects support.

plot_gamsamples <- function(model, samples, select=NULL, ...)
{
  # lazy
  i <- select

  # get plot data
  plotdat <- plot(model, select=i, col="white", n=200, n2=200, ...)

  if(!is.null(dim(plotdat[[i]]$raw))){
    stop("Smooths with > 1 term are not supported")
  }

  # extract and setup prediction data
  pdat <- list()
  i.name <- all.vars(as.formula(paste0("~",plotdat[[i]]$xlab)))
  i.x <- seq(model$var.summary[[i.name]][1], 
             model$var.summary[[i.name]][3], 
             length.out=length(plotdat[[i]]$x)) # Can't use plotdat[[i]]$x directly; it could be transformed

  for (v in names(model$var.summary)){
    if (v == i.name){
      pdat[[v]] <- i.x
    }else{
      pdat[[v]] <- model$var.summary[[v]][1]
    }
  }

  pdat <- do.call(cbind.data.frame, pdat)

  # generate lp matrix
  lp <- predict(model, newdata=pdat, type="lpmatrix")

  ind <- model$smooth[[i]]$first.para:model$smooth[[i]]$last.para

  # zero columns not related to the term we want
  samples[, -ind] <- 0

  # linear predictor
  lp <- lp %*% t(samples)

  # get a transparent grey for samples
  grey_t <- adjustcolor("grey60", alpha.f=0.4)
  
  # apply the transform, if present, to the x coords before plotting
  i.x <- eval(parse(text=plotdat[[i]]$xlab), envir=pdat)

  # plot all those samples
  apply(lp, 2, lines, x=i.x, col=grey_t, lwd=0.5)

  # overplot the old mean/se lines
  lines(plotdat[[i]]$x, plotdat[[i]]$fit[,1])
  lines(plotdat[[i]]$x,
        plotdat[[i]]$fit[,1] + plotdat[[i]]$se,
        lty=2)
  lines(plotdat[[i]]$x,
        plotdat[[i]]$fit[,1] - plotdat[[i]]$se,
        lty=2)
}

