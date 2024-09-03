#' Title
#'
#' @param x matrix or data frame of predictors
#' @param y vector of targets / response values
#' @param weight weights for each sample
#' @param size size for number of units in the hidden layer of nnets
#' @param maxit maximum number of iterations, default 20000
#'
#'
#'
#' @return a dataframe
#' @export
#'
#' @examples none
surveynnet <- function(x,y, weight, size=3, maxit=20000, strat, clust){
  # get y scale and center for undoing later
  myscale <- max(y)-min(y)
  mycenter <- min(y)
  # scale x and y to 0-1
  x.scale <- apply(x, 2, function(x2) (x2 - min(x2))/(max(x2) - min(x2)))
  y.scale <- (y - min(y)) / (max(y) - min(y))
  # calculate design effect
  df.deff <- data.frame(weight = weight, stratum = strat, clust = clust)
  deff<- PracTools::deffCR(w = weight,
               strvar = strat,
               Wh = NULL,
               clvar = clust,
               nest = FALSE,
               y = y.scale)
  # calculate deff.h
  df.deff <- dplyr::left_join(df.deff, deff$`strata components`, by = 'stratum')
  deff.h <- df.deff$deff.w*df.deff$deff.c*df.deff$deff.s
  # calculate adjusted weights
  eff_adj_weight <- weight / deff.h
  # run nnet without weights, with weights, with effect-adjusted weights
  nn.no_wt <- nnet::nnet(x.scale, y.scale, size=size, maxit=maxit)
  nn.wt <- nnet::nnet(x.scale, y.scale, weight, size=size, maxit=maxit)
  nn.eff_adj_wt <- nnet::nnet(x.scale , y.scale, eff_adj_weight ,size = size, maxit = maxit)
  # collect and process results
  results <- data.frame(stratum = strat)
  results$deff.h <- deff.h
  results$survey_wt <- weight
  results$deff_wt <-eff_adj_weight
  #Predicted values
  results$target<- y
  results$fitted <- nn.no_wt$fitted.values*myscale + mycenter
  results$fitted_weighted <- nn.wt$fitted.values*myscale + mycenter
  results$fitted_deff <- nn.eff_adj_wt$fitted.values*myscale + mycenter
  return(results)
}


#
#
#
# # note y must be drawn AFTER adding variation, which means after centering/scaling
# mydata <- read.csv("~/Downloads/body_fat_pct.csv")
# y <- mydata$pct_body_fat
# #y <- (mydata$pct_body_fat)*scale + center
# x <- mydata[,c("Weight_kg", "Height_cm", "Age")]
# weight <- mydata$survey_wt
# strat <- mydata$stratum
# clust <- mydata$cluster
#
# #y <- range01(y)
# y[strat==1] <- y[strat==1] + 30*0.00015*rnorm(sum(strat==1))
# y[strat==2] <- y[strat==2] + 30*0.15*rnorm(sum(strat==2))
#
# myout <- surveynnet(x,y,weight = weight, strat = strat, clust=clust)
# myout
#
#
# # add a note say8ing if you get the original strata etc just use that
#
# # if you only have the pseudo strata % cluster, use that in the function but be awware that results are an bound/ estimate for design effect
#
# # if you only replicate weights, sorry!
#
# # for the help file,
#
# # survey nnet can consider/incorporate the cluster and strata in survey data sets during the training networks,
#
