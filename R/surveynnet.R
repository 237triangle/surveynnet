#' Title
#'
#' @param x Matrix or data frame of predictors. Must not contain any missing values.
#' @param y Vector of targets / response values. Must not contain any missing values.
#' @param weight The weights for each sample.
#' @param strat The stratum for each sample.
#' @param clust The cluster for each sample.
#' @param ... Additional arguments to be passed into `PracTools::deffCR` or `nnet::nnet`. See
#' documentation of those packages and functions for more details. Note that for the neural net (`nnet`),
#' the default here is set to 3 layers ("size" parameter) and maximum iterations ("maxit" parameter) is
#' set to 2000.
#'
#'
#'
#' @return a dataframe
#' @export
#'
#' @examples none
surveynnet <- function(x,y, weight, strat, clust, ...){
  args <- list(...)
  # get y scale and center for undoing later
  # note for later: should put in checks, eg that scale.y != 0 etc...
  # note for later: should I have default for weight, strat and clust? eg ==1...
  # note for later: need to remove any missing values for y and df right?
  scale.y <- max(y)-min(y)
  center.y <- min(y)
  # scale x and y to 0-1
  x.scale <- apply(x, 2, function(x2) (x2 - min(x2))/(max(x2) - min(x2)))
  y.scale <- (y - center.y) / scale.y
  # calculate design effect
  df.deff <- data.frame(weight = weight, stratum = strat, clust = clust)
  # collect args for deffCR
  args.deffCR <- args[
    intersect(names(args), names(formals(PracTools::deffCR)))
  ]
  # the below-vars are needed and hardcoded
  args.deffCR$strvar = strat
  args.deffCR$y = y.scale
  args.deffCR$clvar = clust
  args.deffCR$w = weight

  deff <- do.call(PracTools::deffCR, args.deffCR)
  # calculate deff.h
  df.deff <- dplyr::left_join(df.deff, deff$`strata components`, by = 'stratum')
  deff.h <- df.deff$deff.w*df.deff$deff.c*df.deff$deff.s
  # calculate adjusted weights
  eff_adj_weight <- weight / deff.h
  # collect args for all 3 nnet calls
  args.nnet <- args[
    intersect(names(args), names(formals(nnet::nnet.default)))
  ]
  args.nnet$x <- x.scale
  args.nnet$y <- y.scale
  if(!"size" %in% names(args.nnet)) {
    args.nnet$size = 3
  }
  if(!"maxit" %in% names(args.nnet)) {
    args.nnet$maxit = 2000
  }
  # run nnet without weights, with weights, with effect-adjusted weights
  nn.no_wt <- do.call(nnet::nnet.default, args.nnet)
  # add weights
  args.nnet$weights = weight
  nn.wt <- do.call(nnet::nnet.default, args.nnet)
  # modify weights with new method
  args.nnet$weights = eff_adj_weight
  nn.eff_adj_wt <- do.call(nnet::nnet.default, args.nnet)
  # collect and process results
  results <- data.frame(stratum = strat)
  results$deff.h <- deff.h
  results$survey_wt <- weight
  results$deff_wt <-eff_adj_weight
  #Predicted values
  results$target<- y
  results$fitted <- nn.no_wt$fitted.values*scale.y + center.y
  results$fitted_weighted <- nn.wt$fitted.values*scale.y + center.y
  results$fitted_deff <- nn.eff_adj_wt$fitted.values*scale.y + center.y
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
