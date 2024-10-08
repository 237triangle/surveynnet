#' Neural Net for Complex Survey Data
#'
#' @description
#' The surveynnet package extends the functionality of nnet (Venables and Ripley, 2002),
#' which already supports survey weights, by enabling it to handle clustered and stratified data.
#' It achieves this by incorporating design effects through the use of effective sample sizes in
#' the calculations, performed by the package described in Valliant et al. (2023), by following
#' the methods outlined by Chen and Rust (2017) and Valliant et al. (2018).
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
#' @return A dataframe containing the results of the neural net, using:
#' * no weights
#' * the user-inputted weights
#' * the new method that adjusts the weights by using a design effect incorporating cluster and strata
#'
#' @references * Venables WN, Ripley BD (2002). Modern Applied Statistics with S, Fourth edition.
#' Springer, New York. ISBN 0-387-95457-0, https://www.stats.ox.ac.uk/pub/MASS4/.
#' * Chen, S., and K. F. Rust. 2017."An Extension of Kish’s Formula for Design Effects to
#' Two- and Three-Stage Designs with Stratification.”, Journal of Survey Statistics and
#' Methodology,5 (2): 111–30.
#' * Valliant, R., J. A. Dever, and F. Kreuter. 2018. Practical Tools for Designing
#' and Weighting Survey Samples .2nd ed. New York: Springer-Verlag.
#' * Valliant, R., J. A. Dever, and F. Kreuter. 2023. PracTools: Tools for Designing
#' and Weighting Survey Samples, Version 1.4 . https://CRAN.R-project.org/package=PracTools
#'
#' @export
#'
#' @examples
#'
#' # short example with body fat dataset
#' y <- body_fat$pct_body_fat
#' x <- body_fat[,c("Weight_kg", "Height_cm", "Age")]
#' weight <- body_fat$survey_wt
#' strat <- body_fat$stratum
#' clust <- body_fat$cluster
#' y[strat==1] <- y[strat==1] + 30*0.00015*rnorm(sum(strat==1))
#' y[strat==2] <- y[strat==2] + 30*0.15*rnorm(sum(strat==2))
#' myout <- surveynnet(x,y,weight = weight, strat = strat, clust=clust)
#' myout
surveynnet <- function(x,y, weight, strat, clust, ...){
  args <- list(...)
  # a dummy arg for survival
  zz <- survival::Surv(1,1)
  # get y scale and center for undoing later
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
  args.nnet$trace <- FALSE # adding to suppress iter output
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
  results$fitted_deff_resid <- results$target - results$fitted_deff
  return(results)
}
