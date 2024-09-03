#' Title
#'
#' @param x blch
#' @param y asdf
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
  results <- data.frame(stratum = stratum)
  results$deff.h <- deff.h
  results$survey_wt <- weight
  results$deff_wt <-eff_adj_weight
  #Predicted values
  results$target<- y
  results$fitted <- nn.no_wt$fitted.values*scale + center
  results$fitted_weighted <- nn.wt$fitted.values*scale + center
  results$fitted_deff <- nn.eff_adj_wt$fitted.values*scale + center
  return(results)
}
