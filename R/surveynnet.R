#' Title
#'
#' @param x
#' @param y
#'
#' @return
#' @export
#'
#' @examples
surveynnet <- function(x,y, weight, size, maxit, strat, clust){
  # get y scale and center for undoing later
  myscale <- max(y)-min(y)
  mycenter <- min(y)
  # scale x and y to 0-1
  x.scale <- apply(x, 2, function(x2) (x2 - min(x2))/(max(x2) - min(x2)))
  y.scale <- (y - min(y)) / (max(y) - min(y))
  # calculate design effect
  df.deff <- data.frame(weight = weight, stratum = strat, clust = clust)
  deff<-deffCR(w = weight,
               strvar = strat,
               Wh = NULL,
               clvar = clust,
               nest = FALSE,
               y = y)
  # calculate deff.h
  df.deff <- left_join(df.deff, deff$`strata components`, by = 'stratum')
  deff.h <- df.deff$deff.w*df.deff$deff.c*df.deff$deff.s
  # calculate adjusted weights
  eff_adj_weight <- weight / deff.h
  # run nnet without weights, with weights, with effect-adjusted weights
  nn.no_wt <- nnet(x.scale, y.scale, size=size, maxit=maxit)
  nn.wt <- nnet(x.scale, y.scale, weight, size=size, maxit=maxit)
  nn.eff_adj_wt <- nnet(x.scale , y.scale, eff_adj_weight ,size = size, maxit = maxit)
  # collect and process results
  results <- data.frame(stratum = stratum)
  results$survey_wt <- weight
  results$deff.h <- deff.h
  results$deff_wt <-eff_adj_weight
  #Predicted values
  results$target<- y
  results$fitted <- nn.no_wt$fitted.values*scale + center
  results$fitted_weighted <- nn.wt$fitted.values*scale + center
  results$fitted_deff <- nn.eff_adj_wt$fitted.values*scale + center
  return(results)
}
