#' Functional Multi Split Conformal Prediction Regions
#'
#' Compute prediction regions using functional multi split conformal inference.
#'
#' @param x The input variable, a list of n elements. Each element is composed by a list
#'  of p vectors(with variable length, since the evaluation grid may change).
#'  If x is NULL, the function will sample it from a gaussian.
#' @param t_x The grid points for the evaluation of function x. It is a list of vectors.
#' If the x data type is "fData" or "mfData" is must be NULL.
#' @param t_y The grid points for the evaluation of function y_val. It is a list of vectors.
#' If the y_val data type is "fData" or "mfData" is must be NULL.
#' @param y The response variable. It is either, as with x, a list of list of
#'  vectors or an fda object (of type fd, fData, mfData).
#' @param x0 The new points to evaluate, a list of n0 elements. Each element is composed
#'  by a list of p vectors(with variable length).
#' @param train.fun A function to perform model training, i.e., to produce an
#'   estimator of E(Y|X), the conditional expectation of the response variable
#'   Y given features X. Its input arguments should be x: list of features,
#'   and y: list of responses.
#' @param predict.fun A function to perform prediction for the (mean of the)
#'   responses at new feature values. Its input arguments should be out: output
#'   produced by train.fun, and newx: feature values at which we want to make
#'   predictions.
#' @param alpha Miscoverage level for the prediction intervals, i.e., intervals
#'   with coverage 1-alpha are formed. Default for alpha is 0.1.
#' @param split Indices that define the data-split to be used (i.e., the indices
#'   define the first half of the data-split, on which the model is trained).
#'   Default is NULL, in which case the split is chosen randomly.
#' @param seed Integer to be passed to set.seed before defining the random
#'   data-split to be used. Default is FALSE, which effectively sets no seed.
#'   If both split and seed are passed, the former takes priority and the latter
#'   is ignored.
#' @param randomized Should the randomized approach be used? Default is FALSE.
#' @param seed.rand The seed for the randomized version of the conformal.split.fun.
#' Default is FALSE.
#' @param verbose Should intermediate progress be printed out? Default is FALSE.
#' @param rho Vector containing the split proportion between
#' training and calibration set.
#' It has B components. Default is 0.5.
#' @param s.type The type of modulation function.
#'  Currently we have 3 options: "identity","st-dev","alpha-max".
#' @param B Number of repetitions. Default is 100.
#' @param lambda Smoothing parameter. Default is 0.
#' @param tau It is a smoothing parameter:
#' tau=1-1/B  Bonferroni intersection method
#' tau=0 unadjusted intersection
#' Default is 0.05, a value selected through sensitivity analysis .
#'
#' @return A list containing lo, up, tn. lo and up are lists of length n0,
#' containing lists of length p, with vectors of lower and upper bounds.
#' tn is the list of the grid evaluations.
#'
#' @details The work is an extension of the univariate approach to Multi Split
#' conformal inference to a multivariate functional context, exploiting the concept of
#' depth measures.
#' @details This function is based on the package future.apply to
#'  perform parallelisation. If this package is not installed, then the function
#'  will abort.
#'
#' @references "Multi Split Conformal Prediction" by Solari, Djordjilovic (2021) is
#' the baseline for the univariate case.
#'
#' @importFrom utils tail
#' @example inst/examples/ex.msplit.R
#' @export conformal.fun.msplit


conformal.fun.msplit = function(x,t_x, y,t_y, x0, train.fun, predict.fun, alpha=0.1,
                                split=NULL, seed=FALSE, randomized=FALSE,seed.rand=FALSE,
                                verbose=FALSE, rho=NULL,s.type="alpha-max",B=50,lambda=0,
                                tau = 0.08) {
#base tau 0.10


  if(is.null(rho) || length(rho)!=B)
    rho=rep(0.5,B)

  if (!is.null(seed)) set.seed(seed)

  ### CONVERT DATA

  check.null.data(y)
  conv = convert2data(x,t_x,y,t_y,x0)
  x = conv$x
  y = conv$y
  t_x = conv$t_x
  t_y = conv$t_y
  x0 = conv$x0
  n0 = length(x0)
  n=length(y)
  p=length(y[[1]])
  grid_size=vapply(y[[1]],function(x) length(x),integer(1))
  cum<-c(0,cumsum(grid_size))
  dims=tail(cumsum(grid_size),1)


  ### CHECK for TAU and LAMBDA

  check.pos.num(lambda)
  check.num.01(tau)

  tr = 2*tau*B + .001


  ## Needed to parallelize
  future::plan(future::multisession)
  options(future.rng.onMisuse="ignore")


  #Run B splits
  Y_lo_up <- future.apply::future_lapply(1:B, function(bbb) { #future.apply::future_



    out<-conformal.fun.split(x, t_x, y,t_y, x0, train.fun, predict.fun,
                             alpha*(1-tau) + (alpha*lambda)/B,
                             split, seed+bbb^2, randomized,seed.rand,
                             verbose, rho[bbb] ,s.type)



  return(rbind(unlist(out$lo),unlist(out$up)))

  })




  # Get the needed structures
  sol<-do.call(rbind,Y_lo_up)
  sol2<-Map(function(u,v) sol[,u:v], seq(1,ncol(sol),dims),
            seq(dims,ncol(sol),dims))
  joint<-lapply(1:n0,function(i) lapply(1:(2*B), function(k) lapply(1:p, function(j)
    Map(function(u,v) sol2[[i]][k,u:v], (cum[j]+1),
        cum[j+1])[[1]]    )))

  #print(joint)


  # Compute depth
  joint.dep<-t(sapply(1:n0, function(i) 1/apply(abs(scale(sol2[[i]])),1,max)))

  #return(list(joint=joint,joint.dep=joint.dep))



  #Compute level sets
  join <- lapply(1:n0, function (j) {
    o=order(joint.dep[j,],decreasing = TRUE)
    o.u=order(joint.dep[j,seq(2,2*B,2)],decreasing = TRUE)[1]
    #o.l=order(joint.dep[j,seq(1,2*B,2)],decreasing = TRUE)[1]
    a<-floor(tr)
    qt=o[1:a]
    obs<-joint[[j]][qt]
    #obs<-append(append(obs,joint[[j]][o.u]),joint[[j]][o.l])
    return(obs)
  })

  #return(join)



  lo<-lapply(1:n0, function(i){
    mat<-list2matrix(join[[i]])
    mat_min<-apply(mat,2,min,na.rm=T)
    return(lapply(1:p, function(j) mat_min[(cum[j]+1):cum[j+1]]))
  })

  up<-lapply(1:n0, function(i){
    mat<-list2matrix(join[[i]])
    mat_max<-apply(mat,2,max,na.rm=T)
    return(lapply(1:p, function(j) mat_max[(cum[j]+1):cum[j+1]]))
  })



  ## To avoid CRAN check errors
  ## R CMD check: make sure any open connections are closed afterward
  future::plan(future::sequential)

  return(list(lo=lo,up=up,tn=t_y))

}

