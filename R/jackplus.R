#' Functional Jackknife + Prediction Regions
#'
#' Compute prediction regions using functional Jackknife + inference.
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
#'
#' @return A list containing lo, up, tn. lo and up are lists of length n0,
#' containing lists of length p, with vectors of lower and upper bounds.
#' tn is the list of the grid evaluations.#'
#'
#' @details The work is an extension of the univariate approach to jackknife +
#'  inference to a multivariate functional context, exploiting the concept of
#'  depth measures.
#' @details This function is based on the package future.apply to
#'  perform parallelisation. If this package is not installed, then the function
#'  will abort.
#'
#' @example inst/examples/ex.jackplus.R
#' @export


conformal.fun.jackplus = function(x,t_x, y,t_y, x0, train.fun, predict.fun, alpha=0.1) {

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
  acc_grid=c(0,cumsum(grid_size))
  alpha<-alpha/2

  # Check input arguments
  check.args(x=x, t_y=t_y,y=y,x0=x0,train.fun=train.fun,
             predict.fun=predict.fun, alpha=alpha)


  ### Parallel sessions

  future::plan(future::multisession)
  options(future.rng.onMisuse="ignore")

  ### Train and fit the full model

  out = train.fun(x,t_y,y)
  fit = predict.fun(out,x,t_y)
  pred = predict.fun(out,x0,t_y)


  ## Compute models without each observation

  updated_models = future.apply::future_lapply(1:n,function(jj){
    mod_jj = train.fun(x[-jj],t_y,y[-jj])
    return(mod_jj)})


  ## Compute LOO residuals

  Loo=vector('list',n) #(n,p,neval)
  for(jj in 1:n){

    Loo[[jj]]=predict.fun(updated_models[[jj]] , x[jj],t_y)[[1]]
    Loo[[jj]]=res.list(Loo[[jj]],y[jj][[1]])

  }

  ## fitted values

   fitted=future.apply::future_lapply(1:n0, function(i) future.apply::future_lapply(1:n,
                function(k) predict.fun(updated_models[[k]],list(x0[[i]]), t_y)[[1]]))


  # fitted values + or _ LOO

   joint<-future.apply::future_lapply(1:n0, function(i)
     append(
       lapply(1:n, function(j)
         lapply(1:p, function(k)  fitted[[i]][[j]][[k]] + Loo[[j]][[k]])
         ),
       lapply(1:n, function(j)
         lapply(1:p, function(k)  fitted[[i]][[j]][[k]] - Loo[[j]][[k]])
       )
     )
  )

  # compute depth

   joint.dep<-depth.max(joint)

  #get level set

   join <- future.apply::future_lapply(1:n0, function (j) {
     o=order(joint.dep[j,],decreasing = T)
     a<-ceiling((1-alpha)*2*n)
     qt=o[1:a]
     obs<-joint[[j]][qt]
     return(obs)
   })


   lo<-lapply(1:n0, function(i){
     mat<-list2matrix(join[[i]])
     mat_min<-apply(mat,2,min)
     low<-future.apply::future_lapply(1:p, function(j)
       mat_min[(acc_grid[j]+1):acc_grid[j+1]])
     return(low)
   })

   up<-lapply(1:n0, function(i){
     mat<-list2matrix(join[[i]])
     mat_max<-apply(mat,2,max)
     upp<-future.apply::future_lapply(1:p, function(j)
       mat_max[(acc_grid[j]+1):acc_grid[j+1]])
     return(upp)
   })


  ## To avoid CRAN check errors
  ## R CMD check: make sure any open connections are closed afterward
  future::plan(future::sequential)


  return(list(lo=lo,up=up,tn=t_y))
}

