#' Concurrent Model for Functional Regression
#'
#' It is a concurrent model, which may be fed to \code{\link{conformal.fun.split}}.
#'
#' @return A training and a prediction function.
#'
#' @details For more details about the structure of the inputs go to split.R
#'
#' @importFrom stats lm
#' @export concurrent


concurrent = function() {

  train.fun = function(x,t_y,y) {


    q=length(x[[1]])
    n_eval=length(t_y[[1]])
    xxx<-list2matrix(x)
    yyy<-list2matrix(y)
    full = ncol(yyy)
    full_x = ncol(xxx)

    idx=t(sapply(1:full, function(i){
      a<-(i-1)%%n_eval + 1
      return(seq(a,full_x,n_eval))}
      ))

    coeff=t(vapply(1:full, function(i) lm(formula = yyy[,i] ~  xxx[,idx[i,]])$coefficients,
    numeric(q+1)))

    return(coeff)
  }

  # Prediction function
  predict.fun = function(out,newx,t_y) {

    n0=length(newx)
    n_eval=length(t_y[[1]])
    q=length(t_y)
    p=length(newx[[1]])
    newx2<-list2matrix(newx)
    full=n_eval*q
    full_x=n_eval*p

    grid=c(0,seq(n_eval,full,length.out=q))

    idx=t(sapply(1:full, function(i){
      a<-(i-1)%%n_eval + 1
      return(seq(a,full_x,n_eval))}
    ))


    mat<-lapply(1:n0, function(i){

      vec<-sapply(1:full, function(k){
        return(c(1,newx2[i,idx[k,]])%*%out[k,]) #1,
        })

      return(lapply(1:q, function(j) vec[(grid[j]+1):grid[j+1]]))
    })

    return(mat)

  }

  return(list(train.fun=train.fun, predict.fun=predict.fun))
}
