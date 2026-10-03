#' CONVERSION OF FUNCTIONAL DATA OBJECTS TO GRID EVALUATIONS OF FUNCTIONS
#'
#'
#' @param t_x Either NULL or a list of vectors (grid points in the various p dimensions
#' )
#' @param t_y Either NULL or a list of vectors (grid points in the various p dimensions
#' )
#' @param y An object of is "fData","mfData" or "fd".
#' @param x The input variable, a list of n elements. Each element is composed by a
#' list
#' @param x0 The input variable, a list of n0 elements. Each element is composed by a
#' list
#' @details It returns : t A list of vectors (grid points in the various p
#' dimensions)
#' y A grid of functional evaluated points (a list of list of vectors)
#'
#' @importFrom methods is
#' @noRd



convert2data = function(x,t_x,y,t_y,x0){

  flag_y<-(is.null(t_y) & (is(y,"fData") ||  is(y,"mfData") ))||(!is.null(t_y) & is(y,"fd"))

if(flag_y){
  sol=fun2data(t_y,y)
    t_y=sol$t
    y=sol$y
}


flag_x<- (is.null(t_x) & (is(x,"fData") ||  is(x,"mfData") )) ||
  (!is.null(t_x) & is(x,"fd"))

if(flag_x){
  sol=fun2data(t_x,x)
  t_x=sol$t
  x=sol$y
}

flag_x0<- (is.null(t_x) & (is(x0,"fData") ||  is(x0,"mfData") )) ||
  (!is.null(t_x) & is(x0,"fd"))




n=length(y)
p=length(y[[1]])
grid_size=vapply(y[[1]],function(x) length(x),integer(1))


if(is.null(x) && is.null(x0)){
  stop("Either x or x0 must be provided!\n")
}
if(is.null(x)){
  print("Since x is not provided, the method used for prediction will be the mean \n")
  funs = mean_lists()
  train.fun = funs$train.fun
  predict.fun = funs$predict.fun
  x = y ## I have to preserve the structure of y, the actual values are not meaningful
  ## Since I use the mean_list() function for regression !
}
if(is.null(x0)){
  print("Since x0 is not provided, the function will return fitted values
        with their prediction bands \n")
  x0 = x
}


if( (is.matrix(x) || is.data.frame(x)))
  x = table2list(x)

if(is.matrix(x0) || is.data.frame(x0))
  x0 = table2list(x0)

if(is.matrix(y) || is.data.frame(y))
  y = table2list(y)


if(is.matrix(t_x) || is.data.frame(t_x)){

  t_x = lapply(seq_len(ncol(t_x)), function(u) t_x[,u])

}

if(is.matrix(t_y) || is.data.frame(t_y)){

  t_y = lapply(seq_len(ncol(t_y)), function(u) t_y[,u])

}



return(list(x = x, y = y, t_x = t_x,t_y=t_y, x0 = x0))

}




fun2data=function(t,y_val){



  if(is.null(t)){



    if((is(y_val,"fData") )){ #only univariate

      n=y_val$N
      y= vector(mode = "list", length =n) #initialize, makes sense?

      el=seq(y_val$t0,y_val$tP,length.out=y_val$P)
      t <- list(el)
      y=lapply(seq_len(nrow(y_val$values)), function(i) list(y_val$values[i,]))


    }

    if(is(y_val,"mfData" )){ # even multivariate (roahd)

      n=y_val$N
      p=y_val$L
      t= vector(mode = "list", length =p)
      y=replicate(n, vector("list", p), simplify = FALSE)

      y=lapply(1:n,function(i) lapply(1:p,function(j){ return(y_val$fDList[[j]]$values[i,])}))
      t=lapply(1:p,function(j) seq(y_val$fDList[[j]]$t0,y_val$fDList[[j]]$tP,length.out
                                   =y_val$fDList[[j]]$P) )
    }}

  else{


    if(is(y_val,"fd")){ #Only for univariate data (fda)
      vale=fda::eval.fd(t,y_val)
      val=t(data.matrix(vale,rownames.force=TRUE))
      t_val=list(t)
      t=t_val
      y=lapply(seq_len(nrow(val)), function(i) list(val[i,]))
      rm(val)
      rm(t_val)
    }


  }

  return(list(t=t,y=y))


}



table2list = function(table){

    return(lapply(seq_len(nrow(table)), function(x) lapply(seq_len(ncol(table)),
                                                           function(s) table[x,s]) ))

}


list2matrix = function(lis){

  one_lis=lapply(lis, rapply, f = c)
  mat=do.call(rbind, one_lis)

  return(mat)
}

matrix2list = function(mat,grid){

  p=length(grid)
  grid=c(0,grid)
  n = nrow(mat)

  lis<-lapply(1:n,function(i) lapply(1:p, function(j) mat[i,(grid[j]+1):grid[j+1]]))

  return(lis)

}

matrix2roahd = function(mat,grid){

  p=length(grid)
  grid=c(1,grid)
  n = nrow(mat)

  return(lapply(1:p,function(j) t(sapply(1:n, function(i) mat[i,grid[j]:grid[j+1]]))))

}

