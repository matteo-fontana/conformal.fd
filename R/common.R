#'@noRd

res.list=function(left,right){

  s=length(left)
  r=length(left[[1]])


  cond=(length(left)!=length(right))|| (length(left[[1]])!=length(right[[1]]))

  if(cond)
    stop("Different dimensions in computing absolute residuals")

  for(ii in 1:s){
    for(jj in 1:r){
      left[[ii]][[jj]]=abs(left[[ii]][[jj]]-right[[ii]][[jj]])
    }
  }

  return(left)

}

depth.max=function(inp){

  n0<-length(inp)


  out<-t(sapply(1:n0, function(i){
    mat<-list2matrix(inp[[i]])
    mat<-abs(scale(mat))
    dep<-1/apply(mat,1,max)
    return(dep)

  }))

  return(out)

}

min.lol<-function(l){
  one<-length(l)
  two<-length(l[[1]])
  sol<-min(sapply(1:one, function(i) min(sapply(1:two, function(j) min(l[[i]][[j]])))))

  return(sol)

}

max.lol<-function(l){
  one<-length(l)
  two<-length(l[[1]])
  sol<-max(sapply(1:one, function(i) max(sapply(1:two, function(j) max(l[[i]][[j]])))))

  return(sol)

}

min3.lol<-function(f,s,t){
  return(min(min.lol(f),min.lol(s),min.lol(t)))
}

max3.lol<-function(f,s,t){
  return(max(max.lol(f),max.lol(s),max.lol(t)))
}

covered<-function(lo,up,y0){

  if(length(y0)>1)
    return(0)

  lo<-lo[[1]]
  up<-up[[1]]
  y0<-y0[[1]]
  p<-length(y0)

  out<-sum(sapply(1:p, function(j) sum(sapply(1:length(y0[[j]]), function(k)
    (lo[[j]][k] > y0[[j]][k] || up[[j]][k]<y0[[j]][k]) )   )))


  return(out<1)

}

is.char.vector<-function(vec){
  cond<-is.vector(vec)

  if(!cond)
    return(FALSE)

  cond=min(sapply(vec,is.character))
  return(as.logical(cond))


}

isColor <- function(x)
{
  res <- try(col2rgb(x),silent=TRUE)
  return(!"try-error"%in%class(res))
}
