#' Plot Functional Split Conformal Confidence Bands
#'
#' The function plots the confidence bands provided by the \code{\link{conformal.fun.split}} #'function, \code{\link{conformal.fun.msplit}} and \code{\link{conformal.fun.jackplus}}.
#'
#' @param out The output of the split/msplit/jackknife+ function.
#' @param y0 The true values at x0.
#' @param ylab The label for the y-axes.
#' @param titles The title for the plot.
#' @param date A vector of dates.
#' @param ylim A vector containing the extremes for the y-axes.
#' @param fillc A string of color.
#' @return None
#' @details It exploits the package \code{\link[ggplot2]{ggplot}},
#' \code{\link[ggpubr]{ggarrange}} and \code{\link[ggpubr]{annotate_figure}}.
#' to better visualize the results. It outputs n0=length(x0) plots.
#'
#' @details It plots, for each value in x0, the predicted functional
#' value and bands in all the
#' dimensions of the multivariate functional response.
#'
#' @importFrom grDevices col2rgb
#' @export plot_fun



plot_fun=function(out,y0=NULL,ylab=NULL,titles=NULL,date=NULL,ylim=NULL,fillc="red"){

  tn = out$t
  pred = out$pred
  lo=out$lo
  up=out$up
  n0=length(lo)
  p=length(lo[[1]])
  grid_size=vapply(lo[[1]],function(x) length(x),integer(1))
  acc_size=c(0,cumsum(grid_size))
  nCol <- floor(sqrt(p))
  g<-vector("list",n0*p)
  p_cond<-!is.null(pred)
  y_cond<-!is.null(y0)
  l_cond<-!is.null(ylab) & is.char.vector(ylab) & (length(ylab)==p)
  t_cond<-!is.null(titles) & is.character(titles)
  d_cond<-F
  lim_cond<-!is.null(ylim) & (length(ylim)==2) & is.numeric(ylim[1]) & is.numeric(ylim[2])
  f_cond<-isColor(fillc)

  if(!f_cond)
    fillc="red"

  if(length(date)==length(tn))
    d_cond=sum(sapply(1:length(date), function(k)
      length(date[[k]])==length(tn[[k]])))==length(date)


  if(!t_cond)
    titles="Conformal Predicted Set"



  ##build data frames
  df<-lapply(1:n0, function(i) lapply(1:p, function(j) {

    df_base<-data.frame(t=c(tn[[j]]),
                        y_min=c(lo[[i]][[j]]), y_max=c(up[[i]][[j]]))

    if(p_cond)
      df_base<-cbind.data.frame(df_base,yp=as.numeric(pred[[i]][[j]]))

    if(y_cond)
      df_base<-cbind.data.frame(df_base,y_0=as.numeric(y0[[i]][[j]]))

    if(d_cond)
      df_base[,"t"]<-date[[j]]

    return(df_base)
  }))



  for(i in 1:n0){
    for(j in 1:p){

      g[[i]][[j]]<-ggplot2::ggplot()+
        ggplot2::geom_ribbon(data=df[[i]][[j]], ggplot2::aes(x=t,ymin = y_min, ymax = y_max),
                             fill = fillc, alpha = 0.3)

      if(l_cond)
        g[[i]][[j]]<-g[[i]][[j]]+ggplot2::labs(y = ylab[j])
      else
        g[[i]][[j]]<-g[[i]][[j]]+ggplot2::labs(y = paste("y",as.character(j)))


      if(p_cond){
        g[[i]][[j]]<-g[[i]][[j]]+ggplot2::geom_line(data=df[[i]][[j]],ggplot2::aes(x=t,y = yp,colour="predicted"),size=1)+ggplot2::scale_color_manual(name='',values=c("predicted"="blue"))
      }


      if(y_cond){
        if(p_cond){
          g[[i]][[j]]<-g[[i]][[j]]+ggnewscale::new_scale_color()
        }
        g[[i]][[j]]<-g[[i]][[j]]+
          ggplot2::geom_line(data=df[[i]][[j]],ggplot2::aes(x=t,y = y_0,colour="true value"),size=1)+ggplot2::scale_color_manual(name='', values=c("true value"="black"))
      }


      if(d_cond){
        g[[i]][[j]]<-g[[i]][[j]]+
          ggplot2::scale_x_datetime(labels =scales::date_format("%H:%M"),date_breaks = "3 hours")
      }

      if(j==p && n0>1)
        g[[i]][[j]]<-g[[i]][[j]]+ggplot2::labs(caption = paste("Observation ", as.character(i)),
                        x = "t")

      ##add legend

      g[[i]][[j]]<-g[[i]][[j]]+ ggplot2::theme(legend.position = "bottom")+
        ggplot2::theme(legend.key.size = ggplot2::unit(1, 'cm'), #change legend key size
              legend.key.height = ggplot2::unit(1, 'cm'), #change legend key height
              legend.key.width = ggplot2::unit(1, 'cm'), #change legend key width
              legend.title = ggplot2::element_text(size=20), #change legend title font size
              legend.text = ggplot2::element_text(size=15),
              axis.text=ggplot2::element_text(size=14),
              axis.title=ggplot2::element_text(size=16)) #change legend text font size

      if(lim_cond)
        g[[i]][[j]]<-g[[i]][[j]]+ ggplot2::ylim(ylim)
    }

  }

  return(lapply(1:n0,function(i){
    ppp<-do.call(ggpubr::"ggarrange", c(g[[i]],common.legend = T,nrow=2,legend = "bottom"))

    ggpubr::annotate_figure(ppp, top = ggpubr::text_grob(titles,
                                         color = "black", face = "bold", size = 22))

  }))


}

utils::globalVariables(c("y_max", "y_min", "yg","yp","y_0"))

extract_legend <- function(my_ggp) {
  step1 <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(my_ggp))
  step2 <- which(sapply(step1$grobs, function(x) x$name) == "guide-box")
  step3 <- step1$grobs[[step2]]
  return(step3)
}
