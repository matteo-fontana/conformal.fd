library(roahd)

N = 3
P= 3
grid = seq( 0, 1, length.out = P )
C = exp_cov_function( grid, alpha = 0.3, beta = 0.4 )
values = generate_gauss_fdata( N,
                                      centerline = sin( 2 * pi * grid ),
                                      Cov = C )
fD = fData( grid, values )
x0=list(as.list(grid))
fun=mean_lists()
x0=list(as.list(grid))
fun=mean_lists()
true.jack = conformal.fun.jackplus (x=NULL,t_x=NULL, y=fD,t_y=NULL,
                                    x0=list(x0[[1]]), fun$train.fun,
                                    fun$predict.fun,alpha=0.1)

