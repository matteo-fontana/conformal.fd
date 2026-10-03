###  mfData #

library(roahd)

N = 10
P= 5
grid = seq( 0, 1, length.out = P )
C = exp_cov_function( grid, alpha = 0.3, beta = 0.4 )
Data_1 = generate_gauss_fdata( N, centerline = sin( 2 * pi * grid ), Cov = C )
Data_2 = generate_gauss_fdata( N, centerline = log(1+ 2 * pi * grid ), Cov = C )
mfD=mfData( grid, list( Data_1, Data_2 ) )
x0=list(as.list(grid))
fun=mean_lists()
final.mfData = conformal.fun.split(NULL,NULL, mfD,NULL, x0, fun$train.fun, fun$predict.fun,
                             alpha=0.2,
                             split=NULL, seed=FALSE, randomized=FALSE,seed.rand=FALSE,
                             verbose=TRUE, rho=0.5,s.type="identity")




