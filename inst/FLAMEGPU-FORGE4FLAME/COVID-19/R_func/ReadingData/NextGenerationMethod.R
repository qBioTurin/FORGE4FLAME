############################
# Next Generation Method ###
############################

rho = .2        # recovery rate

## Model 1:1 # R0 = 5.11
# sigma1 = 0.0265 # death age1 rate
# sigma2 = 0.219  # death age2 rate
# lambda.u = 0.5 *1/5      # from E to Iu rate
# lambda.q = 0.2560349*1/5  # from E to Iq rate
# lambda.h = 0.2439651*1/5  # from E to Ih rate
# beta = c(0.0155,0.1465933,0.5258945)

## Model 1:10 # R0 = 5.09
sigma1 = 0.02627107 # death age1 rate
sigma2 = 0.216811  # death age2 rate
lambda.u = 0.9 *1/5      # from E to Iu rate
lambda.q = 0.05120698*1/5  # from E to Iq rate
lambda.h = 0.04879302*1/5  # from E to Ih rate
beta = c(0.008156856,0.08402692,0.3068882)

######### Contact matrices
load("~/Desktop/COVD19/DatiPerFra/TutteLeMatrici/MatrixTime0.RData")

Mij_u <- Mall
Mij_q <- Mh*(1- .8)
Mij_h <- Mo*(1- .9)

Ni <- c(733130, 2780600, 842676)   # size a0 a1 a2 

age_class <- length(Ni)
###### Calculation of F and V 

# F
zero <- matrix(0,ncol=age_class,nrow = age_class)

Nij <- as.matrix(Ni*beta,ncol=1) %*% (1/Ni)

Fu <- Nij * Mij_u 
Fq <- Nij * Mij_q
Fh <- Nij * Mij_h 

Fmatrix.1 = cbind(zero,Fu,Fq,Fh) 
Fmatrix = rbind(Fmatrix.1,matrix(0,nrow = 3*age_class,ncol = 4*age_class))

# V

V11<-diag(rep(lambda.h+lambda.q+lambda.u,3))
V1 <- cbind(V11,matrix(0,ncol = age_class*3,nrow = age_class))
V22 <- diag(c(rep(rho,age_class*2),rep(rho,age_class)+c(0,sigma1,sigma2)))
V21 <- rbind(diag(rep(-lambda.u,age_class)),diag(rep(-lambda.q,age_class)),diag(rep(-lambda.h,age_class)))
V2 <- cbind(V21,V22)

V <- rbind(V1,V2)


#### R0

V.inv<-solve(V)
Rmatrix<-Fmatrix%*%V.inv 
eigen.values<-eigen(Rmatrix)

max(eigen.values$values)

####### R0 subselection without undetected

V.noUndetected <- V[-c(4:6),]
V.noUndetected <- V.noUndetected[,-c(4:6)]

F.noUndetected <- Fmatrix[-c(4:6),]
F.noUndetected <- F.noUndetected[,-c(4:6)]

V.inv<-solve(V.noUndetected)
Rmatrix.Nound<-F.noUndetected%*%V.inv 
eigen.NoUnd<-eigen(Rmatrix.Nound)

max(eigen.NoUnd$values)
