##########Chapter 5 Problem 1#########

#Reconsider Exercise 4 of Chapter 4

###A) Sketch the vector field for this model. Determine  the location of the equalibrium
#in the state space. Can you tell from the vector field which of the equiblibria are stable?

f = function(x){
  c(0.05*x[1]-0.000001*x[1]*x[2],0.08*x[2]-0.000001*x[1]*x[2])
}

options(warn=-1)
library(pracma)
##########################################################################
#### Quiver  - used by vector field ######################################
##########################################################################

quiver <- function(x, y, u, v,
                   scale = 0.05, angle = 10, length = 0.1, ...) {
  stopifnot(is.numeric(x), is.numeric(y), is.numeric(u), is.numeric(v))

  arrows(x, y, x+scale*u, y+scale*v, angle=10, length=length, ...)
}

###########################################################################
##### Vector Field ####################
###########################################################################

vectorfield = function(fun, xlim, ylim, n = 16,
                       scale = 0.05, col = "darkblue",xlab = "xlim", ylab="ylim",
                       main="",...) {
  stopifnot(is.numeric(xlim), length(xlim) == 2,
            is.numeric(ylim), length(ylim) == 2)

  xpts = linspace(xlim[1], xlim[2], n)#seq(xlim[1],xlim[2],length.out=n)
  ypts = linspace(ylim[1], ylim[2], n)#seq(ylim[1],ylim[2],length.out=n)

  M = meshgrid(xpts, ypts)
  x = M$X
  y = M$Y
  px=M$X
  py=M$Y
  for (i  in 1:n){
    for (j in 1:n){
      ans = fun(c(xpts[j],ypts[i]))
      px[i,j]=ans[1]
      py[i,j]=ans[2]
    }
  }


  plot(xlim, ylim, type="n",xlab=xlab,ylab=ylab,main=main); grid()
  quiver(x, y, px, py, scale = scale, col = col, ...)
  #return(list(px=px,py=py))
}

x0=c(80000,50000)
eq1 = zeros(f,x0)
print("Blue Whale / Fin Whale equalibrium (respective order)")
print(eq1)
x1 = c(0,0)
eq2 = zeros(f,x1)
print("Blue Whale / Fin Whale equalibrium (respective order)")
print(eq2)

ans=vectorfield(f,xlim=c(0,120000),ylim=c(0,100000),n=20,
                scale=0.05,col="red",main="Whale Population",xlab="BlueWhales",ylab="FinWhales")

points(eq1[1],eq1[2])
points(eq2[1],eq2[2])


#The model shows that the blue and fin whales can coexist at the equalibria, (80,000 , 50,000) and (0,0).
#Both of these equalibrium points seem to be unstable.

###B) Use the eigenvalue method to test the stability of each equalibrium in the state space.

x0=c(80000,50000)
x0=Zeros(f,x0)
print(x0)

A = Jacobian2(f,x0)
ev=eigen(A)
print(ev)

x0=c(0,0)
x0=Zeros(f,x0)
print(x0)

A = Jacobian2(f,x0)
ev=eigen(A)
print(ev)

#Both equalibria are unstable. When x1 = 80,000 and x2 = 50,000 one eigenvalue is positive while the other
#is negative. This indicates an unstable point. When x1 = 0 and x2 = 0, both eigenvalues are positive. This
#also indicates an unstable point.

####C) For each equilibrium point, determine the linear system that approximates the behavior of the original
#dynamical system in the neighborhood of the equalibrium point.


Zeros = function(f,x0,h=1E-4,tol=1E-4){
  i = 1
  p=c(1,1)
  while (Norm(p)>tol & i<100){
    p = solve(Jacobian2(f,x0),-f(x0)) # linear algebra step
    x0 = x0+p
    #print(i)
    #print(x0)
    i=i+1
  }
  return(x0)
}

path = function(f,x0,deltat=0.01,N=1000,tol=1E-4){
  len = length(x0)
  points=matrix(0,ncol=len)
  points[1,] = x0
  n = 0
  p = c(1,1)
  while(Norm(p)>tol & n<N){
    n=n+1
    p = f(x0)*deltat
    x0=x0+p
    points = rbind(points,x0)
  }

  rownames(points)=0:n
  return(points)
}

path_df1 = path(f,c(70000,40000),deltat = 1)
head(path_df1, 16)

path_df2 = path(f,c(5,5),deltat = 1)
head(path_df2, 16)

path_df3 = path(f,c(100000,65000),deltat = 1)
head(path_df3, 16)

path_df4 = path(f,c(70000,60000),deltat = 1)
head(path_df4, 16)

#The linear systems show that the x1 and x2 levels will continue to grow away from the equalibrium points.

####D) Sketch the complete phase portrait for this model.

ans=vectorfield(f,xlim=c(0,120000),ylim=c(0,100000),n=20,
                scale=0.05,col="red",main="Whale Population",xlab="BlueWhales",ylab="FinWhales")

points(path_df1, type = 'l',lwd = 3)
points(path_df2, type = 'l', lwd = 3)
points(path_df3, type = 'l', lwd = 3)
points(path_df4, type = 'l', lwd = 3)


####E) Given the current estimate of 5000 blue whales and 70000 fin whales, what does this model predict
#about the future of the two species.

path_df = path(f,c(5000,70000),deltat = 1)
head(path_df, 30)

ans=vectorfield(f,xlim=c(0,120000),ylim=c(0,200000),n=20,
                scale=0.05,col="red",main="Whale Population",xlab="BlueWhales",ylab="FinWhales")

points(path_df, type = 'l',lwd = 3)

#This model predicts that the the population of blue whales will die off while the population of
#fin whales grows indefinitely.

