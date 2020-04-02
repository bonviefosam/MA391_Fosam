#####Chapter 4 Problem 4#####

###A) Can blue whale or fin whale populations coexist?

#Step 1: Ask the question

#Variables:
#x1 = blue whale population
#x2 = fin whale population
#rx1 = blue whale population growth rate
#rx2 = fin whlae population growth rate

#rx1 = 0.05*x1
#rx2 = 0.08*x2
#competition = alpha*x1*x2

#Assumptions:
#1. The number of blue whales and the number of fin whales cannot be less than zero.
#2. alpha cannot be less than zero

#Step 2: Select the modeling approach
#I will use a dynamical system to solve this problem

#Step 3: Formulate the model

f = function(x){
  c(0.05*x[1]-0.000001*x[1]*x[2],0.08*x[2]-0.000001*x[1]*x[2])
}

#Step 4: Solve the model

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

#Step 5: Answer the question

#The blue whlae and the fin whale species are able to coexist when there are 80,000 blue whales
#and 50,000 fin whales. These levels are dependent upon the competition effects.

###B) Draw the vector field for this model. Indicate the location of each equalibrium point.

ans=vectorfield(f,xlim=c(0,120000),ylim=c(0,100000),n=20,
                scale=0.05,col="red",main="Whale Population",xlab="BlueWhales",ylab="FinWhales")

points(eq1[1],eq1[2])
points(eq2[1],eq2[2])

###C) Classify each equalibrium point in the state space as stable or unstable.

#Both equalibrium points are unstable because any small deviation from the point will
#cause larger and larger deviations over time.

###D) Suppose that there are currently 5,000 blue whales and 70,000 fun whales. What does
#this model predict about the future of the two species.

#This model predicts that the population of blue whales will die off and approach zero
#while the population of fin whales continue to increase without any constraints.


