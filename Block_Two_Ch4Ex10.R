#######Chapter 4 Problem 10##########

#A population of 100,000 members is subject to a disease that is seldom fatal and leaves the
#victim immune to future infections by this disease. Infection can only occur when a susceptible
#person comes in direct contact with an infectious person. The infectious period lasts approximately
#three weeks. Last week, there were 18 new cases of the disease reported. This week, there were 40 new
#cases. It is estimated that 30% of the population is immune due to previous exposure.

###A) What is the eventual number of people who will become infected?

#Step 1: Ask the question

#Variables:
#x1 = the number of infected people
#x2 = the number of immune people
#x3 = the number of susceptible people
#t = number of weeks

#rx1 = rate at which people are infected (people/week)
#rx2 = rate at which people become immune (people/week)
#dt = change in time

#Given:
#1. The population is 100,000; x1 + x2 + x3 = 100,000
#2. x3(0) = 70,000
#3. x1(0) = 18
#4. x2(0) = 30,000
#5. rx1 = competiton*x1*x2

#Assumptions:
#1. x1, x2, and x3 cannot be less than zero.

#Step 2: Select the modeling approach

#I will use a dynamical system to solve this problem.

#step 3: Formulate the model

#18*70000 = 1260000
f = function(x){
  c((40/1260000)*x[1]*(100000-x[1]-x[2] - 1/3*x[1]), 1/3*x[1])
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

ans=vectorfield(f,xlim=c(0,80000),ylim=c(0,100000),n=20,
                scale= 0.05,col="red",main="Population Standings",xlab="Infected People",ylab="Immune People")


#Step 5: Answer the question

#The model depicts that most people within the population will be infected, then evenutally everyone will be immune.

####B) Estimate the maximum number if new cases in any one week.
#Assuming that we start when there are 18 infected people (x1 = 18) and 30,000 people immune (x2 = 30,000)
#the maximum number of people infected in a given week is a little over 40,000. After the number of people
#hit that maximum, the number of infections decrease, and the number of people immune begins to increase.

###C) Conduct a sensitivity analysis to investigate the effect of any assumptions you made in part (a) that were not supported by hard data.

#18*17500 = 315000
f = function(x){
  c((40/315000)*x[1]*(25000-x[1]-x[2] - 1/3*x[1]), 1/3*x[1])
}
ans1=vectorfield(f,xlim=c(0,20000),ylim=c(0,30000),n=20,
                 scale= 0.01,col="red",main="Population Standings (Pop = 25,000)",xlab="Infected People",ylab="Immune People")

#18*350000 = 6300000
f = function(x){
  c((40/6300000)*x[1]*(500000-x[1]-x[2] - 1/3*x[1]), 1/3*x[1])
}
ans2=vectorfield(f,xlim=c(0,800000),ylim=c(0,900000),n=20,
                 scale= 0.05,col="red",main="Population Standings (Pop = 500,000)",xlab="Infected People",ylab="Immune People")


f = function(x){
  c((40/12600000)*x[1]*(1000000-x[1]-x[2] - 1/3*x[1]), 1/3*x[1])
}
ans3=vectorfield(f,xlim=c(0,800000),ylim=c(0,1000000),n=20,
                 scale= 0.05,col="red",main="Population Standings (Pop = 1,000,000)",xlab="Infected People",ylab="Immune People")


#The models show that a change in the population size parameter has no impact on the shape of the model
#(compared to the original model). So, despite a change in the size of the population, most people will
#always get infected, and all people will eventually be cured. This case was tested by analyzing the effects
#of a 25,000 person population, a 500,000 person population, and a 1,000,000 person population.


####D) Perform a sensitivity analysis for the number of cases (18) reported  last week. It is thought by some
#that in early weeks the epidemic might be underreported.

#25*70000 = 1750000
f = function(x){
  c((40/1750000)*x[1]*(100000-x[1]-x[2] - 1/3*x[1]), 1/3*x[1])
}
ans4=vectorfield(f,xlim=c(0,80000),ylim=c(0,100000),n=20,
                 scale= 0.05,col="red",main="Population Standings (Initial # Infected = 25)",xlab="Infected People",ylab="Immune People")

#35*70000 = 2450000
f = function(x){
  c((40/2450000)*x[1]*(100000-x[1]-x[2] - 1/3*x[1]), 1/3*x[1])
}
ans4=vectorfield(f,xlim=c(0,80000),ylim=c(0,100000),n=20,
                 scale= 0.05,col="red",main="Population Standings (Initial # Infected = 35)",xlab="Infected People",ylab="Immune People")


#Despite a change in the original number of people infected, we still expect a very high number in the
#population to be infected, and we also expected them to all eventually be immune. This case was tested
#by analyzing the effects of an initial infection level of 25 people and an initial infection level of 35 people.







