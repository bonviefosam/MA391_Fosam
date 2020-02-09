library(MASS);library(NlcOptim)
#PROBLEM 1#
obj=function(x){
  return((600-3*x[1]+x[2])*x[1]+(800-2*x[2]+x[1])*x[2])*-1
}
x = c(0,0) #solution for part a

#constraint

con = function(x){
  f = NULL
  f = rbind(f,(-5*X[2]+2500)/12)
  f = rbind(f,(-12*X[1]+2500)/5)
  return(list(ceq = f, c =NULL))
}

#x0 = c(1,1,1)
ans = solnl(x,objfun=obj, confun = con)
print(ans)


#PROBLEM 2#
obj=function(x){
  return((600-3*x[1]+x[2])*x[1]+(800-2*x[2]+x[1])*x[2])*-1
}
x = c(0,0) #solution for part a
x0 = c(3,5)
Aeq = matrix(c(12,5,-1,0,0,-1),nrow = 3, ncol = 2) #RHS
Beq = matrix(c(5000,0,0),nrow = 3)#LHS
ans = solnl(x0,obj,Aeq = Aeq, Beq = Beq)
print(ans)



#PROBLEM 3#
#Problems one and two gave the same solution while we had different solutions above
#becuase the x1 and the x2 of the non negative contraints were not negative . This makes
#it so that the x1 and x2 values must be positive.


#PROBLEM 4#
Outer = function(f,x){
  n1 = length(x[[1]])
  n2 = length(x[[2]])
  res = matrix(0,nrow=n1,ncol=n2)
  rownames(res) = x[[1]]
  colnames(res) = x[[2]]
  for (i in 1:n1){
    for (j in 1:n2){
      res[i,j]=f(c(x[[1]][[i]],x[[2]][[j]]))
    }
  }
  return(res)
}

x = list(x=seq(0,500), y=seq(0,500))
z=Outer(obj,x)
contour(x=x$x, y=x$y, z=-z, lwd = 3)
abline(a=10,b=-5/3,col="red")
contour(x=x$x, y=x$y, z=-z)
abline(a=10,b=-5/3,col="red")


#constraint
con = function(x){
  f = NULL
  f = rbind(f,(-5*X[2]+2500)/12)
  f = rbind(f,(-12*X[1]+2500)/5)
  return(list(ceq = f, c =NULL))
}

#x0 = c(1,1,1)
ans = solnl(x,objfun=obj, confun = con)
print(ans)



#Problem 5
FT = function(x){
  return((10+(22*x[1]^-0.5)+(1.3^x[2]^-0.1))-18^x[1]+x[2]*(5+(15*x[2]^-0.5)+(0.8*x[1]^-0.08)))
}
x0 = (0,0,0)
solnl(x0,FT)

#Problem 6
FT = function(x){
  return((10+(22*x[1]^-0.5)+(1.3^x[2]^-0.1))-18^x[1]+x[2]*(5+(15*x[2]^-0.5)+(0.8*x[1]^-0.08)))
}
x0 = (0,0,0)
A = matrix(c(2,3,-1,0,0,-1), nrow = 3, byrow = T)
B = matrix(c(18,3,2))
ans = solnl(x0,FT,A=A,B=B)
print(ans)

#Problem 7
FT = function(x){
  return((10+(22*x[1]^-0.5)+(1.3^x[2]^-0.1))-18^x[1]+x[2]*(5+(15*x[2]^-0.5)+(0.8*x[1]^-0.08)))
}
x0 = (0,0,0)
A = matrix(c(-50,-100,1,0,0,1), nrow = 3, byrow = T)
B = matrix(c(600,7,5))
ans = solnl(x0,FT,A=A,B=B)
print(ans)


#Problem 8
FT = function(x){
  return((10+(22*x[1]^-0.5)+(1.3^x[2]^-0.1))-18^x[1]+x[2]*(5+(15*x[2]^-0.5)+(0.8*x[1]^-0.08)))
}
x0 = (0,0,0)
Aeq = matrix(c(2,3,1,0,0,-1,0,0,1,0,0,-1,0,0,1), nrow = 3, byrow = T)
Beq = matrix(c(18,3,2))
A= matrix(c(1,1,1)nrow = 3)
B = matrix(c(0,0,0))
ans = solnl(x0,FT,A=A,B=B,Aeq=Aeq, Beq=Beq)
print(ans)
