library(MASS);library(NlcOptim)

func = function(x){
  return(((339-0.01*x[1]-0.003*x[2])*x[1]
          +(399-0.004*x[1]-0.01*x[2])*x[2]
          -(400000+195*x[1]+225*x[2]))*-1)
}

#Problem1
x0 = c(1,1)
f = func
A = matrix(c(1,0,1,-1,0,0,1,1,0,-1),nrow=5)
B = matrix(c(3001,8001,10001,0,0),nrow=5)
ans = solnl(x0,objfun = func, A=A, B=B)
print(ans$par)


#Problem 2
#The R solution confirmed that x[1] is a binding constraint.When all of the constraints
#were increased by one unit, the only parameter that changed was the x[1] and since it was
#equal to the quantity of the constraint, it is binding.

#Problem Three

constraints = seq(5000,15000,500)
ans = 0
ans.x1 = 0
ans.x2 = 0

for (i in length(constraints)){
  x0 = c(1,1)
  f = func
  c = constraints[i]
  A = matrix(c(1,0,1,-1,0,0,1,1,0,-1),nrow=5)
  B = matrix(c(3001,8001,c,0,0),nrow=5)
  ans = solnl(x0,objfun = func, A=A, B=B)
  ans.x1[i] = ans$par[1]
  ans.x2[i] = ans$par[2]
  ans[i] = ans$fn
}

res = data.frame(constraint= constraints, x1 = ans.x1, x2 = ans.x2, profit = ans)
print(res)
#, lamdaconstraint = ans$lamda$ineqlin


