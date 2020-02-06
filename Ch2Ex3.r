f = function(x){(x[1]*x[2]-2*x[1]-2*x[2]-x[1]^2-x[2]^2)*(-1)}

#x = (c(-2,-2))
optim(x,f)

Blue = function(x){(0.05*x[1]*(1-x[1]/150000)-1/100000000*x[1]*x[2])}
Fin = function(x){(0.08*x[2]*(1-x[2]/400000)-1/100000000*x[1]*x[2])}

Rev = function(x){(12*(0.05*x[1]*(1-x[1]/150000)-(10^-8)*x[1]*x[2]) +

                     6*(0.08*x[2]*(1-x[2]/400000)-(10^-8)*x[1]*x[2]))*(-1)}#mulitply fy negative one because we are doing maximization

x = c(50000,50000)
ans = optim(x,Rev,method = "L-BFGS-B")
print(ans$par) #prints x1 and x2
print(ans$val) #prints the objective value
Blue(ans$par)#number of blue whats being harvested

#Part B AND Part C
#SENSITIVITY ANALYSIS for optimal population levels#
#Sensitivity of X1, x2, and revenue in response to change in Blue whale intrinsic growth rate
R1 = function(r1){
  fr1 = function(x){(12*(r1*x[1]*(1-x[1]/150000)-1/100000000*x[1]*x[2]) +

                       6*(0.08*x[2]*(1-x[2]/400000)-1/100000000*x[1]*x[2]))*(-1)}
  x = c(50000,50000)
  ans = optim(x,fr1,method = "L-BFGS-B")
  return(ans)
}
ans.x1 = 0
ans.x2 = 0
ans.rev = 0
r = seq(0.04,0.12,0.01)
for (i in 1:length(r)){
  ans = R1(r[i])
  ans.x1[i] = ans$par[1]
  ans.x2[i] = ans$par[2]
  ans.rev[i] = -ans$value
}
result = data.frame(growth_rate = r, x1 = ans.x1,x2 = ans.x2,rev = ans.rev)
print(result)

#Sensitivity of  X1, x2, and revenue in response to change in Fin whale intrinsic growth rate
R2 = function(r2){
  fr2 = function(x){(12*(0.05*x[1]*(1-x[1]/150000)-1/100000000*x[1]*x[2]) +

                       6*(r2*x[2]*(1-x[2]/400000)-1/100000000*x[1]*x[2]))*(-1)}
  x = c(50000,50000)
  ans = optim(x,fr2,method = "L-BFGS-B")
  return(ans)
}
ans.x1 = 0
ans.x2 = 0
ans.rev = 0
r = seq(0.04,0.12,0.01)
for (i in 1:length(r)){
  ans = R2(r[i])
  ans.x1[i] = ans$par[1]
  ans.x2[i] = ans$par[2]
  ans.rev[i] = -ans$value
}
result = data.frame(growth_rate = r, x1 = ans.x1,x2 = ans.x2,rev = ans.rev)
print(result)


#Part D
alpha = function(a){
  fr2 = function(x){(12*(0.05*x[1]*(1-x[1]/150000)-a*x[1]*x[2]) +

                       6*(0.08*x[2]*(1-x[2]/400000)-a*x[1]*x[2]))*(-1)}
  x = c(50000,50000)
  ans = optim(x,fr2,method = "L-BFGS-B")
  return(ans)
}
ans.x1 = 0
ans.x2 = 0
r = seq((1/1000000000000),(1/10000),0.00001)
print(r)
for (i in 1:length(r)){
  ans = alpha(r[i])
  ans.x1[i] = ans$par[1]
  ans.x2[i] = ans$par[2]
}
result = data.frame(alpha = r, x1 = ans.x1,x2 = ans.x2)
print(result)
plot(r, alpha(r))

