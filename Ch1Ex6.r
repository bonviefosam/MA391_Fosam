fprime = function (f,a,h=0.0001){(f(a+h)-f(a-h))/(2*h)}

bisection = function(f,a,b,tol=0.0001){
  if (f(a)*f(b) > 0){
    return ("Boundary Conditions Not Met")
  }
  else{
    middle = a
    while (abs(f(middle))>tol){
      middle = (a+b)/2
      if (f(middle)*f(a)>0) (a = middle)
      else (b = middle)
      x=middle
      y=f(middle)
      ## if you want to "see" what happens at every step, take off the # of the next line ##
      #cat(sprintf("x-Val: %.4f ; f(x-val): %.4f\n",x,y))
    }
    return (middle)
  }
}
#Part A
#x = seq(0,2000000)
r = 0.08
K = 400000
growthrate = function(x){
  return (r*x*(1-x/K))
}
profit = function(x){
  return ((0.06*x-500)*(8000-0.02*x))#Objective Function for Profit
}
d.profit = function(x){fprime(profit,x)}
population = bisection(d.profit,0,1000000000)
print(population)
E = (growthrate(population)*(1/(population*0.00001)))
print(E)
max.profit = profit(population)
print(max.profit)


#Part B
#Sensitivity to the cost of the whales
cost = seq(300,700,20)
max.profit = 0
effort = 0
for (i in 1:length(cost)){
  r = 0.08
  K = 400000
  c = cost[i]
  growthrate = function(x){
    return (r*x*(1-x/K))
  }
  profit = function(x){
    return ((0.06*x-c)*(8000-0.02*x))#Objective Function for Profit
  }
  d.profit = function(x){fprime(profit,x)}
  population = bisection(d.profit,0,1000000000)
  print(population)
  E = (growthrate(population)*(1/(population*0.00001)))
  effort[i] = E
  max.profit[i] = profit(population)

}

results = data.frame(cost = cost, profit = max.profit, effort.level = effort)
print(results)




#Part C
#Sensitivity to the price of fin whale carcass
price = seq(0.01,0.1,0.01)
max.profit = 0
effort = 0
for (i in 1:length(price)){
  r = 0.08
  K = 400000
  p = price[i]
  growthrate = function(x){
    return (r*x*(1-x/K))
  }
  profit = function(x){
    return ((c*x-500)*(8000-0.02*x))#Objective Function for Profit
  }
  d.profit = function(x){fprime(profit,x)}
  population = bisection(d.profit,0,1000000000)
  print(population)
  E = (growthrate(population)*(1/(population*0.00001)))
  effort[i] = E
  max.profit[i] = profit(population)

}

results = data.frame(price = price.per.fin, profit = max.profit, effort.level = effort)
print(results)


for(i in 1:length(cost)){
  profit = function(p){
    growthrate = function(x){
      return (r*x*(1-x/K))
    }
    x = c(50000,50000)
    d.growthrate = function(x){fprime(growthrate,x)}
    p= bisection(d.growthrate,0,1000000000)
    return(cost[i]*p - 500*(1/p*0.00001))
  }
  d.growthrate = function(x){fprime(growthrate,x)}
  p[i] = bisection(d.growthrate,0,1000000000)
  effort[i] = (growth.rate[i]*(1/(p*0.00001)))
}
results = data.frame(rate = r, population.level = p, effort.level = effort)
print(results)

