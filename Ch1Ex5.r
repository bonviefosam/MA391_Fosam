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
r = 0.08
K = 400000
growthrate = function(x){
  return (r*x*(1-x/K))
}
d.growthrate = function(x){fprime(growthrate,x)}
p = bisection(d.growthrate,0,1000000000)
print(p) #the population at given x
HR = growthrate(p) #Maximum harvest Rate ~~ growth rate
print(HR)

E = (HR*(1/(p*0.00001)))
print(E)


#Part B
#Sensitivity to the intrinsic growth rate  #no returning the right numbers
rate = seq(0.01,0.16,0.01)
population = 0
effort = 0
for (i in 1:length(rate)){
  K = 400000
  growthrate = function(x){
    return (rate[i]*x*(1-x/K)*-1)
  }
  d.growthrate = function(x){fprime(growthrate,x)}
  population[i] = bisection(d.growthrate,0,1000000000) #200000
  print(population[i])
  HR=growthrate(population[i])
  effort[i] = HR/(population[i]*0.0001)
}
results = data.frame(rate = rate, population.level = population, effort.level = effort)
print(results)

#Part C
#Sensitivity to the maximum sustainable population  #no returning the right numbers
K = seq(100000,300000,10000)
population = 0
effort = 0
for (i in 1:length(K)){
  r = 0.08
  growthrate = function(x){
    return (r*x*(1-x/K[i])*-1)
  }
  d.growthrate = function(x){fprime(growthrate,x)}
  population[i] = bisection(d.growthrate,0,1000000000) #200000
  print(population[i])
  HR=growthrate(population[i])
  effort[i] = HR/(population[i]*0.0001)
}
results = data.frame(population.capacity = K, population.level = population, effort.level = effort)
print(results)

