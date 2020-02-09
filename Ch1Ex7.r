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
x = seq(0,10,1)
profit.rate = function(x){
  return ((((200+5*x)*(0.65-0.01*x)-(100+0.45*x))/(x+90)))
}
plot(x,profit.rate(x))
d.profit.rate = function(x){fprime(profit.rate,x)}
opt.days = bisection(d.profit.rate,0,15)
print(opt.days)
profitrate = profit.rate(opt.days)
print(profitrate)


#Part B
#Sensitivity Analysis on the growth rate of the pig
growth.rate = seq(5,10,1)
opt.days = 0
profitrate = 0
for (i in 1:length(growth.rate)){
  x = seq(0,10,1)
  g = growth.rate[i]
  profit.rate = function(x){
    return ((((200+g*x)*(0.65-0.01*x)-(100+0.45*x))/(x+90)))
  }
  #plot(x,profit.rate(x))
  d.profit.rate = function(x){fprime(profit.rate,x)}
  opt.days[i] = bisection(d.profit.rate,0,15)
  print(opt.days)
  profitrate[i] = profit.rate(opt.days)
}
results = data.frame(growthrate = growth.rate, OptimalDays = opt.days, max.profit.rate = profitrate)
print(results)


#Part C
#Sensitivity Analysis on the growth rate of the pig
price.rate = seq(0.01,0.10,0.01)
opt.days = 0
profitrate = 0
for (i in 1:length(growth.rate)){
  x = seq(0,10,1)
  p = price.rate[i]
  profit.rate = function(x){
    return ((((200+5*x)*(0.65-p*x)-(100+0.45*x))/(x+90)))
  }
  #plot(x,profit.rate(x))
  d.profit.rate = function(x){fprime(profit.rate,x)}
  opt.days[i] = bisection(d.profit.rate,0,15)
  print(opt.days)
  profitrate[i] = profit.rate(opt.days)
}
results = data.frame(growthrate = growth.rate, OptimalDays = opt.days, max.profit.rate = profitrate)
print(results)
