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
p = function(t){
  return((0.65-0.01*t)*((5*t-t^2/60)+200)-0.45*t)
}
dp = function(t){fprime(p,t)}
opt.days = bisection(dp,0,10)
print(opt.days)
profit = p(opt.days)
print(profit)

#Sensitivity Analysis
months = seq(1,10,1)
ans.time = 0
ans.profit = 0
for( i in 1:length(months)){
  m = months[i]
  p = function(t){
    return (0.65-0.01*t)*(((5/m)*(m*t-t^2/60)+200))-0.45*t)
  }
  dp = function(t){fprime(p,t)}
  ans.time[i] = bisection(dp,0,10)
  print(ans.time[i])
  ans.profit[i] = p(ans.time[i])
}

result = data.frame(months = months, days = ans.time, profit = ans.profit)
print(result)

