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
oldprice = function(x){
  return (0.65 - 0.01*x)
}

newprice = function(x){
  return (0.65 - 0.01*x +0.00004*x^2)
}

x = seq(0,150,10)
plot(x,oldprice(x), type='o', main = "New Price Func vs Original Price Func", xlab = "#days", ylab = "price")
lines(x, newprice(x),type = 'o', col = "red")
abline(0,0)
#The new price function is colored red

#Part B
profit = function (x){
  return((0.65-0.01*x+0.00004*x^2)*(200+5*x)-.45*x)
}
x = seq(0,20,1)
plot(x,profit(x),type="o")
dprofit = fprime(profit,x,)
opt = bisection(dProfit,-100,50,0.0001)
plot(x,dprofit,type="o")
abline(0,0)
profit(opt)



#Part C
#Sensitivity Analysis on the rate at which the price is leveling off
rate = seq(0.00001,0.00007,0.00001)
time = 0
opt_profit=0
for (i in 1:length(rate)){
  profit = function (x){
    return((0.65-0.01*x+rate[i]*x^2)*(200+5*x)-.45*x)
  }
  dProfit = function(x){fprime(profit,x)}
  time[i] = bisection(dProfit,-10,50)
  opt_profit[i]=profit(time[i])
  print(opt_profit)
}

result=data.frame(rate=rate,optimalDays=time,profit=opt_profit)
print(result)

plot(rate,time,"o",main ="Sensitivity of the Rate of Price Level", xlab="price rate",ylab="x(Days to Sell)")


