
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
#OBJ FUNCTION, Solving for maximum rebate amount
profit = function (x){
  return ((1500-100*x)*(1+r*x)) #r*x*(1+(x/K))
}
x = seq(0,15,1)
plot(x,profit(x),type='o')

dProfit = function(x){fprime(profit,x)}
bisection(dProfit,0,20)

#Part B/C
#SENSITIVITY ANALYSIS
sales = seq(0.05,0.5,0.01)
ans = 0
ans.profit=0
for (i in 1:length(sales)){
    profit = function (x){
      return((1500-100*x)*(1+sales[i]*x))
    }
    dProfit = function(x){fprime(profit,x)}
    ans[i] = bisection(dProfit,-10,20)    
    ans.profit[i]=profit(ans[i])
}
result = data.frame(salesIncrease=sales,optimalRebate=ans,profit=ans.profit)
print(result)
plot(sales,ans,"o",xlab="Sales Increase",ylab="x (# of rebates)")



#Part D
rebateSize = seq(50,200,10)
ans = 0
ans.profit=0
for (i in 1:length(rebateSize)){
    profit = function (x){
      return((1500-rebateSize[i]*x)*(1+0.15*x))
    }
    dProfit = function(x){fprime(profit,x)}
    ans[i] = bisection(dProfit,-10,20)    
    ans.profit[i]=profit(ans[i])
}
result=data.frame(rebateSize=rebateSize,optimalNumber=ans,profit=ans.profit)
print(result)
plot(rebateSize,ans,"o",xlab="RebateSize",ylab="# of rebates")