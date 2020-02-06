#Sensitivity analysis based on the cost of the pig per day
feed = seq(0.3,0.6,0.05)
pr = array(0,length(feed))
ans = array(0,length(feed))
for (i in 1:length(feed)){
    profit = function (x){
      return((0.65-0.01*x)*(200+5*x)-feed[i]*x)
    }
    dProfit = function(x){fprime(profit,x,)}
    ans[i] = bisection(dProfit,-100,50,0.0001)
    pr[i] = profit(round(ans[i]))
}
print(ans)
print(pr)
plot(feed,ans,"o",xlab="feed cost",ylab="x(Days to Sell)")
title("Sensitivity of Feed Cost of the Pig")
results = data.frame(cost = feed, days.to.sell = ans, profit = pr)
print(results)


growth = seq(1,10,1)
pr = array(0,length(growth))
ans = array(0,length(growth))
for (i in 1:length(growth)){
  profit = function (x){
    return((0.65-0.01*x)*(200+growth[i]*x)-(0.60*x))
  }
  dProfit = function(x){fprime(profit,x,)}
  ans[i] = bisection(dProfit,-100,50,0.0001)
  pr[i] = profit(round(ans[i]))
}
print(ans)
print(pr)
plot(growth,ans,"o",xlab="growth rate",ylab="x(Days to Sell)")
title("Sensitivity of growth rate of the Pig")
resultsg = data.frame(growth.rate = growth, days.to.sell = ans, profit = pr)
print(resultsg)
