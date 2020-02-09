

pages = function(x){350 - ((50/100)*(x[2]-250))}
P = function(x){((80000-(5000/0.1)*(x[1]-1.50))-((350-pages(x)*(1000/50))))*-1}
x = c(10,10)
Units = (c(10,10))
ans = optim(x,P) #finds X1 and X2
print(ans)
P(ans$par)#shows the cost/revenue given the number produced

#SENSITIVITY ANALYSIS
#b) alter the quantity of lost sales due to paper price
pages = function(x){350 - ((50/100)*(x[2]-250))}
Profit = function(L){
  P = function(x){
    return(((80000-(L/0.1)*(x[1]-1.50))-((350-pages(x)*(1000/50))))*-1)
  }
  x = c(100,100)
  ans = optim(x,P)
  return(ans)
}

Profit(5000)
lost.sales = seq(4000,6000,200)
ans.profit = 0
ans.x1 = 0
ans.x2 = 0

for(i in length(lost.sales)){
  ans = Profit(lost.sales[i])
  ans.profit = -ans$value
  ans.x1 = Profit(ans$par)[1]
  ans.x2 = Profit(ans$par)[2]

}

results = data.frame(lost.sales.paper = lost.sales, price.per.newspaper = ans.x1, ad.price = ans.x2)
print(results)



#c) alter the quantity of lost sales due to advertsing price
pages = function(x){350 - ((L/100)*(x[2]-250))}
Profit = function(L){
  P = function(x){
    return(((80000-(5000/0.1)*(x[1]-1.50))-((350-(350 - ((L/100)*(x[2]-250)))*(1000/50))))*-1)
  }
  x = c(100,100)
  ans = optim(x,P)
  return(ans)
}

Profit(5000)
lost.sales2 = seq(4000,6000,200)
ans.profit = 0
ans.x1 = 0
ans.x2 = 0

for(i in length(lost.sales2)){
  ans = Profit(lost.sales[i])
  ans.profit = -ans$value
  ans.x1 = Units(ans$par)[1]
  ans.x2 = Units(ans$par)[2]

}

results = data.frame(lost.sales.ad = lost.sales2, price.per.newspaper = ans.x1, ad.price = ans.x2)
print(results)
