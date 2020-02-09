#same as ch2ex7 but add in the constraint
pages = function(x){350 - ((50/100)*(x[2]-250))}
P = function(x){((80000-(5000/0.1)*(x[1]-1.50))-((350-pages(x)*(1000/50))))*-1}
x = c(10,10)
Units(c(10,10))
A = matrix(c(0,1), nrow = 1, byrow = T)
B = matrix(400)
ans = solnl(x, objfun = P, A = A, B = B)#finds X1 and X2
print(ans)
Units(ans$par)#shows the cost/revenue given the number produced

#SENSITIVITY ANALYSIS
#b) alter the quantity of lost sales due to paper price
pages = function(x){350 - ((50/100)*(x[2]-250))}
Profit = function(L){
  P = function(x){
    return(((80000-(L/0.1)*(x[1]-1.50))-((350-pages(x)*(1000/50))))*-1)
  }
  x = c(100,100)
  A = matrix(c(0,1), nrow = 1, byrow = T)
  B = matrix(400)
  ans = solnl(x, objfun = P, A = A, B = B)#finds X1 and X2
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
  ans.x1 = Units(ans$par)[1]
  ans.x2 = Units(ans$par)[2]

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
  A = matrix(c(0,1), nrow = 1, byrow = T)
  B = matrix(c(0,400))
  ans = solnl(x, objfun = P, A = A, B = B)#finds X1 and X2
  return(ans)
}

Profit(5000)
lost.sales2 = seq(4000,6000,200)
ans.profit = 0
ans.x1 = 0
ans.x2 = 0

for(i in length(lost.sales2)){
  ans = Profit(lost.sales2[i])
  ans.profit = -ans$value
  ans.x1 = Units(ans$par)[1]
  ans.x2 = Units(ans$par)[2]

}

results = data.frame(lost.sales.ad = lost.sales2, price.per.newspaper = ans.x1, ad.price = ans.x2)
print(results)


#For both Sensitivity analyses, my methodology was to make a function
#in which the quantity of lost sales for the paper price and the advertising price respectivly
#was replaced by "L" which could be manipulated. Then the for loop would iterate
#through the lost.sales and lost.sales2 sequences (respectively) and spit out the corresponding profit for each of the
#differenent senarios.
