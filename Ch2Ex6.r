install.packages("MASS")
install.packages("NlcOptim")

library(MASS);library(NlcOptim)
solnl(X = NULL, objfun = NULL, confun = NULL, A = NULL, B = NULL,
      Aeq = NULL, Beq = NULL, lb = NULL, ub = NULL, tolX = 1e-05,
      tolFun = 1e-06, tolCon = 1e-06, maxnFun = 1e+07, maxIter = 4000)

#I need to add teh constraint here

Units = function(x){(10000+((0.5/100)*(950-x[1])*10000)+(200/10000)*(x[2]-50000))}
P = function(x){(Units(x)*x[1])-(x[2]+Units(x)*700)*-1}
x0 = c(100,100) #this might have to be a solution to the contraint (0,100000)
Units(c(100,100))
A = matrix(c(0,1), nrow = 1, byrow = T)
B = matrix(100000)
ans = solnl(x0,P,A = A, B = B) #finds X1 and X2
print(ans)





#SENSITVITY ANALYSIS

#b) altering the price elasticity
P2 = function(e){
  Units = function(x){(10000+((e/100)*(950-x[1])*10000)+(200/10000)*(x[2]-50000))}
  P = function(x){(Units(x)*x[1])-(x[2]+Units(x)*700)*-1}
  x0 = c(100,100) #this might have to be a solution to the contraint (0,100000)
  Units(c(100,100))
  A = matrix(c(0,1), nrow = 1, byrow = T)
  B = matrix(100000)
  ans = solnl(x0,P,A = A, B = B) #finds X1 and X2
  print(ans)
}

e = seq(0.1,1,0.1)
ans.profit = 0
ans.x1 = 0
ans.x2 = 0
length(e)
for(i in length(e)){
  P2 = function(e){
    Units = function(x){(10000+((e[i]/100)*(950-x[1])*10000)+(200/10000)*(x[2]-50000))}
    P = function(x){(Units(x)*x[1])-(x[2]+Units(x)*700)*-1}
    x0 = c(100,100) #this might have to be a solution to the contraint (0,100000)
    Units(c(100,100))
    A = matrix(c(0,1), nrow = 1, byrow = T)
    B = matrix(100000)
    ans = solnl(x0,P,A = A, B = B) #finds X1 and X2
    print(ans)
  }
  ans.profit = -ans$value
  ans.x1 = P2(ans$par)[1]
  ans.x2 = P2(ans$par)[2]
}

results = data.frame(price.elasticity = price.elas, price = ans.x1, budget = ans.x2)
print(results)


#c) altering the quantity of new sales
Profit = function(a){
  P = function(x){
    return(((10000+((0.5/100)*(950-x[1])*10000)+(a/10000)*(x[2]-50000))*x[1])*(x[2]+(10000+((0.5/100)*(950-x[1])*10000)+(a/10000)*(x[2]-50000))*700)*-1)
  }
  x = c(100,100)
  ans = optim(x,P)
  return(ans)
}

Profit(200)
new.sale = seq(100,300,20)
ans.profit = 0
ans.x1 = 0
ans.x2 = 0

for(i in length(new.sale)){
  ans = Profit(new.sale[i])
  ans.profit = -ans$value
  ans.x1 = Units(ans$par)[1]
  ans.x2 = Units(ans$par)[2]

}

results = data.frame(quan.new.sale = new.sale, price = ans.x1, budget = ans.x2)
print(results)


