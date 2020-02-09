
#Part A
profit = function(x){
  return(1.5*(80000+0.02*80000)*((x[1]-80000)/8000)
         +250*(350+0.01*350)*((x[1]-80000)/8000 + (0.15*350)*(x[2] - 30000)/6000)
         -(x[1]-x[2]+90000)*-1)
}

#CITE HERE FOR THE OBJECTIVE FUNCTION
x0 = (c(70000,40000))
Outer(Profit,x)
contour(x,Profit(x))
A = matrix(c(-1,0,1,0,0,-1,0,1,1,1), nrow = 5, byrow = T)
B = matrix(c(40000,80000,30000,50000,110000))
ans = solnl(x0,Profit,A = A, B = B)
print(ans)

#For Part A:
#I kept getting the error that the leading minor of order 2
#is not positive definite and I'm not sure what it means.
#My methodology was to set my matrix boundry conditions for
#linear inequality condtraints then use the solnl() function
#to solve the model

#Part B:
#Shadow Prices
#The shadow prices are the lamda of the solnl output. The sadow price
#indicates how much the objective value would change by in respose to a change in
#one unit of a given decision varable.
#The following are the lamda's (shadow prices) of the problem

#Lamda1 = -0.59. This means that for every dollar increase of editorial price, the
#profit will decrease by $0.59
#Lamda2 = 1.1875 This means that for every dollar increase in the weekly sales budget,
#the profit will increase by about $1.19

######CITE HERE FOR EACH LAMDA BUT SAY THAT i interpreted it myself.

#Part C: Graphing constraints
x1 = (c(10000,20000,30000,50000,70000,90000,110000))
x2 = (c(100000,90000,80000,60000,40000,20000,0))

plot(x2,x1, type = "o", main = "Contraints Plot")


#Part D : Sensitivity analysis on the

percent.drop = function(p){
  profit = function(x){
    return(1.5*(80000+2*p*80000)*((x[1]-80000)/8000)
           +250*(350+p*350)*((x[1]-80000)/8000 + (0.15*350)*(x[2] - 30000)/6000)
           -(x[1]-x[2]+90000)*-1)
  }


  x0 = (c(70000,40000))
  A = matrix(c(-1,0,1,0,0,-1,0,1,1,1), nrow = 5, byrow = T)
  B = matrix(c(40000,80000,30000,50000,110000))
  ans = solnl(x0,Profit,A = A, B = B)
  print(ans)

}

p = seq(0.005,0.015,0.001)
ans.profit = 0
for (i in 1:length(p)){
  ans2 = percent.drop(p[i])
  ans.profit[i] = ans2$value
}

results = data.frame(percent = p, profit = ans.profit)
print(results)

#For this Sensitivity Analysis my methodology was to make a function
#in which the percent that the editorial expense and the sales budget dropped
#was replaced by "p" which could be manipulated. Then the for loop would iterate
#through the p sequence and spit out the corresponding profit for each of the
#differenent percentages.


