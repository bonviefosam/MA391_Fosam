#Ch2 5
#Part A

P = function(x){
  return(((339-0.01*x[1]-0.003*x[2])*x[1]
          +(399-0.004*x[1]-0.01*x[2])*x[2]
          -(400000+220*x[1]+250*x[2]))*-1)
}
x=c(500,500) #starting point for optimization
ans = optim(x,P,method="BFGS") #BFGS is a numerical technique used to incrementally calculate the Hessian for Newton
print(ans)
paste(c("Profits are maximized by selling:",round(ans$par[1])," 19 in TVs and ",
        round(ans$par[2])," 21 in TVs. Resulting in a profit of: $",
        format(round(-P(ans$par)),big.mark=",")),collapse="")
paste(c("Average Selling Price of 19 in set is: $",round(339-0.01*ans$par[1]-0.003*ans$par[2],2)),collapse="")
paste(c("Average Selling Price of 21 in set is: $",round(399-0.004*ans$par[1]-0.01*ans$par[2],2)),collapse="")


#Part B

P = function(x){
  return((200000+((339-0.01*x[1]-0.003*x[2])*x[1]
          +(399-0.004*x[1]-0.01*x[2])*x[2])
          -(550000+(400000+195*x[1]+225*x[2])))*-1)
}
x=c(500,500) #starting point for optimization
ans = optim(x,P,method="BFGS") #BFGS is a numerical technique used to incrementally calculate the Hessian for Newton
print(ans)
paste(c("Profits are maximized by selling: ",round(ans$par[1])," 19 in TVs and ",
        round(ans$par[2])," 21 in TVs. Resulting in a profit of: $",
        format(round(-P(ans$par)),big.mark=",")),collapse="")
paste(c("Average Selling Price of 19 in set is: $",round(339-0.01*ans$par[1]-0.003*ans$par[2],2)),collapse="")
paste(c("Average Selling Price of 21 in set is: $",round(399-0.004*ans$par[1]-0.01*ans$par[2],2)),collapse="")

#Part C AND Part D

R2 = function(r2){
  fr2 = function(x){
    return(((339-0.01*x[1]-0.003*x[2])*x[1]
            +(399-0.004*x[1]-0.01*x[2])*x[2]
            -(400000+(195+r2)*x[1]+(225+r2)*x[2]))*-1)
  }
  x = c(50000,50000)
  ans = optim(x,fr2,method = "L-BFGS-B")
  return(ans)
}
R2(25)

ans.x1 = 0
ans.x2 = 0
ans.rev = 0
r = seq(25,35,0.5)
for (i in 1:length(r)){
  ans = R2(r[i])
  ans.x1[i] = ans$par[1]
  ans.x2[i] = ans$par[2]
  ans.rev[i] = -ans$value
}
result = data.frame(growth_rate = r, x1 = ans.x1,x2 = ans.x2,rev = ans.rev)
print(result)


#part D

plot(r,ans.rev)
plot(r,ans.x1)
plot(r,ans.x2)

lm(ans.rev~r)
lm(ans.x1~r)
lm(ans.x2~r)
