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


r=5/7
fT = function(x) {500*(200/(r*(x+1)))+(18000+800*200/(r*(x+1)))*x+(200/(r*(x+1))>14)*(10000*(200/(r*(x+1))-14))}
x = seq(0,50)
plot(x, fT(x))
dT = function(x){fprime(fT,x)}
bisection(dT,0,20)
fT(11.28368)


crews = x
cost = fT(x)
answer = data.frame(crews = crews, cost = cost)
print(answer)

#Part B
rate = seq(1/7,1,1/7)
numcrew = 0
cost = 0
for (i in 1:length(rate)){
  r=rate[i]
  fT = function(x) {500*(200/(r*(x+1)))+(18000+800*200/(r*(x+1)))*x+(200/(r*(x+1))>14)*(10000*(200/(r*(x+1))-14))}
  x = seq(0,50)
  #plot(x, fT(x))
  dT = function(x){fprime(fT,x)}
  numcrew[i] = bisection(dT,0,20)
  cost[i] = fT(numcrew[i])
}

result = data.frame(rate = rate, num.crews = numcrew, total.cost = cost)
print(result)


#Part C
fine = seq(5000,15000,1000)
numdays = 0
cost = 0
for (i in 1:length(rate)){
  r = 5/7
  f = fine[i]
  fT = function(x) {500*(200/(r*(x+1)))+(18000+800*200/(r*(x+1)))*x+(200/(r*(x+1))>14)*(f*(200/(r*(x+1))-14))}
  x = seq(0,50)
  #plot(x, fT(x))
  dT = function(x){fprime(fT,x)}
  numcrew = bisection(dT,0,20)
  numdays[i] = (200/(r*(numcrew+1))
  cost[i] = fT(numcrew[i])
}

result = data.frame(rate = rate, num.crews = numcrew, total.cost = cost)
print(result)




which(answer$cost == min(answer$cost))
#above gives the index people it start at zero so instead show below becuase you use it to find a frame of data in the array
answer[which(answer$cost == min(answer$cost)),]

