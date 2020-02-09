#Part A
C = function(x){
  return(250*x[1] + 250*x[2] +250*x[3])
}
x0 = c(1,1,1)
A = matrix(c(1,1,1,550,300,400,1,0,0,0,1,0,0,0,1), nrow = 5, byrow = T)
B = matrix(c(100,50000,30,40,50))
ans = solnl(x0, C, A=A, B=B)
print(ans)