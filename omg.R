print(2)


x = 67
y = "LMAO"

x2 = x + 3


#Vector shit

v1 = c(3, 10, 4, 2, 25)
v2 = c("A", "2", "LMAO")
v3 = -10:10
v4 = v3^2

plot(v3, v4, type = "l")

v5 = sin(v3)

plot(v3, v5, type = "l", col = "blue", "lwd" = 3)

z = v1[3]

#matrices

data = c(3, 10, 2, 19, 0, 1)


m1 = matrix(data, 3, 2, byrow = TRUE)


m2 = matrix(c("XD", "LMAO"), 2 , 2, byrow = FALSE)

m3 = matrix(3, 2, 2)
m4 = matrix(15, 2, 2)

m5 = m3 * m4 #pointwise operation

m6 = m3 %*% m4 #producto matricial

m7 = t(m1) %*% m1


m8 = solve(m7)

#lists

A = 40
b = c("XD", "XD1", "XD3")
c = "GEORGE FLOYD"
d = "123.221.1.10"
e = matrix(c("LMAO", "LMAO1", "LMAO2", "LMAO3"), 2, 2)

l = list(age = A, what = b, xd = c, how = d, pepega = e)


#condition

o = -1
if (o > 1){
  
  print("omg")
  
} else if (o == 1) {
  print("What5")
} else {
  print("omg 2")
}


#for loop

n = 100
g = rep(0, n)
for (i in 1:n){
  g[i] = sqrt(i) + sin(i)

}

for (i in seq(1, n, by = 3)){
  g[i] = sqrt(i) + sin(i)
  
}


print(g)

#while loop


h = 10
j = rep(0, 2)
while (h <= 100){
  j[h] = h^2 + sqrt(h + 1)^2
  h = h + 1
}


#func

area.circ =  function(radius){
  
  x = pi * radius^2
  
  return (x)
}



r =  1

area.circ(radius = r)

