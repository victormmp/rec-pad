fnormal1var <- function(x,m,r) {
  y <- (1/(sqrt(2*pi*r*r)))*exp(-0.5*((x-m)/(r))**2)
  
  return(y)
}

pcond2var <- function(x1,x2,m1,m2,r1,r2) {
  y <- (1/((2*pi*r1*r2)))*exp(-(0.5*((x1-m1)/(r1))**2 + 0.5*((x2-m2)/(r2))**2))
  
  return(y)
}

pcond2varcorr <- function(x1,x2,m1,m2,r1,r2,p) {
  y <- (1/((2*pi*r1*r2))*sqrt(1-p**2))*exp(-(1/(2*(1-p**2)))*(((x1-m1)/(r1))**2 + ((x2-m2)/(r2))**2 - ((2*p*(x1-m1)*(x2-m2))/(r1*r2))))
  
  return(y)
}

pCondManyVar <- <- function(x1,x2,m1,m2,r1,r2,p) {
  y <- (1/((2*pi*r1*r2))*sqrt(1-p**2))*exp(-(1/(2*(1-p**2)))*(((x1-m1)/(r1))**2 + ((x2-m2)/(r2))**2 - ((2*p*(x1-m1)*(x2-m2))/(r1*r2))))
  
  return(y)
}