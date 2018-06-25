
### R Helper
FindParameter <- function(prob, eta) {
  sapply(prob, function(i) {
    if (i<.009) {
      return (.1)
    }
    if (i>.995) {
      return (eta-.05)
    }
    return(min(skellam$beta[skellam$prob>i]))
  })
}


# skellam

dskellam <- function(x, mu1, mu2) {
  return(exp(-(mu1+mu2))*(mu1/mu2)^(x/2)*besselI(2*sqrt(mu1*mu2),nu=x)
  )
}

