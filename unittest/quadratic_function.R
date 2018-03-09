quadratic_equation <- function(a, b, c)
{
  if (a == 0)
    stop("Leading term cannot be zero")
  # Calculate determinant
  d <- b * b - 4 * a * c
  
  # Calculate real roots
  if (d < 0)
    rr <- c()
  else if (d == 0)
    rr <- c(-b / (2 * a))
  else
    rr <- c((-b - sqrt(d)) / (2 * a), 
            (-b + sqrt(d)) / (2 * a))
  
  return(rr)
}


