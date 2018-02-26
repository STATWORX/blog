foo_01 <- function(x) {
  
  foo_05 <- function(x){
    print("bla")
  }
  
  foo_09 <- function(x){ print("bla") }
  
  foo_10 <- function(x){
    print("JA")
    foo_11 <- function(x) {print("bla") }
    
    print("sg")
    
    foo_12<- function(x) { 
      print("bla") 
      foo_13(1)
    }
    
    foo_13 <- function(x) { 
      print("bla") 
    }
    
    foo_11(2)
    foo_12(2)
  }
  
  foo_05(x = 3)
  foo_05(x = 3)
  foo_05(x = 3)
  foo_10(x = 3)
  sapply(1:5, foo_05)
  
  foo_09(2)
  foo_09(2)
  foo_09(2)
  '
  comments'
  
  a <- 'adgf'
  
  foo_02(x = 4)
}