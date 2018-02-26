foo_02 <- function(x) {
  
  foo_02_intern <- function(x){
    print("blub")
  }
  
  foo_06<-function(x){
    print("blub")
    if (TRUE) {
      1
    } else {
      2
    }
    foo_07(x)
  }
  
  foo_07 <-function(x){
    print("blub")
  }
  
  '
  foo_08<- function(x){
    print("blub")
  }
  '
  foo_08b<- function(x){
    print("blub")
    foo_07(x)
  }
  '
  not used
  but stil here'
  
  foo_02_intern(x = 3)
  foo_06(x)
  foo_07(x)
  foo_08b(x)
  
}