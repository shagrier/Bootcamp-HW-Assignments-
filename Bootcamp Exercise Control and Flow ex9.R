
#Bootcamp Exercise 1 #9 

f4 <- function (x)
  if (x <= -1){
    cat('\nsmall');
  } else if (x <= -1 | x <= 1){
    cat('\nmedium');
  } else if (x >= 1){
    print('nbig');
  }
f4(2)
f4(3)
f4(1)
f4(-1)
f4(-2)
