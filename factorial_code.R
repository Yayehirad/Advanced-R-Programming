# Advanced R Programming
# Student Name : Yayehirad Melsew

#Part 1: Factorial Function
library(purrr)
#  write a function that computes the factorial of an integer greater than or equal to 0. 

# 1. Factorial_loop: a version that computes the factorial of an integer using looping (such as a for loop)
test_number <-10

Factorial_loop <- function(n) {
  stopifnot(n >= 0)
  if (n == 0) {
    return(1)
  } else {
    factorial <- 1
    for (i in 1:n) {
      factorial <- factorial * i
    }
    return(factorial)
  }
}
#test
Factorial_loop(test_number)

#2. Factorial_reduce: a version that computes the factorial using the reduce() function in the purrr package. 
#Alternatively, you can use the Reduce() function in the base package.

Factorial_reduce <- function(n) {
  stopifnot(n >= 0)
  if (n == 0) {
    return(1)
  } else {
    factorial<- reduce(as.numeric(1:n),`*`)
    }
    return(factorial)
}
#test
Factorial_reduce(test_number)


# 3. Factorial_func: a version that uses recursion to compute the factorial.

Factorial_func <- function(n) {
  stopifnot(n >= 0)
  if(n == 0) {return(1)}
  else {factorial <- n * Factorial_func(n-1)
  return(factorial)
  }
}
Factorial_func(test_number)



#  4Factorial_mem: a version that uses memoization to compute the factorial.
Factorial_mem <-  function(n) {
  stopifnot(n >= 0)
  if(n == 0) {return(1)}
  else {
    factorial_tbl[n] <<- n * Factorial_mem(n-1)
  return(factorial_tbl[n])
  }
}
factorial_tbl <- c(rep(NA, test_number))
Factorial_mem(test_number)




# summary of their performance
library(microbenchmark)
benchmark <- function (test_number) {
  microbenchmark(
    loop <- Factorial_loop(test_number),
    
    reduce <- Factorial_reduce(test_number),
    
    func <- Factorial_func(test_number),
    
    mem <- Factorial_mem(test_number)
  )
}

#Comparison for test_number =5
test_number=5
fact_tabel <- c(rep(NA, test_number))
benchmark(test_number)
#Comparison for test_number =100
test_number=100
fact_tabel <- c(rep(NA, test_number))
benchmark(test_number)
#Comparison for test_number =1000
test_number=1000
fact_tabel <- c(rep(NA, test_number))
benchmark(test_number)

