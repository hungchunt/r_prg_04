#遞增/遞減排序函數
#Section 1: library packages

#Section 2: Self-defined functions
exchange.sort <- function(input_vector, decreasing = FALSE) {
  if (decreasing == FALSE){
  for (i in 1:(length(input_vector) - 1)) {
    for (j in (i + 1):length(input_vector)) {
       if (input_vector[i] > input_vector[j]) {
        temp <- input_vector[i]
        input_vector[i] <- input_vector[j]
        input_vector[j] <- temp
      }
    }
  }
  return(input_vector)
}else{
  for (i in 1:(length(input_vector) - 1)) {
    for (j in (i+1):length(input_vector)) {
      if (input_vector[i] < input_vector[j]) {
        temp <- input_vector[i]
        input_vector[i] <- input_vector[j]
        input_vector[j] <- temp
      }
    }
  }
  return(input_vector)
}
}
#Section 3: Inputs and parameters
set.seed(1)
input_vector <- ceiling(runif(10) * 100)
input_vector
#Section 4: Function calls
exchange.sort(input_vector)
exchange.sort(input_vector, decreasing = TRUE)

#計算樣本標準差函數
#Section 1: library packages

#Section 2: Self-defined functions
standard.deviation <- function(input) {
  count <- 0
  summation <- 0
  for (i in input) {
    summation <- summation + i
    count <- count + 1
  }
  mean <- (summation/count)
  X <- 0
  for(i in input) {
    X <- (X+(i-mean)^2)
  }
  return((X/(count-1))^0.5)
}
#Section 3: Inputs and parameters
input_vector <- ceiling(runif(10) * 100)
#Section 4: Function calls
standard.deviation(input_vector)
sd(input_vector)