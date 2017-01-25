##自創函數一:my.sort，預設值decreasing為FALSE

#定義新函數
my.sort <- function(input_vec,decreasing=FALSE){
  for(i in 1:(length(input_vec)-1)){
    for(j in (i+1):length(input_vec)){
      if(input_vec[i]>input_vec[j]){
        temp_i <- input_vec[i]
        input_vec[i]<-input_vec[j]
        input_vec[j]<-temp_i
      } 
    }
  }
  if(decreasing==FALSE){
    return(input_vec)
  }else{
    for(i in 1:(length(input_vec)-1)){
      for(j in (i+1):length(input_vec)){
        if(input_vec[i]<input_vec[j]){
          temp_i <- input_vec[i]
          input_vec[i]<-input_vec[j]
          input_vec[j]<-temp_i
        } 
      }
    }
    
  }
  return(input_vec)
}
#建立input
set.seed(1) 
my_seq <- round(runif(10)*100)
#呼叫函數
my.sort(my_seq,decreasing=FALSE)


##自創函數二:標準差my.sd

#定義新函數
my.sd <- function(input_vec){
  x_bar <- mean(input_vec)
  n_minus_1 <- length(input_vec)-1
  summation <- 0
  for(x_i in summation){
    summation <- summation+ (x_i-x_bar)^2
  }
  n_of_numbers <- summation/n_minus_1
  to_sqrt <- sqrt(n_of_numbers)
  return(to_sqrt)
}
#建立input
set.seed(2) 
my_vec <- runif(10)
#呼叫函數
my.sd(my_vec)

##自創函數二:BMI計算機

#定義新函數
bmi_calculater <- function(w,h){
  h <- h/100
  return(w / h^2)
}
#建立 input
heights <- c(173, 168, 171, 189, 179)
weights <- c(65.4, 59.2, 63.6, 88.4, 68.7)
heights_and_weights <- data.frame(heights, weights)
#呼叫函數
bmi <- mapply(FUN=bmi_calculater,w=weights,h=heights)
#導入新變數
BMI <- cbind(heights_and_weights,bmi)
View(BMI)
