#1

factorial = function(n){
    if (n == 0 || n == 1){
      return (1)
    }
    else {
      return( n * factorial(n - 1))
    }
  }

data = c(5, 7, 10)

for(i in 1:length(data)){
  print(factorial(data[i]))
}

#2

is_even = function(x){
  if (x %% 2 == 0){
    return ("is even!")
  }
  else {
    return ("is odd!")
  }
}


data = c(sample(1:100, 100))

for( i in 1:length(data)){
  print(paste(data[i], is_even(data[i])))
}

#3

vocals = c("a", "e", "i", "o", "u")

letter_to_evaluate = function(letter){
  if (letter %in% vocals){
    return ("V")
  }
  else {
    return ("C")
  }
}

my_letters = matrix(sample(letters, 9, replace = FALSE), nrow = 3, ncol = 3)

evaluated_letter = matrix(apply(my_letters, c(1, 2), letter_to_evaluate), ncol = 3)

#4

data = c("juan", "pablo", "silvestre", "gonzalez")


evaluate_name = function(name){
  
  ev_name = c()
  name_letters = unlist(strsplit(name, split = ""))
  for (i in 1:length(name_letters)){
  
    ev_name = c(ev_name, letter_to_evaluate(name_letters[i]))
  }
  
  return (paste(ev_name, collapse = "-"))
}

for (i in 1:length(data)){
  print(evaluate_name(data[i]))
}

#5

mult_table  = function(n){
  
  for (i in 1:10){
    print(paste(i, "x", number, "=", i * number))
  }
}


number = readline("enter any number!")
number = as.integer(number)
mult_table(number)



#6



calculate_e = function(err){
  
  e_old = 1
 
  for (i in 1:100){
    
    term = 1 / factorial(i)
    e_new = e_old + term
    
    if (abs(e_old - e_new) < err){
      break
    } 
    else{
      e_old = e_new
    }
    
    print(paste(i, ":", e_new))
  }
}

err = 10^(-7)
calculate_e(err)

