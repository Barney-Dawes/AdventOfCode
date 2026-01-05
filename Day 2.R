#code to solve the second day's puzzles

#clearing the environment
rm(list = ls())

library(stringr)
library(dplyr)
library(microbenchmark)
library(numbers)


#### Data Input ####


#reading in the txt file and transposing it
input <- read.table("input - day 2.txt", sep = ",")
transpose <- t(input)

#splitting the two numbers into two columns
split_data <- transpose %>% 
  str_split(, pattern = "-") %>% #creates two elements, one for the first and last number
  data.frame() %>% #turns the list into a dataframe
  t() %>% 
  apply(MARGIN = 2, as.numeric) %>% #converts the values from being characters to numerics
  as.data.frame()
  
#renaming the rows and columns
rownames(split_data) <- 1:nrow(split_data)
colnames(split_data) <- c("First", "Last")


#### Part 1 ####


#function to check that a number is an invalid product ID
pattern_check <- function(x){
  
  #creating the vector where we'll store our answers at the start, rather than building it incrementally, for speed
  output <- rep(FALSE, times = length(x))
  
  for (i in 1:length(x)){
    nchars <- floor(log10(x[i])) + 1
    
    #if we have an odd number of characters then we can't have a repeated set of numbers, and a number such as 555 while having repeated digits, isn't an invalid ID
    if (nchars %% 2 == 0){
      
      #finding the digits
      first_half <- str_sub(x[i], start = 1, end =  nchars / 2) %>% as.numeric()
      second_half <- str_sub(x[i], start = (nchars / 2) + 1, end =  nchars) %>% as.numeric()
      
      if (first_half == second_half){
        output[i] <- TRUE
      }
    }
    
  }
  
  #returns TRUE for invalid product IDs
  return(output)
}

#creating a list to store invalid IDs in
invalid_IDs <- vector("list", length = nrow(split_data))


for (i in 1:nrow(split_data)){
  
  #checking how far through the ranges of product IDs we are
  print(paste0("Checking row ", i, " of ", nrow(split_data), " rows"))
  
  #creating the range which will be the functions input
  range <- split_data$First[i]:split_data$Last[i]
  
  #finding the invalid IDs for that range
  invalid_IDs_temp <- range[pattern_check(range)]
  
  #storing results in the list
  invalid_IDs[[i]] <- invalid_IDs_temp
}

answer_1 <- invalid_IDs %>% 
  unlist() %>% 
  sum()


#### Part 2 ####


# #function to compare multiple numbers together to see if they're the same
# num_comp <- function(vector){
#   
#   #predefine the length of the output vector for speed
#   output <- vector(length = length(vector))
#   
#   #for each element of the vector we check whether it's the same value as the first
#   for (i in 1:length(vector)){
#     if (vector[1] == vector[i]){
#       output[i] <- TRUE
#     
#     #if even one of the pairs don't match then we know that the whole vector isn't a series of repeated sections
#     } else {
#       break
#     }
#   }
#   
#   #if all the elements have the same value as the first, then all elements are equal
#   return(all(output))
# }




#creating a new version of our pattern checking function to now detect when there's any type of repeated pattern, i.e. 555 would now be considered an invalid ID
pattern_check2 <- function(x){
  
  # input type
  #x <- 10:12
  
  #creating the vector where we'll store our answers at the start, rather than building it incrementally, for speed
  output <- rep(FALSE, times = length(x))
  
  for (i in 1:length(x)){

    # #finding the prime factorisation of nchars
    # primes_of_nchars <- primeFactors(nchars) %>% unique()
    
    #extracting the digits of the number
    digits <- strsplit(as.character(x[i]), "") %>% unlist %>% as.numeric()
    
    nchars <- length(digits)
    
    #first check if it's a single digit repeated or an integer between 1-9 inclusive
    if (length(unique(digits)) == 1 && nchars > 1){
      output[i] <- TRUE
      #return(TRUE) or output <- TRUE
      
    } else {
      
      #work out how many possible sections there could be, i.e. with 6 digits they could be sectioned in twos or threes
      divisors <- divisors(nchars)
      divisors <- divisors[-c(1,length(divisors))] #removing 1 and 'nchars'
      
      #looping over each divisor
      for (divs in divisors){
        
        #keeping track of the successes and failures we have
        tracker <- rep(FALSE, times = divs - 1)
        
        #looping over the sections of the digits for a given divisor
        for (sections in 2:divs){
          
          #check to see that the selected sections digits match the first sections - working on the counter factual for speed as we expect most numbers to be valid
          if (any(digits[1:(nchars/divs)] != digits[((nchars * (sections - 1)/divs) + 1) : (nchars * sections/divs)])) {
            break
          } else {
            tracker[sections - 1] <- TRUE
          }
          
        }
        
        #checking that for our current divisor, all sections are equal, if so we record this and skip any remaining divisors as they're unnecessary to check
        if (all(tracker)) {
          output[i] <- TRUE
          break
        } 
      }
      
    }
    
  }
  
  #returns TRUE for invalid product IDs
  return(output)
}


#creating a list to store invalid IDs in
invalid_IDs <- vector("list", length = nrow(split_data))


for (i in 1:nrow(split_data)){
  
  #checking how far through the ranges of product IDs we are
  print(paste0("Checking row ", i, " of ", nrow(split_data), " rows"))
  
  #creating the range which will be the functions input
  range <- split_data$First[i]:split_data$Last[i]
  
  #finding the invalid IDs for that range
  invalid_IDs_temp <- range[pattern_check2(range)]
  
  #storing results in the list
  invalid_IDs[[i]] <- invalid_IDs_temp
}

answer_2 <- invalid_IDs %>% 
  unlist() %>% 
  sum()

