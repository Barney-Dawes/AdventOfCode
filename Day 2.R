#code to solve the second day's puzzles

#clearing the environment
rm(list = ls())

library(stringr)
library(dplyr)
library(microbenchmark)

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


#### 


#function to check that a number is an invalid product ID
pattern_check <- function(x){
  
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
  
  return(output)
  
}





#function to find how many 'silly patterns' there are in a given range
pattern_finder <- function(x, y){
  
  range <- x:y
  
  
  
  
  
  
  
  
}











