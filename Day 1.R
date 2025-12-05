#code to solve the first puzzle of the first day

#clearing the environment
rm(list = ls())

#choosing packages
library(stringr)
library(dplyr)

#reading in the txt file
input <- read.table("input - day 1.txt", sep = " ")

#changing the data from using Right (R) and Left (L) to using positive and negative numbers
input_num <- str_replace_all(input$V1, pattern = "R", replacement = "")
input_num <- str_replace_all(input_num, pattern = "L", replacement = "-") %>% as.numeric()

#defining a matrix which will record our steps, the steps column shows what we need to add, and the dial 
#column shows the value after adding the step
output <- matrix(0, nrow = length(input_num) + 1, ncol = 2) %>% as.data.frame()
colnames(output) <- c("Steps", "Dial")

#adding in our initial data to the output matrix
output[1, ] <- 50
output[2:(length(input_num) + 1), 1] <- input_num

#adding all the steps up
for (step in 1:length(input_num)){
  output[step + 1, 2] <- output[step, 2] + output[step + 1, 1]
}


#### Second part ####


#adding a column to our output which will record if the dial is rotated past the 0 marker
output$passes_zero <- 0

#looping over each time we changed the dial's position and checking if the dial went past zero
for (step in 1:length(input_num)){
  #finding the number of 'hundreds' for the current row in the dial column and the row above, i.e. 123 has one 'hundred' and -498 has four 'hundreds' as we ignore sign
  no_hundreds <- trunc(output[step:(step + 1), "Dial"] / 100)
  
  #finding the number of times the rotation passed by zero
  output$passes_zero[step + 1] <- no_hundreds[2] - no_hundreds[1]
  
}



#finding how many of the dial's values are multiples of 100 or where the dial goes past zero
output_mod <- output[(output[, "Dial"] %% 100) == 0 | output[, "passes_zero"], ]

#the answer is the number of times the dial was set to zero
answer_part1 <- length(output_mod)



#if the number of hundreds isn't the same, i.e. there's been a change, then we have gone past zero. 
#Also have to consider cases where we start at zero and then rotate, as this wouldn't count as going past zero but would involve a change in the hundreds 
if((no_hundreds[1] != no_hundreds[2]) & 
   (no_hundreds[1] %% 100 != 0))
  output[step + 1, "passes_zero"] <- TRUE