#code to solve the first puzzle of the first day

#clearing the environment
rm(list = ls())

#choosing packages
library(stringr)
library(dplyr)

#timing how long the code takes to run
start <- Sys.time()

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
  no_hundreds <- floor(output[step:(step + 1), "Dial"] / 100)
  
  #giving logical conditions shorter names to shorten next section
  a <- output$Dial[step] %% 100 == 0
  b <- output$Dial[step + 1] %% 100 == 0
  c <- output$Dial[step] < output$Dial[step + 1]
  
  #finding the number of times the rotation passed by zero, the complex conditions arise if the dial starts or ends on a multiple of 100
  if((a && b) | (!a && b && c) | (a && !b && !c)){
    output$passes_zero[step + 1] <- abs(no_hundreds[2] - no_hundreds[1]) - 1
    
  } else if ((!a && !b) | (!a && b && !c) | (a && !b && c)){
    output$passes_zero[step + 1] <- abs(no_hundreds[2] - no_hundreds[1])
      
    #if the variable 'logic_check' gets defined then we know that the two conditions above don't span every possibility
  } else {
    logic_check <- TRUE
  }
  
}

if(exists("logic_check")){
  print("Logical conditions don't cover all possible cases")
}


#### Finding the answers to part 1 and 2 ####


#the answer to part 1 is the number of times the dial was set to zero
answer_part1 <- length(output$Dial[output$Dial %% 100 == 0])

#the answer to part 2 is the number of times it was set to zero plus the times it passed zero without stopping on it
answer_part2 <- answer_part1 + sum(output$passes_zero)

#measuring how long the code took to run
time_to_run <- difftime(Sys.time(), start) %>% round(digits = 3)

print(paste("code runtime was: ", time_to_run, units(time_to_run)))


