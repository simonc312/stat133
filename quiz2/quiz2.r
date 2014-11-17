# Quiz 2
# Simulation of Johnny's Office Hours

# The number of students coming to Johnny's OH can be modeled 
# by a Poisson random variable with mean k.

# Write a function called num_students:
# Input: <k>: mean of a Poisson random variable (a single number)
# Output: <num>: a Poisson(k) random variable (a single number)

num_students <- function(k) {
  # your code here
  return(dpois(1,k))
}

# Assume all students arrive at different times.
# The first student arrives Y_1 minutes after OH begins. 
# For i = 2, ..., number of students, 
# student i arrives Y_i minutes after student i - 1 arrives.
# These Y_i are independently idenitically distributed
# Exponential random variables with rate 1/5.
# (This corresponds to the fact that on average,
# a new student comes to OH every five minutes.)

# Write a function called interarrival_times:
# Input: <num>: number of students (a number)
# Output: <inter>: a random sample of num Exponential(1/5) 
# random variables (a vector of length num)

interarrival_times <- function(num) {
  # your code here
  return(rexp(num,rate=1/5))
}

# For student i, it takes Z_i minutes for Johnny to answer questions.
# These Z_i are independently idenitically distributed
# Exponential random variables with rate 1/7.
# (This corresponds to the fact that on average,
# it takes Johnny seven minutes to fully answer a student's questions.)

# Write a function called service_times:
# Input: <num>: number of students (a number)
# Output: <serv>: a random sample of num Exponential(1/7) 
# random variables (a vector of length num)

service_times <- function(num) {
  # your code here
  return(rexp(num,rate=1/7))
}

# Compute the waiting time for each student.
# The waiting time for the first student is always 0.
# The waiting time for the ith student (i is at least 2) can be computed
# by the following:
#####################################
# waiting time for the ith student
# = service time for the (i-1)th student 
# + waiting time for the (i-1)th student
# - interarrival time for the ith student
#########################################
# If the waiting time computed using the formula above is 
# negative, set the waiting time equal to 0. 

# Write a function called waiting_times:
# Input: 
# <inter>: a vector of interrarrival times
# <serv>: a vector of service times
# Output: 
# <wait>: a vector that contains the waiting time for each student
waiting_times <- function(inter, serv){
  # your code here
  wait_times <- c(0)
  for(i in 2:length(inter)){
    time <- serv[i-1]+wait_times[i-1]-inter[i]
    if(time < 0)
      time <- 0
    wait_times <- append(time,wait_times)
  }
  return(wait_times)
}

# Simulation
# Write a function called queueing_sim:
# Input: 
# <k>: the mean number of students coming to Johnny's OH
# Output: 
# <sim>: a data frame with four columns:
#   inter: interarrival times
#   serv: service times
#   wait: waiting times
#   total: total times spent in Johnny's OH (serv + wait)

queueing_sim <- function(k) {
  # your code here
  inter_times <- interarrival_times(k)
  serv <- service_times(k)
  wait_times <- waiting_times(inter_times,serv)
  queue.frame <- data.frame(inter= inter_times,
                            serv= serv,
                            wait= wait_times,
                            total= serv+wait_times)
  return(queue.frame)
}

set.seed(1234)
# Run the simulation 500 times with k = 11. 
# Save the output in a variable called sim500.
# sim500 is a list of 500 data frames.
 sim500 <- lapply(1:500,function(i){queueing_sim(11)})



# For each simulation, compute the average waiting time and
# the average total time spent in OH.
# Save the result in a matrix called avg_wait_total.
# avg_wait_total is a 2x500 matrix (without any row names or column names).
avg_wait_total <- do.call(rbind,lapply(sim500,function(sim){c(mean(sim$wait),mean(sim$total))}))




#-----------------------------------------------------------------------
# Suppose Johnny is not feeling well and he needs to take a break of 
# around 3 minutes for every two students he serves.
# That is, he takes a break after serving the 2nd student, the 4th student, etc.
# He stops serving students until the break is over.
# Also, he needs to take a 17-minute nap right after the fifth student he serve.
# Again, he is not serving any students during his nap.
# In addition, he talks more slowly after the 17-minute nap: 
# The service times of 6th, 7th, ... students are multiplied by 1.5.

# The break times are independent and each can be modeled
# by a Normal random variable with mean 3 and variance 2,
# with the condition that whenever the random variable generated is 
# negative, set it equal to 0.
# DO NOT INCLUDE THE NAP TIME INTO BREAK TIMES.

# Write a function called break_times that generates
# n break times:
# Input: 
# <n>: the number of breaks Johnny is going to take (a single number)
# Output:
# <br_times>: a numeric vector of break times

break_times <- function(n){
  # your code here
  return(sapply(rnorm(n,3,2),function(r){if(r<0){0}else{r}}))
}

# Write a function called serv_wait_sick that computes
# the service time and the waiting time for each student, taking into account 
# that Johnny is taking a break from time to time as 
# described above and a 17-minute nap after serving the fifth student,
# and the service times for 6th, 7th, ... students are multiplied by 1.5:
# Input: 
# <inter>: a vector of interrarrival times
# <serv>: a vector of the usual service times
# <br_times>: a vector of break times
# Output: 
# <wait_serv>: a data frame with two columns,
#  <serv>: a vector of the (updated) service times
#  <wait>: a vector of the waiting times
# Remark: There is no randomness in this function.
waiting_times_sick <- function(inter, serv){
  # your code here
  wait_times <- c(0)
  break_time <- 0
  for(i in 2:length(inter)){
    serv_time <- serv[i-1]
    break_time <- 0
    if(i %% 2 == 0)
      break_time <- 3
    if(i == 5)
      break_time <- 17
    if(i > 5)
      serv_time <- 1.5*serv_time
    time <- serv_time+wait_times[i-1]-inter[i]+break_time
    if(time < 0)
      time <- 0
    wait_times <- append(time,wait_times)
  }
  return(wait_times)
}


serv_wait_sick <- function(inter, serv, br_times){
  # your code here
  wait_times <- waiting_times_sick(inter,serv)
  wait.frame <- data.frame(
                            serv= serv,
                            wait= wait_times
                            )
  return(wait.frame)
}

# End of quiz.

# Just a test case for break_times and a test case for waiting_times_sick. 
# No need to modify the following code.
# If your functions work properly, the following code should return TRUE.
set.seed(12)
all.equal(break_times(3), c(1.6989161, 0.1749075, 2.6149152), tolerance = 0.001)
all.equal(serv_wait_sick(rep(10, 10), rep(10, 10), c(0, 1, 3, 2, 1)), 
          data.frame(serv = c(10,10,10,10,10,15,15,15,15,15), 
                     wait = c(0,0,0,0,1,18,26,31,38,43)))
