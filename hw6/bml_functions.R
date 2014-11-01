#################################################################################
#### Functions for BML Simulation Study


#### Initialization function.
## Input : size of grid [r and c] and density [p]
## Output : A matrix [m] with entries 0 (no cars) 1 (red cars) or 2 (blue cars)
## that stores the state of the system (i.e. location of red and blue cars)

bml.init <- function(r, c, p){
  half.num.cars  <- round(r*c*p/2)
  num.empty.spaces <- r*c - 2*half.num.cars
  random.vector.entries <- sample(rep(c(1,2,0),c(half.num.cars,half.num.cars,num.empty.spaces)))
  m <- matrix(random.vector.entries,ncol=c,nrow=r)
  return(m)  
   #return(m)
}

#### Function to move the system one step (east and north)
## Input : a matrix [m] of the same type as the output from bml.init()
## Output : TWO variables, the updated [m] and a logical variable
## [grid.new] which should be TRUE if the system changed, FALSE otherwise.

## NOTE : the function should move the red cars once and the blue cars once,
## you can write extra functions that do just a step north or just a step east.
isRed <- function(space){if(space == 1) return(TRUE) else return (FALSE)}
isBlue <- function(space){if(space == 2) return(TRUE) else return (FALSE)}
isEmpty <- function(space){if(space == 0) return(TRUE) else return (FALSE)}

moveRedCars <- function(num.rows,num.cols,grid.old,m){
  for(row_index in 1:num.rows){
    for(col_index in 1:(num.cols-1)){
      cur_space = grid.old[row_index,col_index]
      next_space = grid.old[row_index,col_index+1]
      if(isRed(cur_space) & isEmpty(next_space)){
        m[row_index,col_index+1] = cur_space
      }
    }
  }
  #handle corner case last column 
  for(row_index in 1:num.rows){
    cur_space = grid.old[row_index,num.cols]
    next_space = grid.old[row_index,1]
    if(isRed(cur_space) & isEmpty(next_space)){
      m[row_index,1] = cur_space
    }
  }
  return(m)
}

moveBlueCars <- function(num.rows,num.cols,grid.old,m){
  for(col_index in 1:num.cols){
    for(row_index in rev(1:(num.rows-1))){
      cur_space = grid.old[row_index,col_index]
      next_space = grid.old[row_index+1,col_index]
      if(isBlue(cur_space) & isEmpty(next_space)){
        m[row_index-1,col_index] = cur_space
      }
    }
  }
  #handle corner case last column 
  for(col_index in 1:num.cols){
    cur_space = grid.old[row_index,num.cols]
    next_space = grid.old[num.rows,col_index]
    if(isBlue(cur_space) & isEmpty(next_space)){
      m[num.rows,col_index] = cur_space
    }
  }
  return(m)
}

bml.step <- function(m){
  grid.new = TRUE
  grid.old = m
  num.rows = dim(m)[1]
  num.cols = dim(m)[2]
  # update m by move red cars
  m <- moveRedCars(num.rows,num.cols,grid.old,m)
  # update m by move blue cars
  m <- moveBlueCars(num.rows,num.cols,grid.old,m)
  if(identical(m,grid.old)){grid.new==FALSE}
  return(list(m, grid.new))
}

#### Function to do a simulation for a given set of input parameters
## Input : size of grid [r and c] and density [p]
## Output : *up to you* (e.g. number of steps taken, did you hit gridlock, ...)

bml.sim <- function(r, c, p){

}
