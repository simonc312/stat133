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
        m[row_index,col_index] = next_space
      }
    }
  }
  #handle corner case last column 
  for(row_index in 1:num.rows){
    cur_space = grid.old[row_index,num.cols]
    next_space = grid.old[row_index,1]
    if(isRed(cur_space) & isEmpty(next_space)){
      m[row_index,1] = cur_space
      m[row_index,col_index] = next_space
    }
  }
  return(m)
}

moveBlueCars <- function(num.rows,num.cols,grid.old,m){
  for(col_index in 1:num.cols){
    for(row_index in rev(2:num.rows)){
      cur_space = grid.old[row_index,col_index]
      next_space = grid.old[row_index-1,col_index]
      if(isBlue(cur_space) & isEmpty(next_space)){
        m[row_index-1,col_index] = cur_space
        m[row_index,col_index] = next_space
      }
    }
  }
  #handle corner case last column 
  for(col_index in 1:num.cols){
    cur_space = grid.old[1,num.cols]
    next_space = grid.old[num.rows,col_index]
    if(isBlue(cur_space) & isEmpty(next_space)){
      m[num.rows,col_index] = cur_space
      m[row_index,col_index] = next_space
    }
  }
  return(m)
}

bml.step <- function(m){
  grid.new = TRUE
  grid.old = m
  num.rows = dim(m)[1]
  num.cols = dim(m)[2]
  getImage(m)
  m <- moveRedCars(num.rows,num.cols,grid.old,m)
  getImage(m)
  grid.old2 <- m
  m <- moveBlueCars(num.rows,num.cols,grid.old2,m)
  getImage(m)
  if(identical(m,grid.old)){grid.new=FALSE}
  return(list(m, grid.new))
}

#### Function to do a simulation for a given set of input parameters
## Input : size of grid [r and c] and density [p]
## Output : *up to you* (e.g. number of steps taken, did you hit gridlock, ...)

bml.sim <- function(r, c, p){
  grid <- bml.init(r,c,p)
  initial.grid <- grid
  end_iteration <- 0
  is.grid_lock <- FALSE
  for(iteration in 1:(5)){
    step_output <- bml.step(grid)
    if(step_output[[2]] == FALSE){
      getImage(grid);
      return(list(iteration,TRUE))}
    else{ end_iteration=end_iteration+1}
    grid <- step_output[[1]]
    getImage(grid);
  }
  getImage(grid);
  return(list(end_iteration,is.grid_lock))
}

#Helper functions with iterating many experiment samples

getImage <- function(m){
  image(t(apply(m,2,rev)),col=c("White","Red","Blue"))
}

# getDataFrame function outputs data frame that contains n samples 
#per density in density vector
getDataFrame <- function(density_vector,r,c,n){
  grid.data.frame <-  data.frame(matrix(vector(), 0, 3,
                                        dimnames=list(c(),c("Density","Iteration","GridLock"))),
                                 stringsAsFactors=F)
  for(density in density_vector){
    grid.iteration.numbers <- sapply(rep(density,n),function(p){bml.sim(r,c,p)})
    for(index in 1:(dim(grid.iteration.numbers)[2])){
      new.row <- data.frame(Density=density,
                            Iteration=grid.iteration.numbers[[1,index]],
                            GridLock=grid.iteration.numbers[[2,index]])
      grid.data.frame <- rbind(new.row,grid.data.frame)
    }
  }
  return(grid.data.frame)
}

#find average iteration number for each density for grid lock cases
#percentage resulted in grid locks
getAverages <- function(grid.data.frame,density_vector){
  average.matrix <- sapply(density_vector,function(p){
    p.grid <- grid.data.frame[grid.data.frame$Density == p,]
    lock.grid <- p.grid[p.grid$GridLock == TRUE,]
    percent.gridlock <- sum(lock.grid$GridLock)/length(p.grid$GridLock)
    if(length(lock.grid$Iteration) == 0){
      c(p,0,percent.gridlock) 
    }
    else{
      c(p,mean(lock.grid$Iteration),percent.gridlock)
    }
  })
  return(average.matrix)
}
