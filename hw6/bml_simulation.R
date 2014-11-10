#################################################################################
#### BML Simulation Study

#### Put in this file the code to run the BML simulation study for a set of input parameters.
#### Save some of the output data into an R object and use save() to save it to disk for reference
#### when you write up your results.
#### The output can e.g. be how many steps the system took until it hit gridlock or
#### how many steps you observered before concluding that it is in a free flowing state.
source('bml_functions.R')

#small 10x10 grids density varying from 0.1 to 1 by step size 0.1
small.density.vector <- (1:10)/10
small.grid.data.frame <- getDataFrame(small.density.vector,10,10,100)
averages.small.grid <- getAverages(small.grid.data.frame,small.density.vector)
#for small grid sizes there isn't much leeway for having partial gridlocks 
# it seems more likely to occur in larger grids 

plot(averages.small.grid[1,],averages.small.grid[2,], type="l",
     main="Density vs Avg Gridlock Step Count in 10x10 Grid",
     xlab="Density", 
     ylab="Avg Gridlock Step Count")

plot(averages.small.grid[1,],averages.small.grid[3,]*100, type="l",
     main="Density vs %Grid Lock of 100 Iterations in 10x10 Grid",
     xlab="Density", 
     ylab="% Grid Lock")

mid.density.vector <- (20:40)/100
mid.grid.data.frame <- getDataFrame(mid.density.vector,50,50,10)
averages.mid.grid <- getAverages(mid.grid.data.frame,mid.density.vector)

mid.density.vector2 <- (20:40)/50
mid.grid.data.frame2 <- getDataFrame(mid.density.vector2,50,50,10)
averages.mid.grid2 <- getAverages(mid.grid.data.frame2,mid.density.vector2)

plot(averages.mid.grid2[1,],averages.mid.grid2[2,], type="l",
     main="Density vs Avg Gridlock Step Count in 50x50 Grid",
     xlab="Density", 
     ylab="Avg Gridlock Step Count")

plot(averages.mid.grid2[1,],averages.mid.grid2[3,]*100, type="l",
     main="Density vs %Grid Lock of 10 Iterations in 50x50 Grid",
     xlab="Density", 
     ylab="% Grid Lock")

mid.density.vector3 <- (20:40)/50
mid.grid.data.frame3 <- getDataFrame(mid.density.vector3,25,100,10)
averages.mid.grid3 <- getAverages(mid.grid.data.frame3,mid.density.vector3)

mid.density.vector4 <- (20:40)/50
mid.grid.data.frame4 <- getDataFrame(mid.density.vector3,10,250,10)
averages.mid.grid4 <- getAverages(mid.grid.data.frame3,mid.density.vector3)

large.density.vector <- (2:9)/10
large.grid.data.frame <- getDataFrame(large.density.vector,100,100,10)
averages.large.grid <- getAverages(large.grid.data.frame,large.density.vector)

plot(averages.large.grid[1,],averages.large.grid[2,], type="l",
     main="Density vs Avg Gridlock Step Count in 100x100 Grid",
     xlab="Density", 
     ylab="Avg Gridlock Step Count")

plot(averages.large.grid[1,],averages.large.grid[3,]*100, type="l",
     main="Density vs %Grid Lock of 10 Iterations in 100x100 Grid",
     xlab="Density", 
     ylab="% Grid Lock")
#There has also been research in rectangular lattices instead of square ones.
#For rectangles with coprime dimensions, the intermediate states are
#self-organized bands of jams and free-flow with detailed geometric structure,
#that repeat periodically in time.[3] In non-coprime rectangles, 
#the intermediate states are typically disordered rather than periodic.
#From wikipedia 