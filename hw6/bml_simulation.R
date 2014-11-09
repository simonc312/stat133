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
small.grid.data.frame <- getDataFrame(small.density.vector,10,10,1)
averages.small.grid <- getAverages(small.grid.data.frame,small.density.vector)
#for small grid sizes there isn't much leeway for having partial gridlocks 
# it seems more likely to occur in larger grids 

mid.density.vector <- (20:40)/100
mid.grid.data.frame <- getDataFrame(mid.density.vector,50,50,1)
averages.mid.grid <- getAverages(mid.grid.data.frame,mid.density.vector)

large.density.vector <- (2:9)/10
large.grid.data.frame <- getDataFrame(large.density.vector,100,100,100)
averages.large.grid <- getAverages(large.grid.data.frame,large.density.vector)
#There has also been research in rectangular lattices instead of square ones.
#For rectangles with coprime dimensions, the intermediate states are
#self-organized bands of jams and free-flow with detailed geometric structure,
#that repeat periodically in time.[3] In non-coprime rectangles, 
#the intermediate states are typically disordered rather than periodic.
#From wikipedia 