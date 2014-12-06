load('all.rda')

# Classification Tree with rpart
library(rpart)
library(maps)

# grow tree 
#setwd("C:/Users/Simon Chen/Desktop/textbook pdfs to read/stat133/stat133/hw8")
winner <- factor(all$winner)
fit <- rpart(winner ~ bushVote + winParty,
             method="class", data=all, control=rpart.control(minsplit=30, cp=0.001))

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits

# plot tree 
plot(fit, uniform=TRUE, 
     main="Winner ~ bushVote + winParty 2012 Election")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

fit2 <- rpart(winner ~ bushVote + winParty + whitePop + county,
             method="class", data=all, control=rpart.control(minsplit=30, cp=0.001))
plotcp(fit2)
# plot tree 
plot(fit2, uniform=TRUE, 
     main="Winner ~ whitePop + county") #classification tree pruned out bushVote and winParty
text(fit2, use.n=TRUE, all=TRUE, cex=.8)

fit3 <- rpart(winner ~ kerryVote + blackPop + winParty,
              method="class", data=all, control=rpart.control(minsplit=30, cp=0.001))
plotcp(fit3)
# plot tree 
plot(fit3, uniform=TRUE, 
     main="Winner ~ kerryVote + blackPop + winParty")
text(fit3, use.n=TRUE, all=TRUE, cex=.8)

fit4 <- rpart(winner ~ state + bushVote + kerryVote + winParty,
              method="class", data=all, control=rpart.control(minsplit=30, cp=0.001))
plotcp(fit4)
# plot tree 
plot(fit4, uniform=TRUE, 
     main="Winner ~ state + bushVote + kerryVote + winParty")
text(fit4, use.n=TRUE, all=TRUE, cex=.3)

#FINAL PART
# VOTE SHIFT 
 
non_na_indices <- !is.na(all$winner) & !is.na(all$bushVote) 
bushVote_without_na <- all$bushVote[non_na_indices]
kerryVote_without_na <- all$kerryVote[non_na_indices]
repVotes_without_na <- all$repVotes[non_na_indices]
demVotes_without_na <- all$demVotes[non_na_indices]
winner_without_na <- all$winner[non_na_indices]
winParty_without_na <- all$winParty[non_na_indices]
get_arrow_offsets <- function(){
    offsets <- c()
    delta_rep <-  demVotes_without_na - kerryVote_without_na
    delta_dem <-  repVotes_without_na - bushVote_without_na
    offsets <- sapply((delta_rep - delta_dem)*0.00025,function(d){max(min(d,1),-1)})
  #}
  return(offsets)
}

get_arrow_offsets2 <- function(){
  offsets <- c()
  for(i in 1:length(winner_without_na)){
  if(winParty_without_na[i]) #True if Dems won
    offsets[i] <- min(1,max(-1,(kerryVote_without_na[i] - demVotes_without_na[i])*0.001))
  else
   offsets[i] <- max(-1,min(1,(bushVote_without_na[i] - repVotes_without_na[i])*0.001))
  }
  return(offsets)
}
offsets <- get_arrow_offsets()

lat1 <- all$lat[non_na_indices]
lon1 <- all$lon[non_na_indices]
lat2 <- lat1 + abs(offsets) 
lon2 <- lon1 + offsets
cols <- c('red','blue')
party <- sapply(offsets,function(o){if(o >= 0) return(1)
                                    else return(2)})
cols <- cols[party]
state_names <- map("state", fill = TRUE, col = "white", namesonly = TRUE, plot = FALSE)
map('usa', interior = FALSE, fill = TRUE, col = '#d3d3d3', lty = 0)
map('state', region = state_names, interior = TRUE, fill= FALSE, col = 'white', add = TRUE)
addLines <- function(){
  for(i in 1:length(lat1)){
    lines(c(lon1[i],lon2[i]),c(lat1[i],lat2[i]),col= cols[i])
  }
}
addLines()
arrows(lon2-0.1, lat2-0.1, x1=lon2,y1=lat2, length = 0.04, angle = 30,col= cols)


map('usa', interior = FALSE, fill = TRUE, col = '#d3d3d3', lty = 0)
map('state', region = state_names, interior = TRUE, fill= FALSE, col = 'white', add = TRUE)
offsets <- get_arrow_offsets2()
lat2 <- lat1 + abs(offsets) 
lon2 <- lon1 + offsets
cols <- c('red','blue')
cols <- cols[as.numeric(winner_without_na)+1]
addLines()
arrows(lon2-0.1, lat2-0.1, x1=lon2,y1=lat2, length = 0.04, angle = 30,col= cols)
