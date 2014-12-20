# Write a function called powerDiff() that computes the derivative of 
# the form "ax^n", where n is an integer and a is any real number.
# For simplicity, it is okay to leave the output in the form "ax^0" or "ax^1"
# Input:
# <fn>: a character string of the form "ax^n" or "x^n"
# Output:
# <deriv>: a character string of the form "ax^n" or "x^n" or "0"
powerDiff <- function(fn){
  str <- unlist(strsplit(fn,"[x^]"))
  a <- as.numeric(str[1])
  n <- as.numeric(str[3])
  if(is.na(n) | n== 0 | a == 0)
    return(0)
  if(is.na(a) | a == 1)
    return(paste0(n,"x^",n-1))
  else if(n == 2){
    return(paste0(2*a,"x"))
  }
  else if(n == 1)
    return(paste0(a))
  else
    return(paste0(a*n,"x^",n-1))
}

# How to plot the standard Normal curve:
grid <- seq(-10, 10, by = 0.01)
plot(grid, dnorm(grid, mean = 0, sd = 1), type = "l")

# Write a function plotNormal() that plots multiple normal curves in one plot.
# See the given plot for reference (title, xlab, ylab, legend, etc.).
# Each curve should have a different color.
# Input:
# <m>: a numeric vector of means
# <v>: a numeric vector of variances
# Remark: m and v have the same length.
plotNormal <- function(m = 0, v = 1){
  cols = rainbow(length(v))
  legend_text = c(paste0("N(",m[1],",",v[1],")"))
  grid <- seq(-10, 10, by = 0.01)
  png(filename= "output.png",width=480, height=680)
  plot(grid,dnorm(grid,mean = m[1], sd = v[1]),col=cols[1],xlab="x",ylab="Density",main="Normal Curves",type = "l")
  for(i in 2:length(v)){
    lines(grid,dnorm(grid,mean = m[i],sd = v[i]),type="l",col=cols[i])
    legend_text[i] = paste0("N(",m[i],",",v[i],")")
  }
  legend("topleft",legend_text,lty=c(1),col=cols,cex=0.5)
  dev.off()
}

plotNormal(rep(0, 10), 1:10)

# Write a function called cap() that capitalize the 
# 2nd letter and the 4th letter for each word in a sentence.
# Be careful: the 2nd letter and the 4th letter of 
# "non-smoking" is "o" and "s", but not "o" and "-".
# Input:
# <sentence>: a character vector that contains only one string,
# where each word is separated by a space.
# Output:
# <sent>: a character vector that contains only one string,
# where each word is separated by a space. 
# The 2nd letter and the 4th letter for each word in the sentence
# is capitalized.
cap <- function(sentence){
  words = unlist(strsplit(sentence," "))
  letters_indices = gregexpr("[[:alpha:]]",words)
  for(i in 1:length(words)){
    index2 = letters_indices[[i]][2]
    index4 = letters_indices[[i]][4]
    if(!is.na(index2))
      substr(words[i],index2,index2) = toupper(substr(words[i],index2,index2))
    if(!is.na(index4))
      substr(words[i],index4,index4) = toupper(substr(words[i],index4,index4))
  }
  return(paste0(words,collapse=' '))
}


# Write a function called logAdj() that takes logarithm of
# each numeric/integer column and append the log column to the data frame.
# The name of each appended column should be 
# "log." + the name of the original column.
# Assume all the numeric/integer entries in df is positive.
# Input:
# <df>: a data frame that contains at least one numeric/integer column
logAdj <- function(df){
  # iterate subset of dataframe where class column is numeric
  # take log for each element in column vector wise and store that in separate data frame
  # bind the two dataframes together in the end
  n = names(df)
  for(name in n){
    col = df[,name]
    if(is.numeric(col)){
      df[,paste0("log.",name)] = log(col)
    }
  }
  return(df)
}
