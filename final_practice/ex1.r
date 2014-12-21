load("family.rda")

# After loading the dataset above, you will have the following
# objects:
#
# > ls()
# [1] "fage"    "fgender" "fheight" "fnames"  "fweight"

# Create a numeric vector <fbmi> giving indicating a person's BMI. BMI should be
# calculated as their weight divided by their squared heights multiplied by 703

fweight_and_heights <- data.frame(fheight = fheight, fweight = fweight)

fbmi <- apply(fweight_and_heights,1,function(row){(row[2]/(row[1]^2))*703})

# Create a logical vector <foverWt>. A person should be considered overweight
# if their BMI is greater than 25
foverWt <- sapply(fbmi,function(p){p>25})


# Create a dataframe <family>.  The dataframe should contain fnames, fgender,
# fage, fheight, fweight, fbmi, and foverWt.  The names should be as follows:
# > names(family)
# [1] "name" "gender" "age" "height" "weight" "bmi" "overWt"
family <- data.frame(name=fnames,gender=fgender,age=fage,height=fheight,weight=fweight,bmi=fbmi,overWt=foverWt)

# Create a subset of the family dataframe called <family.subset>. This should
# contain all observations who are neither overweight (i.e. bmi > 25) nor older
# than the average (mean) age of all observations.
avg_age <- mean(fage)
family.subset <- family[which(family$overWt == FALSE & family$age < avg_age),]

# For each individual in the family dataframe, plot their age (x-axis) against
# their bmi (y-axis).  Males should be represented with 'red' circles and
# females with 'blue' circles.  Set pch to 'o'. The x-axis should range from 23
# to 80 and your y-axis from 16 to 31. Finally put a legend in the 'topright'
# corner of the plot with a 'red' circle in front the label 'male' and a 'blue'
# circle in front the label 'female'. 
cols <- c('blue','red')
cols <- cols[family$gender]
plot(family$age,family$bmi,pch='o',xlab="Age",ylab="BMI",col=cols)
legend('topright',c("Male","Female"),pch='o',col=c('red','blue'))
