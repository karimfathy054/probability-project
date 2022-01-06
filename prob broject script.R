#1- Loading Data
data('mtcars')
print(mtcars)
print(str(mtcars))
write.csv(mtcars,'mtcars.csv')
#2- Extracting Information
library(dplyr)
#Display the head of each type of transmission -manual and automatic- separately.
manualcars = filter(mtcars , am==0)
print(head(manualcars))
automaticCars = filter(mtcars,am ==1)
print(head(automaticCars))
#Display the top 10 cars according to: Displacement , hp , drat
#method 1 
topDisp=mtcars[order(-mtcars['disp']),]
print(head(topDisp,10))
topHP=mtcars[order(-mtcars['hp']),]
print(head(topHP,10))
topDRAT=mtcars[order(-mtcars['drat']),]
print(head(topDRAT,10))
#method 2 
topDisp2 = filter(mtcars,disp>=304)
print(topDisp2)
topHP2 = filter(mtcars,hp>=180)
print(topHP2)
topDRAT2 = filter(mtcars,drat>=3.92)
print(topDRAT2)
#Display cars whose mpg is above average only.
mpgaverage =mean(mtcars$mpg)
CarsAbove_mpgaverage = filter(mtcars,mpg>mpgaverage)
print(CarsAbove_mpgaverage)
#chart for mpg
mile_per_gallon=mtcars[,'mpg']
print(x)
number_of_cars=dnorm(x,mean(x),1)
plot(mile_per_gallon,number_of_cars)
#chart for cyl
percentage_of_4_cylinder_cars= nrow(filter(mtcars,cyl==4))/ nrow(mtcars)*100
percentage_of_6_cylinder_cars= nrow(filter(mtcars,cyl==6))/ nrow(mtcars)*100
percentage_of_8_cylinder_cars= nrow(filter(mtcars,cyl==8))/ nrow(mtcars)*100
percentages =c(percentage_of_4_cylinder_cars,percentage_of_6_cylinder_cars,percentage_of_8_cylinder_cars)
labels = c("4 cylender cars",'6 cylinder cars','8 cylinder cars')
pie(x=percentages,labels=labels,main='Car Cylinders',col=rainbow(length(percentages)))



#Plot the boxplots for the following features: disp, hp and qsec. Extract the 3 main percentiles. What can you deduce?
boxplot(mtcars[c('disp','hp','qsec')])
#3- Distributions
#A. Assume that the weight fits a normal distribution. Find the percentage of cars having 3.4 lbs or more.
percentage = (1-pnorm(3.4,mean(mtcars$wt)))*100
print(percentage)
#B. What is the probability of getting 18 or less manual cars using these 32 observations? Assume that the probability of getting a manual car in an infinite series of cars is equal to the probability of getting a manual car from this dataset.
pbinom(18,32,0.5)
#C. Suppose there are twelve spots in a car parking area. Each spot is suitable for five possible car types, and only one of them fits perfectly. Find the probability of having four or less spots filled with the corresponding car type if the garagist attempts to park in each spot at random.
pbinom(4,12,1/5) #maybe


#4- Permutations and Combinations
#A. Given that we have a number in the ternary numeral system, this number has 3 digits. Use R to find all the permutations for such number. Solve using 2 different methods.
#method 1
library('gtools')
digit_values=c(0,1,2)
x= permutations(n=3,r=3,v=digit_values,repeats.allowed =TRUE)
nrow(x)
#method 2
perm_with_replacement = function(n,r){
  result=n^r
  print(result)
}
perm_with_replacement(n=3,r=3)
#B. Given a set of numbers A = {1, 2, …, 9}, assume you are picking 3 numbers without replacement. Find the probability that you get 3 numbers where the minimum number is 2 and the maximum is 5 using at least 2 ways. 
#method 1
probabilty = nrow(combinations(n=2,r=1))/nrow(combinations(n=9,r=3))
print(probabilty)
#method 2
comb_without_replacement = function(n,r){
  result=factorial(n)/factorial(n-r)/factorial(r)
  return(result)
}
probabilty2 = comb_without_replacement(n=2,r=1)/comb_without_replacement(n=9,r=3)
print(probabilty2)
