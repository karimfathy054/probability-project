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
topDisp2 = mtcars %>%arrange(desc(disp)) %>% slice(1:10)
print(topDisp2)
topHP2 = mtcars %>%arrange(desc(hp)) %>% slice(1:10)
print(topHP2)
topDRAT2 = mtcars %>%arrange(desc(drat)) %>% slice(1:10)
print(topDRAT2)
#Display cars whose mpg is above average only.
mpgaverage =mean(mtcars$mpg)
CarsAbove_mpgaverage = filter(mtcars,mpg>mpgaverage)
print(CarsAbove_mpgaverage)
#chart for mpg
mile_per_gallon=mtcars[,'mpg']
percentage_of_cars=dnorm(mile_per_gallon,mean(mile_per_gallon),1)*100
plot(mile_per_gallon,percentage_of_cars)
#chart for cyl
percentage_of_4_cylinder_cars= nrow(filter(mtcars,cyl==4))/ nrow(mtcars)*100
percentage_of_6_cylinder_cars= nrow(filter(mtcars,cyl==6))/ nrow(mtcars)*100
percentage_of_8_cylinder_cars= nrow(filter(mtcars,cyl==8))/ nrow(mtcars)*100
percentages =c(percentage_of_4_cylinder_cars,percentage_of_6_cylinder_cars,percentage_of_8_cylinder_cars)
labels = c("4 cylender cars",'6 cylinder cars','8 cylinder cars')
pie(x=percentages,labels=labels,main='Car Cylinders',col=rainbow(length(percentages)))
#chart for disp

#chart for hp

#chart for drat

#chart for wt

#chart for qsec

#chart for vs

percent_of_motor_shape = table(mtcars$vs)/length(mtcars$vs)*100
pie(x=percent_of_motor_shape,labels = percent_of_motor_shape,main='Motor pistons positioning',col=rainbow(2))
legend("topleft",legend=c('V motor shape','Straightline motor shape'),fill,rainbow(2),cex=0.8)
#chart for am
percent_of_car_transmission_type = table(mtcars$am)/length(mtcars$am)*100

pie(x=percent_of_car_transmission_type,labels =percent_of_car_transmission_type ,main='Car transmission type',col=rainbow(2))
legend("topleft",legend = c('Automatic cars','Manual cars'),fill = rainbow(2),cex =0.8 )
#chart for gear
percent_of_car_transmission_gears = table(mtcars$gear)/length(mtcars$gear)*100
pie(x=percent_of_car_transmission_gears,labels =percent_of_car_transmission_gears ,main='Car transmission gears',col=rainbow(3))
legend("topleft",legend = c('3 transmission gears','4 transmission gears','5 transmission gears'),fill = rainbow(3),cex=0.8)
#chart for carb
percent_of_motor_carburators = table(mtcars$carb)/length(mtcars$carb)*100

percentage_of_cars_with_1_carburator= nrow(filter(mtcars,carb == 1))/ nrow(mtcars)*100
percentage_of_cars_with_2_carburator= nrow(filter(mtcars,carb == 2))/ nrow(mtcars)*100
percentage_of_cars_with_3_carburator= nrow(filter(mtcars,carb == 3))/ nrow(mtcars)*100
percentage_of_cars_with_4_carburator= nrow(filter(mtcars,carb == 4))/ nrow(mtcars)*100
percentage_of_cars_with_6_carburator= nrow(filter(mtcars,carb == 6))/ nrow(mtcars)*100
percentage_of_cars_with_8_carburator= nrow(filter(mtcars,carb == 8))/ nrow(mtcars)*100
pie(x=percent_of_motor_carburators,labels =percent_of_motor_carburators ,main='Car carburators',col=rainbow(6))
legend("topleft",legend = c('1 carburrator','2 carburrator','3 carburrator','4 carburrator','6 carburrator','8 carburrator'),fill=rainbow(6),cex=0.8 )
#Plot the boxplots for the following features: disp, hp and qsec. Extract the 3 main percentiles. What can you deduce?
boxplot(mtcars[c('disp','hp','qsec')])
#main percentiles:
#displacement:
quantile(mtcars$disp,c(0.25,0.50,0.75))
#Horse Power:
quantile(mtcars$hp,c(0.25,0.50,0.75))
#acceleration:
quantile(mtcars$qsec,c(0.25,0.50,0.75))
#3- Distributions
#A. Assume that the weight fits a normal distribution. Find the percentage of cars having 3.4 lbs or more.
percentage = (1-pnorm(3.4,mean(mtcars$wt)))*100
print(percentage)
#B. What is the probability of getting 18 or less manual cars using these 32 observations? Assume that the probability of getting a manual car in an infinite series of cars is equal to the probability of getting a manual car from this dataset.
pbinom(18,32,0.5)
#C. Suppose there are twelve spots in a car parking area. Each spot is suitable for five possible car types, and only one of them fits perfectly. Find the probability of having four or less spots filled with the corresponding car type if the garagist attempts to park in each spot at random.
pbinom(4,12,1/5) 


#4- Permutations and Combinations



#A. Given that we have a number in the ternary numeral system, this number has 3 digits. Use R to find all the permutations for such number. Solve using 2 different methods.
#method 1
library('gtools')
digit_values=c(0,1,2)
permutations(n=3,r=3,v=digit_values,repeats.allowed =TRUE)
number_of_permutations = nrow(permutations(n=3,r=3,v=digit_values,repeats.allowed =TRUE))
#method 2
for(i in 0:2){
  for(j in 0:2){
    for(k in 0:2){
      number = c(i,j,k)
      print(number)
    }
  }
}
#B. Given a set of numbers A = {1, 2,.., 9}, assume you are picking 3 numbers without replacement. Find the probability that you get 3 numbers where the minimum number is 2 and the maximum is 5 using at least 2 ways. 
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
#possible combinations of those 3 numbers
for(i in 3:4){
  numbers = c(2,i,5)
  print(numbers)
}


#bonus
#miles per gallon
qqnorm(mtcars$mpg)
qqline(mtcars$mpg)
#motor cylinders
qqnorm(mtcars$cyl)
qqline(mtcars$cyl)
#car displacement
qqnorm(mtcars$disp)
qqline(mtcars$disp)
#car horse power
qqnorm(mtcars$hp)
qqline(mtcars$hp)
#rear axle ratio
qqnorm(mtcars$drat)
qqline(mtcars$drat)
#car weight
qqnorm(mtcars$wt)
qqline(mtcars$wt)
#car acceleration
qqnorm(mtcars$qsec)
qqline(mtcars$qsec)
#motor pistons positions(V hsaped/Straight line)
qqnorm(mtcars$vs)
qqline(mtcars$vs)
#Car transmission type
qqnorm(mtcars$am)
qqline(mtcars$am)
#number of transmission gears
qqnorm(mtcars$gear)
qqline(mtcars$gear)
#number of carburetors
qqnorm(mtcars$carb)
qqline(mtcars$carb)
vshape = filter(mtcars, cyl == 4)
