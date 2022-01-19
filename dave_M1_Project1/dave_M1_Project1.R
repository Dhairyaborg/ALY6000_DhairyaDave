
#1. Print your name at the top of the script 
print("Dhairya Purneshkumar Dave")

#2. install the vcd package
install.packages("vcd")
help(package="vcd")

#3. Import the vcd library
library(vcd)

# 4. Plot a sales ~ temp scatter plot using the data below 
# Sales data: (7,11,15,20,19,11,18,10,6,22) 
# Temperature data: (69,81,77,84,80,97,87,70,65,90) 
Sales_data <- c(7,11,15,20,19,11,18,10,6,22) 
Temperature_data <- c(69,81,77,84,80,97,87,70,65,90)
plot(Sales_data, Temperature_data)

#5. Find the mean temperature 
mean<-mean(Temperature_data)
mean

#6. Delete the 3rd element from the sales vector 
newsales<-Sales_data[-3]
newsales

#7. Insert 16 as the 3rd element into the sales vector
Sales <-append(newsales, 16 , 2)
Sales

#8. Create a vector <names> with elements Tom, Dick, Harry (pg 22) 
names <- c("Tom", "Dick", "Harry")

#9. Create a 5 row and 2 column matrix of 10 integers
matrix(1:10,5,2)

#10. Create a data frame <icSales> with sales and temp attributes 
icSales<- data.frame(Sales,Temperature_data)

#11. Display the data frame structure of icScales
icSales

#12. Display a summary of the icScales data frame 
summary(icSales)
#13. Import the dataset Student.csv
mydata<-read.csv("Student.csv")

#14. Display only the variable names of the Student.csv dataset 
onlyvariable<-mydata[0,]
onlyvariable
