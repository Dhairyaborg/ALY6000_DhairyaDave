# 1. Print your name at the top of the script. Include the prefix: “Plotting Basics:” such that it 
# appears “Plotting Basics: Lastname”
print("Plotting Basics: Dave")

# 2. Import libraries including: FSA, FSAdata, magrittr,  dplyr, plotrix, ggplot2, and moments 
# NOTE:  You must use R version 3.6.3 to gain access to the FSA data set.  If you installed a 
# later version of R, you must uninstall Rstudio and R. Then reinstall R version 3.6.3; then 
# reinstall Rstudio. 
install.packages("FSA")
install.packages("FSAdata")
install.packages("magrittr")
install.packages("dplyr")
install.packages("plotrix")
install.packages("ggplot2")
install.packages("moments")
library("FSA", "FSAdata", "ggplot2", "magittr")
library("dplyr")
library("plotrix")
library("moments")

# 3. Load the BullTroutRML2 dataset (BullTroutRML2.csv) 
# NOTE: The dataset is already imported into your project when you added the FSA and 
# FSAdata libraries. 
data(BullTroutRML2)
print(BullTroutRML2)

#4. Print the first and last 3 records from the BullTroutRMS2 dataset 
headtail(BullTroutRML2,n=3)

# 5. Remove all records except those from Harrison Lake (hint: use the <filterD() function) 
BullTroutRML2%>%dplyr::filter(lake=="Harrison")%>%print->ds

#6. Display the first and last 5 records from the filtered BullTroutRML2 dataset
headtail(ds,n=5)

#7. Display the structure of the filtered BullTroutRML2dataset 
str(ds)

#8. Display the summary of the filtered BullTroutRML2dataset 
summary(ds)
skewness(ds$age,ds$fl)
kurtosis(ds$age,ds$fl)
var(ds$age,ds$fl)
sd(ds$age,ds$fl)
# 9. Create a scatterplot for “age” (y variable) and “fl” (x variable) with the following 
# specifications: 
#  Limit of x axis is (0,500) 
#  Limit of y axis is (0,15) 
#  Title of graph is “Plot 1: Harrison Lake Trout 
#  Y axis label is “Age (yrs)” 
#  X axis label is “Fork Length (mm)” 
#  Use a small filled circle for the plotted data points 
plot(ds$fl,ds$age, 
     ylim=c(0, 15), xlim=c(0,500),
     ylab="Age (yrs)", xlab="Fork length(mm)", 
     las=1, cex=.9, main=" Plot 1: Harrison Lake Trout")

# 10. Plot an “Age” histogram with the following specifications 
#  Y axis label is “Frequency” 
#  X axis label is “Age (yrs)” 
#  Title of the histogram is “Plot 2: Harrison Fish Age Distribution” 
# X and Y axis limits is 0, 15 
#  The color of the frequency plots is “cadetblue” 
#  The color of the Title is “cadetblue” 
hist(ds$age, 
     ylim=c(0,15), xlim=c(0,15), ylab="Frequency", xlab="Age(yrs)", 
     main="Plot2: Harrison Fish Age Distribution",col.main="cadetblue", 
     col=c("cadetblue"))

# 11. Create an overdense plot using the same specifications as the previous scatterplot. But,  
#  Title the plot “Plot 3: Harrison Density Shaded by Era” 
#  Y axis label is “Age (yrs)” 
#  Y axis limits are 0 to 15 
#  X axis label is “Fork Length (mm)” 
#   X axis limits are 0 to 500  
#  include two levels of shading for the “green” data points.  
#  Plot solid circles as data points 
smoothScatter(ds$fl,ds$age,xlab = "Fork Length (mm)",ylab = "Age(yrs)", xlim = c(0,500),ylim = c(0,15),
              main="Plot 3:Harrison Density Shaded by Era",col=c("green"),las=1, cex =0.9,pch = 21)

# 12. Create a new object called “tmp” that includes the first 3 and last 3 records of the 
# BullTroutRML2 data set. 
a<- head(ds,n=3)
b<-tail(ds,n=3)
tmp<-rbind(a,b)

#13. Display the “era” column (variable) in the new “tmp” object
era<-tmp$era
era

#14. Create a pchs vector with the argument values for + and x.  
pchs<- c("+","x")
pchs
#15. Create a cols vector with the two elements “red” and “gray60” 
cols<-c("red","gray60")
cols

#16. Convert the tmp era values to numeric values. 
sapply(ds, class)
ds$era=as.numeric(ds$era)

#17. Initialize the cols vector with the tmp era values 
initialize(cols,ds$era)

# 18. Create a plot of “Age (yrs)” (y variable) versus “Fork Length (mm)” (x variable) with the 
# following specifications: 
#    Title of graph is “Plot 4: Symbol & Color by Era” 
#  Limit of x axis is (0,500) 
#  Limit of y axis is (0,15) 
#  X axis label is “Age (yrs)” 
#  Y axis label is “Fork Length (mm)” 
#  Set pch equal to pchs era values 
#  Set col equal to cols era values 
plot(ds$fl,ds$age,xlim = c(0,500), ylim = c(0,15),ylab = "Age(yrs)",xlab = "Fork Length(mm)",
     main = "Plot 4: Symbol & Color by Era",pch = pchs,col=cols)
# 19. Plot a regression line overlay on Plot 4 and title the new graph “Plot 5: Regression 
# Overlay”. 
plot(ds$fl,ds$age,xlim = c(0,500), ylim = c(0,15),ylab = "Age(yrs)",xlab = "Fork Length(mm)",
     main = "Plot 5: Regression Overlay",pch = pchs,col=cols)
abline(lm(age~fl, data = ds)) 

# 20. Place a legend of on Plot 5 and call the new graph “Plot 6: :Legend Overlay” 
plot(ds$fl,ds$age,xlim = c(0,500), ylim = c(0,15),ylab = "Age(yrs)",xlab = "Fork Length(mm)",
     main = "Plot 6: Legend Overlay",pch = pchs,col=cols)

abline(lm(age~fl, data = ds)) 

legend("topleft",title=("Time duration"),legend=c("1977-80", "1997-01"),pch = pchs,col=cols)

