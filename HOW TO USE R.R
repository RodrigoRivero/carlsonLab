# By: Rodrigo Rivero 
# September 21, 2016

# This R script is meant to teach you how to work with R's basic functionallities, paired with RStudio.
# R is a programming language, written in C and C++ and it replaced the S programming language. 
# RStudio is the environment in which will be using R. Think about writting english in word vs google docs. Same language, different enviornments. 
# Now before we start getting into R, you should now some basic things. This is an R script, and you can have multiple running at once. You can create a script by clikcing file->new->R Script, or simply COMMAND+SHIFT+N
# Also you can change how RStudio looks in the preference pannel, also try to soft wrap the R code so that this script is readable. 
# <- And this indicates whatever follows it is a comment, it has no functionality, except helping other and yourself understand what is going on. Believe me when I say that you will be thankful you wrote comments when you read 6 month old code. Now.....

# HOW TO RUN CODE ON R
# Lets start with some basic math, if you want to find out what a sum or multiplication is you can simply write it
2 + 2 * 8
# And you can click COMMAND+ENTER when the cursor is at the end of the line, or when the text is highlighted. You should be able to see the result in the console underneath this script.

# WORKING WITH DATA
# Lets start by creating some data, I will make a vector called "ages" with the age of five of my friends
ages <- c(21, 20, 20, 19, 23)
# The "<-" is the assignment, think of it as an "=" sign. It is then followed by "c" which means concatanate, a fancy word for joining. You should also see the "ages" in the environment panel on the right ->
# Now how can I find the sum of two of the ages?
ages[1] + ages[3]
# As you can see, writting the number at the end of the name calls that specific value. And unlike other programming langueages, the first value is value number 1, instead of value number 0(I actually do not like this)

# R also has a lot of built in data, mainly for educational purposes but also as good resources. Lets create a vector called "beavers" with data regarding the weights of beavers over a period of time, this data is already in R and is called "beaver1"
beavers <- beaver1
# You should see the data in the environment tab, you can also see what is on it by clicking any of the two buttons around it, or using the function "str" which stands for structure
str(beavers)
# ALSO: if you would like to find out a lot more about something, you can place a question sign before it, and RStudio will tell you what it is and what it does. Try it for "str" as well as "c".
?c
?str
# Now in the console there will be a bunch of $ followed by the name of the column, this means that if you only wanted to work with that color you need to write the name of the data, followed by the $, and then the name of the column
# I am going to find the difference in time between the first and the last measurement(the result will be in minutes)
beavers$time[1] - beavers$time[114]
# Now lets turn that into hours
(beavers$time[1] - beavers$time[114])/60

# PRACTICE
# I want to know the difference in temperature of the beaver between 900 and 1000, as well as between 1400 and 1500. 
# Start from Zero on a sperate script
# I am going to clear the environment so you have to create your own data by using this function
rm(list = ls())  
# As well as clearing the console by clikcing CONTROL+L

# HOW TO INPUT DATA FROM OUTSIDE R
# So now you know some basic moves inside R, how can we get some of our data inside R?
# The best way is my favorite file ever, a txt file. I will add the first ten rows of the beaver data, and then I will explain what happened
NewBeavers <- read.table("~/Desktop/BeaverData.txt", header = TRUE, sep = "\t") 
# The first unfamiliar thing is the "read.table" operation. It simply just tells R that the data in the text file is in a table form. The contents inside the parenthesis include the location of the file, in this case the desktop. If the columns have a header, and what separates the data(in this case is a tab space)
# You can see the txt file on the desktop to confirm.



