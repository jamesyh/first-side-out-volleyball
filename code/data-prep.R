load(file = "fullData.RData") # I got the data from Corley at VolleyMetrics
#full.data <- read.csv("C:/Users/James/Desktop/Work/Work/mensdata.csv")
mensdata <- fullData[,c("sk","skgrd","strzn","x1","y1","x3","y3","Play_In_Rally",
                        "Prg_Rally_Data_ID","wonlost","team","fbso")]


mensdata$home <- ifelse(fullData$team == 1, 1, 0) # 1 if home 0 if away

## subset data to make easier for now

mensdata <- mensdata[1:(nrow(fullData)/6),]

# Following code adds the finally length of the rally to each point in the rally
mensdata$Length_of_Rally <- NA

#mensdata <- mensdata[1:c(nrow(mensdata)/100),]

n <- max(mensdata$Prg_Rally_Data_ID)

for(i in 1:n){
  mensdata[mensdata$Prg_Rally_Data_ID == i,"Length_of_Rally"] <- max(mensdata[mensdata$Prg_Rally_Data_ID == i,"Play_In_Rally"])
}

mensdata$rowid <- 1:nrow(mensdata)


## If the rally crossed the net 3 times then the variable One_Three gives how many times it was touched
mensdata$One_Threes <- 0

for(i in 1:n){
  mensdata[mensdata$Length_of_Rally == 3 & mensdata$Prg_Rally_Data_ID == i,"One_Threes"] <- sum(mensdata[mensdata$Length_of_Rally == 3 & mensdata$Prg_Rally_Data_ID == i,"Play_In_Rally"] == 3)
  
}

mensdata[is.na(mensdata$One_Threes),"One_Threes"] <- 0 # takes out NAs

# Following gives info on the First Side Out, 1 if it is
mensdata$First_Side_Out <- 0

mensdata <- mensdata[!is.na(mensdata$Length_of_Rally == 1 & mensdata$wonlost == 0),]

mensdata[mensdata$wonlost == 0 & mensdata$One_Threes == 1, 'First_Side_Out'] <- 1
mensdata[mensdata$Length_of_Rally == 2 & mensdata$wonlost == 1,'First_Side_Out'] <- 1
mensdata[mensdata$Length_of_Rally == 1 & mensdata$wonlost == 0,"First_Side_Out"] <- 1

### K I think this is it ####



# subsetting by serves
mensdata.2 <- mensdata[mensdata$sk == 1,]
#write.csv(mensdata,"mensdata.csv")
#write.csv(mensdata.2,"mensdata.2.csv")



x <- mensdata.2$x3
y <- abs(mensdata.2$y3-9)

plot(y,x)

abline(v = 0)
abline(v = 4.5)
abline(v = 9)

abline(h = 0)
abline(h = 9)
abline(h = 3)
abline(h = 6)

First.Side.Out <- mensdata.2$First_Side_Out 
# First.Side.Out <- mensdata.2$fbso *1
# First.Side.Out[is.na(First.Side.Out)] <- 0

f1 <- 9 >= x & x >= 6 & 9 >= y & y >= 4.5 & First.Side.Out == 1
f2 <- 9 >= x & x >= 6 & 4.5 >= y & y >= 0 & First.Side.Out == 1  
f3 <- 6 > x & x >= 3 & 4.5 >= y & y >= 0 & First.Side.Out == 1
f4 <- 3 > x & x >= 0 & 4.5 >= y & y >= 0 & First.Side.Out == 1
f5 <- 3 > x & x >= 0 & 9 >= y & y >= 4.5 & First.Side.Out == 1
f6 <- 6 > x & x >= 3 & 9 >= y & y >= 4.5 & First.Side.Out == 1

t1 <- 9 >= x & x >= 6 & 9 >= y & y >= 4.5 
t2 <- 9 >= x & x >= 6 & 4.5 >= y & y >= 0   
t3 <- 6 > x & x >= 3 & 4.5 >= y & y >= 0 
t4 <- 3 > x & x >= 0 & 4.5 >= y & y >= 0 
t5 <- 3 > x & x >= 0 & 9 >= y & y >= 4.5 
t6 <- 6 > x & x >= 3 & 9 >= y & y >= 4.5 


plot(y,x)

points(y[f1], x[f1], col = 'green', pch = 20)
points(y[f2], x[f2], col = 'purple', pch = 20)
points(y[f3], x[f3], col = 'orange', pch = 20)
points(y[f4], x[f4], col = 'yellow', pch = 20)
points(y[f5], x[f5], col = 'red', pch = 20)
points(y[f6], x[f6], col = 'blue', pch = 20)


n <- length(f1)

# Percentage of first side out by area

p1 <- sum(f1)/sum(t1)
p2 <- sum(f2)/sum(t2)
p3 <- sum(f3)/sum(t3)
p4 <- sum(f4)/sum(t4)
p5 <- sum(f5)/sum(t5)
p6 <- sum(f6)/sum(t6)



se1 <- sqrt(p1*(1-p1)/n)
se2 <- sqrt(p2*(1-p2)/n)
se3 <- sqrt(p3*(1-p3)/n)
se4 <- sqrt(p4*(1-p4)/n)
se5 <- sqrt(p5*(1-p5)/n)
se6 <- sqrt(p6*(1-p6)/n)


per <- rbind(
  c(p1 - 2*se1 , p1 , p1 + 2*se1),
  c(p2 - 2*se2 , p2 , p2 + 2*se2),
  c(p3 - 2*se3 , p3 , p3 + 2*se3),
  c(p4 - 2*se4 , p4 , p4 + 2*se4),
  c(p5 - 2*se5 , p5 , p5 + 2*se5),
  c(p6 - 2*se6 , p6 , p6 + 2*se6))

rownames(per) <- c("Area1","Area2","Area3","Area4","Area5","Area6")
colnames(per) <- c("Lower","Estimate","Upper")

