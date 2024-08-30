setwd("D:/Documents/RSMAS/Bayesian/Project")

wtpa <- read.csv("all_bc.csv")

library(tidyverse)
library(FSA)

test <- pivot_longer(wtpa, c(ONE.L, TWO.L, THREE.L, FOUR.L, FIVE.L, SIX.L, SEVEN.L, EIGHT.L), names_to = "otolith", values_drop_na = TRUE)
test1 <- test
test1$otolith <-as.factor(test1$otolith)
test1$otolith <- factor(test1$otolith, levels = c("ONE.L", "TWO.L", "THREE.L", "FOUR.L", "FIVE.L", "SIX.L", "SEVEN.L", "EIGHT.L"))
test1$otolith <- as.numeric(test1$otolith)

test1$otolith

plot(wtpa$Age,wtpa$Length, xlab = "Age", ylab = "Total Length (cm)" )
points(test1$otolith, test1$value)

wtpa$test <- rep("Measured")
test1$test <- rep("Back-Calc")

df1 <- data.frame("Length" = c(test1$value, wtpa$Length), "Age" = c(test1$otolith, wtpa$Age), "Site" = c(test1$Site, wtpa$Site), "BC" = c(test1$test, wtpa$test))
plot(df1$Age, df1$Length)
df1$BC <- as.factor(df1$BC)
#histograms
ggplot(df1, aes(Length, fill = BC)) +
  geom_histogram(binwidth = 10, color = "black") + scale_fill_manual(values=c("white", "black")) +ggtitle("Histogram of Fish Length (cm)") + ylab("Count")+ theme_classic()


#hist(wtpa$Length, xlab = "Total Length (cm)", main = "Histogram of All Fish")
#hist(df1$Length, xlab = "Total Length (cm)", main = "Histogram of All Fish and Back-Calculated Lengths")

#raw data plotted
ggplot(df1, aes(x = Age, y = Length, color = BC)) +
  geom_point() + scale_color_manual(values=c("#999999", "black"))

#plot(wtpa$Age, wtpa$Length, xlab = "Age", ylab = "Total Length (cm)", main = "All Fish")
#plot(df1$Age, df1$Length, xlab = "Age", ylab = "Total Length (cm)", main = "All Fish and Back-Calculations")

#age histograms
ggplot(df1, aes(Age, fill = BC)) +
  geom_histogram(binwidth = 1, color = "black") + scale_fill_manual(values=c("white", "black"))+ggtitle("Histogram of Fish Age") + ylab("Count") + theme_classic()
df1$round <- ceiling(df1$Age)
ggplot(df1, aes(round, fill = BC)) +
  geom_histogram(bins = 9, color = "black") + scale_fill_manual(values=c("white", "black"))+ggtitle("Histogram of Fish Age") + ylab("Count") + theme_classic()

mes <- filter(df1, BC == "Measured")
ggplot(mes, aes(x = Age, y = Length)) +
  geom_point(aes(shape = Site, color = Site))

ggplot(df1, aes(x = Age, y = Length)) +
  geom_point(aes(shape = Site))

