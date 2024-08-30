##### Clean and Examine White Perch Otolith Data #####

# Load packages
library(tidyverse)
library(FSA)

# Read in the white perch age data (wtpa)
wtpa <- read.csv("all_bc.csv")

# Reformat the back-calculated length at ages
bc_wtpa <- pivot_longer(wtpa, c(ONE.L, TWO.L, THREE.L, FOUR.L, FIVE.L, SIX.L, SEVEN.L, EIGHT.L), names_to = "otolith", values_drop_na = TRUE)
bc_wtpa$otolith <-as.factor(bc_wtpa$otolith)
bc_wtpa$otolith <- factor(bc_wtpa$otolith, levels = c("ONE.L", "TWO.L", "THREE.L", "FOUR.L", "FIVE.L", "SIX.L", "SEVEN.L", "EIGHT.L"))
bc_wtpa$otolith <- as.numeric(bc_wtpa$otolith)

# Check that there are 8 factors
unique(bc_wtpa$otolith)

# Add a type of measurement variable
wtpa$type <- rep("Measured")
bc_wtpa$type <- rep("Back-Calc")

# Merge the two types of datasets with cleaned variables
cleaned_wtpa <- data.frame("Length" = c(bc_wtpa$value, wtpa$Length), "Age" = c(bc_wtpa$otolith, wtpa$Age), "Site" = c(bc_wtpa$Site, wtpa$Site), "Type" = c(bc_wtpa$type, wtpa$type))
cleaned_wtpa$Type <- as.factor(cleaned_wtpa$Type)

# Histogram of Fish Length
ggplot(cleaned_wtpa, aes(Length, fill = Type)) +
  geom_histogram(binwidth = 10, color = "black") + scale_fill_manual(values=c("white", "black")) +ggtitle("Histogram of Fish Length (cm)") + ylab("Count")+ theme_classic()

# Histogram of Fish Ages
ggplot(cleaned_wtpa, aes(Age, fill = Type)) +
  geom_histogram(binwidth = 1, color = "black") + scale_fill_manual(values=c("white", "black"))+ggtitle("Histogram of Fish Age") + ylab("Count") + theme_classic()

# Scatterplot of raw length at age by type
ggplot(cleaned_wtpa, aes(x = Age, y = Length, color = Type)) +
  geom_point() + scale_color_manual(values=c("#999999", "black"))

# Measured-type length at age by location
mes <- filter(cleaned_wtpa, Type == "Measured")
ggplot(mes, aes(x = Age, y = Length)) +
  geom_point(aes(shape = Site, color = Site))

# All length at ages by location
ggplot(cleaned_wtpa, aes(x = Age, y = Length)) +
  geom_point(aes(shape = Site))

