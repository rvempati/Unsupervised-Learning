# Iris Data
data <- read.csv(file.choose(), header=T)
# Print the structure of the data
str(data)
# Convert the variety attribute into factor
data$variety <- factor(data$variety)
# Check the new structure of the data
str(data)
# Summary of data
summary(data)

## Partition the data
# Set the seed for repeatability
set.seed(123)
# Make two partitions with 80% and 20% distribution
ind <- sample(2, nrow(data), 
              replace = TRUE, 
              prob = c(.8, .2))

# Assign index 1 to Training data
training <- data[ind==1,]
# Assign index 2 to Testing data
testing <- data[ind==2,]

# Scatter Plots & Correlations
# Install package. Skip if already installed
install.packages("psych")

# load the library
library(psych)
# Correlation plot
pairs.panels(training[,-5], 
             gap=0,
             bg=c("red","yellow","blue")[training$variety],
             pch=21)

# Principle Components
pc <- prcomp(training[,-5],
             center = TRUE,
             scale. = TRUE)
# Attributes of PCA object
attributes(pc)

# The prcomp object stores the eigenvectors as a matrix 
# in the rotation attribute. The name relates to the 
# term rotation matrix and emphasis that a matrix-multiplication
# of the data-matrix with the rotation matrix returns the 
# coordinates of the data in the rotated coordinate system, 
# and thus the principal component scores.
pc$rotation

# The sdev attribute of the prcomp object contains the 
# standard deviation of each principal component as a 
# measure of variance.
pc$sdev

# mean of each attribute
pc$center
mean(training$sepal.length)

# Standard Deviation of each attribute
pc$scale
sd(training$sepal.length)

# Get the variance explained by each principal component 
# we square the standard deviation
pc.var = pc$sdev^2
pc.var

# To compute the proportion of variance explained 
# by each principal component, we simply divide the 
# variance explained by each principal component by 
# the total variance explained by all principal components
pc.var.exp = pc.var/sum(pc.var)
pc.var.exp

# Finally, we are interested in the principal component scores, 
# which correspond to the projection of the original data on 
# the directions of the principal component. The principal component 
# scores are stored in x attribute of the prcomp object. 
# We take a look at the first 10 entries of food.pca$x by applying 
# the head() function.
head(pc$x, 10)

# below prints the rotation attribute
print(pc)

# To get a summary of the object
summary(pc)

# The biplot is a very popular way for visualization of 
# results from PCA, as it combines both, the principal component 
# scores and the loading vectors in a single biplot display. 
# In R we simply call the biplot() function. The scale = 0 argument 
# to biplot() ensures that the arrows are scaled to represent 
# the loadings.
biplot(pc, scale = 0, cex = 0.6)

# Orthogonality of PCs
pairs.panels(pc$x, 
             gap=0,
             bg=c("red","yellow","blue")[training$variety],
             pch=21)

# Bi-Plot
# install.packages("devtools")
library(devtools)
# install_github("vqv/ggbiplot")
library(ggbiplot)
g <- ggbiplot(pc, 
              obs.scale = 1, 
              var.scale = 1, 
              groups = training$variety, 
              ellipse = TRUE, 
              circle = TRUE,
              ellipse.prob = 0.68)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)
