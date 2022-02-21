# Code to process body fat data from real world example ########################
# 
# Original dataset downloaded from 
# http://jse.amstat.org/v4n1/datasets.johnson.html 
# The dataset is free to use for non-commercial purposes. See the 
# link for a description of all variables.
#
# Author: Michael Kammer
# Date: 21-02-2022

# Download #####################################################################
# Uncomment the next two lines to automatically download the file 
# download.file("http://jse.amstat.org/datasets/fat.dat.txt",
#               destfile = "fat.dat.txt", method = "curl")
# Otherwise, please download the file from manually and place it in 
# the current directory.

# Preprocessing ################################################################

# Read data
fat = read.table("./fat.dat.txt", header = FALSE)

# Set column names for clarity
names(fat) = c(
    "ID", 
    "Bodyfat_percent_Brozek", "Bodyfat_percent_Siri", "Density",
    "Age", "Weight_lbs", "Height_inches", 
    "Adiposity_index", "Fat_free_weight", 
    "Neck", "Chest", "Abdomen", "Hip", "Thigh", "Knee", "Ankle", 
    "Biceps", "Forearm", "Wrist"
)

# Correct implausible values as outlined by the providers of the
# dataset
fat$Height_inches[fat$Height_inches == 29.5] = 69.5

# Remove implausible case 39
fat = fat[-39, ]

# Change units for clarity
fat$Age = fat$Age / 10
fat$Weight_kg = round(fat$Weight_lbs / 2.2, 1)
fat$Height_dm = round(fat$Height_inches * 2.54) / 10

# Define modeling data
X = fat[, c("Age", "Weight_kg",  
            "Neck", "Chest", "Abdomen", "Hip", "Thigh", "Knee", 
            "Ankle", "Biceps", "Forearm", "Wrist", 
            "Height_dm")]
X_scaled = scale(X)
y = fat$Bodyfat_percent_Siri
