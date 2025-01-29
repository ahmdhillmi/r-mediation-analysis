library(readr)
ml_sim <- read_csv("path.csv") #example
View(ml_sim)

# create folder in directory in use
dir.create("data")
 
# Save as CSV in folder in directory
write.csv(ml_sim, "data/example_data.csv")