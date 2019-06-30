suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(forcats))

options(scipen = 999,
        readr.num_columns = 0)

print("Starting Workflow")

# import dataframe
print("Importing Dataframe")
df <- read_csv("01_data/us-500.csv")

# manipulate data
print("Manipulating Data")
plot_data <- df %>%
  group_by(state) %>%
  count()

# save manipulated data to output folder
print("Writing manipulated Data to .csv")
write_csv(plot_data, "03_output/plot_data.csv")

# create plot based on manipulated data
print("Creating Plot")
plot <- plot_data %>% 
  ggplot()+
  geom_col(aes(fct_reorder(state, n), 
               n, 
               fill = n))+
  coord_flip()+
  labs(
    title = "Number of people by state",
    subtitle = "From US-500 dataset",
    x = "State",
    y = "Number of people"
  )+ 
  theme_bw()

# save plot to output folder
print("Saving Plot")
ggsave("03_output/myplot.png", width = 10, height = 8, dpi = 100)
print("Worflow Finished")
