library(tidyverse)
library(readr)
library(dplyr)
library(readr)
library(ggplot2)
Master <- read_delim("Raett_2023.csv",delim = ";", escape_double = FALSE, trim_ws = TRUE)

filtered_master <- Master %>%
  filter(!Species %in% c("pagurus", "gammarus"))  # Exclude the two species

view(filtered_master)


# Assuming 'Station' is the name of the column that identifies each station
station_count_per_cluster <- Master %>%
  group_by(Cluster_ID) %>%  # Group by Cluster
  summarise(Number_of_Stations = n_distinct(Station_ID))  # Count unique stations in each cluster

# Print the summary table
print(station_count_per_cluster)

station_count_per_cluster %>%
  kable("html", caption = "Number of Stations per Cluster") %>%  # Create table in HTML format
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),  # Apply bootstrap styles
    full_width = FALSE,  # Table doesn't occupy full width
    font_size = 12  # Font size
  ) %>%
  column_spec(1, bold = TRUE, color = "black", background = "#f2f2f2") %>%  # Style Cluster_ID column
  column_spec(2, color = "black", background = "#white")  # Style Number_of_Stations column



# Select relevant columns and remove duplicates
head(filtered_master)

unique_stations <- filtered_master %>%
  select(Cluster_ID, Lat, Long, Depth) %>%
  distinct() 
install.packages("dplyr")
library(dplyr) 

table(unique_stations)
view(unique_stations)

library(knitr)
library(kableExtra)


#Depth distribiution ####

#Histogram
 Depth_group<- hist(unique_stations$Depth, 
     main = "Distribution of Depths", 
     xlab = "Depth", 
     col = "grey", 
     border = "white")

library(dplyr)

# Generate histogram and store results
Depth_group <- hist(unique_stations$Depth, 
                    main = "Distribution of Depths", 
                    xlab = "Depth", 
                    col = "grey", 
                    border = "white", 
                    plot = FALSE)  # Avoid plotting

# Create a table from the histogram data
depth_table <- data.frame(
  Depth_Interval = paste0(Depth_group$breaks[-length(Depth_group$breaks)], 
                          "-", 
                          Depth_group$breaks[-1]),  # Create interval labels
  Count = Depth_group$counts  # Number of stations in each interval
)

# Print the table
print(depth_table)

Depth_groups <- hist(unique_stations$Depth, 
                    main = "Distribution of Depths", 
                    xlab = "Depth", 
                    col = "grey", 
                    border = "white", 
                    breaks = 40) # Adjust the number of bins as needed




# Denne vil jeg gjøre

library(dplyr)

# Count the number of stations at each unique depth
depth_table <- unique_stations %>%
  count(Depth) %>%
  arrange(Depth)  # Sort by depth in ascending order

# Print the table
print(depth_table)

# Create histogram with 1m bins
histogram <- hist(unique_stations$Depth, 
                  main = "Number of Stations at Each Depth", 
                  xlab = "Depth (m)", 
                  ylab = "Number of Stations",  # Rename "Frequency"
                  col = "grey", 
                  border = "white", 
                  breaks = seq(min(unique_stations$Depth, na.rm = TRUE), 
                               max(unique_stations$Depth, na.rm = TRUE), by = 1),  # 1m bins
                  plot = FALSE)  # Store the histogram data without plotting


# Plot histogram with adjusted y-axis scaling
histogram_data <- hist(unique_stations$Depth, 
     main = "Number of Stations at Each Depth", 
     xlab = "Depth (m)", 
     ylab = "Number of Stations",  # Rename "Frequency"
     col = "grey", 
     border = "white", 
     breaks = seq(min(unique_stations$Depth, na.rm = TRUE), 
                  max(unique_stations$Depth, na.rm = TRUE), by = 1),  # 1m bins
     ylim = c(0, max(histogram$counts) + 2))  # Scale y-axis above max count

depth_table <- data.frame(
  Depth = histogram_data$mids,  # Midpoints of each bin (depths)
  Number_of_Stations = histogram_data$counts  # Count of stations in each bin
)




# Print the table
print(depth_table)


# Load ggplot2
install.packages("ggplot2")
library(ggplot2)

# Create a boxplot of Depth by Cluster ####

# Create a summary table with the range of depths per cluster
depth_range_table <- unique_stations %>%
  group_by(Cluster_ID) %>%
  summarise(
    Min_Depth = min(Depth, na.rm = TRUE),  # Minimum depth for each cluster
    Max_Depth = max(Depth, na.rm = TRUE)   # Maximum depth for each cluster
  )

#Mean and Median

depth_summary <- unique_stations %>%
  summarise(
    Mean_Depth = mean(Depth, na.rm = TRUE),
    Median_Depth = median(Depth, na.rm = TRUE)
  )


print(depth_summary)
# View the table
print(depth_range_table)

#the plot of depth range

 Depth_Cluster <- ggplot(unique_stations, aes(x = as.factor(Cluster_ID), y = Depth)) +
  geom_boxplot(fill = "grey", color = "black") +  # Boxplot with color
  labs(
    title = "Range of Depths in Different Clusters", 
    x = "Cluster ID", 
    y = "Depth (m)"
  ) +
  theme_minimal()  # Use a minimal theme for cleaner visualization
 print(Depth_Cluster)
 
 #
 
 #Table with information from the depth range boxplot for description
 
 # Assuming 'unique_stations' contains your data with Cluster_ID and Depth
 depth_summary <- unique_stations %>%
   group_by(Cluster_ID) %>%
   summarise(
     Min_Depth = min(Depth, na.rm = TRUE),
     Q1_Depth = quantile(Depth, 0.25, na.rm = TRUE),
     Median_Depth = median(Depth, na.rm = TRUE),
     Q3_Depth = quantile(Depth, 0.75, na.rm = TRUE),
     Max_Depth = max(Depth, na.rm = TRUE),
     IQR_Depth = IQR(Depth, na.rm = TRUE),  # Interquartile range
     Lower_Whisker = Q1_Depth - 1.5 * IQR_Depth,
     Upper_Whisker = Q3_Depth + 1.5 * IQR_Depth,
     Outliers_Lower = list(Depth[Depth < Lower_Whisker]),
     Outliers_Upper = list(Depth[Depth > Upper_Whisker])
   )
 
 # View the summary
 print(depth_summary)
 View(depth_summary)
 
 


 # Plot histogram with adjusted y-axis scaling
 histogram_data <- hist(unique_stations$Depth, 
                        main = "Number of Stations at Each Depth", 
                        xlab = "Depth (m)", 
                        ylab = "Number of Stations",  # Rename "Frequency"
                        col = "grey", 
                        border = "white", 
                        breaks = seq(min(unique_stations$Depth, na.rm = TRUE), 
                                     max(unique_stations$Depth, na.rm = TRUE), by = 1),  # 1m bins
                        ylim = c(0, max(histogram$counts) + 2))  # Scale y-axis above max count
 
 depth_table <- data.frame(
   Depth = histogram_data$mids,  # Midpoints of each bin (depths)
   Number_of_Stations = histogram_data$counts  # Count of stations in each bin
 )
 
 # Print the table
 print(depth_table)
 
 # Create a boxplot of Depth by Cluster
 Depth_Cluster <- ggplot(unique_stations, aes(x = as.factor(Cluster_ID), y = Depth)) +
   geom_boxplot(fill = "grey", color = "black") +  # Boxplot with color
   labs(
     title = "Range of Depths in Different Clusters", 
     x = "Cluster ID", 
     y = "Depth (m)"
   ) +
   theme_minimal()  # Use a minimal theme for cleaner visualization
 
 #make the close to eachother ####
 library(gridExtra)
 
 # Arrange plots
 grid.arrange(Depth_Cluster, histogram_data, ncol = 1)  # Stacks them vertically
 
 # Boxplot for depths in clusters
 Depth_Cluster <- ggplot(unique_stations, aes(x = as.factor(Cluster_ID), y = Depth)) +
   geom_boxplot(fill = "grey", color = "black") +
   labs(title = "Range of Depths in Different Clusters", 
        x = "Cluster ID", 
        y = "Depth (m)") +
   theme_minimal() +
 annotate("text", x = 1, y = max(unique_stations$Depth), label = "B", size = 6, hjust = 0)
 
 # Create histogram using ggplot2
 histogram_data_plot <- ggplot(unique_stations, aes(x = Depth)) +
   geom_histogram(binwidth = 1, fill = "grey", color = "black") +
   labs(title = "Distribution of Depths", 
        x = "Depth (m)", 
        y = "Number of Stations") +
   theme_minimal()+
   annotate("text", x = 1, y = max(unique_stations$Depth), label = "A", size = 6, hjust = 0)
 
 # Combine the two plots vertically
 grid.arrange(histogram_data_plot, Depth_Cluster,  ncol = 1)
 



hist(unique_stations$Depth, 
     main = "Number of Stations at Each Depth", 
     xlab = "Depth (m)", 
     ylab = "Number of Stations",  # Rename "Frequency"
     col = "grey", 
     border = "white", 
     breaks = seq(min(unique_stations$Depth, na.rm = TRUE), 
                  max(unique_stations$Depth, na.rm = TRUE), by = 1),  # 1m bins
     ylim = c(0, max(histogram$counts) + 2))  # Scale y-axis above max count



# Generate histogram and store the results without plotting
Depth_groups <- hist(unique_stations$Depth, 
                     main = "Distribution of Depths", 
                     xlab = "Depth", 
                     ylab = "Number of Stations",  # Rename "Frequency"
                     col = "grey", 
                     border = "white", 
                     breaks = 40,  # Adjust the number of bins
                     plot = FALSE)  # Don't plot yet, just store data

# Plot histogram with adjusted y-axis scaling
hist(unique_stations$Depth, 
     main = "Distribution of Depths", 
     xlab = "Depth", 
     ylab = "Number of Stations",  # Rename frequency
     col = "grey", 
     border = "white", 
     breaks = 40,  # Same number of bins
     ylim = c(0, max(Depth_groups$counts) + 2))  # Scale frequency

# Create a table from histogram data
depth_table <- data.frame(
  Depth_Interval = paste0(Depth_groups$breaks[-length(Depth_groups$breaks)], 
                          "-", 
                          Depth_groups$breaks[-1]),  # Create interval labels
  Number_of_Stations = Depth_groups$counts  # Rename column
)

# Print the table
print(depth_table)








# Observations by family ####
library(ggplot2)


Observation_by_family <- ggplot(filtered_master %>% filter(!is.na(Family), !is.na(MaxN)), aes(x = Family, y = MaxN, fill = Family)) +
  geom_bar(stat = "identity", fill = "darkgrey") +
  theme_minimal() +
  labs(title = "Observations by Family", x = "Family", y = "Total Observations") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
        legend.position = "none")  # Remove legend


plot(Observation_by_family)
total_maxn_all_families <- filtered_master %>%
  filter(!is.na(Family), !is.na(MaxN)) %>%
  summarise(Total_MaxN = sum(MaxN, na.rm = TRUE))

print(total_maxn_all_families)

#

##Percentage of all families to describe the family plot
library(dplyr)

# Calculate total MaxN per family and percentage
family_summary <- filtered_master %>%
  filter(!is.na(Family), !is.na(MaxN)) %>%
  group_by(Family) %>%
  summarise(Total_MaxN = sum(MaxN, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Percentage = round(100 * Total_MaxN / sum(Total_MaxN), 2))  # Calculate percentage

# View result
print(family_summary)


# How many stations they were observed ####

family_summary <- filtered_master %>%
  filter(!is.na(Family), !is.na(MaxN)) %>%
  group_by(Family) %>%
  summarise(
    Total_MaxN = sum(MaxN, na.rm = TRUE),
    Station_Count = n_distinct(Station_ID)
  ) %>%
  ungroup() %>%
  mutate(Percentage = round(100 * Total_MaxN / sum(Total_MaxN), 2)) %>%
  arrange(desc(Percentage))  # Optional: sort by importance

# View the result
print(family_summary)



View(Master)
install.packages("dplyr")
library(dplyr)

library(dplyr)
data_summary <- filtered_master %>%
  group_by(Family) %>%
  summarise(MaxN = sum(MaxN))


# View summarized data
print(data_summary)

View(data_summary)
library(ggplot2)

library(dplyr)

# Remove rows where 'Species' is NA
data_filtered <- data_summary %>%
  filter(!is.na(data_summary))

### Heat map for a representation of the distribution of family and depth column ####

library(reshape2)

heatmap_data <- filtered_master %>%
  group_by(Depth, Family) %>%
  summarise(Count = n(), .groups = 'drop')
View(heatmap_data)

ggplot(heatmap_data %>% filter(!is.na(Depth), !is.na(Family), !is.na(Count)), 
       aes(x = Depth, y = Family, fill = Count)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "blue", high = "red", name = "Count") +
  theme_minimal() +
  labs(
    title = "Family Occurrence Across Depths",
    x = "Depth (m)",
    y = "Family"
  )

### Updated ggplot basend on the mean of MaxN/ station number so it can be representable
## since the number of stations variate

library(dplyr)
library(ggplot2)

# Step 1: Mean MaxN per station, grouped by Depth and Family
station_level <- filtered_master %>%
  filter(!is.na(Depth), !is.na(Family), !is.na(MaxN), !is.na(Station_ID)) %>%
  group_by(Depth, Family, Station_ID) %>%
  summarise(MaxN_station = sum(MaxN), .groups = "drop")  # or mean() depending on your logic

# Step 2: Now calculate mean MaxN per station for each Depth-Family combo
heatmap_data <- station_level %>%
  group_by(Depth, Family) %>%
  summarise(Mean_MaxN_per_station = mean(MaxN_station), .groups = "drop")

view(heatmap_data)

# Step 3: Plot it!
ggplot(heatmap_data, aes(x = Depth, y = Family, fill = Mean_MaxN_per_station)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "blue", high = "red", name = "Mean MaxN\nper Station") +
  theme_minimal() +
  labs(
    title = "Mean MaxN per Station Across Depths and Families",
    x = "Depth (m)",
    y = "Family"
  )


### Categorize continuity of individuals in each depths ####

library(dplyr)
depth_group_summary <- Master %>%
  group_by(Depth) %>%
  summarize(MaxN = n(), .groups = "drop")  # Count rows (individuals)

# Step 2: Visualize results
library(ggplot2)
ggplot(depth_group_summary %>% filter(!is.na(Depth), !is.na(MaxN)), aes(x = Depth, y = MaxN)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(
    title = "Number of Individuals in Each Depth",
    x = "Depth (m)",
    y = "Number of Individuals"
  ) +
  theme_minimal()

# View summary table
print(depth_group_summary)


 Trend_of_indv_across_depth_groups <- ggplot(depth_group_summary %>% filter(!is.na(Depth), !is.na(MaxN)), aes(x = Depth, y = MaxN, group = 1)) +
  geom_line(color = "grey", linewidth = 1.2) +
  geom_point(color = "black", linewidth = 3) +
  labs(
    title = "Trend of Individuals Across Depth Groups",
    x = "Depth(m)",
    y = "Number of Individuals"
  ) +
  theme_minimal()

summary(depth_group_summary)
View(depth_group_summary)


#

##Stack Depth groups of all the stations and how many individuals in each depth
# Create two example plots

# Set the plotting area to have 2 rows and 1 column (stacked vertically)
par(mfrow = c(2, 1))

# Now, both plots will be placed on top of each other
Depth_group<- hist(unique_stations$Depth, 
                   main = "Distribution of Depths", 
                   xlab = "Depth", 
                   col = "grey", 
                   border = "white")

Trend_of_indv_across_depth_groups <- ggplot(depth_group_summary %>% filter(!is.na(Depth), !is.na(MaxN)), aes(x = Depth, y = MaxN, group = 1)) +
  geom_line(color = "grey", linewidth = 1.2) +
  geom_point(color = "black", linewidth = 3) +
  labs(
    title = "Trend of Individuals Across Depth Groups",
    x = "Depth(m)",
    y = "Number of Individuals"
  ) +
  theme_minimal()



### count of Genus on Gadidae family ####
Gadidae_data <- filtered_master %>%
  filter(Family == "Gadidae")

view(Gadidae_data)

# Preview the filtered data
head(Gadidae_data)
View(Gadidae_data)

Gadidae_species <- Gadidae_data %>%
  group_by(Species) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  arrange(desc(Count))

Gadidae_genera <- Gadidae_data %>%
  group_by(Genus) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  arrange(desc(Count))

# View the genus counts
print(Gadidae_species)
library(stringr)



# Endre tomme rom til Unknown

Gadidae_data <- Gadidae_data %>%
  mutate(Genus = str_trim(Genus))

Gadidae_data <- Gadidae_data %>%
  mutate(
    Genus = ifelse(is.na(Genus) | Genus == "", "Unknown", Genus)
  )


Gadidae_genera <- Gadidae_data %>%
  group_by(Genus) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  arrange(desc(Count))

view(Gadidae_genera)

Gadidae_ggplot <- ggplot(Gadidae_species, aes(x = reorder(Species, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  # Flip coordinates for better readability
  theme_minimal() +
  labs(
    title = "Genus Abundance in Family Gadidae",
    x = "Genus",
    y = "Count"
  )

print(Gadidae_ggplot)



Gadidae_ggplot <- ggplot(Gadidae_species, aes(x = reorder(Genus, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  # Flip coordinates for better readability
  theme_minimal() +
  labs(
    title = "Genus Abundance in Family Gadidae",
    x = "Genus",
    y = "Count"
  )

library(ggplot2)
library(dplyr)  # For filtering

# Remove a specific species (e.g., "SomeSpecies")
Gadidae_species_filtered <- Gadidae_species %>% 
  filter(Species != "sp2")  

# Create the plot with the filtered data
Gadidae <- ggplot(Gadidae_species_filtered, aes(x = reorder(Species, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = "darkgrey") +
  coord_flip() +  # Flip coordinates for better readability
  theme_minimal() +
  labs(
    title = "Genus Abundance in Family Gadidae",
    x = "Species",
    y = "Count of Individuals"
  )


print(Gadidae_exclude)
print(Gadidae)  # Display the plot


 

 #
 
#ABUNDANCE OF GADIDAE AND LABRIDAE FAMILY, SPECIES LEVEL

 library(dplyr)
 library(ggplot2)
 library(stringr)
 
 # Process for Gadidae family
 Gadidae_data <- filtered_master %>%
   filter(Family == "Gadidae")
 
 # Clean the species names: rename 'sp2' as 'Unknown'
 Gadidae_data <- Gadidae_data %>%
   mutate(Species = ifelse(Species == "sp2", "Unknown", Species))  # Replace 'sp1' with 'Unknown'
 
 # Summarize the total abundance and station counts for each species
 Gadidae_species <- Gadidae_data %>%
   group_by(Species) %>%
   summarise(
     total_MaxN = sum(MaxN, na.rm = TRUE),  # Total abundance
     station_count = n_distinct(Station_ID),  # Count of unique stations
     .groups = 'drop'
   ) %>%
   mutate(percentage = round(100 * total_MaxN / sum(total_MaxN), 2)) %>%
   arrange(desc(total_MaxN))
 
 # Plot for Gadidae family
 Gadidae_plot <- ggplot(Gadidae_species, aes(x = reorder(Species, -total_MaxN), y = total_MaxN)) +
   geom_bar(stat = "identity", fill = "darkgrey") +
   coord_flip() +  # Flip coordinates for better readability
   theme_minimal() +
   labs(
     title = "Species Abundance in Family Gadidae",
     x = "Species",
     y = "Total MaxN"
   ) +
   theme(
     axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
     axis.text.y = element_text(face = "italic")  # Italicize species names on the y-axis
   )
 
 print(Gadidae_plot)
 
 
 # Process for Labridae family
 Labridae_data <- filtered_master %>%
   filter(Family == "Labridae")
 
 # Clean the species names: rename 'sp5' as 'Unknown'
 Labridae_data <- Labridae_data %>%
   mutate(Species = ifelse(Species == "sp5", "Unknown", Species))  # Replace 'sp1' with 'Unknown'
 
 # Summarize the total abundance and station counts for each species
 Labridae_species <- Labridae_data %>%
   group_by(Species) %>%
   summarise(
     total_MaxN = sum(MaxN, na.rm = TRUE),  # Total abundance
     station_count = n_distinct(Station_ID),  # Count of unique stations
     .groups = 'drop'
   ) %>%
   mutate(percentage = round(100 * total_MaxN / sum(total_MaxN), 2)) %>%
   arrange(desc(total_MaxN))
 
 # Plot for Labridae family
 Labridae_plot <- ggplot(Labridae_species, aes(x = reorder(Species, -total_MaxN), y = total_MaxN)) +
   geom_bar(stat = "identity", fill = "darkgrey") +
   coord_flip() +  # Flip coordinates for better readability
   theme_minimal() +
   labs(
     title = "Species Abundance in Family Labridae",
     x = "Species",
     y = "Total MaxN"
   ) +
   theme(
     axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
     axis.text.y = element_text(face = "italic")  # Italicize species names on the y-axis
   )
 
 print(Labridae_plot)
 
 # Combine both plots using gridExtra package
 library(gridExtra)
 grid.arrange(Labridae_plot, Gadidae_plot, ncol = 1)  # Arrange them vertically
 
 
 # Percentage of labridae and gadidae to explain the plot
 
 library(dplyr)
 
 species_summary <- filtered_master %>%
   filter(Family %in% c("Gadidae", "Labridae")) %>%
   group_by(Family, Species) %>%
   summarise(
     Count = sum(MaxN, na.rm = TRUE),
     .groups = "drop_last"
   ) %>%
   mutate(
     FamilyTotal = sum(Count),
     Percentage = round((Count / FamilyTotal) * 100, 2)
   ) %>%
   ungroup() %>%
   arrange(Family, desc(Count))
 
 print(species_summary)
 
 
 
 
 
 
 
 
 
 
#### Species distribution related to depth ####

library(ggplot2)
library(dplyr)

# Ensure DepthGroup is not NA
his chart will show how different species are distributed across depth groups in a stacked format.

Master1 <- filtered_master %>% filter(!is.na(Depth), !is.na(Species))

# Stacked bar chart

Unknown_Species <- c("sp11", "sp13", "sp2", "sp3", "sp7", "sp8", "sp4", "sp5", "sp6")
  




ggplot(Master1, aes(x = Cluster_ID, fill = Species)) +
  geom_bar() +
  labs(
    title = "Species Distribution Across Depth Groups",
    x = "Depth Group (m)",
    y = "Number of Individuals",
    fill = "Species"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

Her viser man hvor mange ganger arten har vært i de forskjellige dybdene





#Her har jeg tatt alle sp til en egen kategory, Unknown_species ####
ggplot(Master1, aes(x = Cluster_ID, fill = recode(Species, sp4 = "Unknown_species", sp5 = "Unknown_species", sp6 = "Unknown_species", sp11 ="Unknown_species", sp13 = "Unknown_species", sp2 = "Unknown_species", sp3 = "Unknown_species", sp7 = "Unknown_species", sp8 = "Unknown_species"))) +
  geom_bar() +
  labs(
    title = "Species Distribution Across Depth Groups",
    x = "Depth Group (m)",
    y = "Number of Individuals",
    fill = "Species"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#

#Alternative of species ditribution across depth groups 

library(dplyr)

# Summarize the data to get the mean MaxN per station for each species and cluster
species_summary <- Master1 %>%
  group_by(Cluster_ID, Species) %>%
  summarise(
    num_stations = n_distinct(Station_ID),  # Number of unique stations per species per cluster
    total_MaxN = sum(MaxN, na.rm = TRUE),   # Total MaxN across all stations for each species in each cluster
    .groups = "drop"  # Ungroup the data after summarizing
  ) %>%
  mutate(
    mean_MaxN_per_station = total_MaxN / num_stations  # Calculate the mean MaxN per station
  )

library(ggplot2)
install.packages("RColorBrewer")
library(dplyr)
library(RColorBrewer)

# Summarize the data to get the mean MaxN per station for each species and cluster
species_summary <- Master1 %>%
  group_by(Cluster_ID, Species) %>%
  summarise(
    num_stations = n_distinct(Station_ID),  # Number of unique stations per species per cluster
    total_MaxN = sum(MaxN, na.rm = TRUE),   # Total MaxN across all stations for each species in each cluster
    .groups = "drop"  # Ungroup the data after summarizing
  ) %>%
  mutate(
    mean_MaxN_per_station = total_MaxN / num_stations  # Calculate the mean MaxN per station
  )


# Define a custom color palette with more than 12 colors
custom_colors <- c(
  "sp1" = "#E41A1C",  # Red
  "sp2" = "#377EB8",  # Blue
  "sp3" = "#4DAF4A",  # Green
  "sp4" = "#FF7F00",  # Orange
  "sp5" = "#FFFF33",  # Yellow
  "sp6" = "#F781BF",  # Pink
  "sp7" = "#A65628",  # Brown
  "sp8" = "#984EA3",  # Purple
  "sp9" = "#FF0000",  # Bright Red
  "sp10" = "#00FF00",  # Bright Green
  "sp11" = "#0000FF",  # Bright Blue
  "sp12" = "#FF00FF",  # Magenta
  "sp13" = "#00FFFF",  # Cyan
  "Unknown_species" = "#CCCCCC"  # Gray for Unknown species
)

# Plot the data with the custom color palette
ggplot(species_summary, aes(x = Cluster_ID, y = mean_MaxN_per_station, fill = recode(Species, 
                                                                                     sp4 = "Unknown_species", 
                                                                                     sp5 = "Unknown_species", 
                                                                                     sp6 = "Unknown_species", 
                                                                                     sp11 = "Unknown_species", 
                                                                                     sp13 = "Unknown_species", 
                                                                                     sp2 = "Unknown_species", 
                                                                                     sp3 = "Unknown_species", 
                                                                                     sp7 = "Unknown_species", 
                                                                                     sp8 = "Unknown_species"))) +
  geom_bar(stat = "identity", position = "stack") +  # Stack bars for each species in the cluster
  scale_fill_manual(values = custom_colors) +  # Apply the custom color palette
  labs(
    title = "Species Distribution Across Clusters (Mean MaxN per Station)",
    x = "Cluster ID",
    y = "Mean MaxN per Station",
    fill = "Species"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    axis.text.y = element_text(face = "italic")  # Set species names (y-axis) in italic
  )





# View the summary data (optional)
# View(species_summary)



#### Species Distribution ####
### Categorize continuity of individuals in each Cluster_ID ####
library(dplyr)
Cluster_group_summary <- filtered_master %>%
  group_by(Cluster_ID) %>%
  summarize(MaxN = n(), .groups = "drop")  # Count rows (individuals)

# Step 2: Visualize results
library(ggplot2)
ggplot(Cluster_group_summary %>% filter(!is.na(Cluster_ID), !is.na(MaxN)), aes(x = Cluster_ID, y = MaxN)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(
    title = "Number of individuals in each cluster",
    x = "Cluster_ID",
    y = "Number of Individuals"
  ) +
  theme_minimal()

# View summary table
print(Cluster_group_summary)


ggplot(Cluster_group_summary %>% filter(!is.na(Cluster_ID), !is.na(MaxN)), aes(x = Cluster_ID, y = MaxN, group = 1)) +
  geom_line(color = "steelblue", linewidth = 1.2) +
  geom_point(color = "darkblue", linewidth = 3) +
  labs(
    title = "Trend of Individuals Across cluster Groups",
    x = "Cluster_ID",
    y = "Number of Individuals"
  ) +
  theme_minimal()

summary(Cluster_group_summary)
View(Cluster_group_summary)



#### Species Richness and Shannon Index on each cluster ID


library(dplyr)

# Assuming your data has columns: "Cluster" and "Species"
species_richness <- Master %>%
  group_by(Cluster_ID) %>%
  summarise(Richness = n_distinct(Species))  # Count unique species in each cluster

print(species_richness)
library(dplyr)
library(vegan)  # For Shannon index
library(ggplot2)
view(Master)
# Calculate species richness (count of unique species per cluster)
species_diversity <- Master %>%
  group_by(Cluster_ID) %>%
  summarise(
    Richness = n_distinct(Species),  # Unique species count
    Shannon_Index = diversity(MaxN, index = "shannon")  # Shannon index
  )

Master$MaxN[is.na(Master$MaxN)] <- 0

# Find the smallest sample size across clusters
min_sample_size <- min(tapply(Master$MaxN, Master$Cluster_ID, sum))

# Compute rarefied richness per cluster
rarefied_richness <- rarefy(Master$MaxN, sample = min_sample_size)

# Add rarefied richness to the summary table
species_diversity$Rarefied_Richness <- rarefied_richness

print(species_diversity)


# Print the summary table
print(species_diversity)
