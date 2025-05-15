library(tidyverse)
library(readr)
library(dplyr)
citation("ggplot2")
packageVersion("ggplot2")
packageVersion(("vegan"))
citation()
citation("glmmTMB")

citation("vegan")
packageVersion("vegan")

Habitat  <- read.csv("C:/Users/Saray/Desktop/Rstudio_Master_Graphs/Master/TMOBs_prensence_absence_2023.csv", sep=";")


Fish <- read_delim("C:/Users/Saray/Desktop/Rstudio_Master_Graphs/Master/Raett_2023.csv",delim = ";",
                   escape_double = FALSE, trim_ws = TRUE)

filtered_fish <- Fish %>%
  filter(!Species %in% c("pagurus", "gammarus"))  # Exclude the two species

#Excluding cancridae and humurus
merged_data_U <- Habitat %>%
  left_join(filtered_fish %>% filter(!is.na(Cluster_ID)), by = c("Station_ID", "Cluster_ID"))

view(merged_data_U)
str(merged_data_U)
# Keeps all habitat data, adds matching fish data

str(merged_data_U)

#housekeeping
merged_data_U$Station_ID<-as.factor(merged_data_Species_R$Station_ID)
merged_data_U$Family <-as.factor(merged_data_U$Family)
merged_data_U$Dominant_habitat <-as.factor(merged_data_U$Dominant_habitat)
merged_data_U$Cluster_ID <-as.factor(merged_data_U$Cluster_ID)
merged_data_U$Station_ID<- as.factor(merged_data_U$Station_ID)

View(Fish)



view(merged_data_U)
#SJEKKE om stasjoner som mangler mellom fisk og habitat 



missing_stations <- setdiff(Fish$Station_ID, Habitat$Station_ID)

print(missing_stations)

View(missing_stations)



view(Habitat)
view(Fish)

Fish <- subset(merged_data_U, Station_ID)
unique(merged_data_U$Station_ID) 

unique_stations <- Fish %>%
  select(Station_ID, Lat, Long, Depth) %>%
  distinct() 
library(dplyr) 

        
view(unique_stations)   



#Make categories of habitat "Reef rock, "vegetation" and "sediment"


library(dplyr)

merged_data_U <- merged_data_U %>%
  mutate(HabitatGroup = case_when(
    Dominant_habitat %in% c("Bedrock", "Boulder", "Cobble") ~ "Hard substrate",
    Dominant_habitat %in% c("Biogenic", "Erect coarse branching", "Erect fine branching", "Filamentous", "Seaweed", "Laminate", "Kelp") ~ "Vegetation",
    Dominant_habitat %in% c("Sand/Mud", "Gravel") ~ "Soft substrate",
    TRUE ~ "Other"
  ))

view(merged_data_U)

##### Mean of unique species in each depth group ### 

# Create Depth Groups (filtering out NA for Depth, but keeping stations with no species)
merged_data_U <- merged_data_U %>%
  mutate(depth_group = cut(Depth, 
                           breaks = seq(5, 40, by = 5),  # Depth groups from 0 to 40
                           include.lowest = TRUE, 
                           labels = c("5-10m", "10-15m", "15-20m", "20-25m", "25-30m", "30-35m", "35-40m"))) 

# Step 1: Count the number of unique species per station (including stations with no species)
depth_summary <- merged_data_U %>%
  group_by(depth_group, Station_ID) %>%
  summarise(
    unique_species_count = n_distinct(Species),  # Count unique species observed at the station
    .groups = 'drop'
  ) %>%
  group_by(depth_group) %>%
  summarise(
    num_stations = n_distinct(Station_ID),  # Count of unique stations per depth group
    mean_unique_species = mean(unique_species_count, na.rm = TRUE),  # Mean unique species per station
    sd_unique_species = sd(unique_species_count, na.rm = TRUE),  # Standard deviation
    se_unique_species = sd_unique_species / sqrt(num_stations),  # Standard error
    .groups = 'drop'
  ) %>%
  # Filter out groups with fewer than 2 stations (confidence intervals can't be calculated)
  filter(num_stations > 1) %>%
  mutate(
    # Calculate 95% Confidence Interval for the mean using qt() for a t-distribution
    ci_lower = mean_unique_species - qt(0.975, df = num_stations - 1) * se_unique_species,
    ci_upper = mean_unique_species + qt(0.975, df = num_stations - 1) * se_unique_species
  )

# View the summary data to check the calculations
print(depth_summary)


# Plotting the results with confidence intervals and number of stations
ggplot(depth_summary, aes(x = depth_group, y = mean_unique_species, fill = depth_group)) +
  geom_bar(stat = "identity", color = "black", fill = "grey") +  # Set bars to grey color
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +  # Add error bars for confidence intervals
  geom_text(aes(label = paste("n =", num_stations)), 
            vjust = -1,  # Move the text above the error bar
            position = position_dodge(width = 0.8),  # Ensure the text appears above the bars
            color = "black") +  # Set color for the station labels
  labs(
    title = "Mean species richness per depth group",
    x = "Depth Group",
    y = "Mean Unique Species per station",
    fill = "Depth Group"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for better readability
  scale_y_continuous(limits = c(0, 6))  # Set the y-axis range from 0 to 10




#including 0-5?? I don't have any at 0? that does not make sence
#I have it still just in case if I want to use it instead

# Plotting the results with confidence intervals and number of stations
ggplot(depth_summary, aes(x = depth_group, y = mean_unique_species, fill = depth_group)) +
  geom_bar(stat = "identity", color = "black", fill = "grey") +  # Set bars to grey color
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +  # Add error bars for confidence intervals
  geom_text(aes(label = paste("n =", num_stations)), 
            vjust = -1,  # Move the text above the error bar
            position = position_dodge(width = 0.8),  # Ensure the text appears above the bars
            color = "black") +  # Set color for the station labels
  labs(
    title = "Mean species richness per depth group",
    x = "Depth Group",
    y = "Mean Unique Species per station",
    fill = "Depth Group"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


#







# Habitats across Depths


# Create a deduplicated version of the data
station_data <- merged_data_U %>%
  select(Station_ID, Depth, Dominant_habitat) %>%
  distinct()

# Make sure Dominant_habitat is still a factor
station_data$Dominant_habitat <- factor(station_data$Dominant_habitat)

# Adjust plot margins for better label spacing
par(mar = c(12, 4, 4, 2))

# Create the boxplot using unique station-level data
boxplot(Depth ~ Dominant_habitat, data = station_data,
        ylab = "Depth",
        main = "Depth Distribution by Habitat",
        col = "Grey",
        las = 2,
        xlab = "")

# Add x-axis label manually
mtext("Dominant_habitat", side = 1, line = 9)


# Step 6: Calculate and overlay mean points
means <- tapply(station_data$Depth, station_data$Dominant_habitat, mean, na.rm = TRUE)
points(x = 1:length(means), y = means, col = "red", pch = 19, cex = 1.2)
#



#Table of the information of the boxplots habtiat + depth 
summary_table <- station_data %>%
  group_by(Dominant_habitat) %>%
  summarise(
    Count = n(),
    Min = min(Depth, na.rm = TRUE),
    Q1 = quantile(Depth, 0.25, na.rm = TRUE),
    Median = median(Depth, na.rm = TRUE),
    Q3 = quantile(Depth, 0.75, na.rm = TRUE),
    Max = max(Depth, na.rm = TRUE),
    Mean = mean(Depth, na.rm = TRUE),
    SD = sd(Depth, na.rm = TRUE)
  )

str(merged_data_U$Dominant_habitat)
unique(merged_data_U$Dominant_habitat)

nrow(merged_data_U)
table(merged_data_U$Dominant_habitat)

view(duplicated_rows)
# Print the table
print(summary_table)

view(summary_table)

#####Make a table with the mean of selected species: COD, POLLACHIUS, RUPESTRIS, MIXTUS ##### 

library(dplyr)

summary_table_meanMaxN <- merged_data_U %>%
  group_by(Cluster_ID) %>%
  summarise(
    num_stations = n_distinct(Station_ID),  # Count the number of distinct stations per cluster
    total_MaxN = sum(MaxN, na.rm = TRUE),   # Sum the MaxN per cluster
    mean_MaxN = total_MaxN / num_stations   # Calculate the mean MaxN per cluster
  )

view(summary_table)


summary_table_meanMaxN1 <- merged_data_U %>%
  filter(Species %in% c("morhua", "pollachius", "rupestris", "mixtus")) %>%
  group_by(Cluster_ID, Species) %>%
  summarise(
    num_stations = n_distinct(Station_ID),  # Count the number of distinct stations per cluster
    total_MaxN = sum(MaxN, na.rm = TRUE),   # Sum the MaxN per species in each cluster
    mean_MaxN = total_MaxN / num_stations   # Calculate the mean MaxN per species in each cluster
  )

view(summary_table_meanMaxN1)






# Step 1: Summarize without depth group (Cluster-Level Summary)
summary_table_meanMaxN2 <- merged_data_U %>%
  filter(Species %in% c("morhua", "pollachius", "rupestris", "mixtus")) %>%
  group_by(Cluster_ID, Species) %>%
  summarise(
    num_stations = n_distinct(Station_ID),  # Total stations in the cluster
    total_MaxN = sum(MaxN, na.rm = TRUE)    # Total MaxN in the cluster
  ) %>%
  mutate(mean_MaxN = total_MaxN / num_stations)  # Calculate mean MaxN per cluster

# Step 2: Summarize by Depth Group (Depth-Level Summary)
depth_summary <- merged_data_U %>%
  filter(Species %in% c("morhua", "pollachius", "rupestris", "mixtus")) %>%
  mutate(depth_group = cut(Depth, 
                           breaks = seq(5, 40, by = 5),  # Starts from 5m
                           include.lowest = TRUE, 
                           labels = c("5-10m", "10-15m", "15-20m", "20-25m", "25-30m", "30-35m", "35-40m"))) %>%
  group_by(Cluster_ID, Species, depth_group) %>%
  summarise(
    num_stations_depth = n_distinct(Station_ID),  # Count stations per depth group
    total_MaxN_depth = sum(MaxN, na.rm = TRUE)   # Total MaxN per depth group
  )

# Step 3: Merge depth group info into main summary
final_summary <- left_join(summary_table_meanMaxN2, depth_summary, by = c("Cluster_ID", "Species"))

# View final result
print(final_summary)
view(final_summary)


# View final result
print(final_summary)
view(final_summary)
library(ggplot2)
library(dplyr)




# Alternativ: MaxN into mean and devided into the number of stations (bruvs)  Good

# Create depth groups and summarize for specific species
depth_summary <- merged_data_U %>%
  filter(Species %in% c("morhua", "pollachius", "rupestris", "mixtus")) %>%
  mutate(depth_group = cut(
    Depth, 
    breaks = seq(5, 40, by = 5),  # Starting from 5m up to 40m
    include.lowest = TRUE, 
    labels = c("5-10m", "10-15m", "15-20m", "20-25m", "25-30m", "30-35m", "35-40m")
  )) %>%
  group_by(Cluster_ID, Species, depth_group) %>%
  summarise(
    num_stations_depth = n_distinct(Station_ID),  # Count stations per depth group
    total_MaxN_depth = sum(MaxN, na.rm = TRUE),   # Total MaxN per depth group
    .groups = "drop"  # Ungroup the data after summarizing
  )


depth_summary <- depth_summary %>%
  mutate(
    mean_MaxN_per_station = total_MaxN_depth / num_stations_depth
  )


ggplot(depth_summary, aes(x = depth_group, y = Species, fill = mean_MaxN_per_station)) +
  geom_tile(color = "white") +  # Add white borders between the tiles
  scale_fill_gradientn(
    colors = c("lightyellow", "orange", "red"),  # Adjust the color gradient
    values = scales::rescale(c(0, 5, 10)),  # Adjust the range for your data
    limits = c(0, 10),  # Define limits for the color scale
    name = "Mean MaxN\nper Station"  # Add a label for the fill
  ) +
  labs(
    title = "Mean Species Abundance by Depth Group (per Station)",
    x = "Depth Group",
    y = "Species"
  ) +
  theme_minimal() +  # Apply a minimal theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    axis.text.y = element_text(face = "italic")  # Make the species names italic on the y-axis
  )


#


# The same just for habitat distribution

# Summarize data for specific species grouped by dominant habitat
habitat_summary <- merged_data_U %>%
  filter(Species %in% c("morhua", "pollachius", "rupestris", "mixtus")) %>%
  group_by(Cluster_ID, Species, Dominant_habitat) %>%
  summarise(
    num_stations_habitat = n_distinct(Station_ID),     # Count of unique stations per habitat
    total_MaxN_habitat = sum(MaxN, na.rm = TRUE),      # Sum of MaxN per habitat
    .groups = "drop"
  ) %>%
  mutate(
    mean_MaxN_per_station = total_MaxN_habitat / num_stations_habitat
  )

# Create histogram plot
# Load viridis for color palette
library(viridis)
library(ggplot2)

# Faceted bar plot of mean MaxN per station by habitat and species
ggplot(habitat_summary, aes(x = Dominant_habitat, y = mean_MaxN_per_station)) +
  geom_col(fill = "grey60") +  # Set all bars to grey
  facet_wrap(~ Species, ncol = 2, scales = "free_y") +
  labs(
    title = "Mean MaxN by habitat type",
    x = "Habitat",
    y = "Mean MaxN"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "italic")
  )



# Make a boxplot mean distribution across clusters of: ATLANTIC COD, POLLOCK 
#GOLDSINNY AND MIXTUS

library(dplyr)
library(ggplot2)
library(patchwork)

# Define the species to plot
target_species_first <- c("morhua", "pollachius")  # First two species
target_species_second <- c("rupestris", "mixtus")  # Last two species

# Step 1: Prepare data for first two species (morhua, pollachius)
plot_data_first <- filtered_master %>%
  filter(Species %in% target_species_first) %>%
  group_by(Cluster_ID, Species, Station_ID) %>%
  summarise(MaxN_per_station = sum(MaxN, na.rm = TRUE), .groups = "drop")

# Step 2: Prepare data for second two species (rupestris, mixtus)
plot_data_second <- filtered_master %>%
  filter(Species %in% target_species_second) %>%
  group_by(Cluster_ID, Species, Station_ID) %>%
  summarise(MaxN_per_station = sum(MaxN, na.rm = TRUE), .groups = "drop")

# Step 3: Calculate summary statistics (Mean and 95% CI) for first two species
summary_stats_first <- plot_data_first %>%
  group_by(Cluster_ID, Species) %>%
  summarise(
    Mean = mean(MaxN_per_station),
    SD = sd(MaxN_per_station),
    N = n(),
    CI95 = 1.96 * (SD / sqrt(N)),
    .groups = "drop"
  )

# Step 4: Calculate summary statistics (Mean and 95% CI) for second two species
summary_stats_second <- plot_data_second %>%
  group_by(Cluster_ID, Species) %>%
  summarise(
    Mean = mean(MaxN_per_station),
    SD = sd(MaxN_per_station),
    N = n(),
    CI95 = 1.96 * (SD / sqrt(N)),
    .groups = "drop"
  )

# Bar Plot with Mean and CI95 Error Bars, using grey color for bars
ggplot(data = summary_stats) +
  # Bar plot for the mean, using grey for bars
  geom_bar(aes(x = Cluster_ID, y = Mean), 
           stat = "identity", position = "dodge", color = "black", fill = "grey") +
  # Error bars for CI95
  geom_errorbar(aes(x = Cluster_ID, ymin = Mean - CI95, ymax = Mean + CI95), 
                width = 0.2, position = position_dodge(0.9)) +
  facet_wrap(~ Species, scales = "free_y") +  # Facet by species
  theme_minimal() +
  labs(
    title = "Mean MaxN per Station by Cluster",
    x = "Cluster",
    y = "Mean MaxN"
  ) +
  theme(
    strip.text = element_text(face = "italic", size = 12),
    axis.text.x = element_text(angle = 0),
    plot.title = element_text(hjust = 0.5, size = 14)
  )




#### Plot of the MaxN of each selected species: Atlantic cod, Pollack, goldsinny and cuckoo

selected_species <- c("morhua", "pollachius", "rupestris", "mixtus")

# Filter for selected species
filtered_data <- merged_data_U %>%
  filter(Species %in% selected_species)

# Determine MaxN levels (include 0 to max)
max_maxn <- max(filtered_data$MaxN, na.rm = TRUE)
all_maxn_levels <- 0:max_maxn

# Count actual occurrences of each MaxN per species
counts <- filtered_data %>%
  count(Species, MaxN)

# Ensure all combinations of Species and MaxN are present (fill missing with 0)
plot_data <- expand_grid(Species = selected_species, MaxN = all_maxn_levels) %>%
  left_join(counts, by = c("Species", "MaxN")) %>%
  mutate(n = replace_na(n, 0),
         MaxN.x = factor(MaxN, levels = as.character(all_maxn_levels)),
         Species_label = paste0("italic('", Species, "')"))

# Plot with geom_col() – no binning, MaxN.x is discrete
ggplot(plot_data, aes(x = MaxN, y = n)) +
  geom_col(fill = "steelblue", color = "black") +
  facet_wrap(~ Species_label, scales = "free_y", labeller = label_parsed) +
  labs(title = "Histogram of MaxN by Species",
       x = "MaxN",
       y = "Count") +
  theme_minimal()









# Merge using Station_ID (or the correct common column name)
merged_data <- Habitat %>%
  left_join(Fish, by = "Station_ID")  # Keeps all habitat data, adds matching fish data

merged_data <- left_join(Fish, Habitat, by = c("Station_ID", "Cluster_ID"))


names(Fish)
names(Habitat)


View(merged_data)

View(merged_data)
dim(merged_data)  # Number of rows and columns
head(merged_data) # View first few rows
summary(merged_data) # Quick summary
View(merged_data)

#Stackplot with depth, fish and habitat type

library(janitor)
merged_data <- janitor::clean_names(merged_data)  # Makes column names unique

merged_long <- merged_data %>%
  select(station_id, depth, kelp,seaweed_tang, laminate, filamentous, erect_coarse_branching, erect_fine_branching, biogenic, sand_mud, gravel, cobble, boulder, bedrock) %>%  # Select only relevant columns
  pivot_longer(cols = -c(station_id, depth),  # Pivot only the habitat columns
               names_to = "Habitat",  
               values_to = "Presence") %>%
  filter(Presence == 1)  # Keep only present habitats (where Presence is 1)

summary_data <- merged_long %>%
  group_by(depth, Habitat) %>%
  summarise(Station_Count = n(), .groups = "drop")

View(merged_long)




#

#I want to update the ggplot of "Habitats and family" by
#using max 100% calculate the percentage of each type of habitat to 
#have a better representation of habitat coverage on each cluster

merged_data_U %>%
  count(Cluster_ID, Lat, Long) %>%
  filter(n > 1)  # Identifies duplicate stations within clusters

clean_dataA <- merged_data_U %>%
  distinct(Cluster_ID, Lat, Long, Dominant_habitat, Station_ID, Family)
library(dplyr)

# Count habitat types per cluster
family_prop <- clean_dataA %>%
  filter(!is.na(Dominant_habitat), !is.na(Family)) %>%
  group_by(Dominant_habitat, Family) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Dominant_habitat) %>%
  mutate(perc = n / sum(n) * 100)  # Convert to percentage

view(family_prop)


library(ggplot2)
ggplot(family_prop, aes(x = as.factor(Dominant_habitat), y = perc, fill = Family)) +
  geom_bar(stat = "identity", color = "black") +
  labs(
    x = "Dominant Habitat",
    y = "Percentage",
    fill = "Family"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(
    values = c(
      "#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", 
      "#8c564b", "#e377c2", "#7f7f7f", "#ee6", "#17becf", 
      "#aec7e8", "#ffbb78", "#98df8a", "#ff9896", "#c5b0d5", 
      "#c49c94", "#f7b6d2", "#c7c7c7"
    )
  )












#I want to calculate the percentage of each habitat from each cluster 

merged_data_U %>%
  count(Cluster_ID, Lat, Long) %>%
  filter(n > 1)  # Show stations appearing more than once

library(dplyr)

# Ensure each station is counted only once per cluster
habitat_percentages <- merged_data_U %>%
  distinct(Cluster_ID, Lat, Long, Dominant_habitat) %>%  # Remove duplicate stations
  group_by(Cluster_ID, Dominant_habitat) %>%  
  summarise(Count = n(), .groups = "drop") %>%  # Count unique stations per habitat
  group_by(Cluster_ID) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)  # Convert to percentage

# View the table
print(habitat_percentages)


merged_data_U %>%
  count(Cluster_ID)  # Total stations per cluster

duplicates <- merged_data_U %>%
  count(Cluster_ID, Lat, Long) %>%
  filter(n > 1)  # Shows stations that appear more than once

print(duplicates)


clean_stations <- merged_data_U %>%
  distinct(Cluster_ID, Lat, Long, Dominant_habitat)  # Keep only unique stations

# Check if the count now makes sense:
clean_stations %>%
  count(Cluster_ID)  # Now should match real total stations per cluster


habitat_percentages <- clean_stations %>%
  group_by(Cluster_ID, Dominant_habitat) %>%  
  summarise(Count = n(), .groups = "drop") %>%  # Count unique stations per habitat
  group_by(Cluster_ID) %>%
  mutate(Percentage = (Count / sum(Count)) * 100)  # Convert to percentage

# View the correct table
print(habitat_percentages)

str(unique_stations)  # Shows structure of the dataset



library(dplyr)
library(ggplot2)







#Make a histogram with the habitats and the number of stations that had them as dominant habitat
# Remove duplicates to ensure each station is counted only once
clean_data_habitat <- merged_data_U %>%
  distinct(Lat, Long, Dominant_habitat)  


ggplot(clean_data_habitat, aes(x = Dominant_habitat)) +
  geom_bar(fill = "darkgrey", color = "black") +  
  labs(
    title = "Distribution of Dominant Habitats", 
    x = "Dominant Habitat", 
    y = "Number of Stations"
  ) +
  theme_minimal() +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate labels for readability

library(dplyr)

# Count stations per habitat and calculate percentage
habitat_table <- merged_data_U %>%
  distinct(Lat, Long, Dominant_habitat) %>%  # Remove duplicates
  count(Dominant_habitat) %>%
  mutate(Percentage = (n / sum(n)) * 100)  # Convert to percentage

# Print the table
print(habitat_table)



library(dplyr)

library(ggplot2)
library(dplyr)


#Prøve igjen
summary_stats <- summary_stats %>%
  mutate(
    ci_upper = ifelse(is.na(ci_upper) | is.infinite(ci_upper), mean_richness, ci_upper),
    ci_lower = ifelse(is.na(ci_lower) | is.infinite(ci_lower), mean_richness, ci_lower)
  )

geom_text(aes(label = num_stations, y = pmin(ci_upper + 0.5, max(mean_richness + 2, na.rm = TRUE))), size = 5)

library(ggplot2)
library(dplyr)

# Step 1: Calculate species richness per station per habitat
species_richness <- merged_data_U %>%
  group_by(Station_ID, Dominant_habitat) %>% 
  summarise(unique_species = n_distinct(Species), .groups = "drop")

# Step 2: Calculate mean and confidence interval per habitat
summary_stats <- species_richness %>%
  group_by(Dominant_habitat) %>%
  summarise(
    mean_richness = mean(unique_species, na.rm = TRUE),
    sd_richness = sd(unique_species, na.rm = TRUE),
    se_richness = sd_richness / sqrt(n()),  # Standard Error
    ci_upper = mean_richness + 1.96 * se_richness,  # 95% CI Upper Bound
    ci_lower = mean_richness - 1.96 * se_richness,  # 95% CI Lower Bound
    num_stations = n()  # Number of stations per habitat
  )

# Fix NA or infinite values in ci_upper and ci_lower
summary_stats <- summary_stats %>%
  mutate(
    ci_upper = ifelse(is.na(ci_upper) | is.infinite(ci_upper), mean_richness, ci_upper),
    ci_lower = ifelse(is.na(ci_lower) | is.infinite(ci_lower), mean_richness, ci_lower)
  )

# Ensure Habitat is a factor
summary_stats$Dominant_habitat <- as.factor(summary_stats$Dominant_habitat)

# Step 3: Plot the histogram with confidence intervals and station count
richness_mean <- ggplot(summary_stats, aes(x = Dominant_habitat, y = mean_richness, fill = Dominant_habitat)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +  # Histogram bars
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +  # Confidence Interval
  geom_text(aes(label = paste0("n = ", num_stations), 
                y = pmin(ci_upper + 0.5, max(mean_richness + 2, na.rm = TRUE))), 
            size = 5) +  # Format labels as "n = X"
  labs(
    title = "Mean of species richness per Habitat Type",
    x = "Habitat Type",
    y = "Mean Species Richness"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    legend.position = "none"  # Remove legend
  ) +
  scale_fill_manual(values = rep("grey", length(unique(summary_stats$Dominant_habitat))))  # Grey bars
plot(richness_mean)

#
#Now the same but with shannon index 
library(dplyr)
library(tidyr)
library(vegan)
library(ggplot2)

# Step 1: Calculate Shannon index per station
shannon_station <- merged_data_U %>%
  filter(!is.na(Species), !is.na(MaxN)) %>%
  group_by(Station_ID, Species) %>%
  summarise(MaxN = sum(MaxN), .groups = "drop") %>%
  pivot_wider(names_from = Species, values_from = MaxN, values_fill = 0) %>%
  column_to_rownames("Station_ID")

shannon_values <- diversity(shannon_station, index = "shannon")

shannon_df <- data.frame(
  Station_ID = rownames(shannon_station),
  Shannon_index = shannon_values
)

# Step 2: Add habitat information
shannon_habitat <- merged_data_U %>%
  select(Station_ID, Dominant_habitat) %>%
  distinct() %>%
  left_join(shannon_df, by = "Station_ID") %>%
  filter(!is.na(Dominant_habitat), !is.na(Shannon_index))

# Step 3: Summarize per habitat
summary_shannon <- shannon_habitat %>%
  group_by(Dominant_habitat) %>%
  summarise(
    mean_shannon = mean(Shannon_index, na.rm = TRUE),
    sd_shannon = sd(Shannon_index, na.rm = TRUE),
    se_shannon = sd_shannon / sqrt(n()),
    ci_upper = mean_shannon + 1.96 * se_shannon,
    ci_lower = mean_shannon - 1.96 * se_shannon,
    num_stations = n()
  ) %>%
  mutate(
    ci_upper = ifelse(is.na(ci_upper) | is.infinite(ci_upper), mean_shannon, ci_upper),
    ci_lower = ifelse(is.na(ci_lower) | is.infinite(ci_lower), mean_shannon, ci_lower)
  )

# Step 4: Plot it
shannon_average<- ggplot(summary_shannon, aes(x = Dominant_habitat, y = mean_shannon, fill = Dominant_habitat)) +
  geom_bar(stat = "identity", color = "black", fill = "grey", width = 0.7) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
  geom_text(aes(label = paste0("n = ", num_stations),
                y = ci_upper + 0.1),  # adjust vertical offset if needed
            size = 4.5) +
  labs(
    title = "Mean Shannon Diversity Index per Habitat",
    x = "Habitat Type",
    y = "Mean Shannon Index"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )
plot(shannon_average)
# Combine both plots (richness on top, diversity below)

install.packages("patchwork")  # If not already installed
library(patchwork)

richness_mean + shannon_average + plot_layout(ncol = 1)





#table with the values of the shannon and species richness across habitats
# Step 1: Rename columns to prepare for merge
richness_table <- summary_stats %>%
  select(
    Habitat = Dominant_habitat,
    Mean_Richness = mean_richness,
    CI_Lower_Richness = ci_lower,
    CI_Upper_Richness = ci_upper,
    Stations = num_stations
  )

shannon_table <- summary_shannon %>%
  select(
    Habitat = Dominant_habitat,
    Mean_Shannon = mean_shannon,
    CI_Lower_Shannon = ci_lower,
    CI_Upper_Shannon = ci_upper
  )

# Step 2: Merge by Habitat
combined_table <- left_join(richness_table, shannon_table, by = "Habitat")

# Step 3: Round values for presentation
combined_table_clean <- combined_table %>%
  mutate(across(where(is.numeric), ~ round(., 2)))

# View it
print(combined_table_clean)
view(combined_table_clean)






#



###Table with Shannon, simpson, and species richness on each cluster
# Load required libraries
library(dplyr)
library(tidyr)
library(vegan)
library(officer)
library(flextable)

# OPTIONAL: Set your working directory where the Word file should be saved
# setwd("your/path/here")

# Step 1: Summarize species abundance per Cluster_ID and Species
species_matrix <- merged_data_U %>%
  group_by(Cluster_ID, Species) %>%
  summarise(abundance = sum(MaxN, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Species, values_from = abundance, values_fill = 0)

# Step 2: Calculate diversity metrics
diversity_table <- species_matrix %>%
  column_to_rownames("Cluster_ID") %>%
  as.matrix() %>%
  {
    tibble(
      Cluster_ID = rownames(.),
      Species_Richness = specnumber(.),
      Shannon_H = diversity(., index = "shannon") )
  }

diversity_table

# Step 3: Create Word document and add the diversity table
my_doc <- read_docx() %>%
  body_add_par("Species Diversity Indices by Cluster", style = "heading 1") %>%
  body_add_flextable(
    flextable(diversity_table) %>%
      autofit() %>%
      set_caption("Table: Species Richness, Shannon (H') and Simpson Index per Cluster")
  )

# Step 4: Save Word document
print(my_doc, target = "diversity_indices_table.docx")

# ✅ Done! Check your working directory for "diversity_indices_table.docx"




#


# Do shannon, simpson, species richness
#But by taking first the mean and decide into station number to get a 
#more proportional distribution since number of stations vary

# Load libraries
library(dplyr)
library(tidyr)
library(vegan)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(tidyr)
library(vegan)
library(ggplot2)
library(gridExtra)

# Step 1: Create species matrix
station_matrix <- merged_data_U %>%
  group_by(Cluster_ID, Station_ID, Species) %>%
  summarise(abundance = sum(MaxN, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Species, values_from = abundance, values_fill = 0)

# Step 2: Separate metadata and species counts
meta_data <- station_matrix %>% dplyr::select(Cluster_ID, Station_ID)
species_data <- station_matrix %>% dplyr::select(-Cluster_ID, -Station_ID)

# Step 3: Calculate diversity metrics (Shannon only)
diversity_station %>%
  group_by(Cluster_ID) %>%
  summarise(
    Mean_Shannon = mean(Shannon_H, na.rm = TRUE),
    Mean_Richness = mean(Species_Richness, na.rm = TRUE)
  )


# Step 4: Prepare data for Shannon plot
diversity_long <- diversity_station %>%
  pivot_longer(
    cols = Shannon_H,
    names_to = "Diversity_Index",
    values_to = "Value"
  )


# Step 1: Calculate summary statistics (mean and 95% CI)
library(dplyr)

summary_stats <- diversity_station %>%
  group_by(Cluster_ID) %>%
  summarise(
    Mean_Shannon = mean(Shannon_H, na.rm = TRUE),
    SE_Shannon = sd(Shannon_H, na.rm = TRUE) / sqrt(n()),
    Lower_CI_Shannon = Mean_Shannon - 1.96 * SE_Shannon,
    Upper_CI_Shannon = Mean_Shannon + 1.96 * SE_Shannon,
    
    Mean_Richness = mean(Species_Richness, na.rm = TRUE),
    SE_Richness = sd(Species_Richness, na.rm = TRUE) / sqrt(n()),
    Lower_CI_Richness = Mean_Richness - 1.96 * SE_Richness,
    Upper_CI_Richness = Mean_Richness + 1.96 * SE_Richness
  )


view(summary_stats)

shannon_boxplot <- ggplot(diversity_station, aes(x = Cluster_ID, y = Shannon_H, fill = Cluster_ID)) +
  geom_boxplot(outlier.shape = NA) +  # Hide outliers if needed
  stat_summary(fun = mean, geom = "point", shape = 21, size = 3, fill = "red") +
  stat_summary(
    fun.data = mean_cl_normal,
    geom = "errorbar",
    width = 0.2,
    color = "red"
  ) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Shannon Diversity by Cluster",
    x = "Cluster",
    y = "Shannon Index"
  ) +
  scale_fill_manual(values = rep("grey", length(unique(diversity_station$Cluster_ID)))) +
  theme(legend.position = "none")



richness_boxplot <- ggplot(diversity_station, aes(x = Cluster_ID, y = Species_Richness, fill = Cluster_ID)) +
  geom_boxplot(outlier.shape = NA) +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 3, fill = "red") +
  stat_summary(
    fun.data = mean_cl_normal,
    geom = "errorbar",
    width = 0.2,
    color = "red"
  ) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Species Richness by Cluster",
    x = "Cluster",
    y = "Number of Species"
  ) +
  scale_fill_manual(values = rep("grey", length(unique(diversity_station$Cluster_ID)))) +
  theme(legend.position = "none")


grid.arrange( richness_boxplot,shannon_boxplot, ncol = 2)



#
#Table of the mean and the SD

library(dplyr)

# Assuming your data is in 'diversity_station'
cluster_summary <- diversity_station %>%
  group_by(Cluster_ID) %>%
  summarise(
    n = n(),
    mean_shannon = mean(Shannon_H, na.rm = TRUE),
    sd_shannon = sd(Shannon_H, na.rm = TRUE),
    mean_richness = mean(Species_Richness, na.rm = TRUE),
    sd_richness = sd(Species_Richness, na.rm = TRUE)
  )

print(cluster_summary)





#table of the mean shannon, simpson, species richness across clusters

cluster_summary <- diversity_station %>%
  group_by(Cluster_ID) %>%
  summarise(
    Mean_Species_Richness = mean(Species_Richness, na.rm = TRUE),
    SD_Species_Richness = sd(Species_Richness, na.rm = TRUE),
    Mean_Shannon_H = mean(Shannon_H, na.rm = TRUE),
    SD_Shannon_H = sd(Shannon_H, na.rm = TRUE)
    
  )


cluster_summary_pretty <- cluster_summary %>%
  mutate(
    Species_Richness = paste0(round(Mean_Species_Richness, 2), " ± ", round(SD_Species_Richness, 2)),
    Shannon_H = paste0(round(Mean_Shannon_H, 2), " ± ", round(SD_Shannon_H, 2))
  ) %>%
  select(Cluster_ID, Species_Richness, Shannon_H)

# Now make the pretty table
tab_df(
  cluster_summary_pretty,
  title = "Diversity Metrics (Mean ± SD) by Cluster"
)








#Table of the mean shannon index and species richeness across all stations

mean_summary <- diversity_station %>%
  summarise(
    Mean_Species_Richness = mean(Species_Richness, na.rm = TRUE),
    SD_Species_Richness = sd(Species_Richness, na.rm = TRUE),
    Mean_Shannon_H = mean(Shannon_H, na.rm = TRUE),
    SD_Shannon_H = sd(Shannon_H, na.rm = TRUE)
  ) %>%
  mutate(
    Species_Richness = sprintf("%.2f ± %.2f", Mean_Species_Richness, SD_Species_Richness),
    Shannon_Index = sprintf("%.2f ± %.2f", Mean_Shannon_H, SD_Shannon_H)
  ) %>%
  select(Species_Richness, Shannon_Index)

print(mean_summary)


# Use your summary dataframe from earlier
tab_df(
  mean_summary,
  title = "Overall Mean Diversity Metrics Across Stations",
  footnote = "Values represent mean ± standard deviation for all sampled stations.",
  col.header = c("Species Richness (Mean ± SD)", "Shannon Index (Mean ± SD)")
)




#





#I want to update the ggplot of "Habitat types across clusters" by
#using max 100% calculate the percentage of each type of habitat to 
#have a better representation of habitat coverage on each cluster

library(dplyr)

# Count habitat types per cluster
habitat_prop <- clean_dataA %>%
  group_by(Cluster_ID, Dominant_habitat) %>%
  summarise(n = n()) %>%
  group_by(Cluster_ID) %>%
  mutate(perc = n / sum(n) * 100)  # Convert to percentage

view(habitat_prop)


library(ggplot2)

ggplot(habitat_prop, aes(x = as.factor(Cluster_ID), y = perc, fill = Dominant_habitat)) +
  geom_bar(stat = "identity", color = "black") +
  labs(
    title = "Relative Habitat Composition per Cluster",
    x = "Cluster ID",
    y = "Percentage",
    fill = "Dominant Habitat"
  ) +
  theme_minimal() +
  scale_fill_manual(
    values = c("lightblue", "lightgreen", "pink", "orange", "red", 
               "turquoise", "darkgreen", "purple", "blue", 
               "yellow", "magenta", "darkblue")
  )




  
  
# Histogram of the number of species
  dev.off()
  while (!is.null(dev.list())) dev.off()
  
  
  # Load necessary libraries
  library(dplyr)
  library(ggplot2)
  
  # Summarize data to get the total count of individuals per species
  species_summary <- merged_data_U %>%
    filter(!is.na(Species)) %>%  # Exclude rows where Species is NA
    mutate(Species = recode(Species, 
                            sp4 = "Unknown_species", 
                            sp5 = "Unknown_species", 
                            sp6 = "Unknown_species", 
                            sp11 = "Unknown_species", 
                            sp13 = "Unknown_species", 
                            sp2 = "Unknown_species", 
                            sp3 = "Unknown_species", 
                            sp7 = "Unknown_species", 
                            sp8 = "Unknown_species")) %>%
    group_by(Species) %>%
    summarise(total_MaxN = sum(MaxN, na.rm = TRUE))  # Sum MaxN for each species
  
  # Create the bar plot with total individuals per species, excluding NA and using grey color
  ggplot(species_summary, aes(x = Species, y = total_MaxN, fill = Species)) +
    geom_bar(stat = "identity", color = "black", fill = "grey") +  # Set fill to grey and no border
    labs(
      title = "Total Number of Individuals per Species",
      x = "Species",
      y = "Total Observations",
      fill = "Species"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "italic")) +  # x-axis labels in italic
    theme(legend.position = "none")  # Hide the legend if it's not needed
  
  
  #A table of the total individuals per species to explain the plot
  
  Unknown_species <- c("sp11", "sp13", "sp2", "sp3", "sp7", "sp8", "sp4", "sp5", "sp6")
  
  # Create species summary with grouped unknowns
  species_summary <- filtered_master %>%
    filter(!is.na(Species), !is.na(MaxN)) %>%
    mutate(Species_grouped = ifelse(Species %in% Unknown_species, "Unknown_species", Species)) %>%
    group_by(Species_grouped) %>%
    summarise(
      total_MaxN = sum(MaxN, na.rm = TRUE),
      station_count = n_distinct(Station_ID)
    ) %>%
    ungroup() %>%
    mutate(percentage = round(100 * total_MaxN / sum(total_MaxN), 2)) %>%
    arrange(desc(percentage))
  
  # View the result
  print(species_summary)
  
  
  
  
  #Make a table that shows the unknow species into family category
  #So I can include it as an appendix.
  
  # Load necessary libraries
  library(dplyr)
  
  # Recode species to 'Unknown_species' and group by family to sum MaxN
  unknown_species_family_table <- merged_data_U %>%
    mutate(Species = recode(Species, 
                            sp4 = "Unknown_species", 
                            sp5 = "Unknown_species", 
                            sp6 = "Unknown_species", 
                            sp11 = "Unknown_species", 
                            sp13 = "Unknown_species", 
                            sp2 = "Unknown_species", 
                            sp3 = "Unknown_species", 
                            sp7 = "Unknown_species", 
                            sp8 = "Unknown_species")) %>%
    filter(Species == "Unknown_species") %>%  # Only keep Unknown_species
    group_by(Family) %>%  # Group by Family
    summarise(total_MaxN = sum(MaxN, na.rm = TRUE))  # Summarize MaxN for each family
  
  
  
  
  
  
#Prøve på PERMAANOVA Fish assemblage across Habitats using abundance
  # Load the necessary library
  library(vegan)
  
  # Step 1: Clean and filter the data
  merged_data_clean <- merged_data_U %>%
    dplyr::select(Station_ID, Species, Dominant_habitat, Depth, MaxN) %>%
    filter(!is.na(Species))
  
  # Step 2: Summarize MaxN for duplicate combinations
  species_summary <- merged_data_clean %>%
    group_by(Station_ID, Dominant_habitat, Depth, Species) %>%
    summarise(MaxN = sum(MaxN, na.rm = TRUE), .groups = "drop")
  
  # Step 3: Pivot wider to make species columns
  abundance_matrix <- species_summary %>%
    pivot_wider(
      names_from = Species,
      values_from = MaxN,
      values_fill = list(MaxN = 0)
    )
  
  view(abundance_matrix)
  # Extract the relevant columns (metadata: Station_ID, Dominant_habitat, Depth)
  metadata <- abundance_matrix[, 1:3]
  
  # Species abundance data (all species columns)
  species_data <- abundance_matrix[, -(1:3)]
  
  # Step 4: Convert metadata columns to the correct data types
  metadata$Dominant_habitat <- as.factor(metadata$Dominant_habitat)
  metadata$Depth <- as.numeric(metadata$Depth)  # If Depth is a continuous variable, use as.numeric()
  
  # Step 5: Run PERMANOVA using Bray-Curtis distance
 adonis2(species_data ~ Dominant_habitat + Depth, 
                              data = metadata, 
                              method = "bray")
  
  adonis2(bray_dist ~ Dominant_habitat, data = metadata)
  adonis2(bray_dist ~ Depth, data = metadata)
  
  view(species_data)
  
  summary(permanova_result)

 
  
  
#The same just for habitat group
  library(vegan)
  
  # Step 1: Clean and filter the data
  merged_data_clean <- merged_data_U %>%
    dplyr::select(Station_ID, Species, HabitatGroup, Depth, MaxN) %>%
    filter(!is.na(Species))
  
  # Step 2: Summarize MaxN for duplicate combinations
  species_summary <- merged_data_clean %>%
    group_by(Station_ID, HabitatGroup, Depth, Species) %>%
    summarise(MaxN = sum(MaxN, na.rm = TRUE), .groups = "drop")
  
  view(species_summary)
  
  # Step 3: Pivot wider to make species columns
  abundance_matrix <- species_summary %>%
    pivot_wider(
      names_from = Species,
      values_from = MaxN,
      values_fill = list(MaxN = 0)
    )
  
  abundance_matrix
  # Extract the relevant columns (metadata: Station_ID, Dominant_habitat, Depth)
  metadata <- abundance_matrix[, 1:3]
  
  # Species abundance data (all species columns)
  species_data <- abundance_matrix[, -(1:3)]
  
  # Step 4: Convert metadata columns to the correct data types
  metadata$HabitatGroup <- as.factor(metadata$HabitatGroup)
  metadata$Depth <- as.numeric(metadata$Depth)  # If Depth is a continuous variable, use as.numeric()
  
  # Step 5: Run PERMANOVA using Bray-Curtis distance
  adonis2(species_data ~ HabitatGroup + Depth, 
                              data = metadata, 
                              method = "bray")
  
  summary(permanova_result)


  
  # Load necessary libraries
  library(vegan)
  library(ggplot2)
  library(dplyr)
  
  # Step 1: Calculate Bray-Curtis distance matrix from species data
  bray_dist <- vegdist(species_data, method = "bray")
  
  # Step 2: Perform NMDS using the Bray-Curtis distance
  nmds_result <- metaMDS(bray_dist, k = 2, trymax = 100)
  
  # Check the result
  summary(nmds_result)
  
  # Step 3: Extract NMDS scores (coordinates for the plot)
  nmds_scores <- as.data.frame(scores(nmds_result))
  
  # Step 4: Combine the NMDS scores with metadata (e.g., HabitatGroup)
  nmds_scores$Station_ID <- metadata$Station_ID
  nmds_scores$HabitatGroup <- metadata$HabitatGroup
  
  # Extract stress
  stress_val <- round(nmds_result$stress, 3)
  
  
  # Step 5: Create a ggplot for NMDS visualization
  ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2, color = HabitatGroup)) +
    geom_point(size = 3) +
    stat_ellipse(aes(group = HabitatGroup), level = 0.95) +  # Add 95% confidence ellipses
    theme_minimal() +
    labs(title = "NMDS of Fish Assemblages by Habitat Group", x = "NMDS1", y = "NMDS2") +
    scale_color_manual(values = c("red", "blue", "green")) +
    annotate("text", x = Inf, y = -Inf, label = paste("Stress =", stress_val),
             hjust = 1.1, vjust = -1.1, size = 4)# Adjust the color palette as needed
  
  
  
  
  
  
# NMDS
  # Step 1: Prepare the species data and metadata
  # Load necessary libraries
  library(vegan)
  library(ggplot2)
  library(dplyr)
  
  # Step 1: Calculate Bray-Curtis distance matrix from species data
  bray_dist <- vegdist(species_data, method = "bray")
  
  # Step 2: Perform NMDS using Bray-Curtis distance
  nmds_result <- metaMDS(bray_dist, k = 2, trymax = 100)
  
  # Check the result to ensure it converged
  summary(nmds_result)
  
  # Step 3: Extract NMDS scores (coordinates for the plot)
  nmds_scores <- as.data.frame(scores(nmds_result))
  
  # Step 4: Combine the NMDS scores with metadata (e.g., Dominant_habitat)
  nmds_scores$Station_ID <- metadata$Station_ID
  nmds_scores$Dominant_habitat <- metadata$Dominant_habitat
  
  
  custom_colors <- c(
    "#E41A1C",  # Red
    "#377EB8",  # Blue
    "#4DAF4A",  # Green
    "#984EA3",  # Purple
    "#FF7F00",  # Orange
    "#FFFF33",  # Yellow
    "#A65628",  # Brown
    "#F781BF",  # Pink
    "#999999",  # Grey
    "#66C2A5",  # Teal
    "#FC8D62",  # Coral
    "#00008B"   # Light Blue
  )
  
  
  
  
  
  
  
  # Step 5: Create a ggplot for NMDS visualization (with Dominant_habitat as color)
  ggplot(nmds_scores, aes(x = NMDS1, y = NMDS2, color = Dominant_habitat)) +
    geom_point(size = 3) +
    stat_ellipse(aes(group = Dominant_habitat), level = 0.95, size = 1) +  # Add 95% confidence ellipses
    theme_minimal() +
    labs(
      title = "NMDS of Fish Assemblages by Dominant Habitat",
      x = "NMDS1",
      y = "NMDS2"
    ) +
    scale_color_manual(values = custom_colors)  # Adjust the color palette as needed
  
  
  
  
#### I want to exclude cancridae and humurus 
  

#### Species Richness without taskekrabbe og european lobster ####

richness_results <- merged_data_U %>%
  group_by(Station_ID) %>%
  summarise(Species_Richness = n_distinct(Species), .groups = "drop")

print(richness_results)
  
#### Merge richness results into our dataframe
merged_data_Species_R <- merged_data_U %>%
  left_join(richness_results %>% filter(!is.na(Station_ID)), by = c("Station_ID"))

print(richness_results)
str(merged_data_Species_R)
print(merged_data_Species_R)


Species_R <- as.data.frame(aggregate(x = merged_data_Species_R$Species_Richness, 
                                    by = list(merged_data_Species_R$Cluster_ID, merged_data_Species_R$Station_ID, merged_data_Species_R$Dominant_habitat, merged_data_Species_R$Depth),
                                    FUN = mean, na.rm= TRUE))


colnames(Species_R)<- c("Cluster_ID", "Station_ID", "Habitat", "Depth", "Species_Richness")

str(Species_R)

merged_data_Species_R <- merged_data_U %>%
  left_join(richness_results %>% filter(!is.na(Station_ID)), by = c("Station_ID"))

print(richness_results)
str(merged_data_Species_R)
print(merged_data_Species_R)

view(Species_R)

plot(Species_R)
hist(Species_R$Species_Richness)


#Update

# Step 1: Exclude NA species when calculating species richness
richness_results <- merged_data_U %>%
  filter(!is.na(Species)) %>%  # Remove rows with NA in Species
  group_by(Station_ID) %>%
  summarise(Species_Richness = n_distinct(Species), .groups = "drop")

# Step 2: Ensure that all stations are included (even those with no species)
all_stations <- data.frame(Station_ID = unique(merged_data_U$Station_ID))

# Step 3: Left join richness_results to all_stations to include stations with no species
richness_results <- all_stations %>%
  left_join(richness_results, by = "Station_ID") %>%
  mutate(Species_Richness = ifelse(is.na(Species_Richness), 0, Species_Richness))  # Set missing Species_Richness to 0

# Check the final richness results
print(richness_results)


merged_data_Species_R <- merged_data_U %>%
  left_join(richness_results, by = "Station_ID")

# Check the merged data (especially stations with no species)
print(merged_data_Species_R)
str(merged_data_Species_R)

# Step 5: Aggregate by Cluster_ID, Station_ID, Dominant_habitat, and Depth, then calculate mean Species_Richness
Species_R <- as.data.frame(aggregate(x = merged_data_Species_R$Species_Richness, 
                                     by = list(merged_data_Species_R$Cluster_ID, 
                                               merged_data_Species_R$Station_ID, 
                                               merged_data_Species_R$Dominant_habitat, 
                                               merged_data_Species_R$Depth,
                                               merged_data_Species_R$Date,
                                               merged_data_Species_R$HabitatGroup),
                                     FUN = mean, na.rm = TRUE))

# Step 6: Rename columns for clarity
colnames(Species_R) <- c("Cluster_ID", "Station_ID", "Habitat", "Depth", "Date","Habitat_Group", "Species_Richness")

# Check the structure of the aggregated data
str(Species_R)


view(Species_R)
unique_species <- unique(merged_data_U$Species)
print(unique_species)



# model for species richness #####
glm_model1 <- glm(Species_Richness ~ Habitat + Depth + Cluster_ID, 
                 data = Species_R, 
                 family = poisson(link = "log"))

dispersion <- sum(residuals(glm_model, type = "pearson")^2) / df.residual(glm_model)
print(dispersion)

glm_model2 <- glm(Species_Richness ~ Habitat + Depth,  #DEFINITIV MED DEPTH ####
                  data = Species_R, 
                  family = poisson(link = "log"))



glmm_model <- glmmTMB(Species_Richness ~ Habitat + Depth + (1 | Cluster_ID),
                      data = Species_R,
                      family = poisson) 



AIC(glm_model1, glm_model2, glmm_model)

#          df      AIC
#glm_model1 21 674.7055
#glm_model2 13 670.3958
#glmm_model 14 672.3958

summary(glm_model3)
summary(glm_model2)

Species_R$Cluster_ID <- as.numeric(as.factor(Species_R$Cluster_ID))

rich1 <- glm(Species_Richness ~ Habitat + Date + Depth + Cluster_ID+ I(Cluster_ID^2),
            data = Species_R,
            family = poisson)

rich2 <- glm(Species_Richness~ Habitat + Depth +Cluster_ID+ I(Cluster_ID^2),
             data = Species_R,
             family = poisson)



R_TEMB <- glmmTMB(Species_Richness ~ Habitat + Date + Depth +Cluster_ID+ I(Cluster_ID^2),
                  data = Species_R,
                  family = poisson)

R_TEMB2 <- glmmTMB(Species_Richness ~ Habitat + Depth + Cluster_ID+ I(Cluster_ID^2),
                   data = Species_R,
                   family = poisson)



Species_R$Cluster_ID <- as.factor(as.numeric(Species_R$Cluster_ID))


Rvan <- glmmTMB(Species_Richness ~ Habitat + Date + Depth + (1|Cluster_ID),
               data = Species_R,
               family = poisson)

Rvan2 <- glmmTMB(Species_Richness ~ Habitat + Depth + (1|Cluster_ID),
                data = Species_R,
                family = poisson)



AIC(rich1, rich2,  R_TEMB, R_TEMB2, Rvan, Rvan2)

#      df      AIC
#rich1   28 683.4497
#rich2   15 669.5036  #####
#R_TEMB  28 683.4497
#R_TEMB2 15 669.5036
#Rvan    27 682.8306
#Rvan2   14 672.3958

summary(rich1)
summary(rich2)
summary(rich3)


summary(Rvan)
summary(Rvan2)
summary(Rvan3)
simulation_rich <- simulateResiduals(rich2)


# Plot the results
plot(simulation_rich)

summary(glm_model3)

#


#Habitat group on species richness
library(MASS)
library(glmmTMB)


gr_rich <- glm(Species_Richness ~ Habitat_Group + Depth,  ### BRUKE MED DEPTH DEFINITIV
                 data = Species_R, 
                 family = poisson(link = "log"))


summary(gr_rich2)

glmm_rich <- glmmTMB(Species_Richness ~ Habitat_Group + Depth + (1 | Cluster_ID),
                      data = Species_R,
                      family = poisson) 




AIC(gr_rich,  glmm_rich)
#         df      AIC
#gr_rich    4 664.1129
#glmm_rich  5 666.0943

Species_R$Cluster_ID <- as.numeric(as.factor(Species_R$Cluster_ID))

rich1 <- glm(Species_Richness ~ Habitat_Group + Date + Depth + Cluster_ID+ I(Cluster_ID^2),
             data = Species_R,
             family = poisson)

rich2_GR <- glm(Species_Richness~ Habitat_Group + Depth + Cluster_ID+ I(Cluster_ID^2),
             data = Species_R,
             family = poisson)


R_TEMB <- glmmTMB(Species_Richness ~ Habitat_Group + Date + Depth + Cluster_ID+ I(Cluster_ID^2),
                  data = Species_R,
                  family = poisson)

R_TEMB2 <- glmmTMB(Species_Richness ~ Habitat_Group + Depth + I(Cluster_ID^2),
                   data = Species_R,
                   family = poisson)



Species_R$Cluster_ID <- as.factor(as.numeric(Species_R$Cluster_ID))


Rvan <- glmmTMB(Species_Richness ~ Habitat_Group + Date + Depth + (1|Cluster_ID),
                data = Species_R,
                family = poisson)

Rvan2 <- glmmTMB(Species_Richness ~ Habitat_Group + Depth + (1|Cluster_ID),
                 data = Species_R,
                 family = poisson)




AIC(gr_rich, glmm_rich, rich1, rich2_GR, R_TEMB, R_TEMB2,  Rvan, Rvan2)

#df      AIC
#gr_rich    4 664.1129
#glmm_rich  5 666.0943
#rich1     19 675.6167
#rich2      6 663.1298  ### DEFINITIV
#R_TEMB    19 675.6167
#R_TEMB2    5 665.4712
#Rvan      18 674.6976
#Rvan2      5 666.0943



sim_rich <- simulateResiduals(gr_rich2)

summary(rich1)
summary(rich2)
summary(rich3)

summary(Rvan)
summary(Rvan2)
summary(Rvan3)

# Plot the results
plot(sim_rich)


summary(gr_rich2)
summary(gr_rich)


#
#### Shannon Index for modeling ####

library(vegan) 
install.packages(tibble) 
library(tibble)
library(tidyr)
library(dplyr)

# Create Species Abundance Matrix



abundance_matrix <- merged_data_U %>%
  group_by(Station_ID, Species) %>%
  summarise(Abundance = sum(MaxN, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Species, values_from = Abundance, values_fill = list(Abundance = 0)) %>%
  column_to_rownames(var = "Station_ID")

# Compute Shannon Index
shannon_index <- diversity(abundance_matrix, index = "shannon")

print(shannon_index)
# Convert to a DataFrame
shannon_df <- data.frame(Station_ID = rownames(abundance_matrix), Shannon_H = shannon_index)

print(shannon_df)

merged_Shannon <- merged_data_U %>%
  left_join(shannon_df %>% filter(!is.na(Station_ID)), by = c("Station_ID"))

print(merged_Shannon)
str(merged_Shannon)
print(merged_data_Species_R)

view(merged_data_U)
#### merged_data_U#### Housekeeping ####
merged_Shannon$Station_ID<-as.factor(merged_Shannon$Station_ID)
merged_data_U$Family <-as.factor(merged_data_U$Family)
merged_data_U$Dominant_habitat <-as.factor(merged_data_U$Dominant_habitat)
merged_data_U$Cluster_ID <-as.factor(merged_data_U$Cluster_ID)
merged_Shannon$Habitat
######

str(merged_Shannon)
Species_Shannon <- as.data.frame(aggregate(x = merged_Shannon$Shannon_H, 
                                     by = list(merged_Shannon$Cluster_ID, merged_Shannon$Station_ID, merged_Shannon$Dominant_habitat, merged_Shannon$Depth, merged_Shannon$HabitatGroup, merged_Shannon$Date),
                                     FUN = mean, na.rm= TRUE))
str(Species_Shannon)
colnames(Species_Shannon) <- c("Cluster_ID", "Station_ID", "Habitat", "Depth","Habitat_Group", "Date", "Shannon_H")


###housekeeping
Species_Shannon$Habitat <- as.factor(Species_Shannon$Habitat)
Species_Shannon$Habitat_Group <- as.factor(Species_Shannon$Habitat_Group)

par(mar=c(4,2,2,1))
plot(Species_Shannon$Shannon_H)
hist(Species_Shannon$Shannon_H)
str(Species_Shannon) 
view(Species_Shannon)


library(dplyr)

# Create a new column with the total MaxN per Station_ID
species_data <- merged_data_U %>%
  group_by(Station_ID) %>%
  mutate(Total_MaxN = sum(MaxN, na.rm = TRUE)) %>%
  ungroup()

view(species_data)

Species_data1 <- as.data.frame(aggregate(x = species_data$Total_MaxN, 
                                           by = list(species_data$Cluster_ID, species_data$Station_ID, species_data$Dominant_habitat, species_data$Depth),
                                           FUN = mean, na.rm= TRUE))
colnames(Species_data1) <- c("Cluster_ID", "Station_ID", "Habitat", "Depth","TotalMaxN")
str(Species_data1)

view(Species_data1)

view(Species_Shannon)

#### NORMAL GLM = GAUSSIAN, Shannon Index#### 
library(MASS)
install.packages("pscl")
library(pscl)
#Det mangler en cluster: A inn i modellen

reinstall(TMB)

Species_Shannon$Cluster_ID <- as.factor(as.numeric(Species_Shannon$Cluster_ID))



shapiro.test(Species_Shannon$Shannon_H)



modelShannon <- glm(Shannon_H ~ Depth + Habitat ,  ###DEFINITv
                 data = Species_Shannon, 
                 family = gaussian())








modelShannon_habitat <- glm(Shannon_H ~ Habitat, 
                    data = Species_Shannon, 
                    family = gaussian())

modelShannon_habitat <- glm(Shannon_H ~ Habitat, 
                            data = Species_Shannon, 
                            family = gaussian())


modelShannon_habitat_depth <- glm(Shannon_H ~ Habitat*Depth, 
                            data = Species_Shannon, 
                            family = gaussian())


summary(modelShannon_habitat_depth)
xxxxxx #mye bedre med bare habitat, AIC: 189.58 ### men om jeg tar -1 så er alt significant


AIC(modelShannon, modelShannon_habitat_depth)

#                    df      AIC
#modelShannon         14 197.5283  
#modelShannon_cluster 22 201.2415


summary(modelShannon_habitat)
summary(modelShannon)

class(modelShannon_habitat)
confint(modelShannon_habitat)
library(broom.mixed)

tidy(modelShannon_habitat, conf.int = TRUE)


Shannon_dispersion <- sum(residuals(modelShannon_cluster, type = "pearson")^2) / 
  modelShannon_cluster$df.residual
print(Shannon_dispersion) #0.17 dispersion (underdispersed?)



#
view(Species_Shannon)

#Trying a more complex modell of modelShannon


modelShannon2 <- glm(Shannon_H ~ Depth + I(Depth^2) + Habitat 
                      + Depth:Habitat, 
                      data = Species_Shannon, 
                      family = gaussian())

pR2(modelShannon2)["McFadden"]  # 0.21 R-square






summary(modelShannon)
summary(modelShannonCluster)

plot(modelShannon)
hist(modelShannon)
str(merged_data_Species_R$Species_Richness)

hist(Species_Shannon$Shannon_H, breaks = 20, col = "lightblue", main = "Histogram of Shannon Index")


#

# quadratic cluster

Species_Shannon$Cluster_ID <- as.numeric(as.factor(Species_Shannon$Cluster_ID))

S_TE <- glm(Shannon_H ~ Habitat + Date + Depth + Cluster_ID + I(Cluster_ID^2),
        data = Species_Shannon,
        family = gaussian)

S_TE2 <- glm(Shannon_H ~ Habitat + Depth + Cluster_ID + I(Cluster_ID^2),
        data = Species_Shannon,
        family = gaussian)  ### Definitiv model



S_TEMB <- glmmTMB(Shannon_H ~ Habitat + Date + Depth + Cluster_ID+ I(Cluster_ID^2),
            data = Species_Shannon,
            family = gaussian)

S_TEMB2 <- glmmTMB(Shannon_H ~ Habitat + Depth + Cluster_ID + I(Cluster_ID^2),
             data = Species_Shannon,
             family = gaussian)

Species_Shannon$Cluster_ID <- as.factor(as.numeric(Species_Shannon$Cluster_ID))

van <- glmmTMB(Shannon_H ~ Habitat + Date + Depth + (1|Cluster_ID),
                      data = Species_Shannon,
                      family = gaussian)

van2 <- glmmTMB(Shannon_H ~ Habitat + Depth + (1|Cluster_ID),
                   data = Species_Shannon,
                   family = gaussian)





AIC(S_TE, S_TE2, S_TEMB, S_TEMB2, van, van2)

#       df      AIC
#S_TE    29 208.4358
#S_TE2   16 196.6288 #### DEFINITIV MODEL
#S_TEMB  29 208.4358
#S_TEMB2 16 196.6288
#van     28 208.2760
#van2    15 199.5283

summary(S_TE)
summary(S_TE2)
summary(S_TE3)

summary(van)
summary(van2)
summary(van3)

# 


#Do the same just for HabitatGroup with Shannon Index ####





modelShannon_Group2 <- glm(Shannon_H ~ Habitat_Group + Depth, #187.43
                          data = Species_Shannon, 
                          family = gaussian())





summary(modelShannon_Group2)  #AIC 200
summary(modelShannonRandomGroup)
AIC(modelShannon_Group, modelShannon_Group2)

Species_Shannon$Cluster_ID <- as.numeric(as.factor(Species_Shannon$Cluster_ID))

S_TE_gr <- glm(Shannon_H ~ Habitat_Group + Date + Depth + Cluster_ID +  I(Cluster_ID^2),
            data = Species_Shannon,
            family = gaussian)

S_TE2_gr <- glm(Shannon_H ~ Habitat_Group + Depth + Cluster_ID + I(Cluster_ID^2),
             data = Species_Shannon,
             family = gaussian)


S_TEMB_gr <- glmmTMB(Shannon_H ~ Habitat_Group + Date + Depth + Cluster_ID+ I(Cluster_ID^2),
                  data = Species_Shannon,
                  family = gaussian)

S_TEMB2_gr <- glmmTMB(Shannon_H ~ Habitat_Group + Depth + Cluster_ID + I(Cluster_ID^2),
                   data = Species_Shannon,
                   family = gaussian)



Species_Shannon$Cluster_ID <- as.factor(as.numeric(Species_Shannon$Cluster_ID))

van_gr <- glmmTMB(Shannon_H ~ Habitat_Group + Date + Depth + (1|Cluster_ID),
               data = Species_Shannon,
               family = gaussian)

van2_gr <- glmmTMB(Shannon_H ~ Habitat_Group + Depth + (1|Cluster_ID),
                data = Species_Shannon,
                family = gaussian)



#df      AIC
#S_TE_gr    20 208.2489
#S_TE2_gr    7 199.0097 #### DEFINITIV MODEL
#S_TEMB_gr  19 207.3634
#S_TEMB2_gr  6 201.0675
#S_TEMB3_gr  5 199.0743
#van_gr     19 207.4456
#van2_gr     6 201.9153





AIC(S_TE_gr, S_TE2_gr, S_TEMB_gr, S_TEMB2_gr, S_TEMB3_gr, van_gr, van2_gr)

summary(S_TE_gr)
summary(S_TE2_gr)
summary(S_TE3_gr)
summary(van_gr)
summary(van2_gr)
summary(van3_gr)

summary(modelShannon_Group)

#### Use DHARMA to check if the model suits

install.packages("DHARMa")

# Load the DHARMa package
library(DHARMa)

# Perform DHARMa residual diagnostic
simulation_output <- simulateResiduals(modelShannon)
Simulation_output_shannon_group <- simulateResiduals(modelShannon_Group)
simulation_output_shannon_nb <- simulateResiduals(modelShannon_nb)


# Plot the results
plot(simulation_output)
plot(Simulation_output_shannon_group)
plot(simulation_output_shannon_nb)



# Run tests to check if the model fits well
testResiduals(simulation_output)



#### Prøver på anova for å se på om det er noe forskjell mellom Cluster ID ####

anova_model <- aov(Shannon_H ~ Cluster_ID, data = merged_Shannon)
summary(anova_model)




#### Table of chosen model : modelShannon ####

install.packages("sjPlot")
library(sjPlot)

# Create a nice table
tab_model( S_TEMB2, rich2, show.ci = TRUE, show.aic = TRUE,
          dv.labels = c("Shannon Index (Gaussion with quadratic Cluster) ", "Species richness (Poisson with quadratic Cluster)"))



#

#table for shannon and habitat group

# Create a nice table
tab_model(
  S_TEMB2_gr, rich2_GR,  
  show.ci = TRUE, 
  show.aic = TRUE,
  dv.labels = c("Shannon Index (Gaussion with quadratic Cluster)", "Species richness (Poisson with quadratic Cluster)")
)



#### Boxplot of Shannon H ####
ggplot(Species_Shannon, aes(x = as.factor(Habitat), y = Shannon_H)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Observed Shannon Index by Dominant Habitat",
       x = "Dominant Habitat",
       y = "Shannon Index") +
  theme_minimal()

# Add prediction as data
Species_Shannon$predicted <- predict(modelShannon, type = "response")

#Visualize the modelShannon on a plot ####
ggplot(Species_Shannon, aes(x = as.factor(Habitat), y = predicted)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(title = "Predicted Shannon Index by Dominant Habitat (GLM)",
       x = "Dominant Habitat",
       y = "Predicted Shannon Index") +
  theme_minimal()


#

#Make a plot of the linear relationship of Cluster  

S_TEMB2_linear <- glmmTMB(Shannon_H ~ Habitat + Depth + Cluster_ID,
                   data = Species_Shannon,
                   family = gaussian)


library(glmmTMB)
library(ggplot2)
library(ggeffects)

pred_cluster <- ggpredict(S_TEMB2_linear, terms = "Cluster_ID [all]")  # [all] uses all observed values

# 2. Plot the predictions
ggplot(pred_cluster, aes(x = x, y = predicted)) +
  geom_line(color = "#0072B2", size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#0072B2") +
  labs(
    x = "Cluster (south to north)",
    y = "Predicted Shannon diversity",
    title = "Linear Effect of Cluster on Fish Abundance"
  ) +
  theme_minimal()




# Species Richness


rich2_linear <- glm(Species_Richness~ Habitat + Depth +Cluster_ID,
             data = Species_R,
             family = poisson)



pred_cluster <- ggpredict(rich2_linear, terms = "Cluster_ID [all]")  # [all] uses all observed values

# 2. Plot the predictions
ggplot(pred_cluster, aes(x = x, y = predicted)) +
  geom_line(color = "#0072B2", size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#0072B2") +
  labs(
    x = "Cluster (south to north)",
    y = "Predicted Species Richness",
    title = "Linear Effect of Cluster on Fish Abundance"
  ) +
  theme_minimal()










#### MODELING OF UNIQUE SPECIES: COD, RUPESTRIS, POLLIACHIUS, MIXTUS #####

library(dplyr)

# Rydd opp Station_ID i merged_data_U
merged_data_U$Station_ID <- trimws(as.character(merged_data_U$Station_ID))
merged_data_U <- merged_data_U[!is.na(merged_data_U$Station_ID) & merged_data_U$Station_ID != "", ]



# 3. Lag stasjonsinfo (unik per stasjon)
station_info <- merged_data_U %>%
  distinct(Station_ID, Cluster_ID, Date, Lat, Long, Depth, Dominant_habitat, HabitatGroup)

# 4. Lag liste over alle stasjoner og arter (uten NA i Species her!)
species_list <- unique(na.omit(merged_data_U$Species))
station_list <- unique(station_info$Station_ID)

full_grid <- expand.grid(Species = species_list, Station_ID = station_list, stringsAsFactors = FALSE)

# 5. Slå sammen med stasjonsmetadata
full_data <- full_grid %>%
  left_join(station_info, by = "Station_ID")

# 6. Aggreger MaxN per art og stasjon (fjern NA i Species her!)
agg_data <- merged_data_U %>%
  filter(!is.na(Species)) %>%
  group_by(Species, Station_ID) %>%
  summarise(MaxN = max(MaxN, na.rm = TRUE), .groups = "drop")

# 7. Slå sammen alt (full_grid + observasjoner)
aggregated_Species <- full_data %>%
  left_join(agg_data, by = c("Species", "Station_ID"))

# 8. Arter ikke observert = MaxN = 0
aggregated_Species$MaxN[is.na(aggregated_Species$MaxN)] <- 0

# 9. Legg til binær tilstedeværelse
aggregated_Species <- aggregated_Species %>%
  mutate(Present = ifelse(MaxN > 0, 1, 0))

# 10. Rydd kolonnenavn
colnames(aggregated_Species) <- c("Species", "Station_ID", "Cluster", "Date", "Lat", "Long", "Depth", 
                                  "Habitat", "Habitat_Group", "MaxN", "Present")

# 11. Vis resultat
head(aggregated_Species)

view(aggregated_Species)

view(aggregated_Species)






#Make a histogram with the MazN values of cod, pollack, goldsinny and cuckoo

selected_species <- c("morhua", "pollachius", "rupestris", "mixtus")

# Filter the data
filtered_data <- aggregated_Species %>%
  filter(Species %in% selected_species)

# Count occurrences of each MaxN per species
counts <- filtered_data %>%
  count(Species, MaxN)

# Ensure all combinations are present (to show empty bars)
max_maxn <- max(filtered_data$MaxN, na.rm = TRUE)
plot_data <- expand_grid(Species = selected_species, MaxN = 0:max_maxn) %>%
  left_join(counts, by = c("Species", "MaxN")) %>%
  mutate(
    n = replace_na(n, 0),
    MaxN = factor(MaxN, levels = as.character(0:max_maxn)),
    Species_label = paste0("italic('", Species, "')")
  )


ggplot(plot_data, aes(x = MaxN, y = n)) +
  geom_col(fill = "steelblue", color = "black") +
  facet_wrap(~ Species_label, scales = "free_y", labeller = label_parsed) +
  labs(title = "Histogram of MaxN by Species",
       x = "MaxN",
       y = "Count") +
  theme_minimal()






#####morhua, pollachius, rupestris, mixtus aggregated with 0's

morhua2 <- aggregated_Species[aggregated_Species$Species == "morhua", ]
pollachius2 <- aggregated_Species[aggregated_Species$Species == "pollachius", ]
rupestris2 <- aggregated_Species[aggregated_Species$Species == "rupestris", ]
mixtus2 <- aggregated_Species[aggregated_Species$Species == "mixtus", ]


view(morhua2)
view(pollachius2)



###### Data exploration #####
# Replace NA in Depth with 25
morhua2$Depth[is.na(morhua2$Depth)] <- 12
pollachius2$Depth <- as.numeric(as.character(pollachius2$Depth))
pollachius2$Depth[is.na(pollachius2$Depth)] <- 12
rupestris2$Depth[is.na(rupestris2$Depth)] <- 12
mixtus2$Depth[is.na(mixtus2$Depth)] <- 12


hist(morhua2$MaxN,
     breaks = seq(from = min(morhua2$MaxN), 
                  to = max(morhua2$MaxN) + 1, 
                  by = 1),
     right = FALSE,
     main = "Histogram of MaxN",
     xlab = "MaxN (counts)",
     col = "lightblue",
     border = "white")

hist(morhua2$Depth,
     breaks = seq(from = min(morhua2$Depth), 
                  to = max(morhua2$Depth) + 1, 
                  by = 1),
     right = FALSE,
     main = "Histogram of depth",
     xlab = "depth (counts)",
     col = "lightblue",
     border = "white")

view(morhua2)


hist(pollachius2$MaxN,
     breaks = seq(from = min(pollachius2$MaxN), 
                  to = max(pollachius2$MaxN) + 1, 
                  by = 1),
     right = FALSE,
     main = "Histogram of MaxN",
     xlab = "MaxN (counts)",
     col = "lightblue",
     border = "white")

hist(pollachius2$Depth,
     breaks = seq(from = min(pollachius2$Depth), 
                  to = max(pollachius2$Depth) + 1, 
                  by = 1),
     right = FALSE,
     main = "Histogram of depth",
     xlab = "depth (counts)",
     col = "lightblue",
     border = "white")

view(pollachius2)



hist(rupestris2$MaxN,
     breaks = seq(from = min(rupestris2$MaxN), 
                  to = max(rupestris2$MaxN) + 1, 
                  by = 1),
     right = FALSE,
     main = "Histogram of MaxN",
     xlab = "MaxN (counts)",
     col = "lightblue",
     border = "white")

hist(rupestris2$Depth,
     breaks = seq(from = min(rupestris2$Depth), 
                  to = max(rupestris2$Depth) + 1, 
                  by = 1),
     right = FALSE,
     main = "Histogram of depth",
     xlab = "depth (counts)",
     col = "lightblue",
     border = "white")

view(rupestris2)

hist(mixtus2$MaxN,
     breaks = seq(from = min(mixtus2$MaxN), 
                  to = max(mixtus2$MaxN) + 1, 
                  by = 1),
     right = FALSE,
     main = "Histogram of MaxN",
     xlab = "MaxN (counts)",
     col = "lightblue",
     border = "white")

hist(mixtus2$Depth,
     breaks = seq(from = min(mixtus2$Depth), 
                  to = max(mixtus2$Depth) + 1, 
                  by = 1),
     right = FALSE,
     main = "Histogram of depth",
     xlab = "depth (counts)",
     col = "lightblue",
     border = "white")





hist(mixtus2$MaxN)    #zero-inflated 

boxplot(morhua2$MaxN)
boxplot(pollachius2$MaxN)
boxplot(rupestris2$MaxN)
boxplot(mixtus2$MaxN)


#outliers

library(performance)
check_outliers(morhua2, method = "zscore_robust", ID = "MaxN")

plot(morhua2$MaxN)
plot(pollachius2$MaxN)
plot(rupestris2$MaxN)
plot(mixtus2$MaxN)

#check how it looksa 

# Cleveland Dot Plot
dotchart(morhua2$MaxN, main = "Cleveland Dot Plot", xlab = "Value", ylab = "Index")

dotchart(pollachius2$MaxN, main = "Cleveland Dot Plot", xlab = "Value", ylab = "Index")

dotchart(rupestris2$MaxN, main = "Cleveland Dot Plot", xlab = "Value", ylab = "Index")

dotchart(mixtus2$MaxN, main = "Cleveland Dot Plot", xlab = "Value", ylab = "Index")

#Pairs

# Assign colors to habitats
colors <- as.factor(morhua2$Habitat)
colors <- as.factor(morhua2$Cluster)
palette <- rainbow(length(levels(colors)))

colors <- as.factor(pollachius2$Habitat)
colors <- as.factor(pollachius2$Cluster)
palette <- rainbow(length(levels(colors)))

colors <- as.factor(rupestris2$Habitat)
colors <- as.factor(rupestris2$Cluster)
palette <- rainbow(length(levels(colors)))

colors <- as.factor(mixtus2$Habitat)
colors <- as.factor(mixtus2$Cluster)
palette <- rainbow(length(levels(colors)))

# Select numeric columns
numeric_morhua <- morhua2[, c("MaxN", "Depth", "Habitat", "Cluster")]

# Pairs plot with colored points
pairs(numeric_morhua, 
      main = "Pairs Plot Colored by Habitat",
      pch = 21, 
      bg = palette[colors])


# Select numeric columns
numeric_pollachius <- pollachius2[, c("MaxN", "Depth", "Habitat", "Cluster")]

# Pairs plot with colored points
pairs(numeric_pollachius, 
      main = "Pairs Plot Colored by Habitat",
      pch = 21, 
      bg = palette[colors])


# Select numeric columns
numeric_rupestris <- rupestris2[, c("MaxN", "Depth", "Habitat", "Cluster")]

# Pairs plot with colored points
pairs(numeric_rupestris, 
      main = "Pairs Plot Colored by Habitat",
      pch = 21, 
      bg = palette[colors])


# Select numeric columns
numeric_mixtus <- mixtus2[, c("MaxN", "Depth", "Habitat", "Cluster")]

# Pairs plot with colored points
pairs(numeric_mixtus, 
      main = "Pairs Plot Colored by Habitat",
      pch = 21, 
      bg = palette[colors])










# QQ plot
qqnorm(morhua2$MaxN, main = "QQ Plot of MaxN")
qqline(morhua2$MaxN, col = "red")

qqnorm(morhua2$Depth)
qqline(morhua2$Depth, col = "blue")

qqnorm(pollachius2$MaxN, main = "QQ Plot of MaxN")
qqline(pollachius2$MaxN, col = "red")

qqnorm(pollachius2$Depth)
qqline(pollachius2$Depth, col = "blue")

qqnorm(rupestris2$MaxN, main = "QQ Plot of MaxN")
qqline(rupestris2$MaxN, col = "red")

qqnorm(rupestris2$Depth)
qqline(rupestris2$Depth, col = "blue")


qqnorm(mixtus2$MaxN, main = "QQ Plot of MaxN")
qqline(mixtus2$MaxN, col = "red")

qqnorm(mixtus2$Depth)
qqline(mixtus2$Depth, col = "blue")


# Shapiro-Wilk test
shapiro.test(morhua2$MaxN)




#Morhua ####

install.packages("lme4")

library(lme4)
library(MASS)
library(glmmTMB)

view(morhua2)
plot(morhua2$MaxN)

 
 
 modelMorhuaPoisson_check <- glm(MaxN ~ Habitat + Depth + Cluster, 
                           data = morhua2, 
                           family = poisson(link = "log"))

summary(modelMorhuaPoisson_check)
 
 
 
 library(dplyr)
 
morhua2_filtered_data <- morhua2 %>%
   filter(!(`Station_ID` %in% c("B22", "AA16"))) # Replace "Station_X" with the specific station name you want to exclude
 view(morhua2_filtered_data)
 
 summary(modelMorhuaPoisson)
 # Check overdispersion
 poisson_dispersion <- sum(residuals(modelMorhuaPoisson, type = "pearson")^2) / modelMorhuaPoisson$df.residual
 print(poisson_dispersion)  #0.7139 (med inkludert 0 i dataen, good for en poisson?)
 
 library(DHARMa)
 
 #sjekke begge 
 
 
 
 modelMorhuaPoisson <- glm(MaxN ~ Habitat + Depth + Cluster, 
                           data = morhua2_filtered_data, 
                           family = poisson(link = "log")) 
 
 modelMorhuaPoisson2 <- glm(MaxN ~ Habitat + Depth,     
                           data =morhua2_filtered_data, 
                           family = poisson(link = "log"))
 

 
 modelMorhuaPoisson4 <- glm(MaxN ~ Habitat, 
                           data = morhua2_filtered_data, 
                           family = poisson(link = "log"))
 

 
 AIC(modelMorhuaPoisson2,modelMorhuaPoisson4)
 AIC(model_nb, model_nb2)
 #                    df   AIC
 #modelMorhuaPoisson2 13 437.4781
 #modelMorhuaPoisson4 12 439.3886
summary(modelMorhuaPoisson2)
 table(morhua2_filtered_data$MaxN)
 
#            df      AIC
# morhua_nb  14 433.4095
# morhua_nb2 15 429.1802
 
 
 summary(modelMorhuaPoisson2)
 
 summary(modelMorhuaPoisson4)
 
 table(morhua2_filtered_data$MaxN)
 
 modelMorhuaPoissonglmTMB <- glmmTMB(MaxN ~ Habitat + Cluster+ Depth , 
                           data = morhua2_filtered_data, 
                           family = (poisson))
 
 summary(modelMorhuaPoissonglmTMB)
 summary(glmmMorhuaGau)
 library(sjPlot)
 plot_model(glmmMorhuaGau, type="pred", terms=c("Depth", "Habitat"), show.data=T)
 
 glmmMorhuaGau <- glmmTMB(MaxN ~ Depth + Habitat + (1|Cluster), 
                          data = morhua2_filtered_data, 
                          family = (poisson)) 
 
 glmmMorhuaGau2 <- glmmTMB(MaxN ~ Habitat + (1|Cluster), 
                           data = morhua2_filtered_data, 
                           family = (poisson))
 
 glmmMorhuaGau3 <- glmmTMB(MaxN ~ Depth + Habitat + Date + (1|Cluster), 
                          data = morhua2_filtered_data, 
                          family = (poisson)) 
 
 summary(glmmMorhuaGau)
 summary(glmmMorhuaGau2)
 summary(glmmMorhuaGau3)
 AIC(glmmMorhuaGau,glmmMorhuaGau2, glmmMorhuaGau3)
 
 #              df      AIC
 #glmmMorhuaGau  14 427.1802
 #glmmMorhuaGau2 13 431.4095
 #glmmMorhuagau3  31 431.8891
 
 summary(glmmMorhuaGau)
 summary(glmmMorhuaGau2)
 AIC(glmmMorhuaGau,glmmMorhuaGau2)
 
 #              df      AIC
 #glmmMorhuaGau  14 427.1802
 #glmmMorhuaGau2 13 431.4095
 
 
 library(DHARMa)
 # Perform DHARMa residual diagnostic
 simulation_output_Morhua <- simulateResiduals(glmmMorhuaGau)
 simulation_output_Morhua2 <- simulateResiduals(glmmMorhuaGau2)
 sim_morhua <- simulateResiduals(modelMorhuaPoisson2)
 sim_morhua <- simulateResiduals(model_nb)
 sim_nb <- simulateResiduals(morhua_nb2)
 # Plot the results
 plot(simulation_output_Morhua)
 plot(simulation_output_Morhua2)
 plot(sim_nb)
 plot(sim_morhua)  
 plot(sim_nb)
 
 #
 
 #Sjekke tydeligere om cluster og dag har en sammenheng, muligens bedre AIC?
 
 morhua2_filtered_data$Cluster <- as.numeric(as.factor(morhua2_filtered_data$Cluster))

glmmTE <- glmmTMB(MaxN ~ Depth + Habitat + Date + Cluster + I(Cluster^2) , 
                    data = morhua2_filtered_data, 
                    family = (poisson)) 

glmmTE2 <- glmmTMB(MaxN ~ Habitat + Depth + Cluster + I(Cluster^2) , 
                     data = morhua2_filtered_data, 
                     family = (poisson)) 


summary(glmmTE2)


AIC(glmmTE, glmmTE2)
#       df      AIC
#glmmTE  28 425.1767
#glmmTE2 15 420.5037 DEFINITIV MODEL

summary(glmmTE5)

sim_test5 <- simulateResiduals(glmmTE2)
# Plot the results
plot(sim_test5)


summary(glmmTE)
summary(glmmTE2)
summary(glmmTE3)


#


#check the curve of Cluster

# Get predictions from the model
morhua2_filtered_data$predicted <- predict(glmmTE2, type = "response")

# Plot with smooth curve fit
ggplot(morhua2_filtered_data, aes(x = Cluster, y = MaxN)) +
  geom_point() +  # observed data
  geom_smooth(method = "loess", color = "blue", se = FALSE) +  # smooth curve (loess)
  theme_minimal() +
  labs(title = "Smooth Fit: Relationship between Cluster and Abundance (MaxN)",
       x = "Cluster (South to North)",
       y = "Abundance (MaxN)") +
  theme(plot.title = element_text(hjust = 0.5))  # Center the title







 #Include habitat group 
library(glmmTMB)
 
 View(morhua2_filtered_data)
 morhua2_filtered_data$Cluster <- as.factor(as.numeric(morhua2_filtered_data$Cluster))
 
 morhua_gr <- glm(MaxN ~ Habitat_Group + Depth, 
                                         data = morhua2_filtered_data, 
                                         family = poisson(link = "log"))
 
 
 morhua_gr2 <- glm(MaxN ~ Habitat_Group,  
                  data = morhua2_filtered_data, 
                  family = poisson(link = "log"))
 
 
 
 glmmMorhuaGau_Gr <- glmmTMB(MaxN ~ Depth + Habitat_Group + (1|Cluster),  ### DEFINITIV MODEL
                          data = morhua2_filtered_data, 
                          family = (poisson)) 
 summary(glmmMorhuaGau_Gr)
 
 glmmMorhuaGau_Gr2 <- glmmTMB(MaxN ~ Habitat_Group + (1|Cluster), 
                           data = morhua2_filtered_data, 
                           family = (poisson))
 
 
 morhua2_filtered_data$Cluster <- as.numeric(as.factor(morhua2_filtered_data$Cluster))
 
 glmmTE_gr <- glmmTMB(MaxN ~ Depth + Habitat_Group + Date + Cluster + I(Cluster^2) , 
                   data = morhua2_filtered_data, 
                   family = (poisson)) 
 
 glmmTE2_gr <- glmmTMB(MaxN ~ Habitat_Group + Cluster + I(Cluster^2) , 
                    data = morhua2_filtered_data, 
                    family = (poisson)) 
 
 glmmTE3_gr <- glmmTMB(MaxN ~ Habitat_Group + Cluster + Depth + I(Cluster^2) , 
                    data = morhua2_filtered_data, 
                    family = (poisson))
 
 glmmTE3_gr <- glmmTMB(MaxN ~ Habitat_Group + I(Cluster^2) , 
                       data = morhua2_filtered_data, 
                       family = (poisson))
 

 
AIC(glmmMorhuaGau_Gr, glmmMorhuaGau_Gr2,morhua_gr, morhua_gr2, glmmTE_gr, glmmTE2_gr, glmmTE3_gr)

#                  df      AIC
#glmmMorhuaGau_Gr   5 417.1399
#glmmMorhuaGau_Gr2  4 424.7851
#morhua_gr          4 426.5419
#morhua_gr2         3 431.5125
#glmmTE_gr         19 417.1238
#glmmTE2_gr         5 424.6208
#glmmTE3_gr         4 424.010           

summary(glmmTE_gr)

# Perform DHARMa residual diagnostic
library(DHARMa)
simulation_output_Morhua_gr <- simulateResiduals(glmmTE2_gr)


# Plot the results
plot(simulation_output_Morhua_gr)

summary(glmmTE_gr)

#

#I want to see the quadratic effect if U-shaped or Inverted U-shape










#pollachius #####
install.packages("lme4")

library(lme4)

install.packages("lme4")


modelPollachiusPoisson <- glm(MaxN ~ Habitat + Depth + Cluster, #bruke dette som utgangspunkt for å sjekke outliers og dermed overdispersion
                          data = pollachius2, 
                          family = poisson(link = "log")) 


summary(modelPollachiusPoisson)
plot(modelPollachiusPoisson)


AIC(modelPollachiusPoisson,modelPollachiusPoisson2, modelPollachiusPoisson3, modelPollachiusPoisson4, modelPollachiusPoisson5)
summary(modelPollachiusPoisson4)

plot(modelPollachiusPoisson4) #Her er det residual som overvekter hele datasettet

view(pollachius2)



pollachius2_filtered_data <- pollachius2 %>%
  filter(!(`Station_ID` %in% c("B12"))) # Replace "Station_X" with the specific station name you want to exclude


modelPollachiusPoisson_filter <- glm(MaxN ~ Habitat + Depth + Cluster, 
                              data = pollachius2_filtered_data, 
                              family = poisson(link = "log")) # etterpå sjekke for overdispersion


poisson_dispersion2 <- sum(residuals(modelPollachiusPoisson_filter, type = "pearson")^2) / 
  modelPollachiusPoisson_filter$df.residual
print(poisson_dispersion2)  #0.85 poisson skal gå fint
plot(modelPollachiusPoisson_filter)

modelPollachiusPoisson2_filter <- glm(MaxN ~ Habitat + Depth, 
                               data = pollachius2_filtered_data, 
                               family = poisson(link = "log")) 


modelPollachiusPoisson4_filter <- glm(MaxN ~ Habitat, 
                               data = pollachius2_filtered_data, 
                               family = poisson(link = "log"))




AIC(modelPollachiusPoisson2_filter, modelPollachiusPoisson4_filter)
summary(modelPollachiusPoisson_filter)
plot(modelPollachiusPoisson5_filter)

#                               df      AIC
#modelPollachiusPoisson2_filter 13 274.5493
#modelPollachiusPoisson4_filter 12 276.1003

summary(modelPollachiusPoisson2_filter)

summary(modelPollachiusPoisson2_filter)
summary(modelPollachiusPoisson4_filter)




pollachius2_filtered_data$Cluster <- as.factor(as.numeric(pollachius2_filtered_data$Cluster))

glmmpol1 <- glmmTMB(MaxN ~ Depth + Habitat + (1|Cluster), 
                         data = pollachius2_filtered_data, 
                         family = (poisson)) 

glmmpol2 <- glmmTMB(MaxN ~ Habitat + (1|Cluster), 
                          data = pollachius2_filtered_data, 
                          family = (poisson))

glmmpol3 <- glmmTMB(MaxN ~ Cluster + (1|Habitat), 
                          data = pollachius2_filtered_data, 
                          family = (poisson))


AIC(glmmpol1,glmmpol2, glmmpol3)

#df      AIC
#glmmpol1 14 273.8542  
#glmmpol2 13 274.7870
#glmmpol3 10 277.2046

summary(glmmpol1)
summary(glmmpol2)

# Load the DHARMa package
library(DHARMa)

# Perform DHARMa residual diagnostic
Pollachius_poisson <- simulateResiduals(glmmpol1)
plot(Pollachius_poisson)

pollachius2_filtered_data$Cluster <- as.numeric(as.factor(pollachius2_filtered_data$Cluster))

glmmTE_pol <- glmmTMB(MaxN ~ Depth + Habitat + Date + Cluster+ I(Cluster^2) , 
                  data = pollachius2_filtered_data, 
                  family = (poisson)) 

glmmTE2_pol <- glmmTMB(MaxN ~ Habitat + Depth + Cluster+ I(Cluster^2) , 
                   data = pollachius2_filtered_data, 
                   family = (poisson)) 

glmmTE3_pol <- glmmTMB(MaxN ~ Habitat + I(Cluster^2), 
                   data = pollachius2_filtered_data, 
                   family = (poisson))

AIC(glmmTE_pol, glmmTE2_pol, glmmTE3_pol)
#           df      AIC
#glmmTE_pol  27 283.2108
#glmmTE2_pol 15 267.5758  #### MODEL
#glmmTE3_pol 13 269.4055


summary(glmmTE2_pol)

#DHARMA

te_pol<- simulateResiduals(glmmTE2_pol)
plot(te_pol)

summary(glmmTE_pol)
Summary(glmmTE2_pol)
summary(glmmTE3_pol)



#include habitat group


glmmpol1_Gr <- glmmTMB(MaxN ~ Depth + Habitat_Group + (1|Cluster), 
                    data = pollachius2_filtered_data, 
                    family = (poisson)) 



modelPollachius_gr <- glm(MaxN ~ Habitat_Group + Depth, 
                                      data = pollachius2_filtered_data, 
                                      family = poisson(link = "log")) 





pollachius2_filtered_data$Cluster <- as.numeric(as.factor(pollachius2_filtered_data$Cluster))

glmmTE_pol_gr <- glmmTMB(MaxN ~ Depth + Habitat_Group + Date + Cluster + I(Cluster^2) , 
                      data = pollachius2_filtered_data, 
                      family = (poisson)) 

glmmTE2_pol_gr <- glmmTMB(MaxN ~ Habitat_Group + Depth + Cluster + I(Cluster^2) , 
                       data = pollachius2_filtered_data, 
                       family = (poisson)) 




AIC(glmmpol1_Gr,glmmpol2_Gr, modelPollachius_gr, modelPollachius_gr2, glmmTE_pol_gr, glmmTE2_pol_gr)


#                   df      AIC
#glmmpol1_Gr          5 274.6256
#glmmpol2_Gr          4 274.4711
#modelPollachius_gr   4 273.2033
#modelPollachius_gr2  3 273.3663
#glmmTE_pol_gr       18 285.4436
#glmmTE2_pol_gr       5 268.9187   ##### DEFINITIV MODEL
  

summary(glmmTE3_pol_gr)
# Perform DHARMa residual diagnostic
Pollachius_poisson_gr <- simulateResiduals(glmmTE3_pol_gr)
plot(Pollachius_poisson_gr)



#


#check shape of quadratic cluster


# Get predictions from the model
pollachius2_filtered_data$predicted <- predict(glmmTE2_pol, type = "response")

# Plot with smooth curve fit
ggplot(pollachius2_filtered_data, aes(x = Cluster, y = MaxN)) +
  geom_point() +  # observed data
  geom_smooth(method = "loess", color = "blue", se = FALSE) +  # smooth curve (loess)
  theme_minimal() +
  labs(title = "Smooth Fit: Relationship between Cluster and Abundance (MaxN)",
       x = "Cluster (South to North)",
       y = "Abundance (MaxN)") +
  theme(plot.title = element_text(hjust = 0.5))  # Center the title








#Rupestris #####



modelrupestrisPoisson <- glm(MaxN ~ Depth + Habitat + Cluster, 
                              data = rupestris2, 
                              family = poisson(link = "log"))
plot(modelrupestrisPoisson) # har en outlier BB26
view(rupestris2)

rupestris2_filtered_data <- rupestris2 %>%
  filter(!(`Station_ID` %in% c("BB26"))) 
                       
# 

modelrupestrisPoisson_filter <- glm(MaxN ~ Depth + Habitat + Cluster, 
                             data = rupestris2_filtered_data, 
                             family = poisson(link = "log"))

plot(modelrupestrisPoisson_filter)

poisson_dispersion3 <- sum(residuals(modelrupestrisPoisson_filter, type = "pearson")^2) / 
  modelrupestrisPoisson_filter$df.residual
print(poisson_dispersion3)  # ~1.36 = overdispersion dispersed , Negative Binomial


modelrupestrisPoisson2_filter <- glm(MaxN ~ Habitat + Depth, 
                                     data = rupestris2_filtered_data, 
                                     family = poisson(link = "log"))


modelrupestrisPoisson3_filter <- glm(MaxN ~ Habitat + Cluster, 
                                      data = rupestris2_filtered_data, 
                                      family = poisson(link = "log"))

modelrupestrisPoisson4_filter <- glm(MaxN ~ Habitat, 
                                      data = rupestris2_filtered_data, 
                                      family = poisson(link = "log"))


AIC(modelrupestrisPoisson_filter,modelrupestrisPoisson2_filter,
    modelrupestrisPoisson3_filter, modelrupestrisPoisson4_filter,
    modelrupestrisPoisson5_filter) 

                            # df      AIC
#modelrupestrisPoisson2_filter 13 582.9720
#modelrupestrisPoisson4_filter 12 593.5238


summary(modelrupestrisPoisson2_filter)
plot(modelrupestrisPoisson4_filter)


#Overdispersion model: Negative binomial 
install.packages("MASS")  # Install if not already installed
library(MASS)

nb_model1 <- glm.nb(MaxN ~ Depth + Habitat + Cluster, data = rupestris2_filtered_data)

nb_model2 <- glm.nb(MaxN ~ Depth + Habitat, data = rupestris2_filtered_data)

nb_model3 <- glm.nb(MaxN ~ Cluster + Habitat, data = rupestris2_filtered_data)

nb_model4 <- glm.nb(MaxN ~ Habitat, data = rupestris2_filtered_data)

nb_model5 <- glm.nb(MaxN ~ Cluster, data = rupestris2_filtered_data)


AIC(nb_model1, nb_model2, nb_model3, nb_model4, nb_model5)

#         df      AIC
#nb_model1 22 591.0040
#nb_model2 14 582.7326 ######
#nb_model3 21 599.5571
#nb_model4 13 592.2100
#nb_model5 10 664.9647
summary(nb_model2)
summary(nb_model4)

#med quadratic cluster og date 

rupestris2_filtered_data$Cluster <- as.numeric(as.factor(rupestris2_filtered_data$Cluster))

qu_ru1 <- glmmTMB(MaxN ~ Depth + Habitat + Date + Cluster+ I(Cluster^2) , 
                         data = rupestris2_filtered_data, 
                         family = (nbinom2)) 

qu_ru2 <- glmmTMB(MaxN ~ Habitat + Depth + Cluster +  I(Cluster^2) , 
                          data = rupestris2_filtered_data, 
                          family = (nbinom2)) 

qu_ru3 <- glmmTMB(MaxN ~ Habitat + I(Cluster^2) , 
                          data = rupestris2_filtered_data, 
                          family = (nbinom2))

AIC(qu_ru1, qu_ru2, qu_ru3)
summary(qu_ru1)
summary(qu_ru2)
summary(qu_ru3)



summary(nb_model2)
#glmmTMB with negative binomial 
install.packages("glmmTMB")  # Install if not already installed
library(glmmTMB)
rupestris2_filtered_data$Cluster <- as.factor(as.numeric(rupestris2_filtered_data$Cluster))

nb_glmm1 <- glmmTMB(MaxN ~ Depth + Habitat + (1 | Cluster),
                    data = rupestris2_filtered_data,
                    family = nbinom2())

nb_glmm2 <- glmmTMB(MaxN ~ Habitat + (1 | Cluster),
                    data = rupestris2_filtered_data,
                    family = nbinom2())

nb_glmm3 <- glmmTMB(MaxN ~ Habitat + (1 | Depth),
             data = rupestris2_filtered_data,
             family = nbinom2())

AIC(nb_glmm1, nb_glmm2, nb_glmm3)
#       df      AIC
#nb_glmm1 15 584.7326
#nb_glmm2 14 594.2100
#nb_glmm3 14 594.2100

summary(nb_glmm1)
summary(nb_glmm2)


# Load the DHARMa package
library(DHARMa)

# Perform DHARMa residual diagnostic
rupestris_poisson <- simulateResiduals(qu_ru2)
plot(rupestris_poisson)









#

#Include habitat group
library(MASS)

nb_Gr <- glm.nb(MaxN ~ Depth + Habitat_Group, data = rupestris2_filtered_data)

nb_Gr2 <- glm.nb(MaxN ~ Habitat_Group, data = rupestris2_filtered_data)

nb_glmmGr <- glmmTMB(MaxN ~ Depth + Habitat_Group + (1 | Cluster),
                    data = rupestris2_filtered_data,
                    family = nbinom2())

nb_glmm2_Gr <- glmmTMB(MaxN ~ Habitat_Group + (1 | Cluster),
                    data = rupestris2_filtered_data,
                    family = nbinom2())

nb_glmm3_Gr <- glmmTMB(MaxN ~ Habitat_Group + (1 | Depth),
                    data = rupestris2_filtered_data,
                  family = nbinom2())


rupestris2_filtered_data$Cluster <- as.numeric(as.factor(rupestris2_filtered_data$Cluster))

qu_ru1_gr <- glmmTMB(MaxN ~ Depth + Habitat_Group + Date + Cluster + I(Cluster^2) , 
                  data = rupestris2_filtered_data, 
                  family = (nbinom2)) 

qu_ru2_gr <- glmmTMB(MaxN ~ Habitat_Group + Depth + Cluster + I(Cluster^2) , 
                  data = rupestris2_filtered_data, 
                  family = (nbinom2)) 


summary(qu_ru2_gr)


AIC(nb_Gr, nb_Gr2, nb_glmmGr, nb_glmm2_Gr, nb_glmm3_Gr, qu_ru1_gr, qu_ru2_gr, qu_ru3_gr)


#           df      AIC
#nb_Gr        5 571.5807
#nb_Gr2       4 582.5376
#nb_glmmGr    6 573.5807
#nb_glmm2_Gr  5 584.5376
#nb_glmm3_Gr  5 584.5152
#qu_ru1_gr   20 588.7615
#qu_ru2_gr    7 570.2906    ###### DEFINITIV
#qu_ru3_gr    5 584.2450

# Perform DHARMa residual diagnostic
rupestris_Gr <- simulateResiduals(nb_Gr)
plot(rupestris_Gr)





# Mixtus #####
view(mixtus2)

modelmixtusPoisson <- glm(MaxN ~ Depth + Habitat + Cluster, 
                             data = mixtus2, 
                             family = poisson(link = "log"))

poisson_dispersionmix <- sum(residuals(modelmixtusPoisson, type = "pearson")^2) / 
  modelmixtusPoisson$df.residual
print(poisson_dispersionmix) # 0.768 dispersion, kjøre precense absence, lage to grupper der 0 er ikke present of alle tall er da present som vil si 1
# forklare at at det var mange nuller så valgte jeg å gjøre det om 0-1 for en binomial analyse
plot(modelmixtusPoisson)
View(mixtus2)

mixtus2_filtered_data <- mixtus2 %>%
  filter(`Station_ID` != "C19") 

modelmixtusPoisson_filter <- glm(MaxN ~ Depth + Habitat + Cluster, 
                          data = mixtus2_filtered_data, 
                          family = poisson(link = "log"))

poisson_dispersion4 <- sum(residuals(modelmixtusPoisson_filter, type = "pearson")^2) / 
  modelmixtusPoisson_filter$df.residual
print(poisson_dispersion4)  # ~0.7712377 okey for poisson? however, many 0's i dataen, 


view(mixtus2_filtered_data)


var_ratio <- var(mixtus2_filtered_data$MaxN) / mean(mixtus2_filtered_data$MaxN)
print(var_ratio)




# Install and load necessary libraries
install.packages("glmmTMB")
library(glmmTMB)


#
view(mixtus2_filtered_data)

#using present instead of MaxN due to a lot of 0, I don't exclude the outliers, and see

mixtus2_filtered_data$Cluster <- as.factor(as.numeric(mixtus2_filtered_data$Cluster))

Present1 <- glm(Present ~ Habitat + Depth, 
                                  data = mixtus2, 
                                  family = binomial())


Present2 <- glm(Present ~ Habitat + Cluster, 
                                  data = mixtus2, 
                                  family = binomial())

Present3 <- glm(Present ~ Habitat, 
                                  data = mixtus2, 
                                  family = binomial())

mixtus2$Cluster <- as.numeric(as.factor(mixtus2$Cluster))

qu_gr1 <- glmmTMB(Present ~ Depth + Habitat + Date + Cluster + I(Cluster^2) , 
                   data = mixtus2, 
                   family = (binomial)) 

qu_gr2 <- glmmTMB(Present ~ Habitat + Depth + Cluster +  I(Cluster^2) , 
                   data = mixtus2, 
                   family = (binomial)) 



AIC(Present1, Present2, Present3, qu_gr1,qu_gr2)

#        df      AIC
#Present1 13 185.4903  ##DEFINITIv MODEL
#Present2 13 187.9713
#Present3 12 186.9961
#qu_gr1   31 209.1453
#qu_gr2   14 187.4393
#qu_gr3   13 188.5255

summary(Present1)
summary(Present3)
summary(qu_gr1)
summary(qu_gr2)
summary(qu_gr3)
# Perform DHARMa residual diagnostic
mixtus_present <- simulateResiduals(Present1)
plot(mixtus_present)

mixtus2_filtered_data$Cluster <- as.factor(as.numeric(mixtus2_filtered_data$Cluster))

glmmMIX <- glmmTMB(Present ~ Habitat + Depth, 
                   data = mixtus2, 
                   family = (nbinom2))

glmmMIX2 <- glmmTMB(Present ~ Depth + Habitat + (1|Cluster), 
                    data = mixtus2, 
                    family = (nbinom2)) 

glmmMIX3 <- glmmTMB(Present ~ Habitat + (1|Cluster), 
                    data = mixtus2, 
                    family = (nbinom2))



AIC(glmmMIX, glmmMIX2,glmmMIX3)

#df      AIC
#glmmMIX  14 217.7785
#glmmMIX2 15 219.7785
#glmmMIX3 14 220.2003

summary(glmmMIX3)

mixtus_mix <- simulateResiduals(glmmMIX)
plot(mixtus_mix)



summary(modelmixtusPoisson5_filter)
plot(modelmixtusPoisson5_filter)

#Dharma

mixtus_mix <- simulateResiduals(glmmmix)
plot(mixtus_mix)

#

#check habitat groups aswell

Mixtus_pr <- glm(Present ~ Habitat_Group + Depth, 
                                  data = mixtus2, 
                                  family = (binomial))


Mixtus_pr2 <- glm(Present ~ Habitat_Group, 
                data = mixtus2, 
                family =(binomial))



Mixtus_pr3 <- glmmTMB(Present ~ Habitat_Group + (1|Cluster), 
                    data = mixtus2, 
                    family = (binomial))

Mixtus_pr4 <- glmmTMB(Present ~ Habitat_Group + Depth + (1|Cluster), 
                      data = mixtus2_filtered_data, 
                      family = (binomial))


mixtus2_filtered_data$Cluster <- as.numeric(as.factor(mixtus2_filtered_data$Cluster))

qu_pr1 <- glmmTMB(Present ~ Depth + Habitat_Group + Date + Cluster +  I(Cluster^2) , 
                  data = mixtus2, 
                  family = (binomial)) 

qu_pr2 <- glmmTMB(Present ~ Habitat_Group + Depth + Cluster + I(Cluster^2) , 
                  data = mixtus2, 
                  family = (binomial)) 

qu_pr3 <- glmmTMB(Present ~ Habitat_Group + Cluster + I(Cluster^2) , 
                  data = mixtus2, 
                  family = (binomial))



AIC(Mixtus_pr, Mixtus_pr2, Mixtus_pr3, Mixtus_pr4, qu_pr1, qu_pr2, qu_pr3)
AIC(qu_pr1, qu_pr2, qu_pr3)


#           df      AIC
#Mixtus_pr   4 188.9203
#Mixtus_pr2  3 188.3908   
#Mixtus_pr3  4 190.3908
#Mixtus_pr4  5 189.5254
#f      AIC
#qu_pr1 19 208.0206
#qu_pr2  6 187.6915    #### gruppe
#qu_pr3  5 189.7141

summary(Mixtus_gr2)
summary(Mixtus_pr)

# Perform DHARMa residual diagnostic

library(DHARMa)
mixtus_Gruppe <- simulateResiduals(Mixtus_pr2)
plot(mixtus_Gruppe)


mixtus_pr_plot <- simulateResiduals(Mixtus_pr)
plot(mixtus_pr_plot)



#make a table of the species with habitat type ####

install.packages("sjPlot")
library(sjPlot)

tab_model(
  glmmTE2,
  glmmTE2_pol,
  nb_model2,
  Present1,
  show.ci = FALSE,
  show.p = TRUE,
  show.std = TRUE,
  dv.labels = c(
    "<i>Gadus morhua (Poisson with quadratic Cluster)</i>", 
    "<i>Pollachius pollachius (Poisson with quadratic Cluster)</i>", 
    "<i>Ctenolabrus rupestris (Negative Binomial)</i>", 
    "<i>Labrus mixtus (Binomial)</i>"
  )
)

summary(qu_ru2)
MuMIn::r.squaredGLMM(qu_ru2)
#


#make table of species model with habitat group ####




tab_model(
  glmmTE_gr,
  glmmTE2_pol_gr,
  nb_Gr,
  qu_pr2,
  show.ci = FALSE,
  show.p = TRUE,
  show.std = TRUE,
  dv.labels = c(
    "<i>Gadus morhua (Poisson with quadratic Cluster)</i>", 
    "<i>Pollachius pollachius (Poisson with quadratic Cluster)</i>", 
    "<i>Ctenolabrus rupestris (Negative Binomial) </i>", 
    "<i>Labrus mixtus (Binomial with quadratic Cluster)</i>"
  )
)



library(vegan)


view(merged_data_U)

diversity_data <- merged_data_U %>%
  group_by(Station_ID) %>%
  summarise(
    shannon = diversity(across(Species(is.numeric)), index = "shannon"),
    richness = specnumber(across(Species(is.numeric)))
  )

# Now join with relevant metadata: cluster, depth, dominant habitat, habitat group
# We'll take the first occurrence per station assuming those are constant per station

station_metadata <- merged_data_U %>%
  group_by(station) %>%
  summarise(
    cluster = first(cluster),
    depth = first(depth),
    dominant_habitat = first(`dominant habitat`),
    habitat_group = first(`habitat group`)
  )

# Merge diversity metrics with metadata
final_summary <- left_join(station_metadata, diversity_data, by = "station")
