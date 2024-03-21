library(ggplot2)
library(raster)

path <- "/Users/macbookairdemilo/Desktop/SenegalMODIS_NDVI"
files <- list.files(path, pattern = ".tif$", full.names = TRUE)

# Create a list of rasters
raster_list <- lapply(files, raster)

# Stack the rasters
raster_stack <- stack(raster_list)

# Calculate average NDVI for each raster
avg_ndvi_list <- cellStats(raster_stack, 'mean')

# Create a data frame for plotting
plot_data <- data.frame(File = basename(files), Avg_NDVI = avg_ndvi_list)

# Plot
ggplot(plot_data, aes(x = File, y = Avg_NDVI)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Date", y = "Average NDVI", title = "Average NDVI at each date")



# Assuming plot_data is already available from previous code

# Extract day of the year from file names
day_of_year <- as.numeric(sub("NDVI2010", "", substr(plot_data$File, 1, 10)))

# Sort plot_data based on day_of_year
plot_data <- plot_data[order(day_of_year), ]

# Define threshold NDVI value (you need to adjust this based on your data)
threshold_ndvi <- 0.25  # Example threshold, adjust as needed

# Detect start of the flood
start_flood <- which(plot_data$Avg_NDVI > threshold_ndvi)[1]

# Detect end of flood
end_flood <- start_flood + which(plot_data$Avg_NDVI[start_flood:length(plot_data$Avg_NDVI)] < threshold_ndvi)[1]

# Calculate duration of flood
flood_duration <- end_flood - start_flood

# Output results
start_date <- plot_data$File[start_flood]
end_date <- plot_data$File[end_flood]
cat("Flood starts at:", start_date, "\n")
cat("Flood ends at:", end_date, "\n")
cat("Flood duration (in days):", flood_duration, "\n")



