library(ggplot2)
library(raster)

path <- "/Users/macbookairdemilo/Desktop/SenegalMODIS_NDVI"
files <- list.files(path, pattern = ".tif$", full.names = TRUE)

# Create a list of rasters
raster_list <- lapply(files, raster)

# Stack the rasters
raster_stack <- stack(raster_list)

# Calculate maximum NDVI for each raster
max_ndvi_list <- cellStats(raster_stack, 'max')

# Calculate minimum NDVI for each raster
min_ndvi_list <- cellStats(raster_stack, 'min')

# Create a data frame for plotting
plot_data <- data.frame(File = basename(files), Max_NDVI = max_ndvi_list, Min_NDVI = min_ndvi_list)

# Melt the data frame for plotting both max and min NDVI
library(reshape2)
plot_data_melted <- melt(plot_data, id.vars = "File", measure.vars = c("Max_NDVI", "Min_NDVI"))

# Plot
ggplot(plot_data_melted, aes(x = File, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Date", y = "NDVI", title = "Maximum and Minimum NDVI at each date") +
  scale_fill_manual(values = c("skyblue", "salmon"), labels = c("Max NDVI", "Min NDVI"))





# Assuming plot_data is already available from previous code

# Define threshold NDVI value (you need to adjust this based on your data)
threshold_ndvi <- 0.4  # Example threshold, adjust as needed

# Detect start of flood
start_flood <- which(plot_data$Min_NDVI < threshold_ndvi)[1]

# Detect end of flood
end_flood <- start_flood + which(plot_data$Min_NDVI[start_flood:length(plot_data$Min_NDVI)] > threshold_ndvi)[1]

# Calculate duration of flood
flood_duration <- end_flood - start_flood

# Output results
start_date <- plot_data$File[start_flood]
end_date <- plot_data$File[end_flood]
cat("Flood starts at:", start_date, "\n")
cat("Flood ends at:", end_date, "\n")
cat("Flood duration (in days):", flood_duration, "\n")
