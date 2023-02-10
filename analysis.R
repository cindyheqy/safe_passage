###################################################
### EPSA Workshop                               ###
### Working with Spatial Data                   ###
### Austin Wright, The University of Chicago    ###
###################################################

library(sf)
library(tidyverse)

###########################
######### Overview ########
###########################

# Today we'll be covering the basics of spatial data using R's simple features (sf). The sf help manual can be found here: https://r-spatial.github.io/sf/ and is very useful, please read it!

# 1. Loading and plotting shapefiles
# 2. Attaching more data
# 3. Spatial joins
# 4. Better map plots
# 5. Other geometric operations
# 6. Mapping with Census data

##### Loading and Plotting Shapefiles #####

# Most vector spatial data still comes in the shapefile format. Shapefiles are separated into multiple files in a directory. All the components of the shapefile (.shp, .dbf, .proj) must be in the same directory to be read.

# Unzipping the Chicago shapefiles
  unzip("chicago_tracts_2010.zip") # if needed

# Reading in the shapefile. Note the geometry column, this is what stores the coordinates that make up a shape
  chicago_gdf <- st_read("chicago_tracts_2010.shp")
    glimpse(chicago_gdf)

# plot() will always plot every column unless you tell it a specific one
  plot(chicago_gdf)

##### Attaching More Data #####

# Generally speaking, you will need to join interesting data TO a shapefile in order to create meaningful plots. We can accomplish this using the simple joins (left_join, right_join, etc...) that we learned earlier.

# Loading some more interesting data on Chicago incomes
  chicago_income <- read_csv("chicago_tracts_income.csv")
    glimpse(chicago_income)

# Columns of data imported with sf sometimes start as factors, we should convert them to characters
  chicago_gdf <- chicago_gdf %>%
    mutate(
      geoid = as.character(geoid),
      commarea = as.character(commarea)
    )

# Same for the income data imported with read_csv. Must use characters so that leading zeroes are not dropped. as.numeric() will drop leading zeroes
  chicago_income <- chicago_income %>%
    mutate(geoid = as.character(geoid))

# Geospatial dataframes are just like normal ones. They can be filtered, joined, and mutated, but ONLY st_ functions will work on the geometry column.
  chicago_gdf <- left_join(chicago_gdf, chicago_income, by = "geoid")
    glimpse(chicago_gdf)

# Plot the new columns added to our spatial data frame
  plot(chicago_gdf["total"])
  plot(chicago_gdf[4])                  #what variable is this?
  plot(chicago_gdf["per_under_25k"])

## Plot them side-by-side for comparison, they're very different maps
  plot(chicago_gdf[c("total", "per_under_25k")])

# Now let's read in some point data, note that this isn't a spatial data frame
  chicago_crime <- read_csv("chicago_crimes_2013.csv")

# We need to convert the latitude and longitude here into a geometry column. Latitude and longitude almost always use the CRS 4326
  
chicago_crime <- subset(chicago_crime, !is.na(chicago_crime$Longitude)&chicago_crime$`Primary Type`=="ASSAULT")
    
chicago_crime_sf <- st_as_sf(
  chicago_crime,
  coords = c("Longitude", "Latitude"),
  crs = 4326)
glimpse(chicago_crime_sf)

#Don't plot the whole dataframe, it may take a long time
chicago_crime_sf_sample <- chicago_crime_sf %>%
  sample_frac(0.1)

#Plotting features
  plot(chicago_crime_sf_sample["Primary Type"])

  chicago_crime_sf_sample$event <- 1
  plot(chicago_crime_sf_sample["event"])

  chicago_crime_sf_sample %>%
    ggplot() +
    geom_sf(aes(color = event)) 
  
#Safe passages program
    chicago_poly_gdf <- st_read("spp_poly.shp")
      glimpse(chicago_poly_gdf)  
      
    chicago_poly_gdf$buffer <- 1
      
  joint_plot <- ggplot() +
    geom_sf(data = chicago_gdf) +
    geom_sf(data = chicago_crime_sf_sample, aes(color =  event)) +
    geom_sf(data = chicago_poly_gdf, aes(color =  buffer, fill = 'red')) +
    theme(legend.position = "none")
  
  joint_plot 
    
  joint_plot_simple <- ggplot() +
    geom_sf(data = chicago_gdf, aes(fill = per_under_25k)) +
    #scale_fill_viridis_c() +
    #geom_sf(data = chicago_crime_sf_sample, aes(color =  event)) +
    geom_sf(data = chicago_poly_gdf, aes(color =  buffer), fill = 'red') +
    guides(color = FALSE) + 
    scale_fill_gradient(low="#006400", high="#FFFF00", name="Percentage of \nlow-income residents") +
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(), 
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank()) +
    ggtitle("The Chicago Public Schools Safe Passage Program ")
    #theme(legend.position = "none")

  joint_plot_simple
  
  