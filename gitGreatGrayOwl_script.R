
########################################################################
# Using R Zonal Statistics, field observations, and LiDAR data to Identify Great Grey Owl Habitat
#  Jan 20, 2021
#  Julian Scott (julian.a.scott@usda.gov) & Sarah Malick Wahls (sarah.malickwahls@usda.gov)
#    
#  code description...
########################################################################
# load packages
packages <- c("raster","sp","sf","doParallel","tmap","tidyverse")

# Then, check to see if each is installed, and install if not.
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {    
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

# now, use the lapply function to load the installed libraries in the packages list 
lapply(packages,library,character.only=TRUE)

################################################################################
## Package details
################################################################################
# sp: A package for spatial data: points, lines, polygons and grids
?sp

# sf: Simple Features; More advanced and intuitive than sp, but less ubiquitous, for now. 
help(package="sf")

# raster: The raster package provides classes and functions to manipulate geographic (spatial) data in grid format.
?raster

# tidyverse: provides raft of useful packages including:
#             -ggplot2: Create Elegant Data Visualisations Using the 
#                     Grammar of Graphics (gg)
#             -dplyr: a flexible grammar of data manipulation. 
#             - lots of other useful packages
?tidyverse

# tmap: A new mapping package that is flexible and easy to use to create maps. It can access an ESRI world basemap, plus neat cartographic templates.  It is based on the grammar of graphics, and resembles the syntax of ggplot2.
?tmap

# doParallel: for multi-core, parallel computing. See the vignette("foreach") for more examples
vignette("foreach")

# Set the mapping package, tmap, to always have earth imagery as a background
# Add world imagery as default
tmap_options(basemaps = c(Imagery = "Esri.WorldImagery",
                          Canvas = "Esri.WorldGreyCanvas",
                          StreetMap = "OpenStreetMap",
                          Topo = "Esri.WorldTopoMap"))


# Working directory inherited from directory of project folder
getwd()

# Read in prepared canopy density (LiDAR) data
# First, unzip the raster data; unzipped files get placed in your working directory)
unzip(zipfile = "lidarCanopyData2.zip",
      overwrite = TRUE,exdir = "grids")

# Read each raster into your R environment 

# Create a list of the rasters in the grids directory
grid_files <- list.files(path = "./grids/",pattern = "classdns", full.names = T) 
grid_files

# Read in as a stack of rasters
canopy_stack <- stack(grid_files,RAT = F) 
# Setting the Raster Attribute Table to False keeps the data continuous, instead of categorical

# view one raster
plot(canopy_stack[[1]])
tm_shape(canopy_stack) +
  tm_raster()

# Lets make the names of the raster more intuitive and shorter
# First, view the names of each grid in the raster stack
names(canopy_stack)

# Use the substitute function to delete the characters that come before the first number in each name
my_names <- sub('classdns',"",names(canopy_stack))

# R doesn't like names that begin with a number, so I'll add on a 'CD' for canopy density as a prefix.
my_names <- paste0(c('b','a','d','c'),'_CD',my_names)
my_names

# Now set the names of the layers of the raster stack like this:
names(canopy_stack) <- my_names

# Now, observe the order of the rasters in the stack is not desirable. 
# Fix so that they run from highest canopy to the lowest
canopy_stack

# We can reorder the layers using the subset function of the raster package. 
canopy_stack <- raster::subset(canopy_stack, c("a_CD20_30","b_CD10_20","c_CD5_10","d_CD3_5"))
canopy_stack

# Plot canopy density grids
tm_shape(canopy_stack)+
  tm_raster(title = names(canopy_stack))

# If you wanted to, this is how you write and read the raster stack to disk
# writeRaster(canopy_stack, filename = "canopy_stack.tif")

# Read from disc
# canopy_stack <- stack("canopy_stack.tif")

# Get zipped up shapefile data
unzip(zipfile = "vector_data.zip",
      overwrite = TRUE,exdir = "vector")

# Now we read into the R environment the observed Great Grey Owl nest locations
nests <- sf::st_read("./vector/class_nests_2.shp")
nests

# Add an ID column. 
nests <-  mutate(nests, NestID = paste0("Nest_",dplyr::row_number()))
nests

# Lets set the default mapping to occur in 'interactive mode'
tmap::ttm()

# Plot nests with canopy density grids

tm_shape(canopy_stack,unit = "mi")+
  tm_raster()+
    tm_shape(nests)+
    tm_dots(size = 0.5)+
  tm_scale_bar(text.size = 1,breaks = c(0,1))

# Buffer points by 120 m
nests_buf <- st_buffer(nests,120)

# convert to Spatial Object
nests_buf_sp <- as(nests_buf,"Spatial")

# Plot nests with the masked canopy density grids
# First, lets update our color palette

tmaptools::palette_explorer() 
color_pal <- viridisLite::viridis(20, 
                                  begin = 0,
                                  end = 1
                                  ,direction = -1)

# Plot nests with canopy density grids
tm_shape(canopy_stack,unit = "mi") +
  tm_raster(palette = color_pal,legend.show = F) +
  tm_shape(nests_buf) +
  tm_polygons(alpha = 0) +
  tm_facets(as.layers = TRUE) +
  tm_scale_bar(text.size = 1,breaks = c(0,1))



#########################################################################################
# Extract canopy density data stored in the rasters for each buffered nest point
#########################################################################################

# Extract the actual cell values covered by each polygon 
?raster::extract
nest_cells <- raster:::extract(canopy_stack,nests_buf_sp,df = T,cellnumber = T)

# Wrangle the data into 'long' (i.e., tidy), modify the ID column, and group by nest and canopy level
nest_cells_long <- nest_cells %>% 
  pivot_longer(cols = a_CD20_30:d_CD3_5, 
               names_to = 'Canopy',
               values_to ='Density') %>% 
  mutate(NestID = paste0('Nest',ID)) %>% 
  group_by(NestID,Canopy)

# Calculate mean density by canopy level, by nest. 
method1_mean_density <- nest_cells_long %>% 
  summarise(mean_d = mean(Density))

ggplot(nest_cells_long,aes(Density)) +
  geom_histogram()+
  geom_vline(data = method1_mean_density, aes(xintercept = mean_d),linetype = 'dashed',color = 'red')+
  theme_bw()+
  facet_grid(rows = vars(Canopy),cols = vars(NestID))
  
# Compare means calculated using the two methods
# method 1: Use raster::extract to get cell values, then calculate mean using tidyverse
mean_density

# method 2: Use raster::extract and the fun argument to calculate the mean cell value directly
method2_mean_density <- raster:::extract(canopy_stack,nests_buf_sp,fun = mean,df = T)
method2_mean_density

# Wrangle the method 2 results to be eaily comparable to the method 1 results
method2_mean_density <- method2_mean_density %>% 
  pivot_longer(cols = a_CD20_30:d_CD3_5, 
               names_to = 'Canopy',
               values_to ='Density') %>% 
  mutate(NestID = paste0('Nest',ID))
  
# Now I have two vectors of means, one for each method
method1_mean_density$mean_d
method2_mean_density$Density

# Test if they are equal to each other
all.equal(method1_mean_density$mean_d,method2_mean_density$Density)


# We will use the mean canopy cover at each nest for each height as our search criteria.
# For our search, we will use the range of means among all of our nests for each canopy level.
# First convert the list into a data frame. 
# Dataframes lend themselves to carrying out numerical summaries and spreadsheet-type analysis 
method1_mean_density
mean_df <- as.data.frame(mean_cell_value_r) %>% 
  rownames_to_column("canopy")

adjust_variable <- 0

search_conditions <- method1_mean_density %>% 
  group_by(Canopy) %>% 
  summarise(min_mean = min(mean_d) - adjust_variable * min(mean_d),
            max_mean = max(mean_d) + adjust_variable * max(mean_d))

# The range of means canopy density calculated across all nest locations, by canopy level, will be our search criteria.
search_conditions

# To further constrain our search for suitable habitat, we'll mask our canopy density data by vegetation type
# using the Forest Stands layer (FSVeg). For this FSVeg dataset, we've removed all lowlands and open habitats.
vegclass <- st_read('./vector/veg_dissolve.shp')

tm_shape(vegclass)+
tm_polygons() 

# Mask the canopy data by the vegclass polygon
# Add filename to write the file to disk (and reduces RAM usage)
canopy_stack_vegmasked <- raster::mask(canopy_stack,vegclass,filename = "canopy_stack_vegmasked.tif")
names(canopy_stack_vegmasked) <- names(canopy_stack)

nest_map <- tm_shape(canopy_stack_vegmasked,unit = "mi") +
 tm_raster(palette = color_pal,legend.show = F) +
 tm_shape(nests_buf) +
 tm_polygons(lwd = 2,alpha = 0) +
 tm_facets(as.layers = TRUE) +
 tm_scale_bar(text.size = 1,breaks = c(0,1))

nest_map
# Read in owl observation point. Set buffer radius and buffer. Clip out owl buffer from canopy_stack.
# We observed a Great Grey Owl at this point
GGOobs <- st_read("./vector/owl_obs2.shp")

# Plot with nest map
nest_map +
tm_shape(GGOobs) + 
  tm_dots(size = 0.25,col = "red")
  
# To limit our search area, lets Buffer the observed owl location point by 0.75 miles.
owl_buf <- st_buffer(GGOobs,units::set_units(0.75,mi))

# Map the search area
tm_shape(owl_buf) + 
  tm_polygons(size = 1,col = "red",alpha = 0.25) +
  nest_map

# Convert the buffered owl observation point to Spatial Object
owl_buf_sp <- as(owl_buf,"Spatial")

# Lets mask the canopy data by the search area
search_stack <- raster::mask(x = canopy_stack_vegmasked, mask = owl_buf_sp) 

# remove 'white space'
search_stack <- raster::crop(search_stack,owl_buf_sp)

# Plot the search area
tm_shape(search_stack) + 
  tm_raster(palette = color_pal,legend.show = F) +
  tm_facets(as.layers = T)

# Other ways to plot
plot(canopy_stack)
plot(search_stack)


# The raster::overlay function can be used to to search for multiple conditions across all layers of a raster
# stack.

# Search search the canopy data around the observed owl location for 
# those cells that meet the  mean canopy cover conditions as defined within the 
# range identified by known nests above for all 4 height(i.e, search_conditions) 

CD2030_cnd <- filter(search_conditions,Canopy == "a_CD20_30")
CD1020_cnd <- filter(search_conditions,Canopy == "b_CD10_20")
CD510_cnd <- filter(search_conditions,Canopy == "c_CD5_10")
CD35_cnd <- filter(search_conditions,Canopy == "d_CD3_5")

# This overlay function sets the cell to '1' if all the canopy conditions are met and
# sets the cell to '0' if not.
search_results  <- overlay(x = search_stack, 
                           fun = function(a_CD20_30,b_CD10_20,c_CD5_10,d_CD3_5) {
                             if_else(between(a_CD20_30,CD2030_cnd$min_mean,CD2030_cnd$max_mean) &
                                     between(b_CD10_20,CD1020_cnd$min_mean,CD1020_cnd$max_mean) &
                                     between(c_CD5_10,CD510_cnd$min_mean,CD510_cnd$max_mean) &
                                     between(d_CD3_5,CD35_cnd$min_mean,CD35_cnd$max_mean),1,0)
                           }
)

# Plot results
tm_shape(search_results) + 
  tm_raster(style = "cat") +
  tm_shape(nests_buf)+
  tm_polygons(lwd = 2, border.col = 'red',alpha = 0)

# We see clusters of individual '1s'. 
# One way generalize the 'best' spots could be to take a local mean around each cell.
# This takes the mean of the 25 surrounding cells, in a 5x5 square.
# The cell size is 20 meters, so this is in effect a 100 m square moving average

search_results25 <- raster::focal(search_results,matrix(1/25,nc = 5, nr = 5))

# Plot results
tm_shape(search_results25) + 
  tm_raster(style = "cat") +
  tm_shape(nests_buf)+
  tm_polygons(lwd = 2, border.col = 'red',alpha = 0)

# Save grid to disk
writeRaster(search_results25, "GreayGrayOwl_nest_index", format="GTiff")

