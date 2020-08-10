############################################################

# returns bbox little larger than the data itself. works with sf objects.

add_bbox_buffer <- function(sf_object, 
                            x_buffer = 0.1,
                            y_buffer = 0.1
) {
  
  # make map boundaries little larger than data dimensions
  bbox <- st_bbox(sf_object)
  
  #box_buffer <- buffer
  
  # dimensions of data 
  height <- bbox$ymax - bbox$ymin
  width <- bbox$xmax - bbox$xmin
  
  bbox$xmin <- suppressWarnings(as.numeric(bbox$xmin - width*x_buffer))
  bbox$xmax <- suppressWarnings(as.numeric(bbox$xmax + width*x_buffer))
  bbox$ymin <- suppressWarnings(as.numeric(bbox$ymin - height*y_buffer))
  bbox$ymax <- suppressWarnings(as.numeric(bbox$ymax + height*y_buffer))
  
  bbox <- c(
    left = bbox$xmin,
    bottom = bbox$ymin,
    right = bbox$xmax,
    top = bbox$ymax
  )
  
  return(bbox)
  
}


############################################################

# fn returns coordinates for a different transformation. it convertes the dataset into a spatial object, calculates coordinates for a diff refernce system, converts these to a df, and attaches these to the original coordinates

# tutorial: https://ryanpeek.org/2017-08-03-converting-XY-data-with-sf-package/ 

add_crs <- function(
  dt,
  original_crs, original_coords,
  new_crs, new_coord_names = c("long", "lat") 
) {
  
  library(sf)
  
  #convert flat file to spatial file, give it the original CRS
  dt_sp <- st_as_sf(dt, coords = original_coords, crs = original_crs)
  
  #convert to different CRS, and save the coordinates
  new_coords <- st_transform(dt_sp, crs = new_crs) %>%
    st_coordinates() %>%
    as.data.frame() %>%
    rename(new_x = X, 
           new_y = Y) %>%
    mutate(new_crs = new_crs)  
  
  #rename new columns
  names(new_coords) <- c(new_coord_names, paste0(c(new_coord_names, "crs"), collapse = "_"))
  
  # add new coords to original dt
  dt2 <- cbind(dt, new_coords)
  
  
  return(dt2)
  
############################################################
  
}