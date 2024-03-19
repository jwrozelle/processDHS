# points_sf <- FCdata
# raster <- malawi_2015_hfp.raster

# Function to extract sum of values from a cell and its queen's case adjacent cells
extractAdjacent <- function(points_sf, raster) {
  
  stop("This function is not yet operational")
  
  require(sf)
  require(terra)
  
  # # Convert sf points to SpatVector if not already
  points_vect <- vect(points_sf)
  
  # Extract cell numbers from raster
  cells <- extract(raster, points_vect, xy=TRUE, cells=TRUE)
  
  # Define queen's case adjacency (including the cell itself + 8 surrounding cells)
  adj_matrix <- matrix(c(0,1,1,1,0,1,1,1,1), nrow=3, byrow=TRUE)
  
  # Function to get sum of adjacent cell values
  get_adjacent_sum <- function(cell_num, raster) {
    # Get row and column of the cell
    rc <- rowColFromCell(raster, cell_num)
    row <- rc[1]
    col <- rc[2]
    
    # Define the range for queen's case
    rows_adj <- (row-1):(row+1)
    cols_adj <- (col-1):(col+1)
    
    # Clamp to raster boundaries
    rows_adj <- rows_adj[rows_adj > 0 & rows_adj <= nrow(raster)]
    cols_adj <- cols_adj[cols_adj > 0 & cols_adj <= ncol(raster)]
    
    # Extract values and sum them up
    sum_values <- sum(values(raster[rows_adj, cols_adj]), na.rm = TRUE)
    
    return(sum_values)
  }
  
  # Apply function to each point's cell
  sums <- sapply(cells$cell, get_adjacent_sum, raster=raster)
  
  # Return a vector with the sums
  return(sums)
}
