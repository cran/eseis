#' Clip values of spatial data.
#' 
#' The function replaces raster values based on different thresholds.
#' 
#' @param data \code{SpatRaster} object, spatial data set to be processed.
#' 
#' @param quantile \code{Numeric} value, quantile value below which raster 
#' values are clipped.
#' 
#' @param replace \code{Numeric} value, replacement value, default is
#' \code{NA}.
#' 
#' @param normalise \code{Logical} value, optionally normalise values above 
#' threshold quantile between 0 and 1. Default is \code{TRUE}. 
#' 
#' @return \code{SpatRaster} object, data set with clipped values.
#' 
#' @author Michael Dietze
#' @keywords eseis
#' @examples
#' 
#' ## load example data set
#' data(volcano)
#' 
#' ## convert matrix to raster object
#' volcano <- terra::rast(volcano)
#' 
#' ## clip values to those > quantile 0.5
#' volcano_clip <- spatial_clip(data = volcano, 
#'                              quantile = 0.5)
#'                                     
#' ## plot clipped data set
#' terra::plot(volcano_clip)
#'                      
#' @export spatial_clip
#' 
spatial_clip <- function(
  data,
  quantile,
  replace = NA,
  normalise = TRUE
) {
  
  ## check/set parameters
  if(missing(quantile) == TRUE) {
    
    quantile <- 1
  }
  
  ## replace values
  terra::values(data)[terra::values(data) < quantile(terra::values(data), 
                                                     probs = quantile, 
                                                     na.rm = TRUE)] <- replace
  
  ## optionally normaise data set
  if(normalise == TRUE) {
    
    terra::values(data) <- (terra::values(data) - 
                              min(terra::values(data), na.rm = TRUE)) / 
      (max(terra::values(data), na.rm = TRUE) - 
         min(terra::values(data), na.rm = TRUE))
  }
  
  ## return data set
  return(data)
}