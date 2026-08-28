#' @title point_in_poly_stats
#' @description
#'  This function takes an sf point object, an sf polygon object, and a 
#'  fieldname from each and outputs the sf polygon object with additional fields 
#'  showing the count of the sf_points within each polygon, as well as the mean 
#'  and standard error for a specified field for a specified field
#' 
#' @param sf_points This is an sf point object
#' @param sf_points_field This is the field within \code{sf_points} for which you would like to calculate a mean and standard error (for a given polygon)
#' @param sf_polys This is an sf polygon object
#' @param sf_polys_field This is the field from \code{sf_polys} that identifies the polygons (e.g. a unique identifier for each polygon)
#'
#' @returns an sf polygon
#' @export
point_in_poly_stats <- function(sf_points, sf_points_field = NULL, sf_polys, sf_polys_field = NULL){
  w_areas <- Mar.utils::identify_area(df = sf_points, 
                                    agg.poly.shp = sf_polys, 
                                    agg.poly.field = sf_polys_field )
  if(!is.null(sf_points_field)){
  w_areas_summary <- w_areas |>
    dplyr::group_by(.data[[sf_polys_field]]) |>
    dplyr::summarise(
      Count = n(),
      Mean =   ifelse(is.nan(mean(.data[[sf_points_field]], na.rm = TRUE)), NA_real_, mean(.data[[sf_points_field]], na.rm = TRUE)),
      StdErr = ifelse(is.nan(Mar.utils::st_err(.data[[sf_points_field]], na.rm = TRUE)), NA_real_,Mar.utils::st_err(.data[[sf_points_field]], na.rm = TRUE))
    ) |>
    dplyr::ungroup() |>
    sf::st_drop_geometry()
  }else{
    w_areas_summary <- w_areas |>
      dplyr::group_by(.data[[sf_polys_field]]) |>
      dplyr::summarise(Count = n()) |>
      dplyr::ungroup() |>
      sf::st_drop_geometry()
  }

  results <- merge(sf_polys, w_areas_summary, all.x=T)
  return(results)  
}