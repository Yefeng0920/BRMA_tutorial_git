#'---------------------------------------------------------------#'
#' Functions used in the methodological paper 
#' Quantitative synthesis: a practical guide on meta-analysis, meta-regression, and publication bias tests for environmental sciences
#' Oct 2022
#'---------------------------------------------------------------#'

# Editted so orchard_plot would work

############################################################################
#' @title cv_avg
#' @description Calculates the weighted average CV^2 within a study and the weighted average CV^2 across a study
cv_avg <- function(x, sd, n, group, data, label = NULL, sub_b = TRUE, cv2=FALSE){

  # Check if the name is specified or not. If not, then assign it the name of the mean, x, variable input in the function. https://stackoverflow.com/questions/60644445/converting-tidyeval-arguments-to-string
  if(is.null(label)){
    label <- purrr::map_chr(enquos(x), rlang::as_label)
  }

  # Calculate between study CV. Take weighted mean CV within study, and then take a weighted mean across studies of the within study CV. Weighted based on sample size and pooled sample size.
  b_grp_cv_data <- data                                             %>%
    dplyr::group_by({{group}})                            %>%
    dplyr::mutate(   w_CV2 = weighted_CV({{sd}}, {{x}}, {{n}}, cv2=cv2),
                     n_mean = mean({{n}}, na.rm = TRUE))   %>%
    dplyr::ungroup(.)                                     %>%
    dplyr::mutate(b_CV2 = weighted.mean(w_CV2, n_mean, na.rm = TRUE), .keep = "used")

  # Make sure that label of the calculated columns is distinct from any other columns
  names(b_grp_cv_data) <- paste0(names(b_grp_cv_data), "_", label)

  # Append these calculated columns back to the original data and return the full dataset.
  if(sub_b){
    b_grp_cv_data <- b_grp_cv_data %>% dplyr::select(grep("b_", names(b_grp_cv_data)))
    dat_new <- cbind(data, b_grp_cv_data)
  } else {
    dat_new <- cbind(data, b_grp_cv_data)
  }

  return(data.frame(dat_new))
}

############################################################################

#' @title weighted_CV
#' @description Calculates the weighted average CV^2 or CV followed by squaring within a study and the weighted averages CV^2 across a studies
weighted_CV <- function(sd, x, n, cv2=FALSE){
  if(cv2){
    weighted.mean(na_if((sd / x)^2, Inf), n, na.rm = TRUE)
  }else{
    weighted.mean(na_if((sd / x), Inf), n, na.rm = TRUE)^2
  }
}

