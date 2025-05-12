
#' Calculate Rolling Window Regression Intercepts and CIs
#'
#' Fits a rolling window linear regression and returns intercept estimates,
#' standard errors, and 95% confidence intervals. Simplified version.
#'
#' @param data A dataframe containing the response and predictor variables.
#' @param response_col_name A string specifying the name of the response variable column.
#' @param window_size An integer specifying the number of periods in the rolling window.
#' @param predictors A character vector specifying the names of the predictor variable columns.
#'        Defaults to c("def", "term").
#'
#' @return A dataframe containing the original data columns plus new columns for:
#'         - `rolling_alpha`: The rolling intercept estimate.
#'         - `rolling_alpha_se`: The standard error of the rolling intercept estimate.
#'         - `rolling_alpha_lower_ci`: The lower bound of the 95% confidence interval.
#'         - `rolling_alpha_upper_ci`: The upper bound of the 95% confidence interval.
#'         The first `window_size - 1` rows for these new columns will be NA.

roll_alpha <- function(data,
                       response_col_name,
                       window_size,
                       predictors = c("def", "term")) {
  
  # Basic check for sufficient rows
  if (nrow(data) < window_size) {
    stop("Not enough rows in 'data' for the specified 'window_size'.")
  }
  
  # --- Construct Formula ---
  formula_str <- paste(response_col_name, "~", paste(predictors, collapse = " + "))
  # Make formula available in the environment of the helper function
  formula_obj <- as.formula(formula_str)
  
  # --- Define Simplified Helper Function for runner ---
  fit_lm_extract <- function(window_df) {
    
    # Initialize result vector with 4 NAs (alpha, se, lower_ci, upper_ci)
    result <- rep(NA_real_, 4)
    names(result) <- c("alpha", "se", "lower_ci", "upper_ci")
    
    # Use try to catch any error during lm fitting or summary extraction
    # It will silently return if an error occurs, leaving result as NAs.
    try({
      # Fit the model using the formula from the parent environment
      # na.action=na.omit handles NAs within the window
      model <- lm(formula_obj, data = window_df, na.action = na.omit)
      
      # Proceed only if model seems valid and intercept exists
      # Check degrees of freedom to ensure model is not degenerate
      if (model$df.residual > 0 && "(Intercept)" %in% names(coef(model))) {
        model_summary <- summary(model)
        intercept_row <- model_summary$coefficients["(Intercept)", , drop = FALSE]
        alpha_est <- intercept_row[, "Estimate"]
        alpha_se <- intercept_row[, "Std. Error"]
        
        # Store the estimate and SE if they are finite
        if(is.finite(alpha_est)) result[1] <- alpha_est
        if(is.finite(alpha_se)) result[2] <- alpha_se
        
        # Calculate 95% CI if SE is valid and finite
        if (is.finite(alpha_se) && alpha_se > 0) {
          t_crit <- qt(0.975, df = model$df.residual) # 0.975 for 95% CI
          lower_bound <- alpha_est - t_crit * alpha_se
          upper_bound <- alpha_est + t_crit * alpha_se
          result[3] <- lower_bound
          result[4] <- upper_bound
        }
      }
    }, silent = TRUE) # silent=TRUE prevents try from printing error messages
    
    # Always return the result vector (either populated or full of NAs)
    return(result)
  }
  
  # --- Apply runner ---
  # runner applies 'fit_lm_extract_simple' to each window
  # Expects a matrix output because the helper function always returns a fixed-size vector
  rolling_results_matrix <- runner(
    x = data,                    # The input data frame
    k = window_size,             # The window size
    f = fit_lm_extract,   # The function to apply to each window
    na_pad = TRUE                # Pad beginning with NAs until window is full
  )
  
  # Create a new list to store the modified elements
  modified_results_list <- vector("list", length(rolling_results_matrix))
  
  # Iterate through the list and replace logical entries with NA vector
  for (i in seq_along(rolling_results_matrix)) {
    if (is.logical(rolling_results_matrix[[i]])) {
      # Replace logical with a numeric vector of 4 NAs
      modified_results_list[[i]] <- as.numeric(c(NA, NA, NA, NA))
    } else {
      # Keep the original element if it's not logical
      modified_results_list[[i]] <- rolling_results_matrix[[i]]
    }
  }
  
  # Coerce the modified list into a matrix
  result_matrix <- do.call(rbind, modified_results_list)
  
  
  # Assign clear column names to the matrix returned by runner
  colnames(result_matrix) <- c("alpha", "se",
                                        "low_ci", "high_ci")
  
  # Add back dates from data
  result_matrix <- as.data.frame(result_matrix)
  result_matrix$date <- data$date
  
  
  
  return(result_matrix)
}
