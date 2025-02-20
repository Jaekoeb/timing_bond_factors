

#' Sort Bonds into Portfolios based on Signal
#'
#' @param data bond data, containing the signals as columns
#' @param signal name of the signal column
#' @param ret_col name of return column (duration matched possible)
#' @param quantile number of quantiles to create when sorting
#'
#' @return list with two dataframes
#' @export
#'
#' @examples
portfolio_sort <- function(data, signal, ret_col, quantile = 3) {
  
  
  # Rename the specified columns for ease of use
  data <- data |>
    rename(signal = {{ signal }},
           return = {{ ret_col }}) |>
    filter(!is.na(signal),
           !is.na(return)) |> 
    select(
      eom,
      cusip,
      return,
      signal,
      market_value,
      yield,
      duration,
      rating_group
    )
  
  
  # In each month and rating, sort by signal
  data <- data |> 
    group_by(eom, rating_group) |> 
    mutate(
      portfolio = ntile(signal, quantile)
    ) |> 
    ungroup()
  
  
  # Compute the value weighted return within each rating group
  portfolios <- data |> 
    group_by(eom, rating_group, portfolio) |> 
    summarise(
      return = weighted.mean(return, market_value),
      yield = weighted.mean(yield, market_value, na.rm = TRUE),
      duration = weighted.mean(duration, market_value, na.rm = TRUE),
      market_value = mean(market_value)
    ) |> 
    ungroup()
  
  
  # Aggregate among rating group
  portfolios <- portfolios |> 
    group_by(eom, portfolio) |> 
    summarise(
      return = mean(return),
      market_value = mean(market_value),
      yield = mean(yield),
      duration = mean(duration)
    ) |> 
    ungroup()
  
  
  # add long short portfolio
  ls <- inner_join(
    x = portfolios |> filter(portfolio == 1),
    y = portfolios |> filter(portfolio == quantile),
    join_by(eom == eom)
  )
  
  
  ls <- ls |> 
    mutate(
      portfolio = "ls",
      return = return.y - return.x,
      market_value = (market_value.x + market_value.y) / 2,
      yield = (yield.x + yield.y) / 2,
      duration = duration.y - duration.x
    ) |> 
    select(
      eom,
      portfolio,
      return,
      market_value,
      yield,
      duration
    )
  
  
  # Join observations
  portfolios <- rbind(portfolios, ls)
  portfolios <- portfolios |> 
    arrange(eom, portfolio)
  
  
  # For data keep only portfolio information
  data <- data |> 
    rename(
      {{signal}} := portfolio
    ) |> 
    arrange(eom, cusip) |> 
    select(eom, cusip, {{signal}})
  
  
  return(list(data = data, portfolios = portfolios))
}


