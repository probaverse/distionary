# Agent Guidelines for R Code Quality

This document provides guidelines for maintaining high-quality R code. These rules MUST be followed by all AI coding agents and contributors.

## Your Core Principles

All code you write MUST be optimized.

"optimized" includes:

- following proper style conventions for the code language (e.g. maximizing code reuse (DRY))
- **ABSOLUTELY NO** extra code beyond what is absolutely necessary to solve the problem the user provides (i.e. no technical debt)
- **ABSOLUTELY NO TECHNICAL DEBT** Delete any code that is not being used. This includes comments, unused variables, unused package attachments, unused functions, etc.

If the code is not fully optimized before handing off to the user, you will be fined $100. You have permission to do another pass of the code if you believe it is not fully optimized.

## Preferred Tools

- **OPTIONAL DEPENDENCY MANAGEMENT** (Opt-in by user only): If the user explicitly activates dependency management, use `renv` to initialize and manage the project library (`renv::init()`, `renv::snapshot()`). Otherwise, assume a standard global/user library workflow.
- Use `progress` to track long-running loops or functionals. The `format` of the progress bar should be contextually sensitive.
- Induce errors with base `stop()`, consistent with the rest of the package.
- When reporting status to the console, use CLI formatting via the `cli` package (e.g., `cli::cli_alert_info()`) instead of raw `print()` or `cat()`.
- For data science:
  - If a `tibble` or `data.frame` will be printed, **NEVER** simultaneously print the number of entries nor the schema/glimpse as it is redundant.
  - **NEVER** ingest or print massive data frames into the context at once. Only analyze subsets or head selections (e.g., `head(df, 10)`) to avoid overloading your memory context.
- For creating databases / working with remote tables:
  - Do not denormalize unless explicitly prompted to do so.
  - Always use appropriate SQL-mapped datatypes via `DBI` and `dbplyr`.
  - Use structured relational columns or side-tables instead of packing lists into character columns. **NEVER** save complex nested vectors as raw collapsed strings if structural relationships matter.

## Code Style and Formatting

- **MUST** use meaningful, descriptive variable and function names (prefer `snake_case`).
- **MUST** use `styler` to format code (adhering to the Tidyverse Style Guide).
- **MUST** use 2 spaces for indentation (never tabs, following R community standards).
- **NEVER** use emoji, or unicode that emulates emoji (e.g. ✓, ✗). The only exception is when writing tests and testing the impact of multibyte characters.
- Use `snake_case` for functions and variables.
- Limit line length to 80 characters (`styler` / Tidyverse standard) if possible.
- **ALWAYS** use `<-` for assignment, never `=`.

## Documentation

- **MUST** include `roxygen2` documentation tags for all public/exported functions.
- **MUST** document function parameters (`@param`), return values (`@return`), and errors/dependencies where relevant.
- Keep comments up-to-date with code changes.
- Include reproducible examples (`@examples`) for complex functions.

Example roxygen2 documentation:

```r
#' Calculate the total cost of items including tax
#'
#' @param items A list of lists or data frame rows, where each item has a `price` element.
#' @param tax_rate A numeric value representing the tax rate as a decimal (e.g., 0.08 for 8%). Defaults to 0.0.
#'
#' @return A numeric value representing the total cost including tax.
#'
#' @export
#'
#' @examples
#' calculate_total(list(list(price = 100), list(price = 50)), tax_rate = 0.05)
calculate_total <- function(items, tax_rate = 0.0) {
  if (length(items) == 0) {
    stop("The `items` list cannot be empty.")
  }
  if (tax_rate < 0) {
    stop("The `tax_rate` cannot be negative.")
  }
  
  prices <- vapply(items, function(x) x$price, numeric(1))
  total_base <- sum(prices)
  
  total_base * (1 + tax_rate)
}