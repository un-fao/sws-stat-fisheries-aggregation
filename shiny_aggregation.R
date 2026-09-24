library(shiny)
library(bslib)
library(SwsApiClient)
library(faoswsFlag)
library(data.table)
library(DT)
library(ggplot2)
library(treemapify)
library(shinyTree)
# -------------------------------------------------------------------------
# Fisheries aggregation Shiny app
#
# Purpose:
# - Initialise the SWS API client.
# - Load Fisheries or disseminated Fisheries-related datasets.
# - Allow users to filter and/or aggregate across several dimensions:
#     1. Country / geographical area
#     2. Species / ASFIS
#     3. Fishing area
#     4. Environment type
#     5. Flag / quality-status values
# - Allow several dimensions to be used at the same time.
# - Produce result tables, yearly summaries, visual checks, and CSV downloads.
# - Process quantity, price, and value datasets through the same workflow
#   for the moment, as requested.
#
# Current aggregation assumption:
# - The selected value column is aggregated using the same aggregation logic
#   across datasets.
# - If flagObservationStatus and flagMethod are available, flags are aggregated
#   with faoswsFlag::aggregateFlagData().
# - If those flag columns are not available, values are summed without flag
#   aggregation.
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------
# Dataset registry
#
# This registry defines exactly which datasets the app supports.
# Each configured dataset has:
# - a dataset id
# - a user-facing label
# - a group: current or disseminated
# - a type: quantity, price, or value
# - the exact column names to use
# -------------------------------------------------------------------------


# Common column names used across all supported Fisheries datasets.
COMMON_FISHERIES_COLUMNS <- list(
  year_col = "timePointYears",
  value_col = "Value",
  geographical_area_col = "geographicAreaM49_fi",
  species_col = "fisheriesAsfis",
  fishing_area_col = "fisheriesCatchArea",
  measured_element_col = "measuredElement",
  observation_flag_col = "flagObservationStatus",
  method_flag_col = "flagMethod"
)


# Build the configuration for one dataset, combining dataset metadata,
# common Fisheries columns, and dataset-specific optional columns.
make_dataset_config <- function(dataset_id,
                                label,
                                dataset_group,
                                dataset_type,
                                has_production_source = FALSE,
                                has_currency_flag = FALSE) {
  c(
    list(
      dataset_id = dataset_id,
      label = label,
      dataset_group = dataset_group,
      dataset_type = dataset_type
    ),
    COMMON_FISHERIES_COLUMNS,
    list(
      production_source_col = if (has_production_source) {
        "fisheriesProductionSource"
      } else {
        NULL
      },
      currency_flag_col = if (has_currency_flag) {
        "flagCurrency"
      } else {
        NULL
      }
    )
  )
}


# Configuration of all Fisheries datasets supported by the application.
# Each entry defines the dataset metadata and any optional dataset-specific columns.
DATASET_CONFIG <- list(
  capture = make_dataset_config(
    dataset_id = "capture",
    label = "Capture production",
    dataset_group = "current",
    dataset_type = "quantity"
  ),
  
  fi_capture_cecaf = make_dataset_config(
    dataset_id = "fi_capture_cecaf",
    label = "CECAF regional capture production",
    dataset_group = "current",
    dataset_type = "quantity"
  ),
  
  fi_capture_gfcm = make_dataset_config(
    dataset_id = "fi_capture_gfcm",
    label = "GFCM regional capture production",
    dataset_group = "current",
    dataset_type = "quantity"
  ),
  
  fi_capture_recofi = make_dataset_config(
    dataset_id = "fi_capture_recofi",
    label = "RECOFI regional capture production",
    dataset_group = "current",
    dataset_type = "quantity"
  ),
  
  fi_capture_seatl = make_dataset_config(
    dataset_id = "fi_capture_seatl",
    label = "SEATL regional capture production",
    dataset_group = "current",
    dataset_type = "quantity"
  ),
  
  aqua = make_dataset_config(
    dataset_id = "aqua",
    label = "Aquaculture production",
    dataset_group = "current",
    dataset_type = "quantity",
    has_production_source = TRUE,
    has_currency_flag = TRUE
  ),
  
  fi_global_production = make_dataset_config(
    dataset_id = "fi_global_production",
    label = "Global production",
    dataset_group = "current",
    dataset_type = "quantity"
  ),
  
  capture_price = make_dataset_config(
    dataset_id = "capture_price",
    label = "Capture price",
    dataset_group = "current",
    dataset_type = "price",
    has_currency_flag = TRUE
  ),
  
  aquaculture_value = make_dataset_config(
    dataset_id = "aquaculture_value",
    label = "Aquaculture value",
    dataset_group = "current",
    dataset_type = "value",
    has_production_source = TRUE,
    has_currency_flag = TRUE
  ),
  
  aqua_disseminated = make_dataset_config(
    dataset_id = "aqua_disseminated",
    label = "Aquaculture production validated",
    dataset_group = "disseminated",
    dataset_type = "quantity",
    has_production_source = TRUE,
    has_currency_flag = TRUE
  ),
  
  capture_disseminated = make_dataset_config(
    dataset_id = "capture_disseminated",
    label = "Capture production disseminated",
    dataset_group = "disseminated",
    dataset_type = "quantity"
  )
)

AGGREGATION_DIMENSIONS <- list(
  geographical_area = list(
    label = "Country / geographical area",
    dataset_column = "geographicAreaM49_fi",
    codelist = "geographicAreaM49_fi",
    remainder_label = "Other filtered geographical areas"
  ),
  
  species = list(
    label = "Species / ASFIS",
    dataset_column = "fisheriesAsfis",
    codelist = "fisheriesAsfis",
    remainder_label = "Other filtered species"
  ),
  
  fishing_area = list(
    label = "Fishing area",
    dataset_column = "fisheriesCatchArea",
    codelist = "fisheriesCatchArea",
    remainder_label = "Other filtered fishing areas"
  ),
  
  production_source = list(
    label = "Production source / environment type",
    dataset_column = "fisheriesProductionSource",
    codelist = "fisheriesProductionSource",
    remainder_label = "Other filtered production sources"
  )
)

# Define the dataset dimensions available for filtering in the application,
# including the corresponding dataset column and codelist, where applicable.
FILTER_DIMENSIONS <- list(
  
  measured_element = list(
    label = "Measured element",
    dataset_column = "measuredElement",
    codelist = NULL
  ),
  
  geographical_area = list(
    label = "Geographical area",
    dataset_column = "geographicAreaM49_fi",
    codelist = "geographicAreaM49_fi"
  ),
  
  species = list(
    label = "Species / ASFIS",
    dataset_column = "fisheriesAsfis",
    codelist = "fisheriesAsfis"
  ),
  
  fishing_area = list(
    label = "Fishing area",
    dataset_column = "fisheriesCatchArea",
    codelist = "fisheriesCatchArea"
  ),
  
  production_source = list(
    label = "Production source / environment type",
    dataset_column = "fisheriesProductionSource",
    codelist = "fisheriesProductionSource"
  ),
  
  observation_flag = list(
    label = "Observation flag",
    dataset_column = "flagObservationStatus",
    codelist = NULL
  ),
  
  # method_flag = list(
  #   label = "Method flag",
  #   dataset_column = "flagMethod",
  #   codelist = NULL
  # ),
  
  currency_flag = list(
    label = "Currency flag",
    dataset_column = "flagCurrency",
    codelist = NULL
  )
)



# Retrieve the configuration associated with the selected dataset.
get_dataset_config <- function(dataset_id) {
  if (is.null(dataset_id) || !nzchar(dataset_id)) {
    stop("No dataset has been selected.")
  }
  
  if (!dataset_id %in% names(DATASET_CONFIG)) {
    stop(
      "Dataset '", dataset_id, "' is not configured in DATASET_CONFIG."
    )
  }
  
  DATASET_CONFIG[[dataset_id]]
}


# Return the datasets available for selection, optionally restricted to a dataset group.
get_dataset_choices <- function(dataset_group = NULL) {
  configs <- DATASET_CONFIG
  
  if (!is.null(dataset_group)) {
    configs <- configs[
      vapply(
        configs,
        function(x) identical(x$dataset_group, dataset_group),
        logical(1)
      )
    ]
  }
  
  ids <- names(configs)
  
  labels <- vapply(
    configs,
    function(x) paste0(x$dataset_id, " - ", x$label),
    character(1)
  )
  
  stats::setNames(ids, labels)
}


# Check that the selected dataset contains all required columns
# and report any missing optional columns.
validate_dataset_columns <- function(data, dataset_id) {
  cfg <- get_dataset_config(dataset_id)
  
  required_cols <- c(
    cfg$year_col,
    cfg$value_col,
    cfg$geographical_area_col,
    cfg$species_col,
    cfg$fishing_area_col,
    cfg$measured_element_col,
    cfg$observation_flag_col,
    cfg$method_flag_col
  )
  
  optional_cols <- c(
    cfg$production_source_col,
    cfg$currency_flag_col
  )
  
  missing_required <- setdiff(required_cols, names(data))
  missing_optional <- setdiff(optional_cols, names(data))
  
  if (length(missing_required) > 0) {
    stop(
      "The selected dataset is missing required columns: ",
      paste(missing_required, collapse = ", ")
    )
  }
  
  list(
    config = cfg,
    required_cols = required_cols,
    optional_cols = optional_cols,
    missing_required = missing_required,
    missing_optional = missing_optional
  )
}




# Return y when x is NULL or empty; otherwise return x.
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

# Fisheries source values contain at most four decimal places.
VALUE_DECIMAL_DIGITS <- 4L
# Standardize a vector of codes by converting to character,
# trimming whitespace, removing missing/empty values, and keeping unique codes.
clean_code_vector <- function(x) {
  
  if (is.null(x)) {
    return(character(0))
  }
  
  x <- unique(
    trimws(
      as.character(x)
    )
  )
  
  x[
    !is.na(x) &
      nzchar(x)
  ]
}


# Build a descriptive context string for aggregation operations,
# including the dimension, dataset column, codelist, selections, and processing stage.
format_aggregation_context <- function(
    dimension_id,
    label,
    dataset_column,
    codelist = NULL,
    selected_codes = NULL,
    stage = NULL
) {
  
  # Clean and standardize all context inputs.
  label <- clean_code_vector(label)
  dimension_id <- clean_code_vector(dimension_id)
  dataset_column <- clean_code_vector(dataset_column)
  codelist <- clean_code_vector(codelist)
  selected_codes <- clean_code_vector(selected_codes)
  stage <- clean_code_vector(stage)
  
  # Use the provided label when available; otherwise use a fallback label.
  label_text <- if (length(label) > 0L) {
    label[1L]
  } else {
    "Unknown aggregation dimension"
  }
  # Initialize the vector that will contain the context details.
  details <- character(0)
  # Add the aggregation dimension ID when available.
  if (length(dimension_id) > 0L) {
    details <- c(
      details,
      paste0(
        "dimension ID: ",
        dimension_id[1L]
      )
    )
  }
  # Add the corresponding dataset column when available.
  if (length(dataset_column) > 0L) {
    details <- c(
      details,
      paste0(
        "dataset column: ",
        dataset_column[1L]
      )
    )
  }
  # Add the codelist used for the dimension when available.
  if (length(codelist) > 0L) {
    details <- c(
      details,
      paste0(
        "codelist: ",
        codelist[1L]
      )
    )
  }
  # Add the selected aggregation codes when available.
  if (length(selected_codes) > 0L) {
    details <- c(
      details,
      paste0(
        "selected code(s): ",
        paste(
          selected_codes,
          collapse = ", "
        )
      )
    )
  }
  # Add the processing stage when available.
  if (length(stage) > 0L) {
    details <- c(
      details,
      paste0(
        "stage: ",
        stage[1L]
      )
    )
  }
  # Combine the label and all available details into one context string.
  paste0(
    label_text,
    " [",
    paste(
      details,
      collapse = "; "
    ),
    "]"
  )
}

# Evaluate an aggregation expression while attaching detailed aggregation
# context to any error message that occurs.
with_aggregation_context <- function(
    expression,
    dimension_id,
    label,
    dataset_column,
    codelist = NULL,
    selected_codes = NULL,
    stage = NULL
) {
  # Build a descriptive context string for the current aggregation operation.
  context <- format_aggregation_context(
    dimension_id = dimension_id,
    label = label,
    dataset_column = dataset_column,
    codelist = codelist,
    selected_codes = selected_codes,
    stage = stage
  )
  # Run the expression and intercept any error raised during execution.
  tryCatch(
    force(expression),
    # If an error occurs, re-throw it together with the aggregation context.
    error = function(e) {
      stop(
        paste0(
          context,
          ": ",
          conditionMessage(e)
        ),
        call. = FALSE
      )
    }
  )
}


normalise_and_drop_empty_values <- function(
    data,
    value_col = "Value"
) {
  
  dt <- copy(
    as.data.table(data)
  )
  
  # Tagged datasets may return lowercase "value".
  if (
    "value" %in% names(dt) &&
    !value_col %in% names(dt)
  ) {
    setnames(
      dt,
      "value",
      value_col
    )
  }
  
  if (!value_col %in% names(dt)) {
    stop(
      paste0(
        "Value column '",
        value_col,
        "' was not found."
      )
    )
  }
  
  value_character <- trimws(
    as.character(
      dt[[value_col]]
    )
  )
  
  # Empty and whitespace-only values become NA.
  value_character[
    is.na(value_character) |
      !nzchar(value_character)
  ] <- NA_character_
  
  # Nonnumeric values also become NA.
  value_numeric <- suppressWarnings(
    as.numeric(value_character)
  )
  
  dt[
    ,
    (value_col) := value_numeric
  ]
  
  # Zero remains valid. Only NA rows are discarded.
  dt <- dt[
    !is.na(get(value_col))
  ]
  
  dt[]
}

uses_filter <- function(action) {
  action %in% c("filter", "filter_aggregate")
}

uses_aggregate <- function(action) {
  action %in% c("aggregate", "filter_aggregate")
}


sum_or_na <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  
  if (all(is.na(x))) {
    return(NA_real_)
  }
  
  sum(x, na.rm = TRUE)
}


infer_value_decimal_places <- function(
    x,
    max_digits = 10L,
    tolerance = 1e-9
) {
  x <- suppressWarnings(
    as.numeric(x)
  )
  
  x <- x[
    !is.na(x) &
      is.finite(x)
  ]
  
  if (length(x) == 0) {
    return(0L)
  }
  
  for (digits_i in 0:max_digits) {
    
    rounded_x <- round(
      x,
      digits = digits_i
    )
    
    if (
      all(
        abs(x - rounded_x) <= tolerance
      )
    ) {
      return(
        as.integer(digits_i)
      )
    }
  }
  
  as.integer(max_digits)
}


sum_or_na_rounded <- function(
    x,
    digits
) {
  x <- suppressWarnings(
    as.numeric(x)
  )
  
  if (all(is.na(x))) {
    return(NA_real_)
  }
  
  round(
    sum(
      x,
      na.rm = TRUE
    ),
    digits = digits
  )
}

has_children <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(FALSE)
  }
  y <- unlist(x, recursive = TRUE, use.names = FALSE)
  y <- y[!is.na(y) & nzchar(as.character(y))]
  length(y) > 0
}

extract_child_ids <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(character(0))
  }
  
  # If SWS returns children as a data.frame/data.table with an id column.
  if (is.data.frame(x)) {
    if ("id" %in% names(x)) {
      ids <- as.character(x$id)
      ids <- ids[!is.na(ids) & nzchar(ids)]
      return(unique(ids))
    }
    
    ids <- unlist(x, recursive = TRUE, use.names = FALSE)
    ids <- as.character(ids)
    ids <- ids[!is.na(ids) & nzchar(ids)]
    return(unique(ids))
  }
  
  # If SWS returns children as a simple vector.
  if (is.atomic(x)) {
    ids <- as.character(x)
    ids <- ids[!is.na(ids) & nzchar(ids)]
    return(unique(ids))
  }
  
  # If SWS returns children as a list.
  if (is.list(x)) {
    if (!is.null(x$id)) {
      ids <- as.character(x$id)
      ids <- ids[!is.na(ids) & nzchar(ids)]
      return(unique(ids))
    }
    
    ids <- unlist(
      lapply(x, extract_child_ids),
      recursive = TRUE,
      use.names = FALSE
    )
    
    ids <- as.character(ids)
    ids <- ids[!is.na(ids) & nzchar(ids)]
    return(unique(ids))
  }
  
  character(0)
}

get_direct_children <- function(codes, parent_code) {
  codes <- as.data.table(codes)
  codes[, id := as.character(id)]
  
  parent_code <- as.character(parent_code)
  
  if (!"id" %in% names(codes) || !"children" %in% names(codes)) {
    stop("The codelist must contain columns 'id' and 'children'.")
  }
  
  row <- codes[id == parent_code]
  
  if (nrow(row) == 0) {
    return(character(0))
  }
  
  extract_child_ids(row$children[[1]])
}

get_descendants <- function(codes, parent_code) {
  
  # Make sure the codelist is a data.table.
  # This allows us to use data.table syntax inside get_direct_children().
  codes <- as.data.table(codes)
  
  # Make sure the parent code is treated as character,
  # because codelist ids are usually character codes.
  parent_code <- as.character(parent_code)
  
  # This will store all descendants found under the parent code.
  # It starts empty.
  out <- character(0)
  
  # This keeps track of codes that have already been processed.
  # It avoids processing the same code more than once.
  visited <- character(0)
  
  # Start with the direct children of the selected parent code.
  # For example, if parent_code = "ISSCAAP",
  # this may return c("1501", "1502", ..., "1509").
  queue <- get_direct_children(codes, parent_code)
  
  # Continue as long as there are still codes to process.
  while (length(queue) > 0) {
    
    # Take the first code from the queue.
    current <- queue[1]
    
    # Remove that code from the queue,
    # because we are processing it now.
    queue <- queue[-1]
    
    # If this code has already been processed before,
    # skip it and move to the next one.
    if (current %in% visited) {
      next
    }
    
    # Mark the current code as processed.
    visited <- c(visited, current)
    
    # Add the current code to the list of descendants.
    out <- c(out, current)
    
    # Look for children of the current code.
    # If the current code has children, they are added to the queue.
    # If it has no children, get_direct_children() returns character(0),
    # so nothing is added.
    queue <- c(queue, get_direct_children(codes, current))
  }
  
  # Return all descendants without duplicates.
  unique(out)
}

# Create named code choices for display, using the best available label column.
make_code_choices <- function(codes) {
  codes <- as.data.table(codes)
  # Return an empty vector if the codelist does not contain an ID column.
  if (!"id" %in% names(codes)) {
    return(character(0))
  }
  # Use the first available descriptive label column.
  label_col <- intersect(c("label_en", "label", "description"), names(codes))[1]
  # Combine each code with its label when available; otherwise use the code alone.
  if (!is.na(label_col)) {
    labels <- paste0(codes$id, " - ", codes[[label_col]])
  } else {
    labels <- codes$id
  }
  # Return the codes as values, with the display labels as names.
  stats::setNames(as.character(codes$id), labels)
}



# Create filter choices using only values that are present in the loaded dataset.
# When codelist information is available, use descriptive labels and units for display.
make_filter_choices_from_data <- function(data,
                                          column_name,
                                          codes = NULL) {
  dt <- copy(data)
  # Return no choices if the requested column is not available.
  if (is.null(column_name) || !column_name %in% names(dt)) {
    return(character(0))
  }
  
  # Keep only distinct, non-missing values that actually occur in the dataset.
  values <- sort(unique(as.character(dt[[column_name]])))
  values <- values[!is.na(values) & nzchar(values)]
  # Return no choices if the column contains no usable values.
  if (length(values) == 0) {
    return(character(0))
  }
  # By default, use the values themselves as both stored values and display labels.
  out <- values
  names(out) <- values
  # If a codelist is available, enrich the display labels.
  if (!is.null(codes)) {
    codes <- as.data.table(codes)
    codes[, id := as.character(id)]
    # Use the first available descriptive label column.
    label_col <- intersect(
      c("label_en", "label", "description"),
      names(codes)
    )[1]
    # Build user-facing labels when a descriptive column is available.
    if (!is.na(label_col)) {
      label_map <- codes[
        id %in% values,
        .(
          id,
          label_text = as.character(get(label_col)),
          unit_text = if ("unit" %in% names(codes)) {
            as.character(unit)
          } else {
            ""
          }
        )
      ]
      # Fall back to the code itself when the descriptive label is missing.
      label_map[
        is.na(label_text) | !nzchar(label_text),
        label_text := id
      ]
      # Replace missing units with an empty string.
      label_map[
        is.na(unit_text),
        unit_text := ""
      ]
      
      label_map[, unit_text := trimws(unit_text)]
      # Display code, label and unit when a unit is available.
      label_map[
        nzchar(unit_text),
        label := paste0(id, " - ", label_text, " [", unit_text, "]")
      ]
      # Display only code and label when no unit is available.
      label_map[
        !nzchar(unit_text),
        label := paste0(id, " - ", label_text)
      ]
      # Match the dataset values to their corresponding codelist labels.
      matched <- match(values, label_map$id)
      has_match <- !is.na(matched)
      # Replace the default display names only for values found in the codelist.
      names(out)[has_match] <- label_map$label[matched[has_match]]
    }
  }
  # Return named choices: displayed labels as names and codes as values.
  out
}

# Create display choices from the full codelist.
# Optionally restrict the choices to aggregate codes that have children.
make_full_codelist_choices <- function(codes,
                                       aggregate_only = FALSE) {
  codes <- as.data.table(codes)
  # Return no choices if the codelist does not contain an ID column.
  if (!"id" %in% names(codes)) {
    return(character(0))
  }
  # Standardize codelist IDs as character values.
  codes[, id := as.character(id)]
  # If requested, keep only aggregate codes that have child categories.
  if (isTRUE(aggregate_only)) {
    # Apply the restriction only when hierarchy information is available.
    if ("children" %in% names(codes)) {
      codes <- codes[
        vapply(children, has_children, logical(1))
      ]
    }
  }
  # Use the first available descriptive label column.
  label_col <- intersect(
    c("label_en", "label", "description"),
    names(codes)
  )[1]
  # Combine each code with its label when available;
  # otherwise use the code itself as the display label.
  if (!is.na(label_col)) {
    labels <- paste0(codes$id, " - ", codes[[label_col]])
  } else {
    labels <- codes$id
  }
  # Return named choices: display labels as names and codes as values.
  stats::setNames(as.character(codes$id), labels)
}


# Expand selected hierarchy codes to include each selected code
# together with all of its descendants.
expand_selected_codes_with_descendants <- function(selected_codes,
                                                   codes) {
  # Return an empty vector when no codes have been selected.
  if (is.null(selected_codes) || length(selected_codes) == 0) {
    return(character(0))
  }
  # Standardize the codelist and code IDs.
  codes <- as.data.table(codes)
  codes[, id := as.character(id)]
  # Convert selected codes to unique, non-missing character values.
  selected_codes <- unique(as.character(selected_codes))
  selected_codes <- selected_codes[!is.na(selected_codes) & nzchar(selected_codes)]
  # Return an empty vector if no valid selected codes remain.
  if (length(selected_codes) == 0) {
    return(character(0))
  }
  # For each selected code, include the code itself
  # and all descendants below it in the hierarchy.
  expanded <- unique(unlist(
    lapply(
      selected_codes,
      function(code_i) {
        c(
          code_i,
          get_descendants(
            codes = codes,
            parent_code = code_i
          )
        )
      }
    ),
    use.names = FALSE
  ))
  # Remove any missing or empty codes from the expanded selection.
  expanded <- expanded[!is.na(expanded) & nzchar(expanded)]
  # Return the final set of unique selected and descendant codes.
  unique(expanded)
}

# Check whether values represent a logical TRUE condition.
is_true_value <- function(x) {
  # Convert input values to character for consistent comparison.
  x <- as.character(x)
  # Treat "true", "t", "1", and "yes" as TRUE, ignoring capitalization.
  # Missing values are always treated as FALSE.
  !is.na(x) & tolower(x) %in% c("true", "t", "1", "yes")
}

# Create a display label for a hierarchy tree node using its code
# and, when available, its descriptive label.
make_tree_label <- function(code_id, label_text = NULL) {
  # Standardize the code as character.
  code_id <- as.character(code_id)
  # If no descriptive label is available, display only the code in brackets.
  if (is.null(label_text) || is.na(label_text) || !nzchar(label_text)) {
    return(paste0("[", code_id, "]"))
  }
  # Otherwise display both the code and the descriptive label.
  paste0("[", code_id, "] ", label_text)
}


extract_code_from_tree_label <- function(x) {
  # shinyTree selections can arrive as a character vector, a named vector,
  # or a nested list. We collect both values and names because sometimes
  # the useful label is stored as the name, not as the value.
  values <- unlist(
    x,
    recursive = TRUE,
    use.names = FALSE
  )
  
  names_with_path <- names(
    unlist(
      x,
      recursive = TRUE,
      use.names = TRUE
    )
  )
  
  x <- unique(c(values, names_with_path))
  x <- as.character(x)
  x <- trimws(x)
  x <- x[!is.na(x) & nzchar(x)]
  
  # Keep only the actual node label if shinyTree gives a path-like name.
  # Example: "[ISSCAAP] ISSCAAP.[1501] Freshwater fishes"
  # We want the last bracketed code if there are multiple.
  out <- vapply(
    x,
    function(label_i) {
      matches <- regmatches(
        label_i,
        gregexpr("\\[[^\\]]+\\]", label_i)
      )[[1]]
      
      if (length(matches) == 0) {
        return(label_i)
      }
      
      last_match <- matches[length(matches)]
      sub("^\\[([^\\]]+)\\]$", "\\1", last_match)
    },
    character(1)
  )
  
  out <- trimws(out)
  out <- out[!is.na(out) & nzchar(out)]
  
  unique(out)
}

# Return the first available descriptive label column from the codelist.
get_codelist_label_column <- function(codes) {
  intersect(
    c("label_en", "label", "description"),
    names(codes)
  )[1]
}

# Create display labels for hierarchy-tree nodes using code IDs
# and the best available descriptive labels.
make_tree_display_labels <- function(codes) {
  # Standardize the codelist and code IDs.
  codes <- as.data.table(codes)
  codes[, id := as.character(id)]
  # Identify the first available descriptive label column.
  label_col <- get_codelist_label_column(codes)
  # If no descriptive label column exists, use the code itself.
  if (is.na(label_col)) {
    label_values <- codes$id
  } else {
    
    # Otherwise use the descriptive labels from the codelist.
    label_values <- as.character(codes[[label_col]])
    # Replace missing or empty labels with the corresponding code ID.
    label_values[
      is.na(label_values) | !nzchar(label_values)
    ] <- codes$id[
      is.na(label_values) | !nzchar(label_values)
    ]
  }
  # Use the regular code ID as the default displayed code.
  display_code <- codes$id
  # If an alternative display ID is available, use it where provided.
  if ("display_id" %in% names(codes)) {
    display_code <- ifelse(
      !is.na(codes$display_id) & nzchar(as.character(codes$display_id)),
      as.character(codes$display_id),
      display_code
    )
  }
  # Combine the displayed code and descriptive label.
  labels <- paste0(
    display_code,
    " - ",
    label_values
  )
  # Some synthetic hierarchy nodes should display only their descriptive label.
  if (
    "synthetic_label_only" %in%
    names(codes)
  ) {
    # Identify the rows marked as label-only synthetic nodes.
    synthetic_rows <- is_true_value(
      codes$synthetic_label_only
    )
    # Remove the code prefix from those synthetic-node labels.
    labels[synthetic_rows] <-
      label_values[synthetic_rows]
  }
  # Return display labels named by the underlying code IDs.
  stats::setNames(
    labels,
    codes$id
  )
}

# Convert selected hierarchy-tree display labels back to their underlying code IDs.
# For flat dimensions without a codelist, return the selected values directly.
tree_display_labels_to_codes <- function(selected_labels,
                                         codes = NULL) {
  if (is.null(selected_labels) || length(selected_labels) == 0) {
    return(character(0))
  }
  
  selected_labels <- unlist(
    selected_labels,
    recursive = TRUE,
    use.names = FALSE
  )
  
  selected_labels <- as.character(selected_labels)
  selected_labels <- trimws(selected_labels)
  selected_labels <- selected_labels[!is.na(selected_labels) & nzchar(selected_labels)]
  
  if (length(selected_labels) == 0) {
    return(character(0))
  }
  
  # For flat dimensions, such as flags, there is no codelist.
  # In that case the selected labels are already the values to filter by.
  if (is.null(codes)) {
    return(unique(selected_labels))
  }
  
  codes <- as.data.table(codes)
  codes[, id := as.character(id)]
  
  display_labels <- make_tree_display_labels(codes)
  
  # lookup: "1501 - Freshwater fishes" -> "1501"
  label_to_id <- stats::setNames(
    names(display_labels),
    unname(display_labels)
  )
  
  selected_codes <- unname(label_to_id[selected_labels])
  
  # Safety fallback:
  # if a selected value is already just "1501", keep "1501".
  missing <- is.na(selected_codes)
  
  if (any(missing)) {
    fallback <- selected_labels[missing]
    
    fallback <- sub(
      "^([^ ]+)\\s+-\\s+.*$",
      "\\1",
      fallback
    )
    
    fallback <- sub(
      "^\\[([^\\]]+)\\].*$",
      "\\1",
      fallback
    )
    
    selected_codes[missing] <- fallback
  }
  
  selected_codes <- as.character(selected_codes)
  selected_codes <- trimws(selected_codes)
  selected_codes <- selected_codes[!is.na(selected_codes) & nzchar(selected_codes)]
  
  unique(selected_codes)
}

# Identify the root nodes of an SWS codelist hierarchy.
# Virtual roots are preferred when available; otherwise infer roots from parent-child relationships.
get_sws_tree_root_codes <- function(codes) {
  # Standardize the codelist and code IDs.
  codes <- as.data.table(codes)
  codes[, id := as.character(id)]
  
  # If no hierarchy information is available, treat all codes as possible roots.
  if (!"children" %in% names(codes)) {
    return(codes$id)
  }
  
  # Keep only codes that have at least one child.
  parent_codes <- codes[
    vapply(children, has_children, logical(1)),
    id
  ]
  
  # If no parent codes exist, return all codes.
  if (length(parent_codes) == 0) {
    return(codes$id)
  }
  
  # First preference: use SWS virtual root nodes when available.
  # Example in fisheriesAsfis:
  # ISSCAAP and TAXONOMIC have virtual == TRUE.
  if ("virtual" %in% names(codes)) {
    virtual_roots <- codes[
      id %in% parent_codes &
        is_true_value(virtual),
      id
    ]
    
    # Return virtual roots when at least one is available.
    if (length(virtual_roots) > 0) {
      return(virtual_roots)
    }
  }
  
  # Fallback: roots are parent codes that are not children of any other code.
  all_children <- unique(unlist(
    lapply(codes$children, extract_child_ids),
    recursive = TRUE,
    use.names = FALSE
  ))
  
  all_children <- as.character(all_children)
  all_children <- all_children[!is.na(all_children) & nzchar(all_children)]
  
  root_codes <- setdiff(parent_codes, all_children)
  
  if (length(root_codes) == 0) {
    root_codes <- parent_codes
  }
  
  root_codes
}

# Build a nested hierarchy tree from an SWS codelist using its parent–child relationships.
build_sws_codelist_tree <- function(codes,
                                    max_depth = 50) {
  codes <- as.data.table(codes)
  codes[, id := as.character(id)]
  
  label_col <- get_codelist_label_column(codes)
  
  display_labels <- make_tree_display_labels(codes)
  
  label_for_code <- function(code_id) {
    code_id <- as.character(code_id)
    
    out <- unname(display_labels[code_id])
    
    if (length(out) == 0 || is.na(out) || !nzchar(out)) {
      return(code_id)
    }
    
    out
  }
  
  
  order_codes <- function(code_vector) {
    code_vector <- as.character(code_vector)
    code_vector <- code_vector[!is.na(code_vector) & nzchar(code_vector)]
    code_vector <- code_vector[code_vector %in% codes$id]
    
    if (length(code_vector) == 0) {
      return(character(0))
    }
    
    if (!"order" %in% names(codes)) {
      return(code_vector)
    }
    
    tmp <- codes[
      id %in% code_vector,
      .(
        id,
        order_tmp = suppressWarnings(as.numeric(order))
      )
    ]
    
    tmp <- tmp[order(order_tmp, id)]
    
    tmp$id
  }
  
  make_node <- function(code_id,
                        depth = 1,
                        visited = character(0)) {
    code_id <- as.character(code_id)
    
    if (code_id %in% visited) {
      return("")
    }
    
    if (depth > max_depth) {
      return("")
    }
    
    children <- get_direct_children(
      codes = codes,
      parent_code = code_id
    )
    
    children <- order_codes(children)
    
    if (length(children) == 0) {
      return("")
    }
    
    child_nodes <- lapply(
      children,
      function(child_id) {
        make_node(
          code_id = child_id,
          depth = depth + 1,
          visited = c(visited, code_id)
        )
      }
    )
    
    child_labels <- vapply(
      children,
      label_for_code,
      character(1),
      USE.NAMES = FALSE
    )
    
    child_labels[is.na(child_labels) | !nzchar(child_labels)] <- children[
      is.na(child_labels) | !nzchar(child_labels)
    ]
    
    child_nodes <- stats::setNames(
      child_nodes,
      child_labels
    )
    
    child_nodes
  }
  
  root_codes <- get_sws_tree_root_codes(codes)
  root_codes <- order_codes(root_codes)
  
  if (length(root_codes) == 0) {
    return(list())
  }
  
  root_nodes <- lapply(
    root_codes,
    function(root_code) {
      make_node(root_code)
    }
  )
  
  root_labels <- vapply(
    root_codes,
    label_for_code,
    character(1),
    USE.NAMES = FALSE
  )
  
  root_labels[is.na(root_labels) | !nzchar(root_labels)] <- root_codes[
    is.na(root_labels) | !nzchar(root_labels)
  ]
  
  root_nodes <- stats::setNames(
    root_nodes,
    root_labels
  )
  
  root_nodes
}

#Return the number of hierarchy levels available in a codelist tree.
get_codelist_tree_display_depth <- function(tree_dt) {
  id_cols <- get_tree_id_cols(tree_dt)
  
  if (length(id_cols) == 0L) {
    return(1L)
  }
  
  # Display every hierarchy level available in getCodelistTree().
  as.integer(length(id_cols))
}


#Identifies and orders the columns representing hierarchy levels in a codelist tree.
get_tree_id_cols <- function(tree_dt) {
  id_cols <- grep("^level_[0-9]+_id$", names(tree_dt), value = TRUE)
  id_cols[order(as.integer(sub("^level_([0-9]+)_id$", "\\1", id_cols)))]
}

#Returns the immediate children of a selected parent code from the flattened codelist tree.
get_direct_children_from_codelist_tree <- function(tree_dt, parent_code) {
  tree_dt <- as.data.table(tree_dt)
  id_cols <- get_tree_id_cols(tree_dt)
  
  parent_code <- as.character(parent_code)
  out <- character(0)
  
  for (i in seq_along(id_cols)) {
    if (i == length(id_cols)) {
      next
    }
    
    rows_i <- tree_dt[as.character(get(id_cols[i])) == parent_code]
    
    if (nrow(rows_i) == 0) {
      next
    }
    
    next_col <- id_cols[i + 1]
    vals <- as.character(rows_i[[next_col]])
    vals <- vals[!is.na(vals) & nzchar(vals)]
    
    out <- c(out, vals)
  }
  
  unique(out)
}


# Fishing-area paths can contain the same code at several hierarchy levels.
# For a selected node, use only its shallowest occurrence in the flattened
# tree so that descendants are not mistaken for direct children.
get_direct_children_from_shallowest_tree_level <- function(
    tree_dt,
    parent_code
) {
  tree_dt <- as.data.table(tree_dt)
  id_cols <- get_tree_id_cols(tree_dt)
  
  parent_code <- trimws(as.character(parent_code))[1L]
  
  if (
    length(id_cols) < 2L ||
    is.na(parent_code) ||
    !nzchar(parent_code)
  ) {
    return(character(0))
  }
  
  parent_levels <- which(
    vapply(
      id_cols,
      function(column_i) {
        values_i <- trimws(as.character(tree_dt[[column_i]]))
        any(
          !is.na(values_i) &
            nzchar(values_i) &
            values_i == parent_code
        )
      },
      logical(1)
    )
  )
  
  parent_levels <- parent_levels[
    parent_levels < length(id_cols)
  ]
  
  if (length(parent_levels) == 0L) {
    return(character(0))
  }
  
  parent_level <- min(parent_levels)
  parent_col <- id_cols[parent_level]
  child_col <- id_cols[parent_level + 1L]
  
  rows_i <- tree_dt[
    trimws(as.character(get(parent_col))) == parent_code
  ]
  
  children <- trimws(
    as.character(
      rows_i[[child_col]]
    )
  )
  
  children <- children[
    !is.na(children) &
      nzchar(children) &
      children != parent_code
  ]
  
  unique(children)
}

#Returns all descendants below a selected code in the hierarchy.
get_descendants_from_codelist_tree <- function(tree_dt, parent_code) {
  tree_dt <- as.data.table(tree_dt)
  id_cols <- get_tree_id_cols(tree_dt)
  
  parent_code <- as.character(parent_code)
  out <- character(0)
  
  for (i in seq_along(id_cols)) {
    rows_i <- tree_dt[as.character(get(id_cols[i])) == parent_code]
    
    if (nrow(rows_i) == 0) {
      next
    }
    
    if (i < length(id_cols)) {
      later_cols <- id_cols[(i + 1):length(id_cols)]
      vals <- unlist(rows_i[, ..later_cols], use.names = FALSE)
      vals <- as.character(vals)
      vals <- vals[!is.na(vals) & nzchar(vals)]
      out <- c(out, vals)
    }
  }
  
  out <- unique(out)
  setdiff(out, parent_code)
}

#Returns all ancestor codes above a selected code in the hierarchy.
get_ancestors_from_codelist_tree <- function(tree_dt, code) {
  tree_dt <- as.data.table(tree_dt)
  id_cols <- get_tree_id_cols(tree_dt)
  
  code <- as.character(code)
  out <- character(0)
  
  for (i in seq_along(id_cols)) {
    rows_i <- tree_dt[as.character(get(id_cols[i])) == code]
    
    if (nrow(rows_i) == 0) {
      next
    }
    
    if (i > 1) {
      earlier_cols <- id_cols[1:(i - 1)]
      vals <- unlist(rows_i[, ..earlier_cols], use.names = FALSE)
      vals <- as.character(vals)
      vals <- vals[!is.na(vals) & nzchar(vals)]
      out <- c(out, vals)
    }
  }
  
  unique(out)
}

#Keeps only the highest selected nodes when both a parent and one of its descendants are selected.
keep_top_selected_codes_from_codelist_tree <- function(selected_codes, tree_dt) {
  selected_codes <- unique(as.character(selected_codes))
  selected_codes <- selected_codes[!is.na(selected_codes) & nzchar(selected_codes)]
  
  if (length(selected_codes) <= 1) {
    return(selected_codes)
  }
  
  selected_set <- selected_codes
  
  keep <- vapply(
    selected_codes,
    function(code_i) {
      ancestors_i <- get_ancestors_from_codelist_tree(
        tree_dt = tree_dt,
        code = code_i
      )
      
      !any(ancestors_i %in% setdiff(selected_set, code_i))
    },
    logical(1)
  )
  
  selected_codes[keep]
}

#Returns a selected hierarchy node together with all codes belonging to its branch, 
#with special handling for fishing-area hierarchies.
get_hierarchy_branch_codes <- function(
    tree_dt,
    root_code,
    codelist_id = NULL
) {
  
  tree_dt <- as.data.table(
    tree_dt
  )
  
  root_code <- clean_code_vector(
    root_code
  )
  
  if (length(root_code) == 0L) {
    return(character(0))
  }
  
  root_code <- root_code[1L]
  
  # ------------------------------------------------------------
  # Fishing areas can contain the same code at multiple levels.
  # Use the shallowest occurrence, consistently with the existing
  # direct-child fishing-area logic.
  # ------------------------------------------------------------
  if (identical(
    codelist_id,
    "fisheriesCatchArea"
  )) {
    
    id_cols <- get_tree_id_cols(
      tree_dt
    )
    
    if (length(id_cols) == 0L) {
      return(root_code)
    }
    
    root_levels <- which(
      vapply(
        id_cols,
        function(column_i) {
          
          values_i <- trimws(
            as.character(
              tree_dt[[column_i]]
            )
          )
          
          any(
            !is.na(values_i) &
              nzchar(values_i) &
              values_i == root_code
          )
        },
        logical(1)
      )
    )
    
    if (length(root_levels) == 0L) {
      return(root_code)
    }
    
    root_level <- min(
      root_levels
    )
    
    root_column <- id_cols[
      root_level
    ]
    
    branch_rows <- tree_dt[
      trimws(
        as.character(
          get(root_column)
        )
      ) == root_code
    ]
    
    branch_columns <- id_cols[
      root_level:length(id_cols)
    ]
    
    branch_codes <- unlist(
      branch_rows[
        ,
        ..branch_columns
      ],
      recursive = TRUE,
      use.names = FALSE
    )
    
  } else {
    
    # ASFIS, geographical area and production source.
    branch_codes <- c(
      root_code,
      
      get_descendants_from_codelist_tree(
        tree_dt = tree_dt,
        parent_code = root_code
      )
    )
  }
  
  clean_code_vector(
    branch_codes
  )
}

#Keeps only the most specific selected nodes when both a parent and one or more descendants are selected.
keep_most_specific_selected_codes_from_codelist_tree <-
  function(
    selected_codes,
    tree_dt,
    codelist_id = NULL
  ) {
    
    selected_codes <- clean_code_vector(
      selected_codes
    )
    
    if (length(selected_codes) <= 1L) {
      return(selected_codes)
    }
    
    selected_set <- selected_codes
    
    keep <- vapply(
      selected_codes,
      function(code_i) {
        
        descendants_i <- setdiff(
          get_hierarchy_branch_codes(
            tree_dt = tree_dt,
            root_code = code_i,
            codelist_id = codelist_id
          ),
          code_i
        )
        
        # Remove code_i when another selected code is
        # more specific and belongs to its branch.
        !any(
          descendants_i %in%
            setdiff(
              selected_set,
              code_i
            )
        )
      },
      logical(1)
    )
    
    selected_codes[keep]
  }

#Identifies the root nodes of a flattened SWS codelist tree, preferring virtual roots when available.
get_sws_tree_root_codes_from_codelist_tree <- function(tree_dt, codes = NULL) {
  tree_dt <- as.data.table(tree_dt)
  id_cols <- get_tree_id_cols(tree_dt)
  
  parent_codes <- character(0)
  
  for (i in seq_along(id_cols)) {
    if (i == length(id_cols)) {
      next
    }
    
    rows_i <- tree_dt[
      !is.na(get(id_cols[i])) &
        nzchar(as.character(get(id_cols[i]))) &
        !is.na(get(id_cols[i + 1])) &
        nzchar(as.character(get(id_cols[i + 1])))
    ]
    
    parent_codes <- c(parent_codes, as.character(rows_i[[id_cols[i]]]))
  }
  
  parent_codes <- unique(parent_codes)
  
  if (length(parent_codes) == 0) {
    return(unique(as.character(tree_dt[[id_cols[1]]])))
  }
  
  all_children <- character(0)
  
  if (length(id_cols) >= 2) {
    for (j in 2:length(id_cols)) {
      vals <- as.character(tree_dt[[id_cols[j]]])
      vals <- vals[!is.na(vals) & nzchar(vals)]
      all_children <- c(all_children, vals)
    }
  }
  
  all_children <- unique(all_children)
  
  root_codes <- setdiff(parent_codes, all_children)
  
  if (!is.null(codes)) {
    codes <- as.data.table(codes)
    codes[, id := as.character(id)]
    
    if ("virtual" %in% names(codes)) {
      virtual_roots <- codes[
        id %in% parent_codes &
          is_true_value(virtual),
        id
      ]
      
      if (length(virtual_roots) > 0) {
        root_codes <- virtual_roots
      }
    }
    
    if ("order" %in% names(codes)) {
      tmp <- codes[
        id %in% root_codes,
        .(
          id,
          order_tmp = suppressWarnings(as.numeric(order))
        )
      ]
      
      tmp <- tmp[order(order_tmp, id)]
      root_codes <- tmp$id
    }
  }
  
  root_codes
}

# Build the nested hierarchy displayed in the app from the flattened
# SWS codelist tree, using a precomputed parent-child lookup.
build_sws_codelist_tree_from_codelist_tree <- function(
    tree_dt,
    codes,
    max_depth = 50,
    root_codes = NULL
) {
  
  tree_dt <- as.data.table(tree_dt)
  codes <- as.data.table(codes)
  
  codes[, id := as.character(id)]
  
  id_cols <- get_tree_id_cols(tree_dt)
  
  display_labels <- make_tree_display_labels(codes)
  
  # Prepare code IDs and ordering once for repeated tree-node ordering.
  code_ids <- as.character(codes$id)
  
  code_order_values <- NULL
  
  if ("order" %in% names(codes)) {
    code_order_values <- suppressWarnings(
      as.numeric(codes$order)
    )
  }
  
  
  label_for_code <- function(code_id) {
    
    code_id <- as.character(code_id)
    
    out <- unname(
      display_labels[code_id]
    )
    
    if (
      length(out) == 0 ||
      is.na(out) ||
      !nzchar(out)
    ) {
      return(code_id)
    }
    
    out
  }
  
  
  order_codes <- function(code_vector) {
    
    code_vector <- as.character(code_vector)
    
    code_vector <- code_vector[
      !is.na(code_vector) &
        nzchar(code_vector)
    ]
    
    if (length(code_vector) == 0L) {
      return(character(0))
    }
    
    if (is.null(code_order_values)) {
      return(code_vector)
    }
    
    matched <- match(
      code_vector,
      code_ids
    )
    
    found <- !is.na(matched)
    
    found_codes <- code_vector[
      found
    ]
    
    found_order <- code_order_values[
      matched[found]
    ]
    
    if (length(found_codes) > 0L) {
      
      found_codes <- found_codes[
        base::order(
          found_order,
          found_codes
        )
      ]
    }
    
    c(
      found_codes,
      setdiff(
        code_vector,
        found_codes
      )
    )
  }
  
  
  # Build all direct parent-child relationships once.
  parent_child_pairs <- data.table(
    parent = character(0),
    child = character(0)
  )
  
  if (length(id_cols) >= 2L) {
    
    parent_child_pairs <- rbindlist(
      lapply(
        seq_len(length(id_cols) - 1L),
        function(i) {
          
          parent_values <- as.character(
            tree_dt[[id_cols[i]]]
          )
          
          child_values <- as.character(
            tree_dt[[id_cols[i + 1L]]]
          )
          
          keep <- (
            !is.na(parent_values) &
              nzchar(parent_values) &
              !is.na(child_values) &
              nzchar(child_values)
          )
          
          data.table(
            parent = parent_values[keep],
            child = child_values[keep]
          )
        }
      ),
      use.names = TRUE,
      fill = TRUE
    )
    
    parent_child_pairs <- unique(
      parent_child_pairs
    )
  }
  
  
  # Store the children associated with each parent.
  child_lookup <- split(
    parent_child_pairs$child,
    parent_child_pairs$parent
  )
  
  
  get_children <- function(code_id) {
    
    code_id <- as.character(code_id)
    
    children <- child_lookup[[code_id]]
    
    if (is.null(children)) {
      return(character(0))
    }
    
    unique(
      as.character(children)
    )
  }
  
  
  make_node <- function(
    code_id,
    depth = 1L,
    visited = character(0)
  ) {
    
    code_id <- as.character(code_id)
    
    if (code_id %in% visited) {
      return("")
    }
    
    if (depth > max_depth) {
      return("")
    }
    
    children <- get_children(
      code_id
    )
    
    children <- order_codes(
      children
    )
    
    if (length(children) == 0L) {
      return("")
    }
    
    child_nodes <- lapply(
      children,
      function(child_id) {
        
        make_node(
          code_id = child_id,
          depth = depth + 1L,
          visited = c(
            visited,
            code_id
          )
        )
      }
    )
    
    child_labels <- vapply(
      children,
      label_for_code,
      character(1),
      USE.NAMES = FALSE
    )
    
    stats::setNames(
      child_nodes,
      child_labels
    )
  }
  
  
  if (
    is.null(root_codes) ||
    length(root_codes) == 0L
  ) {
    
    root_codes <-
      get_sws_tree_root_codes_from_codelist_tree(
        tree_dt = tree_dt,
        codes = codes
      )
    
  } else {
    
    root_codes <- unique(
      trimws(
        as.character(root_codes)
      )
    )
    
    root_codes <- root_codes[
      !is.na(root_codes) &
        nzchar(root_codes) &
        root_codes %in% codes$id
    ]
  }
  
  
  root_codes <- order_codes(
    root_codes
  )
  
  if (length(root_codes) == 0L) {
    return(list())
  }
  
  
  root_nodes <- lapply(
    root_codes,
    function(root_code) {
      make_node(root_code)
    }
  )
  
  
  root_labels <- vapply(
    root_codes,
    label_for_code,
    character(1),
    USE.NAMES = FALSE
  )
  
  
  stats::setNames(
    root_nodes,
    root_labels
  )
}




#Creates a child-to-parent lookup table from the codelist hierarchy for efficient ancestor searches.
make_parent_lookup <- function(codes) {
  codes <- as.data.table(codes)
  codes[, id := as.character(id)]
  
  out <- rbindlist(
    lapply(seq_len(nrow(codes)), function(i) {
      children_i <- extract_child_ids(codes$children[[i]])
      
      if (length(children_i) == 0) {
        return(NULL)
      }
      
      data.table(
        parent = codes$id[i],
        child = as.character(children_i)
      )
    }),
    fill = TRUE
  )
  
  if (is.null(out) || nrow(out) == 0) {
    return(data.table(parent = character(0), child = character(0)))
  }
  
  out <- unique(out[!is.na(child) & nzchar(child)])
  setkey(out, child)
  
  out
}

#Retrieve all ancestor codes of a selected code using the parent lookup table.
get_ancestors_fast <- function(code, parent_lookup) {
  code <- as.character(code)
  
  out <- character(0)
  queue <- code
  visited <- character(0)
  
  while (length(queue) > 0) {
    current <- queue[1]
    queue <- queue[-1]
    
    if (current %in% visited) {
      next
    }
    
    visited <- c(visited, current)
    
    parents <- parent_lookup[.(current), parent]
    parents <- parents[!is.na(parents) & nzchar(parents)]
    
    if (length(parents) > 0) {
      out <- c(out, parents)
      queue <- c(queue, parents)
    }
  }
  
  unique(out)
}

#Keep only the highest selected hierarchy nodes when both a parent and one of its descendants are selected.
keep_top_selected_codes <- function(selected_codes, codes) {
  selected_codes <- unique(as.character(selected_codes))
  selected_codes <- selected_codes[!is.na(selected_codes) & nzchar(selected_codes)]
  
  if (length(selected_codes) <= 1) {
    return(selected_codes)
  }
  
  parent_lookup <- make_parent_lookup(codes)
  
  if (nrow(parent_lookup) == 0) {
    return(selected_codes)
  }
  
  selected_set <- selected_codes
  
  keep <- vapply(
    selected_codes,
    function(code_i) {
      ancestors_i <- get_ancestors_fast(
        code = code_i,
        parent_lookup = parent_lookup
      )
      
      !any(ancestors_i %in% setdiff(selected_set, code_i))
    },
    logical(1)
  )
  
  selected_codes[keep]
}

#Read the selected tree nodes, convert them to codes, 
#apply the requested selection rule, and optionally include descendants.
get_selected_codes_from_tree <- function(
    tree_input,
    codes = NULL,
    tree_dt = NULL,
    expand_descendants = TRUE,
    selection_rule = c(
      "top",
      "most_specific",
      "none"
    ),
    codelist_id = NULL
) {
  
  selection_rule <- match.arg(
    selection_rule
  )
  
  selected_labels <- shinyTree::get_selected(
    tree_input,
    format = "names"
  )
  
  if (length(selected_labels) == 0L) {
    return(character(0))
  }
  
  selected_codes <- tree_display_labels_to_codes(
    selected_labels = selected_labels,
    codes = codes
  )
  
  selected_codes <- clean_code_vector(
    selected_codes
  )
  
  if (length(selected_codes) == 0L) {
    return(character(0))
  }
  
  if (!is.null(tree_dt)) {
    
    if (identical(
      selection_rule,
      "top"
    )) {
      
      selected_codes <-
        keep_top_selected_codes_from_codelist_tree(
          selected_codes = selected_codes,
          tree_dt = tree_dt
        )
    }
    
    if (identical(
      selection_rule,
      "most_specific"
    )) {
      
      selected_codes <-
        keep_most_specific_selected_codes_from_codelist_tree(
          selected_codes = selected_codes,
          tree_dt = tree_dt,
          codelist_id = codelist_id
        )
    }
  }
  
  if (
    !is.null(tree_dt) &&
    isTRUE(expand_descendants)
  ) {
    
    expanded_codes <- unique(
      unlist(
        lapply(
          selected_codes,
          function(code_i) {
            
            get_hierarchy_branch_codes(
              tree_dt = tree_dt,
              root_code = code_i,
              codelist_id = codelist_id
            )
          }
        ),
        use.names = FALSE
      )
    )
    
    return(
      clean_code_vector(
        expanded_codes
      )
    )
  }
  
  selected_codes
}

#Retrieve the selected root and its descendants, keeping only codes that represent aggregate groups with children.
get_aggregate_codes_under_root <- function(codes, root_code) {
  codes <- as.data.table(codes)
  root_code <- as.character(root_code)
  
  # Get the root itself plus everything below it
  codes_under_root <- c(root_code, get_descendants(codes, root_code))
  
  # Keep only codes that are aggregate/group codes,
  # meaning codes that have children.
  aggregate_codes <- codes[
    id %in% codes_under_root &
      vapply(children, has_children, logical(1))
  ]
  
  aggregate_codes[]
}

#Create direct-child aggregation groups for the selected hierarchy parents 
#and assign categories outside those groups to the corresponding Other output.
build_classification_map_with_remainder <- function(
    tree_dt,
    selected_parents,
    filtered_raw_codes,
    codes = NULL,
    remainder_label = "Other filtered records",
    allow_remainder_only = FALSE
) {
  
  tree_dt <- as.data.table(
    tree_dt
  )
  
  selected_parents <- clean_code_vector(
    selected_parents
  )
  
  filtered_raw_codes <- clean_code_vector(
    filtered_raw_codes
  )
  
  if (length(selected_parents) == 0L) {
    stop(
      "No hierarchy parents were selected for direct-child aggregation."
    )
  }
  
  if (length(filtered_raw_codes) == 0L) {
    stop(
      "No filtered codes are available for classification aggregation."
    )
  }
  
  # Build one direct-child map for every selected parent.
  selected_parent_map <- rbindlist(
    lapply(
      selected_parents,
      function(parent_code_i) {
        
        map_i <- tryCatch(
          build_direct_child_aggregation_map(
            tree_dt = tree_dt,
            root_code = parent_code_i,
            codes = codes
          ),
          error = function(e) {
            stop(
              paste0(
                "Direct-child aggregation failed for hierarchy parent '",
                parent_code_i,
                "': ",
                e$message
              ),
              call. = FALSE
            )
          }
        )
        
        map_i[
          ,
          selected_parent_code := as.character(
            parent_code_i
          )
        ]
        
        map_i
      }
    ),
    use.names = TRUE,
    fill = TRUE
  )
  
  # Keep only codes that are present after filtering.
  selected_parent_map <- selected_parent_map[
    raw_code %in% filtered_raw_codes
  ]
  
  if (
    nrow(selected_parent_map) == 0L &&
    !isTRUE(allow_remainder_only)
  ) {
    stop(
      paste0(
        "The selected hierarchy parents contain no records ",
        "in the filtered data."
      )
    )
  }
  
  # The same direct-child output must not belong to two
  # different selected parents.
  duplicated_output_groups <- unique(
    selected_parent_map[
      ,
      .(
        selected_parent_code,
        group_code
      )
    ]
  )[
    ,
    .(
      number_of_parents = uniqueN(
        selected_parent_code
      ),
      
      parent_codes = paste(
        sort(
          unique(
            selected_parent_code
          )
        ),
        collapse = ", "
      )
    ),
    by = group_code
  ][number_of_parents > 1L]
  
  if (nrow(duplicated_output_groups) > 0L) {
    stop(
      paste0(
        "At least one direct-child output belongs to more than one ",
        "selected hierarchy parent. Select non-overlapping parents."
      )
    )
  }
  
  # Every raw filtered code must belong to only one output.
  ambiguous_codes <- selected_parent_map[
    ,
    .(
      number_of_groups = uniqueN(
        group_code
      )
    ),
    by = raw_code
  ][number_of_groups > 1L]
  
  if (nrow(ambiguous_codes) > 0L) {
    stop(
      paste0(
        "Some filtered codes belong to direct-child outputs under more ",
        "than one selected hierarchy parent. Select non-overlapping parents."
      )
    )
  }
  
  classified_codes <- unique(
    as.character(
      selected_parent_map$raw_code
    )
  )
  
  selected_parent_map[
    ,
    selected_parent_code := NULL
  ]
  
  # Everything outside all selected parent classifications
  # is assigned to one dimension-specific Other output.
  remainder_codes <- setdiff(
    filtered_raw_codes,
    classified_codes
  )
  
  remainder_map <- data.table(
    raw_code = character(0),
    group_code = character(0)
  )
  
  if (length(remainder_codes) > 0L) {
    remainder_map <- data.table(
      raw_code = remainder_codes,
      group_code = as.character(
        remainder_label
      )
    )
  }
  
  aggregation_map <- rbindlist(
    list(
      selected_parent_map,
      remainder_map
    ),
    use.names = TRUE,
    fill = TRUE
  )
  
  unique(
    aggregation_map[
      !is.na(raw_code) &
        nzchar(raw_code) &
        !is.na(group_code) &
        nzchar(group_code)
    ],
    by = "raw_code"
  )
}




#Create the mapping that assigns each code under 
#a selected parent to one of its direct-child groups and check for overlaps.
build_direct_child_aggregation_map <- function(
    tree_dt,
    root_code,
    codes = NULL
) {
  
  tree_dt <- as.data.table(tree_dt)
  
  root_code <- trimws(
    as.character(root_code)
  )[1L]
  
  if (
    is.na(root_code) ||
    !nzchar(root_code)
  ) {
    stop("The aggregation root code is missing.")
  }
  
  if (!is.null(codes)) {
    
    # Fishing area: use only the shallowest occurrence of the
    # selected parent, then keep every child tied to that exact path.
    id_cols <- get_tree_id_cols(tree_dt)
    
    parent_levels <- which(
      vapply(
        id_cols,
        function(column_i) {
          values_i <- trimws(
            as.character(
              tree_dt[[column_i]]
            )
          )
          
          any(
            !is.na(values_i) &
              nzchar(values_i) &
              values_i == root_code
          )
        },
        logical(1)
      )
    )
    
    parent_levels <- parent_levels[
      parent_levels < length(id_cols)
    ]
    
    if (length(parent_levels) == 0L) {
      stop(
        paste0(
          "The selected aggregation classification '",
          root_code,
          "' has no direct child groups."
        )
      )
    }
    
    parent_level <- min(parent_levels)
    parent_col <- id_cols[parent_level]
    child_col <- id_cols[parent_level + 1L]
    
    parent_rows <- tree_dt[
      trimws(
        as.character(
          get(parent_col)
        )
      ) == root_code
    ]
    
    direct_groups <- trimws(
      as.character(
        parent_rows[[child_col]]
      )
    )
    
    direct_groups <- unique(
      direct_groups[
        !is.na(direct_groups) &
          nzchar(direct_groups) &
          direct_groups != root_code
      ]
    )
    
    get_group_members <- function(group_code_i) {
      
      group_rows <- parent_rows[
        trimws(
          as.character(
            get(child_col)
          )
        ) == group_code_i
      ]
      
      member_cols <- id_cols[
        (parent_level + 1L):length(id_cols)
      ]
      
      member_codes <- unlist(
        group_rows[, ..member_cols],
        recursive = TRUE,
        use.names = FALSE
      )
      
      member_codes <- trimws(
        as.character(member_codes)
      )
      
      unique(
        member_codes[
          !is.na(member_codes) &
            nzchar(member_codes)
        ]
      )
    }
    
  } else {
    
    # Existing behaviour for ASFIS, geographical area
    # and production source.
    direct_groups <-
      get_direct_children_from_codelist_tree(
        tree_dt = tree_dt,
        parent_code = root_code
      )
    
    get_group_members <- function(group_code_i) {
      
      unique(
        c(
          group_code_i,
          get_descendants_from_codelist_tree(
            tree_dt = tree_dt,
            parent_code = group_code_i
          )
        )
      )
    }
  }
  
  direct_groups <- unique(
    trimws(
      as.character(direct_groups)
    )
  )
  
  direct_groups <- direct_groups[
    !is.na(direct_groups) &
      nzchar(direct_groups)
  ]
  
  if (length(direct_groups) == 0L) {
    stop(
      paste0(
        "The selected aggregation classification '",
        root_code,
        "' has no direct child groups."
      )
    )
  }
  
  aggregation_map <- rbindlist(
    lapply(
      direct_groups,
      function(group_code_i) {
        
        member_codes <- get_group_members(
          group_code_i
        )
        
        data.table(
          raw_code = as.character(member_codes),
          group_code = as.character(group_code_i)
        )
      }
    ),
    use.names = TRUE,
    fill = TRUE
  )
  
  aggregation_map <- unique(
    aggregation_map[
      !is.na(raw_code) &
        nzchar(raw_code) &
        !is.na(group_code) &
        nzchar(group_code)
    ]
  )
  
  ambiguous_codes <- aggregation_map[
    ,
    .(
      number_of_groups = uniqueN(group_code)
    ),
    by = raw_code
  ][number_of_groups > 1L]
  
  if (nrow(ambiguous_codes) > 0L) {
    
    ambiguous_details <- aggregation_map[
      raw_code %in% ambiguous_codes$raw_code,
      .(
        direct_groups = paste(
          sort(unique(group_code)),
          collapse = " | "
        )
      ),
      by = raw_code
    ][order(raw_code)]
    
    print(
      list(
        aggregation_root = root_code,
        ambiguous_details = ambiguous_details
      )
    )
    
    stop(
      paste0(
        "Some codes belong to more than one direct child ",
        "of aggregation root '",
        root_code,
        "'. See the R console for the exact codes."
      )
    )
  }
  
  aggregation_map[]
}

#Create separate aggregation groups from the selected hierarchy groups, 
#including the codes contained in each selected branch.
build_selected_groups_aggregation_map <- function(
    tree_dt,
    selected_codes,
    codelist_id = NULL
) {
  
  tree_dt <- as.data.table(
    tree_dt
  )
  
  selected_codes <- clean_code_vector(
    selected_codes
  )
  
  selected_codes <-
    keep_top_selected_codes_from_codelist_tree(
      selected_codes = selected_codes,
      tree_dt = tree_dt
    )
  
  if (length(selected_codes) == 0L) {
    stop(
      "No filter groups were selected."
    )
  }
  
  aggregation_map <- rbindlist(
    lapply(
      selected_codes,
      function(group_code_i) {
        
        member_codes <- get_hierarchy_branch_codes(
          tree_dt = tree_dt,
          root_code = group_code_i,
          codelist_id = codelist_id
        )
        
        data.table(
          raw_code = as.character(
            member_codes
          ),
          group_code = as.character(
            group_code_i
          )
        )
      }
    ),
    use.names = TRUE,
    fill = TRUE
  )
  
  aggregation_map <- unique(
    aggregation_map[
      !is.na(raw_code) &
        nzchar(raw_code) &
        !is.na(group_code) &
        nzchar(group_code)
    ]
  )
  
  ambiguous_codes <- aggregation_map[
    ,
    .(
      number_of_groups = uniqueN(
        group_code
      )
    ),
    by = raw_code
  ][number_of_groups > 1L]
  
  if (nrow(ambiguous_codes) > 0L) {
    stop(
      paste0(
        "Some selected groups overlap. ",
        "Do not select both a parent and one of its descendants."
      )
    )
  }
  
  aggregation_map[]
}

#Combine the selected hierarchy nodes and their branches into one custom aggregation group.
build_custom_aggregation_map <- function(
    tree_dt,
    selected_codes,
    aggregate_code,
    codelist_id = NULL
) {
  
  tree_dt <- as.data.table(
    tree_dt
  )
  
  selected_codes <- clean_code_vector(
    selected_codes
  )
  
  selected_codes <-
    keep_top_selected_codes_from_codelist_tree(
      selected_codes = selected_codes,
      tree_dt = tree_dt
    )
  
  if (length(selected_codes) == 0L) {
    stop(
      "No nodes were selected for the custom aggregation."
    )
  }
  
  member_codes <- unique(
    unlist(
      lapply(
        selected_codes,
        function(code_i) {
          
          get_hierarchy_branch_codes(
            tree_dt = tree_dt,
            root_code = code_i,
            codelist_id = codelist_id
          )
        }
      ),
      use.names = FALSE
    )
  )
  
  member_codes <- clean_code_vector(
    member_codes
  )
  
  data.table(
    raw_code = member_codes,
    group_code = as.character(
      aggregate_code
    )
  )
}

#Create outputs for the selected filter groups and assign all remaining filtered categories to the Other group.
build_selected_groups_map_with_remainder <- function(
    tree_dt,
    selected_codes,
    filtered_raw_codes,
    codelist_id = NULL,
    remainder_label = "Other filtered records"
) {
  
  filtered_raw_codes <- clean_code_vector(
    filtered_raw_codes
  )
  
  aggregation_map <-
    build_selected_groups_aggregation_map(
      tree_dt = tree_dt,
      selected_codes = selected_codes,
      codelist_id = codelist_id
    )
  
  aggregation_map <- aggregation_map[
    raw_code %in% filtered_raw_codes
  ]
  
  mapped_codes <- unique(
    as.character(
      aggregation_map$raw_code
    )
  )
  
  remainder_codes <- setdiff(
    filtered_raw_codes,
    mapped_codes
  )
  
  if (length(remainder_codes) > 0L) {
    
    aggregation_map <- rbindlist(
      list(
        aggregation_map,
        
        data.table(
          raw_code = remainder_codes,
          group_code = as.character(
            remainder_label
          )
        )
      ),
      use.names = TRUE,
      fill = TRUE
    )
  }
  
  unique(
    aggregation_map[
      !is.na(raw_code) &
        nzchar(raw_code) &
        !is.na(group_code) &
        nzchar(group_code)
    ],
    by = "raw_code"
  )
}

#Create the custom aggregation group and assign all remaining filtered categories to the Other group.
build_custom_map_with_remainder <- function(
    tree_dt,
    selected_codes,
    aggregate_code,
    filtered_raw_codes,
    codelist_id = NULL,
    remainder_label = "Other filtered records",
    allow_remainder_only = FALSE
) {
  
  filtered_raw_codes <- clean_code_vector(
    filtered_raw_codes
  )
  
  custom_map <- build_custom_aggregation_map(
    tree_dt = tree_dt,
    selected_codes = selected_codes,
    aggregate_code = aggregate_code,
    codelist_id = codelist_id
  )
  
  custom_map <- custom_map[
    raw_code %in% filtered_raw_codes
  ]
  
  if (
    nrow(custom_map) == 0L &&
    !isTRUE(allow_remainder_only)
  ) {
    stop(
      "None of the selected custom-aggregation nodes is present in the filtered data."
    )
  }
  
  custom_codes <- unique(
    as.character(
      custom_map$raw_code
    )
  )
  
  remainder_codes <- setdiff(
    filtered_raw_codes,
    custom_codes
  )
  
  remainder_map <- data.table(
    raw_code = character(0),
    group_code = character(0)
  )
  
  if (length(remainder_codes) > 0L) {
    
    remainder_map <- data.table(
      raw_code = remainder_codes,
      group_code = as.character(
        remainder_label
      )
    )
  }
  
  unique(
    rbindlist(
      list(
        custom_map,
        remainder_map
      ),
      use.names = TRUE,
      fill = TRUE
    ),
    by = "raw_code"
  )
}

#Aggregate one dimension according to the aggregation map, 
#sum values, and calculate the resulting observation and method flags.
aggregate_by_codelist <- function(
    data,
    key_dim_name,
    aggregation_map,
    value_col = "Value",
    observation_flag = "flagObservationStatus",
    method_flag = "flagMethod",
    group_by_observation_flag = FALSE,
    value_digits = VALUE_DECIMAL_DIGITS
) {
  
  dt <- copy(data)
  
  technical_cols <- intersect(
    c("raw_code", "group_code"),
    names(dt)
  )
  
  if (length(technical_cols) > 0) {
    dt[, (technical_cols) := NULL]
  }
  
  original_cols <- names(dt)
  
  if (!value_col %in% names(dt)) {
    stop(
      sprintf(
        "Column '%s' was not found in the selected dataset.",
        value_col
      )
    )
  }
  
  if (!key_dim_name %in% names(dt)) {
    stop(
      sprintf(
        "Column '%s' was not found in the selected dataset.",
        key_dim_name
      )
    )
  }
  
  if (!observation_flag %in% names(dt)) {
    stop(
      sprintf(
        "Column '%s' was not found in the selected dataset.",
        observation_flag
      )
    )
  }
  
  if (!method_flag %in% names(dt)) {
    stop(
      sprintf(
        "Column '%s' was not found in the selected dataset.",
        method_flag
      )
    )
  }
  
  aggregation_map <- as.data.table(
    aggregation_map
  )
  
  if (
    !all(
      c("raw_code", "group_code") %in%
      names(aggregation_map)
    )
  ) {
    stop(
      "The aggregation map must contain raw_code and group_code."
    )
  }
  
  aggregation_map <- unique(
    aggregation_map[
      ,
      .(
        raw_code = as.character(raw_code),
        group_code = as.character(group_code)
      )
    ]
  )
  
  dt[
    ,
    raw_code := as.character(
      get(key_dim_name)
    )
  ]
  
  
  mapped_codes <- unique(
    as.character(
      aggregation_map$raw_code
    )
  )
  
  data_codes <- unique(
    as.character(
      dt$raw_code
    )
  )
  
  unmapped_codes <- setdiff(
    data_codes,
    mapped_codes
  )
  
  if (length(unmapped_codes) > 0) {
    stop(
      paste0(
        "The selected aggregation hierarchy does not contain ",
        length(unmapped_codes),
        " filtered code(s) for dimension '",
        key_dim_name,
        "'. No records were aggregated because otherwise those codes ",
        "would be silently discarded. Unmapped codes: ",
        paste(
          head(unmapped_codes, 20),
          collapse = ", "
        ),
        if (length(unmapped_codes) > 20) {
          paste0(
            " ... and ",
            length(unmapped_codes) - 20,
            " more."
          )
        } else {
          ""
        }
      )
    )
  }
  
  dt_child <- merge(
    dt,
    aggregation_map,
    by = "raw_code",
    all = FALSE,
    sort = FALSE
  )
  
  if (nrow(dt_child) == 0) {
    stop(
      paste0(
        "No rows could be assigned to the selected ",
        "aggregation classification for dimension '",
        key_dim_name,
        "'."
      )
    )
  }
  
  dt_child[
    ,
    (key_dim_name) := NULL
  ]
  
  observation_group_col <- "__observation_flag_group__"
  
  if (isTRUE(group_by_observation_flag)) {
    dt_child[
      ,
      (observation_group_col) := as.character(
        get(observation_flag)
      )
    ]
  }
  
  keys_by <- c(
    "group_code",
    
    if (isTRUE(group_by_observation_flag)) {
      observation_group_col
    } else {
      character(0)
    },
    
    setdiff(
      names(dt_child),
      c(
        value_col,
        observation_flag,
        method_flag,
        "raw_code",
        "group_code",
        observation_group_col
      )
    )
  )
  
  keys_by <- unique(keys_by)
  
  missing_key_value <- "__MISSING_KEY__"
  
  key_cols <- intersect(
    keys_by,
    names(dt_child)
  )
  
  for (col in key_cols) {
    dt_child[
      ,
      (col) := as.character(
        get(col)
      )
    ]
    
    dt_child[
      is.na(get(col)) |
        !nzchar(get(col)),
      (col) := missing_key_value
    ]
  }
  
  
  deterministic_order_cols <- intersect(
    c(
      keys_by,
      observation_flag,
      method_flag,
      value_col
    ),
    names(dt_child)
  )
  
  if (
    length(deterministic_order_cols) > 0 &&
    nrow(dt_child) > 0
  ) {
    setorderv(
      dt_child,
      cols = deterministic_order_cols
    )
  }
  
  # Calculate the numerical total directly from the input records.
  # faoswsFlag is still used to calculate the resulting flags.
  direct_value_totals <- dt_child[
    ,
    .(
      direct_value_total = sum_or_na_rounded(
        get(value_col),
        digits = value_digits
      )
    ),
    by = keys_by
  ]
  
  out <- faoswsFlag::aggregateFlagData(
    data = dt_child,
    keys = keys_by,
    observationFlag = observation_flag,
    methodFlag = method_flag,
    forceMissingAggregateValues = FALSE,
    includeMissingFlags = TRUE
  )
  
  out <- unique(
    as.data.table(out)
  )
  
  if (!value_col %in% names(out)) {
    stop(
      paste0(
        "faoswsFlag did not return the expected value column '",
        value_col,
        "'."
      )
    )
  }
  
  # Retain the flags produced by faoswsFlag, but use the
  # direct sum calculated from the actual input records.
  setnames(
    out,
    value_col,
    "faosws_value"
  )
  
  out <- merge(
    out,
    direct_value_totals,
    by = keys_by,
    all.x = TRUE,
    sort = FALSE
  )
  
  out[
    ,
    (value_col) := direct_value_total
  ]
  
  out[
    ,
    c(
      "faosws_value",
      "direct_value_total"
    ) := NULL
  ]
  
  out <- out[
    !is.na(get(value_col))
  ]
  
  
  for (
    col in intersect(
      key_cols,
      names(out)
    )
  ) {
    out[
      get(col) == missing_key_value,
      (col) := NA_character_
    ]
  }
  
  if (
    isTRUE(group_by_observation_flag) &&
    observation_group_col %in% names(out)
  ) {
    out[
      ,
      (observation_flag) := as.character(
        get(observation_group_col)
      )
    ]
    
    out[, (observation_group_col) := NULL]
  }
  
  setnames(
    out,
    "group_code",
    key_dim_name,
    skip_absent = TRUE
  )
  
  if ("raw_code" %in% names(out)) {
    out[, raw_code := NULL]
  }
  
  missing_cols <- setdiff(
    original_cols,
    names(out)
  )
  
  for (col in missing_cols) {
    out[, (col) := NA]
  }
  
  setcolorder(
    out,
    original_cols
  )
  
  out[]
}

#Aggregate records separately by observation flag while preserving the flag categories in the output.
aggregate_rows_by_observation_flag <- function(
    data,
    value_col = "Value",
    observation_flag = "flagObservationStatus",
    method_flag = "flagMethod"
) {
  dt <- copy(data)
  original_cols <- names(dt)
  
  value_digits <- VALUE_DECIMAL_DIGITS
  
  if (!value_col %in% names(dt)) {
    stop(
      sprintf(
        "Column '%s' was not found in the selected dataset.",
        value_col
      )
    )
  }
  
  
  if (!observation_flag %in% names(dt)) {
    stop(
      sprintf(
        "Column '%s' was not found in the selected dataset.",
        observation_flag
      )
    )
  }
  
  added_method_flag <- FALSE
  
  if (!method_flag %in% names(dt)) {
    method_flag <- "__methodFlag"
    dt[, (method_flag) := NA_character_]
    added_method_flag <- TRUE
  }
  
  observation_group_col <- "__observation_flag_group__"
  
  dt[
    ,
    (observation_group_col) := as.character(
      get(observation_flag)
    )
  ]
  
  keys_by <- c(
    observation_group_col,
    
    setdiff(
      names(dt),
      c(
        value_col,
        observation_flag,
        method_flag,
        observation_group_col
      )
    )
  )
  
  keys_by <- unique(keys_by)
  
  missing_key_value <- "__MISSING_KEY__"
  key_cols <- intersect(keys_by, names(dt))
  
  for (col in key_cols) {
    dt[, (col) := as.character(get(col))]
    
    dt[
      is.na(get(col)) | !nzchar(get(col)),
      (col) := missing_key_value
    ]
  }
  
  
  deterministic_order_cols <- intersect(
    c(
      keys_by,
      observation_flag,
      method_flag,
      value_col
    ),
    names(dt)
  )
  
  if (
    length(deterministic_order_cols) > 0 &&
    nrow(dt) > 0
  ) {
    setorderv(
      dt,
      cols = deterministic_order_cols
    )
  }
  
  direct_value_totals <- dt[
    ,
    .(
      direct_value_total = sum_or_na_rounded(
        get(value_col),
        digits = value_digits
      )
    ),
    by = keys_by
  ]
  
  out <- faoswsFlag::aggregateFlagData(
    data = dt,
    keys = keys_by,
    observationFlag = observation_flag,
    methodFlag = method_flag,
    forceMissingAggregateValues = FALSE,
    includeMissingFlags = TRUE
  )
  
  out <- unique(
    as.data.table(out)
  )
  
  if (!value_col %in% names(out)) {
    stop(
      paste0(
        "faoswsFlag did not return the expected value column '",
        value_col,
        "'."
      )
    )
  }
  
  setnames(
    out,
    value_col,
    "faosws_value"
  )
  
  out <- merge(
    out,
    direct_value_totals,
    by = keys_by,
    all.x = TRUE,
    sort = FALSE
  )
  
  out[
    ,
    (value_col) := direct_value_total
  ]
  
  out[
    ,
    c(
      "faosws_value",
      "direct_value_total"
    ) := NULL
  ]
  
  out <- out[
    !is.na(get(value_col))
  ]
  
  for (col in intersect(key_cols, names(out))) {
    out[
      get(col) == missing_key_value,
      (col) := NA_character_
    ]
  }
  
  # Restore the observation flag used as the grouping category.
  out[
    ,
    (observation_flag) := as.character(
      get(observation_group_col)
    )
  ]
  
  out[, (observation_group_col) := NULL]
  
  if (
    isTRUE(added_method_flag) &&
    method_flag %in% names(out)
  ) {
    out[, (method_flag) := NULL]
  }
  
  missing_cols <- setdiff(
    original_cols,
    names(out)
  )
  
  for (col in missing_cols) {
    out[, (col) := NA]
  }
  
  setcolorder(out, original_cols)
  
  out[]
}

#Restrict the dataset to the selected inclusive year range.
filter_data_by_year <- function(data,
                                year_range = NULL,
                                year_col = "timePointYears") {
  dt <- copy(data)
  
  if (!year_col %in% names(dt)) {
    return(dt)
  }
  
  if (is.null(year_range) || length(year_range) != 2) {
    return(dt)
  }
  
  dt[, year_tmp := as.integer(get(year_col))]
  
  dt <- dt[
    year_tmp >= as.integer(year_range[1]) &
      year_tmp <= as.integer(year_range[2])
  ]
  
  dt[, year_tmp := NULL]
  
  dt[]
}

#Restrict the dataset to the selected measured elements.
filter_data_by_measured_element <- function(data,
                                            measured_elements = NULL,
                                            measured_element_col = "measuredElement") {
  dt <- copy(data)
  
  if (is.null(measured_elements) || length(measured_elements) == 0) {
    return(dt)
  }
  
  if (!measured_element_col %in% names(dt)) {
    return(dt)
  }
  
  measured_elements <- as.character(measured_elements)
  
  dt[
    as.character(get(measured_element_col)) %in% measured_elements
  ]
}

#Restrict a dataset column to the selected values.
filter_data_by_selected_values <- function(data,
                                           selected_values = NULL,
                                           column_name = NULL) {
  dt <- copy(data)
  
  if (is.null(column_name) || !column_name %in% names(dt)) {
    return(dt)
  }
  
  if (is.null(selected_values) || length(selected_values) == 0) {
    return(dt)
  }
  
  selected_values <- as.character(selected_values)
  
  dt[
    as.character(get(column_name)) %in% selected_values
  ]
}

#Apply the selected aggregation rules across multiple dimensions and 
#optionally aggregate years or group separately by observation flag.
aggregate_by_multiple_dimensions <- function(
    data,
    aggregation_specs,
    value_col = "Value",
    observation_flag = "flagObservationStatus",
    method_flag = "flagMethod",
    group_by_observation_flag = FALSE,
    aggregate_selected_years = FALSE,
    year_col = "timePointYears",
    year_total_label = "Selected period"
) {
  
  dt <- copy(data)
  
  if (
    length(aggregation_specs) == 0 &&
    !isTRUE(group_by_observation_flag) &&
    !isTRUE(aggregate_selected_years)
  ) {
    return(dt[])
  }
  
  aggregation_order <- c(
    "geographical_area",
    "production_source",
    "species",
    "fishing_area"
  )
  
  aggregation_order <- aggregation_order[
    aggregation_order %in%
      names(aggregation_specs)
  ]
  
  for (dim_id in aggregation_order) {
    
    spec <- aggregation_specs[[dim_id]]
    aggregation_map_i <- spec$aggregation_map
    
    if (identical(
      spec$aggregation_mode,
      "total"
    )) {
      
      raw_values <- unique(
        as.character(
          dt[[spec$key_dim_name]]
        )
      )
      
      raw_values <- raw_values[
        !is.na(raw_values) &
          nzchar(raw_values)
      ]
      
      aggregation_map_i <- data.table(
        raw_code = raw_values,
        group_code = as.character(
          spec$aggregate_code %||% "TOTAL"
        )
      )
    }
    
    dt <- with_aggregation_context(
      
      aggregate_by_codelist(
        data = dt,
        key_dim_name = spec$key_dim_name,
        aggregation_map = aggregation_map_i,
        value_col = value_col,
        observation_flag = observation_flag,
        method_flag = method_flag,
        group_by_observation_flag =
          group_by_observation_flag
      ),
      
      dimension_id =
        spec$dimension %||%
        dim_id,
      
      label =
        spec$label %||%
        dim_id,
      
      dataset_column =
        spec$key_dim_name,
      
      codelist =
        spec$codelist,
      
      selected_codes =
        spec$selected_codes %||%
        spec$aggregate_code %||%
        spec$root_code,
      
      stage = paste0(
        spec$aggregation_mode,
        " aggregation execution"
      )
    )
  }
  
  if (
    isTRUE(aggregate_selected_years) &&
    year_col %in% names(dt)
  ) {
    
    years_present <- unique(
      as.character(
        dt[[year_col]]
      )
    )
    
    years_present <- years_present[
      !is.na(years_present) &
        nzchar(years_present)
    ]
    
    dt <- aggregate_by_codelist(
      data = dt,
      key_dim_name = year_col,
      
      aggregation_map = data.table(
        raw_code = years_present,
        group_code = as.character(
          year_total_label
        )
      ),
      
      value_col = value_col,
      observation_flag = observation_flag,
      method_flag = method_flag,
      group_by_observation_flag =
        group_by_observation_flag
    )
  }
  
  # Needed when observation flag is the only aggregation selected.
  if (
    isTRUE(group_by_observation_flag) &&
    length(aggregation_order) == 0 &&
    !isTRUE(aggregate_selected_years)
  ) {
    dt <- aggregate_rows_by_observation_flag(
      data = dt,
      value_col = value_col,
      observation_flag = observation_flag,
      method_flag = method_flag
    )
  }
  
  technical_cols <- intersect(
    c("raw_code", "group_code"),
    names(dt)
  )
  
  if (length(technical_cols) > 0) {
    dt[, (technical_cols) := NULL]
  }
  
  dt[]
}


#Extract the distinct available years from the dataset and return them in sorted order.
get_year_values <- function(data, year_col = "timePointYears") {
  dt <- copy(data)
  
  if (!year_col %in% names(dt)) {
    return(integer(0))
  }
  
  years <- suppressWarnings(as.integer(dt[[year_col]]))
  years <- sort(unique(years))
  years <- years[!is.na(years)]
  
  years
}

#Calculate the total value and number of records for each year or aggregated period.
summarise_total_by_year <- function(
    data,
    year_col = "timePointYears",
    value_col = "Value"
) {
  dt <- copy(data)
  
  if (!year_col %in% names(dt)) {
    return(data.table())
  }
  
  if (!value_col %in% names(dt)) {
    return(data.table())
  }
  
  dt[
    ,
    period_tmp :=
      normalise_period_label(
        get(year_col)
      )
  ]
  
  dt <- dt[!is.na(period_tmp)]
  
  if (nrow(dt) == 0) {
    return(data.table())
  }
  
  out <- dt[
    ,
    .(
      total_value = sum_or_na(
        get(value_col)
      ),
      n_rows = .N
    ),
    by = .(
      year = period_tmp
    )
  ]
  
  out[
    ,
    year_order :=
      period_start_value(year)
  ]
  
  setorder(
    out,
    year_order,
    year
  )
  
  out[, year_order := NULL]
  
  out[]
}

#Standardize year or period labels and convert empty values to missing values.
normalise_period_label <- function(x) {
  out <- trimws(as.character(x))
  out[is.na(out) | !nzchar(out)] <- NA_character_
  out
}

#Extract the starting numeric year from a year or period label for ordering purposes.
period_start_value <- function(x) {
  suppressWarnings(
    as.numeric(
      sub(
        "^\\s*([0-9]+).*$",
        "\\1",
        as.character(x)
      )
    )
  )
}


#Calculate total values and record counts by year and measured element.
summarise_total_by_year_and_element <- function(data,
                                                year_col = "timePointYears",
                                                value_col = "Value",
                                                measured_element_col = "measuredElement") {
  dt <- copy(data)
  
  if (!year_col %in% names(dt)) {
    return(data.table())
  }
  
  if (!value_col %in% names(dt)) {
    return(data.table())
  }
  
  if (!measured_element_col %in% names(dt)) {
    return(data.table())
  }
  
  dt[, year_tmp := suppressWarnings(as.integer(get(year_col)))]
  dt <- dt[!is.na(year_tmp)]
  
  dt[, measured_element_tmp := as.character(get(measured_element_col))]
  dt <- dt[!is.na(measured_element_tmp) & nzchar(measured_element_tmp)]
  
  if (nrow(dt) == 0) {
    return(data.table())
  }
  
  dt[
    ,
    .(
      total_value = sum_or_na(get(value_col)),
      n_rows = .N
    ),
    by = .(
      year = year_tmp,
      measured_element = measured_element_tmp
    )
  ][order(measured_element, year)]
}

#Calculate yearly totals for each component defined by the aggregation map.
summarise_composition_by_year <- function(
    data,
    aggregation_spec,
    codes,
    tree_dt,
    year_col = "timePointYears",
    value_col = "Value"
) {
  dt <- copy(data)
  
  if (is.null(aggregation_spec)) {
    return(data.table())
  }
  
  key_col <- aggregation_spec$key_dim_name
  
  if (!year_col %in% names(dt)) {
    return(data.table())
  }
  
  if (!value_col %in% names(dt)) {
    return(data.table())
  }
  
  if (!key_col %in% names(dt)) {
    return(data.table())
  }
  
  # Use the actual aggregation map that produced the result.
  component_map <- as.data.table(
    copy(
      aggregation_spec$aggregation_map
    )
  )
  
  if (
    !all(
      c("raw_code", "group_code") %in%
      names(component_map)
    )
  ) {
    return(data.table())
  }
  
  component_map <- unique(
    component_map[
      ,
      .(
        raw_code = as.character(raw_code),
        component_code = as.character(group_code)
      )
    ]
  )
  
  dt[
    ,
    period_tmp :=
      normalise_period_label(
        get(year_col)
      )
  ]
  
  dt[
    ,
    raw_code := as.character(
      get(key_col)
    )
  ]
  
  dt <- dt[
    !is.na(period_tmp) &
      raw_code %in%
      component_map$raw_code
  ]
  
  if (nrow(dt) == 0) {
    return(data.table())
  }
  
  dt <- merge(
    dt,
    component_map,
    by = "raw_code",
    all.x = FALSE,
    all.y = FALSE,
    sort = FALSE
  )
  
  composition <- dt[
    ,
    .(
      total_value = sum_or_na(
        get(value_col)
      )
    ),
    by = .(
      year = period_tmp,
      component_code
    )
  ]
  
  composition[
    ,
    year_order :=
      period_start_value(year)
  ]
  
  setorder(
    composition,
    year_order,
    year,
    component_code
  )
  
  composition[, year_order := NULL]
  
  composition[]
}

#Keep the largest categories and combine all remaining categories into Other.
collapse_top_categories <- function(plot_data,
                                    top_n = 10,
                                    category_col = "category",
                                    value_col = "total_value") {
  dt <- copy(plot_data)
  
  if (nrow(dt) == 0) {
    return(dt)
  }
  
  if (!all(c("year", category_col, value_col) %in% names(dt))) {
    return(data.table())
  }
  
  dt[, (category_col) := as.character(get(category_col))]
  dt[is.na(get(category_col)) | !nzchar(get(category_col)), (category_col) := "<missing>"]
  
  category_totals <- dt[
    ,
    .(
      period_total = sum(get(value_col), na.rm = TRUE)
    ),
    by = category_col
  ][order(-period_total)]
  
  keep_categories <- head(category_totals[[category_col]], top_n)
  
  dt[
    !get(category_col) %in% keep_categories,
    (category_col) := "Other"
  ]
  
  dt[
    ,
    .(
      total_value = sum(get(value_col), na.rm = TRUE)
    ),
    by = .(
      year,
      category = get(category_col)
    )
  ][order(year, category)]
}


#Restrict the data according to the aggregation settings of all dimensions except the selected one.
filter_data_to_other_aggregation_context <- function(data,
                                                     aggregation_specs,
                                                     selected_dim_id) {
  dt <- copy(data)
  
  if (is.null(aggregation_specs) || length(aggregation_specs) == 0) {
    return(dt)
  }
  
  for (dim_id in names(aggregation_specs)) {
    if (identical(dim_id, selected_dim_id)) {
      next
    }
    
    spec <- aggregation_specs[[dim_id]]
    
    if (!spec$key_dim_name %in% names(dt)) {
      next
    }
    
    dt[, (spec$key_dim_name) := as.character(get(spec$key_dim_name))]
    
    dt <- dt[
      get(spec$key_dim_name) %in% as.character(spec$child_codes)
    ]
  }
  
  dt[]
}

#Prepare treemap values for a selected year, keep the largest components, 
#group the rest as Other, and calculate their shares.
prepare_treemap_data <- function(composition,
                                 selected_year,
                                 top_n = 15) {
  dt <- copy(composition)
  
  if (nrow(dt) == 0) {
    return(data.table())
  }
  
  if (!all(c("year", "component_code", "total_value") %in% names(dt))) {
    return(data.table())
  }
  
  dt <- dt[year == selected_year]
  
  if (nrow(dt) == 0) {
    return(data.table())
  }
  
  dt <- dt[
    ,
    .(
      total_value = sum(total_value, na.rm = TRUE)
    ),
    by = component_code
  ]
  
  dt <- dt[!is.na(total_value) & total_value > 0]
  
  if (nrow(dt) == 0) {
    return(data.table())
  }
  
  component_totals <- dt[order(-total_value)]
  
  keep_components <- head(component_totals$component_code, top_n)
  
  dt[
    !component_code %in% keep_components,
    component_code := "Other"
  ]
  
  dt <- dt[
    ,
    .(
      total_value = sum(total_value, na.rm = TRUE)
    ),
    by = component_code
  ][order(-total_value)]
  
  dt[
    ,
    share := total_value / sum(total_value, na.rm = TRUE)
  ]
  
  dt[
    ,
    label := paste0(
      component_code,
      "\n",
      round(100 * share, 1),
      "%"
    )
  ]
  
  dt[]
}


#############################################
# UI
#############################################
ui <- page_navbar(
  title = "Fisheries Aggregation app",
  fillable = FALSE,
  
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#0072B2"
  ),
  
  tags$head(
    
    tags$style(
      HTML("
      .navbar {
        background-image:
        linear-gradient(rgba(0, 70, 120, 0.45), rgba(0, 70, 120, 0.45)),
        url('fisheries_header.jpg') !important;
        background-size: cover !important;
        background-position: center center !important;
        background-repeat: no-repeat !important;
        min-height: 110px;
        padding-top: 5px !important;
        padding-bottom: 15px !important;
        border-bottom: none !important;
       }

       .navbar > .container-fluid {
         min-height: unset !important;
         align-items: flex-start !important;
       }

      .navbar-brand {
        color: white !important;
        font-weight: bold !important;
        text-shadow: 1px 1px 3px rgba(0, 0, 0, 0.8);
      }

      .navbar-nav .nav-link {
        color: white !important;
        font-weight: 600 !important;
        text-shadow: 1px 1px 3px rgba(0, 0, 0, 0.8);
      }

      .navbar-nav .nav-link.active {
        background-color: rgba(255, 255, 255, 0.25) !important;
        border-radius: 6px;
      }

      .navbar-nav .nav-link:hover {
        background-color: rgba(255, 255, 255, 0.18) !important;
        border-radius: 6px;
      }
      
      
      body {
  background-image:
    linear-gradient(rgba(255, 255, 255, 0.70), rgba(255, 255, 255, 0.70)),
    url('fisheries_header.jpg') !important;
  background-size: cover !important;
  background-position: center center !important;
  background-repeat: no-repeat !important;
  background-attachment: fixed !important;
}

/* Keep cards and output windows readable */
.card {
  background-color: rgba(255, 255, 255, 0.96) !important;
  border-radius: 10px !important;
}

/* Keep sidebars readable */
.sidebar {
  background-color: rgba(255, 255, 255, 0.94) !important;
  border-radius: 10px !important;
}

/* Make main page area transparent so the sea background is visible */
.bslib-page-navbar,
.bslib-page-navbar > .container-fluid,
.tab-content {
  background-color: transparent !important;
}
    /* -------------------------------------------------------------
   Compact Filters & aggregation page
------------------------------------------------------------- */

.aggregation-page {
  padding: 0.2rem 0 0.75rem 0;
}

.aggregation-intro {
  padding: 0.45rem 0.7rem;
  margin-bottom: 0.5rem;
  background-color: rgba(255, 255, 255, 0.94);
  border: 1px solid rgba(0, 0, 0, 0.15);
  border-radius: 8px;
}

/* Compact centred year toolbar */
.year-toolbar {
  max-width: 1150px;
  margin: 0 auto 0.55rem auto;
  padding: 0.4rem 0.65rem 0.15rem 0.65rem;
  background-color: rgba(255, 255, 255, 0.94);
  border: 1px solid rgba(0, 0, 0, 0.15);
  border-radius: 8px;
}

.year-toolbar .form-group {
  margin-bottom: 0 !important;
}

.year-toolbar .shiny-text-output {
  margin-bottom: 0 !important;
}

.year-toolbar .irs {
  margin-top: -6px;
  margin-bottom: -10px;
}

.year-toolbar .btn {
  white-space: nowrap;
}

/* Remove the unnecessary vertical gap above the two columns */
.aggregation-columns {
  margin-top: 0 !important;
}

/* Make both cards fill their Bootstrap columns */
.aggregation-columns > div {
  display: flex;
}

.aggregation-columns .card {
  width: 100%;
  margin-bottom: 0 !important;
}

/* Slightly reduce excessive card padding */
.aggregation-columns .card-body {
  padding: 0.75rem !important;
}

/* Collapsible aggregation explanation */
.aggregation-help {
  margin-bottom: 0.75rem;
  padding: 0.6rem 0.75rem;
  background-color: white;
  border: 1px solid rgba(52, 152, 219, 0.45);
  border-radius: 7px;
}

.aggregation-help summary {
  cursor: pointer;
  font-weight: 700;
}

.aggregation-help-body {
  padding-top: 0.65rem;
}

.aggregation-help-body ul {
  margin-bottom: 0;
}

/* Selected values shown below each tree */
.tree-selection-toolbar {
  display: flex;
  align-items: flex-start;
  gap: 8px;
  margin-top: 6px;
}

.tree-selection-values {
  flex: 1 1 auto;
  min-width: 0;
}

.tree-selection-chips {
  display: flex;
  flex-wrap: wrap;
  gap: 5px;
}

.tree-selection-chip {
  position: relative;
  display: inline-block;
  max-width: 100%;
  padding: 4px 27px 4px 8px;
  border-radius: 12px;
  background-color: #e8f2f8;
  border: 1px solid #b8d5e5;
  font-size: 12px;
}

.tree-selection-chip-label {
  display: inline-block;
  max-width: 100%;
  overflow-wrap: anywhere;
}

.tree-selection-remove {
  position: absolute;
  top: 50%;
  right: 5px;
  transform: translateY(-50%);
  padding: 0;
  border: 0;
  background: transparent;
  font-size: 17px;
  font-weight: 700;
  line-height: 1;
  cursor: pointer;
}

.tree-selection-remove:hover {
  opacity: 0.65;
}

.tree-selection-clear {
  flex: 0 0 auto;
  white-space: nowrap;
}

.tree-selection-summary-block {
  padding-top: 8px;
}")
    ),
    
    tags$script(
      HTML("
  (function() {
    
    function getTree(treeId) {
      var element = $('#' + treeId);
      
      if (!element.length || !element.jstree(true)) {
        return null;
      }
      
      return {
        element: element,
        tree: element.jstree(true)
      };
    }
    
    
    function clearOneTree(treeId, closeTree) {
      var treeObject = getTree(treeId);
      
      if (!treeObject) {
        return false;
      }
      
      treeObject.tree.uncheck_all();
      treeObject.tree.deselect_all();
      
      if (closeTree === true) {
        treeObject.tree.close_all();
      }
      
      Shiny.setInputValue(
        treeId,
        null,
        {priority: 'event'}
      );
      
      return true;
    }
    
    
    function removeTreeNodeByLabel(treeId, nodeLabel) {
      var treeObject = getTree(treeId);
      
      if (!treeObject) {
        return false;
      }
      
      var targetLabel = String(nodeLabel || '').trim();
      var nodes = treeObject.tree.get_json('#', {flat: true});
      var removed = false;
      
      nodes.forEach(function(node) {
        if (String(node.text || '').trim() === targetLabel) {
          treeObject.tree.uncheck_node(node.id);
          treeObject.tree.deselect_node(node.id);
          removed = true;
        }
      });
      
      if (
        removed &&
        treeObject.tree.get_checked().length === 0 &&
        treeObject.tree.get_selected().length === 0
      ) {
        Shiny.setInputValue(
          treeId,
          null,
          {priority: 'event'}
        );
      }
      
      return removed;
    }
    
    
    Shiny.addCustomMessageHandler(
      'clear_shiny_trees',
      function(message) {
        var ids = message.ids || [];
        var attempts = 0;
        
        var timer = setInterval(function() {
          attempts = attempts + 1;
          
          ids.forEach(function(id) {
            clearOneTree(id, true);
          });
          
          if (attempts >= 20) {
            clearInterval(timer);
          }
        }, 100);
      }
    );
    
    
    $(document).on(
      'click',
      '.tree-selection-clear',
      function(event) {
        event.preventDefault();
        event.stopPropagation();
        
        clearOneTree(
          $(this).attr('data-tree-id'),
          false
        );
      }
    );
    
    
    $(document).on(
      'click',
      '.tree-selection-remove',
      function(event) {
        event.preventDefault();
        event.stopPropagation();
        
        removeTreeNodeByLabel(
          $(this).attr('data-tree-id'),
          $(this).attr('data-node-label')
        );
      }
    );
    
  })();
")
    )
  ),
  
  nav_panel(
    "1. Data selection",
    
    layout_sidebar(
      sidebar = sidebar(
        width = 360,
        
        actionButton(
          "initialise",
          "Initialise Client",
          class = "btn-primary",
          width = "100%"
        ),
        
        hr(),
        
        selectInput(
          "dataset_group",
          "Dataset group",
          choices = c(
            "Current Fisheries datasets" = "current",
            "Previous disseminated datasets" = "disseminated"
          ),
          selected = "current"
        ),
        
        selectizeInput(
          "dataset_id",
          "Dataset",
          choices = NULL,
          options = list(
            placeholder = "Initialise the client, then choose a dataset",
            maxOptions = 5000
          )
        ),
        
        textOutput("dataset_count"),
        
        actionButton(
          "load_dataset",
          "Load selected dataset",
          class = "btn-secondary",
          width = "100%"
        )
      ),
      
      layout_columns(
        col_widths = c(5, 7),
        
        card(
          card_header("Client"),
          tableOutput("client")
        ),
        
        card(
          card_header("Loaded dataset"),
          verbatimTextOutput("dataset_status")
        )
      )
    )
  ),
  
  nav_panel(
    "2. Dataset summary",
    
    layout_columns(
      col_widths = c(4, 8),
      
      card(
        card_header("Loaded dataset"),
        uiOutput("dataset_summary")
      ),
      
      card(
        full_screen = TRUE,
        card_header("Raw dataset time series"),
        plotOutput("raw_year_plot", height = 360)
      )
    ),
    
    card(
      full_screen = TRUE,
      card_header("Raw data preview"),
      DTOutput("raw_preview")
    )
  ),
  
  nav_panel(
    "3. Filters & aggregation",
    
    div(
      class = "aggregation-page",
      
      div(
        class = "aggregation-intro",
        
        tags$strong("Aggregation setup. "),
        
        paste0(
          "Select the years and filters, define how the filtered records ",
          "should be aggregated, and then run the aggregation."
        )
      ),
      
      # Compact horizontal year toolbar.
      div(
        class = "year-toolbar",
        uiOutput("year_selector")
      ),
      
      # Filters and aggregation organized by dimension.
      uiOutput("dimension_accordion_ui"),
      
      actionButton(
        "run_aggregation",
        "Run aggregation",
        class = "btn-success",
        width = "100%"
      )
    )
  ),
  
  
  nav_panel(
    "4. Dataset table",
    
    card(
      full_screen = TRUE,
      card_header("Aggregated data preview"),
      uiOutput("aggregated_outputs_tables")
    )
  ),
  
  nav_panel(
    "5. Graphs",
    
    layout_sidebar(
      sidebar = sidebar(
        width = 320,
        
        uiOutput("graph_output_selector"),
        
        hr(),
        
        conditionalPanel(
          condition = "input.graph_tab != 'total'",
          
          uiOutput("plot_category_selector"),
          
          helpText(
            "Choose the category used for colours/components in the composition plots."
          )
        ),
        
        conditionalPanel(
          condition = "input.graph_tab == 'treemap'",
          uiOutput("treemap_year_selector")
        )
      ),
      
      navset_card_tab(
        id = "graph_tab",
        
        nav_panel(
          "Aggregated total",
          value = "total",
          plotOutput("aggregated_year_plot", height = 420)
        ),
        
        nav_panel(
          "Stacked bar chart",
          value = "stacked",
          plotOutput("composition_plot", height = 440)
        ),
        
        nav_panel(
          "Relative stacked bar chart",
          value = "relative",
          plotOutput("relative_composition_plot", height = 440)
        ),
        
        nav_panel(
          "Component time series",
          value = "component_series",
          plotOutput("component_time_series_plot", height = 440)
        ),
        
        nav_panel(
          "Treemap chart",
          value = "treemap",
          plotOutput("treemap_plot", height = 560)
        )
      )
    )
  ),
  
  nav_panel(
    "6. Comparison",
    
    # This message is displayed when comparison is not allowed.
    conditionalPanel(
      condition = "!output.comparison_available",
      
      card(
        card_header("Comparison unavailable"),
        
        p(
          "Comparison can be run only when the main loaded dataset is a ",
          "current Fisheries dataset."
        ),
        
        p(
          "The loaded dataset is disseminated, so it can still be filtered, ",
          "aggregated, plotted and analysed for outliers, but it cannot be used ",
          "as the main dataset on the Comparison page."
        ),
        
        p(
          "To run a comparison, return to Data selection, load the corresponding ",
          "current dataset, run the aggregation, and then compare it with either ",
          "a disseminated dataset or one of its tagged datasets."
        )
      )
    ),
    
    # All normal comparison controls are displayed only for current datasets.
    conditionalPanel(
      condition = "output.comparison_available",
      
      card(
        card_header("Current vs comparison dataset"),
        
        p(
          "This page allows the user to select a comparison dataset either from ",
          "the disseminated domain or from the list of tagged datasets available ",
          "for the currently loaded Fisheries dataset."
        )
      ),
      
      layout_sidebar(
        sidebar = sidebar(
          width = 360,
          
          selectInput(
            "comparison_source",
            "Comparison dataset source",
            choices = c(
              "Disseminated domain" = "disseminated",
              "Tagged datasets" = "tagged"
            ),
            selected = "disseminated"
          ),
          
          uiOutput("comparison_dataset_selector"),
          
          actionButton(
            "load_comparison_dataset",
            "Load comparison dataset",
            class = "btn-secondary",
            width = "100%"
          ),
          
          uiOutput("comparison_compatibility_warning_ui"),
          
          hr(),
          
          actionButton(
            "run_comparison",
            "Run comparison",
            class = "btn-success",
            width = "100%"
          )
        ),
        
        layout_columns(
          col_widths = c(5, 7),
          
          card(
            card_header("Comparison dataset status"),
            verbatimTextOutput("comparison_status")
          ),
          
          card(
            full_screen = TRUE,
            card_header("Comparison dataset measured elements"),
            DTOutput("comparison_measured_elements_summary")
          )
        ),
        
        uiOutput("comparison_code_warning_ui"),
        
        navset_card_tab(
          id = "comparison_result_view",
          
          nav_panel(
            "All comparison rows",
            uiOutput("comparison_results_tables")
          ),
          
          nav_panel(
            "Differences only",
            uiOutput("comparison_differences_tables")
          )
        )
      )
    )
  ),
  
  
  
  nav_panel(
    "7. Outlier analysis",
    
    div(
      class = "alert alert-info",
      "Outlier analysis is used here as a screening tool to explore unusually large values and year-on-year changes in the aggregated output, rather than as a formal statistical outlier-detection procedure."
    ),
    
    layout_sidebar(
      sidebar = sidebar(
        width = 320,
        
        uiOutput("outlier_output_selector"),
        
        hr(),
        
        uiOutput("outlier_group_selector"),
        uiOutput("outlier_group_value_selector"),
        
        uiOutput("outlier_metric_selector"),
        
        conditionalPanel(
          condition = "input.outlier_tab == 'largest'",
          
          sliderInput(
            "outlier_top_n",
            "Number of top records to display",
            min = 10,
            max = 200,
            value = 50,
            step = 10
          )
        ),
        
        helpText(
          "Use the group selector to inspect outliers within comparable groups",
          "for example within one country, species group or fishing area."
        )
      ),
      
      navset_card_tab(
        id = "outlier_tab",
        
        nav_panel(
          "Distribution by year",
          value = "distribution",
          
          card(
            full_screen = TRUE,
            card_header("Distribution by year"),
            plotOutput("outlier_boxplot", height = 520)
          ),
          
          br(),
          
          card(
            card_header("How to read this plot"),
            uiOutput("outlier_distribution_explanation")
          )
        ),
        
        nav_panel(
          "Largest records",
          value = "largest",
          
          card(
            full_screen = TRUE,
            
            card_header(
              "Largest records"
            ),
            
            card_body(
              DTOutput("largest_values_table")
            ),
            
            card_footer(
              downloadButton(
                "download_outlier_table",
                "Download outlier CSV"
              )
            )
          ),
          
          br(),
          
          card(
            card_header("How to read this table"),
            uiOutput("outlier_table_explanation")
          )
        )
      )
    )
  )
)

# Is the codelist code active or it has an end date?
# Active codes usually have end_date = NA or an empty value
is_active_codelist_code <- function(codes) {
  codes <- as.data.table(codes)
  
  # If the codelist has no end_date column, I can not identify expired codes.
  # In that case, keep all codes.
  if (!"end_date" %in% names(codes)) {
    return(rep(TRUE, nrow(codes)))
  }
  
  # Convert end_date to character to handle both numeric timestamps and strings
  end_date_chr <- trimws(as.character(codes$end_date))
  # A code is considered active only when end_date is missing or empty.
  return(is.na(end_date_chr) | !nzchar(end_date_chr))
}


drop_expired_codelist_codes <- function(codes) {
  codes <- as.data.table(codes)
  codes[, id := as.character(id)]
  
  active <- is_active_codelist_code(codes)
  active_ids <- codes[active, id]
  
  codes <- codes[active]
  
  # Also clean the children lists, otherwise an active parent could still point
  # to expired children.
  if ("children" %in% names(codes)) {
    codes[, children := lapply(children, function(x) {
      child_ids <- extract_child_ids(x)
      child_ids <- child_ids[child_ids %in% active_ids]
      
      if (length(child_ids) == 0) {
        return(NULL)
      }
      
      child_ids
    })]
  }
  
  codes[]
}

#Remove hierarchy paths containing expired codelist codes.
drop_expired_codes_from_codelist_tree <- function(tree_dt, active_codes) {
  tree_dt <- as.data.table(tree_dt)
  active_codes <- as.data.table(active_codes)
  
  active_ids <- unique(as.character(active_codes$id))
  id_cols <- get_tree_id_cols(tree_dt)
  
  if (length(id_cols) == 0) {
    return(tree_dt)
  }
  
  keep <- rep(TRUE, nrow(tree_dt))
  
  for (col in id_cols) {
    vals <- as.character(tree_dt[[col]])
    
    keep <- keep & (
      is.na(vals) |
        !nzchar(vals) |
        vals %in% active_ids
    )
  }
  
  tree_dt[keep]
}



# -------------------------------------------------------------------------
# Expired codelists and artificial hierarchy roots
# -------------------------------------------------------------------------

KEEP_EXPIRED_CODELISTS <- c(
  "geographicAreaM49_fi",
  "fisheriesAsfis",
  "fisheriesCatchArea"
)

CODELISTS_WITH_ALL_ROOT <- c(
  "fisheriesAsfis",
  "fisheriesCatchArea"
)

SYNTHETIC_ALL_ROOT_ID <- "__ALL__"
SYNTHETIC_EXPIRED_ROOTS_ID <- "__EXPIRED_ROOTS__"

#Check whether a codelist uses the additional synthetic All hierarchy root.
uses_augmented_all_root <- function(codelist_id) {
  as.character(codelist_id) %in%
    CODELISTS_WITH_ALL_ROOT
}

#Retrieve the original top-level codes from a codelist tree.
get_original_tree_roots <- function(tree_dt) {
  
  tree_dt <- as.data.table(tree_dt)
  
  id_cols <- get_tree_id_cols(tree_dt)
  
  if (
    length(id_cols) == 0L ||
    nrow(tree_dt) == 0L
  ) {
    return(character(0))
  }
  
  roots <- trimws(
    as.character(
      tree_dt[[id_cols[1L]]]
    )
  )
  
  unique(
    roots[
      !is.na(roots) &
        nzchar(roots)
    ]
  )
}

#Separate the original hierarchy roots into current and expired roots.
get_current_and_expired_tree_roots <- function(
    tree_dt,
    codes
) {
  
  codes <- copy(as.data.table(codes))
  codes[, id := trimws(as.character(id))]
  
  original_roots <- get_original_tree_roots(
    tree_dt
  )
  
  active_status <- is_active_codelist_code(
    codes
  )
  
  expired_ids <- codes[
    !active_status &
      !is.na(id) &
      nzchar(id),
    id
  ]
  
  expired_roots <- intersect(
    original_roots,
    expired_ids
  )
  
  current_roots <- setdiff(
    original_roots,
    expired_roots
  )
  
  list(
    current_roots = unique(current_roots),
    expired_roots = unique(expired_roots)
  )
}

#Add one or more hierarchy levels before the existing levels of a codelist tree.
prepend_tree_levels <- function(
    tree_dt,
    prefix_codes
) {
  
  tree_dt <- copy(as.data.table(tree_dt))
  
  old_id_cols <- get_tree_id_cols(tree_dt)
  
  if (
    length(old_id_cols) == 0L ||
    nrow(tree_dt) == 0L
  ) {
    return(data.table())
  }
  
  prefix_codes <- trimws(
    as.character(prefix_codes)
  )
  
  prefix_codes <- prefix_codes[
    !is.na(prefix_codes) &
      nzchar(prefix_codes)
  ]
  
  out <- copy(
    tree_dt[, ..old_id_cols]
  )
  
  shifted_names <- paste0(
    "level_",
    seq_along(old_id_cols) + length(prefix_codes),
    "_id"
  )
  
  setnames(
    out,
    old_id_cols,
    shifted_names
  )
  
  if (length(prefix_codes) > 0L) {
    
    for (i in seq_along(prefix_codes)) {
      out[
        ,
        (paste0("level_", i, "_id")) :=
          prefix_codes[i]
      ]
    }
  }
  
  final_names <- paste0(
    "level_",
    seq_len(
      length(prefix_codes) +
        length(old_id_cols)
    ),
    "_id"
  )
  
  setcolorder(
    out,
    final_names
  )
  
  out[]
}

#Add synthetic All and Expired roots entries to a codelist.
add_synthetic_all_codes <- function(codes) {
  
  codes <- copy(as.data.table(codes))
  codes[, id := as.character(id)]
  
  label_col <- get_codelist_label_column(
    codes
  )
  
  if (is.na(label_col)) {
    codes[, label_en := id]
    label_col <- "label_en"
  }
  
  codes[, synthetic_label_only := FALSE]
  
  synthetic_codes <- data.table(
    id = c(
      SYNTHETIC_ALL_ROOT_ID,
      SYNTHETIC_EXPIRED_ROOTS_ID
    ),
    synthetic_label_only = c(
      TRUE,
      TRUE
    )
  )
  
  synthetic_codes[
    ,
    (label_col) := c(
      "All",
      "Expired roots"
    )
  ]
  
  out <- rbindlist(
    list(
      codes,
      synthetic_codes
    ),
    use.names = TRUE,
    fill = TRUE
  )
  
  unique(
    out,
    by = "id"
  )
}

#Add an All branch to the hierarchy and place expired roots under All > Expired roots.
add_all_and_expired_branches_to_tree <- function(
    tree_dt,
    codes
) {
  
  tree_dt <- copy(as.data.table(tree_dt))
  codes <- copy(as.data.table(codes))
  
  id_cols <- get_tree_id_cols(tree_dt)
  
  if (length(id_cols) == 0L) {
    return(tree_dt)
  }
  
  root_info <- get_current_and_expired_tree_roots(
    tree_dt = tree_dt,
    codes = codes
  )
  
  original_root_column <- id_cols[1L]
  
  # Keep the current roots exactly where they currently are.
  current_top_level_paths <- tree_dt[
    as.character(
      get(original_root_column)
    ) %in% root_info$current_roots,
    ..id_cols
  ]
  
  # Expired roots will no longer appear independently at the top level.
  expired_original_paths <- tree_dt[
    as.character(
      get(original_root_column)
    ) %in% root_info$expired_roots,
    ..id_cols
  ]
  
  # Duplicate the current hierarchy under All.
  current_paths_under_all <- prepend_tree_levels(
    tree_dt = current_top_level_paths,
    prefix_codes = SYNTHETIC_ALL_ROOT_ID
  )
  
  # Put expired roots under All > Expired roots.
  expired_paths_under_all <- prepend_tree_levels(
    tree_dt = expired_original_paths,
    prefix_codes = c(
      SYNTHETIC_ALL_ROOT_ID,
      SYNTHETIC_EXPIRED_ROOTS_ID
    )
  )
  
  expired_branch_header <- data.table(
    level_1_id = SYNTHETIC_ALL_ROOT_ID,
    level_2_id = SYNTHETIC_EXPIRED_ROOTS_ID
  )
  
  out <- rbindlist(
    list(
      current_top_level_paths,
      current_paths_under_all,
      expired_paths_under_all,
      expired_branch_header
    ),
    use.names = TRUE,
    fill = TRUE
  )
  
  unique(out)
}

#Select the hierarchy roots to display according to the codelist, tree purpose, configured roots, and relevant codes.
get_display_roots_for_tree <- function(
    codelist_id,
    configured_roots,
    relevant_codes,
    purpose = c(
      "filter",
      "classification",
      "custom"
    )
) {
  
  purpose <- match.arg(purpose)
  
  configured_roots <- unique(
    trimws(
      as.character(configured_roots)
    )
  )
  
  configured_roots <- configured_roots[
    !is.na(configured_roots) &
      nzchar(configured_roots)
  ]
  
  relevant_codes <- unique(
    trimws(
      as.character(relevant_codes)
    )
  )
  
  relevant_codes <- relevant_codes[
    !is.na(relevant_codes) &
      nzchar(relevant_codes)
  ]
  
  if (uses_augmented_all_root(codelist_id)) {
    
    if (identical(purpose, "classification")) {
      
      # Existing roots stay, and Expired roots is added.
      roots <- unique(
        c(
          configured_roots,
          SYNTHETIC_EXPIRED_ROOTS_ID
        )
      )
      
    } else {
      
      # Existing roots stay, and All is added.
      roots <- unique(
        c(
          configured_roots,
          SYNTHETIC_ALL_ROOT_ID
        )
      )
    }
    
  } else {
    
    roots <- configured_roots
  }
  
  if (length(relevant_codes) > 0L) {
    roots <- intersect(
      roots,
      relevant_codes
    )
  }
  
  unique(roots)
}


##########################################
# Helper: dimensions that should be shown as a flat checkbox tree

# Most codelist dimensions, such as geographic areas, ASFIS and fishing area, are hierarchical and should be displayed as trees.

# Some dimensions are not used as hierarchies. for some dimensons, the app should display only the values actually present
# in the selected dataset, as a flat list of tickable values
##########################################
is_flat_filter_dimension <- function(dim_id){
  dim_id %in% c(
    "measured_element",
    "observation_flag",
    "currency_flag"
  )
}


# Splitting the aggregated outputs if two measured elements are chosen
split_aggregated_outputs_by_measured_element <- function(aggr, cfg) {
  dt <- copy(aggr)
  
  measured_col <- cfg$measured_element_col
  
  if (is.null(measured_col) || !measured_col %in% names(dt)) {
    return(list("Aggregated output" = dt))
  }
  
  measured_elements <- sort(unique(as.character(dt[[measured_col]])))
  measured_elements <- measured_elements[
    !is.na(measured_elements) &
      nzchar(measured_elements)
  ]
  
  if (length(measured_elements) == 0) {
    return(list("Aggregated output" = dt))
  }
  
  out <- lapply(
    measured_elements,
    function(me_i) {
      dt[as.character(get(measured_col)) == me_i]
    }
  )
  
  names(out) <- measured_elements
  
  out
}

##########################################
# Server
##########################################
server <- function(input, output, session) {
  user <- reactiveVal(NULL)
  dataset_data <- reactiveVal(NULL)
  loaded_dataset_id <- reactiveVal(NULL)
  
  # Stores the result returned by getDatasetInfo()
  loaded_dataset_info <- reactiveVal(NULL)
  
  aggregated_data <- reactiveVal(NULL)     # combined aggregated output
  aggregated_outputs <- reactiveVal(list()) # separate outputs by measured element
  comparison_data <- reactiveVal(NULL)
  comparison_metadata <- reactiveVal(NULL)
  comparison_results <- reactiveVal(NULL)
  comparison_compatibility_warning <- reactiveVal(character(0))
  
  # Stores non-fatal differences between the raw dimension codes
  # found in the current and comparison datasets.
  comparison_code_warnings <- reactiveVal(character(0))
  
  last_force_run_comparison <- reactiveVal(NULL)
  filter_debug <- reactiveVal(NULL)
  tree_reset_counter <- reactiveVal(0)
  
  
  
  comparison_is_available <- reactive({
    dataset_id <- loaded_dataset_id()
    
    if (
      is.null(dataset_data()) ||
      is.null(dataset_id) ||
      !nzchar(dataset_id)
    ) {
      return(FALSE)
    }
    
    cfg <- get_dataset_config(dataset_id)
    
    identical(
      cfg$dataset_group,
      "current"
    )
  })
  
  output$comparison_available <- reactive({
    comparison_is_available()
  })
  
  outputOptions(
    output,
    "comparison_available",
    suspendWhenHidden = FALSE
  )
  # Data used to build aggregation composition plots.
  # This stores the data after year filtering but before aggregation.
  aggregation_input_data <- reactiveVal(NULL)
  
  # This stores the last aggregation specification used.
  last_aggregation_specs <- reactiveVal(NULL)
  
  # Stores the exact filters and additional aggregation settings
  # used to create the most recent aggregation output.
  last_comparison_state <- reactiveVal(NULL)
  
  codelist_cache <- reactiveValues()
  codelist_tree_cache <- reactiveValues()
  
  
  get_dataset_dimension_roots <- function(dataset_info, dimension_id) {
    
    if (
      is.null(dataset_info) ||
      is.null(dataset_info$dimensions)
    ) {
      return(character(0))
    }
    
    dimensions <- as.data.table(
      dataset_info$dimensions
    )
    
    if (!all(c("id", "roots") %in% names(dimensions))) {
      return(character(0))
    }
    
    dimension_row <- dimensions[
      as.character(id) == as.character(dimension_id)
    ]
    
    if (nrow(dimension_row) == 0) {
      return(character(0))
    }
    
    roots <- unlist(
      dimension_row$roots[[1]],
      recursive = TRUE,
      use.names = FALSE
    )
    
    roots <- trimws(
      as.character(roots)
    )
    
    roots <- roots[
      !is.na(roots) &
        nzchar(roots)
    ]
    
    unique(roots)
  }
  
  #Retrieve and clean the configured roots associated with an aggregation dimension.
  get_configured_roots_for_dimension <- function(meta) {
    
    if (
      is.null(meta$dataset_column) ||
      is.null(loaded_dataset_info())
    ) {
      return(character(0))
    }
    
    roots <- get_dataset_dimension_roots(
      dataset_info = loaded_dataset_info(),
      dimension_id = meta$dataset_column
    )
    
    roots <- unique(
      trimws(
        as.character(roots)
      )
    )
    
    roots <- roots[
      !is.na(roots) &
        nzchar(roots)
    ]
    
    roots
  }
  #Retrieve and cache codelist codes, removing expired codes when required.
  get_regular_codelist_codes <- function(codelist_id) {
    cache_id <- paste0("regular_codes__", codelist_id)
    
    if (is.null(codelist_cache[[cache_id]])) {
      showNotification(
        paste("Reading codelist:", codelist_id),
        type = "default"
      )
      
      codelist_info <- getCodelistInfo(codelist_id)
      
      codes <- as.data.table(codelist_info$codes)
      codes[, id := as.character(id)]
      
      if (
        !codelist_id %in%
        KEEP_EXPIRED_CODELISTS
      ) {
        
        codes <- drop_expired_codelist_codes(
          codes
        )
      }
      
      codelist_cache[[cache_id]] <- codes
    }
    
    codelist_cache[[cache_id]]
  }
  
  #Retrieve and cache a codelist hierarchy, removing expired hierarchy paths when required.
  get_regular_codelist_tree_cached <- function(codelist_id) {
    cache_id <- paste0("regular_tree__", codelist_id)
    
    if (is.null(codelist_tree_cache[[cache_id]])) {
      showNotification(
        paste("Reading codelist tree:", codelist_id),
        type = "default"
      )
      
      tree_dt <- as.data.table(getCodelistTree(codelist_id))
      
      if (
        !codelist_id %in%
        KEEP_EXPIRED_CODELISTS
      ) {
        
        active_codes <-
          get_regular_codelist_codes(
            codelist_id
          )
        
        tree_dt <-
          drop_expired_codes_from_codelist_tree(
            tree_dt = tree_dt,
            active_codes = active_codes
          )
      }
      
      codelist_tree_cache[[cache_id]] <- tree_dt
    }
    
    codelist_tree_cache[[cache_id]]
  }
  
  #Retrieve the Economic Commission branch from the M49 hierarchy together with the codes needed to display it.
  get_m49_economic_commission_branch <- function() {
    
    tree_m49 <- as.data.table(
      getCodelistTree("geographicAreaM49")
    )
    
    codes_m49 <- as.data.table(
      getCodelistInfo("geographicAreaM49")$codes
    )
    
    codes_m49[, id := as.character(id)]
    
    id_cols <- get_tree_id_cols(tree_m49)
    
    if (length(id_cols) == 0) {
      return(
        list(
          branch_tree = data.table(),
          branch_codes = data.table()
        )
      )
    }
    
    for (col_i in id_cols) {
      tree_m49[, (col_i) := as.character(get(col_i))]
    }
    
    branch_tree <- tree_m49[
      as.character(level_1_id) == "ECC"
    ]
    
    if (nrow(branch_tree) == 0) {
      return(
        list(
          branch_tree = data.table(),
          branch_codes = data.table()
        )
      )
    }
    
    branch_tree <- unique(branch_tree)
    
    branch_ids <- unique(
      as.character(
        unlist(
          branch_tree[, ..id_cols],
          use.names = FALSE
        )
      )
    )
    
    branch_ids <- branch_ids[
      !is.na(branch_ids) &
        nzchar(branch_ids)
    ]
    
    branch_codes <- codes_m49[
      id %in% branch_ids
    ]
    
    missing_ids <- setdiff(branch_ids, branch_codes$id)
    
    if (length(missing_ids) > 0) {
      
      tree_labels <- rbindlist(
        lapply(
          id_cols,
          function(id_col_i) {
            label_col_i <- sub("_id$", "_label", id_col_i)
            
            data.table(
              id = as.character(branch_tree[[id_col_i]]),
              label_en = if (label_col_i %in% names(branch_tree)) {
                as.character(branch_tree[[label_col_i]])
              } else {
                NA_character_
              }
            )
          }
        ),
        fill = TRUE
      )
      
      tree_labels <- tree_labels[
        id %in% missing_ids
      ]
      
      tree_labels <- tree_labels[
        !is.na(id) & nzchar(id)
      ]
      
      tree_labels <- unique(tree_labels, by = "id")
      
      tree_labels[
        is.na(label_en) | !nzchar(label_en),
        label_en := id
      ]
      
      branch_codes <- rbindlist(
        list(
          branch_codes,
          tree_labels
        ),
        fill = TRUE
      )
    }
    
    branch_codes <- unique(branch_codes, by = "id")
    
    if (!"display_id" %in% names(branch_codes)) {
      branch_codes[, display_id := NA_character_]
    }
    
    if ("order" %in% names(branch_codes)) {
      branch_codes[
        id == "ECC" & !is.na(order),
        display_id := as.character(order)
      ]
      
      branch_codes[
        id == "ECC",
        order := NA_real_
      ]
    }
    
    branch_codes[
      id == "ECC" & (is.na(display_id) | !nzchar(display_id)),
      display_id := id
    ]
    
    list(
      branch_tree = branch_tree,
      branch_codes = branch_codes
    )
  }
  
  #Retrieve the codelist codes used by the app, adding synthetic hierarchy codes 
  #or the M49 Economic Commission branch when required.
  get_codelist_codes <- function(codelist_id) {
    if (
      uses_augmented_all_root(
        codelist_id
      )
    ) {
      
      cache_id <- paste0(
        "augmented_codes__current_roots_plus_all__",
        codelist_id
      )
      
      if (
        is.null(
          codelist_cache[[cache_id]]
        )
      ) {
        
        original_codes <- copy(
          get_regular_codelist_codes(
            codelist_id
          )
        )
        
        out <- add_synthetic_all_codes(
          original_codes
        )
        
        codelist_cache[[cache_id]] <- out
      }
      
      return(
        codelist_cache[[cache_id]]
      )
    }
    
    if (identical(codelist_id, "geographicAreaM49_fi")) {
      
      cache_id <- "augmented_codes__geographicAreaM49_fi_plus_economic_commissions"
      
      if (is.null(codelist_cache[[cache_id]])) {
        
        codes_fi <- copy(
          get_regular_codelist_codes("geographicAreaM49_fi")
        )
        
        codes_fi[, id := as.character(id)]
        
        branch <- get_m49_economic_commission_branch()
        
        out <- rbindlist(
          list(
            codes_fi,
            branch$branch_codes
          ),
          fill = TRUE
        )
        
        out <- unique(out, by = "id")
        
        if ("virtual" %in% names(out)) {
          out[id %in% branch$branch_codes$id, virtual := NA_character_]
        }
        
        codelist_cache[[cache_id]] <- out
      }
      
      return(codelist_cache[[cache_id]])
    }
    
    get_regular_codelist_codes(codelist_id)
  }
  
  #Retrieve the hierarchy used by the app, adding synthetic branches or the M49 Economic Commission branch when required.
  get_codelist_tree_cached <- function(codelist_id) {
    
    if (
      uses_augmented_all_root(
        codelist_id
      )
    ) {
      
      cache_id <- paste0(
        "augmented_tree__current_roots_plus_all__",
        codelist_id
      )
      
      if (
        is.null(
          codelist_tree_cache[[cache_id]]
        )
      ) {
        
        original_tree <- copy(
          get_regular_codelist_tree_cached(
            codelist_id
          )
        )
        
        original_codes <- copy(
          get_regular_codelist_codes(
            codelist_id
          )
        )
        
        out <-
          add_all_and_expired_branches_to_tree(
            tree_dt = original_tree,
            codes = original_codes
          )
        
        codelist_tree_cache[[cache_id]] <- out
      }
      
      return(
        codelist_tree_cache[[cache_id]]
      )
    }
    
    if (identical(codelist_id, "geographicAreaM49_fi")) {
      
      cache_id <- "augmented_tree__geographicAreaM49_fi_plus_economic_commissions"
      
      if (is.null(codelist_tree_cache[[cache_id]])) {
        
        tree_fi <- copy(
          get_regular_codelist_tree_cached("geographicAreaM49_fi")
        )
        
        branch <- get_m49_economic_commission_branch()
        
        out <- rbindlist(
          list(
            tree_fi,
            branch$branch_tree
          ),
          fill = TRUE
        )
        
        out <- unique(out)
        
        codelist_tree_cache[[cache_id]] <- out
      }
      
      return(codelist_tree_cache[[cache_id]])
    }
    
    get_regular_codelist_tree_cached(codelist_id)
  }
  
  #Restrict dataset records to codes allowed by the configured codelist hierarchies and dataset roots.
  restrict_to_active_configured_codes <- function(
    data,
    dataset_info
  ) {
    
    dt <- copy(as.data.table(data))
    
    if (is.null(dataset_info)) {
      stop(
        "The current dataset metadata are not available."
      )
    }
    
    for (
      dim_id in names(
        AGGREGATION_DIMENSIONS
      )
    ) {
      
      meta <- AGGREGATION_DIMENSIONS[[dim_id]]
      
      column_name <- meta$dataset_column
      codelist_id <- meta$codelist
      
      if (
        is.null(column_name) ||
        is.null(codelist_id) ||
        !column_name %in% names(dt)
      ) {
        next
      }
      
      codes <- as.data.table(
        get_codelist_codes(
          codelist_id
        )
      )
      
      codes[
        ,
        id := trimws(
          as.character(id)
        )
      ]
      
      real_codelist_ids <- unique(
        codes[
          !is.na(id) &
            nzchar(id) &
            !id %in% c(
              SYNTHETIC_ALL_ROOT_ID,
              SYNTHETIC_EXPIRED_ROOTS_ID
            ),
          id
        ]
      )
      
      configured_roots <-
        get_dataset_dimension_roots(
          dataset_info = dataset_info,
          dimension_id = column_name
        )
      
      configured_roots <- unique(
        trimws(
          as.character(
            configured_roots
          )
        )
      )
      
      configured_roots <- configured_roots[
        !is.na(configured_roots) &
          nzchar(configured_roots)
      ]
      
      tree_dt <- as.data.table(
        get_codelist_tree_cached(
          codelist_id
        )
      )
      
      if (
        uses_augmented_all_root(
          codelist_id
        )
      ) {
        
        # ASFIS and catch area:
        # current and expired real codes are available.
        allowed_ids <- real_codelist_ids
        
      } else if (
        length(configured_roots) > 0L
      ) {
        
        available_roots <- intersect(
          configured_roots,
          real_codelist_ids
        )
        
        if (length(available_roots) == 0L) {
          dt <- dt[0]
          next
        }
        
        allowed_ids <- unique(
          c(
            available_roots,
            
            unlist(
              lapply(
                available_roots,
                function(root_i) {
                  
                  get_descendants_from_codelist_tree(
                    tree_dt = tree_dt,
                    parent_code = root_i
                  )
                }
              ),
              recursive = TRUE,
              use.names = FALSE
            )
          )
        )
        
        allowed_ids <- trimws(
          as.character(
            allowed_ids
          )
        )
        
        allowed_ids <- intersect(
          allowed_ids,
          real_codelist_ids
        )
        
      } else {
        
        allowed_ids <- real_codelist_ids
      }
      
      dt <- dt[
        as.character(
          get(column_name)
        ) %in% allowed_ids
      ]
    }
    
    dt[]
  }
  
  # Prepare the loaded dataset once using the configured
  # codelist hierarchies and dataset roots.
  base_analysis_data <- reactive({
    
    req(
      dataset_data(),
      loaded_dataset_info()
    )
    
    restrict_to_active_configured_codes(
      data = dataset_data(),
      dataset_info = loaded_dataset_info()
    )
  })
  
  #Build the available top-level aggregation choices from the roots configured for the selected dataset dimension.
  get_aggregation_root_choices <- function(meta) {
    
    if (
      is.null(meta$codelist) ||
      !nzchar(meta$codelist) ||
      is.null(meta$dataset_column) ||
      is.null(loaded_dataset_info())
    ) {
      return(character(0))
    }
    
    codes <- as.data.table(
      get_codelist_codes(
        meta$codelist
      )
    )
    
    tree_dt <- as.data.table(
      get_codelist_tree_cached(
        meta$codelist
      )
    )
    
    codes[, id := as.character(id)]
    
    # Only the top classes configured for this dataset.
    root_codes <- get_configured_roots_for_dimension(
      meta
    )
    
    root_codes <- unique(
      trimws(
        as.character(root_codes)
      )
    )
    
    root_codes <- root_codes[
      !is.na(root_codes) &
        nzchar(root_codes) &
        root_codes %in% codes$id
    ]
    
    # A top class must contain at least one class underneath it.
    root_codes <- root_codes[
      vapply(
        root_codes,
        function(root_code_i) {
          length(
            get_direct_children_from_codelist_tree(
              tree_dt = tree_dt,
              parent_code = root_code_i
            )
          ) > 0
        },
        logical(1)
      )
    ]
    
    if (length(root_codes) == 0) {
      return(character(0))
    }
    
    display_labels <- make_tree_display_labels(
      codes
    )
    
    root_labels <- unname(
      display_labels[root_codes]
    )
    
    missing_labels <- (
      is.na(root_labels) |
        !nzchar(root_labels)
    )
    
    root_labels[missing_labels] <-
      root_codes[missing_labels]
    
    stats::setNames(
      root_codes,
      root_labels
    )
  }
  
  #Build the available direct-child choices for a selected aggregation parent.
  get_aggregation_child_choices <- function(
    meta,
    parent_code
  ) {
    
    if (
      is.null(parent_code) ||
      length(parent_code) == 0 ||
      !nzchar(as.character(parent_code)) ||
      is.null(meta$codelist)
    ) {
      return(character(0))
    }
    
    parent_code <- as.character(
      parent_code
    )
    
    codes <- as.data.table(
      get_codelist_codes(
        meta$codelist
      )
    )
    
    tree_dt <- as.data.table(
      get_codelist_tree_cached(
        meta$codelist
      )
    )
    
    codes[, id := as.character(id)]
    
    child_codes <- get_direct_children_from_codelist_tree(
      tree_dt = tree_dt,
      parent_code = parent_code
    )
    
    child_codes <- unique(
      trimws(
        as.character(child_codes)
      )
    )
    
    child_codes <- child_codes[
      !is.na(child_codes) &
        nzchar(child_codes) &
        child_codes %in% codes$id
    ]
    
    if (length(child_codes) == 0) {
      return(character(0))
    }
    
    display_labels <- make_tree_display_labels(
      codes
    )
    
    child_labels <- unname(
      display_labels[child_codes]
    )
    
    missing_labels <- (
      is.na(child_labels) |
        !nzchar(child_labels)
    )
    
    child_labels[missing_labels] <-
      child_codes[missing_labels]
    
    stats::setNames(
      child_codes,
      child_labels
    )
  }
  
  #Retrieve available tagged datasets and create their display labels using the tag name, ID, and release date.
  get_tagged_dataset_choices <- function(dataset_id) {
    if (is.null(dataset_id) || !nzchar(dataset_id)) {
      return(character(0))
    }
    
    tags <- as.data.table(getAllTags(dataset = dataset_id))
    
    if (nrow(tags) == 0) {
      return(character(0))
    }
    
    tags[, id := as.character(id)]
    
    tags[
      ,
      released_date := ifelse(
        !is.na(released) & nzchar(released),
        substr(released, 1, 10),
        "not released"
      )
    ]
    
    tags[
      ,
      label := paste0(
        name,
        " [tag id: ",
        id,
        ", released: ",
        released_date,
        "]"
      )
    ]
    
    stats::setNames(tags$id, tags$label)
  }
  
  
  
  output$comparison_dataset_selector <- renderUI({
    req(input$comparison_source)
    
    if (identical(input$comparison_source, "disseminated")) {
      
      choices <- get_dataset_choices("disseminated")
      
      if (length(choices) == 0) {
        return(helpText("No configured disseminated datasets are available."))
      }
      
      return(
        selectizeInput(
          "comparison_dataset_id",
          "Comparison disseminated dataset",
          choices = choices,
          selected = character(0),
          options = list(
            placeholder = "Choose a disseminated dataset",
            maxOptions = 5000
          )
        )
      )
    }
    
    if (identical(input$comparison_source, "tagged")) {
      
      current_choices <- get_dataset_choices("current")
      
      selected_current <- input$dataset_id
      if (is.null(selected_current) || length(selected_current) == 0) {
        selected_current <- character(0)
      }
      
      return(
        tagList(
          selectizeInput(
            "comparison_base_dataset_id",
            "Dataset for tagged comparison",
            choices = current_choices,
            selected = selected_current,
            options = list(
              placeholder = "Choose the dataset whose tags should be shown",
              maxOptions = 5000
            )
          ),
          
          uiOutput("comparison_tag_selector")
        )
      )
    }
    
    NULL
  })
  
  output$comparison_tag_selector <- renderUI({
    req(input$comparison_base_dataset_id)
    
    choices <- get_tagged_dataset_choices(input$comparison_base_dataset_id)
    
    if (length(choices) == 0) {
      return(
        helpText(
          "No tags were found for the selected dataset."
        )
      )
    }
    
    selectizeInput(
      "comparison_tag_id",
      "Tagged comparison dataset",
      choices = choices,
      selected = character(0),
      options = list(
        placeholder = "Choose a tagged dataset",
        maxOptions = 5000
      )
    )
  })
  
  
  output$comparison_compatibility_warning_ui <- renderUI({
    
    problems <- comparison_compatibility_warning()
    
    if (is.null(problems) || length(problems) == 0) {
      return(NULL)
    }
    
    div(
      class = "alert alert-warning",
      style = "margin-top: 12px;",
      
      strong("Compatibility warning"),
      
      tags$p(
        "The selected comparison dataset may not correspond to the current dataset. ",
        "Please check before running the comparison."
      ),
      
      tags$ul(
        lapply(
          problems,
          tags$li
        )
      ),
      
      actionButton(
        "run_comparison_anyway",
        "Run comparison anyway",
        class = "btn-warning",
        width = "100%",
        onclick = "Shiny.setInputValue('force_run_comparison', Date.now(), {priority: 'event'}); $('#run_comparison').click();"
      )
    )
  })
  
  output$comparison_code_warning_ui <- renderUI({
    
    warnings <- comparison_code_warnings()
    
    if (
      is.null(warnings) ||
      length(warnings) == 0
    ) {
      return(NULL)
    }
    
    div(
      class = "alert alert-warning",
      style = "margin-top: 12px;",
      
      strong("Filtered code coverage warning"),
      
      tags$p(
        paste0(
          "The current and comparison datasets do not contain exactly ",
          "the same filtered dimension codes. The same filters and ",
          "aggregation rules were applied, and the comparison was continued."
        )
      ),
      
      tags$ul(
        lapply(
          warnings,
          function(warning_i) {
            tags$li(warning_i)
          }
        )
      )
    )
  })
  
  
  output$comparison_status <- renderPrint({
    meta <- comparison_metadata()
    dt <- comparison_data()
    
    if (is.null(meta) || is.null(dt)) {
      cat("No comparison dataset has been loaded yet.")
      return(NULL)
    }
    
    cat("Source:", meta$source, "\n")
    cat("ID:", meta$id, "\n")
    cat("Label:", meta$label, "\n")
    cat("Base dataset:", meta$base_dataset, "\n")
    cat("Rows:", nrow(dt), "\n")
    cat("Columns:", ncol(dt), "\n")
    cat("Column names:\n")
    print(names(dt))
  })
  
  
  
  output$comparison_measured_elements_summary <- renderDT({
    req(comparison_data(), input$dataset_id)
    
    cfg <- get_dataset_config(input$dataset_id)
    dt <- standardise_comparison_columns(
      data = comparison_data(),
      cfg = cfg
    )
    
    measured_col <- cfg$measured_element_col
    
    if (is.null(measured_col) || !measured_col %in% names(dt)) {
      return(
        datatable(
          data.table(
            Message = "The loaded comparison dataset has no measured-element column."
          ),
          rownames = FALSE,
          options = list(dom = "t")
        )
      )
    }
    
    selected_measured_elements <- as.character(input$filter_measured_element_values %||% character(0))
    
    out <- dt[
      ,
      .(
        rows = .N,
        non_missing_values = sum(!is.na(suppressWarnings(as.numeric(get(cfg$value_col))))),
        total_value = sum_or_na_rounded(
          get(cfg$value_col),
          digits = VALUE_DECIMAL_DIGITS
        )
      ),
      by = .(
        measured_element = as.character(get(measured_col))
      )
    ][order(measured_element)]
    
    out[
      ,
      selected_in_current_aggregation := measured_element %in% selected_measured_elements
    ]
    
    datatable(
      out,
      rownames = FALSE,
      options = list(
        pageLength = 20,
        scrollX = TRUE
      )
    )
  })
  
  
  observeEvent(input$initialise, {
    tryCatch(
      {
        showNotification("Initializing client...", type = "default")
        
        initialiseClient(
          session = session,
          sws_endpoint = "https://sws.qa.fao.org"
        )
        
        user(getCurrentUser())
        
        updateSelectizeInput(
          session,
          "dataset_id",
          choices = get_dataset_choices(input$dataset_group %||% "current"),
          selected = character(0),
          server = TRUE
        )
        
        showNotification("Client initialized successfully.", type = "message")
      },
      error = function(e) {
        showNotification(
          paste0("Initialization failed: ", e$message),
          type = "error"
        )
      }
    )
  })
  
  
  observeEvent(input$load_comparison_dataset, {
    
    if (!isTRUE(comparison_is_available())) {
      showNotification(
        paste0(
          "Comparison is available only when the main loaded dataset ",
          "is a current Fisheries dataset."
        ),
        type = "error",
        duration = 10
      )
      
      return(NULL)
    }
    
    req(input$comparison_source)
    
    # Remove the previous comparison immediately.
    comparison_data(NULL)
    comparison_metadata(NULL)
    comparison_results(NULL)
    comparison_compatibility_warning(character(0))
    last_force_run_comparison(NULL)
    comparison_code_warnings(character(0))
    
    tryCatch(
      {
        current_cfg <- get_dataset_config(
          input$dataset_id
        )
        
        if (identical(input$comparison_source, "disseminated")) {
          req(input$comparison_dataset_id)
          
          showNotification(
            paste0("Loading disseminated comparison dataset: ", input$comparison_dataset_id),
            type = "default"
          )
          
          dt <- as.data.table(
            readDataset(
              dataset_id = input$comparison_dataset_id
            )
          )
          
          dt <- normalise_and_drop_empty_values(
            data = dt,
            value_col = current_cfg$value_col
          )
          
          comparison_data(dt)
          comparison_metadata(
            list(
              source = "disseminated",
              id = input$comparison_dataset_id,
              label = input$comparison_dataset_id,
              base_dataset = input$comparison_dataset_id
            )
          )
        }
        
        if (identical(input$comparison_source, "tagged")) {
          req(input$comparison_base_dataset_id, input$comparison_tag_id)
          
          showNotification(
            paste0("Loading tagged comparison dataset: ", input$comparison_tag_id),
            type = "default"
          )
          
          dt <- as.data.table(
            getTagData(
              as.character(input$comparison_tag_id)
            )
          )
          
          dt <- normalise_and_drop_empty_values(
            data = dt,
            value_col = current_cfg$value_col
          )
          
          tag_info <- as.data.table(
            getAllTags(dataset = input$comparison_base_dataset_id)
          )
          tag_info[, id := as.character(id)]
          tag_row <- tag_info[id == as.character(input$comparison_tag_id)]
          
          comparison_data(dt)
          comparison_metadata(
            list(
              source = "tagged",
              id = as.character(input$comparison_tag_id),
              label = if (nrow(tag_row) > 0) tag_row$name[1] else as.character(input$comparison_tag_id),
              base_dataset = input$comparison_base_dataset_id
            )
          )
        }
        
        comparison_results(NULL)
        
        compatibility_problems <- check_comparison_compatibility(
          current_dataset_id = input$dataset_id %||% "",
          comparison_meta = comparison_metadata(),
          comparison_dt = comparison_data()
        )
        
        comparison_compatibility_warning(compatibility_problems)
        
        if (length(compatibility_problems) > 0) {
          showNotification(
            "Comparison dataset loaded, but compatibility warning detected. Please review it before running the comparison.",
            type = "warning",
            duration = 12
          )
        } else {
          showNotification(
            "Comparison dataset loaded successfully.",
            type = "message"
          )
        }
      },
      error = function(e) {
        comparison_data(NULL)
        comparison_metadata(NULL)
        comparison_results(NULL)
        comparison_compatibility_warning(character(0))
        comparison_code_warnings(character(0))
        
        showNotification(
          paste0("Comparison dataset loading failed: ", e$message),
          type = "error"
        )
      }
    )
  })
  
  make_comparison_warning_output <- function(message) {
    out <- data.table(Message = message)
    attr(out, "is_warning_output") <- TRUE
    out
  }
  
  
  standardise_comparison_columns <- function(
    data,
    cfg
  ) {
    
    dt <- normalise_and_drop_empty_values(
      data = data,
      value_col = cfg$value_col
    )
    
    # Convert dimension columns to character to avoid
    # filter and join mismatches.
    key_like_cols <- intersect(
      c(
        cfg$year_col,
        cfg$geographical_area_col,
        cfg$species_col,
        cfg$fishing_area_col,
        cfg$measured_element_col,
        cfg$observation_flag_col,
        cfg$method_flag_col,
        cfg$production_source_col,
        cfg$currency_flag_col
      ),
      names(dt)
    )
    
    for (col in key_like_cols) {
      dt[
        ,
        (col) := trimws(
          as.character(
            get(col)
          )
        )
      ]
    }
    
    dt[]
  }
  
  
  restrict_comparison_to_current_dimensions <- function(
    current_data,
    comparison_data,
    cfg
  ) {
    current <- standardise_comparison_columns(
      data = current_data,
      cfg = cfg
    )
    
    comparison <- standardise_comparison_columns(
      data = comparison_data,
      cfg = cfg
    )
    
    # Flags are intentionally excluded.
    dimension_keys <- unique(
      na.omit(
        c(
          cfg$year_col,
          cfg$geographical_area_col,
          cfg$species_col,
          cfg$fishing_area_col,
          cfg$measured_element_col,
          cfg$production_source_col,
          cfg$currency_flag_col
        )
      )
    )
    
    dimension_keys <- dimension_keys[
      dimension_keys %in% names(current) &
        dimension_keys %in% names(comparison)
    ]
    
    if (length(dimension_keys) == 0) {
      stop(
        "No common analytical dimensions are available for comparison."
      )
    }
    
    if (
      !cfg$value_col %in% names(current) ||
      !cfg$value_col %in% names(comparison)
    ) {
      stop(
        paste0(
          "Column '",
          cfg$value_col,
          "' must exist in both datasets."
        )
      )
    }
    
    # Preserve original comparison-row order.
    current[, .__current_row_id__ := .I]
    comparison[, .__comparison_row_id__ := .I]
    
    # ------------------------------------------------------------
    # First match exact records:
    # same analytical dimensions and same Value.
    # ------------------------------------------------------------
    exact_keys <- c(
      dimension_keys,
      cfg$value_col
    )
    
    current_exact_order <- unique(
      c(
        exact_keys,
        intersect(
          c(
            cfg$observation_flag_col,
            cfg$method_flag_col
          ),
          names(current)
        ),
        ".__current_row_id__"
      )
    )
    
    comparison_exact_order <- unique(
      c(
        exact_keys,
        intersect(
          c(
            cfg$observation_flag_col,
            cfg$method_flag_col
          ),
          names(comparison)
        ),
        ".__comparison_row_id__"
      )
    )
    
    setorderv(
      current,
      cols = current_exact_order,
      na.last = TRUE
    )
    
    setorderv(
      comparison,
      cols = comparison_exact_order,
      na.last = TRUE
    )
    
    current[
      ,
      .__exact_occurrence__ := seq_len(.N),
      by = exact_keys
    ]
    
    comparison[
      ,
      .__exact_occurrence__ := seq_len(.N),
      by = exact_keys
    ]
    
    exact_pairs <- merge(
      current[
        ,
        c(
          exact_keys,
          ".__exact_occurrence__",
          ".__current_row_id__"
        ),
        with = FALSE
      ],
      
      comparison[
        ,
        c(
          exact_keys,
          ".__exact_occurrence__",
          ".__comparison_row_id__"
        ),
        with = FALSE
      ],
      
      by = c(
        exact_keys,
        ".__exact_occurrence__"
      ),
      
      all = FALSE,
      sort = FALSE
    )
    
    matched_current_ids <-
      exact_pairs[[".__current_row_id__"]]
    
    matched_comparison_ids <-
      exact_pairs[[".__comparison_row_id__"]]
    
    # ------------------------------------------------------------
    # Pair any remaining current and tagged records one-to-one
    # within the same analytical dimensions.
    #
    # This retains genuinely changed tagged values, so they still
    # appear as differences, while surplus tagged occurrences are
    # not added to the comparison aggregation.
    # ------------------------------------------------------------
    current_remaining <- current[
      !.__current_row_id__ %in%
        matched_current_ids
    ]
    
    comparison_remaining <- comparison[
      !.__comparison_row_id__ %in%
        matched_comparison_ids
    ]
    
    current_remaining_order <- unique(
      c(
        dimension_keys,
        cfg$value_col,
        ".__current_row_id__"
      )
    )
    
    comparison_remaining_order <- unique(
      c(
        dimension_keys,
        cfg$value_col,
        ".__comparison_row_id__"
      )
    )
    
    setorderv(
      current_remaining,
      cols = current_remaining_order,
      na.last = TRUE
    )
    
    setorderv(
      comparison_remaining,
      cols = comparison_remaining_order,
      na.last = TRUE
    )
    
    current_remaining[
      ,
      .__remaining_occurrence__ := seq_len(.N),
      by = dimension_keys
    ]
    
    comparison_remaining[
      ,
      .__remaining_occurrence__ := seq_len(.N),
      by = dimension_keys
    ]
    
    remaining_pairs <- merge(
      current_remaining[
        ,
        c(
          dimension_keys,
          ".__remaining_occurrence__",
          ".__current_row_id__"
        ),
        with = FALSE
      ],
      
      comparison_remaining[
        ,
        c(
          dimension_keys,
          ".__remaining_occurrence__",
          ".__comparison_row_id__"
        ),
        with = FALSE
      ],
      
      by = c(
        dimension_keys,
        ".__remaining_occurrence__"
      ),
      
      all = FALSE,
      sort = FALSE
    )
    
    comparison_ids_to_keep <- unique(
      c(
        matched_comparison_ids,
        remaining_pairs[[".__comparison_row_id__"]]
      )
    )
    
    comparison_out <- comparison[
      .__comparison_row_id__ %in%
        comparison_ids_to_keep
    ]
    
    setorder(
      comparison_out,
      .__comparison_row_id__
    )
    
    comparison_out[
      ,
      c(
        ".__comparison_row_id__",
        ".__exact_occurrence__"
      ) := NULL
    ]
    
    comparison_out[]
  }
  
  
  comparison_inputs_are_identical <- function(
    current_data,
    comparison_data
  ) {
    current_dt <- copy(
      as.data.table(current_data)
    )
    
    comparison_dt <- copy(
      as.data.table(comparison_data)
    )
    
    # The same data must contain the same columns.
    if (!setequal(
      names(current_dt),
      names(comparison_dt)
    )) {
      return(FALSE)
    }
    
    # Compare columns in the same order.
    ordered_columns <- sort(
      names(current_dt)
    )
    
    setcolorder(
      current_dt,
      ordered_columns
    )
    
    setcolorder(
      comparison_dt,
      ordered_columns
    )
    
    # Do not claim exact identity when a list column cannot
    # be ordered reliably.
    has_list_column <- any(
      vapply(
        current_dt,
        is.list,
        logical(1)
      )
    ) || any(
      vapply(
        comparison_dt,
        is.list,
        logical(1)
      )
    )
    
    if (isTRUE(has_list_column)) {
      return(FALSE)
    }
    
    # Row order returned by SWS must not affect the equality test.
    if (
      length(ordered_columns) > 0 &&
      nrow(current_dt) > 0
    ) {
      setorderv(
        current_dt,
        cols = ordered_columns
      )
      
      setorderv(
        comparison_dt,
        cols = ordered_columns
      )
    }
    
    identical(
      as.list(current_dt),
      as.list(comparison_dt)
    )
  }
  
  
  
  get_comparison_config_from_metadata <- function(meta) {
    
    if (is.null(meta)) {
      return(NULL)
    }
    
    candidate_ids <- unique(c(
      meta$id,
      meta$base_dataset
    ))
    
    candidate_ids <- candidate_ids[
      !is.na(candidate_ids) &
        nzchar(candidate_ids) &
        candidate_ids %in% names(DATASET_CONFIG)
    ]
    
    if (length(candidate_ids) == 0) {
      return(NULL)
    }
    
    get_dataset_config(candidate_ids[1])
  }
  
  
  check_comparison_compatibility <- function(current_dataset_id,
                                             comparison_meta,
                                             comparison_dt) {
    
    if (is.null(current_dataset_id) || !nzchar(current_dataset_id)) {
      return(character(0))
    }
    
    if (is.null(comparison_meta) || is.null(comparison_dt)) {
      return(character(0))
    }
    
    current_cfg <- get_dataset_config(current_dataset_id)
    comparison_cfg <- get_comparison_config_from_metadata(comparison_meta)
    
    problems <- character(0)
    
    expected_disseminated <- c(
      capture = "capture_disseminated",
      aqua = "aqua_disseminated",
      aquaculture_value = "aqua_disseminated"
    )
    
    if (
      identical(comparison_meta$source, "disseminated") &&
      current_dataset_id %in% names(expected_disseminated)
    ) {
      
      expected_id <- expected_disseminated[[current_dataset_id]]
      
      if (!identical(comparison_meta$id, expected_id)) {
        problems <- c(
          problems,
          paste0(
            "The selected comparison dataset does not look like the expected disseminated counterpart. ",
            "Current dataset: ", current_dataset_id,
            ". Expected comparison dataset: ", expected_id,
            ". Selected comparison dataset: ", comparison_meta$id, "."
          )
        )
      }
    }
    
    if (
      identical(comparison_meta$source, "tagged") &&
      !identical(comparison_meta$base_dataset, current_dataset_id)
    ) {
      
      problems <- c(
        problems,
        paste0(
          "The tagged comparison dataset belongs to a different base dataset. ",
          "Current dataset: ", current_dataset_id,
          ". Tagged base dataset: ", comparison_meta$base_dataset, "."
        )
      )
    }
    
    if (!is.null(comparison_cfg)) {
      
      if (!identical(current_cfg$dataset_type, comparison_cfg$dataset_type)) {
        problems <- c(
          problems,
          paste0(
            "The dataset types are different. Current dataset type: ",
            current_cfg$dataset_type,
            ". Comparison dataset type: ",
            comparison_cfg$dataset_type,
            "."
          )
        )
      }
      
      current_dimensions <- unique(na.omit(c(
        current_cfg$geographical_area_col,
        current_cfg$species_col,
        current_cfg$fishing_area_col,
        current_cfg$production_source_col
      )))
      
      comparison_dimensions <- unique(na.omit(c(
        comparison_cfg$geographical_area_col,
        comparison_cfg$species_col,
        comparison_cfg$fishing_area_col,
        comparison_cfg$production_source_col
      )))
      
      extra_comparison_dimensions <- setdiff(
        comparison_dimensions,
        current_dimensions
      )
      
      missing_comparison_dimensions <- setdiff(
        current_dimensions,
        comparison_dimensions
      )
      
      if (length(extra_comparison_dimensions) > 0) {
        problems <- c(
          problems,
          paste0(
            "The comparison dataset has additional analytical dimension(s) not present in the current dataset: ",
            paste(extra_comparison_dimensions, collapse = ", "),
            ". These dimensions would be ignored by the current comparison logic."
          )
        )
      }
      
      if (length(missing_comparison_dimensions) > 0) {
        problems <- c(
          problems,
          paste0(
            "The comparison dataset is missing analytical dimension(s) present in the current dataset: ",
            paste(missing_comparison_dimensions, collapse = ", "),
            "."
          )
        )
      }
      
    } else {
      
      problems <- c(
        problems,
        paste0(
          "The comparison dataset is not configured in DATASET_CONFIG, so compatibility cannot be checked safely. Selected comparison dataset: ",
          comparison_meta$id,
          "."
        )
      )
    }
    
    comparison_column_names <- names(as.data.table(comparison_dt))
    
    current_required_columns <- unique(na.omit(c(
      current_cfg$year_col,
      current_cfg$value_col,
      current_cfg$geographical_area_col,
      current_cfg$species_col,
      current_cfg$fishing_area_col,
      current_cfg$measured_element_col,
      current_cfg$observation_flag_col,
      current_cfg$method_flag_col
    )))
    
    missing_actual_columns <- setdiff(
      current_required_columns,
      comparison_column_names
    )
    
    if (length(missing_actual_columns) > 0) {
      problems <- c(
        problems,
        paste0(
          "The comparison dataset does not contain some columns required by the current dataset configuration: ",
          paste(missing_actual_columns, collapse = ", "),
          "."
        )
      )
    }
    
    unique(problems)
  }
  
  
  
  capture_current_comparison_state <- function(
    group_by_observation_flag,
    aggregate_selected_years,
    year_total_label
  ) {
    
    req(
      dataset_data(),
      input$dataset_id
    )
    
    current_data <- dataset_data()
    
    active_dims <- FILTER_DIMENSIONS[
      vapply(
        FILTER_DIMENSIONS,
        function(meta) {
          !is.null(meta$dataset_column) &&
            meta$dataset_column %in% names(current_data)
        },
        logical(1)
      )
    ]
    
    selected_values_by_dimension <- list()
    
    for (dim_id in names(active_dims)) {
      
      meta <- active_dims[[dim_id]]
      selected_values <- character(0)
      
      if (identical(dim_id, "measured_element")) {
        
        selected_values <- as.character(
          input$filter_measured_element_values %||%
            character(0)
        )
        
      } else {
        
        tree_input <- input[[
          paste0(
            "filter_tree_",
            dim_id
          )
        ]]
        
        if (!is.null(tree_input)) {
          
          codes <- NULL
          
          if (!is.null(meta$codelist)) {
            codes <- tryCatch(
              get_codelist_codes(
                meta$codelist
              ),
              error = function(e) NULL
            )
          }
          
          tree_dt <- NULL
          
          if (
            !is_flat_filter_dimension(dim_id) &&
            !is.null(meta$codelist)
          ) {
            
            tree_dt <- tryCatch(
              get_codelist_tree_cached(
                meta$codelist
              ),
              error = function(e) NULL
            )
          }
          
          selected_values <- get_selected_codes_from_tree(
            tree_input = tree_input,
            codes = codes,
            tree_dt = tree_dt,
            expand_descendants =
              !is_flat_filter_dimension(dim_id),
            selection_rule = if (
              is_flat_filter_dimension(dim_id)
            ) {
              "none"
            } else {
              "most_specific"
            },
            codelist_id = meta$codelist
          )
        }
      }
      
      selected_values <- unique(
        trimws(
          as.character(
            selected_values %||%
              character(0)
          )
        )
      )
      
      selected_values <- selected_values[
        !is.na(selected_values) &
          nzchar(selected_values)
      ]
      
      selected_values_by_dimension[[dim_id]] <-
        selected_values
    }
    
    list(
      dataset_id = as.character(
        input$dataset_id
      ),
      
      year_range = as.numeric(
        input$year_range
      ),
      
      selected_values =
        selected_values_by_dimension,
      
      group_by_observation_flag = isTRUE(
        group_by_observation_flag
      ),
      
      aggregate_selected_years = isTRUE(
        aggregate_selected_years
      ),
      
      year_total_label = as.character(
        year_total_label
      )
    )
  }
  
  
  apply_saved_filters_to_comparison_data <- function(
    data,
    cfg,
    comparison_state,
    exclude_dims = character(0)
  ) {
    
    if (is.null(comparison_state)) {
      stop(
        paste0(
          "The filter state used for the current aggregation ",
          "was not saved. Please rerun the current aggregation."
        )
      )
    }
    
    dt <- standardise_comparison_columns(
      data = data,
      cfg = cfg
    )
    
    dt <- restrict_to_active_configured_codes(
      data = dt,
      dataset_info = loaded_dataset_info()
    )
    
    saved_year_range <- as.numeric(
      comparison_state$year_range %||%
        numeric(0)
    )
    
    if (length(saved_year_range) == 2) {
      
      dt <- filter_data_by_year(
        data = dt,
        year_range = saved_year_range,
        year_col = cfg$year_col
      )
    }
    
    saved_values <-
      comparison_state$selected_values %||%
      list()
    
    for (dim_id in names(saved_values)) {
      
      if (dim_id %in% exclude_dims) {
        next
      }
      
      meta <- FILTER_DIMENSIONS[[dim_id]]
      
      if (is.null(meta)) {
        next
      }
      
      selected_values <-
        saved_values[[dim_id]] %||%
        character(0)
      
      selected_values <- unique(
        trimws(
          as.character(
            selected_values
          )
        )
      )
      
      selected_values <- selected_values[
        !is.na(selected_values) &
          nzchar(selected_values)
      ]
      
      if (
        is.null(meta$dataset_column) ||
        !meta$dataset_column %in% names(dt)
      ) {
        
        if (length(selected_values) > 0) {
          stop(
            paste0(
              "The comparison dataset does not contain column '",
              meta$dataset_column,
              "', which is required to reproduce the saved ",
              meta$label,
              " filter."
            )
          )
        }
        
        next
      }
      
      dt <- filter_data_by_selected_values(
        data = dt,
        selected_values = selected_values,
        column_name = meta$dataset_column
      )
    }
    
    dt[]
  }
  
  
  summarise_comparison_side <- function(
    data,
    keys,
    value_col,
    output_value_col
  ) {
    dt <- copy(data)
    
    if (!value_col %in% names(dt)) {
      stop(
        paste0(
          "Column ",
          value_col,
          " was not found."
        )
      )
    }
    
    dt[
      ,
      value_tmp :=
        suppressWarnings(
          as.numeric(
            get(value_col)
          )
        )
    ]
    
    if (length(keys) == 0) {
      
      out <- dt[
        ,
        .(
          value_tmp = sum_or_na_rounded(
            value_tmp,
            digits = VALUE_DECIMAL_DIGITS
          )
        )
      ]
      
    } else {
      
      out <- dt[
        ,
        .(
          value_tmp = sum_or_na_rounded(
            value_tmp,
            digits = VALUE_DECIMAL_DIGITS
          )
        ),
        by = keys
      ]
    }
    
    setnames(
      out,
      "value_tmp",
      output_value_col
    )
    
    out[]
  }
  
  
  build_comparison_result_table <- function(
    current_dt,
    comparison_dt,
    cfg,
    measured_element_id,
    compare_by_observation_flag = FALSE
  ) {
    
    current <- copy(current_dt)
    comparison <- copy(comparison_dt)
    
    if (isTRUE(
      attr(
        current,
        "is_warning_output"
      )
    )) {
      
      return(
        make_comparison_warning_output(
          paste0(
            "The current aggregation output for measured element ",
            measured_element_id,
            " contains no data. No comparison table can be calculated."
          )
        )
      )
    }
    
    if (isTRUE(
      attr(
        comparison,
        "is_warning_output"
      )
    )) {
      
      return(comparison)
    }
    
    if (
      nrow(current) == 0 &&
      nrow(comparison) == 0
    ) {
      
      return(
        make_comparison_warning_output(
          paste0(
            "No current or comparison rows are available for measured element ",
            measured_element_id,
            "."
          )
        )
      )
    }
    
    current <- standardise_comparison_columns(
      data = current,
      cfg = cfg
    )
    
    comparison <- standardise_comparison_columns(
      data = comparison,
      cfg = cfg
    )
    
    common_cols <- intersect(
      names(current),
      names(comparison)
    )
    
    # Only genuine analytical dimensions are used as join keys.
    #
    # Currency is included because values expressed in different
    # currencies must never be summed together.
    key_candidates <- unique(
      na.omit(
        c(
          cfg$year_col,
          cfg$geographical_area_col,
          cfg$species_col,
          cfg$fishing_area_col,
          cfg$measured_element_col,
          cfg$production_source_col,
          cfg$currency_flag_col,
          
          if (isTRUE(compare_by_observation_flag)) {
            cfg$observation_flag_col
          } else {
            NULL
          }
        )
      )
    )
    
    keys <- intersect(
      key_candidates,
      common_cols
    )
    
    current_side <- summarise_comparison_side(
      data = current,
      keys = keys,
      value_col = cfg$value_col,
      output_value_col = "current_value"
    )
    
    comparison_side <- summarise_comparison_side(
      data = comparison,
      keys = keys,
      value_col = cfg$value_col,
      output_value_col = "comparison_value"
    )
    
    # These columns distinguish an absent row from an existing row
    # whose Value happens to be NA.
    current_side[
      ,
      current_row_present := TRUE
    ]
    
    comparison_side[
      ,
      comparison_row_present := TRUE
    ]
    
    out <- merge(
      current_side,
      comparison_side,
      by = keys,
      all = TRUE
    )
    
    out[
      is.na(current_row_present),
      current_row_present := FALSE
    ]
    
    out[
      is.na(comparison_row_present),
      comparison_row_present := FALSE
    ]
    
    out[
      ,
      absolute_difference := NA_real_
    ]
    
    out[
      current_row_present &
        comparison_row_present &
        !is.na(current_value) &
        !is.na(comparison_value),
      
      absolute_difference := round(
        current_value - comparison_value,
        digits = VALUE_DECIMAL_DIGITS
      )
    ]
    
    out[
      ,
      values_equal := FALSE
    ]
    
    # Existing rows with NA on both sides are equal.
    out[
      current_row_present &
        comparison_row_present &
        is.na(current_value) &
        is.na(comparison_value),
      
      values_equal := TRUE
    ]
    
    
    # Both values have already been rounded to the source precision.
    out[
      current_row_present &
        comparison_row_present &
        !is.na(current_value) &
        !is.na(comparison_value) &
        absolute_difference == 0,
      
      values_equal := TRUE
    ]
    
    # Replace numerical noise with an exact zero.
    out[
      values_equal == TRUE &
        !is.na(absolute_difference),
      
      absolute_difference := 0
    ]
    
    out[
      ,
      percentage_difference := NA_real_
    ]
    
    out[
      current_row_present &
        comparison_row_present &
        !is.na(comparison_value) &
        comparison_value != 0 &
        !is.na(absolute_difference),
      
      percentage_difference :=
        100 *
        absolute_difference /
        comparison_value
    ]
    
    # This also sets 0 versus 0 to a zero percentage difference.
    out[
      values_equal == TRUE &
        !is.na(current_value) &
        !is.na(comparison_value),
      
      percentage_difference := 0
    ]
    
    out[
      ,
      comparison_status := "Different value"
    ]
    
    out[
      current_row_present &
        !comparison_row_present,
      
      comparison_status :=
        "Only in current output"
    ]
    
    out[
      !current_row_present &
        comparison_row_present,
      
      comparison_status :=
        "Only in comparison output"
    ]
    
    out[
      current_row_present &
        comparison_row_present &
        values_equal == TRUE,
      
      comparison_status :=
        "Same value"
    ]
    
    # Remove internal helper columns.
    out[
      ,
      c(
        "current_row_present",
        "comparison_row_present",
        "values_equal"
      ) := NULL
    ]
    
    out[
      ,
      measured_element_compared :=
        measured_element_id
    ]
    
    setcolorder(
      out,
      c(
        "measured_element_compared",
        keys,
        "current_value",
        "comparison_value",
        "absolute_difference",
        "percentage_difference",
        "comparison_status"
      )
    )
    
    out[]
  }
  
  normalise_codes_from_data <- function(
    data,
    column_name
  ) {
    
    dt <- as.data.table(data)
    
    if (
      is.null(column_name) ||
      !column_name %in% names(dt)
    ) {
      return(character(0))
    }
    
    out <- unique(
      trimws(
        as.character(
          dt[[column_name]]
        )
      )
    )
    
    out[
      !is.na(out) &
        nzchar(out)
    ]
  }
  
  
  get_codes_with_non_missing_values <- function(
    data,
    code_column,
    value_column,
    target_codes
  ) {
    
    target_codes <- unique(
      trimws(
        as.character(target_codes)
      )
    )
    
    target_codes <- target_codes[
      !is.na(target_codes) &
        nzchar(target_codes)
    ]
    
    if (length(target_codes) == 0) {
      return(character(0))
    }
    
    dt <- copy(
      as.data.table(data)
    )
    
    if (
      !code_column %in% names(dt) ||
      !value_column %in% names(dt)
    ) {
      return(character(0))
    }
    
    dt[
      ,
      code_tmp :=
        trimws(
          as.character(
            get(code_column)
          )
        )
    ]
    
    dt[
      ,
      value_tmp :=
        suppressWarnings(
          as.numeric(
            get(value_column)
          )
        )
    ]
    
    unique(
      dt[
        code_tmp %in% target_codes &
          !is.na(value_tmp),
        code_tmp
      ]
    )
  }
  
  
  format_code_sample <- function(
    codes,
    maximum_codes = 20L
  ) {
    
    codes <- sort(
      unique(
        trimws(
          as.character(codes)
        )
      )
    )
    
    codes <- codes[
      !is.na(codes) &
        nzchar(codes)
    ]
    
    if (length(codes) == 0) {
      return("")
    }
    
    paste0(
      " Codes: ",
      paste(
        head(codes, maximum_codes),
        collapse = ", "
      ),
      
      if (length(codes) > maximum_codes) {
        paste0(
          " ... and ",
          length(codes) - maximum_codes,
          " more"
        )
      } else {
        ""
      },
      
      "."
    )
  }
  
  
  build_comparison_code_warnings <- function(
    current_data,
    comparison_data,
    aggregation_specs,
    value_col,
    measured_element_id,
    comparison_side_label = "comparison dataset"
  ) {
    
    warnings <- character(0)
    
    if (
      is.null(aggregation_specs) ||
      length(aggregation_specs) == 0
    ) {
      return(warnings)
    }
    
    for (dim_id in names(aggregation_specs)) {
      
      spec <- aggregation_specs[[dim_id]]
      column_name <- spec$key_dim_name
      
      if (
        is.null(column_name) ||
        !column_name %in% names(current_data) ||
        !column_name %in% names(comparison_data)
      ) {
        next
      }
      
      current_codes <- normalise_codes_from_data(
        data = current_data,
        column_name = column_name
      )
      
      comparison_codes <- normalise_codes_from_data(
        data = comparison_data,
        column_name = column_name
      )
      
      only_current <- sort(
        setdiff(
          current_codes,
          comparison_codes
        )
      )
      
      only_comparison <- sort(
        setdiff(
          comparison_codes,
          current_codes
        )
      )
      
      if (
        length(only_current) == 0 &&
        length(only_comparison) == 0
      ) {
        next
      }
      
      only_current_with_values <-
        get_codes_with_non_missing_values(
          data = current_data,
          code_column = column_name,
          value_column = value_col,
          target_codes = only_current
        )
      
      only_comparison_with_values <-
        get_codes_with_non_missing_values(
          data = comparison_data,
          code_column = column_name,
          value_column = value_col,
          target_codes = only_comparison
        )
      
      comparison_text <- paste0(
        length(only_comparison),
        " filtered code(s) occur only in the ",
        comparison_side_label,
        "; ",
        length(only_comparison_with_values),
        " of them have at least one non-missing value.",
        format_code_sample(only_comparison)
      )
      
      current_text <- paste0(
        length(only_current),
        " filtered code(s) occur only in the current dataset; ",
        length(only_current_with_values),
        " of them have at least one non-missing value.",
        format_code_sample(only_current)
      )
      
      warnings <- c(
        warnings,
        paste0(
          "Measured element ",
          measured_element_id,
          " — ",
          spec$label,
          ": ",
          comparison_text,
          " ",
          current_text
        )
      )
    }
    
    unique(warnings)
  }
  
  
  rebuild_hierarchy_specs_for_data <- function(
    data,
    aggregation_specs,
    allow_remainder_only = TRUE
  ) {
    
    dt <- copy(
      as.data.table(data)
    )
    
    rebuilt_specs <- aggregation_specs
    
    if (
      is.null(rebuilt_specs) ||
      length(rebuilt_specs) == 0L
    ) {
      return(rebuilt_specs)
    }
    
    for (dim_id in names(rebuilt_specs)) {
      
      spec <- rebuilt_specs[[dim_id]]
      
      aggregation_mode <- as.character(
        spec$aggregation_mode %||% ""
      )
      
      # Total aggregation rebuilds its map dynamically inside
      # aggregate_by_multiple_dimensions().
      if (identical(
        aggregation_mode,
        "total"
      )) {
        next
      }
      
      if (
        !aggregation_mode %in% c(
          "classification",
          "selected_groups",
          "custom"
        )
      ) {
        next
      }
      
      column_name <- spec$key_dim_name
      
      if (
        is.null(column_name) ||
        !column_name %in% names(dt)
      ) {
        stop(
          paste0(
            "The comparison dataset does not contain dimension '",
            column_name,
            "', required for ",
            spec$label,
            "."
          )
        )
      }
      
      filtered_raw_codes <- normalise_codes_from_data(
        data = dt,
        column_name = column_name
      )
      
      if (length(filtered_raw_codes) == 0L) {
        stop(
          paste0(
            "No filtered codes are available for ",
            spec$label,
            " in the comparison dataset."
          )
        )
      }
      
      if (
        is.null(spec$codelist) ||
        !nzchar(spec$codelist)
      ) {
        stop(
          paste0(
            "No codelist is available for ",
            spec$label,
            "."
          )
        )
      }
      
      tree_dt <- get_codelist_tree_cached(
        spec$codelist
      )
      
      dimension_meta <-
        AGGREGATION_DIMENSIONS[[dim_id]]
      
      remainder_label <- if (
        !is.null(dimension_meta)
      ) {
        dimension_meta$remainder_label %||%
          "Other filtered records"
      } else {
        "Other filtered records"
      }
      
      rebuilt_map <- NULL
      
      ## ----------------------------------------------------------
      # Direct-child classification for one or more parents.
      # ----------------------------------------------------------
      if (identical(
        aggregation_mode,
        "classification"
      )) {
        
        selected_parents <- clean_code_vector(
          spec$selected_codes %||%
            spec$root_code %||%
            spec$aggregate_code
        )
        
        if (length(selected_parents) == 0L) {
          stop(
            paste0(
              "The saved hierarchy parents are missing for ",
              spec$label,
              "."
            )
          )
        }
        
        classification_codes <- if (
          identical(
            spec$codelist,
            "fisheriesCatchArea"
          )
        ) {
          get_codelist_codes(
            spec$codelist
          )
        } else {
          NULL
        }
        
        rebuilt_map <-
          rebuilt_map <-
          with_aggregation_context(
            
            build_classification_map_with_remainder(
              tree_dt = tree_dt,
              selected_parents = selected_parents,
              filtered_raw_codes = filtered_raw_codes,
              codes = classification_codes,
              remainder_label = remainder_label,
              allow_remainder_only =
                allow_remainder_only
            ),
            
            dimension_id =
              spec$dimension %||%
              dim_id,
            
            label =
              spec$label,
            
            dataset_column =
              spec$key_dim_name,
            
            codelist =
              spec$codelist,
            
            selected_codes =
              selected_parents,
            
            stage =
              "comparison direct-child aggregation setup"
          )
      }
      
      # ----------------------------------------------------------
      # Selected filter groups separately.
      # ----------------------------------------------------------
      if (identical(
        aggregation_mode,
        "selected_groups"
      )) {
        
        selected_groups <- clean_code_vector(
          spec$selected_codes
        )
        
        if (length(selected_groups) == 0L) {
          stop(
            paste0(
              "The saved selected filter groups are missing for ",
              spec$label,
              "."
            )
          )
        }
        
        rebuilt_map <-
          build_selected_groups_map_with_remainder(
            tree_dt = tree_dt,
            selected_codes = selected_groups,
            filtered_raw_codes = filtered_raw_codes,
            codelist_id = spec$codelist,
            remainder_label = remainder_label
          )
      }
      
      # ----------------------------------------------------------
      # Custom group plus Other.
      # ----------------------------------------------------------
      if (identical(
        aggregation_mode,
        "custom"
      )) {
        
        selected_codes <- clean_code_vector(
          spec$selected_codes
        )
        
        aggregate_code <- clean_code_vector(
          spec$aggregate_code %||%
            spec$root_code
        )
        
        if (length(selected_codes) == 0L) {
          stop(
            paste0(
              "The saved custom aggregation nodes are missing for ",
              spec$label,
              "."
            )
          )
        }
        
        if (length(aggregate_code) == 0L) {
          stop(
            paste0(
              "The saved custom aggregation code is missing for ",
              spec$label,
              "."
            )
          )
        }
        
        rebuilt_map <-
          build_custom_map_with_remainder(
            tree_dt = tree_dt,
            selected_codes = selected_codes,
            aggregate_code = aggregate_code[1L],
            filtered_raw_codes = filtered_raw_codes,
            codelist_id = spec$codelist,
            remainder_label = remainder_label,
            allow_remainder_only =
              allow_remainder_only
          )
      }
      
      if (
        is.null(rebuilt_map) ||
        nrow(rebuilt_map) == 0L
      ) {
        stop(
          paste0(
            "No aggregation map could be rebuilt for ",
            spec$label,
            " in the comparison dataset."
          )
        )
      }
      
      rebuilt_specs[[dim_id]]$aggregation_map <-
        rebuilt_map
      
      rebuilt_specs[[dim_id]]$output_codes <-
        unique(
          as.character(
            rebuilt_map$group_code
          )
        )
      
      rebuilt_specs[[dim_id]]$child_codes <-
        unique(
          as.character(
            rebuilt_map$raw_code
          )
        )
    }
    
    rebuilt_specs
  }
  
  
  observeEvent(input$run_comparison, {
    
    # Remove warnings generated by the previous comparison run.
    comparison_code_warnings(character(0))
    
    if (!isTRUE(comparison_is_available())) {
      comparison_results(NULL)
      
      showNotification(
        paste0(
          "Comparison cannot be run because the main loaded dataset ",
          "is not a current Fisheries dataset."
        ),
        type = "error",
        duration = 10
      )
      
      return(NULL)
    }
    
    
    withProgress(
      message = "Running comparison",
      value = 0,
      {
        tryCatch(
          {
            incProgress(
              amount = 0.03,
              detail = "Checking current dataset and comparison dataset."
            )
            
            if (is.null(dataset_data()) ||
                is.null(input$dataset_id) ||
                !nzchar(input$dataset_id)) {
              
              showNotification(
                "Please load a current dataset before running the comparison.",
                type = "error",
                duration = 10
              )
              
              return(NULL)
            }
            
            if (is.null(comparison_data())) {
              
              showNotification(
                "Please load a comparison dataset before running the comparison.",
                type = "error",
                duration = 10
              )
              
              return(NULL)
            }
            
            incProgress(
              amount = 0.05,
              detail = "Checking current aggregation outputs."
            )
            
            current_outputs <- aggregated_outputs()
            
            if (length(current_outputs) == 0) {
              showNotification(
                "Please run the current aggregation first. The comparison uses the current aggregation outputs.",
                type = "error",
                duration = 10
              )
              return(NULL)
            }
            
            incProgress(
              amount = 0.05,
              detail = "Reading dataset configuration."
            )
            
            cfg <- get_dataset_config(input$dataset_id)
            
            force_comparison <- !is.null(input$force_run_comparison) &&
              !identical(
                input$force_run_comparison,
                isolate(last_force_run_comparison())
              )
            
            last_force_run_comparison(input$force_run_comparison %||% NULL)
            
            compatibility_problems <- check_comparison_compatibility(
              current_dataset_id = input$dataset_id %||% "",
              comparison_meta = comparison_metadata(),
              comparison_dt = comparison_data()
            )
            
            comparison_compatibility_warning(compatibility_problems)
            
            if (length(compatibility_problems) > 0 && !isTRUE(force_comparison)) {
              
              comparison_results(NULL)
              
              showNotification(
                "Comparison stopped because the selected comparison dataset may not correspond to the current dataset. Review the warning and click 'Run comparison anyway' only if this is intentional.",
                type = "warning",
                duration = 15
              )
              
              return(NULL)
            }
            
            aggregation_specs <- last_aggregation_specs()
            comparison_state <- last_comparison_state()
            
            if (
              is.null(aggregation_specs) ||
              is.null(comparison_state)
            ) {
              
              showNotification(
                paste0(
                  "The aggregation settings or saved filter state are missing. ",
                  "Please rerun the current aggregation."
                ),
                type = "error",
                duration = 10
              )
              
              return(NULL)
            }
            
            # Reuse the settings that actually generated current_outputs.
            # Old and current datasets use incompatible flag classifications.
            # Flags must never split or match comparison rows.
            group_by_observation_flag <- FALSE
            
            aggregate_selected_years <- isTRUE(
              comparison_state$aggregate_selected_years
            )
            
            year_total_label <- as.character(
              comparison_state$year_total_label %||%
                "Selected period"
            )
            
            # Compare only the measured-element outputs that were actually
            # generated by the most recent current aggregation.
            selected_comparison_measured_elements <- unique(
              trimws(
                as.character(
                  names(current_outputs)
                )
              )
            )
            
            selected_comparison_measured_elements <-
              selected_comparison_measured_elements[
                !is.na(selected_comparison_measured_elements) &
                  nzchar(selected_comparison_measured_elements)
              ]
            
            if (length(selected_comparison_measured_elements) == 0) {
              showNotification(
                "No measured elements were found for comparison.",
                type = "error",
                duration = 10
              )
              return(NULL)
            }
            
            incProgress(
              amount = 0.06,
              detail = "Reading comparison dataset."
            )
            
            comparison_raw <- comparison_data()
            
            comparison_raw_standardised <- standardise_comparison_columns(
              data = comparison_raw,
              cfg = cfg
            )
            
            
            
            incProgress(
              amount = 0.10,
              detail = "Applying current filters to the comparison dataset."
            )
            
            # Apply the same filters to both datasets, but ignore
            # observation flags because old and current flag systems differ.
            comparison_filtered <- apply_saved_filters_to_comparison_data(
              data = comparison_raw_standardised,
              cfg = cfg,
              comparison_state = comparison_state,
              exclude_dims = c(
                "measured_element",
                "observation_flag"
              )
            )
            
            current_filtered <- apply_saved_filters_to_comparison_data(
              data = dataset_data(),
              cfg = cfg,
              comparison_state = comparison_state,
              exclude_dims = c(
                "measured_element",
                "observation_flag"
              )
            )
            
            if (nrow(current_filtered) == 0) {
              stop(
                "No current rows remain after applying the saved non-flag filters."
              )
            }
            
            current_filtered <- standardise_comparison_columns(
              data = current_filtered,
              cfg = cfg
            )
            
            measured_col <- cfg$measured_element_col
            
            result_list <- list()
            
            code_warning_messages <- character(0)
            
            comparison_side_label <- switch(
              as.character(
                comparison_metadata()$source
              ),
              
              tagged = "tagged dataset",
              disseminated = "disseminated dataset",
              "comparison dataset"
            )
            
            n_elements <- length(selected_comparison_measured_elements)
            
            progress_step <- if (n_elements > 0) {
              0.55 / n_elements
            } else {
              0.55
            }
            
            for (me_i in selected_comparison_measured_elements) {
              
              incProgress(
                amount = progress_step,
                detail = paste0(
                  "Comparing measured element ",
                  me_i,
                  "."
                )
              )
              
              if (!me_i %in% names(current_outputs)) {
                result_list[[me_i]] <- make_comparison_warning_output(
                  paste0(
                    "Measured element ",
                    me_i,
                    " was selected, but no current aggregation output exists for it. ",
                    "Please rerun the current aggregation before running the comparison."
                  )
                )
                next
              }
              
              if (
                !is.null(measured_col) &&
                measured_col %in% names(current_filtered)
              ) {
                current_i_raw <- current_filtered[
                  as.character(get(measured_col)) ==
                    as.character(me_i)
                ]
              } else {
                current_i_raw <- copy(current_filtered)
              }
              
              if (nrow(current_i_raw) == 0) {
                result_list[[me_i]] <- make_comparison_warning_output(
                  paste0(
                    "No current rows are available for measured element ",
                    me_i,
                    " in the exact filtered data saved for the last aggregation."
                  )
                )
                next
              }
              
              current_i <- tryCatch(
                aggregate_by_multiple_dimensions(
                  data = current_i_raw,
                  aggregation_specs = aggregation_specs,
                  value_col = cfg$value_col,
                  observation_flag = cfg$observation_flag_col,
                  method_flag = cfg$method_flag_col,
                  group_by_observation_flag =
                    group_by_observation_flag,
                  aggregate_selected_years =
                    aggregate_selected_years,
                  year_col = cfg$year_col,
                  year_total_label = year_total_label
                ),
                error = function(e) {
                  make_comparison_warning_output(
                    paste0(
                      "Current-side aggregation could not be reproduced for measured element ",
                      me_i,
                      ". Error: ",
                      e$message
                    )
                  )
                }
              )
              
              if (isTRUE(attr(current_i, "is_warning_output"))) {
                result_list[[me_i]] <- current_i
                next
              }
              
              if (
                !is.null(measured_col) &&
                measured_col %in% names(comparison_filtered)
              ) {
                comparison_i_raw <- comparison_filtered[
                  as.character(get(measured_col)) ==
                    as.character(me_i)
                ]
              } else {
                comparison_i_raw <- copy(comparison_filtered)
              }
              
              code_warning_messages <- c(
                code_warning_messages,
                
                build_comparison_code_warnings(
                  current_data = current_i_raw,
                  comparison_data = comparison_i_raw,
                  aggregation_specs = aggregation_specs,
                  value_col = cfg$value_col,
                  measured_element_id = me_i,
                  comparison_side_label =
                    comparison_side_label
                )
              )
              
              # Do not allow tagged-only dimension combinations to
              # contribute to the comparison aggregation.
              # comparison_i_raw <-
              #   restrict_comparison_to_current_dimensions(
              #     current_data = current_i_raw,
              #     comparison_data = comparison_i_raw,
              #     cfg = cfg
              #   )
              
              # if (nrow(comparison_i_raw) == 0) {
              #   
              #   result_list[[me_i]] <- make_comparison_warning_output(
              #     paste0(
              #       "No comparison rows remain for measured element ",
              #       me_i,
              #       " after removing expired/out-of-root codes and ",
              #       "restricting the comparison to dimension combinations ",
              #       "present in the current dataset."
              #     )
              #   )
              #   
              #   next
              # }
              
              
              raw_inputs_identical <- comparison_inputs_are_identical(
                current_data = current_i_raw,
                comparison_data = comparison_i_raw
              )
              
              comparison_i <- if (nrow(comparison_i_raw) == 0) {
                
                # Keep an empty table with the correct columns.
                # build_comparison_result_table() will retain the current-side
                # rows and leave comparison_value empty.
                copy(comparison_i_raw)
                
              } else if (isTRUE(raw_inputs_identical)) {
                
                # Exact same filtered raw records plus exact same
                # aggregation specifications must produce the same output.
                copy(current_i)
                
              } else {
                
                comparison_specs_i <-
                  rebuild_hierarchy_specs_for_data(
                    data = comparison_i_raw,
                    aggregation_specs = aggregation_specs,
                    allow_remainder_only = TRUE
                  )
                
                tryCatch(
                  aggregate_by_multiple_dimensions(
                    data = comparison_i_raw,
                    aggregation_specs = comparison_specs_i,
                    value_col = cfg$value_col,
                    observation_flag = cfg$observation_flag_col,
                    method_flag = cfg$method_flag_col,
                    group_by_observation_flag =
                      group_by_observation_flag,
                    aggregate_selected_years =
                      aggregate_selected_years,
                    year_col = cfg$year_col,
                    year_total_label = year_total_label
                  ),
                  
                  error = function(e) {
                    make_comparison_warning_output(
                      paste0(
                        "Comparison aggregation could not be completed for measured element ",
                        me_i,
                        ". Error: ",
                        e$message
                      )
                    )
                  }
                )
              }
              
              result_list[[me_i]] <- build_comparison_result_table(
                current_dt = current_i,
                comparison_dt = comparison_i,
                cfg = cfg,
                measured_element_id = me_i,
                compare_by_observation_flag = FALSE
              )
            }
            
            incProgress(
              amount = 0.10,
              detail = "Saving comparison results."
            )
            
            comparison_code_warnings(
              unique(
                code_warning_messages
              )
            )
            
            comparison_results(result_list)
            
            incProgress(
              amount = 0.02,
              detail = "Comparison completed."
            )
            
            if (length(unique(code_warning_messages)) > 0) {
              
              showNotification(
                paste0(
                  "Comparison completed with filtered-code coverage warnings. ",
                  "See the warning shown above the comparison tables."
                ),
                type = "warning",
                duration = 15
              )
              
            } else {
              
              showNotification(
                paste0(
                  "Comparison completed for measured element(s): ",
                  paste(names(result_list), collapse = ", ")
                ),
                type = "message",
                duration = 10
              )
            }
          },
          error = function(e) {
            comparison_results(NULL)
            
            showNotification(
              paste0("Comparison failed: ", e$message),
              type = "error",
              duration = 10
            )
          }
        )
      }
    )
  })
  
  
  remove_internal_aggregation_columns <- function(data) {
    dt <- copy(data)
    
    technical_cols <- intersect(
      c("raw_code", "group_code"),
      names(dt)
    )
    
    if (length(technical_cols) > 0) {
      dt[, (technical_cols) := NULL]
    }
    
    dt[]
  }
  
  
  # Order aggregation outputs consistently for display.
  order_aggregation_output_for_display <- function(data, cfg) {
    
    dt <- copy(
      as.data.table(data)
    )
    
    sort_columns <- c(
      cfg$geographical_area_col,
      cfg$production_source_col,
      cfg$species_col,
      cfg$fishing_area_col
    )
    
    sort_columns <- sort_columns[
      !is.null(sort_columns) &
        sort_columns %in% names(dt)
    ]
    
    temporary_columns <- character(0)
    
    for (column_i in sort_columns) {
      
      values <- as.character(
        dt[[column_i]]
      )
      
      prefix <- paste0(
        ".__sort_",
        column_i
      )
      
      type_col <- paste0(
        prefix,
        "_type"
      )
      
      numeric_col <- paste0(
        prefix,
        "_numeric"
      )
      
      text_col <- paste0(
        prefix,
        "_text"
      )
      
      # Custom aggregations first, ordinary codes next, Other last.
      dt[
        ,
        (type_col) := fifelse(
          grepl(
            "^Custom Aggregation",
            values
          ),
          0L,
          fifelse(
            grepl(
              "^Other",
              values
            ),
            2L,
            1L
          )
        )
      ]
      
      dt[
        ,
        (numeric_col) :=
          suppressWarnings(
            as.numeric(values)
          )
      ]
      
      dt[
        ,
        (text_col) := values
      ]
      
      temporary_columns <- c(
        temporary_columns,
        type_col,
        numeric_col,
        text_col
      )
    }
    
    # Keep years in chronological order within each aggregation combination.
    year_sort_col <- NULL
    
    if (
      !is.null(cfg$year_col) &&
      cfg$year_col %in% names(dt)
    ) {
      
      year_sort_col <- ".__sort_year"
      
      dt[
        ,
        (year_sort_col) :=
          period_start_value(
            get(cfg$year_col)
          )
      ]
      
      temporary_columns <- c(
        temporary_columns,
        year_sort_col
      )
    }
    
    if (length(temporary_columns) > 0L) {
      
      setorderv(
        dt,
        temporary_columns,
        na.last = TRUE
      )
      
      dt[
        ,
        (temporary_columns) := NULL
      ]
    }
    
    dt[]
  }
  
  
  format_dimension_codes_for_display <- function(data) {
    dt <- remove_internal_aggregation_columns(data)
    
    for (dim_id in names(AGGREGATION_DIMENSIONS)) {
      
      meta <- AGGREGATION_DIMENSIONS[[dim_id]]
      column_name <- meta$dataset_column
      
      if (
        is.null(column_name) ||
        !column_name %in% names(dt)
      ) {
        next
      }
      
      codes <- tryCatch(
        get_codelist_codes(meta$codelist),
        error = function(e) NULL
      )
      
      if (is.null(codes)) {
        next
      }
      
      display_map <- make_tree_display_labels(codes)
      original_values <- as.character(dt[[column_name]])
      display_values <- unname(display_map[original_values])
      
      has_label <- !is.na(display_values) &
        nzchar(display_values)
      
      original_values[has_label] <-
        display_values[has_label]
      
      dt[, (column_name) := original_values]
    }
    
    dt[]
  }
  
  
  comparison_difference_rows <- function(data) {
    dt <- copy(data)
    
    if (!"comparison_status" %in% names(dt)) {
      return(dt[0])
    }
    
    dt[
      is.na(comparison_status) |
        comparison_status != "Same value"
    ]
  }
  
  
  render_comparison_output_set <- function(
    differences_only = FALSE
  ) {
    results <- comparison_results()
    
    if (is.null(results) || length(results) == 0) {
      return(
        helpText(
          paste0(
            "No comparison result is available yet. ",
            "Load a comparison dataset and click Run comparison."
          )
        )
      )
    }
    
    tab_panels <- lapply(
      names(results),
      function(output_name) {
        
        safe_id <- gsub(
          "[^A-Za-z0-9_]",
          "_",
          output_name
        )
        
        table_id <- paste0(
          if (isTRUE(differences_only)) {
            "comparison_differences_"
          } else {
            "comparison_all_"
          },
          safe_id
        )
        
        download_id <- paste0(
          if (isTRUE(differences_only)) {
            "download_comparison_differences_"
          } else {
            "download_comparison_all_"
          },
          safe_id
        )
        
        result_dt <- results[[output_name]]
        
        is_warning <- isTRUE(
          attr(
            result_dt,
            "is_warning_output"
          )
        )
        
        if (is_warning) {
          return(
            nav_panel(
              title = paste0(output_name, " ⚠"),
              
              card(
                card_header(
                  paste0(
                    "Comparison output — measured element: ",
                    output_name
                  )
                ),
                
                div(
                  class = "alert alert-warning",
                  result_dt$Message[1]
                )
              )
            )
          )
        }
        
        number_of_differences <- nrow(
          comparison_difference_rows(
            result_dt
          )
        )
        
        local({
          output_name_local <- output_name
          table_id_local <- table_id
          download_id_local <- download_id
          differences_only_local <- differences_only
          
          output[[table_id_local]] <- renderDT({
            dt_to_show <- comparison_results()[[
              output_name_local
            ]]
            
            if (isTRUE(differences_only_local)) {
              dt_to_show <- comparison_difference_rows(
                dt_to_show
              )
            }
            
            dt_to_show <- format_dimension_codes_for_display(
              dt_to_show
            )
            
            datatable(
              dt_to_show,
              rownames = FALSE,
              filter = "top",
              options = list(
                pageLength = 10,
                scrollX = TRUE
              )
            )
          })
          
          output[[download_id_local]] <-
            downloadHandler(
              filename = function() {
                paste0(
                  if (isTRUE(differences_only_local)) {
                    "comparison_differences_"
                  } else {
                    "comparison_all_"
                  },
                  output_name_local,
                  ".csv"
                )
              },
              
              content = function(file) {
                dt_download <- comparison_results()[[
                  output_name_local
                ]]
                
                if (isTRUE(differences_only_local)) {
                  dt_download <- comparison_difference_rows(
                    dt_download
                  )
                }
                
                dt_download <- remove_internal_aggregation_columns(
                  dt_download
                )
                
                fwrite(
                  dt_download,
                  file
                )
              }
            )
          
          outputOptions(
            output,
            download_id_local,
            suspendWhenHidden = FALSE
          )
        })
        
        header_text <- if (isTRUE(differences_only)) {
          paste0(
            "Differences only — measured element: ",
            output_name,
            " — ",
            number_of_differences,
            " row(s)"
          )
        } else {
          paste0(
            "All comparison rows — measured element: ",
            output_name
          )
        }
        
        nav_panel(
          title = output_name,
          
          card(
            full_screen = TRUE,
            
            card_header(
              header_text
            ),
            
            DTOutput(
              table_id
            ),
            
            card_footer(
              downloadButton(
                download_id,
                if (isTRUE(differences_only)) {
                  paste0(
                    "Download ",
                    output_name,
                    " differences"
                  )
                } else {
                  paste0(
                    "Download all ",
                    output_name,
                    " comparison rows"
                  )
                }
              )
            )
          )
        )
      }
    )
    
    do.call(
      navset_card_tab,
      tab_panels
    )
  }
  
  
  output$comparison_results_tables <- renderUI({
    render_comparison_output_set(
      differences_only = FALSE
    )
  })
  
  
  output$comparison_differences_tables <- renderUI({
    render_comparison_output_set(
      differences_only = TRUE
    )
  })
  
  
  output$client <- renderTable({
    req(user())
    u <- as.data.table(user())
    u[, seq_len(min(5, ncol(u))), with = FALSE]
  })
  
  output$dataset_count <- renderText({
    req(input$dataset_group)
    
    n <- length(get_dataset_choices(input$dataset_group))
    
    paste0(
      n,
      " configured datasets available in group '",
      input$dataset_group,
      "'."
    )
  })
  
  observe({
    req(input$dataset_group)
    
    updateSelectizeInput(
      session,
      "dataset_id",
      choices = get_dataset_choices(input$dataset_group),
      selected = character(0),
      server = TRUE
    )
  })
  
  observeEvent(input$load_dataset, {
    req(input$dataset_id)
    
    tryCatch(
      {
        showNotification(
          paste("Reading dataset:", input$dataset_id),
          type = "default"
        )
        
        dataset_info <- getDatasetInfo(
          input$dataset_id
        )
        
        dt <- as.data.table(
          readDataset(
            dataset_id = input$dataset_id
          )
        )
        
        validation <- validate_dataset_columns(
          data = dt,
          dataset_id = input$dataset_id
        )
        
        rows_before_value_cleaning <- nrow(dt)
        
        dt <- normalise_and_drop_empty_values(
          data = dt,
          value_col = validation$config$value_col
        )
        
        rows_discarded_without_values <-
          rows_before_value_cleaning - nrow(dt)
        
        dataset_data(dt)
        loaded_dataset_id(input$dataset_id)
        loaded_dataset_info(dataset_info)
        
        aggregated_data(NULL)
        
        aggregated_outputs(list())
        aggregation_input_data(NULL)
        last_aggregation_specs(NULL)
        last_comparison_state(NULL)
        
        showNotification(
          paste0(
            "Dataset loaded: ",
            nrow(dt),
            " rows. Type: ",
            validation$config$dataset_type,
            "."
          ),
          type = "message"
        )
      },
      error = function(e) {
        showNotification(
          paste0("Dataset loading failed: ", e$message),
          type = "error"
        )
      }
    )
  })
  
  
  output$dataset_status <- renderPrint({
    if (is.null(dataset_data()) || is.null(input$dataset_id) || !nzchar(input$dataset_id)) {
      cat("No dataset loaded yet.\n")
      return(NULL)
    }
    
    dt <- dataset_data()
    cfg <- get_dataset_config(input$dataset_id)
    
    cat("Dataset:", input$dataset_id, "\n")
    cat("Label:", cfg$label, "\n")
    cat("Group:", cfg$dataset_group, "\n")
    cat("Type:", cfg$dataset_type, "\n")
    cat("Rows:", nrow(dt), "\n")
    cat("Columns:", ncol(dt), "\n")
  })
  
  
  output$dataset_summary <- renderUI({
    
    req(
      dataset_data(),
      loaded_dataset_info(),
      input$dataset_id
    )
    
    # Prevent the summary from using metadata belonging
    # to a previously loaded dataset.
    req(
      identical(
        loaded_dataset_id(),
        input$dataset_id
      )
    )
    
    dt <- copy(dataset_data())
    cfg <- get_dataset_config(input$dataset_id)
    
    measured_col <- cfg$measured_element_col
    
    # Read the measured-element roots configured in SWS.
    measured_elements <- get_dataset_dimension_roots(
      dataset_info = loaded_dataset_info(),
      dimension_id = measured_col
    )
    
    measured_elements <- sort(
      unique(
        as.character(measured_elements)
      )
    )
    
    measured_elements <- measured_elements[
      !is.na(measured_elements) &
        nzchar(measured_elements)
    ]
    
    # Restrict the summary data to the configured
    # measured-element roots.
    dt_summary <- copy(dt)
    
    if (
      !is.null(measured_col) &&
      measured_col %in% names(dt_summary)
    ) {
      
      if (length(measured_elements) > 0) {
        
        dt_summary <- dt_summary[
          as.character(get(measured_col)) %in%
            measured_elements
        ]
        
      } else {
        
        # The dataset has a measured-element dimension,
        # but no measured-element roots are configured.
        dt_summary <- dt_summary[0]
      }
    }
    
    # Among the configured SWS roots, identify those that
    # actually have at least one usable row.
    measured_elements_with_data <- character(0)
    
    if (
      !is.null(measured_col) &&
      measured_col %in% names(dt_summary) &&
      nrow(dt_summary) > 0L
    ) {
      
      measured_elements_with_data <- intersect(
        measured_elements,
        sort(
          clean_code_vector(
            dt_summary[[measured_col]]
          )
        )
      )
    }
    
    # Calculate the year range only from records belonging
    # to the configured measured-element roots.
    years <- get_year_values(
      data = dt_summary,
      year_col = cfg$year_col
    )
    
    # Add measured-element labels and units.
    measured_element_labels <- measured_elements
    
    if (length(measured_elements) > 0) {
      
      codes <- tryCatch(
        get_codelist_codes("measuredElement"),
        error = function(e) NULL
      )
      
      roots_dt <- data.table(
        measured_element_root = measured_elements
      )
      
      setnames(
        roots_dt,
        "measured_element_root",
        measured_col
      )
      
      measured_element_choices <- make_filter_choices_from_data(
        data = roots_dt,
        column_name = measured_col,
        codes = codes
      )
      
      if (length(measured_element_choices) > 0) {
        measured_element_labels <- names(
          measured_element_choices
        )
      }
    }
    
    
    measured_element_labels_with_data <-
      measured_elements_with_data
    
    if (length(measured_elements_with_data) > 0L) {
      
      matched_root_positions <- match(
        measured_elements_with_data,
        measured_elements
      )
      
      matched_roots <- !is.na(
        matched_root_positions
      )
      
      measured_element_labels_with_data[matched_roots] <-
        measured_element_labels[
          matched_root_positions[matched_roots]
        ]
    }
    
    
    available_dimensions <- c(
      "Geographical area",
      "Species / ASFIS",
      "Fishing area"
    )
    
    if (
      !is.null(cfg$production_source_col) &&
      cfg$production_source_col %in% names(dt)
    ) {
      available_dimensions <- c(
        available_dimensions,
        "Production source / environment type"
      )
    }
    
    tagList(
      tags$p(
        tags$strong("Dataset: "),
        paste0(
          cfg$dataset_id,
          " — ",
          cfg$label
        )
      ),
      
      tags$p(
        tags$strong("Dataset group: "),
        ifelse(
          identical(
            cfg$dataset_group,
            "current"
          ),
          "Current Fisheries dataset",
          "Disseminated / previous dataset"
        )
      ),
      
      tags$p(
        tags$strong(
          "Number of records under configured measured-element roots: "
        ),
        format(
          nrow(dt_summary),
          big.mark = ","
        )
      ),
      
      tags$p(
        tags$strong("Years available: "),
        if (length(years) > 0) {
          paste0(
            min(years),
            "–",
            max(years)
          )
        } else {
          "No valid year information available"
        }
      ),
      
      tags$p(
        tags$strong(
          "Number of configured measured-element roots: "
        ),
        length(measured_elements)
      ),
      
      tags$p(
        tags$strong(
          "Configured measured-element roots: "
        ),
        if (length(measured_element_labels) > 0) {
          paste(
            measured_element_labels,
            collapse = ", "
          )
        } else {
          "No measured-element roots are configured"
        }
      ),
      
      tags$p(
        tags$strong(
          "Number of configured roots with usable data: "
        ),
        length(
          measured_elements_with_data
        )
      ),
      
      tags$p(
        tags$strong(
          "Configured roots with usable data: "
        ),
        if (
          length(
            measured_element_labels_with_data
          ) > 0L
        ) {
          paste(
            measured_element_labels_with_data,
            collapse = ", "
          )
        } else {
          "None of the configured roots contains usable data"
        }
      ),
      
      tags$p(
        tags$strong("Main dimensions available: "),
        paste(
          available_dimensions,
          collapse = ", "
        )
      ),
      
      tags$div(
        class = "alert alert-info",
        paste0(
          "This summary is restricted to the measured-element roots ",
          "configured for the loaded dataset in SWS."
        )
      )
    )
  })
  
  output$raw_preview <- renderDT({
    req(dataset_data())
    
    datatable(
      head(dataset_data(), 1000),
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
  
  
  
  
  
  
  
  output$raw_year_plot <- renderPlot({
    
    req(
      dataset_data(),
      loaded_dataset_info(),
      input$dataset_id
    )
    
    # Do not use metadata left from a previously loaded dataset.
    req(
      identical(
        loaded_dataset_id(),
        input$dataset_id
      )
    )
    
    dt <- copy(dataset_data())
    cfg <- get_dataset_config(input$dataset_id)
    
    measured_col <- cfg$measured_element_col
    
    if (
      is.null(measured_col) ||
      !measured_col %in% names(dt)
    ) {
      plot.new()
      text(
        0.5,
        0.5,
        "No measured-element column is available for this dataset"
      )
      return(NULL)
    }
    
    # Read only the measured-element roots configured for
    # this specific dataset in SWS.
    measured_roots <- get_dataset_dimension_roots(
      dataset_info = loaded_dataset_info(),
      dimension_id = measured_col
    )
    
    measured_roots <- unique(
      trimws(
        as.character(
          measured_roots
        )
      )
    )
    
    measured_roots <- measured_roots[
      !is.na(measured_roots) &
        nzchar(measured_roots)
    ]
    
    if (length(measured_roots) == 0) {
      plot.new()
      text(
        0.5,
        0.5,
        "No measured-element roots are configured for this dataset"
      )
      return(NULL)
    }
    
    # The plot must contain only records belonging to the
    # configured measured-element roots.
    dt <- dt[
      as.character(get(measured_col)) %in%
        measured_roots
    ]
    
    if (nrow(dt) == 0) {
      plot.new()
      text(
        0.5,
        0.5,
        "No data are available for the configured measured-element roots"
      )
      return(NULL)
    }
    
    yearly <- summarise_total_by_year_and_element(
      data = dt,
      year_col = cfg$year_col,
      value_col = cfg$value_col,
      measured_element_col = measured_col
    )
    
    if (nrow(yearly) == 0) {
      plot.new()
      text(
        0.5,
        0.5,
        "No yearly data are available for the configured measured-element roots"
      )
      return(NULL)
    }
    
    ggplot(
      yearly,
      aes(
        x = year,
        y = total_value
      )
    ) +
      geom_line() +
      facet_wrap(
        ~ measured_element,
        scales = "free_y"
      ) +
      labs(
        x = "Year",
        y = "Total value",
        title = paste0(
          "Raw dataset total by year — configured measured-element roots: ",
          paste(measured_roots, collapse = ", ")
        )
      ) +
      theme_minimal()
  })
  
  output$year_selector <- renderUI({
    
    req(
      dataset_data(),
      input$dataset_id
    )
    
    cfg <- get_dataset_config(
      input$dataset_id
    )
    
    years <- get_year_values(
      data = dataset_data(),
      year_col = cfg$year_col
    )
    
    if (length(years) == 0) {
      return(
        helpText(
          "No valid years were found in timePointYears."
        )
      )
    }
    
    min_year <- min(years)
    max_year <- max(years)
    
    div(
      class = "row g-2 align-items-end",
      
      # Slider and selected-period text.
      div(
        class = "col-12 col-xl-7",
        
        div(
          class = paste(
            "d-flex",
            "justify-content-between",
            "align-items-center",
            "mb-1"
          ),
          
          tags$strong("Years"),
          
          div(
            class = "small text-muted",
            
            textOutput(
              "selected_year_range_label",
              inline = TRUE
            )
          )
        ),
        
        sliderInput(
          "year_range",
          NULL,
          min = min_year,
          max = max_year,
          value = c(min_year, max_year),
          step = 1,
          sep = "",
          ticks = FALSE,
          width = "100%"
        )
      ),
      
      # All buttons on the right side of the same toolbar.
      div(
        class = "col-12 col-xl-5",
        
        div(
          class = paste(
            "d-flex",
            "flex-wrap",
            "gap-2",
            "justify-content-xl-end"
          ),
          
          actionButton(
            "year_full_range",
            "Full range",
            class = "btn-secondary btn-sm"
          ),
          
          actionButton(
            "year_latest_10",
            "Latest 10",
            class = "btn-secondary btn-sm"
          ),
          
          actionButton(
            "year_latest_20",
            "Latest 20",
            class = "btn-secondary btn-sm"
          ),
          
          actionButton(
            "reset_filter_page",
            "Reset page",
            class = "btn-warning btn-sm"
          )
        )
      )
    )
  })
  
  
  output$selected_year_range_label <- renderText({
    req(input$year_range)
    
    paste0(
      "Selected years: ",
      input$year_range[1],
      " - ",
      input$year_range[2]
    )
  })
  
  
  observeEvent(input$year_full_range, {
    req(dataset_data(), input$dataset_id)
    
    cfg <- get_dataset_config(input$dataset_id)
    
    years <- get_year_values(
      data = dataset_data(),
      year_col = cfg$year_col
    )
    
    if (length(years) == 0) {
      return(NULL)
    }
    
    updateSliderInput(
      session,
      "year_range",
      value = c(min(years), max(years))
    )
  })
  
  observeEvent(input$year_latest_10, {
    req(dataset_data(), input$dataset_id)
    
    cfg <- get_dataset_config(input$dataset_id)
    
    years <- get_year_values(
      data = dataset_data(),
      year_col = cfg$year_col
    )
    
    if (length(years) == 0) {
      return(NULL)
    }
    
    max_year <- max(years)
    min_year <- max(min(years), max_year - 9)
    
    updateSliderInput(
      session,
      "year_range",
      value = c(min_year, max_year)
    )
  })
  
  observeEvent(input$year_latest_20, {
    req(dataset_data(), input$dataset_id)
    
    cfg <- get_dataset_config(input$dataset_id)
    
    years <- get_year_values(
      data = dataset_data(),
      year_col = cfg$year_col
    )
    
    if (length(years) == 0) {
      return(NULL)
    }
    
    max_year <- max(years)
    min_year <- max(min(years), max_year - 19)
    
    updateSliderInput(
      session,
      "year_range",
      value = c(min_year, max_year)
    )
  })
  
  
  
  
  
  output$measured_element_checkbox_filter <- renderUI({
    
    req(
      dataset_data(),
      loaded_dataset_info(),
      input$dataset_id
    )
    
    # Prevent using information from a previously loaded dataset
    req(
      identical(
        loaded_dataset_id(),
        input$dataset_id
      )
    )
    
    dt <- dataset_data()
    cfg <- get_dataset_config(input$dataset_id)
    
    measured_col <- cfg$measured_element_col
    
    if (
      is.null(measured_col) ||
      !measured_col %in% names(dt)
    ) {
      return(
        helpText(
          "No measured-element column is available for this dataset."
        )
      )
    }
    
    # Read only the roots configured for measuredElement
    measured_roots <- get_dataset_dimension_roots(
      dataset_info = loaded_dataset_info(),
      dimension_id = measured_col
    )
    
    measured_roots <- intersect(
      clean_code_vector(
        measured_roots
      ),
      clean_code_vector(
        dt[[measured_col]]
      )
    )
    
    if (length(measured_roots) == 0) {
      return(
        helpText(
          "No configured measured-element root has usable data in this dataset."
        )
      )
    }
    
    # Read labels and units from the measuredElement codelist
    codes <- tryCatch(
      get_codelist_codes("measuredElement"),
      error = function(e) NULL
    )
    
    # Reuse make_filter_choices_from_data(), but give it only the roots
    roots_dt <- data.table(
      measured_element_root = measured_roots
    )
    
    setnames(
      roots_dt,
      "measured_element_root",
      measured_col
    )
    
    choices <- make_filter_choices_from_data(
      data = roots_dt,
      column_name = measured_col,
      codes = codes
    )
    
    if (length(choices) == 0) {
      return(
        helpText(
          "No measured-element choices could be created from the configured roots."
        )
      )
    }
    
    checkboxGroupInput(
      "filter_measured_element_values",
      "Select measured element(s)",
      choices = choices,
      selected = unname(choices)[1]
    )
  })
  
  
  # -------------------------------------------------------------------------
  # Aggregation-tree scope and per-tree selection controls
  # -------------------------------------------------------------------------
  
  clean_non_empty_codes <- function(x) {
    
    x <- unique(
      trimws(
        as.character(
          x %||% character(0)
        )
      )
    )
    
    x[
      !is.na(x) &
        nzchar(x)
    ]
  }
  
  
  get_relevant_hierarchy_codes <- function(
    tree_dt,
    filtered_raw_codes
  ) {
    
    tree_dt <- as.data.table(tree_dt)
    id_cols <- get_tree_id_cols(tree_dt)
    
    if (length(id_cols) == 0L) {
      return(character(0))
    }
    
    filtered_raw_codes <- clean_non_empty_codes(
      filtered_raw_codes
    )
    
    if (length(filtered_raw_codes) == 0L) {
      return(character(0))
    }
    
    # Work only with the hierarchy ID columns.
    tree_ids <- copy(
      tree_dt[, ..id_cols]
    )
    
    # Convert hierarchy codes to character once.
    for (column_i in id_cols) {
      set(
        tree_ids,
        j = column_i,
        value = as.character(
          tree_ids[[column_i]]
        )
      )
    }
    
    # Keep only filtered codes that actually occur
    # somewhere in the hierarchy.
    tree_codes <- unique(
      unlist(
        tree_ids,
        recursive = TRUE,
        use.names = FALSE
      )
    )
    
    tree_codes <- tree_codes[
      !is.na(tree_codes) &
        nzchar(tree_codes)
    ]
    
    filtered_raw_codes <- intersect(
      filtered_raw_codes,
      tree_codes
    )
    
    if (length(filtered_raw_codes) == 0L) {
      return(character(0))
    }
    
    ancestor_codes <- character(0)
    
    # Find the ancestors of all filtered codes by scanning
    # each hierarchy level once instead of once per code.
    for (i in seq_along(id_cols)) {
      
      values_i <- tree_ids[[id_cols[i]]]
      
      matching_rows <- which(
        !is.na(values_i) &
          nzchar(values_i) &
          values_i %in% filtered_raw_codes
      )
      
      if (
        length(matching_rows) > 0L &&
        i > 1L
      ) {
        
        ancestor_columns <- id_cols[
          seq_len(i - 1L)
        ]
        
        ancestor_codes <- c(
          ancestor_codes,
          unlist(
            tree_ids[
              matching_rows,
              ..ancestor_columns
            ],
            recursive = TRUE,
            use.names = FALSE
          )
        )
      }
    }
    
    ancestor_codes <- clean_non_empty_codes(
      ancestor_codes
    )
    
    unique(
      c(
        filtered_raw_codes,
        ancestor_codes
      )
    )
  }
  
  
  prune_codelist_tree_to_relevant_codes <- function(
    tree_dt,
    relevant_codes
  ) {
    
    out <- copy(
      as.data.table(tree_dt)
    )
    
    id_cols <- get_tree_id_cols(out)
    
    relevant_codes <- clean_non_empty_codes(
      relevant_codes
    )
    
    if (
      length(id_cols) == 0 ||
      length(relevant_codes) == 0
    ) {
      return(out[0])
    }
    
    # Remove hierarchy nodes that do not contain any filtered records.
    # The required ancestors remain because they were added to
    # relevant_codes by get_relevant_hierarchy_codes().
    for (column_i in id_cols) {
      
      values_i <- as.character(
        out[[column_i]]
      )
      
      remove_i <- (
        !is.na(values_i) &
          nzchar(values_i) &
          !values_i %in% relevant_codes
      )
      
      values_i[remove_i] <- NA_character_
      
      set(
        out,
        j = column_i,
        value = values_i
      )
    }
    
    keep_row <- Reduce(
      `|`,
      lapply(
        id_cols,
        function(column_i) {
          
          values_i <- as.character(
            out[[column_i]]
          )
          
          !is.na(values_i) &
            nzchar(values_i)
        }
      )
    )
    
    out <- out[keep_row]
    
    if (nrow(out) == 0) {
      return(out)
    }
    
    unique(
      out,
      by = id_cols
    )
  }
  
  
  filtered_data_for_aggregation_controls <- reactive({
    
    req(
      dataset_data(),
      input$dataset_id
    )
    
    # Apply year and all ordinary dimension filters.
    # Measured element is excluded here because it uses
    # checkboxGroupInput rather than shinyTree.
    dt <- get_current_filtered_data(
      exclude_dims = "measured_element",
      update_debug = FALSE
    )
    
    cfg <- get_dataset_config(
      input$dataset_id
    )
    
    selected_measured_elements <- clean_non_empty_codes(
      input$filter_measured_element_values
    )
    
    # Measured-element selection also affects the records available
    # to the aggregation controls, but no Clear all control is added
    # to the measured-element UI.
    if (
      length(selected_measured_elements) > 0 &&
      !is.null(cfg$measured_element_col) &&
      cfg$measured_element_col %in% names(dt)
    ) {
      
      dt <- filter_data_by_measured_element(
        data = dt,
        measured_elements =
          selected_measured_elements,
        measured_element_col =
          cfg$measured_element_col
      )
    }
    
    dt[]
  })
  
  
  get_effective_filter_roots <- function(
    dim_id,
    meta,
    codes,
    tree_dt
  ) {
    
    filter_tree_input <- input[[
      paste0(
        "filter_tree_",
        dim_id
      )
    ]]
    
    if (is.null(filter_tree_input)) {
      return(character(0))
    }
    
    roots <- tryCatch(
      get_selected_codes_from_tree(
        tree_input = filter_tree_input,
        codes = codes,
        tree_dt = tree_dt,
        expand_descendants = FALSE,
        selection_rule = "most_specific",
        codelist_id = meta$codelist
      ),
      error = function(e) {
        character(0)
      }
    )
    
    clean_non_empty_codes(
      roots
    )
  }
  
  build_filtered_aggregation_tree <- function(
    dim_id,
    meta,
    tree_purpose = c(
      "classification",
      "custom"
    )
  ) {
    
    tree_purpose <- match.arg(
      tree_purpose
    )
    
    dt_filtered <-
      filtered_data_for_aggregation_controls()
    
    if (
      nrow(dt_filtered) == 0L ||
      is.null(meta$dataset_column) ||
      !meta$dataset_column %in% names(dt_filtered) ||
      is.null(meta$codelist)
    ) {
      return(list())
    }
    
    filtered_raw_codes <- clean_non_empty_codes(
      dt_filtered[[meta$dataset_column]]
    )
    
    if (length(filtered_raw_codes) == 0L) {
      return(list())
    }
    
    codes <- as.data.table(
      get_codelist_codes(
        meta$codelist
      )
    )
    
    codes[
      ,
      id := as.character(id)
    ]
    
    complete_tree_dt <-
      get_codelist_tree_cached(
        meta$codelist
      )
    
    # ------------------------------------------------------------
    # Filtering and hierarchy aggregation are independent.
    #
    # The filtered records determine which raw codes are available.
    # The aggregation tree then shows every codelist hierarchy path
    # containing at least one of those filtered raw codes.
    #
    # Example:
    # - filtering through ISSCAAP may leave detailed ASFIS species;
    # - those same species may also occur under TAXONOMIC;
    # - therefore both ISSCAAP and TAXONOMIC must be shown as
    #   separate aggregation roots.
    # ------------------------------------------------------------
    data_relevant_codes <-
      get_relevant_hierarchy_codes(
        tree_dt = complete_tree_dt,
        filtered_raw_codes = filtered_raw_codes
      )
    
    relevant_codes <- clean_non_empty_codes(
      data_relevant_codes
    )
    
    # Read all real top-level classifications available in the
    # complete codelist tree. This includes alternative hierarchy
    # systems, not only roots configured for the dataset.
    codelist_root_codes <- clean_non_empty_codes(
      get_original_tree_roots(
        complete_tree_dt
      )
    )
    
    # Synthetic All and Expired roots are added, when appropriate,
    # by get_display_roots_for_tree(). Do not treat them as ordinary
    # codelist classification roots here.
    codelist_root_codes <- setdiff(
      codelist_root_codes,
      c(
        SYNTHETIC_ALL_ROOT_ID,
        SYNTHETIC_EXPIRED_ROOTS_ID
      )
    )
    
    # Preserve the configured dataset roots first, then append any
    # additional codelist classifications that contain filtered data.
    configured_roots <- clean_non_empty_codes(
      get_configured_roots_for_dimension(
        meta
      )
    )
    
    aggregation_root_candidates <- unique(
      c(
        configured_roots,
        codelist_root_codes
      )
    )
    
    # Only roots that contain at least one filtered raw code survive
    # the intersection performed by get_display_roots_for_tree().
    tree_root_codes <- get_display_roots_for_tree(
      codelist_id = meta$codelist,
      configured_roots = aggregation_root_candidates,
      relevant_codes = relevant_codes,
      purpose = tree_purpose
    )
    
    if (
      length(relevant_codes) == 0L ||
      length(tree_root_codes) == 0L
    ) {
      return(list())
    }
    
    filtered_tree_dt <-
      prune_codelist_tree_to_relevant_codes(
        tree_dt = complete_tree_dt,
        relevant_codes = relevant_codes
      )
    
    if (nrow(filtered_tree_dt) == 0L) {
      return(list())
    }
    
    relevant_codelist_codes <- codes[
      id %in% relevant_codes
    ]
    
    build_sws_codelist_tree_from_codelist_tree(
      tree_dt = filtered_tree_dt,
      codes = relevant_codelist_codes,
      max_depth =
        get_codelist_tree_display_depth(
          filtered_tree_dt
        ),
      root_codes = tree_root_codes
    )
  }
  
  
  get_explicit_tree_selection_labels <- function(
    tree_input,
    meta,
    selection_rule = c(
      "top",
      "most_specific",
      "none"
    )
  ) {
    
    selection_rule <- match.arg(
      selection_rule
    )
    
    if (is.null(tree_input)) {
      return(character(0))
    }
    
    codes <- NULL
    tree_dt <- NULL
    
    if (!is.null(meta$codelist)) {
      
      codes <- tryCatch(
        get_codelist_codes(
          meta$codelist
        ),
        error = function(e) NULL
      )
      
      tree_dt <- tryCatch(
        get_codelist_tree_cached(
          meta$codelist
        ),
        error = function(e) NULL
      )
    }
    
    selected_codes <- tryCatch(
      get_selected_codes_from_tree(
        tree_input = tree_input,
        codes = codes,
        tree_dt = tree_dt,
        expand_descendants = FALSE,
        selection_rule = selection_rule,
        codelist_id = meta$codelist
      ),
      error = function(e) {
        character(0)
      }
    )
    
    selected_codes <- clean_non_empty_codes(
      selected_codes
    )
    
    if (length(selected_codes) == 0) {
      return(character(0))
    }
    
    display_values <- selected_codes
    
    if (!is.null(codes)) {
      
      display_labels <- make_tree_display_labels(
        codes
      )
      
      matched_labels <- unname(
        display_labels[selected_codes]
      )
      
      valid_labels <- (
        !is.na(matched_labels) &
          nzchar(matched_labels)
      )
      
      display_values[valid_labels] <-
        matched_labels[valid_labels]
    }
    
    clean_non_empty_codes(
      display_values
    )
  }
  
  
  make_tree_selection_summary_ui <- function(
    tree_id,
    selected_labels,
    empty_text = "None"
  ) {
    
    selected_labels <- clean_non_empty_codes(
      selected_labels
    )
    
    selection_values <- if (
      length(selected_labels) == 0
    ) {
      
      span(
        style = "color: #777;",
        empty_text
      )
      
    } else {
      
      div(
        class = "tree-selection-chips",
        
        tagList(
          lapply(
            selected_labels,
            function(label_i) {
              
              span(
                class = "tree-selection-chip",
                
                span(
                  class = "tree-selection-chip-label",
                  label_i
                ),
                
                tags$button(
                  type = "button",
                  class = "tree-selection-remove",
                  `data-tree-id` = tree_id,
                  `data-node-label` = label_i,
                  title = paste0(
                    "Remove ",
                    label_i
                  ),
                  `aria-label` = paste0(
                    "Remove ",
                    label_i
                  ),
                  HTML("&times;")
                )
              )
            }
          )
        )
      )
    }
    
    div(
      class = "tree-selection-toolbar",
      
      div(
        class = "tree-selection-values",
        selection_values
      ),
      
      tags$button(
        type = "button",
        class = paste(
          "btn btn-sm btn-outline-secondary",
          "tree-selection-clear"
        ),
        `data-tree-id` = tree_id,
        disabled = if (
          length(selected_labels) == 0
        ) {
          "disabled"
        } else {
          NULL
        },
        "Clear all"
      )
    )
  }
  
  
  
  output$dimension_accordion_ui <- renderUI({
    
    req(
      dataset_data(),
      input$dataset_id
    )
    
    reset_id <- tree_reset_counter()
    dt <- dataset_data()
    
    
    # ============================================================
    # Dimensions available in the loaded dataset
    # ============================================================
    
    active_filter_dims <- FILTER_DIMENSIONS[
      vapply(
        FILTER_DIMENSIONS,
        function(meta) {
          !is.null(meta$dataset_column) &&
            meta$dataset_column %in% names(dt)
        },
        logical(1)
      )
    ]
    
    
    active_aggregation_dims <- AGGREGATION_DIMENSIONS[
      vapply(
        AGGREGATION_DIMENSIONS,
        function(meta) {
          !is.null(meta$dataset_column) &&
            meta$dataset_column %in% names(dt)
        },
        logical(1)
      )
    ]
    
    
    # ============================================================
    # COMPLETE ORIGINAL EXPLANATION
    # The wording below is unchanged.
    # ============================================================
    
    aggregation_help_ui <- tags$details(
      class = "aggregation-help",
      
      tags$summary(
        "How the aggregation controls work"
      ),
      
      div(
        class = "aggregation-help-body",
        
        p(
          paste0(
            "Filters decide which records are included. ",
            "Aggregation decides how those filtered records ",
            "are grouped in the output."
          )
        ),
        
        p(
          tags$b(
            "Available aggregation hierarchies: "
          ),
          paste0(
            "for direct-child and custom aggregation, the tree shows every ",
            "hierarchy classification containing at least one filtered raw code. ",
            "The classification names are shown as separate top-level roots. ",
            "For example, filtered ASFIS species may appear under both ISSCAAP ",
            "and TAXONOMIC, allowing filtering through one classification and ",
            "aggregation through the other."
          )
        ),
        
        tags$ul(
          tags$li(
            tags$b("Keep filtered codes separate: "),
            paste0(
              "no grouping is applied to that dimension. ",
              "Every detailed code remaining after filtering stays separate."
            )
          ),
          
          tags$li(
            tags$b("Combine all filtered values into one output: "),
            paste0(
              "all remaining codes for that dimension are combined into one result. ",
              "If exactly one hierarchy node was selected in the filter, its code ",
              "is retained; otherwise the output code is TOTAL."
            )
          ),
          
          tags$li(
            tags$b(
              "Aggregate selected filter groups separately: "
            ),
            paste0(
              "one output is created for each effective hierarchy group selected ",
              "in the filter. Each selected parent includes all filtered descendants ",
              "beneath it. For example, selecting Europe and Asia in the filter ",
              "produces separate Europe and Asia outputs. Selecting 1501, 1502 and ",
              "1503 produces separate outputs for 1501, 1502 and 1503. If only ",
              "ISSCAAP is selected, the output is one ISSCAAP group."
            )
          ),
          
          tags$li(
            tags$b(
              "Aggregate by direct children of one or more hierarchy classes: "
            ),
            paste0(
              "select one or more non-overlapping parent nodes in the hierarchy. ",
              "Every direct child beneath each selected parent becomes a separate ",
              "output and includes all descendants beneath that child. For example, ",
              "selecting ISSCAAP produces one output for every direct ISSCAAP class; ",
              "selecting 1501, 1502 and 1503 produces the direct children of all ",
              "three selected classes."
            )
          ),
          
          tags$li(
            tags$b("Custom aggregation: "),
            paste0(
              "select one or more hierarchy nodes and combine the selected nodes ",
              "and all their descendants into one output. One selected node keeps ",
              "its code; several selected nodes are labelled as a Custom Aggregation. ",
              "All filtered classes not included in the custom aggregation are combined ",
              "into one dimension-specific Other output."
            )
          ),
          
          tags$li(
            tags$b("Total all selected years into one period: "),
            paste0(
              "combines all selected years into one period instead of ",
              "keeping annual rows."
            )
          ),
          
          tags$li(
            tags$b("Aggregate separately by observation flag: "),
            paste0(
              "keeps observation-status categories separate throughout the ",
              "aggregation. For example, A records are aggregated only with A ",
              "records, E records only with E records, and N records only with N ",
              "records. The observation flag is preserved in the result."
            )
          )
        )
      )
    )
    
    
    # ============================================================
    # Existing filtering control for one dimension
    # ============================================================
    
    make_filter_card <- function(
    dim_id,
    meta
    ) {
      
      if (identical(dim_id, "measured_element")) {
        
        return(
          card(
            fill = FALSE,
            
            card_header(meta$label),
            
            uiOutput(
              "measured_element_checkbox_filter"
            )
          )
        )
      }
      
      
      card(
        fill = FALSE,
        
        card_header(meta$label),
        
        div(
          style = paste(
            "max-height: 260px;",
            "overflow-y: auto;",
            "overflow-x: auto;",
            "border: 1px solid #e5e5e5;",
            "border-radius: 6px;",
            "padding: 6px;",
            "background-color: white;"
          ),
          
          div(
            id = paste0(
              "filter_tree_wrapper_",
              dim_id,
              "_",
              reset_id
            ),
            
            shinyTree(
              paste0(
                "filter_tree_",
                dim_id
              ),
              checkbox = TRUE,
              search = TRUE,
              themeIcons = FALSE,
              themeDots = TRUE,
              three_state = FALSE,
              tie_selection = TRUE,
              whole_node = FALSE
            )
          )
        ),
        
        hr(),
        
        div(
          style = "padding: 0 6px 6px 6px;",
          
          strong("Selected filters:"),
          
          uiOutput(
            paste0(
              "selected_filter_summary_",
              dim_id
            )
          )
        )
      )
    }
    
    
    # ============================================================
    # Existing aggregation controls for one hierarchical dimension
    # ============================================================
    
    make_aggregation_card <- function(
    dim_id,
    meta
    ) {
      
      hierarchy_available <- length(
        tryCatch(
          get_aggregation_root_choices(meta),
          error = function(e) {
            character(0)
          }
        )
      ) > 0
      
      
      mode_choices <- c(
        "Keep filtered codes separate — no aggregation" =
          "none",
        
        "Combine all filtered values into one output" =
          "total"
      )
      
      
      if (isTRUE(hierarchy_available)) {
        
        mode_choices <- c(
          mode_choices,
          
          "Aggregate selected filter groups separately" =
            "selected_groups",
          
          "Aggregate by direct children of one or more hierarchy classes" =
            "classification"
        )
      }
      
      
      mode_choices <- c(
        mode_choices,
        
        "Custom aggregation — combine selected nodes into one output" =
          "custom"
      )
      
      
      # ----------------------------------------------------------
      # Original direct-child control and original explanation
      # ----------------------------------------------------------
      
      classification_control <- if (
        isTRUE(hierarchy_available)
      ) {
        
        conditionalPanel(
          condition = paste0(
            "input.aggregation_mode_",
            dim_id,
            " == 'classification'"
          ),
          
          tagList(
            tags$strong(
              "Select one or more non-overlapping hierarchy parents."
            ),
            
            tags$p(
              style = "margin-bottom: 6px;",
              
              paste0(
                "The top-level nodes identify the available hierarchy classifications. ",
                "For example, ISSCAAP and TAXONOMIC are displayed as separate roots ",
                "when both contain filtered ASFIS species. ",
                "Tick the boxes beside one or more non-overlapping parent classes. ",
                "Each direct child of every selected parent becomes a separate output ",
                "and includes all descendants beneath that child. Other filtered codes ",
                "in the same dimension are combined into the dimension-specific Other output."
              )
            )
          ),
          
          div(
            style = paste(
              "max-height: 260px;",
              "overflow-y: auto;",
              "overflow-x: auto;",
              "border: 1px solid #e5e5e5;",
              "border-radius: 6px;",
              "padding: 6px;",
              "margin-top: 6px;",
              "background-color: white;"
            ),
            
            shinyTree(
              paste0(
                "aggregation_classification_tree_",
                dim_id
              ),
              checkbox = TRUE,
              search = TRUE,
              themeIcons = FALSE,
              themeDots = TRUE,
              multiple = TRUE,
              three_state = FALSE,
              tie_selection = TRUE,
              whole_node = FALSE,
              wholerow = TRUE
            )
          ),
          
          div(
            class = "tree-selection-summary-block",
            
            strong(
              "Selected aggregation parents:"
            ),
            
            uiOutput(
              paste0(
                "selected_aggregation_classification_summary_",
                dim_id
              )
            )
          )
        )
        
      } else {
        
        NULL
      }
      
      
      # ----------------------------------------------------------
      # Original custom control and original explanation
      # ----------------------------------------------------------
      
      custom_control <- conditionalPanel(
        condition = paste0(
          "input.aggregation_mode_",
          dim_id,
          " == 'custom'"
        ),
        
        tags$strong(
          paste0(
            "The top-level nodes identify every hierarchy classification containing ",
            "the filtered raw codes. Select one or more nodes from any available ",
            "classification and combine them into one custom output. All other ",
            "filtered records are combined into the dimension-specific Other output."
          )
        ),
        
        div(
          style = paste(
            "max-height: 260px;",
            "overflow-y: auto;",
            "overflow-x: auto;",
            "border: 1px solid #e5e5e5;",
            "border-radius: 6px;",
            "padding: 6px;",
            "margin-top: 6px;",
            "background-color: white;"
          ),
          
          shinyTree(
            paste0(
              "aggregation_custom_tree_",
              dim_id
            ),
            checkbox = TRUE,
            search = TRUE,
            themeIcons = FALSE,
            themeDots = TRUE,
            three_state = FALSE,
            tie_selection = TRUE,
            whole_node = FALSE
          )
        ),
        
        div(
          class = "tree-selection-summary-block",
          
          strong(
            "Selected custom aggregation nodes:"
          ),
          
          uiOutput(
            paste0(
              "selected_aggregation_custom_summary_",
              dim_id
            )
          )
        )
      )
      
      
      card(
        fill = FALSE,
        
        card_header(meta$label),
        
        radioButtons(
          inputId = paste0(
            "aggregation_mode_",
            dim_id
          ),
          label = NULL,
          choices = mode_choices,
          selected = "none"
        ),
        
        classification_control,
        custom_control
      )
    }
    
    
    # ============================================================
    # Measured element centred above the accordion
    # ============================================================
    
    measured_element_ui <- NULL
    
    if (
      "measured_element" %in%
      names(active_filter_dims)
    ) {
      
      measured_element_ui <- div(
        style = paste(
          "max-width: 900px;",
          "margin: 0 auto 12px auto;"
        ),
        
        make_filter_card(
          dim_id = "measured_element",
          meta = active_filter_dims[[
            "measured_element"
          ]]
        )
      )
    }
    
    
    # ============================================================
    # Hierarchical accordion panels
    # ============================================================
    
    hierarchical_panels <- lapply(
      names(active_aggregation_dims),
      function(dim_id) {
        
        filter_meta <- active_filter_dims[[
          dim_id
        ]]
        
        aggregation_meta <-
          active_aggregation_dims[[
            dim_id
          ]]
        
        if (is.null(filter_meta)) {
          return(NULL)
        }
        
        accordion_panel(
          title = aggregation_meta$label,
          value = dim_id,
          
          div(
            class = "row g-2 align-items-start",
            
            div(
              class = "col-12 col-xl-5",
              
              make_filter_card(
                dim_id = dim_id,
                meta = filter_meta
              )
            ),
            
            div(
              class = "col-12 col-xl-7",
              
              make_aggregation_card(
                dim_id = dim_id,
                meta = aggregation_meta
              )
            )
          )
        )
      }
    )
    
    
    # ============================================================
    # Currency filter accordion panel
    # It appears before Observation flag.
    # ============================================================
    
    currency_panel <- NULL
    
    if (
      "currency_flag" %in%
      names(active_filter_dims)
    ) {
      
      currency_panel <- accordion_panel(
        title = active_filter_dims[[
          "currency_flag"
        ]]$label,
        
        value = "currency_flag",
        
        make_filter_card(
          dim_id = "currency_flag",
          meta = active_filter_dims[[
            "currency_flag"
          ]]
        )
      )
    }
    
    
    # ============================================================
    # Observation flag accordion panel
    # This is deliberately the LAST accordion panel.
    # ============================================================
    
    observation_panel <- NULL
    
    if (
      "observation_flag" %in%
      names(active_filter_dims)
    ) {
      
      observation_panel <- accordion_panel(
        title = active_filter_dims[[
          "observation_flag"
        ]]$label,
        
        value = "observation_flag",
        
        div(
          class = "row g-2 align-items-start",
          
          div(
            class = "col-12 col-xl-7",
            
            make_filter_card(
              dim_id = "observation_flag",
              meta = active_filter_dims[[
                "observation_flag"
              ]]
            )
          ),
          
          div(
            class = "col-12 col-xl-5",
            
            card(
              fill = FALSE,
              
              card_header(
                "Observation flag aggregation"
              ),
              
              checkboxInput(
                "apply_observation_flag",
                "Aggregate separately by observation flag",
                value = FALSE
              )
            )
          )
        )
      )
    }
    
    
    # ============================================================
    # Assemble accordion in the requested order
    # ============================================================
    
    accordion_panels <- Filter(
      Negate(is.null),
      
      c(
        hierarchical_panels,
        list(
          currency_panel,
          observation_panel
        )
      )
    )
    
    
    accordion_ui <- if (
      length(accordion_panels) > 0
    ) {
      
      do.call(
        bslib::accordion,
        
        c(
          accordion_panels,
          
          list(
            id = "dimension_accordion",
            open = FALSE,
            multiple = TRUE
          )
        )
      )
      
    } else {
      
      helpText(
        "No filter or aggregation dimensions are available."
      )
    }
    
    
    # ============================================================
    # Final page content
    # ============================================================
    
    tagList(
      
      # Complete original explanation.
      aggregation_help_ui,
      
      
      # Measured element stays visible and centred.
      measured_element_ui,
      
      
      # All remaining dimensions can be opened and closed.
      accordion_ui,
      
      
      # Existing selected-year aggregation option.
      card(
        fill = FALSE,
        style = "margin-top: 12px;",
        
        checkboxInput(
          "aggregate_selected_years",
          "Total all selected years into one period",
          value = FALSE
        )
      )
    )
  })
  
  
  output$dimension_filter_selectors <- renderUI({
    req(dataset_data(), input$dataset_id)
    
    reset_id <- tree_reset_counter()
    
    dt <- dataset_data()
    
    active_dims <- FILTER_DIMENSIONS[
      vapply(
        FILTER_DIMENSIONS,
        function(meta) {
          !is.null(meta$dataset_column) &&
            meta$dataset_column %in% names(dt)
        },
        logical(1)
      )
    ]
    
    if (length(active_dims) == 0) {
      return(helpText("No additional filter dimensions are available."))
    }
    
    tagList(
      lapply(names(active_dims), function(dim_id) {
        meta <- active_dims[[dim_id]]
        
        if (identical(dim_id, "measured_element")) {
          return(
            card(
              card_header(meta$label),
              uiOutput("measured_element_checkbox_filter")
            )
          )
        }
        
        card(
          card_header(meta$label),
          
          div(
            style = paste(
              "max-height: 190px;",
              "overflow-y: auto;",
              "overflow-x: auto;",
              "border: 1px solid #e5e5e5;",
              "border-radius: 6px;",
              "padding: 6px;",
              "background-color: white;"
            ),
            
            div(
              id = paste0(
                "filter_tree_wrapper_",
                dim_id,
                "_",
                reset_id
              ),
              
              shinyTree(
                paste0("filter_tree_", dim_id),
                checkbox = TRUE,
                search = TRUE,
                themeIcons = FALSE,
                themeDots = TRUE,
                three_state = FALSE,
                tie_selection = TRUE,
                whole_node = FALSE
              )
            )
          ),
          
          hr(),
          
          div(
            style = "padding: 0 6px 6px 6px;",
            
            strong("Selected filters:"),
            
            uiOutput(
              paste0(
                "selected_filter_summary_",
                dim_id
              )
            )
          )
        )
      })
    )
  })
  
  
  output$aggregation_controls_ui <- renderUI({
    
    req(
      dataset_data(),
      input$dataset_id
    )
    
    dt <- dataset_data()
    
    # One explanation only. The same four modes are then shown
    # independently for every aggregation dimension.
    controls <- list(
      tags$details(
        class = "aggregation-help",
        
        tags$summary(
          "How the aggregation controls work"
        ),
        
        div(
          class = "aggregation-help-body",
          
          p(
            paste0(
              "Filters decide which records are included. ",
              "Aggregation decides how those filtered records ",
              "are grouped in the output."
            )
          ),
          
          p(
            tags$b(
              "Available aggregation hierarchies: "
            ),
            paste0(
              "for direct-child and custom aggregation, the tree shows every ",
              "hierarchy classification containing at least one filtered raw code. ",
              "The classification names are shown as separate top-level roots. ",
              "For example, filtered ASFIS species may appear under both ISSCAAP ",
              "and TAXONOMIC, allowing filtering through one classification and ",
              "aggregation through the other."
            )
          ),
          
          tags$ul(
            tags$li(
              tags$b("Keep filtered codes separate: "),
              paste0(
                "no grouping is applied to that dimension. ",
                "Every detailed code remaining after filtering stays separate."
              )
            ),
            
            tags$li(
              tags$b("Combine all filtered values into one output: "),
              paste0(
                "all remaining codes for that dimension are combined into one result. ",
                "If exactly one hierarchy node was selected in the filter, its code ",
                "is retained; otherwise the output code is TOTAL."
              )
            ),
            
            
            tags$li(
              tags$b(
                "Aggregate selected filter groups separately: "
              ),
              paste0(
                "one output is created for each effective hierarchy group selected ",
                "in the filter. Each selected parent includes all filtered descendants ",
                "beneath it. For example, selecting Europe and Asia in the filter ",
                "produces separate Europe and Asia outputs. Selecting 1501, 1502 and ",
                "1503 produces separate outputs for 1501, 1502 and 1503. If only ",
                "ISSCAAP is selected, the output is one ISSCAAP group."
              )
            ),
            
            
            tags$li(
              tags$b(
                "Aggregate by direct children of one or more hierarchy classes: "
              ),
              paste0(
                "select one or more non-overlapping parent nodes in the hierarchy. ",
                "Every direct child beneath each selected parent becomes a separate ",
                "output and includes all descendants beneath that child. For example, ",
                "selecting ISSCAAP produces one output for every direct ISSCAAP class; ",
                "selecting 1501, 1502 and 1503 produces the direct children of all ",
                "three selected classes."
              )
            ),
            
            tags$li(
              tags$b("Custom aggregation: "),
              paste0(
                "select one or more hierarchy nodes and combine the selected nodes ",
                "and all their descendants into one output. One selected node keeps ",
                "its code; several selected nodes are labelled as a Custom Aggregation. ",
                "All filtered classes not included in the custom aggregation are combined ",
                "into one dimension-specific Other output."
              )
            ),
            
            tags$li(
              tags$b("Total all selected years into one period: "),
              paste0(
                "combines all selected years into one period instead of ",
                "keeping annual rows."
              )
            ),
            
            tags$li(
              tags$b("Aggregate separately by observation flag: "),
              paste0(
                "keeps observation-status categories separate throughout the ",
                "aggregation. For example, A records are aggregated only with A ",
                "records, E records only with E records, and N records only with N ",
                "records. The observation flag is preserved in the result."
              )
            )
          )
        )
      )
    )
    
    for (
      dim_id in names(
        AGGREGATION_DIMENSIONS
      )
    ) {
      
      meta <- AGGREGATION_DIMENSIONS[[dim_id]]
      
      if (
        is.null(meta$dataset_column) ||
        !meta$dataset_column %in% names(dt)
      ) {
        next
      }
      
      hierarchy_available <- length(
        tryCatch(
          get_aggregation_root_choices(meta),
          error = function(e) character(0)
        )
      ) > 0
      
      mode_choices <- c(
        "Keep filtered codes separate — no aggregation" =
          "none",
        
        "Combine all filtered values into one output" =
          "total"
      )
      
      if (isTRUE(hierarchy_available)) {
        mode_choices <- c(
          mode_choices,
          
          "Aggregate selected filter groups separately" =
            "selected_groups",
          
          "Aggregate by direct children of one or more hierarchy classes" =
            "classification"
        )
      }
      
      mode_choices <- c(
        mode_choices,
        
        "Custom aggregation — combine selected nodes into one output" =
          "custom"
      )
      
      classification_control <- if (
        isTRUE(hierarchy_available)
      ) {
        conditionalPanel(
          condition = paste0(
            "input.aggregation_mode_",
            dim_id,
            " == 'classification'"
          ),
          
          tagList(
            tags$strong(
              "Select one or more non-overlapping hierarchy parents."
            ),
            
            tags$p(
              style = "margin-bottom: 6px;",
              paste0(
                "The top-level nodes identify the available hierarchy classifications. ",
                "For example, ISSCAAP and TAXONOMIC are displayed as separate roots ",
                "when both contain filtered ASFIS species. ",
                "Tick the boxes beside one or more non-overlapping parent classes. ",
                "Each direct child of every selected parent becomes a separate output ",
                "and includes all descendants beneath that child. Other filtered codes ",
                "in the same dimension are combined into the dimension-specific Other output."
              )
            )
          ),
          
          div(
            style = paste(
              "max-height: 260px;",
              "overflow-y: auto;",
              "overflow-x: auto;",
              "border: 1px solid #e5e5e5;",
              "border-radius: 6px;",
              "padding: 6px;",
              "margin-top: 6px;",
              "background-color: white;"
            ),
            
            shinyTree(
              paste0(
                "aggregation_classification_tree_",
                dim_id
              ),
              checkbox = TRUE,
              search = TRUE,
              themeIcons = FALSE,
              themeDots = TRUE,
              multiple = TRUE,
              three_state = FALSE,
              tie_selection = TRUE,
              whole_node = FALSE,
              wholerow = TRUE
            )
          ),
          
          div(
            class = "tree-selection-summary-block",
            
            strong(
              "Selected aggregation parents:"
            ),
            
            uiOutput(
              paste0(
                "selected_aggregation_classification_summary_",
                dim_id
              )
            )
          )
        )
      } else {
        NULL
      }
      
      custom_control <- conditionalPanel(
        condition = paste0(
          "input.aggregation_mode_",
          dim_id,
          " == 'custom'"
        ),
        
        tags$strong(
          paste0(
            "The top-level nodes identify every hierarchy classification containing ",
            "the filtered raw codes. Select one or more nodes from any available ",
            "classification and combine them into one custom output. All other ",
            "filtered records are combined into the dimension-specific Other output."
          )
        ),
        
        div(
          style = paste(
            "max-height: 260px;",
            "overflow-y: auto;",
            "overflow-x: auto;",
            "border: 1px solid #e5e5e5;",
            "border-radius: 6px;",
            "padding: 6px;",
            "margin-top: 6px;",
            "background-color: white;"
          ),
          
          shinyTree(
            paste0(
              "aggregation_custom_tree_",
              dim_id
            ),
            checkbox = TRUE,
            search = TRUE,
            themeIcons = FALSE,
            themeDots = TRUE,
            three_state = FALSE,
            tie_selection = TRUE,
            whole_node = FALSE
          )
        ),
        
        div(
          class = "tree-selection-summary-block",
          
          strong(
            "Selected custom aggregation nodes:"
          ),
          
          uiOutput(
            paste0(
              "selected_aggregation_custom_summary_",
              dim_id
            )
          )
        )
      )
      
      controls <- c(
        controls,
        list(
          div(
            style = paste(
              "border: 1px solid #dddddd;",
              "border-radius: 6px;",
              "padding: 10px;",
              "margin-bottom: 12px;"
            ),
            
            tags$strong(meta$label),
            
            radioButtons(
              inputId = paste0(
                "aggregation_mode_",
                dim_id
              ),
              
              label = NULL,
              
              choices = mode_choices,
              
              selected = "none"
            ),
            
            classification_control,
            custom_control
          )
        )
      )
    }
    
    controls <- c(
      controls,
      list(
        checkboxInput(
          "aggregate_selected_years",
          "Total all selected years into one period",
          value = FALSE
        )
      )
    )
    
    cfg <- get_dataset_config(
      input$dataset_id
    )
    
    if (
      !is.null(cfg$observation_flag_col) &&
      cfg$observation_flag_col %in% names(dt)
    ) {
      controls <- c(
        controls,
        list(
          checkboxInput(
            "apply_observation_flag",
            "Aggregate separately by observation flag",
            value = FALSE
          )
        )
      )
    }
    
    tagList(controls)
  })
  
  
  # Classification aggregation displays only hierarchy branches
  # that overlap the currently filtered data.
  for (dim_id in names(AGGREGATION_DIMENSIONS)) {
    local({
      
      current_dim <- dim_id
      meta <- AGGREGATION_DIMENSIONS[[current_dim]]
      
      output[[
        paste0(
          "aggregation_classification_tree_",
          current_dim
        )
      ]] <- renderTree({
        
        req(
          dataset_data(),
          input$dataset_id
        )
        
        tree_reset_counter()
        
        build_filtered_aggregation_tree(
          dim_id = current_dim,
          meta = meta,
          tree_purpose = "classification"
        )
      })
    })
  }
  
  
  # Custom aggregation uses the same filtered hierarchy scope.
  for (dim_id in names(AGGREGATION_DIMENSIONS)) {
    local({
      
      current_dim <- dim_id
      meta <- AGGREGATION_DIMENSIONS[[current_dim]]
      
      output[[
        paste0(
          "aggregation_custom_tree_",
          current_dim
        )
      ]] <- renderTree({
        
        req(
          dataset_data(),
          input$dataset_id
        )
        
        tree_reset_counter()
        
        build_filtered_aggregation_tree(
          dim_id = current_dim,
          meta = meta,
          tree_purpose = "custom"
        )
      })
    })
  }
  
  
  # Selected-value summaries for classification and custom aggregation.
  for (dim_id in names(AGGREGATION_DIMENSIONS)) {
    local({
      
      current_dim <- dim_id
      meta <- AGGREGATION_DIMENSIONS[[current_dim]]
      
      
      output[[
        paste0(
          "selected_aggregation_classification_summary_",
          current_dim
        )
      ]] <- renderUI({
        
        req(
          dataset_data(),
          input$dataset_id
        )
        
        tree_id <- paste0(
          "aggregation_classification_tree_",
          current_dim
        )
        
        selected_labels <-
          get_explicit_tree_selection_labels(
            tree_input = input[[tree_id]],
            meta = meta
          )
        
        make_tree_selection_summary_ui(
          tree_id = tree_id,
          selected_labels = selected_labels,
          empty_text = "None"
        )
      })
      
      
      output[[
        paste0(
          "selected_aggregation_custom_summary_",
          current_dim
        )
      ]] <- renderUI({
        
        req(
          dataset_data(),
          input$dataset_id
        )
        
        tree_id <- paste0(
          "aggregation_custom_tree_",
          current_dim
        )
        
        selected_labels <-
          get_explicit_tree_selection_labels(
            tree_input = input[[tree_id]],
            meta = meta
          )
        
        make_tree_selection_summary_ui(
          tree_id = tree_id,
          selected_labels = selected_labels,
          empty_text = "None"
        )
      })
    })
  }
  
  for (dim_id in names(FILTER_DIMENSIONS)) {
    local({
      current_dim <- dim_id
      meta <- FILTER_DIMENSIONS[[current_dim]]
      
      output[[paste0("filter_tree_", current_dim)]] <- renderTree({
        req(dataset_data(), input$dataset_id)
        
        tree_reset_counter()
        
        dt <- dataset_data()
        
        if (
          is.null(meta$dataset_column) ||
          !meta$dataset_column %in% names(dt)
        ) {
          return(list())
        }
        
        configured_roots <- get_configured_roots_for_dimension(
          meta
        )
        
        # ------------------------------------------------------------
        # Measured element must NOT be built from the full codelist tree.
        # It must show only measured elements actually present
        # in the currently loaded dataset.
        # ------------------------------------------------------------
        if (identical(current_dim, "measured_element")) {
          
          values <- sort(unique(as.character(dt[[meta$dataset_column]])))
          values <- values[!is.na(values) & nzchar(values)]
          
          if (length(values) == 0) {
            return(list())
          }
          
          codes <- tryCatch(
            get_codelist_codes("measuredElement"),
            error = function(e) NULL
          )
          
          labels <- values
          
          if (!is.null(codes)) {
            codes <- as.data.table(codes)
            codes[, id := as.character(id)]
            
            label_col <- intersect(
              c("label_en", "label", "description"),
              names(codes)
            )[1]
            
            label_dt <- codes[
              id %in% values,
              .(
                id,
                label_text = if (!is.na(label_col)) {
                  as.character(get(label_col))
                } else {
                  id
                },
                unit_text = if ("unit" %in% names(codes)) {
                  as.character(unit)
                } else {
                  ""
                }
              )
            ]
            
            label_dt[
              is.na(label_text) | !nzchar(label_text),
              label_text := id
            ]
            
            label_dt[
              is.na(unit_text),
              unit_text := ""
            ]
            
            label_dt[, unit_text := trimws(unit_text)]
            
            label_dt[
              nzchar(unit_text),
              display_label := paste0(id, " - ", label_text, " [", unit_text, "]")
            ]
            
            label_dt[
              !nzchar(unit_text),
              display_label := paste0(id, " - ", label_text)
            ]
            
            matched <- match(values, label_dt$id)
            has_match <- !is.na(matched)
            
            labels[has_match] <- label_dt$display_label[matched[has_match]]
          }
          
          flat_tree <- as.list(rep("", length(values)))
          names(flat_tree) <- labels
          
          return(flat_tree)
        }
        
        # ------------------------------------------------------------
        # Other flat filters: observation flag and currency flag.
        # These also use only values present in the loaded dataset.
        # ------------------------------------------------------------
        if (current_dim %in% c("observation_flag", "currency_flag")) {
          
          values <- sort(
            unique(
              as.character(
                dt[[meta$dataset_column]]
              )
            )
          )
          
          values <- values[
            !is.na(values) &
              nzchar(values)
          ]
          
          if (length(configured_roots) > 0) {
            values <- intersect(
              configured_roots,
              values
            )
          }
          
          if (length(values) == 0) {
            return(list())
          }
          
          flat_tree <- as.list(rep("", length(values)))
          names(flat_tree) <- values
          
          return(flat_tree)
        }
        
        # ------------------------------------------------------------
        # Hierarchical dimensions only.
        # These are built from the SWS codelist tree.
        # ------------------------------------------------------------
        if (!is.null(meta$codelist)) {
          
          raw_codes <- clean_non_empty_codes(
            dt[[meta$dataset_column]]
          )
          
          if (length(raw_codes) == 0) {
            return(list())
          }
          
          codes <- as.data.table(
            get_codelist_codes(
              meta$codelist
            )
          )
          
          codes[, id := as.character(id)]
          
          complete_tree_dt <- get_codelist_tree_cached(
            meta$codelist
          )
          
          relevant_codes <- get_relevant_hierarchy_codes(
            tree_dt = complete_tree_dt,
            filtered_raw_codes = raw_codes
          )
          
          if (length(relevant_codes) == 0) {
            return(list())
          }
          
          filtered_tree_dt <-
            prune_codelist_tree_to_relevant_codes(
              tree_dt = complete_tree_dt,
              relevant_codes = relevant_codes
            )
          
          if (nrow(filtered_tree_dt) == 0) {
            return(list())
          }
          
          relevant_codelist_codes <- codes[
            id %in% relevant_codes
          ]
          
          configured_roots_clean <- clean_non_empty_codes(
            configured_roots
          )
          
          tree_root_codes <- get_display_roots_for_tree(
            codelist_id = meta$codelist,
            configured_roots = configured_roots_clean,
            relevant_codes = relevant_codes,
            purpose = "filter"
          )
          
          if (
            length(tree_root_codes) == 0L
          ) {
            return(list())
          }
          
          return(
            build_sws_codelist_tree_from_codelist_tree(
              tree_dt = filtered_tree_dt,
              codes = relevant_codelist_codes,
              max_depth =
                get_codelist_tree_display_depth(
                  filtered_tree_dt
                ),
              root_codes = tree_root_codes
            )
          )
        }
        
        values <- sort(unique(as.character(dt[[meta$dataset_column]])))
        values <- values[!is.na(values) & nzchar(values)]
        
        flat_tree <- as.list(rep("", length(values)))
        names(flat_tree) <- values
        
        flat_tree
      })
      
      
      
      output[[
        paste0(
          "selected_filter_summary_",
          current_dim
        )
      ]] <- renderUI({
        
        req(
          dataset_data(),
          input$dataset_id
        )
        
        # Measured elements retain their existing checkbox selector.
        if (identical(
          current_dim,
          "measured_element"
        )) {
          return(NULL)
        }
        
        tree_id <- paste0(
          "filter_tree_",
          current_dim
        )
        
        selected_labels <- get_explicit_tree_selection_labels(
          tree_input = input[[tree_id]],
          meta = meta,
          selection_rule = if (
            is_flat_filter_dimension(current_dim)
          ) {
            "none"
          } else {
            "most_specific"
          }
        )
        
        make_tree_selection_summary_ui(
          tree_id = tree_id,
          selected_labels = selected_labels,
          empty_text = "None"
        )
      })
      
    })
  }
  
  observeEvent(input$reset_filter_page, {
    
    showNotification(
      "Reset button clicked.",
      type = "default",
      duration = 3
    )
    
    if (is.null(dataset_data()) ||
        is.null(input$dataset_id) ||
        !nzchar(input$dataset_id)) {
      
      showNotification(
        "No dataset is loaded, so there is nothing to reset.",
        type = "warning",
        duration = 8
      )
      
      return(NULL)
    }
    
    cfg <- get_dataset_config(input$dataset_id)
    dt <- dataset_data()
    
    # Clear outputs/results, but keep the loaded dataset.
    aggregated_data(NULL)
    aggregated_outputs(list())
    filter_debug(NULL)
    aggregation_input_data(NULL)
    last_aggregation_specs(NULL)
    last_comparison_state(NULL)
    comparison_results(NULL)
    
    # Reset year range.
    years <- get_year_values(
      data = dt,
      year_col = cfg$year_col
    )
    
    if (length(years) > 0) {
      updateSliderInput(
        session,
        "year_range",
        value = c(min(years), max(years))
      )
    }
    
    # Reset measured elements to the first configured measured-element root.
    measured_col <- cfg$measured_element_col
    
    if (
      !is.null(measured_col) &&
      measured_col %in% names(dt) &&
      !is.null(loaded_dataset_info())
    ) {
      
      measured_roots <- get_dataset_dimension_roots(
        dataset_info = loaded_dataset_info(),
        dimension_id = measured_col
      )
      
      if (length(measured_roots) > 0) {
        
        codes <- tryCatch(
          get_codelist_codes("measuredElement"),
          error = function(e) NULL
        )
        
        roots_dt <- data.table(
          measured_element_root = measured_roots
        )
        
        setnames(
          roots_dt,
          "measured_element_root",
          measured_col
        )
        
        measured_choices <- make_filter_choices_from_data(
          data = roots_dt,
          column_name = measured_col,
          codes = codes
        )
        
        if (length(measured_choices) > 0) {
          updateCheckboxGroupInput(
            session,
            "filter_measured_element_values",
            choices = measured_choices,
            selected = unname(measured_choices)[1]
          )
        }
      }
    }
    
    # Reset classification and custom aggregation controls.
    for (
      dim_id in names(
        AGGREGATION_DIMENSIONS
      )
    ) {
      updateRadioButtons(
        session,
        paste0(
          "aggregation_mode_",
          dim_id
        ),
        selected = "none"
      )
      
    }
    
    updateCheckboxInput(
      session,
      "apply_observation_flag",
      value = FALSE
    )
    
    
    updateCheckboxInput(
      session,
      "aggregate_selected_years",
      value = FALSE
    )
    # Force the filter UI to rebuild.
    tree_reset_counter(isolate(tree_reset_counter()) + 1)
    
    # Clear the checked nodes inside the browser-side shinyTree widgets.
    active_dims <- FILTER_DIMENSIONS[
      vapply(
        FILTER_DIMENSIONS,
        function(meta) {
          !is.null(meta$dataset_column) &&
            meta$dataset_column %in% names(dt)
        },
        logical(1)
      )
    ]
    
    tree_dims <- setdiff(
      names(active_dims),
      "measured_element"
    )
    
    filter_tree_ids <- paste0(
      "filter_tree_",
      tree_dims
    )
    
    custom_tree_dims <- names(
      AGGREGATION_DIMENSIONS
    )[
      vapply(
        AGGREGATION_DIMENSIONS,
        function(meta) {
          !is.null(meta$dataset_column) &&
            meta$dataset_column %in% names(dt)
        },
        logical(1)
      )
    ]
    
    classification_tree_ids <- paste0(
      "aggregation_classification_tree_",
      custom_tree_dims
    )
    
    custom_tree_ids <- paste0(
      "aggregation_custom_tree_",
      custom_tree_dims
    )
    
    tree_ids <- c(
      filter_tree_ids,
      classification_tree_ids,
      custom_tree_ids
    )
    
    session$onFlushed(
      function() {
        session$sendCustomMessage(
          "clear_shiny_trees",
          list(ids = tree_ids)
        )
      },
      once = TRUE
    )
    
    showNotification(
      "Filters and aggregation page reset. Tree selections were cleared.",
      type = "message",
      duration = 8
    )
  })
  
  get_current_filtered_data <- function(
    exclude_dims = character(0),
    update_debug = TRUE
  ) {
    req(dataset_data(), input$dataset_id)
    
    cfg <- get_dataset_config(input$dataset_id)
    
    debug_lines <- character(0)
    
    dt <- copy(
      base_analysis_data()
    )
    
    debug_lines <- c(
      debug_lines,
      paste0("Initial dataset rows: ", nrow(dt))
    )
    
    # ------------------------------------------------------------
    # Year filter
    # ------------------------------------------------------------
    before_n <- nrow(dt)
    
    dt <- filter_data_by_year(
      data = dt,
      year_range = input$year_range,
      year_col = cfg$year_col
    )
    
    debug_lines <- c(
      debug_lines,
      paste0(
        "After year filter ",
        paste(input$year_range %||% "<NULL>", collapse = " - "),
        ": ",
        nrow(dt),
        " rows; removed ",
        before_n - nrow(dt)
      )
    )
    
    # ------------------------------------------------------------
    # Tree/dimension filters
    # ------------------------------------------------------------
    active_dims <- FILTER_DIMENSIONS[
      vapply(
        FILTER_DIMENSIONS,
        function(meta) {
          !is.null(meta$dataset_column) &&
            meta$dataset_column %in% names(dt)
        },
        logical(1)
      )
    ]
    
    for (dim_id in names(active_dims)) {
      
      if (dim_id %in% exclude_dims) {
        next
      }
      
      meta <- active_dims[[dim_id]]
      
      before_n <- nrow(dt)
      selected_values <- character(0)
      selected_labels <- character(0)
      
      tree_input <- input[[paste0("filter_tree_", dim_id)]]
      
      if (!is.null(tree_input)) {
        selected_labels <- tryCatch(
          shinyTree::get_checked(
            tree_input,
            format = "names"
          ),
          error = function(e) {
            paste0("<error reading checked labels: ", e$message, ">")
          }
        )
        
        codes <- NULL
        
        if (!is.null(meta$codelist)) {
          codes <- tryCatch(
            get_codelist_codes(meta$codelist),
            error = function(e) NULL
          )
        }
        
        tree_dt <- NULL
        
        if (
          !is_flat_filter_dimension(dim_id) &&
          !is.null(meta$codelist)
        ) {
          
          tree_dt <- tryCatch(
            get_codelist_tree_cached(
              meta$codelist
            ),
            error = function(e) NULL
          )
        }
        
        selected_values <- get_selected_codes_from_tree(
          tree_input = tree_input,
          codes = codes,
          tree_dt = tree_dt,
          expand_descendants =
            !is_flat_filter_dimension(dim_id),
          selection_rule = if (
            is_flat_filter_dimension(dim_id)
          ) {
            "none"
          } else {
            "most_specific"
          },
          codelist_id = meta$codelist
        )
      }
      
      dt <- filter_data_by_selected_values(
        data = dt,
        selected_values = selected_values,
        column_name = meta$dataset_column
      )
      
      debug_lines <- c(
        debug_lines,
        paste0("---- ", meta$label, " ----"),
        paste0("Column: ", meta$dataset_column),
        paste0(
          "Selected raw tree labels: ",
          if (length(selected_labels) == 0) {
            "<none>"
          } else {
            paste(head(selected_labels, 20), collapse = " | ")
          }
        ),
        paste0(
          "Selected/expanded codes count: ",
          length(selected_values)
        ),
        paste0(
          "First selected/expanded codes: ",
          if (length(selected_values) == 0) {
            "<none>"
          } else {
            paste(head(selected_values, 30), collapse = ", ")
          }
        ),
        paste0(
          "Rows after this filter: ",
          nrow(dt),
          "; removed ",
          before_n - nrow(dt)
        )
      )
    }
    
    if (isTRUE(update_debug)) {
      filter_debug(debug_lines)
    }
    
    dt[]
  }
  
  
  
  get_active_aggregation_specs <- function(data) {
    
    dt_filtered <- copy(data)
    aggregation_specs <- list()
    custom_aggregation_index <- 0L
    
    for (
      dim_id in names(
        AGGREGATION_DIMENSIONS
      )
    ) {
      
      meta <- AGGREGATION_DIMENSIONS[[dim_id]]
      
      if (
        is.null(meta$dataset_column) ||
        !meta$dataset_column %in% names(dt_filtered)
      ) {
        next
      }
      
      aggregation_mode <- input[[
        paste0(
          "aggregation_mode_",
          dim_id
        )
      ]] %||% "none"
      
      if (identical(
        aggregation_mode,
        "none"
      )) {
        next
      }
      
      if (
        !aggregation_mode %in% c(
          "total",
          "selected_groups",
          "classification",
          "custom"
        )
      ) {
        stop(
          paste0(
            "Unknown aggregation mode for ",
            meta$label,
            "."
          )
        )
      }
      
      codes <- get_codelist_codes(
        meta$codelist
      )
      
      tree_dt <- get_codelist_tree_cached(
        meta$codelist
      )
      
      # ----------------------------------------------------------
      # Combine all filtered values into one TOTAL.
      # ----------------------------------------------------------
      # ----------------------------------------------------------
      # Combine all filtered values into one output.
      #
      # When exactly one filter group was explicitly selected,
      # preserve that group code. Example:
      # 1 - World becomes output code 1.
      #
      # With no explicit selection or several selected groups,
      # use the generic output code TOTAL.
      # ----------------------------------------------------------
      if (identical(
        aggregation_mode,
        "total"
      )) {
        
        raw_member_codes <- unique(
          as.character(
            dt_filtered[[meta$dataset_column]]
          )
        )
        
        raw_member_codes <- raw_member_codes[
          !is.na(raw_member_codes) &
            nzchar(raw_member_codes)
        ]
        
        if (length(raw_member_codes) == 0) {
          stop(
            paste0(
              "No filtered values are available for ",
              meta$label,
              "."
            )
          )
        }
        
        explicitly_selected_codes <-
          get_effective_filter_roots(
            dim_id = dim_id,
            meta = meta,
            codes = codes,
            tree_dt = tree_dt
          )
        
        total_output_code <- if (
          length(explicitly_selected_codes) == 1L
        ) {
          explicitly_selected_codes[1]
        } else {
          "TOTAL"
        }
        
        aggregation_specs[[dim_id]] <- list(
          dimension = dim_id,
          label = meta$label,
          key_dim_name = meta$dataset_column,
          codelist = meta$codelist,
          
          aggregation_mode = "total",
          root_code = total_output_code,
          aggregate_code = total_output_code,
          target_mode = "total_filtered_values",
          descendants_mode = "all_filtered_values",
          
          selected_codes = total_output_code,
          output_codes = total_output_code,
          child_codes = raw_member_codes,
          
          aggregation_map = data.table(
            raw_code = raw_member_codes,
            group_code = total_output_code
          )
        )
        
        next
      }
      
      
      # ----------------------------------------------------------
      # Aggregate each effective filter group separately.
      # ----------------------------------------------------------
      if (identical(
        aggregation_mode,
        "selected_groups"
      )) {
        
        selected_filter_groups <-
          get_effective_filter_roots(
            dim_id = dim_id,
            meta = meta,
            codes = codes,
            tree_dt = tree_dt
          )
        
        if (length(selected_filter_groups) == 0L) {
          stop(
            paste0(
              "Please select at least one hierarchy filter for ",
              meta$label,
              " before using 'Aggregate selected filter groups separately'."
            )
          )
        }
        
        filtered_raw_codes <- clean_non_empty_codes(
          dt_filtered[[meta$dataset_column]]
        )
        
        aggregation_map <-
          build_selected_groups_map_with_remainder(
            tree_dt = tree_dt,
            selected_codes = selected_filter_groups,
            filtered_raw_codes = filtered_raw_codes,
            codelist_id = meta$codelist,
            remainder_label =
              meta$remainder_label %||%
              "Other filtered records"
          )
        
        if (nrow(aggregation_map) == 0L) {
          stop(
            paste0(
              "None of the selected filter groups contains data for ",
              meta$label,
              "."
            )
          )
        }
        
        output_codes <- unique(
          as.character(
            aggregation_map$group_code
          )
        )
        
        raw_member_codes <- unique(
          as.character(
            aggregation_map$raw_code
          )
        )
        
        saved_root_code <- if (
          length(selected_filter_groups) == 1L
        ) {
          selected_filter_groups[1L]
        } else {
          "SELECTED_FILTER_GROUPS"
        }
        
        aggregation_specs[[dim_id]] <- list(
          dimension = dim_id,
          label = meta$label,
          key_dim_name = meta$dataset_column,
          codelist = meta$codelist,
          
          aggregation_mode = "selected_groups",
          root_code = saved_root_code,
          aggregate_code = saved_root_code,
          target_mode =
            "selected_filter_groups_separately",
          descendants_mode =
            "each_selected_filter_group_and_descendants",
          
          selected_codes = selected_filter_groups,
          output_codes = output_codes,
          child_codes = raw_member_codes,
          aggregation_map = aggregation_map
        )
        
        next
      }
      
      
      # ----------------------------------------------------------
      # Select one or more hierarchy parents. Every direct child
      # beneath every selected parent becomes a separate output and
      # includes all descendants beneath that child.
      # ----------------------------------------------------------
      if (identical(
        aggregation_mode,
        "classification"
      )) {
        
        classification_tree_input <- input[[
          paste0(
            "aggregation_classification_tree_",
            dim_id
          )
        ]]
        
        if (is.null(classification_tree_input)) {
          stop(
            paste0(
              "No hierarchy-classification tree is available for ",
              meta$label,
              "."
            )
          )
        }
        
        selected_parents <- get_selected_codes_from_tree(
          tree_input = classification_tree_input,
          codes = codes,
          tree_dt = tree_dt,
          expand_descendants = FALSE,
          selection_rule = "top",
          codelist_id = meta$codelist
        )
        
        selected_parents <- clean_code_vector(
          selected_parents
        )
        
        if (length(selected_parents) == 0L) {
          stop(
            paste0(
              "Please select at least one hierarchy parent for ",
              meta$label,
              ". Its direct children will become separate outputs."
            )
          )
        }
        
        classification_codes <- if (
          identical(
            meta$codelist,
            "fisheriesCatchArea"
          )
        ) {
          codes
        } else {
          NULL
        }
        
        filtered_raw_codes <- clean_code_vector(
          dt_filtered[[meta$dataset_column]]
        )
        
        aggregation_map <-
          with_aggregation_context(
            
            build_classification_map_with_remainder(
              tree_dt = tree_dt,
              selected_parents = selected_parents,
              filtered_raw_codes = filtered_raw_codes,
              codes = classification_codes,
              remainder_label =
                meta$remainder_label %||%
                "Other filtered records"
            ),
            
            dimension_id = dim_id,
            label = meta$label,
            dataset_column = meta$dataset_column,
            codelist = meta$codelist,
            selected_codes = selected_parents,
            stage = "direct-child aggregation setup"
          )
        
        output_codes <- unique(
          as.character(
            aggregation_map$group_code
          )
        )
        
        raw_member_codes <- unique(
          as.character(
            aggregation_map$raw_code
          )
        )
        
        aggregation_specs[[dim_id]] <- list(
          dimension = dim_id,
          label = meta$label,
          key_dim_name = meta$dataset_column,
          codelist = meta$codelist,
          
          aggregation_mode = "classification",
          root_code = selected_parents,
          aggregate_code = selected_parents,
          target_mode =
            "direct_children_of_selected_parents",
          descendants_mode =
            "each_direct_child_and_all_descendants",
          
          selected_codes = selected_parents,
          output_codes = output_codes,
          child_codes = raw_member_codes,
          aggregation_map = aggregation_map
        )
        
        next
      }
      
      # ----------------------------------------------------------
      # Existing original custom aggregation.
      # ----------------------------------------------------------
      custom_tree_input <- input[[
        paste0(
          "aggregation_custom_tree_",
          dim_id
        )
      ]]
      
      if (is.null(custom_tree_input)) {
        stop(
          paste0(
            "No custom-aggregation tree is available for ",
            meta$label,
            "."
          )
        )
      }
      
      selected_codes <- get_selected_codes_from_tree(
        tree_input = custom_tree_input,
        codes = codes,
        tree_dt = tree_dt,
        expand_descendants = FALSE
      )
      
      selected_codes <- keep_top_selected_codes_from_codelist_tree(
        selected_codes = selected_codes,
        tree_dt = tree_dt
      )
      
      if (length(selected_codes) == 0) {
        stop(
          paste0(
            "Please select at least one custom-aggregation node for ",
            meta$label,
            "."
          )
        )
      }
      
      if (length(selected_codes) == 1) {
        
        aggregate_code <- selected_codes[1]
        
      } else {
        
        custom_aggregation_index <-
          custom_aggregation_index + 1L
        
        aggregate_code <- paste0(
          "Custom Aggregation ",
          custom_aggregation_index
        )
      }
      
      filtered_raw_codes <- unique(
        trimws(
          as.character(
            dt_filtered[[meta$dataset_column]]
          )
        )
      )
      
      filtered_raw_codes <- filtered_raw_codes[
        !is.na(filtered_raw_codes) &
          nzchar(filtered_raw_codes)
      ]
      
      # ----------------------------------------------------------
      # Build the custom output and combine every remaining filtered
      # code into the dimension-specific Other output.
      # ----------------------------------------------------------
      aggregation_map <-
        with_aggregation_context(
          
          build_custom_map_with_remainder(
            tree_dt = tree_dt,
            selected_codes = selected_codes,
            aggregate_code = aggregate_code,
            filtered_raw_codes = filtered_raw_codes,
            codelist_id = meta$codelist,
            remainder_label =
              meta$remainder_label %||%
              "Other filtered records",
            allow_remainder_only = FALSE
          ),
          
          dimension_id = dim_id,
          label = meta$label,
          dataset_column = meta$dataset_column,
          codelist = meta$codelist,
          selected_codes = selected_codes,
          stage = "custom aggregation setup"
        )
      
      custom_member_codes <- unique(
        as.character(
          aggregation_map[
            group_code == aggregate_code,
            raw_code
          ]
        )
      )
      
      raw_member_codes <- unique(
        as.character(
          aggregation_map$raw_code
        )
      )
      
      output_codes <- unique(
        as.character(
          aggregation_map$group_code
        )
      )
      
      if (length(selected_codes) == 1) {
        
        custom_descendant_codes <- setdiff(
          custom_member_codes,
          aggregate_code
        )
        
        if (length(custom_descendant_codes) == 0) {
          target_mode <- "leaf_passthrough"
        } else {
          target_mode <-
            "parent_with_descendants_including_target"
        }
        
      } else {
        
        target_mode <- "custom_selection"
      }
      
      aggregation_specs[[dim_id]] <- list(
        dimension = dim_id,
        label = meta$label,
        key_dim_name = meta$dataset_column,
        codelist = meta$codelist,
        
        aggregation_mode = "custom",
        root_code = aggregate_code,
        aggregate_code = aggregate_code,
        target_mode = target_mode,
        descendants_mode = "selected_nodes_and_descendants",
        
        selected_codes = selected_codes,
        output_codes = output_codes,
        child_codes = raw_member_codes,
        aggregation_map = aggregation_map
      )
    }
    
    aggregation_specs
  }
  
  
  # output$aggregation_setup_preview_ui <- renderUI({
  #   req(dataset_data())
  #   
  #   specs <- tryCatch(
  #     get_active_aggregation_specs(),
  #     error = function(e) {
  #       return(NULL)
  #     }
  #   )
  #   
  #   if (is.null(specs) || length(specs) == 0) {
  #     return(
  #       div(
  #         style = "padding: 12px; color: #666;",
  #         "No aggregation target selected yet. The filtered data below is still available."
  #       )
  #     )
  #   }
  #   
  #   DTOutput("children_preview")
  # })
  
  # output$children_preview <- renderDT({
  #   req(dataset_data())
  #   
  #   specs <- tryCatch(
  #     get_active_aggregation_specs(),
  #     error = function(e) {
  #       return(list())
  #     }
  #   )
  #   
  #   if (length(specs) == 0) {
  #     return(
  #       datatable(
  #         data.table(
  #           message = "No complete aggregation selection yet."
  #         ),
  #         rownames = FALSE
  #       )
  #     )
  #   }
  #   
  #   preview <- rbindlist(
  #     lapply(specs, function(spec) {
  #       data.table(
  #         dimension = spec$label,
  #         root_code = spec$root_code,
  #         target_mode = ifelse(
  #           spec$target_mode == "root",
  #           "Using selected root",
  #           "Using lower-level aggregate"
  #         ),
  #         aggregate_code = spec$aggregate_code,
  #         aggregation_basis = "Full selected hierarchy",
  #         dataset_column = spec$key_dim_name,
  #         n_child_codes = length(spec$child_codes),
  #         first_child_codes = paste(head(spec$child_codes, 20), collapse = ", ")
  #       )
  #     }),
  #     fill = TRUE
  #   )
  #   
  #   datatable(
  #     preview,
  #     rownames = FALSE,
  #     options = list(pageLength = 10, scrollX = TRUE)
  #   )
  # })
  
  get_selected_measured_elements_for_aggregation <- function(data, cfg) {
    measured_col <- cfg$measured_element_col
    
    if (is.null(measured_col) || !measured_col %in% names(data)) {
      return("Aggregated output")
    }
    
    selected_measured_elements <- input$filter_measured_element_values
    
    if (is.null(selected_measured_elements) || length(selected_measured_elements) == 0) {
      stop("Please select at least one measured element.")
    }
    
    selected_measured_elements <- as.character(selected_measured_elements)
    selected_measured_elements <- trimws(selected_measured_elements)
    selected_measured_elements <- selected_measured_elements[
      !is.na(selected_measured_elements) &
        nzchar(selected_measured_elements)
    ]
    
    if (length(selected_measured_elements) == 0) {
      stop("Please select at least one measured element.")
    }
    
    unique(selected_measured_elements)
  }
  
  observeEvent(input$run_aggregation, {
    
    withProgress(
      message = "Running aggregation",
      value = 0,
      {
        tryCatch(
          {
            incProgress(
              amount = 0.02,
              detail = "Checking loaded dataset..."
            )
            
            req(dataset_data(), input$dataset_id)
            
            # Invalidate the previous aggregation immediately.
            # A failed new run must not leave the old aggregation
            # available to the Comparison page.
            aggregated_data(NULL)
            aggregated_outputs(list())
            aggregation_input_data(NULL)
            last_aggregation_specs(NULL)
            last_comparison_state(NULL)
            comparison_results(NULL)
            
            incProgress(
              amount = 0.05,
              detail = "Reading dataset configuration..."
            )
            
            cfg <- get_dataset_config(input$dataset_id)
            measured_col <- cfg$measured_element_col
            
            incProgress(
              amount = 0.10,
              detail = "Applying selected filters..."
            )
            
            # This can be slow, so it must be inside withProgress().
            dt <- get_current_filtered_data(
              exclude_dims = "measured_element"
            )
            
            if (nrow(dt) == 0) {
              showNotification(
                "No rows are available after applying the selected filters.",
                type = "error",
                duration = 10
              )
              return(NULL)
            }
            
            incProgress(
              amount = 0.10,
              detail = "Reading aggregation settings..."
            )
            
            aggregation_specs <- tryCatch(
              get_active_aggregation_specs(
                data = dt
              ),
              error = function(e) {
                showNotification(
                  paste0("Aggregation setup error: ", e$message),
                  type = "error",
                  duration = 10
                )
                return(NULL)
              }
            )
            
            if (is.null(aggregation_specs)) {
              return(NULL)
            }
            
            group_by_observation_flag <- isTRUE(
              input$apply_observation_flag
            )
            
            aggregate_selected_years <- isTRUE(
              input$aggregate_selected_years
            )
            
            year_total_label <- paste0(
              input$year_range[1],
              "-",
              input$year_range[2]
            )
            
            if (
              length(aggregation_specs) == 0 &&
              !group_by_observation_flag &&
              !aggregate_selected_years
            ) {
              showNotification(
                "Please select at least one aggregation dimension.",
                type = "error",
                duration = 10
              )
              return(NULL)
            }
            
            incProgress(
              amount = 0.10,
              detail = "Checking selected measured elements..."
            )
            
            selected_measured_elements <- get_selected_measured_elements_for_aggregation(
              data = dt,
              cfg = cfg
            )
            
            showNotification(
              paste0(
                "Measured elements selected for aggregation: ",
                paste(selected_measured_elements, collapse = ", ")
              ),
              type = "message",
              duration = 8
            )
            
            comparison_state <- capture_current_comparison_state(
              group_by_observation_flag =
                group_by_observation_flag,
              
              aggregate_selected_years =
                aggregate_selected_years,
              
              year_total_label =
                year_total_label
            )
            
            output_list <- list()
            successful_outputs <- list()
            
            n_elements <- length(selected_measured_elements)
            
            progress_step <- if (n_elements > 0) {
              0.50 / n_elements
            } else {
              0.50
            }
            
            for (me_i in selected_measured_elements) {
              
              incProgress(
                amount = progress_step,
                detail = paste0(
                  "Aggregating measured element ",
                  me_i,
                  "..."
                )
              )
              
              if (!is.null(measured_col) && measured_col %in% names(dt)) {
                dt_i <- dt[
                  as.character(get(measured_col)) == as.character(me_i)
                ]
              } else {
                dt_i <- copy(dt)
              }
              
              if (nrow(dt_i) == 0) {
                
                warning_dt <- data.table(
                  Message = paste0(
                    "No data available for measured element ",
                    me_i,
                    " after applying the selected year range and dimension filters. ",
                    "No aggregation was run for this measured element."
                  )
                )
                
                attr(warning_dt, "is_warning_output") <- TRUE
                
                output_list[[me_i]] <- warning_dt
                
                next
              }
              
              aggr_i <- tryCatch(
                aggregate_by_multiple_dimensions(
                  data = dt_i,
                  aggregation_specs = aggregation_specs,
                  value_col = cfg$value_col,
                  observation_flag = cfg$observation_flag_col,
                  method_flag = cfg$method_flag_col,
                  group_by_observation_flag = group_by_observation_flag,
                  aggregate_selected_years = aggregate_selected_years,
                  year_col = cfg$year_col,
                  year_total_label = year_total_label
                ),
                error = function(e) {
                  warning_dt <- data.table(
                    Message = paste0(
                      "Aggregation could not be completed for measured element ",
                      me_i,
                      ". Error: ",
                      e$message
                    )
                  )
                  
                  attr(warning_dt, "is_warning_output") <- TRUE
                  
                  warning_dt
                }
              )
              
              output_list[[me_i]] <- aggr_i
              
              if (!isTRUE(attr(aggr_i, "is_warning_output"))) {
                successful_outputs[[me_i]] <- aggr_i
              }
            }
            
            incProgress(
              amount = 0.08,
              detail = "Combining aggregation outputs..."
            )
            
            if (length(output_list) == 0) {
              stop("No output was produced.")
            }
            
            if (length(successful_outputs) > 0) {
              aggr_all <- rbindlist(
                successful_outputs,
                use.names = TRUE,
                fill = TRUE
              )
            } else {
              aggr_all <- data.table()
            }
            
            incProgress(
              amount = 0.05,
              detail = "Saving aggregation results..."
            )
            
            aggregated_data(aggr_all)
            aggregated_outputs(output_list)
            
            # Save the exact data, aggregation specifications and filter state
            # that produced aggregated_outputs().
            aggregation_input_data(dt)
            last_aggregation_specs(aggregation_specs)
            last_comparison_state(comparison_state)
            
            incProgress(
              amount = 0.05,
              detail = "Aggregation completed."
            )
            
            showNotification(
              paste0(
                "Aggregation completed: ",
                length(successful_outputs),
                " successful output(s); ",
                length(output_list) - length(successful_outputs),
                " warning tab(s) created."
              ),
              type = if (length(successful_outputs) == length(output_list)) {
                "message"
              } else {
                "warning"
              },
              duration = 10
            )
          },
          error = function(e) {
            showNotification(
              paste0("Aggregation failed: ", e$message),
              type = "error",
              duration = 10
            )
          }
        )
      }
    )
  })
  
  
  # Helper: returns the aggregation output currently selected by the user.
  # If there is only one output, it returns the full aggregated_data().
  selected_aggregated_data <- reactive({
    outputs <- aggregated_outputs()
    
    if (length(outputs) == 0) {
      req(aggregated_data())
      return(aggregated_data())
    }
    
    selected_output <- input$aggregation_output_id
    
    if (is.null(selected_output) || !selected_output %in% names(outputs)) {
      selected_output <- names(outputs)[1]
    }
    
    outputs[[selected_output]]
  })
  
  #This will show actual tabs like: FI_001, FI_002. Each tab is a separate aggregation output.
  output$aggregated_outputs_tables <- renderUI({
    outputs <- aggregated_outputs()
    
    if (length(outputs) == 0) {
      return(helpText("No aggregation output is available yet."))
    }
    
    tab_panels <- lapply(names(outputs), function(output_name) {
      
      safe_id <- gsub("[^A-Za-z0-9_]", "_", output_name)
      table_id <- paste0("aggregated_preview_", safe_id)
      download_id <- paste0("download_aggregated_", safe_id)
      
      is_warning <- isTRUE(attr(outputs[[output_name]], "is_warning_output"))
      
      if (is_warning) {
        warning_message <- outputs[[output_name]]$Message[1]
        
        return(
          nav_panel(
            title = paste0(output_name, " ⚠"),
            
            card(
              card_header(
                paste0("Aggregation output — measured element: ", output_name)
              ),
              div(
                class = "alert alert-warning",
                warning_message
              )
            )
          )
        )
      }
      
      local({
        output_name_local <- output_name
        table_id_local <- table_id
        download_id_local <- download_id
        
        output[[table_id_local]] <- renderDT({
          
          cfg <- get_dataset_config(
            input$dataset_id
          )
          
          dt_to_show <- order_aggregation_output_for_display(
            data = aggregated_outputs()[[
              output_name_local
            ]],
            cfg = cfg
          )
          
          dt_to_show <- format_dimension_codes_for_display(
            dt_to_show
          )
          
          datatable(
            dt_to_show,
            rownames = FALSE,
            filter = "top",
            options = list(
              pageLength = 10,
              scrollX = TRUE
            )
          )
        })
        
        output[[download_id_local]] <- downloadHandler(
          filename = function() {
            paste0(
              "aggregated_output_",
              output_name_local,
              ".csv"
            )
          },
          
          content = function(file) {
            dt_download <- remove_internal_aggregation_columns(
              aggregated_outputs()[[
                output_name_local
              ]]
            )
            
            req(!is.null(dt_download))
            req(nrow(dt_download) > 0)
            
            fwrite(
              dt_download,
              file
            )
          }
        )
        
        outputOptions(
          output,
          download_id_local,
          suspendWhenHidden = FALSE
        )
      })
      
      nav_panel(
        title = output_name,
        
        card(
          full_screen = TRUE,
          
          card_header(
            paste0(
              "Aggregation output — measured element: ",
              output_name
            )
          ),
          
          card_body(
            DTOutput(table_id)
          ),
          
          card_footer(
            downloadButton(
              download_id,
              paste0(
                "Download ",
                output_name,
                " CSV"
              )
            )
          )
        )
        
      )
    })
    
    do.call(
      navset_card_tab,
      tab_panels
    )
  })
  
  
  selected_graph_output_name <- reactive({
    outputs <- aggregated_outputs()
    
    if (length(outputs) == 0) {
      return(NULL)
    }
    
    selected <- input$graph_output_id
    
    if (is.null(selected) || !selected %in% names(outputs)) {
      selected <- names(outputs)[1]
    }
    
    selected
  })
  
  
  selected_graph_data <- reactive({
    outputs <- aggregated_outputs()
    
    if (length(outputs) == 0) {
      req(aggregated_data())
      return(aggregated_data())
    }
    
    selected <- selected_graph_output_name()
    
    if (is.null(selected) || !selected %in% names(outputs)) {
      return(data.table())
    }
    
    outputs[[selected]]
  })
  
  
  selected_graph_is_warning <- reactive({
    dt <- selected_graph_data()
    isTRUE(attr(dt, "is_warning_output"))
  })
  
  
  selected_graph_input_data <- reactive({
    req(input$dataset_id)
    
    dt <- aggregation_input_data()
    
    if (is.null(dt) || nrow(dt) == 0) {
      return(data.table())
    }
    
    dt <- copy(dt)
    cfg <- get_dataset_config(input$dataset_id)
    measured_col <- cfg$measured_element_col
    selected <- selected_graph_output_name()
    
    if (!is.null(selected) &&
        !identical(selected, "Aggregated output") &&
        !is.null(measured_col) &&
        measured_col %in% names(dt)) {
      
      dt <- dt[
        as.character(get(measured_col)) == as.character(selected)
      ]
    }
    
    saved_state <- last_comparison_state()
    
    if (
      !is.null(saved_state) &&
      isTRUE(saved_state$aggregate_selected_years) &&
      cfg$year_col %in% names(dt)
    ) {
      dt[
        ,
        (cfg$year_col) := as.character(
          saved_state$year_total_label %||%
            "Selected period"
        )
      ]
    }
    
    
    dt[]
  })
  
  
  draw_graph_warning <- function(message) {
    plot.new()
    text(
      0.5,
      0.5,
      message,
      cex = 0.9
    )
  }
  
  
  output$graph_output_selector <- renderUI({
    outputs <- aggregated_outputs()
    
    if (length(outputs) == 0) {
      return(helpText("No aggregation output is available yet."))
    }
    
    choices <- names(outputs)
    
    labels <- vapply(
      choices,
      function(x) {
        if (isTRUE(attr(outputs[[x]], "is_warning_output"))) {
          paste0(x, " — no data")
        } else {
          x
        }
      },
      character(1)
    )
    
    choices_named <- stats::setNames(choices, labels)
    
    if (length(choices) == 1) {
      return(
        tagList(
          strong("Measured element"),
          p(labels[1])
        )
      )
    }
    
    selectInput(
      "graph_output_id",
      "Measured element for graphs",
      choices = choices_named,
      selected = choices[1]
    )
  })
  
  
  output$aggregated_year_plot <- renderPlot({
    req(input$dataset_id)
    
    if (isTRUE(selected_graph_is_warning())) {
      dt_warning <- selected_graph_data()
      draw_graph_warning(dt_warning$Message[1])
      return(NULL)
    }
    
    dt <- selected_graph_data()
    
    if (nrow(dt) == 0) {
      plot.new()
      text(0.5, 0.5, "No aggregated yearly data available")
      return(NULL)
    }
    
    cfg <- get_dataset_config(input$dataset_id)
    
    yearly <- summarise_total_by_year(
      data = dt,
      year_col = cfg$year_col,
      value_col = cfg$value_col
    )
    
    if (nrow(yearly) == 0) {
      plot.new()
      text(0.5, 0.5, "No aggregated yearly data available")
      return(NULL)
    }
    
    selected_element <- selected_graph_output_name()
    
    yearly[
      ,
      year := factor(
        year,
        levels = year
      )
    ]
    
    ggplot(
      yearly,
      aes(
        x = year,
        y = total_value
      )
    ) +
      geom_col(width = 0.65) +
      scale_y_continuous(labels = scales::label_number()) +
      labs(
        x = "Year",
        y = "Total aggregated value",
        title = paste0(
          "Aggregated total time series — ",
          selected_element
        )
      ) +
      theme_minimal()
  })
  
  
  get_available_category_columns <- function(data, cfg, specs) {
    dt <- copy(data)
    
    candidate_cols <- c(
      cfg$geographical_area_col,
      cfg$species_col,
      cfg$fishing_area_col,
      cfg$measured_element_col,
      cfg$observation_flag_col,
      #cfg$method_flag_col,
      cfg$production_source_col,
      cfg$currency_flag_col
    )
    
    candidate_cols <- unique(unlist(candidate_cols, use.names = FALSE))
    candidate_cols <- candidate_cols[!is.na(candidate_cols) & nzchar(candidate_cols)]
    
    candidate_cols <- setdiff(
      candidate_cols,
      c(cfg$year_col, cfg$value_col)
    )
    
    # Do not show final-result columns for dimensions that were already aggregated.
    # Example: if species was aggregated, fisheriesAsfis should not appear as
    # "Final result column: fisheriesAsfis", because it now contains the computed
    # aggregate code, not the original ASFIS composition.
    aggregated_key_columns <- character(0)
    
    if (!is.null(specs) && length(specs) > 0) {
      aggregated_key_columns <- vapply(
        specs,
        function(spec) spec$key_dim_name,
        character(1)
      )
    }
    
    candidate_cols <- setdiff(candidate_cols, aggregated_key_columns)
    
    candidate_cols <- candidate_cols[candidate_cols %in% names(dt)]
    
    available_cols <- candidate_cols[
      vapply(
        candidate_cols,
        function(col) {
          values <- unique(as.character(dt[[col]]))
          values <- values[!is.na(values) & nzchar(values)]
          
          length(values) >= 2
        },
        logical(1)
      )
    ]
    
    available_cols
  }
  
  output$plot_category_selector <- renderUI({
    req(input$dataset_id)
    
    if (isTRUE(selected_graph_is_warning())) {
      return(helpText("No graph category is available because this measured element has no data."))
    }
    
    cfg <- get_dataset_config(input$dataset_id)
    specs <- last_aggregation_specs() %||% list()
    dt <- selected_graph_data()
    
    if (nrow(dt) == 0) {
      return(helpText("No graph category is available for the selected measured element."))
    }
    
    choices <- character(0)
    
    if (length(specs) > 0) {
      aggregation_choices <- stats::setNames(
        paste0("agg:", names(specs)),
        paste0(
          "Aggregation composition: ",
          vapply(specs, function(spec) spec$label, character(1))
        )
      )
      
      choices <- c(choices, aggregation_choices)
    }
    
    available_cols <- get_available_category_columns(
      data = dt,
      cfg = cfg,
      specs = specs
    )
    
    if (length(available_cols) > 0) {
      column_choices <- stats::setNames(
        paste0("col:", available_cols),
        paste0("Final result column: ", available_cols)
      )
      
      choices <- c(choices, column_choices)
    }
    
    if (length(choices) == 0) {
      return(
        helpText(
          "No categorization variable is available for the selected measured element."
        )
      )
    }
    
    selected_value <- input$plot_category
    
    if (is.null(selected_value) || !selected_value %in% unname(choices)) {
      selected_value <- unname(choices[1])
    }
    
    selectInput(
      "plot_category",
      "Categorize by",
      choices = choices,
      selected = selected_value
    )
  })
  
  get_plot_category_data <- function() {
    req(input$dataset_id)
    
    if (isTRUE(selected_graph_is_warning())) {
      return(data.table())
    }
    
    cfg <- get_dataset_config(input$dataset_id)
    
    selected_category <- input$plot_category
    
    if (is.null(selected_category) || !nzchar(selected_category)) {
      return(data.table())
    }
    
    
    if (startsWith(selected_category, "agg:")) {
      req(last_aggregation_specs())
      
      raw_dt <- selected_graph_input_data()
      
      if (nrow(raw_dt) == 0) {
        return(data.table())
      }
      
      dim_id <- sub("^agg:", "", selected_category)
      specs <- last_aggregation_specs()
      
      if (!dim_id %in% names(specs)) {
        return(data.table())
      }
      
      spec <- specs[[dim_id]]
      
      dt_for_composition <- filter_data_to_other_aggregation_context(
        data = raw_dt,
        aggregation_specs = specs,
        selected_dim_id = dim_id
      )
      
      codes <- get_codelist_codes(spec$codelist)
      tree_dt <- get_codelist_tree_cached(spec$codelist)
      
      composition <- summarise_composition_by_year(
        data = dt_for_composition,
        aggregation_spec = spec,
        codes = codes,
        tree_dt = tree_dt,
        year_col = cfg$year_col,
        value_col = cfg$value_col
      )
      
      if (nrow(composition) == 0) {
        return(data.table())
      }
      
      setnames(composition, "component_code", "category")
      
      return(
        composition[
          ,
          .(
            year,
            category,
            total_value
          )
        ]
      )
    }
    
    if (startsWith(selected_category, "col:")) {
      selected_col <- sub("^col:", "", selected_category)
      
      dt <- copy(selected_graph_data())
      
      if (!selected_col %in% names(dt)) {
        return(data.table())
      }
      
      if (!cfg$year_col %in% names(dt)) {
        return(data.table())
      }
      
      if (!cfg$value_col %in% names(dt)) {
        return(data.table())
      }
      
      dt[
        ,
        period_tmp :=
          normalise_period_label(
            get(cfg$year_col)
          )
      ]
      
      dt[, category := as.character(get(selected_col))]
      dt[is.na(category) | !nzchar(category), category := "<missing>"]
      
      plot_data <- dt[
        !is.na(period_tmp),
        .(
          total_value = sum_or_na(
            get(cfg$value_col)
          )
        ),
        by = .(
          year = period_tmp,
          category
        )
      ]
      
      plot_data[
        ,
        year_order :=
          period_start_value(year)
      ]
      
      setorder(
        plot_data,
        year_order,
        year,
        category
      )
      
      plot_data[, year_order := NULL]
      
      return(plot_data)
    }
    
    data.table()
  }
  
  
  
  output$treemap_year_selector <- renderUI({
    req(input$dataset_id)
    
    if (isTRUE(selected_graph_is_warning())) {
      return(NULL)
    }
    
    plot_data <- get_plot_category_data()
    
    if (nrow(plot_data) == 0 || !"year" %in% names(plot_data)) {
      return(NULL)
    }
    
    years <- unique(
      normalise_period_label(
        plot_data$year
      )
    )
    
    years <- years[!is.na(years)]
    
    years <- years[
      order(
        period_start_value(years),
        years
      )
    ]
    
    if (length(years) == 0) {
      return(NULL)
    }
    
    selectInput(
      "treemap_year",
      "Treemap year",
      choices = years,
      selected = tail(years, 1)
    )
  })
  
  output$treemap_plot <- renderPlot({
    req(input$dataset_id, input$treemap_year)
    
    if (isTRUE(selected_graph_is_warning())) {
      dt_warning <- selected_graph_data()
      draw_graph_warning(dt_warning$Message[1])
      return(NULL)
    }
    
    plot_data <- get_plot_category_data()
    
    if (nrow(plot_data) == 0) {
      plot.new()
      text(0.5, 0.5, "No treemap data available")
      return(NULL)
    }
    
    treemap_data <- plot_data[
      as.character(year) ==
        as.character(input$treemap_year),
      .(
        total_value = sum(total_value, na.rm = TRUE)
      ),
      by = category
    ]
    
    treemap_data <- treemap_data[!is.na(total_value) & total_value > 0]
    
    if (nrow(treemap_data) == 0) {
      plot.new()
      text(0.5, 0.5, "No treemap data available for selected year")
      return(NULL)
    }
    
    treemap_data[
      ,
      share := total_value / sum(total_value, na.rm = TRUE)
    ]
    
    treemap_data[
      ,
      label := paste0(
        category,
        "\n",
        round(100 * share, 1),
        "%"
      )
    ]
    
    selected_element <- selected_graph_output_name()
    
    ggplot(
      treemap_data,
      aes(
        area = total_value,
        fill = category,
        label = label
      )
    ) +
      geom_treemap(
        colour = "white",
        linewidth = 0.6
      ) +
      geom_treemap_text(
        colour = "black",
        place = "centre",
        grow = FALSE,
        reflow = TRUE,
        size=20,
        min.size = 3
      ) +
      labs(
        title = paste0(
          "Treemap composition in ",
          input$treemap_year,
          " — ",
          selected_element
        ),
        fill = "Category"
      ) +
      theme_minimal() +
      theme(
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        legend.position = "bottom"
      )
  })
  
  
  
  output$relative_composition_plot <- renderPlot({
    req(input$dataset_id)
    
    if (isTRUE(selected_graph_is_warning())) {
      dt_warning <- selected_graph_data()
      draw_graph_warning(dt_warning$Message[1])
      return(NULL)
    }
    
    plot_data <- get_plot_category_data()
    
    if (nrow(plot_data) == 0) {
      plot.new()
      text(0.5, 0.5, "No relative composition data available")
      return(NULL)
    }
    
    plot_data[
      ,
      year_total := sum(total_value, na.rm = TRUE),
      by = year
    ]
    
    plot_data[
      ,
      proportion := ifelse(year_total > 0, total_value / year_total, NA_real_)
    ]
    
    selected_element <- selected_graph_output_name()
    
    ggplot(
      plot_data,
      aes(
        x = factor(year),
        y = proportion,
        fill = category
      )
    ) +
      geom_col() +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(
        x = "Year",
        y = "Share",
        fill = "Category",
        title = paste0(
          "Relative composition by selected category — ",
          selected_element
        )
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
      )
  })
  
  
  output$component_time_series_plot <- renderPlot({
    req(input$dataset_id)
    
    if (isTRUE(selected_graph_is_warning())) {
      dt_warning <- selected_graph_data()
      draw_graph_warning(dt_warning$Message[1])
      return(NULL)
    }
    
    plot_data <- get_plot_category_data()
    
    if (nrow(plot_data) == 0) {
      plot.new()
      text(0.5, 0.5, "No component time-series data available")
      return(NULL)
    }
    
    selected_element <- selected_graph_output_name()
    
    ggplot(
      plot_data,
      aes(
        x = year,
        y = total_value,
        colour = category,
        group = category
      )
    ) +
      geom_line() +
      geom_point() +
      scale_y_continuous(labels = scales::label_number()) +
      labs(
        x = "Year",
        y = "Aggregated value",
        colour = "Category",
        title = paste0(
          "Component time series by selected category — ",
          selected_element
        )
      ) +
      theme_minimal()
  })
  
  
  get_outlier_group_columns <- function(data, cfg) {
    dt <- copy(data)
    
    candidate_cols <- c(
      cfg$geographical_area_col,
      cfg$species_col,
      cfg$fishing_area_col,
      cfg$measured_element_col,
      cfg$observation_flag_col,
      cfg$production_source_col,
      cfg$currency_flag_col
    )
    
    candidate_cols <- unique(unlist(candidate_cols, use.names = FALSE))
    candidate_cols <- candidate_cols[!is.na(candidate_cols) & nzchar(candidate_cols)]
    candidate_cols <- candidate_cols[candidate_cols %in% names(dt)]
    
    candidate_cols[
      vapply(
        candidate_cols,
        function(col) {
          values <- unique(as.character(dt[[col]]))
          values <- values[!is.na(values) & nzchar(values)]
          length(values) >= 1
        },
        logical(1)
      )
    ]
  }
  
  
  
  selected_outlier_output_name <- reactive({
    outputs <- aggregated_outputs()
    
    if (length(outputs) == 0) {
      return(NULL)
    }
    
    selected <- input$outlier_output_id
    
    if (is.null(selected) || !selected %in% names(outputs)) {
      selected <- names(outputs)[1]
    }
    
    selected
  })
  
  
  selected_outlier_data <- reactive({
    outputs <- aggregated_outputs()
    
    if (length(outputs) == 0) {
      req(aggregated_data())
      return(aggregated_data())
    }
    
    selected <- selected_outlier_output_name()
    
    if (is.null(selected) || !selected %in% names(outputs)) {
      return(data.table())
    }
    
    outputs[[selected]]
  })
  
  
  selected_outlier_is_warning <- reactive({
    dt <- selected_outlier_data()
    isTRUE(attr(dt, "is_warning_output"))
  })
  
  
  output$outlier_output_selector <- renderUI({
    outputs <- aggregated_outputs()
    
    if (length(outputs) == 0) {
      return(helpText("No aggregation output is available yet."))
    }
    
    choices <- names(outputs)
    
    labels <- vapply(
      choices,
      function(x) {
        if (isTRUE(attr(outputs[[x]], "is_warning_output"))) {
          paste0(x, " — no data")
        } else {
          x
        }
      },
      character(1)
    )
    
    choices_named <- stats::setNames(choices, labels)
    
    if (length(choices) == 1) {
      return(
        tagList(
          strong("Measured element"),
          p(labels[1])
        )
      )
    }
    
    selectInput(
      "outlier_output_id",
      "Measured element for outlier analysis",
      choices = choices_named,
      selected = choices[1]
    )
  })
  
  
  
  output$outlier_group_selector <- renderUI({
    req(input$dataset_id)
    
    dt <- selected_outlier_data()
    
    if (nrow(dt) == 0) {
      return(NULL)
    }
    
    if (isTRUE(selected_outlier_is_warning())) {
      return(NULL)
    }
    
    cfg <- get_dataset_config(input$dataset_id)
    
    group_cols <- get_outlier_group_columns(
      data = dt,
      cfg = cfg
    )
    
    choices <- c(
      "Full selected aggregation output" = "__all__",
      stats::setNames(group_cols, group_cols)
    )
    
    selectInput(
      "outlier_group_col",
      "Analyse outliers for",
      choices = choices,
      selected = "__all__"
    )
  })
  
  
  output$outlier_group_value_selector <- renderUI({
    req(input$dataset_id, input$outlier_group_col)
    
    if (identical(input$outlier_group_col, "__all__")) {
      return(NULL)
    }
    
    dt <- copy(selected_outlier_data())
    
    if (nrow(dt) == 0 || isTRUE(selected_outlier_is_warning())) {
      return(NULL)
    }
    
    group_col <- input$outlier_group_col
    
    if (!group_col %in% names(dt)) {
      return(NULL)
    }
    
    values <- sort(unique(as.character(dt[[group_col]])))
    values <- values[!is.na(values) & nzchar(values)]
    
    if (length(values) == 0) {
      return(NULL)
    }
    
    selectizeInput(
      "outlier_group_value",
      "Selected group value",
      choices = values,
      selected = values[1],
      options = list(
        placeholder = "Choose one group value",
        maxOptions = 5000
      )
    )
  })
  
  
  get_outlier_base_data <- reactive({
    req(input$dataset_id)
    
    dt <- copy(selected_outlier_data())
    
    if (nrow(dt) == 0) {
      return(data.table())
    }
    
    if (isTRUE(selected_outlier_is_warning())) {
      return(dt[])
    }
    
    if (!is.null(input$outlier_group_col) &&
        !identical(input$outlier_group_col, "__all__") &&
        !is.null(input$outlier_group_value) &&
        input$outlier_group_col %in% names(dt)) {
      
      dt <- dt[
        as.character(get(input$outlier_group_col)) ==
          as.character(input$outlier_group_value)
      ]
    }
    
    dt[]
  })
  
  output$outlier_metric_selector <- renderUI({
    req(input$dataset_id)
    
    dt <- selected_outlier_data()
    cfg <- get_dataset_config(input$dataset_id)
    
    choices <- c(
      "Value" = "value"
    )
    
    if (
      !is.null(dt) &&
      nrow(dt) > 0 &&
      cfg$year_col %in% names(dt) &&
      !isTRUE(attr(dt, "is_warning_output"))
    ) {
      periods <- unique(
        normalise_period_label(
          dt[[cfg$year_col]]
        )
      )
      
      periods <- periods[!is.na(periods)]
      
      if (length(periods) >= 2) {
        choices <- c(
          choices,
          "Year-on-year absolute change" = "yoy_abs",
          "Year-on-year percentage change" = "yoy_pct"
        )
      }
    }
    
    selected_metric <- input$outlier_metric
    
    if (
      is.null(selected_metric) ||
      !selected_metric %in% unname(choices)
    ) {
      selected_metric <- "value"
    }
    
    selectInput(
      "outlier_metric",
      "Outlier metric",
      choices = choices,
      selected = selected_metric
    )
  })
  
  
  prepare_outlier_data <- function(
    data,
    cfg,
    metric = "value"
  ) {
    dt <- copy(data)
    
    if (
      !cfg$value_col %in% names(dt) ||
      !cfg$year_col %in% names(dt)
    ) {
      return(data.table())
    }
    
    dt[
      ,
      value_tmp :=
        suppressWarnings(
          as.numeric(
            get(cfg$value_col)
          )
        )
    ]
    
    dt[
      ,
      period_tmp :=
        normalise_period_label(
          get(cfg$year_col)
        )
    ]
    
    dt[
      ,
      year_tmp :=
        period_start_value(
          period_tmp
        )
    ]
    
    dt <- dt[
      !is.na(value_tmp) &
        !is.na(period_tmp)
    ]
    
    if (nrow(dt) == 0) {
      return(data.table())
    }
    
    if (identical(metric, "value")) {
      dt[, outlier_metric_value := value_tmp]
      dt[, outlier_metric_label := "Value"]
      return(dt[])
    }
    
    if (uniqueN(dt$period_tmp) < 2) {
      return(data.table())
    }
    
    saved_state <- last_comparison_state()
    
    group_by_observation_flag <- (
      !is.null(saved_state) &&
        isTRUE(saved_state$group_by_observation_flag)
    )
    
    excluded_id_cols <- c(
      cfg$value_col,
      cfg$year_col,
      cfg$method_flag_col,
      "value_tmp",
      "period_tmp",
      "year_tmp"
    )
    
    # Use observation flag to define separate time series only when
    # the aggregation was explicitly performed separately by flag.
    if (!group_by_observation_flag) {
      excluded_id_cols <- c(
        excluded_id_cols,
        cfg$observation_flag_col
      )
    }
    
    id_cols <- setdiff(
      names(dt),
      excluded_id_cols
    )
    
    id_cols <- id_cols[
      id_cols %in% names(dt)
    ]
    
    if (length(id_cols) == 0) {
      dt[, outlier_group_id := "all"]
      id_cols <- "outlier_group_id"
    }
    
    setorderv(
      dt,
      c(
        id_cols,
        "year_tmp",
        "period_tmp"
      )
    )
    
    dt[
      ,
      previous_value := shift(value_tmp),
      by = id_cols
    ]
    
    if (identical(metric, "yoy_abs")) {
      dt[
        ,
        outlier_metric_value :=
          value_tmp - previous_value
      ]
      
      dt[
        ,
        outlier_metric_label :=
          "Year-on-year absolute change"
      ]
    }
    
    if (identical(metric, "yoy_pct")) {
      dt[
        ,
        outlier_metric_value := fifelse(
          !is.na(previous_value) &
            previous_value != 0,
          100 *
            (value_tmp - previous_value) /
            abs(previous_value),
          NA_real_
        )
      ]
      
      dt[
        ,
        outlier_metric_label :=
          "Year-on-year percentage change"
      ]
    }
    
    dt <- dt[
      !is.na(outlier_metric_value) &
        is.finite(outlier_metric_value)
    ]
    
    dt[]
  }
  
  
  get_top_outlier_data <- reactive({
    req(input$dataset_id)
    
    if (isTRUE(selected_outlier_is_warning())) {
      return(data.table())
    }
    
    cfg <- get_dataset_config(input$dataset_id)
    
    dt <- prepare_outlier_data(
      data = get_outlier_base_data(),
      cfg = cfg,
      metric = input$outlier_metric %||% "value"
    )
    
    if (nrow(dt) == 0) {
      return(data.table())
    }
    
    if (identical(input$outlier_metric, "value")) {
      dt <- dt[order(-outlier_metric_value)]
    } else {
      dt <- dt[order(-abs(outlier_metric_value))]
    }
    
    n_show <- input$outlier_top_n %||% 50
    
    dt <- head(dt, n_show)
    
    dt[]
  })
  
  get_outlier_scope_text <- reactive({
    element_name <- selected_outlier_output_name()
    
    if (is.null(element_name)) {
      element_text <- "the selected aggregation output"
    } else {
      element_text <- paste0("measured element ", element_name)
    }
    
    if (is.null(input$outlier_group_col) ||
        identical(input$outlier_group_col, "__all__")) {
      return(paste0("the full aggregation output for ", element_text))
    }
    
    if (is.null(input$outlier_group_value) ||
        !nzchar(input$outlier_group_value)) {
      return(
        paste0(
          "the selected group in ",
          input$outlier_group_col,
          " for ",
          element_text
        )
      )
    }
    
    paste0(
      input$outlier_group_col,
      " = ",
      input$outlier_group_value,
      " for ",
      element_text
    )
  })
  
  
  output$outlier_distribution_explanation <- renderUI({
    metric <- input$outlier_metric %||% "value"
    scope_text <- get_outlier_scope_text()
    
    if (identical(metric, "value")) {
      explanation <- tagList(
        tags$p(
          "Each point in the plot corresponds to one row of the aggregated output for ",
          scope_text,
          ". The y-axis shows the aggregated value itself."
        ),
        tags$p(
          "The boxplot summarizes how these values are distributed within each year. ",
          "Large values may simply correspond to large countries, species groups or fishing areas, ",
          "so this view should be interpreted as a broad screening tool."
        )
      )
    } else if (identical(metric, "yoy_abs")) {
      explanation <- tagList(
        tags$p(
          "Each point in the plot corresponds to the change from the previous year for the same remaining data combination in ",
          scope_text,
          "."
        ),
        tags$p(
          "The quantity is calculated as: current-year value minus previous-year value. ",
          "It is expressed in the same unit as the selected measured element, for example tonnes if the selected element is a quantity in tonnes."
        ),
        tags$p(
          "Positive values indicate increases compared with the previous year; negative values indicate decreases. ",
          "The first available year for each combination is not shown because there is no previous value for comparison."
        )
      )
    } else {
      explanation <- tagList(
        tags$p(
          "Each point in the plot corresponds to the percentage change from the previous year for the same remaining data combination in ",
          scope_text,
          "."
        ),
        tags$p(
          "The quantity is calculated as: 100 × (current-year value minus previous-year value) divided by the absolute value of the previous-year value."
        ),
        tags$p(
          "This helps identify unusually large relative changes. However, very large percentages can occur when the previous-year value was very small, so these records should be checked together with the original values."
        )
      )
    }
    
    tags$div(
      class = "alert alert-info",
      explanation
    )
  })
  
  
  output$outlier_table_explanation <- renderUI({
    metric <- input$outlier_metric %||% "value"
    scope_text <- get_outlier_scope_text()
    
    if (identical(metric, "value")) {
      explanation <- tagList(
        tags$p(
          "The table shows the largest aggregated records for ",
          scope_text,
          ". Records are ordered from the highest value to the lowest value."
        ),
        tags$p(
          "This is useful for identifying which combinations of year, country, species, fishing area, measured element and other remaining dimensions generate the largest values."
        )
      )
    } else if (identical(metric, "yoy_abs")) {
      explanation <- tagList(
        tags$p(
          "The table shows the strongest year-on-year absolute changes for ",
          scope_text,
          ". Records are ordered by the absolute size of the change, so both large increases and large decreases can appear at the top."
        ),
        tags$p(
          "The original Value column is the current-year value. The previous_value column is the value from the previous year used to calculate the change."
        )
      )
    } else {
      explanation <- tagList(
        tags$p(
          "The table shows the strongest year-on-year percentage changes for ",
          scope_text,
          ". Records are ordered by the absolute size of the percentage change, so both large increases and large decreases can appear at the top."
        ),
        tags$p(
          "The original Value column is the current-year value. The previous_value column is the previous-year value used in the denominator. Percentage changes based on very small previous values should be interpreted carefully."
        )
      )
    }
    
    tags$div(
      class = "alert alert-info",
      explanation
    )
  })
  
  
  output$outlier_boxplot <- renderPlot({
    req(input$dataset_id)
    
    if (isTRUE(selected_outlier_is_warning())) {
      dt_warning <- selected_outlier_data()
      plot.new()
      text(
        0.5,
        0.5,
        dt_warning$Message[1],
        cex = 0.9
      )
      return(NULL)
    }
    
    cfg <- get_dataset_config(input$dataset_id)
    
    dt <- prepare_outlier_data(
      data = get_outlier_base_data(),
      cfg = cfg,
      metric = input$outlier_metric %||% "value"
    )
    
    if (nrow(dt) == 0) {
      plot.new()
      text(0.5, 0.5, "No outlier data available for the selected settings")
      return(NULL)
    }
    
    metric_label <- unique(dt$outlier_metric_label)[1]
    
    selected_element <- selected_outlier_output_name()
    
    title_text <- if (identical(input$outlier_group_col, "__all__")) {
      paste0(
        "Distribution of ",
        metric_label,
        " by year — ",
        selected_element
      )
    } else {
      paste0(
        "Distribution of ", metric_label,
        " by year for ",
        input$outlier_group_col,
        " = ",
        input$outlier_group_value,
        " — ",
        selected_element
      )
    }
    
    ggplot(
      dt,
      aes(
        x = factor(
          period_tmp,
          levels = unique(period_tmp)
        ),
        y = outlier_metric_value
      )
    ) +
      geom_boxplot(outlier.alpha = 0.4) +
      geom_jitter(width = 0.15, alpha = 0.35, size = 1) +
      scale_y_continuous(labels = scales::label_number()) +
      labs(
        x = "Year",
        y = metric_label,
        title = title_text
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
      )
  })
  
  
  output$largest_values_table <- renderDT({
    req(input$dataset_id)
    
    if (isTRUE(selected_outlier_is_warning())) {
      dt_warning <- selected_outlier_data()
      
      return(
        datatable(
          data.table(message = dt_warning$Message[1]),
          rownames = FALSE,
          options = list(dom = "t")
        )
      )
    }
    
    dt <- get_top_outlier_data()
    
    if (nrow(dt) == 0) {
      return(
        datatable(
          data.table(message = "No outlier records available for the selected settings."),
          rownames = FALSE
        )
      )
    }
    
    display_cols <- setdiff(
      names(dt),
      c(
        "value_tmp",
        "period_tmp",
        "year_tmp",
        "outlier_group_id"
      )
    )
    
    n_show <- input$outlier_top_n %||% 50
    
    datatable(
      dt[, ..display_cols],
      rownames = FALSE,
      options = list(
        pageLength = n_show,
        lengthChange = FALSE,
        scrollX = TRUE,
        scrollY = "520px"
      )
    )
  })
  
  output$download_outlier_table <- downloadHandler(
    
    filename = function() {
      element_name <- selected_outlier_output_name() %||% "outliers"
      metric_name <- input$outlier_metric %||% "value"
      
      element_name <- gsub(
        "[^A-Za-z0-9_-]+",
        "_",
        element_name
      )
      
      paste0(
        "outliers_",
        element_name,
        "_",
        metric_name,
        ".csv"
      )
    },
    
    content = function(file) {
      dt <- get_top_outlier_data()
      
      req(nrow(dt) > 0)
      
      download_cols <- setdiff(
        names(dt),
        c(
          "value_tmp",
          "period_tmp",
          "year_tmp",
          "outlier_group_id"
        )
      )
      
      fwrite(
        dt[, ..download_cols],
        file
      )
    }
  )
  
  outputOptions(
    output,
    "download_outlier_table",
    suspendWhenHidden = FALSE
  )
  
  
  
  output$composition_plot <- renderPlot({
    req(input$dataset_id)
    
    if (isTRUE(selected_graph_is_warning())) {
      dt_warning <- selected_graph_data()
      draw_graph_warning(dt_warning$Message[1])
      return(NULL)
    }
    
    plot_data <- get_plot_category_data()
    
    if (nrow(plot_data) == 0) {
      plot.new()
      text(0.5, 0.5, "No composition data available")
      return(NULL)
    }
    
    selected_element <- selected_graph_output_name()
    
    ggplot(
      plot_data,
      aes(
        x = factor(year),
        y = total_value,
        fill = category
      )
    ) +
      geom_col() +
      scale_y_continuous(labels = scales::label_number()) +
      labs(
        x = "Year",
        y = "Aggregated value",
        fill = "Category",
        title = paste0(
          "Composition by selected category — ",
          selected_element
        )
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
      )
  })
  
  output$download_csv <- downloadHandler(
    filename = function() {
      selected_dims <- names(Filter(
        isTRUE,
        list(
          species = input$apply_species,
          geographical_area = input$apply_geographical_area,
          fishing_area = input$apply_fishing_area,
          production_source = input$apply_production_source,
          observation_status = input$apply_observation_flag
        )
      ))
      
      if (length(selected_dims) == 0) {
        selected_dims <- "no_aggregation"
      } else {
        selected_dims <- paste(selected_dims, collapse = "_")
      }
      
      paste0(
        "aggregated_",
        input$dataset_id %||% "dataset",
        "_",
        selected_dims,
        ".csv"
      )
    },
    content = function(file) {
      req(aggregated_data())
      fwrite(aggregated_data(), file)
    }
  )
}

shinyApp(ui, server)
