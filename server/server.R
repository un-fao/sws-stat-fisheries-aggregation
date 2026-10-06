##########################################
# Server
##########################################
server <- function(input, output, session) {
  observeEvent(input$fisheries_diagnostic, {
    # Restrict browser diagnostics to known fields; never log bodies or query tokens.
    event <- input$fisheries_diagnostic
    fields <- intersect(names(event), c("kind", "table", "path", "status", "content_type", "directive"))
    details <- vapply(event[fields], function(value) {
      substr(gsub("[\r\n]", " ", as.character(value)[1L]), 1L, 500L)
    }, character(1))
    message("[Fisheries diagnostics] ", paste(paste(fields, details, sep = "="), collapse = " "))
  }, ignoreInit = TRUE)
  user <- reactiveVal(NULL)
  dataset_data <- reactiveVal(NULL)
  loaded_dataset_id <- reactiveVal(NULL)
  
  # Actual source used to load the main data:
  # current, disseminated or tagged.
  loaded_dataset_source <- reactiveVal(NULL)
  
  # Only populated when the main source is tagged.
  loaded_tag_id <- reactiveVal(NULL)
  loaded_tag_label <- reactiveVal(NULL)
  
  dataset_loading <- reactiveVal(FALSE)
  dataset_config_ready <- reactiveVal(FALSE)
  comparison_dataset_loading <- reactiveVal(FALSE)
  
  # Stores the result returned by getDatasetInfo()
  loaded_dataset_info <- reactiveVal(NULL)
  comparison_dataset_info <- reactiveVal(NULL)
  
  
  #aggregated_data <- reactiveVal(NULL)     # combined aggregated output
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
  
  # codelist_cache <- reactiveValues()
  # codelist_tree_cache <- reactiveValues()
  
  
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
  
  
  
  # Resolve configured measured-element roots to the measured elements
  # that should actually be read from the dataset.
  get_effective_measured_elements <- function(
    dataset_info,
    data,
    measured_col
  ) {
    
    dt <- as.data.table(data)
    
    configured_roots <- get_dataset_dimension_roots(
      dataset_info = dataset_info,
      dimension_id = measured_col
    )
    
    configured_roots <- clean_code_vector(
      configured_roots
    )
    
    if (length(configured_roots) == 0L) {
      return(character(0))
    }
    
    codes <- as.data.table(
      get_codelist_codes(
        "measuredElement"
      )
    )
    
    codes[, id := as.character(id)]
    
    effective_elements <- unique(
      unlist(
        lapply(
          configured_roots,
          function(root_i) {
            
            children_i <- get_direct_children(
              codes = codes,
              parent_code = root_i
            )
            
            if (length(children_i) > 0L) {
              children_i
            } else {
              root_i
            }
          }
        ),
        use.names = FALSE
      )
    )
    
    effective_elements <- clean_code_vector(
      effective_elements
    )
    
    # Keep only measured elements actually present in the dataset.
    intersect(
      effective_elements,
      clean_code_vector(
        dt[[measured_col]]
      )
    )
  }
  
  
  restrict_to_effective_measured_elements <- function(
    data,
    dataset_info,
    measured_col
  ) {
    
    dt <- as.data.table(data)
    
    if (
      is.null(measured_col) ||
      !measured_col %in% names(dt)
    ) {
      return(dt)
    }
    
    effective_measured_elements <-
      get_effective_measured_elements(
        dataset_info = dataset_info,
        data = dt,
        measured_col = measured_col
      )
    
    if (length(effective_measured_elements) == 0L) {
      return(dt[0])
    }
    
    dt[
      as.character(
        get(measured_col)
      ) %in%
        effective_measured_elements
    ]
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
  
  # Retrieve and cache codelist codes once per day on the shared drive.
  get_regular_codelist_codes <- function(
    codelist_id
  ) {
    
    cache_id <- paste0(
      "regular_codes__",
      codelist_id
    )
    
    cached <- get_daily_shared_cache(
      cache_id
    )
    
    if (is_valid_codelist_table(cached)) {
      return(
        cached
      )
    }
    
    loading_notification <- showNotification(
      paste0(
        "Loading codelist ",
        codelist_id,
        ". Please wait..."
      ),
      type = "default",
      duration = NULL,
      closeButton = FALSE
    )
    
    on.exit(
      removeNotification(
        loading_notification
      ),
      add = TRUE
    )
    
    codes <- fetch_codelist_table(codelist_id)
    
    codes[
      ,
      id := as.character(id)
    ]
    
    if (
      !codelist_id %in%
      KEEP_EXPIRED_CODELISTS
    ) {
      
      codes <- drop_expired_codelist_codes(
        codes
      )
    }
    
    set_daily_shared_cache(
      cache_id = cache_id,
      value = codes
    )
    
    codes
  }
  
  # Retrieve and cache a codelist hierarchy once per day on the shared drive.
  get_regular_codelist_tree_cached <- function(
    codelist_id
  ) {
    
    cache_id <- paste0(
      "regular_tree__",
      codelist_id
    )
    
    cached <- get_daily_shared_cache(
      cache_id
    )
    
    if (is_valid_codelist_table(cached, tree = TRUE)) {
      return(
        cached
      )
    }
    
    loading_notification <- showNotification(
      paste0(
        "Loading codelist hierarchy ",
        codelist_id,
        ". Please wait..."
      ),
      type = "default",
      duration = NULL,
      closeButton = FALSE
    )
    
    on.exit(
      removeNotification(
        loading_notification
      ),
      add = TRUE
    )
    
    tree_dt <- fetch_codelist_table(codelist_id, tree = TRUE)
    
    if (
      !codelist_id %in%
      KEEP_EXPIRED_CODELISTS
    ) {
      
      active_codes <- get_regular_codelist_codes(
        codelist_id
      )
      
      tree_dt <- drop_expired_codes_from_codelist_tree(
        tree_dt = tree_dt,
        active_codes = active_codes
      )
    }
    
    set_daily_shared_cache(
      cache_id = cache_id,
      value = tree_dt
    )
    
    tree_dt
  }
  
  
  
  # Retrieve the codelist codes used by the app,
  # adding synthetic hierarchy codes when required.
  get_codelist_codes <- function(
    codelist_id
  ) {
    
    if (
      uses_expired_roots_branch(
        codelist_id
      )
    ) {
      
      cache_id <- paste0(
        "augmented_codes__current_plus_expired__",
        codelist_id
      )
      
      cached <- get_daily_shared_cache(
        cache_id
      )
      
      if (is_valid_codelist_table(cached)) {
        return(
          cached
        )
      }
      
      original_codes <- copy(
        get_regular_codelist_codes(
          codelist_id
        )
      )
      
      out <- add_synthetic_expired_root_code(
        original_codes
      )
      
      set_daily_shared_cache(
        cache_id = cache_id,
        value = out
      )
      
      return(
        out
      )
    }
    
    get_regular_codelist_codes(
      codelist_id
    )
  }
  
  # Retrieve the hierarchy used by the app,
  # adding synthetic branches when required.
  get_codelist_tree_cached <- function(
    codelist_id
  ) {
    
    if (
      uses_expired_roots_branch(
        codelist_id
      )
    ) {
      
      cache_id <- paste0(
        "augmented_tree__current_plus_expired_v2__",
        codelist_id
      )
      
      cached <- get_daily_shared_cache(
        cache_id
      )
      
      if (is_valid_codelist_table(cached, tree = TRUE)) {
        return(
          cached
        )
      }
      
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
      
      out <- add_expired_roots_branch_to_tree(
        tree_dt = original_tree,
        codes = original_codes
      )
      
      set_daily_shared_cache(
        cache_id = cache_id,
        value = out
      )
      
      return(
        out
      )
    }
    
    get_regular_codelist_tree_cached(
      codelist_id
    )
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
            id != SYNTHETIC_EXPIRED_ROOTS_ID,
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
        uses_expired_roots_branch(
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
  
  output$primary_tag_selector <- renderUI({
    
    if (!identical(
      input$dataset_group,
      "tagged"
    )) {
      return(NULL)
    }
    
    req(input$dataset_id)
    
    choices <- get_tagged_dataset_choices(
      input$dataset_id
    )
    
    if (length(choices) == 0L) {
      return(
        helpText(
          "No tags were found for the selected dataset."
        )
      )
    }
    
    selectizeInput(
      "primary_tag_id",
      "Tagged dataset",
      choices = choices,
      selected = character(0),
      options = list(
        placeholder = "Choose a tagged dataset",
        maxOptions = 5000
      )
    )
  })
  
  
  
  
  output$comparison_dataset_selector <- renderUI({
    req(input$comparison_source)
    
    if (identical(
      input$comparison_source,
      "current"
    )) {
      
      choices <- get_dataset_choices(
        "current"
      )
      
      if (length(choices) == 0L) {
        return(
          helpText(
            "No current Fisheries datasets are available."
          )
        )
      }
      
      return(
        selectizeInput(
          "comparison_dataset_id",
          "Comparison current dataset",
          choices = choices,
          selected = character(0),
          options = list(
            placeholder = "Choose a current Fisheries dataset",
            maxOptions = 5000
          )
        )
      )
    }
    
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
        "The selected comparison dataset may not correspond to the main dataset. ",
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
          "The main and comparison datasets do not contain exactly ",
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
          sws_endpoint = SWS_ENDPOINT
          #sws_endpoint = "https://sws.qa.fao.org"
        )
        
        user(getCurrentUser())
        
        DATASET_CONFIG <<-
          build_dataset_config_from_sws()
        
        dataset_config_ready(TRUE)
        
        updateSelectizeInput(
          session,
          "dataset_id",
          choices = get_primary_dataset_choices(
            input$dataset_group %||% "current"
          ),
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
          "is a current Fisheries dataset or a tagged version of one."
        ),
        type = "error",
        duration = 10
      )
      
      return(NULL)
    }
    
    if (isTRUE(comparison_dataset_loading())) {
      showNotification(
        "The comparison dataset is already loading. Please wait.",
        type = "warning",
        duration = 3
      )
      
      return(NULL)
    }
    
    comparison_dataset_loading(TRUE)
    
    loading_notification <- showNotification(
      "Loading comparison dataset. Please wait...",
      type = "default",
      duration = NULL,
      closeButton = FALSE
    )
    
    on.exit(
      {
        comparison_dataset_loading(FALSE)
        
        removeNotification(
          loading_notification
        )
      },
      add = TRUE
    )
    
    req(input$comparison_source)
    
    # Remove the previous comparison immediately.
    comparison_data(NULL)
    comparison_dataset_info(NULL)
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
        
        # ------------------------------------------------------------
        # Load the selected comparison source.
        # Source-specific code should only retrieve the data,
        # metadata and source label.
        # All restrictions are applied once afterwards.
        # ------------------------------------------------------------
        
        if (identical(
          input$comparison_source,
          "current"
        )) {
          
          req(input$comparison_dataset_id)
          
          comparison_info <- getDatasetInfo(
            input$comparison_dataset_id
          )
          
          dt <- readDataset(
            dataset_id =
              input$comparison_dataset_id
          )
          
          
          comparison_metadata(
            list(
              source = "current",
              id = input$comparison_dataset_id,
              label = input$comparison_dataset_id,
              base_dataset =
                input$comparison_dataset_id
            )
          )
        }
        
        
        if (identical(
          input$comparison_source,
          "disseminated"
        )) {
          
          req(input$comparison_dataset_id)
          
          comparison_info <- getDatasetInfo(
            input$comparison_dataset_id
          )
          
          dt <- as.data.table(
            readDataset(
              dataset_id =
                input$comparison_dataset_id
            )
          )
          
          comparison_metadata(
            list(
              source = "disseminated",
              id = input$comparison_dataset_id,
              label = input$comparison_dataset_id,
              base_dataset =
                input$comparison_dataset_id
            )
          )
        }
        
        
        if (identical(
          input$comparison_source,
          "tagged"
        )) {
          
          req(
            input$comparison_base_dataset_id,
            input$comparison_tag_id
          )
          
          comparison_info <- getDatasetInfo(
            input$comparison_base_dataset_id
          )
          
          dt <- as.data.table(
            getTagData(
              as.character(
                input$comparison_tag_id
              )
            )
          )
          
          tag_info <- as.data.table(
            getAllTags(
              dataset =
                input$comparison_base_dataset_id
            )
          )
          
          tag_info[
            ,
            id := as.character(id)
          ]
          
          tag_row <- tag_info[
            id ==
              as.character(
                input$comparison_tag_id
              )
          ]
          
          comparison_metadata(
            list(
              source = "tagged",
              id =
                as.character(
                  input$comparison_tag_id
                ),
              label =
                if (nrow(tag_row) > 0L) {
                  as.character(
                    tag_row$name[1L]
                  )
                } else {
                  as.character(
                    input$comparison_tag_id
                  )
                },
              base_dataset =
                input$comparison_base_dataset_id
            )
          )
        }
        
        
        # ------------------------------------------------------------
        # Common processing for EVERY comparison dataset.
        # From this point on, source no longer matters.
        # ------------------------------------------------------------
        
        dt <- normalise_and_drop_empty_values(
          data = dt,
          value_col = current_cfg$value_col
        )
        
        dt <- restrict_to_active_configured_codes(
          data = dt,
          dataset_info = comparison_info
        )
        
        dt <- restrict_to_effective_measured_elements(
          data = dt,
          dataset_info = comparison_info,
          measured_col =
            current_cfg$measured_element_col
        )
        
        comparison_dataset_info(
          comparison_info
        )
        
        comparison_data(dt)
        
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
        comparison_dataset_info(NULL)
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
  
  # Print the mismatches of codes detected during the comparison
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
      
      warning_parts <- character(0)
      
      if (length(only_comparison) > 0) {
        
        warning_parts <- c(
          warning_parts,
          paste0(
            length(only_comparison),
            " filtered code(s) occur only in the ",
            comparison_side_label,
            "; ",
            length(only_comparison_with_values),
            " of them have at least one non-missing value.",
            format_code_sample(only_comparison)
          )
        )
      }
      
      if (length(only_current) > 0) {
        
        warning_parts <- c(
          warning_parts,
          paste0(
            length(only_current),
            " filtered code(s) occur only in the current dataset; ",
            length(only_current_with_values),
            " of them have at least one non-missing value.",
            format_code_sample(only_current)
          )
        )
      }
      
      warnings <- c(
        warnings,
        paste0(
          "Measured element ",
          measured_element_id,
          " — ",
          spec$label,
          ": ",
          paste(
            warning_parts,
            collapse = " "
          )
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
          "is not a current Fisheries dataset or a tagged version of one."
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
              detail = "Checking main dataset and comparison dataset."
            )
            
            if (is.null(dataset_data()) ||
                is.null(input$dataset_id) ||
                !nzchar(input$dataset_id)) {
              
              showNotification(
                "Please load a compatible main dataset before running the comparison.",
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
              detail = "Checking main aggregation outputs."
            )
            
            current_outputs <- aggregated_outputs()
            
            if (length(current_outputs) == 0) {
              showNotification(
                "Please run the main dataset aggregation first. The comparison uses those aggregation outputs.",
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
                "Comparison stopped because the selected comparison dataset may not correspond to the main dataset. Review the warning and click 'Run comparison anyway' only if this is intentional.",
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
                  "Please rerun the main dataset aggregation."
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
            
            incProgress(
              amount = 0.10,
              detail = "Applying main dataset filters to the comparison dataset."
            )
            
            # Apply the same filters to both datasets, but ignore
            # observation flags because old and current flag systems differ.
            comparison_filtered <- apply_saved_filters_to_comparison_data(
              data = comparison_raw,
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
                "No main dataset rows remain after applying the saved non-flag filters."
              )
            }
            
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
                current_i_raw <- current_filtered
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
                comparison_i_raw <- comparison_filtered
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
            
            if (
              "percentage_difference" %in%
              names(dt_to_show)
            ) {
              
              dt_to_show[
                ,
                percentage_difference :=
                  ifelse(
                    is.na(percentage_difference),
                    NA_character_,
                    paste0(
                      formatC(
                        percentage_difference,
                        format = "f",
                        digits = 2,
                        big.mark = " ",
                        decimal.mark = "."
                      ),
                      "%"
                    )
                  )
              ]
            }
            
            comparison_table <- datatable(
              dt_to_show,
              rownames = FALSE,
              filter = "top",
              options = list(
                pageLength = 10,
                scrollX = TRUE
              )
            )
            
            value_columns <- intersect(
              c(
                "current_value",
                "comparison_value",
                "absolute_difference"
              ),
              names(dt_to_show)
            )
            
            if (length(value_columns) > 0L) {
              
              comparison_table <- formatRound(
                comparison_table,
                columns = value_columns,
                digits = VALUE_DECIMAL_DIGITS,
                mark = " ",
                dec.mark = "."
              )
            }
            
            comparison_table
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
  
  # -------------------------------------------------------------------------
  # Comparison charts
  # -------------------------------------------------------------------------
  
  
  # -------------------------------------------------------------------------
  # Available measured-element outputs
  # -------------------------------------------------------------------------
  
  comparison_plot_valid_outputs <- reactive({
    
    results <- comparison_results()
    
    if (
      is.null(results) ||
      length(results) == 0L
    ) {
      return(character(0))
    }
    
    names(results)[
      !vapply(
        results,
        function(x) {
          isTRUE(
            attr(
              x,
              "is_warning_output"
            )
          )
        },
        logical(1)
      )
    ]
  })
  
  
  # -------------------------------------------------------------------------
  # Measured-element selectors
  # -------------------------------------------------------------------------
  
  output$comparison_plot_output_selector <- renderUI({
    
    valid_outputs <- comparison_plot_valid_outputs()
    
    if (length(valid_outputs) == 0L) {
      return(
        helpText(
          "Run the comparison first to display a chart."
        )
      )
    }
    
    selectInput(
      "comparison_plot_output_id",
      "Measured element",
      choices = valid_outputs,
      selected = valid_outputs[1L]
    )
  })
  
  
  output$comparison_difference_plot_output_selector <- renderUI({
    
    valid_outputs <- comparison_plot_valid_outputs()
    
    if (length(valid_outputs) == 0L) {
      return(
        helpText(
          "Run the comparison first to display a chart."
        )
      )
    }
    
    selectInput(
      "comparison_difference_plot_output_id",
      "Measured element",
      choices = valid_outputs,
      selected = valid_outputs[1L]
    )
  })
  
  
  # -------------------------------------------------------------------------
  # Retrieve one comparison output
  # -------------------------------------------------------------------------
  
  get_comparison_plot_data <- function(
    output_id,
    differences_only = FALSE
  ) {
    
    results <- comparison_results()
    
    req(
      !is.null(results),
      length(results) > 0L,
      !is.null(output_id),
      output_id %in% names(results)
    )
    
    dt <-results[[output_id]]
    
    
    req(
      !isTRUE(
        attr(
          dt,
          "is_warning_output"
        )
      )
    )
    
    if (isTRUE(differences_only)) {
      dt <- comparison_difference_rows(
        dt
      )
    }
    
    dt[]
  }
  
  
  selected_comparison_plot_data <- reactive({
    
    req(
      input$comparison_plot_output_id
    )
    
    get_comparison_plot_data(
      output_id =
        input$comparison_plot_output_id,
      differences_only = FALSE
    )
  })
  
  
  selected_comparison_difference_plot_data <- reactive({
    
    req(
      input$comparison_difference_plot_output_id
    )
    
    get_comparison_plot_data(
      output_id =
        input$comparison_difference_plot_output_id,
      differences_only = TRUE
    )
  })
  
  
  # -------------------------------------------------------------------------
  # Analytical dimensions available in one comparison result
  # -------------------------------------------------------------------------
  
  get_comparison_plot_dimensions <- function(
    dt
  ) {
    
    cfg <- get_dataset_config(
      input$dataset_id
    )
    
    dimension_columns <- unique(
      na.omit(
        c(
          cfg$geographical_area_col,
          cfg$species_col,
          cfg$fishing_area_col,
          cfg$production_source_col,
          cfg$currency_flag_col
        )
      )
    )
    
    dimension_columns <- intersect(
      dimension_columns,
      names(dt)
    )
    
    # Show a selector only when the dimension actually varies.
    dimension_columns[
      vapply(
        dimension_columns,
        function(column_i) {
          
          values_i <- unique(
            as.character(
              dt[[column_i]]
            )
          )
          
          values_i <- values_i[
            !is.na(values_i) &
              nzchar(values_i)
          ]
          
          length(values_i) > 1L
        },
        logical(1)
      )
    ]
  }
  
  
  comparison_dimension_label <- function(
    column_name
  ) {
    
    cfg <- get_dataset_config(
      input$dataset_id
    )
    
    if (identical(
      column_name,
      cfg$geographical_area_col
    )) {
      return(
        "Country / geographical area"
      )
    }
    
    if (identical(
      column_name,
      cfg$species_col
    )) {
      return(
        "Species / ASFIS"
      )
    }
    
    if (identical(
      column_name,
      cfg$fishing_area_col
    )) {
      return(
        "Fishing area"
      )
    }
    
    if (identical(
      column_name,
      cfg$production_source_col
    )) {
      return(
        "Production source / environment type"
      )
    }
    
    if (identical(
      column_name,
      cfg$currency_flag_col
    )) {
      return(
        "Currency flag"
      )
    }
    
    column_name
  }
  
  
  # -------------------------------------------------------------------------
  # Build choices for one dimension
  # -------------------------------------------------------------------------
  
  get_comparison_dimension_choices <- function(
    dt,
    column_name
  ) {
    
    raw_values <- unique(
      as.character(
        dt[[column_name]]
      )
    )
    
    raw_values <- raw_values[
      !is.na(raw_values) &
        nzchar(raw_values)
    ]
    
    if (length(raw_values) == 0L) {
      return(character(0))
    }
    
    display_dt <- format_dimension_codes_for_display(
      dt
    )
    
    lookup <- unique(
      data.table(
        raw = as.character(
          dt[[column_name]]
        ),
        display = as.character(
          display_dt[[column_name]]
        )
      )
    )
    
    lookup <- lookup[
      !is.na(raw) &
        nzchar(raw)
    ]
    
    lookup[
      is.na(display) |
        !nzchar(display),
      display := raw
    ]
    
    lookup <- lookup[
      raw %in% raw_values
    ]
    
    stats::setNames(
      lookup$raw,
      lookup$display
    )
  }
  
  
  # -------------------------------------------------------------------------
  # Cascading dimension selectors
  # -------------------------------------------------------------------------
  
  build_comparison_dimension_selectors <- function(
    dt,
    input_prefix
  ) {
    
    dimension_columns <- get_comparison_plot_dimensions(
      dt
    )
    
    if (length(dimension_columns) == 0L) {
      return(NULL)
    }
    
    controls <- list()
    
    # This object becomes progressively smaller as each
    # selected dimension is applied.
    available_dt <- dt
    
    for (column_i in dimension_columns) {
      
      if (nrow(available_dt) == 0L) {
        break
      }
      
      choices_i <- get_comparison_dimension_choices(
        dt = available_dt,
        column_name = column_i
      )
      
      if (length(choices_i) == 0L) {
        next
      }
      
      input_id <- paste0(
        input_prefix,
        gsub(
          "[^A-Za-z0-9_]",
          "_",
          column_i
        )
      )
      
      selected_value <- input[[input_id]]
      
      valid_values <- unname(
        choices_i
      )
      
      # Keep the current selection only if it is still possible
      # given the dimensions selected before it.
      if (
        is.null(selected_value) ||
        length(selected_value) == 0L ||
        !as.character(selected_value[1L]) %in% valid_values
      ) {
        selected_value <- valid_values[1L]
      } else {
        selected_value <- as.character(
          selected_value[1L]
        )
      }
      
      controls[[length(controls) + 1L]] <-
        selectizeInput(
          inputId = input_id,
          label = comparison_dimension_label(
            column_i
          ),
          choices = choices_i,
          selected = selected_value,
          options = list(
            maxOptions = 5000
          )
        )
      
      # Restrict the data before constructing the next selector.
      # Therefore every later selector contains only combinations
      # that actually exist.
      available_dt <- available_dt[
        as.character(
          get(column_i)
        ) == selected_value
      ]
    }
    
    tagList(
      controls
    )
  }
  
  
  # -------------------------------------------------------------------------
  # Dimension selectors — all comparison rows
  # -------------------------------------------------------------------------
  
  output$comparison_plot_dimension_selectors <- renderUI({
    
    dt <- selected_comparison_plot_data()
    
    build_comparison_dimension_selectors(
      dt = dt,
      input_prefix =
        "comparison_plot_dimension_"
    )
  })
  
  
  # -------------------------------------------------------------------------
  # Dimension selectors — differences only
  # -------------------------------------------------------------------------
  
  output$comparison_difference_plot_dimension_selectors <- renderUI({
    
    dt <- selected_comparison_difference_plot_data()
    
    if (nrow(dt) == 0L) {
      return(
        helpText(
          "No differences are available for this measured element."
        )
      )
    }
    
    build_comparison_dimension_selectors(
      dt = dt,
      input_prefix =
        "comparison_difference_plot_dimension_"
    )
  })
  
  
  # -------------------------------------------------------------------------
  # Apply the cascading dimension selections
  # -------------------------------------------------------------------------
  
  filter_comparison_plot_dimensions <- function(
    dt,
    input_prefix
  ) {
    
    dimension_columns <- get_comparison_plot_dimensions(
      dt
    )
    
    if (length(dimension_columns) == 0L) {
      return(dt[])
    }
    
    for (column_i in dimension_columns) {
      
      if (nrow(dt) == 0L) {
        break
      }
      
      available_values <- unique(
        as.character(
          dt[[column_i]]
        )
      )
      
      available_values <- available_values[
        !is.na(available_values) &
          nzchar(available_values)
      ]
      
      if (length(available_values) == 0L) {
        next
      }
      
      input_id <- paste0(
        input_prefix,
        gsub(
          "[^A-Za-z0-9_]",
          "_",
          column_i
        )
      )
      
      selected_value <- input[[
        input_id
      ]]
      
      # If an old selector value is temporarily still present after
      # an earlier selector changed, use the first valid value instead
      # of allowing the result to become empty.
      if (
        is.null(selected_value) ||
        length(selected_value) == 0L ||
        !as.character(selected_value[1L]) %in%
        available_values
      ) {
        selected_value <- available_values[1L]
      } else {
        selected_value <- as.character(
          selected_value[1L]
        )
      }
      
      dt <- dt[
        as.character(
          get(column_i)
        ) == selected_value
      ]
    }
    
    dt[]
  }
  
  
  comparison_plot_filtered_data <- reactive({
    
    dt <- selected_comparison_plot_data()
    
    
    filter_comparison_plot_dimensions(
      dt = dt,
      input_prefix =
        "comparison_plot_dimension_"
    )
  })
  
  
  comparison_difference_plot_filtered_data <- reactive({
    
    dt <- selected_comparison_difference_plot_data()
    
    
    filter_comparison_plot_dimensions(
      dt = dt,
      input_prefix =
        "comparison_difference_plot_dimension_"
    )
  })
  
  
  # -------------------------------------------------------------------------
  # Shared plot builder
  # -------------------------------------------------------------------------
  
  build_comparison_time_series_plot <- function(
    dt,
    measured_element_id,
    differences_only = FALSE
  ) {
    
    cfg <- get_dataset_config(
      input$dataset_id
    )
    
    year_col <- cfg$year_col
    
    validate(
      need(
        nrow(dt) > 0L,
        if (isTRUE(differences_only)) {
          "No differences are available for the selected dimensions."
        } else {
          "No comparison rows are available for the selected dimensions."
        }
      ),
      
      need(
        !is.null(year_col) &&
          year_col %in% names(dt),
        "The comparison output does not contain a time dimension."
      )
    )
    
    
    # ------------------------------------------------------------
    # Build descriptive title from selected dimensions
    # ------------------------------------------------------------
    
    display_dt <- format_dimension_codes_for_display(
      dt
    )
    
    title_dimensions <- list(
      
      "Country / geographical area" =
        cfg$geographical_area_col,
      
      "Species / ASFIS" =
        cfg$species_col,
      
      "Fishing area" =
        cfg$fishing_area_col,
      
      "Production source / environment type" =
        cfg$production_source_col,
      
      "Currency flag" =
        cfg$currency_flag_col
    )
    
    title_parts <- character(0)
    
    for (dimension_label in names(title_dimensions)) {
      
      column_name <- title_dimensions[[
        dimension_label
      ]]
      
      if (
        is.null(column_name) ||
        !column_name %in% names(display_dt)
      ) {
        next
      }
      
      values_i <- unique(
        trimws(
          as.character(
            display_dt[[column_name]]
          )
        )
      )
      
      values_i <- values_i[
        !is.na(values_i) &
          nzchar(values_i)
      ]
      
      if (length(values_i) == 0L) {
        next
      }
      
      title_parts <- c(
        title_parts,
        paste0(
          dimension_label,
          ": ",
          paste(
            values_i,
            collapse = ", "
          )
        )
      )
    }
    
    
    plot_title <- paste0(
      if (isTRUE(differences_only)) {
        "Differences only — "
      } else {
        ""
      },
      "Current vs comparison — measured element ",
      measured_element_id
    )
    
    if (length(title_parts) > 0L) {
      
      plot_title <- paste0(
        plot_title,
        "\n",
        paste(
          title_parts,
          collapse = " | "
        )
      )
    }
    
    
    # ------------------------------------------------------------
    # Time dimension
    # ------------------------------------------------------------
    
    year_values <- as.character(
      dt[[year_col]]
    )
    
    validate(
      need(
        !all(
          year_values ==
            "Selected period"
        ),
        paste0(
          "The selected aggregation combines all selected years into one period. ",
          "A time-series comparison cannot be displayed."
        )
      )
    )
    
    dt[
      ,
      year_numeric := suppressWarnings(
        as.numeric(
          as.character(
            get(year_col)
          )
        )
      )
    ]
    
    validate(
      need(
        all(
          !is.na(
            dt$year_numeric
          )
        ),
        "The year values cannot be converted to numeric values."
      )
    )
    
    setorder(
      dt,
      year_numeric
    )
    
    
    # ------------------------------------------------------------
    # Data availability
    # ------------------------------------------------------------
    
    n_current <- sum(
      !is.na(
        dt$current_value
      )
    )
    
    n_comparison <- sum(
      !is.na(
        dt$comparison_value
      )
    )
    
    validate(
      need(
        n_current > 0L ||
          n_comparison > 0L,
        "Neither dataset contains values for the selected dimensions."
      )
    )
    
    
    # ------------------------------------------------------------
    # Explanatory subtitle
    # ------------------------------------------------------------
    
    comparable_rows <- dt[
      !is.na(current_value) &
        !is.na(comparison_value)
    ]
    
    common_values_equal <- (
      nrow(comparable_rows) > 0L &&
        all(
          round(
            comparable_rows$current_value -
              comparable_rows$comparison_value,
            digits =
              VALUE_DECIMAL_DIGITS
          ) == 0
        )
    )
    
    current_years <- dt[
      !is.na(current_value),
      year_numeric
    ]
    
    comparison_years <- dt[
      !is.na(comparison_value),
      year_numeric
    ]
    
    same_year_coverage <- identical(
      current_years,
      comparison_years
    )
    
    completely_identical <- (
      n_current > 0L &&
        n_comparison > 0L &&
        same_year_coverage &&
        isTRUE(
          common_values_equal
        )
    )
    
    plot_notes <- character(0)
    
    if (isTRUE(differences_only)) {
      
      plot_notes <- c(
        plot_notes,
        paste0(
          "Only rows classified as differences in the comparison table are displayed."
        )
      )
    }
    
    if (
      n_current == 0L &&
      n_comparison > 0L
    ) {
      
      plot_notes <- c(
        plot_notes,
        paste0(
          "This selection occurs only in the comparison dataset; ",
          "no corresponding values are available in the current dataset."
        )
      )
      
    } else if (
      n_comparison == 0L &&
      n_current > 0L
    ) {
      
      plot_notes <- c(
        plot_notes,
        paste0(
          "This selection occurs only in the current dataset; ",
          "no corresponding values are available in the comparison dataset."
        )
      )
      
    } else if (
      !isTRUE(differences_only)
    ) {
      
      if (isTRUE(completely_identical)) {
        
        plot_notes <- c(
          plot_notes,
          paste0(
            "The current and comparison datasets have identical values ",
            "for all displayed years."
          )
        )
        
      } else if (
        isTRUE(common_values_equal) &&
        !isTRUE(same_year_coverage)
      ) {
        
        plot_notes <- c(
          plot_notes,
          paste0(
            "Values are identical wherever both datasets contain data, ",
            "but the two datasets have different year coverage."
          )
        )
      }
    }
    
    if (
      n_current == 1L &&
      n_comparison > 0L
    ) {
      
      plot_notes <- c(
        plot_notes,
        paste0(
          "The current dataset contains only one available observation; ",
          "it is shown as a point."
        )
      )
    }
    
    if (
      n_comparison == 1L &&
      n_current > 0L
    ) {
      
      plot_notes <- c(
        plot_notes,
        paste0(
          "The comparison dataset contains only one available observation; ",
          "it is shown as a point."
        )
      )
    }
    
    plot_subtitle <- if (
      length(plot_notes) > 0L
    ) {
      
      paste(
        plot_notes,
        collapse = "\n"
      )
      
    } else {
      
      NULL
    }
    
    
    # ------------------------------------------------------------
    # Plot
    # ------------------------------------------------------------
    
    ggplot() +
      
      geom_line(
        data = dt,
        aes(
          x = year_numeric,
          y = current_value,
          colour = "Current dataset",
          linetype = "Current dataset",
          group = 1
        ),
        linewidth = 1.15,
        na.rm = FALSE
      ) +
      
      geom_point(
        data = dt[
          !is.na(current_value)
        ],
        aes(
          x = year_numeric,
          y = current_value,
          colour = "Current dataset",
          shape = "Current dataset"
        ),
        size = 2.8
      ) +
      
      geom_line(
        data = dt,
        aes(
          x = year_numeric,
          y = comparison_value,
          colour = "Comparison dataset",
          linetype = "Comparison dataset",
          group = 1
        ),
        linewidth = 1.15,
        na.rm = FALSE
      ) +
      
      geom_point(
        data = dt[
          !is.na(comparison_value)
        ],
        aes(
          x = year_numeric,
          y = comparison_value,
          colour = "Comparison dataset",
          shape = "Comparison dataset"
        ),
        size = 3
      ) +
      
      scale_linetype_manual(
        values = c(
          "Current dataset" = "solid",
          "Comparison dataset" = "dashed"
        )
      ) +
      
      scale_shape_manual(
        values = c(
          "Current dataset" = 16,
          "Comparison dataset" = 2
        )
      ) +
      
      labs(
        x = "Year",
        y = "Value",
        colour = "Dataset",
        linetype = "Dataset",
        shape = "Dataset",
        title = plot_title,
        subtitle = plot_subtitle
      ) +
      
      scale_x_continuous(
        breaks = sort(
          unique(
            dt$year_numeric
          )
        )
      ) +
      
      theme_minimal() +
      
      theme(
        
        legend.position = "top",
        
        legend.title = element_text(
          face = "bold",
          size = 12
        ),
        
        legend.text = element_text(
          size = 11
        ),
        
        plot.title = element_text(
          face = "bold",
          size = 14
        ),
        
        plot.subtitle = element_text(
          size = 12,
          margin = margin(
            t = 6,
            b = 12
          )
        ),
        
        axis.title = element_text(
          face = "bold",
          size = 11
        ),
        
        axis.text = element_text(
          size = 10
        )
      ) +
      
      guides(
        
        colour = guide_legend(
          order = 1,
          override.aes = list(
            linetype = c(
              "solid",
              "dashed"
            ),
            shape = c(
              16,
              2
            )
          )
        ),
        
        linetype = "none",
        shape = "none"
      )
  }
  
  
  # -------------------------------------------------------------------------
  # All comparison rows chart
  # -------------------------------------------------------------------------
  
  output$comparison_time_series_plot <- renderPlot({
    
    dt <- comparison_plot_filtered_data()
    
    
    build_comparison_time_series_plot(
      dt = dt,
      measured_element_id =
        input$comparison_plot_output_id,
      differences_only = FALSE
    )
  })
  
  
  # -------------------------------------------------------------------------
  # Differences-only chart
  # -------------------------------------------------------------------------
  
  output$comparison_difference_time_series_plot <- renderPlot({
    
    dt <- comparison_difference_plot_filtered_data()
    
    
    build_comparison_time_series_plot(
      dt = dt,
      measured_element_id =
        input$comparison_difference_plot_output_id,
      differences_only = TRUE
    )
  })
  
  
  
  output$client <- renderTable({
    req(user())
    u <- as.data.table(user())
    u[, seq_len(min(5, ncol(u))), with = FALSE]
  })
  
  output$dataset_count <- renderText({
    req(
      input$dataset_group,
      dataset_config_ready()
    )
    
    n <- length(
      get_primary_dataset_choices(
        input$dataset_group
      )
    )
    
    paste0(
      n,
      " datasets available in group '",
      input$dataset_group,
      "'."
    )
  })
  
  observe({
    req(
      input$dataset_group,
      dataset_config_ready()
    )
    
    updateSelectizeInput(
      session,
      "dataset_id",
      choices = get_primary_dataset_choices(
        input$dataset_group
      ),
      selected = character(0),
      server = TRUE
    )
  })
  
  observeEvent(input$load_dataset, {
    
    req(input$dataset_id)
    
    if (isTRUE(dataset_loading())) {
      showNotification(
        "The dataset is already loading. Please wait.",
        type = "warning",
        duration = 3
      )
      
      return(NULL)
    }
    
    dataset_loading(TRUE)
    
    loading_notification <- showNotification(
      paste0(
        "Loading dataset ",
        input$dataset_id,
        ". Please wait..."
      ),
      type = "default",
      duration = NULL,
      closeButton = FALSE
    )
    
    on.exit(
      {
        dataset_loading(FALSE)
        
        removeNotification(
          loading_notification
        )
      },
      add = TRUE
    )
    
    tryCatch(
      {
        # The selected dataset ID remains the base SWS dataset ID.
        # For example, for a tagged aqua dataset this is still "aqua".
        dataset_info <- getDatasetInfo(
          input$dataset_id
        )
        
        cfg <- get_dataset_config(
          input$dataset_id
        )
        
        # Load either the ordinary dataset or one of its tagged versions.
        if (identical(
          input$dataset_group,
          "tagged"
        )) {
          
          req(input$primary_tag_id)
          
          dt <- as.data.table(
            getTagData(
              as.character(
                input$primary_tag_id
              )
            )
          )
          
        } else {
          
          dt <- readDataset(
            dataset_id = input$dataset_id
          )
        }
        
        rows_before_value_cleaning <- nrow(dt)
        
        # Important for tagged datasets, which may return lowercase "value".
        dt <- normalise_and_drop_empty_values(
          data = dt,
          value_col = cfg$value_col
        )
        
        validation <- validate_dataset_columns(
          data = dt,
          dataset_id = input$dataset_id
        )
        
        rows_discarded_without_values <-
          rows_before_value_cleaning - nrow(dt)
        
        dataset_data(dt)
        loaded_dataset_id(input$dataset_id)
        loaded_dataset_info(dataset_info)
        
        loaded_dataset_source(
          input$dataset_group
        )
        
        if (identical(
          input$dataset_group,
          "tagged"
        )) {
          
          loaded_tag_id(
            as.character(
              input$primary_tag_id
            )
          )
          
          tag_info <- as.data.table(
            getAllTags(
              dataset = input$dataset_id
            )
          )
          
          tag_info[
            ,
            id := as.character(id)
          ]
          
          tag_row <- tag_info[
            id == as.character(
              input$primary_tag_id
            )
          ]
          
          loaded_tag_label(
            if (nrow(tag_row) > 0L) {
              as.character(
                tag_row$name[1L]
              )
            } else {
              as.character(
                input$primary_tag_id
              )
            }
          )
          
        } else {
          
          loaded_tag_id(NULL)
          loaded_tag_label(NULL)
        }
        
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
          paste0(
            "Dataset loading failed: ",
            e$message
          ),
          type = "error"
        )
      }
    )
  })
  
  
  
  
  output$dataset_summary <- renderUI({
    
    req(
      dataset_data(),
      loaded_dataset_info(),
      input$dataset_id
    )
    
    req(
      identical(
        loaded_dataset_id(),
        input$dataset_id
      )
    )
    
    dt <- dataset_data()
    
    cfg <- get_dataset_config(
      input$dataset_id
    )
    
    measured_col <- cfg$measured_element_col
    
    
    # ------------------------------------------------------------
    # Configured measured-element roots from SWS metadata.
    # ------------------------------------------------------------
    
    configured_roots <- get_dataset_dimension_roots(
      dataset_info = loaded_dataset_info(),
      dimension_id = measured_col
    )
    
    configured_roots <- clean_code_vector(
      configured_roots
    )
    
    
    # ------------------------------------------------------------
    # Resolve configured roots to the measured elements that
    # actually occur in the dataset.
    #
    # If a configured root has children, use its children.
    # If it has no children, use the root itself.
    # ------------------------------------------------------------
    
    effective_measured_elements <- get_effective_measured_elements(
      dataset_info = loaded_dataset_info(),
      data = dt,
      measured_col = measured_col
    )
    
    
    # ------------------------------------------------------------
    # Restrict summary data to the effective measured elements.
    # ------------------------------------------------------------
    
    dt_summary <- copy(dt)
    
    if (
      !is.null(measured_col) &&
      measured_col %in% names(dt_summary)
    ) {
      
      if (length(effective_measured_elements) > 0L) {
        
        dt_summary <- dt_summary[
          as.character(
            get(measured_col)
          ) %in%
            effective_measured_elements
        ]
        
      } else {
        
        dt_summary <- dt_summary[0]
      }
    }
    
    
    # ------------------------------------------------------------
    # Calculate available years from the actual measured elements.
    # ------------------------------------------------------------
    
    years <- get_year_values(
      data = dt_summary,
      year_col = cfg$year_col
    )
    
    
    # ------------------------------------------------------------
    # Read measured-element codelist for labels and hierarchy.
    # ------------------------------------------------------------
    
    codes <- tryCatch(
      get_codelist_codes(
        "measuredElement"
      ),
      error = function(e) NULL
    )
    
    
    # ------------------------------------------------------------
    # Labels for configured roots.
    # ------------------------------------------------------------
    
    configured_root_labels <- configured_roots
    
    if (
      length(configured_roots) > 0L &&
      !is.null(codes)
    ) {
      
      roots_dt <- data.table(
        measured_element_root =
          configured_roots
      )
      
      setnames(
        roots_dt,
        "measured_element_root",
        measured_col
      )
      
      root_choices <- make_filter_choices_from_data(
        data = roots_dt,
        column_name = measured_col,
        codes = codes
      )
      
      if (length(root_choices) > 0L) {
        configured_root_labels <- names(
          root_choices
        )
      }
    }
    
    
    # ------------------------------------------------------------
    # Labels for effective measured elements.
    # ------------------------------------------------------------
    
    effective_element_labels <-
      effective_measured_elements
    
    if (
      length(effective_measured_elements) > 0L &&
      !is.null(codes)
    ) {
      
      effective_dt <- data.table(
        measured_element =
          effective_measured_elements
      )
      
      setnames(
        effective_dt,
        "measured_element",
        measured_col
      )
      
      effective_choices <-
        make_filter_choices_from_data(
          data = effective_dt,
          column_name = measured_col,
          codes = codes
        )
      
      if (length(effective_choices) > 0L) {
        effective_element_labels <- names(
          effective_choices
        )
      }
    }
    
    
    # ------------------------------------------------------------
    # Identify which configured roots have usable data underneath.
    # ------------------------------------------------------------
    
    configured_roots_with_data <-
      character(0)
    
    if (
      length(configured_roots) > 0L &&
      !is.null(codes) &&
      !is.null(measured_col) &&
      measured_col %in% names(dt)
    ) {
      
      data_measured_elements <- clean_code_vector(
        dt[[measured_col]]
      )
      
      configured_roots_with_data <- configured_roots[
        vapply(
          configured_roots,
          function(root_i) {
            
            children_i <- get_direct_children(
              codes = codes,
              parent_code = root_i
            )
            
            effective_i <- if (
              length(children_i) > 0L
            ) {
              children_i
            } else {
              root_i
            }
            
            any(
              effective_i %in%
                data_measured_elements
            )
          },
          logical(1)
        )
      ]
    }
    
    
    configured_root_labels_with_data <-
      configured_roots_with_data
    
    if (
      length(configured_roots_with_data) > 0L
    ) {
      
      root_positions <- match(
        configured_roots_with_data,
        configured_roots
      )
      
      matched <- !is.na(
        root_positions
      )
      
      configured_root_labels_with_data[
        matched
      ] <- configured_root_labels[
        root_positions[matched]
      ]
    }
    
    
    # ------------------------------------------------------------
    # Other available dimensions.
    # ------------------------------------------------------------
    
    available_dimensions <- c(
      "Geographical area",
      "Species / ASFIS",
      "Fishing area"
    )
    
    if (
      !is.null(cfg$production_source_col) &&
      cfg$production_source_col %in%
      names(dt)
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
        switch(
          loaded_dataset_source() %||% cfg$dataset_group,
          current = "Current Fisheries dataset",
          disseminated = "Disseminated / previous dataset",
          tagged = "Tagged Fisheries dataset",
          "Unknown"
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
        if (length(years) > 0L) {
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
        length(
          configured_roots
        )
      ),
      
      tags$p(
        tags$strong(
          "Configured measured-element roots: "
        ),
        if (
          length(configured_root_labels) > 0L
        ) {
          paste(
            configured_root_labels,
            collapse = ", "
          )
        } else {
          "No measured-element roots are configured"
        }
      ),
      
      tags$p(
        tags$strong(
          "Measured elements available under configured roots: "
        ),
        if (
          length(effective_element_labels) > 0L
        ) {
          paste(
            effective_element_labels,
            collapse = ", "
          )
        } else {
          "None"
        }
      ),
      
      tags$p(
        tags$strong(
          "Number of configured roots with usable data: "
        ),
        length(
          configured_roots_with_data
        )
      ),
      
      tags$p(
        tags$strong(
          "Configured roots with usable data: "
        ),
        if (
          length(
            configured_root_labels_with_data
          ) > 0L
        ) {
          paste(
            configured_root_labels_with_data,
            collapse = ", "
          )
        } else {
          "None of the configured roots contains usable data"
        }
      ),
      
      tags$p(
        tags$strong(
          "Main dimensions available: "
        ),
        paste(
          available_dimensions,
          collapse = ", "
        )
      ),
      
      tags$div(
        class = "alert alert-info",
        paste0(
          "Measured-element roots are read from the dataset configuration. ",
          "When a configured root contains child measured elements, ",
          "the summary uses the child elements available in the dataset."
        )
      )
    )
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
    measured_roots <- get_effective_measured_elements(
      dataset_info = loaded_dataset_info(),
      data = dt,
      measured_col = measured_col
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
    
    id_cols <- get_tree_id_cols(
      tree_dt
    )
    
    if (length(id_cols) == 0L) {
      return(character(0))
    }
    
    filtered_raw_codes <- clean_non_empty_codes(
      filtered_raw_codes
    )
    
    if (length(filtered_raw_codes) == 0L) {
      return(character(0))
    }
    
    
    # ------------------------------------------------------------
    # Identify which filtered codes actually occur in the hierarchy.
    #
    # Do this directly from the hierarchy columns instead of creating
    # a complete copy of all hierarchy ID columns.
    # ------------------------------------------------------------
    
    tree_codes <- unique(
      unlist(
        lapply(
          id_cols,
          function(column_i) {
            
            values_i <- as.character(
              tree_dt[[column_i]]
            )
            
            values_i[
              !is.na(values_i) &
                nzchar(values_i)
            ]
          }
        ),
        recursive = TRUE,
        use.names = FALSE
      )
    )
    
    filtered_raw_codes <- intersect(
      filtered_raw_codes,
      tree_codes
    )
    
    if (length(filtered_raw_codes) == 0L) {
      return(character(0))
    }
    
    
    # ------------------------------------------------------------
    # Find ancestors of the filtered raw codes.
    #
    # Scan each hierarchy level once, but do not create a complete
    # character copy of the hierarchy table.
    # ------------------------------------------------------------
    
    ancestor_codes <- character(0)
    
    for (i in seq_along(id_cols)) {
      
      values_i <- as.character(
        tree_dt[[id_cols[i]]]
      )
      
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
        
        ancestors_i <- unlist(
          lapply(
            ancestor_columns,
            function(column_i) {
              as.character(
                tree_dt[[column_i]][matching_rows]
              )
            }
          ),
          recursive = TRUE,
          use.names = FALSE
        )
        
        ancestor_codes <- c(
          ancestor_codes,
          ancestors_i
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
    
    tree_dt <- as.data.table(tree_dt)
    
    id_cols <- get_tree_id_cols(
      tree_dt
    )
    
    relevant_codes <- clean_non_empty_codes(
      relevant_codes
    )
    
    if (
      length(id_cols) == 0L ||
      length(relevant_codes) == 0L
    ) {
      return(
        tree_dt[0]
      )
    }
    
    
    # ------------------------------------------------------------
    # First identify only rows containing at least one relevant code.
    #
    # This avoids copying the complete hierarchy before pruning it.
    # ------------------------------------------------------------
    
    keep_row <- Reduce(
      `|`,
      lapply(
        id_cols,
        function(column_i) {
          
          values_i <- as.character(
            tree_dt[[column_i]]
          )
          
          !is.na(values_i) &
            nzchar(values_i) &
            values_i %in% relevant_codes
        }
      )
    )
    
    if (!any(keep_row)) {
      return(
        tree_dt[0]
      )
    }
    
    
    # Copy only the rows that will actually survive.
    out <- copy(
      tree_dt[keep_row]
    )
    
    
    # ------------------------------------------------------------
    # Within the retained rows, remove hierarchy nodes that do not
    # belong to the relevant-code set.
    # ------------------------------------------------------------
    
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
    
    selected_filter_roots <- get_effective_filter_roots(
      dim_id = dim_id,
      meta = meta,
      codes = codes,
      tree_dt = complete_tree_dt
    )
    
    relevant_codes <- clean_non_empty_codes(
      c(
        data_relevant_codes,
        selected_filter_roots
      )
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
      SYNTHETIC_EXPIRED_ROOTS_ID
    )
    
    # Use exactly the same dataset-configured roots as the filtering tree.
    configured_roots <- clean_non_empty_codes(
      get_configured_roots_for_dimension(
        meta
      )
    )
    
    tree_root_codes <- get_display_roots_for_tree(
      codelist_id = meta$codelist,
      configured_roots = configured_roots,
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
          
          raw_codes <- sort(
            clean_non_empty_codes(
              dt[[meta$dataset_column]]
            )
          )
          
          if (length(raw_codes) == 0L) {
            return(list())
          }
          
          
          # ----------------------------------------------------------
          # Clean the configured roots before checking the cached tree.
          # ----------------------------------------------------------
          configured_roots_clean <- sort(
            clean_non_empty_codes(
              configured_roots
            )
          )
          
          
          # ----------------------------------------------------------
          # Check whether the final filter tree has already been built
          # today for exactly the same dataset values and roots.
          # ----------------------------------------------------------
          filter_tree_cache_id <- paste0(
            "filter_tree_v2__",
            input$dataset_id,
            "__",
            current_dim,
            "__",
            meta$codelist
          )
          
          cached_filter_tree <- get_daily_shared_cache(
            filter_tree_cache_id
          )
          
          if (
            !is.null(cached_filter_tree) &&
            is.list(cached_filter_tree) &&
            identical(
              cached_filter_tree$raw_codes,
              raw_codes
            ) &&
            identical(
              cached_filter_tree$configured_roots,
              configured_roots_clean
            )
          ) {
            return(
              cached_filter_tree$tree
            )
          }
          
          
          # ----------------------------------------------------------
          # No valid final-tree cache exists.
          # Build the tree exactly as before.
          # ----------------------------------------------------------
          codes <- as.data.table(
            get_codelist_codes(
              meta$codelist
            )
          )
          
          codes[
            ,
            id := as.character(id)
          ]
          
          complete_tree_dt <- get_codelist_tree_cached(
            meta$codelist
          )
          
          relevant_codes <- get_relevant_hierarchy_codes(
            tree_dt = complete_tree_dt,
            filtered_raw_codes = raw_codes
          )
          
          if (length(relevant_codes) == 0L) {
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
          
          tree_root_codes <- get_display_roots_for_tree(
            codelist_id = meta$codelist,
            configured_roots = configured_roots_clean,
            relevant_codes = relevant_codes,
            purpose = "filter"
          )
          
          if (length(tree_root_codes) == 0L) {
            return(list())
          }
          
          
          # ----------------------------------------------------------
          # Build the final tree.
          # ----------------------------------------------------------
          final_tree <- build_sws_codelist_tree_from_codelist_tree(
            tree_dt = filtered_tree_dt,
            codes = relevant_codelist_codes,
            max_depth =
              get_codelist_tree_display_depth(
                filtered_tree_dt
              ),
            root_codes = tree_root_codes
          )
          
          
          # ----------------------------------------------------------
          # Save the final tree in today's shared cache.
          #
          # Store the exact raw codes and configured roots used to
          # create it so that a cached tree is reused only when its
          # inputs are identical.
          # ----------------------------------------------------------
          set_daily_shared_cache(
            cache_id = filter_tree_cache_id,
            value = list(
              raw_codes = raw_codes,
              configured_roots = configured_roots_clean,
              tree = final_tree
            )
          )
          
          
          return(
            final_tree
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
    #aggregated_data(NULL)
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
      
      measured_roots <- get_effective_measured_elements(
        dataset_info = loaded_dataset_info(),
        data = dt,
        measured_col = measured_col
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
    
    dt <- base_analysis_data()
    
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
            "brCatchArea"
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
            #aggregated_data(NULL)
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
            
            # if (length(successful_outputs) > 0) {
            #   aggr_all <- rbindlist(
            #     successful_outputs,
            #     use.names = TRUE,
            #     fill = TRUE
            #   )
            # } else {
            #   aggr_all <- data.table()
            # }
            
            incProgress(
              amount = 0.05,
              detail = "Saving aggregation results..."
            )
            
            # aggregated_data(aggr_all)
            aggregated_outputs(output_list)
            
            # Save the exact data, aggregation specifications and filter state
            # that produced aggregated_outputs().
            aggregation_input_data(dt)
            last_aggregation_specs(aggregation_specs)
            last_comparison_state(comparison_state)
            
            nav_select(
              id = "main_nav",
              selected = "dataset_table"
            )
            
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
    
    if (length(outputs) == 0L) {
      return(data.table())
    }
    
    selected_output <- input$aggregation_output_id
    
    if (
      is.null(selected_output) ||
      !selected_output %in% names(outputs)
    ) {
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
          
          aggregation_table <- datatable(
            dt_to_show,
            rownames = FALSE,
            filter = "top",
            options = list(
              pageLength = 10,
              scrollX = TRUE
            )
          )
          
          if (cfg$value_col %in% names(dt_to_show)) {
            
            aggregation_table <- formatRound(
              aggregation_table,
              columns = cfg$value_col,
              digits = VALUE_DECIMAL_DIGITS,
              mark = " ",
              dec.mark = "."
            )
          }
          
          aggregation_table
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
  
  # Return the y-axis label associated with the selected measured element.
  get_graph_y_axis_label <- function() {
    
    selected_element <- selected_graph_output_name()
    
    if (
      is.null(selected_element) ||
      !nzchar(selected_element)
    ) {
      return("Aggregated value")
    }
    
    codes <- tryCatch(
      as.data.table(
        get_codelist_codes(
          "measuredElement"
        )
      ),
      error = function(e) NULL
    )
    
    if (
      is.null(codes) ||
      !"id" %in% names(codes) ||
      !"unit" %in% names(codes)
    ) {
      return("Aggregated value")
    }
    
    codes[
      ,
      id := as.character(id)
    ]
    
    unit_value <- codes[
      id == as.character(selected_element),
      as.character(unit)
    ]
    
    unit_value <- trimws(
      unit_value[
        !is.na(unit_value) &
          nzchar(unit_value)
      ]
    )
    
    if (length(unit_value) == 0L) {
      return("Aggregated value")
    }
    
    unit_value <- unit_value[1L]
    
    if (
      tolower(unit_value) %in%
      c("t", "tonne", "tonnes")
    ) {
      return("Quantity (tonnes)")
    }
    
    if (
      tolower(unit_value) %in%
      c(
        "number",
        "numbers",
        "no",
        "no."
      )
    ) {
      return("Quantity (numbers)")
    }
    
    paste0(
      "Quantity (",
      unit_value,
      ")"
    )
  }
  
  
  
  selected_graph_data <- reactive({
    outputs <- aggregated_outputs()
    
    if (length(outputs) == 0L) {
      return(data.table())
    }
    
    selected <- selected_graph_output_name()
    
    if (
      is.null(selected) ||
      !selected %in% names(outputs)
    ) {
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
      
      dt <- copy(dt)
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
        y = get_graph_y_axis_label(),
        title = paste0(
          "Aggregated total time series — ",
          selected_element
        )
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(
          angle = 90,
          vjust = 0.5,
          hjust = 1
        )
      )
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
  
  # Add descriptive labels to graph category codes when available.
  add_graph_category_labels <- function(
    plot_data,
    selected_category
  ) {
    
    dt <- copy(
      as.data.table(plot_data)
    )
    
    if (
      nrow(dt) == 0L ||
      !"category" %in% names(dt)
    ) {
      return(dt)
    }
    
    dt[
      ,
      category_label := as.character(category)
    ]
    
    
    # ------------------------------------------------------------
    # Aggregation composition.
    # ------------------------------------------------------------
    if (
      !is.null(selected_category) &&
      startsWith(
        selected_category,
        "agg:"
      )
    ) {
      
      dim_id <- sub(
        "^agg:",
        "",
        selected_category
      )
      
      specs <- last_aggregation_specs() %||% list()
      
      if (dim_id %in% names(specs)) {
        
        codelist_id <- specs[[dim_id]]$codelist
        
        codes <- tryCatch(
          as.data.table(
            get_codelist_codes(
              codelist_id
            )
          ),
          error = function(e) NULL
        )
        
        if (
          !is.null(codes) &&
          "id" %in% names(codes)
        ) {
          
          codes[
            ,
            id := as.character(id)
          ]
          
          label_col <- intersect(
            c(
              "label_en",
              "label",
              "description"
            ),
            names(codes)
          )[1]
          
          if (!is.na(label_col)) {
            
            label_lookup <- codes[
              ,
              .(
                id,
                category_label = paste0(
                  id,
                  " - ",
                  as.character(
                    get(label_col)
                  )
                )
              )
            ]
            
            matched <- match(
              dt$category,
              label_lookup$id
            )
            
            has_match <- !is.na(matched)
            
            dt$category_label[has_match] <-
              label_lookup$category_label[
                matched[has_match]
              ]
          }
        }
      }
    }
    
    
    # ------------------------------------------------------------
    # Final-result dataset column.
    # ------------------------------------------------------------
    if (
      !is.null(selected_category) &&
      startsWith(
        selected_category,
        "col:"
      )
    ) {
      
      selected_col <- sub(
        "^col:",
        "",
        selected_category
      )
      
      dimension_match <- Filter(
        function(meta) {
          identical(
            meta$dataset_column,
            selected_col
          )
        },
        AGGREGATION_DIMENSIONS
      )
      
      if (length(dimension_match) > 0L) {
        
        codelist_id <-
          dimension_match[[1L]]$codelist
        
        codes <- tryCatch(
          as.data.table(
            get_codelist_codes(
              codelist_id
            )
          ),
          error = function(e) NULL
        )
        
        if (
          !is.null(codes) &&
          "id" %in% names(codes)
        ) {
          
          codes[
            ,
            id := as.character(id)
          ]
          
          label_col <- intersect(
            c(
              "label_en",
              "label",
              "description"
            ),
            names(codes)
          )[1]
          
          if (!is.na(label_col)) {
            
            label_lookup <- codes[
              ,
              .(
                id,
                category_label = paste0(
                  id,
                  " - ",
                  as.character(
                    get(label_col)
                  )
                )
              )
            ]
            
            matched <- match(
              dt$category,
              label_lookup$id
            )
            
            has_match <- !is.na(matched)
            
            dt$category_label[has_match] <-
              label_lookup$category_label[
                matched[has_match]
              ]
          }
        }
      }
    }
    
    dt[]
  }
  
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
    plot_data <- add_graph_category_labels(
      plot_data = plot_data,
      selected_category = input$plot_category
    )
    
    
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
    plot_data <- add_graph_category_labels(
      plot_data = plot_data,
      selected_category = input$plot_category
    )
    
    if (nrow(plot_data) == 0) {
      plot.new()
      text(0.5, 0.5, "No treemap data available")
      return(NULL)
    }
    
    treemap_data <- plot_data[
      as.character(year) ==
        as.character(input$treemap_year),
      .(
        total_value = sum(
          total_value,
          na.rm = TRUE
        )
      ),
      by = .(
        category,
        category_label
      )
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
        category_label,
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
        fill = category_label,
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
    plot_data <- add_graph_category_labels(
      plot_data = plot_data,
      selected_category = input$plot_category
    )
    
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
        fill = category_label
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
    plot_data <- add_graph_category_labels(
      plot_data = plot_data,
      selected_category = input$plot_category
    )
    
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
        colour = category_label,
        group = category_label
      )
    ) +
      geom_line() +
      geom_point() +
      scale_y_continuous(labels = scales::label_number()) +
      labs(
        x = "Year",
        y = get_graph_y_axis_label(),
        colour = "Category",
        title = paste0(
          "Component time series by selected category — ",
          selected_element
        )
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(
          angle = 90,
          vjust = 0.5,
          hjust = 1
        )
      )
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
    
    if (length(outputs) == 0L) {
      return(data.table())
    }
    
    selected <- selected_outlier_output_name()
    
    if (
      is.null(selected) ||
      !selected %in% names(outputs)
    ) {
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
    
    outlier_table <- datatable(
      dt[, ..display_cols],
      rownames = FALSE,
      options = list(
        pageLength = n_show,
        lengthChange = FALSE,
        scrollX = TRUE,
        scrollY = "520px"
      )
    )
    
    value_columns <- intersect(
      c(
        cfg$value_col,
        "previous_value",
        "outlier_metric_value"
      ),
      display_cols
    )
    
    if (length(value_columns) > 0L) {
      
      outlier_table <- formatRound(
        outlier_table,
        columns = value_columns,
        digits = VALUE_DECIMAL_DIGITS,
        mark = " ",
        dec.mark = "."
      )
    }
    
    outlier_table
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
    plot_data <- add_graph_category_labels(
      plot_data = plot_data,
      selected_category = input$plot_category
    )
    
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
        fill = category_label
      )
    ) +
      geom_col() +
      scale_y_continuous(labels = scales::label_number()) +
      labs(
        x = "Year",
        y = get_graph_y_axis_label(),
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
  
  # output$download_csv <- downloadHandler(
  #   filename = function() {
  #     selected_dims <- names(Filter(
  #       isTRUE,
  #       list(
  #         species = input$apply_species,
  #         geographical_area = input$apply_geographical_area,
  #         fishing_area = input$apply_fishing_area,
  #         production_source = input$apply_production_source,
  #         observation_status = input$apply_observation_flag
  #       )
  #     ))
  #     
  #     if (length(selected_dims) == 0) {
  #       selected_dims <- "no_aggregation"
  #     } else {
  #       selected_dims <- paste(selected_dims, collapse = "_")
  #     }
  #     
  #     paste0(
  #       "aggregated_",
  #       input$dataset_id %||% "dataset",
  #       "_",
  #       selected_dims,
  #       ".csv"
  #     )
  #   },
  #   content = function(file) {
  #     req(aggregated_data())
  #     fwrite(aggregated_data(), file)
  #   }
  # )
}

