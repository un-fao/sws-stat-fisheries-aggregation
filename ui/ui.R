
#############################################
# UI
#############################################
ui <- page_navbar(
  id = "main_nav",
  title = "Fisheries Aggregation app",
  fillable = FALSE,
  
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#0072B2"
  ),
  
  tags$head(
    tags$script(src = "fisheries-diagnostics.js"),
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
            "Previous disseminated datasets" = "disseminated",
            "Tagged Fisheries datasets" = "tagged"
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
        
        uiOutput("primary_tag_selector"),
        
        textOutput("dataset_count"),
        
        actionButton(
          "load_dataset",
          "Load selected dataset",
          class = "btn-secondary",
          width = "100%"
        )
      ),
      
      layout_columns(
        col_widths = c(4, 8),
        
        card(
          card_header("Client"),
          tableOutput("client")
        ),
        
        card(
          card_header("Loaded dataset"),
          uiOutput("dataset_summary")
        )
      )
    )
  ),
  
  nav_panel(
    "2. Filters & aggregation",
    
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
    "3. Dataset table",
    value = "dataset_table",
    
    card(
      full_screen = TRUE,
      card_header("Aggregated data preview"),
      uiOutput("aggregated_outputs_tables")
    )
  ),
  
  nav_panel(
    "4. Graphs",
    
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
    "5. Comparison",
    
    # This message is displayed when comparison is not allowed.
    conditionalPanel(
      condition = "!output.comparison_available",
      
      card(
        card_header("Comparison unavailable"),
        
        p(
          "Comparison is available when the main loaded dataset is a current ",
          "Fisheries dataset or a tagged version of a current Fisheries dataset."
        ),
        
        p(
          "A disseminated dataset can still be filtered, aggregated, plotted and ",
          "analysed for outliers, but it cannot currently be used as the main ",
          "dataset on the Comparison page."
        )
      )
    ),
    
    # Comparison controls are displayed for current datasets
    # and tagged versions of current Fisheries datasets.
    conditionalPanel(
      condition = "output.comparison_available",
      
      card(
        card_header("Main vs comparison dataset"),
        
        p(
          "Compare the currently loaded dataset with a current Fisheries dataset, ",
          "a disseminated dataset, or a tagged dataset."
        )
      ),
      
      layout_sidebar(
        sidebar = sidebar(
          width = 360,
          
          selectInput(
            "comparison_source",
            "Comparison dataset source",
            choices = c(
              "Current Fisheries datasets" = "current",
              "Disseminated domain" = "disseminated",
              "Tagged datasets" = "tagged"
            ),
            selected = "current"
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
          ),
          
          nav_panel(
            "Charts",
            
            navset_card_tab(
              
              nav_panel(
                "All comparison rows",
                
                layout_sidebar(
                  
                  sidebar = sidebar(
                    width = 340,
                    
                    uiOutput(
                      "comparison_plot_output_selector"
                    ),
                    
                    uiOutput(
                      "comparison_plot_dimension_selectors"
                    )
                  ),
                  
                  card(
                    full_screen = TRUE,
                    card_header(
                      "Main vs comparison dataset"
                    ),
                    
                    plotOutput(
                      "comparison_time_series_plot",
                      height = 480
                    )
                  )
                )
              ),
              
              
              nav_panel(
                "Differences only",
                
                layout_sidebar(
                  
                  sidebar = sidebar(
                    width = 340,
                    
                    uiOutput(
                      "comparison_difference_plot_output_selector"
                    ),
                    
                    uiOutput(
                      "comparison_difference_plot_dimension_selectors"
                    )
                  ),
                  
                  card(
                    full_screen = TRUE,
                    card_header(
                      "Main vs comparison dataset — differences only"
                    ),
                    
                    plotOutput(
                      "comparison_difference_time_series_plot",
                      height = 480
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  ),
  
  
  
  nav_panel(
    "6. Outlier analysis",
    
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
  ),
  
  nav_item(
    tags$a(
      "Documentation",
      href = "https://sws.qa.fao.org/link/FisheriesAggregation-doc/",
      target = "_blank",
      class = "nav-link"
    )
  )
)
