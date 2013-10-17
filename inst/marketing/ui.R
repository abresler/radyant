shinyUI(

  pageWithSidebar(

    # Using a navbar rather than headerPanel to display app title
    headerPanel(''),

    sidebarPanel(

      tags$head(
        tags$link(rel="stylesheet", type="text/css", href="style.css"),
        tags$script(src = 'https://c328740.ssl.cf1.rackcdn.com/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML', type = 'text/javascript')
      ),

      includeHTML("www/js/sel2.js"),
      includeHTML('www/js/lr.js'), 
      getTool("tool"),

      wellPanel(
        uiOutput("datasets")
      ),

      # only show data loading and selection options when in dataview
      conditionalPanel(condition = "input.tool == 'data'",
        conditionalPanel(condition = "input.datatabs == 'Manage'",
          uiOutput("ui_manage"),
          helpModal('Manage','manage',includeRmd("tools/help/example.Rmd"))
        ),
        conditionalPanel(condition = "input.datatabs == 'View'",
          uiOutput("ui_view"),
          helpModal('View','view',includeRmd("tools/help/example.Rmd"))
        ),
        conditionalPanel(condition = "input.datatabs == 'Visualize'",
          uiOutput("ui_visualize"),
          helpModal('Visualize','visualize',includeRmd("tools/help/example.Rmd"))
        ),
        conditionalPanel(condition = "input.datatabs == 'Explore'",
          uiOutput("ui_explore"),
          helpModal('Explore','explore',includeRmd("tools/help/example.Rmd"))
        ),
        conditionalPanel(condition = "input.datatabs == 'Transform'",
          uiOutput("ui_transform"),
          helpModal('Transform','transform',includeRmd("tools/help/example.Rmd"))
        )
        # conditionalPanel(condition = "input.datatabs == 'About'",
        #   actionButton("update", "Update")
        # )
      ),
      conditionalPanel(condition = "input.tool != 'data'",
        # the appropriate analysis code called based on the selection from the navbar
        uiOutput("ui_analysis")
      )

    ),

    # Dataview changes
    # View --> Manage -- focus on loading, saving, removing data 
    # Transform --> Change -- Change the variables and the data (e.g., keeping/removing variables) 
    # Summarize --> Explore -- Put a sortable table in here. Also subset commands etc. and 
    # Visualize as is but with more plotting options
    
    mainPanel(
      conditionalPanel(condition = "input.datasets != ''",
        conditionalPanel(condition = "input.tool == 'data'", 
          tabsetPanel(id = "datatabs",
            tabPanel("Manage", tableOutput("dataexample")),
            tabPanel("View", tableOutput("dataviewer")),
            tabPanel("Visualize", plotOutput("visualize", width = "100%", height = "100%")),
            tabPanel("Explore", verbatimTextOutput("expl_data"), 
              plotOutput("expl_viz", width = "100%", height = "100%")),
            # tabPanel("Merge", #   HTML('<label>Merge data.<br>In progress. Check back soon.</label>') # ),
            tabPanel("Transform", 
              tableOutput("transform_data"), br(),
              verbatimTextOutput("transform_summary")
            ),
            tabPanel("About", includeRmd("about.Rmd"))
          )
        ),
        conditionalPanel(condition = "input.tool != 'data'",
          uiOutput("ui_analysis_tabs")
        )
      )
    )
  )
)