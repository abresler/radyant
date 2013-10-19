output$view_order <- renderUI({
  if(is.null(input$columns)) return()
  selectInput(inputId = "view_order", label = "Order by:", choices = c("None",input$columns), selected = "None", multiple = FALSE)
})

ui_View <- function() {
  list(wellPanel(
      uiOutput("columns"), 
     	uiOutput("view_order"), checkboxInput("view_order_desc", "DESC", value = FALSE),
      textInput("dv_select", "Subset (e.g., mpg > 20 & vs == 1)", ''), 
      actionButton("sub_select", "Go"),
      uiOutput("nrRows")
    ),
    helpModal('View','view',includeMarkdown("tools/help/view.md"))
  )
}
