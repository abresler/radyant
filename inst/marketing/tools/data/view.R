# used in ui.R. Structure relevant for (future) modularization
output$ui_view <- renderUI({
	ui_view()
})

output$view_order <- renderUI({
  if(is.null(input$columns)) return()
  selectInput(inputId = "view_order", label = "Order by:", choices = c("None",input$columns), selected = "None", multiple = FALSE)
})

ui_view <- function() {
  wellPanel(
    uiOutput("columns"), 
   	uiOutput("view_order"), checkboxInput("view_order_desc", "DESC", value = FALSE),
    textInput("dv_select", "Subset (e.g., mpg > 20 & vs == 1)", ''), 
    actionButton("sub_select", "Go"),
    uiOutput("nrRows")
  )
}
