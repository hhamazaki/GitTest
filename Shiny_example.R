ui <- fluidPage(
  actionButton("update", "Update other buttons and link"),
  br(),
  actionButton("goButton", "Go"),
  br(),
  actionButton("goButton2", "Go 2", icon = icon("area-chart")),
  br(),
  actionButton("goButton3", "Go 3"),
  br(),
  actionLink("goLink", "Go Link")
)

server <- function(input, output, session) {
  observe({
    req(input$update)

    # Updates goButton's label and icon
    updateActionButton(session, "goButton",
      label = "New label",
      icon = icon("calendar"))

    # Leaves goButton2's label unchaged and
    # removes its icon
    updateActionButton(session, "goButton2",
      icon = character(0))

    # Leaves goButton3's icon, if it exists,
    # unchaged and changes its label
    updateActionButton(session, "goButton3",
      label = "New label 3")

    # Updates goLink's label and icon
    updateActionButton(session, "goLink",
      label = "New link label",
      icon = icon("link"))
  })
}

shinyApp(ui, server)