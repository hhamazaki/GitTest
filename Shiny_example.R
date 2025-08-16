set.seed(123)
n <- 5
scheme <- paste("Scheme", 1:n)
mitigator <- paste("Mitigator", LETTERS[1:n])
combos <- tidyr::crossing(scheme, mitigator)
nrows <- nrow(combos)
dat <- combos |>
  dplyr::mutate(
    mitigator_group = dplyr::if_else(
      stringr::str_detect(mitigator, " [ABC]$"),
      "Group X",
      "Group Y"
    ),
    lo = runif(nrows),
    hi = pmin(lo + runif(nrows, 0.25, 0.5), 1),
    mid = (lo + hi) / 2,
    years = sample(20:30, nrows, replace = TRUE)
  )

ui <- bslib::page_navbar(
  title = "Test layouts and content",
  sidebar = bslib::sidebar(
    shiny::selectInput(
      inputId = "schemes",
      label = "Select schemes",
      choices = unique(dat$scheme),
      selected = paste0("Scheme ", 1:3),
      multiple = TRUE
    ),
    shiny::selectInput(
      inputId = "mitigator_groups",
      label = "Select mitigator group",
      choices = unique(dat$mitigator_group),
      selected = "Group X",
      multiple = FALSE
    ),
    shiny::selectInput(
      inputId = "mitigators",
      label = "Select mitigators",
      choices = dat$mitigator,
      selected = paste("Mitigator", LETTERS[1:3]),
      multiple = TRUE
    ),
    shiny::checkboxInput(
      inputId = "toggle_horizon",
      label = "Standardise by horizon length?",
      value = FALSE
    ),
    shiny::checkboxInput(
      inputId = "toggle_invert_facets",
      label = "Facet by scheme?",
      value = FALSE
    )
  ),
  bslib::nav_panel(
    title = "Tab 1",
    bslib::navset_card_underline(
      bslib::nav_panel(
        title = "Plot",
        shiny::plotOutput("p")
      ),
      bslib::nav_panel(
        title = "Panel 2",
        htmltools::p("Placeholder text (Tab 1, Panel 2).")
      )
    )
  ),
  bslib::nav_panel(
    title = "Tab 2",
    bslib::navset_card_underline(
      bslib::nav_panel(
        "Panel 1",
        htmltools::p("Placeholder text (Tab 2, Panel 1).")
      ),
      bslib::nav_panel(
        "Panel 2",
        htmltools::p("Placeholder text (Tab 2, Panel 2).")
      )
    )
  ),
  bslib::nav_spacer(),
  bslib::nav_item(bslib::input_dark_mode(mode = "light"))
)

server <- function(input, output, session) {
  
  dat_selected <- reactive({
    
    if (input$toggle_horizon) {
      dat <- dat |> dplyr::mutate(dplyr::across(c(lo, hi, mid), \(x) x / years))
    }
    
    dat |>
      dplyr::filter(
        scheme %in% input$schemes,
        mitigator %in% input$mitigators
      )
    
  })
  
  shiny::observeEvent(input$mitigator_groups, {
    
    mitigator_group_set <- dat |>
      dplyr::filter(mitigator_group == input$mitigator_groups) |>
      dplyr::distinct(mitigator) |>
      dplyr::pull()
    
    shiny::updateSelectInput(
      session,
      "mitigators",
      choices = unique(dat$mitigator),
      selected = mitigator_group_set
    )
    
  })
  
  output$p <- shiny::renderPlot({
    
    p <- dat_selected() |> ggplot2::ggplot()
    
    if (!input$toggle_invert_facets) {
      p <- p +
        ggplot2::geom_pointrange(
          ggplot2::aes(x = mid, y = scheme, xmin = lo, xmax = hi)
        ) +
        ggplot2::facet_grid(~mitigator)
    }
    
    if (input$toggle_invert_facets) {
      p <- p +
        ggplot2::geom_pointrange(
          ggplot2::aes(x = mid, y = mitigator, xmin = lo, xmax = hi)
        ) +
        ggplot2::facet_grid(~scheme)
    }
    
    p +
      ggplot2::labs(x = "80% Confidence Interval") +
      ggplot2::theme_bw(base_size = 20) +
      ggplot2::theme(axis.title.y = ggplot2::element_blank())
    
  })
  
}

shiny::shinyApp(ui, server)