estimate <-
  tabPanel(
    value = "est",
    "Estimate",

    fluidRow(
      column(
        9,
        h1(strong("Effective reproduction number in Czechia"),
          style = "margin-top: 0px;"
        ),
        h2("+ estimates based on hospitalized, critical and dead",
          style = "margin-top: 6px; margin-bottom: 24px"
        )
      )
    ),

    fluidRow(
      column(
        5,
        sliderInput("window", "Time window (days) over which the reproduction number is estimated",
          min = 7, value = 7, max = 30, step = 1, width = "500px"
        )
      ),
      column(
        3,
        selectInput("ts_source", label = "Time series sources", multiple = TRUE, choice = c(
          "Incidence (tested positive)" = "incidence",
          "Hospitalized" = "hospitalized",
          "Critical" = "critical",
          "Dead" = "dead"
        ), selected = c("incidence", "hospitalized", "critical", "dead"))
      ),
      column(4, br(),
             helpText("Select the source(s) you do not want to be displayed with a mouse click\nand press delete or backspace on your keyboard. To bring the source(s) back, click on the blank space and pick from the dropdown list.")),
    ),
    plotlyOutput("sources_plot", height = "500px"),

    h3("Time series lag (in days)"),
    fluidRow(
      column(3, sliderInput("inc_lag", label = "Incidence", min = 0, value = 0, max = 30, step = 1)),
      column(3, sliderInput("hosp_lag", label = "Hospitalization", min = 0, value = 0, max = 30, step = 1)),
      column(3, sliderInput("crit_lag", label = "Critical", min = 0, value = 0, max = 30, step = 1)),
      column(3, sliderInput("deaths_lag", label = "Deaths", min = 0, value = 0, max = 30, step = 1))
    ),


    h3("(Pseudo)-serial interval mean and SD"),
    fluidRow(
      column(
        3, h4("Incidence"),
        sliderInput("mean_si_inc", "Mean SI", min = 1.1, value = 5.2, max = 30, step = .1),
        sliderInput("std_si_inc", "SD SI", min = 1.1, value = 4.7, max = 30, step = .1)
      ),
      column(
        3, h4("Hospitalization"),
        sliderInput("mean_si_hosp", "Mean SI", min = 1.1, value = 5.2, max = 30, step = .1),
        sliderInput("std_si_hosp", "SD SI", min = 1.1, value = 4.7, max = 30, step = .1)
      ),
      column(
        3, h4("Critical"),
        sliderInput("mean_si_crit", "Mean SI", min = 1.1, value = 5.2, max = 30, step = .1),
        sliderInput("std_si_crit", "SD SI", min = 1.1, value = 4.7, max = 30, step = .1)
      ),
      column(
        3, h4("Deaths"),
        sliderInput("mean_si_deaths", "Mean SI", min = 1.1, value = 5.2, max = 30, step = .1),
        sliderInput("std_si_deaths", "SD SI", min = 1.1, value = 4.7, max = 30, step = .1)
      )
    ),



    h3("Data overview"),
    plotlyOutput("overview_plots", width = "auto", height = "900px"),
    dataTableOutput("merged_table")
    # uiOutput("r_title"),
    # fluidRow(column(12, withSpinner(
    #   plotlyOutput("plot_r")
    # )))
    #


    # inputs:
    # window size in days
    # mean & SD of SI for every source separately
    # time lag between inc/hosp/deaths
    #






    # h3(strong("Epidemic curve")),
    # fluidRow(column(12, withSpinner(
    #   plotlyOutput("plot_inc")
    # ))),
    # uiOutput("si_title"),
    # fluidRow(column(12, withSpinner(
    #   plotlyOutput("plot_si")
    # ))),

    # h3(strong("Table")),
    # fluidRow(column(12, DTOutput("table")), align = "center"),
    # br(),
    # fluidRow(column(
    #   12, downloadButton("download_table", "Download table")
    # ), align = "right"),
  )