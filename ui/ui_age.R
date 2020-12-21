age <-
  tabPanel(
    value = "age",
    "Incidence by age",

    h3(strong("Daily incidence by age")),
    h4(em('based solely on KHS data, as the "complete" ones does not provide such a granularity; the excess reported by the laboratories is provided below')),
    fluidRow(column(12, withSpinner(
      plotlyOutput("age_time_days")
    ))),
    fluidRow(column(11, downloadButton("age_time_days_down"), align = "right")),
    br(),

    h3(strong("Excess incidence reported by laboratories only")),
    h4(em('shows the amount of so-called "unvalidated" data, without available age information, you can opt for', HTML(paste0("log", tags$sub("10"))), "y-axis below")),
    fluidRow(column(12, withSpinner(
      plotlyOutput("labs_excess")
    ))),
    fluidRow(
      column(3, offset = 7, selectInput("lab_excess_scale", "Choose the scale of the y-axis", choices = c("linear", "log10")), style = "white-space: nowrap;"),
      column(1, downloadButton("labs_excess_incidence"), style = "margin-top: 25px;")
    ),
    br(),

    h3(strong("Weekly incidence by age categories")),
    h4(em("only completely observed weeks (Mon-Su) are shown")),
    fluidRow(column(12, withSpinner(
      plotlyOutput("age_time_weeks")
    ))),
    fluidRow(column(11, downloadButton("age_time_weeks_down"), align = "right")),
    br(),

    h3(strong("Distribution of weekly incidence among age groups in Czechia")),
    h4(em("proportion of age group per week")),
    fluidRow(column(12, withSpinner(
      plotlyOutput("age_prop_time_week")
    ))),
    fluidRow(column(11, downloadButton("age_prop_time_week_down"), align = "right")),
    br(),
  )