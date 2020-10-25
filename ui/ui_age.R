age_risk <-
  tabPanel(
    value = "age",
    "Risk-adjusted counts",

    h1(strong("Risk-adjusted case counts & other age-related statistics"),
      style = "margin-top: 0px;"
    ),


    HTML('<p>The chart below visualizes the difference between a) <em>hospitalization and death risk-adjusted case counts</em> and b) <em>reported daily incidence of cases tested positive</em>. Time series are constrained to be equal on 1 Semptember 2020 (risk indices are just mere incidence this day). Hospitalization and death rate ratios are based on age and comes from <a href="https://www.cdc.gov/coronavirus/2019-ncov/covid-data/investigations-discovery/hospitalization-death-by-age.html">CDC rates ratios reported by age categories</a> (the base category is 18-29 year-olds). Those discrete ratio-age relationships were approximated by 4th and 8th order polynomial, respectively, and used as continuous in order to gain more "sensitivity" for within-category shifts. Thick lines represent rolling averages for previous 7 days.</p>'),
    HTML('<p>Note that age-related data comes from KHSs and are lagged behind the complete and most up-to-date ones (due to "validation" etc.). One of the plots below visualizes the discrepancy betweet KHSs’ and complete data. </p>'),
    h3(strong("Risk-adjusted and raw incidence diference")),
    h4(em("raw incidence and adjusted risk ratios constrained to be equal on 1 September 2020")),
    withSpinner(plotlyOutput("risks_inc")),
    fluidRow(column(3, offset = 9, selectInput("risks_inc_scale", "Choose the scale of the y-axis",
      choices = c("linear" = "linear", "pseudo-log10" = "log10")
    ), style = "white-space: nowrap;")),
    br(), br(),


    h3(strong("Risk-adjusted and raw incidence (simple counts)")),
    h4(em("raw incidence and adjusted risk ratios constrained to be equal on 1 September 2020")),
    withSpinner(plotlyOutput("risks_inc_counts")),
    fluidRow(column(3, offset = 9, selectInput("risks_inc_counts_scale", "Choose the scale of the y-axis",
                                               choices = c("linear", "log10")
    ), style = "white-space: nowrap;")),
    
    br(), br(),

    HTML('<p>Following plot provides raw incidence as reported by KHSs and the hospitalization and death risk <em>expressed in multiples of expected hospitalizations/deaths of 23.5-year-olds</em> (as expected given the distribution of incidence across age). Risk-adjusted indices are related to the reference category of youngs, no case counts are depicted. According to CDC, when a 85+ person is tested positive, the probability of his or her hospitalization is 13× higher than that of a young one (18-29 years old). One can thus infer that 13 young positive individuals constitute a comparable burden regarding hospitalizations. In other words, it takes 13 times more people "to produce" the same amout of hospitalized.</p>'),
    h3(strong("Risk-adjusted and raw incidence comparison")),
    withSpinner(plotlyOutput("inc_multiples")),
    fluidRow(column(3, offset = 9, selectInput("inc_multiples_scale", "Choose the scale of the y-axis",
                                               choices = c("linear", "log10")
    ), style = "white-space: nowrap;")),
    



    # p(
    #   "Following plots are based on many wonderful ideas and inspirational feedback from the members of the", a(href = "https://covid19cz.cz/", "COVID19CZ"),
    #   "initiative (namely Jakub Steiner). The simpliest one depicts absolute daily incidence (columns) distributed across positive cases' age (rows).",
    #   "A bit more complicated plots are also available, showing proportional distribution among age categories in particular week (Mon-Su) etc.",
    #   "All plots are downloadable in a publication-ready PDF format, accompanied with information such as data source timestamp etc. (see the About, or Estimate tabs for further details).",
    #   br(), br(),
    #   strong(em("THE SECTION IS UNDER CONSTRUCTION!"))
    # ),

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