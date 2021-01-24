library(shiny)
library(jsonlite)
library(tidyverse)
library(magrittr)
library(shinycssloaders)
library(lubridate)
# remotes::install_github("annecori/EpiEstim")
library(EpiEstim)
library(DT)
# library(dqshiny)
library(httr)
library(rvest)
library(zoo)
library(shinythemes)
library(plotly)
library(patchwork)
if (!require(mtaux)) {
  remotes::install_github("netique/mtaux")
} else {
  library(mtaux)
} # just theme

# create handler
# handler <- function(data, ...){
#   jsonlite::fromJSON(data)
# }

# register with shiny
# shiny::registerInputHandler("tablify", handler, force = TRUE)

server <- function(input, output, session) {
  
  output$title <-
    renderUI({
      strong(withMathJax("$R_t$", " in Czechia"), style = "font-size:22px")
    })
  
inc <- reactive({
  fromJSON("https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19/nakaza.json")$data %>%
    transmute(
      date = as_date(datum),
      inc = prirustkovy_pocet_nakazenych
    ) %>%
    filter(date >= "2020-03-01") %>% as_tibble()
})


deaths <- reactive({
  fromJSON("https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19/nakazeni-vyleceni-umrti-testy.json")$data %>%
    transmute(
      date = as_date(datum),
      deaths = c(0, diff(kumulativni_pocet_umrti))
    ) %>%
    filter(date >= "2020-03-01") %>% as_tibble()
})


  
hosp <- reactive({
  hosp <- fromJSON("https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19/hospitalizace.json")$data # new API
  
  # no hospitalizations in API........
  # hosp_resp <- GET("https://onemocneni-aktualne.mzcr.cz/covid-19/prehled-hospitalizaci")
  
  # hosp_obj <- hosp_resp %>%
  #   content() %>%
  #   html_node("#js-hospitalization-table-data") %>%
  #   html_attr("data-table") %>% 
  #   fromJSON()
  # 
  # hosp <- hosp_obj$body %>% as_tibble() %>% set_names(hosp_obj$header$title)
  
  hosp %<>% transmute(
    date = ymd(datum),
    patients_in = pocet_hosp,
    patients_critical = stav_tezky,
    patients_asymptomatic = stav_bez_priznaku,
    patients_mild = stav_lehky,
    patients_moderate = stav_lehky
  )
  
  # # convert cummulative "discharge" to daily, with 0 first
  # hosp %<>% mutate(patients_out = c(0, diff(patients_out)))
  hosp
})
  
merged <- reactive({
  
  max_date <- Reduce(intersect, list(inc()$date, hosp()$date, deaths()$date)) %>% max(na.rm = TRUE) %>% as_date
  
  merged <- left_join(inc(), deaths()) %>% left_join(hosp()) %>% filter(date <= max_date)
  
  merged %<>% mutate(inc = lag(inc, input$inc_lag),
                     patients_in = lag(patients_in, input$hosp_lag),
                     patients_critical = lag(patients_critical, input$crit_lag),
                     patients_asymptomatic = lag(patients_asymptomatic, input$asymp_lag),
                     patients_mild = lag(patients_mild, input$mild_lag),
                     patients_moderate = lag(patients_moderate, input$mod_lag),
                     deaths = lag(deaths, input$deaths_lag)
                     )
  
  merged[is.na(merged)] <- 0
  merged
})
  

output$merged_table <- renderDT({
  merged() %>% datatable(
    rownames = FALSE,
    selection = "none",
    callback = JS('table.page("last").draw(false);'),
    style = "bootstrap4",
    options = list(
      dom = "tip",
      searching = FALSE,
      pageLength = 10
    )
  )
}, server = FALSE)



output$overview_plots <- renderPlotly({
  {merged() %>%
    pivot_longer(-date) %>%
    ggplot(aes(date, value)) +
    geom_line() +
    scale_x_date(date_breaks = "2 weeks", labels = function(x) {
      paste0(lubridate::day(x), "/", lubridate::month(x))
    }) +
    facet_wrap(~name, scales = "free_y", ncol = 1) +
    theme_minimal() +
    theme(legend.position = "bottom")} %>% 
    ggplotly()
})

t_start <- reactive({
  seq(2, nrow(merged()) - (input$window -1))
})

t_end <- reactive({
  t_start() + (input$window -1)
})


r_inc_data <- reactive({
  estimate_R(merged() %>% transmute(dates = date, I = inc),
    method = "parametric_si",
    config = make_config(list(
      mean_si = input$mean_si_inc,
      std_si = input$std_si_inc,
      t_start = t_start(),
      t_end = t_end()
    ))
  ) %>%
    plot("R") %>%
    ggplot_build() %$% .$plot$data %>%
    add_column(.before = 1, ts_source = "incidence")
})

r_hosp_data <- reactive({
  estimate_R(merged() %>% transmute(dates = date, I = patients_in),
             method = "parametric_si",
             config = make_config(list(
               mean_si = input$mean_si_hosp,
               std_si = input$std_si_hosp,
               t_start = t_start(),
               t_end = t_end()
             ))
  ) %>%
    plot("R") %>%
    ggplot_build() %$% .$plot$data %>%
    add_column(.before = 1, ts_source = "hospitalized")
})

r_asymp_data <- reactive({
  estimate_R(merged() %>% transmute(dates = date, I = patients_asymptomatic),
             method = "parametric_si",
             config = make_config(list(
               mean_si = input$mean_si_asymp,
               std_si = input$std_si_asymp,
               t_start = t_start(),
               t_end = t_end()
             ))
  ) %>%
    plot("R") %>%
    ggplot_build() %$% .$plot$data %>%
    add_column(.before = 1, ts_source = "hosp. asymptomatic")
})

r_mild_data <- reactive({
  estimate_R(merged() %>% transmute(dates = date, I = patients_mild),
             method = "parametric_si",
             config = make_config(list(
               mean_si = input$mean_si_mild,
               std_si = input$std_si_mild,
               t_start = t_start(),
               t_end = t_end()
             ))
  ) %>%
    plot("R") %>%
    ggplot_build() %$% .$plot$data %>%
    add_column(.before = 1, ts_source = "hosp. mild")
})

r_mod_data <- reactive({
  estimate_R(merged() %>% transmute(dates = date, I = patients_moderate),
             method = "parametric_si",
             config = make_config(list(
               mean_si = input$mean_si_mod,
               std_si = input$std_si_mod,
               t_start = t_start(),
               t_end = t_end()
             ))
  ) %>%
    plot("R") %>%
    ggplot_build() %$% .$plot$data %>%
    add_column(.before = 1, ts_source = "hosp. moderate")
})

r_crit_data <- reactive({
  estimate_R(merged() %>% transmute(dates = date, I = patients_critical),
             method = "parametric_si",
             config = make_config(list(
               mean_si = input$mean_si_crit,
               std_si = input$std_si_crit,
               t_start = t_start(),
               t_end = t_end()
             ))
  ) %>%
    plot("R") %>%
    ggplot_build() %$% .$plot$data %>%
    add_column(.before = 1, ts_source = "hosp. critical")
})

r_deaths_data <- reactive({
  estimate_R(merged() %>% transmute(dates = date, I = deaths),
             method = "parametric_si",
             config = make_config(list(
               mean_si = input$mean_si_deaths,
               std_si = input$std_si_deaths,
               t_start = t_start(),
               t_end = t_end()
             ))
  ) %>%
    plot("R") %>%
    ggplot_build() %$% .$plot$data %>%
    add_column(.before = 1, ts_source = "dead")
})
  
plot_data <- reactive({
  bind_rows(r_inc_data(), r_hosp_data(), r_asymp_data(), r_mild_data(), r_mod_data(),
            r_crit_data(), r_deaths_data()) %>% 
    filter(ts_source %in% input$ts_source) %>% 
    mutate(ts_source = ts_source %>% str_to_sentence())
})


output$sources_plot <- renderPlotly({
  
  
  p <- plot_data()
  
  line_text <-
    paste0(
      p$ts_source,
      "\nR: ",
      p$meanR %>% round(2),
      "\nWindow end: ",
      p$end,
      "\nLower CrI: ",
      p$lower %>% round(2),
      "\nUpper CrI: ",
      p$upper %>% round(2)
    )
  
  plt <- p %>% 
    ggplot(aes(end, meanR)) +
    geom_line(aes(col = ts_source, text = line_text, group = ts_source)) +
      geom_hline(yintercept = 1, linetype = "dashed", alpha = .55) +
      geom_ribbon(aes(fill = ts_source,
        ymin = lower,
        ymax = upper
      ), alpha = .15) +
      scale_x_date(date_breaks = "2 weeks", labels = function(x) {
        paste0(lubridate::day(x), "/", lubridate::month(x))
      }) +
      coord_cartesian(ylim = c(0, 3)) +
      theme_minimal() +
      labs(x = "time window end", y = "R(t)")
  
  plt %<>% ggplotly(tooltip = "text")
  
  plt$x$data %<>% modify(~ modify_at(., "name", ~ str_extract(.x, "[a-z|A-Z|.\ ]+")))

plt %>%
  layout(
    legend = list(x = .225, y = 1, orientation = "h"),
    hovermode = "compare"
  )
})


observeEvent(input$reset_ts_lags, {
  c(
    "inc_lag",
    "hosp_lag",
    "asymp_lag",
    "mild_lag",
    "mod_lag",
    "crit_lag",
    "deaths_lag"
  ) %>%
    map(~ updateSliderInput(session, .x, min = 0, value = 0, max = 30, step = 1))
})

observeEvent(input$reset_si, {
  c(
    "mean_si_inc",
    "mean_si_hosp",
    "mean_si_asymp",
    "mean_si_mild",
    "mean_si_mod",
    "mean_si_crit",
    "mean_si_deaths"
  ) %>%
    map(~ updateSliderInput(session, .x, min = 1.1, value = 5.2, max = 30, step = .1))

  c(
    "std_si_inc",
    "std_si_hosp",
    "std_si_asymp",
    "std_si_mild",
    "std_si_mod",
    "std_si_crit",
    "std_si_deaths"
  ) %>%
    map(~ updateSliderInput(session, .x, min = 1.1, value = 4.7, max = 30, step = .1))
})

source("server/age_risk.R", local = TRUE)

# url navigation ----------------------------------------------------------

observe({
  query <- parseQueryString(session$clientData$url_search)
  if (!is.null(query$nav)) {
    updateNavbarPage(session, "nav", selected = query$nav)
  }
})


}