
cdc_hospitalization_rate_ratio <- read_rds("cdc_hospitalization_rate_ratio.rds")
cdc_death_rate_ratio <- read_rds("cdc_death_rate_ratio.rds")
# cdc_ratios <- read_rds("cdc_ratios_approx.rds")

# comp_risk <- function(type, inp_age) {
#   lookup_vect <- ifelse(type == "hosp", cdc_hospitalization_rate_ratio, cdc_death_rate_ratio)
#   lookup_vect[inp_age]
# }

# only "validated" data from HKS!!!
  # khs_raw <-  fromJSON("https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19/osoby.json")
  
khs_raw <- readRDS("khs_raw.rds")

khs_updated <- reactive({
  khs_raw$modified %>%
    as_datetime(tz = "Europe/Prague") %>%
    strftime(usetz = TRUE)
})

khs <- reactive({
  khs_raw$data %>%
    transmute(
      date = ymd(datum),
      age = vek
      # sex = as.factor(pohlavi),
      # nuts = kraj_nuts_kod,
      # lau = okres_lau_kod,
      # inf_abroad = nakaza_v_zahranici,
      # country = nakaza_zeme_csu_kod
    ) %>%
    filter(date >= "2020-03-01") %>%
    as_tibble() %>%
    arrange(date) %>%
    mutate(
      week = fct_inorder(paste0(isoweek(date), "/", year(date))),
      age_group = case_when(
        age %in% 0:9 ~ "0-9",
        age %in% 10:19 ~ "10-19",
        age %in% 20:29 ~ "20-29",
        age %in% 30:39 ~ "30-39",
        age %in% 40:49 ~ "40-49",
        age %in% 50:59 ~ "50-59",
        age %in% 60:69 ~ "60-69",
        age %in% 70:79 ~ "70-79",
        age %in% 80:89 ~ "80-89",
        TRUE ~ "90+"
      ),
      cdc_age_group = case_when(
        age %in% 0:4 ~ "0-4",
        age %in% 5:17 ~ "5-17 ",
        age %in% 18:29 ~ "18-29",
        age %in% 30:39 ~ "30-39",
        age %in% 40:49 ~ "40-49",
        age %in% 50:64 ~ "50-64",
        age %in% 65:74 ~ "65-74",
        age %in% 75:84 ~ "75-84",
        TRUE ~ "85+"
      )
    ) %>%
    relocate(date, week, age, age_group)
})

# all avaiable data
khs_labs_raw <- reactive({
  fromJSON("https://onemocneni-aktualne.mzcr.cz/api/v2/covid-19/nakaza.json")
})

khs_labs_updated <- reactive({
  khs_labs_raw()$modified %>%
    as_datetime(tz = "Europe/Prague") %>%
    strftime(usetz = TRUE)
})

khs_labs <- reactive({
  khs_labs_raw()$data %>%
    as_tibble() %>%
    transmute(date = ymd(datum), inc_khs_labs = prirustkovy_pocet_nakazenych)
})

# last data timestamp
data_updated <- reactive({
  if (identical(khs_updated(), khs_labs_updated())) {
    paste("KHS & labs data both updated", kh_updated())
  } else {
    paste("data updated:", khs_updated(), "(KHS),", khs_labs_updated(), "(labs)")
  }
})

# aggregated khs data and khs_labs comparison
comparison <- reactive({
  comparison <- khs() %>%
    count(date, name = "inc_khs") %>%
    left_join(khs_labs())

  # khs_labs excess over khs only incidence
  comparison %>%
    mutate(diff = inc_khs_labs - inc_khs, diff_plus_one = diff + 1)
})


# weeks with complete (i.e. 7) observations
complete_weeks <- reactive({
  khs() %>%
    group_by(week) %>%
    transmute(n_days = n_distinct(date)) %>%
    filter(n_days == 7) %>%
    pull(week) %>%
    unique()
})


age_time_weeks <- reactive({
  khs() %>%
    filter(week %in% complete_weeks()) %>%
    ggplot(aes(week, age_group)) +
    geom_bin2d(drop = FALSE, binwidth = 1) +
    scale_fill_viridis_c() +
    scale_x_discrete(labels = function(x) {str_extract(x, ".*(?=/)")}) +
    coord_cartesian(expand = FALSE) +
    labs(
      title = "Weekly incidence by age categories in Czechia",
      subtitle = "based only on highly incomplete, so-called validated KHS data; only completely observed weeks are shown",
      y = "age group",
      caption = paste("data updated", khs_updated() %>% strftime(usetz = TRUE), " |  © 2020 Jan Netík, source at github.com/netique/corona")
    ) +
    mtaux::theme_mt(
      axis.ticks = element_line(color = "gray80"),
      plot.caption = element_text(face = "italic"),
      plot.title.position = "panel",
      plot.subtitle = element_text(margin = margin(b = 12, t = -3))
    )
})

output$age_time_weeks <- renderPlotly({
  (age_time_weeks() + ggtitle("")) %>%
    ggplotly() %>%
    # rangeslider() %>%
    plotly::config(displayModeBar = F)
})

output$age_time_weeks_down <- downloadHandler(
  filename = function() {
    "age_time_weeks.pdf"
  },
  content = function(file) {
    ggsave(file, plot = age_time_weeks(), device = cairo_pdf, width = 9.81, height = 6.76)
  }
)
# ggsave(here("plots", "age_time_weeks.png"), width = 9.81, height = 6.76)


# Jakub Steiner's idea, divide counts by total incidence,
# so the proportion of given age group in the given week is shown
age_prop_time_week <- reactive({
  khs() %>%
    count(week, age_group, name = "week_age_inc") %>%
    group_by(week) %>%
    mutate(week_inc = sum(week_age_inc), pct_age = week_age_inc / week_inc) %>%
    group_by(week) %>%
    # filter(week %in% complete_weeks() ) %>%
    ggplot(aes(week, age_group, fill = pct_age)) +
    geom_tile() +
    scale_x_discrete(labels = function(x) {str_extract(x, ".*(?=/)")}) +
    scale_fill_viridis_c() +
    coord_cartesian(expand = FALSE) +
    labs(
      title = "Distribution of weekly incidence among age groups in Czechia",
      subtitle = "based only on highly incomplete, so-called validated KHS data",
      x = "week (ISO 8601)",
      y = "age group",
      fill = "% of week",
      caption = paste("data updated", khs_updated(), " |  © 2020 Jan Netík, source at github.com/netique/corona")
    ) +
    mtaux::theme_mt(
      axis.ticks = element_line(color = "gray80"),
      panel.grid.major.y = element_line(color = "gray80"),
      plot.caption = element_text(face = "italic"),
      plot.title.position = "panel",
      plot.subtitle = element_text(margin = margin(b = 12, t = -3))
    )
})

output$age_prop_time_week <- renderPlotly({
  (age_prop_time_week() + ggtitle("")) %>%
    ggplotly() %>%
    plotly::config(displayModeBar = F)
})

output$age_prop_time_week_down <- downloadHandler(
  filename = function() {
    "age_prop_time_week.pdf"
  },
  content = function(file) {
    ggsave(file, plot = age_prop_time_week(), device = cairo_pdf, width = 9.81, height = 6.76)
  }
)

# ggsave(here("plots", "age_prop_time_week.png"), width = 9.81, height = 6.76)

# maximal granularity plot (year of age / days)
age_time_days <- reactive({
  khs() %>%
    ggplot(aes(date, age)) +
    geom_bin2d(drop = FALSE, binwidth = 1) +
    scale_x_date(date_breaks = "2 weeks", labels = function(x) {
      paste0(day(x), "/", month(x))
    }) +
    scale_y_continuous(n.breaks = 15) +
    xlab("day/month") +
    scale_fill_viridis_c() +
    coord_cartesian(expand = FALSE, ylim = c(NA, 100)) +
    mtaux::theme_mt(
      axis.ticks = element_line(color = "gray80"),
      plot.caption = element_text(face = "italic"),
      plot.title.position = "panel"
    )
})


labs_excess_patch <- reactive({
  comparison() %>%
    ggplot(aes(date, diff)) +
    geom_line() +
    scale_y_continuous(n.breaks = 3) +
    ylab("labs exc. inc.") +
    coord_cartesian(expand = FALSE) +
    mtaux::theme_mt(axis.text.x = element_blank(), axis.title.x = element_blank())
})


labs_excess <- reactive({
  comparison() %>%
    ggplot(aes(date, switch(input$lab_excess_scale,
      "log10" = diff_plus_one,
      "linear" = diff
    ))) + # for log
    geom_line() +
    {
      switch(input$lab_excess_scale,
        "log10" = scale_y_log10(n.breaks = 6),
        "linear" = scale_y_continuous()
      )
    } +
    scale_x_date(date_breaks = "2 weeks", labels = function(x) {
      paste0(day(x), "/", month(x))
    }) +
    xlab("day/month") +
    ylab(paste0("labs excess incidence", if (input$lab_excess_scale == "log10") {
      " [log10]"
    })) +
    coord_cartesian(expand = FALSE) +
    mtaux::theme_mt()
})

labs_excess_sep <- reactive({
  labs_excess() + labs(
    title = "Excess incidence reported by laboratories only",
    subtitle = 'shows the amount of so-called "unvalidated" data, without available age information',
    caption = paste(data_updated(), " |  © 2020 Jan Netík, source at github.com/netique/corona")
  ) + theme(plot.subtitle = element_text(margin = margin(b = 12, t = -3)))
})

output$labs_excess <- renderPlotly({
  labs_excess() %>%
    ggplotly() %>%
    plotly::config(displayModeBar = F)
})

output$labs_excess_incidence <- downloadHandler(
  filename = function() {
    switch(input$lab_excess_scale,
      "log10" = "labs_excess_incidence_log10.pdf",
      "linear" = "labs_excess_incidence.pdf"
    )
  },
  content = function(file) {
    ggsave(file, plot = labs_excess_sep(), device = cairo_pdf, width = 9.81, height = 6.76)
  }
)


patchwork <- reactive({
  labs_excess_patch() + age_time_days() + plot_layout(heights = c(1, 5))
})

res_patchwork <- reactive({
  patchwork() + plot_annotation(
    title = "Daily incidence by age in Czechia",
    subtitle = "only KHS data stratified by age available, excess incidence reported by labs shown in the upper plot",
    caption = paste(data_updated(), " |  © 2020 Jan Netík, source at github.com/netique/corona")
  ) &
    theme(
      plot.title = element_text(family = "Roboto", face = "bold", size = 16, colour = "grey30"),
      plot.subtitle = element_text(family = "Roboto Condensed", size = 11, colour = "grey30"),
      plot.caption = element_text(family = "Roboto Condensed", face = "italic", colour = "grey30")
    )
})


output$age_time_days <- renderPlotly({
  age_time_days() %>%
    ggplotly() %>%
    plotly::config(displayModeBar = F)
})
output$age_time_days_down <- downloadHandler(
  filename = function() {
    "age_time_days.pdf"
  },
  content = function(file) {
    ggsave(file, plot = res_patchwork(), device = cairo_pdf, width = 9.81, height = 6.76)
  }
)



# new risk ratios
risks_inc_main <- reactive({
  khs() %>%
    mutate(
      hospitalization_rate_ratio = cdc_hospitalization_rate_ratio[as.character(age)],
      death_rate_ratio = cdc_death_rate_ratio[as.character(age)]
    ) %>%
  # right_join(cdc_ratios, by = "age") %>%
  group_by(date) %>%
    mutate(
      hosp_risk = sum(hospitalization_rate_ratio), # sum of RR for given date
      death_risk = sum(death_rate_ratio),
      inc = n()
    ) %>%
    ungroup()
})

ref_date_ratios <- reactive({
  risks_inc_main() %>%
    filter(date == "2020-09-01") %>%
    mutate(
      p_hosp = hosp_risk / inc,
      p_death = death_risk / inc
    ) %>%
    distinct(across(c(p_hosp, p_death)))
})

risk_inc_comp <- reactive({
  risks_inc_main() %>%
    # mutate(
    #   hosp_risk = (hosp_risk - min(hosp_risk)) / diff(range(hosp_risk)), # nnormalized
    #   death_risk = (death_risk - min(death_risk)) / diff(range(death_risk)),
    #   inc = (inc - min(inc)) / diff(range(inc))
    # ) %>%
    # mutate(
    #   hosp_risk = hosp_risk - inc,
    #   death_risk = death_risk - inc
    # ) %>%
    # distinct(across(c(date, inc, hosp_risk, death_risk))) %>%
    # select(-inc) %>%
    mutate(
      p_hosp = ref_date_ratios() %>% pull(p_hosp),
      p_death = ref_date_ratios() %>% pull(p_death),
      n_adj_hosp = hosp_risk / p_hosp,
      n_adj_death = death_risk / p_death
    ) %>%
    mutate(
      n_adj_hosp = n_adj_hosp - inc,
      n_adj_death = n_adj_death - inc
    ) %>%
    distinct(across(c(date, inc, hosp_risk, death_risk, p_hosp, p_death, n_adj_hosp, n_adj_death))) %>%
    pivot_longer(contains("adj")) %>%
    group_by(name) %>%
    mutate(roll = rollmean(value, 7, fill = NA, align = "right")) %>%
    ungroup()
})

output$risks_inc <- renderPlotly({
  plt <- risk_inc_comp() %>%
    # mutate(value = case_when(input$risks_inc_scale == "linear" ~ value,
    #                          TRUE ~ value + 100),
    #        roll = case_when(input$risks_inc_scale == "linear" ~ roll,
    #                         TRUE ~ roll + 100)) %>% 
    ggplot(aes(date, value, col = name)) +
    geom_line(size = .5, alpha = .25) +
    geom_line(size = .8, alpha = .8, aes(y = roll)) +
    scale_x_date(date_breaks = "2 weeks", labels = function(x) {
      paste0(day(x), "/", month(x))
    }) +
    {
      switch(input$risks_inc_scale,
             "log10" = scale_y_continuous(trans = scales::pseudo_log_trans(base = 10), n.breaks = 6, labels = scales::number_format()),
             "linear" = scale_y_continuous(labels = scales::number_format())
      )
    } +    scale_color_manual(values = c("inc" = "#619CFF", "n_adj_hosp" = "orange", "n_adj_death" = "red")) +
    labs(
      # title = "Age-adjusted infection flow (tested positive incidence subtracted)",
      x = "time [day/month]", y = "adjusted and raw incidence diference",
      caption = paste0("based on interpolated risk ratios by CDC; data from KHS updated:", data_updated(), "  |  © 2020 Jan Netík")
    ) +
    guides(colour = guide_legend(override.aes = list(size = 1.2))) +
    theme_minimal() +
    # coord_cartesian(xlim = c(ymd(NA, NA))) +
    theme_mt(
      legend.title = element_blank(),
      plot.title.position = "panel",
      plot.title = element_text(face = "bold"),
      legend.position = c(.15, .15),
      legend.background = element_rect(fill = "white", colour = NA)
    )
  plt %>%
    ggplotly() %>%
    layout(
      legend = list(x = .1, y = .85, orientation = "h")
    )
})


risk_inc_comp_counts <- reactive({
  risks_inc_main() %>%
  mutate(
    p_hosp = ref_date_ratios() %>% pull(p_hosp),
    p_death = ref_date_ratios() %>% pull(p_death),
    n_adj_hosp = hosp_risk / p_hosp,
    n_adj_death = death_risk / p_death
  ) %>%
    distinct(across(c(date, inc, hosp_risk, death_risk, p_hosp, p_death, n_adj_hosp, n_adj_death))) %>%
    pivot_longer(c(inc, contains("adj"))) %>%
    group_by(name) %>%
    mutate(roll = rollmean(value, 7, fill = NA, align = "right")) %>%
    ungroup()
})

output$risks_inc_counts <- renderPlotly({
  plt <- risk_inc_comp_counts() %>%
    ggplot(aes(date, value, col = name)) +
    geom_line(size = .5, alpha = .25) +
    geom_line(size = .8, alpha = .8, aes(y = roll)) +
    scale_x_date(date_breaks = "2 weeks", labels = function(x) {
      paste0(day(x), "/", month(x))
    }) +
    {
      switch(input$risks_inc_counts_scale,
             "log10" = scale_y_log10(n.breaks = 6, labels = scales::number_format()),
             "linear" = scale_y_continuous(labels = scales::number_format())
      )
    } +    scale_color_manual(values = c("inc" = "#619CFF", "n_adj_hosp" = "orange", "n_adj_death" = "red")) +
    labs(
      # title = "Age-adjusted infection flow (tested positive incidence subtracted)",
      x = "time [day/month]", y = "adjusted and raw incidence",
      caption = paste0("based on interpolated risk ratios by CDC; data from KHS updated:", data_updated(), "  |  © 2020 Jan Netík")
    ) +
    guides(colour = guide_legend(override.aes = list(size = 1.2))) +
    theme_minimal() +
    # coord_cartesian(xlim = c(ymd(NA, NA))) +
    theme_mt(
      legend.title = element_blank(),
      plot.title.position = "panel",
      plot.title = element_text(face = "bold"),
      legend.position = c(.15, .15),
      legend.background = element_rect(fill = "white", colour = NA)
    )
  plt %>%
    ggplotly() %>%
    layout(
      legend = list(x = .1, y = .85, orientation = "h")
    )
})











inc_multiples <- reactive({
  risks_inc_main() %>%
    distinct(across(c(date, inc, hosp_risk, death_risk))) %>%
    pivot_longer(-date) %>%
    group_by(name) %>% 
    mutate(roll = rollmean(value, 7, fill = NA, align = "right")) %>%
    ggplot(aes(date, value, col = name)) +
    geom_line(size = .5, alpha = .25) +
    geom_line(size = .8, alpha = .8, aes(y = roll)) +
    scale_x_date(date_breaks = "2 weeks", labels = function(x) {
      paste0(day(x), "/", month(x))
    }) +
    {
      switch(input$inc_multiples_scale,
             "log10" = scale_y_log10(n.breaks = 6, labels = scales::number_format()),
             "linear" = scale_y_continuous(labels = scales::number_format())
      )
    } +
    scale_color_manual(values = c("inc" = "#619CFF", "hosp_risk" = "orange", "death_risk" = "red")) +
    labs(
      # title = "Age-adjusted infection flow (tested positive incidence subtracted)",
      x = "time [day/month]", y = "incidence / 23.5-year-olds hosp./death rate equivalent",
      caption = paste0("based on interpolated risk ratios by CDC; data from KHS updated:", data_updated(), "  |  © 2020 Jan Netík")
    ) +
    guides(colour = guide_legend(override.aes = list(size = 1.2))) +
    theme_minimal() +
    # coord_cartesian(xlim = c(ymd(NA, NA))) +
    theme_mt(
      legend.title = element_blank(),
      plot.title.position = "panel",
      plot.title = element_text(face = "bold"),
      legend.position = c(.15, .15),
      legend.background = element_rect(fill = "white", colour = NA)
    )
})

output$inc_multiples <- renderPlotly({
  inc_multiples() %>%
    ggplotly() %>%
    layout(
      legend = list(x = .1, y = .75, orientation = "h")
    )
})