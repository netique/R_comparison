settings <-
  tabPanel(value = "settings",
           "Settings",
           p(style = "text-align:justify",
             "You can customize the estimate in two ways:"),
            
           fluidRow(column(
             3,
             
             selectInput(
               "inc_method",
               "Incidence",
               c("raw" = "raw", "decomposed" = "decomp")
             )), column(9, style = "text-align:justify",
             'Raw incidence is untreated daily incidence of positive tested patients, "decomposed" refers to the "Seasonal and Trend decomposition using Loess" (STL) algorithm which is iteratively applied in order to derive so-called "trend component" that is henceforth used on its own. Multiple seasonal periods are allowed.')),
           
             br(),
             
             fluidRow(column(3,
               
             selectInput(
               "si_method",
               "Serial interval distribution",
               c(
                 "parametric" = "par",
                 "uncertain" = "uncert",
                 "direct (MCMC-MH)" = "mcmc"
               ),
               "par"
             )),
             column(9, style = "text-align:justify",
             'Serial interval (SI) distribution option can be either "parametric", "uncertain" or "direct". "Parametric" option utilizes SI distribution approximated by truncated lognormal distribution with parameters from Du et al. (2020), "uncertain" takes uncertainty into account by integrating over a range of means and standard deviations of the SI (see Cori et al., 2013, for details), and "direct" method uses sample of SI distributions obtained with Metropolis-Hastings Markov Chain Monte Carlo algorithm directly from infector-infectee pairs data by Du et al. (2020). Please inspect the selected SI distribution(s) in the "Estimate" section (third plot).'
           )))