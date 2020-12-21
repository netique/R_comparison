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
library(forecast)
library(shinythemes)
library(plotly)
library(shinyalert)
library(patchwork)
if (!require(mtaux)) {
  remotes::install_github("netique/mtaux")
} else {
  library(mtaux)
} # just theme


source("ui/ui_estimate.R", local = T, encoding = "UTF-8")
source("ui/ui_age.R", local = T, encoding = "UTF-8")
source("ui/ui_risk.R", local = T, encoding = "UTF-8")
# source("ui/ui_settings.R", local = T, encoding = "UTF-8")
source("ui/ui_about.R", local = T, encoding = "UTF-8")

ui <-
  navbarPage(
    id = "nav",
    windowTitle = "R estimate in Czechia",
    uiOutput("title"),
    # position = "fixed-top",
    # header = list(tags$style(type="text/css", "")),
    # theme = "bootstrap.css",
    selected = "est",

    estimate,
    risk,
    age,
    # settings,
    about,

    footer = list(
      HTML(
        '<div style = "clear: both; height: 112px;"></div>
         <div class = "panel-footer", style = "opacity: 1.00; z-index: 1000; position: fixed; right: 0;bottom: 0;left: 0;">
         <div class = "footer-title" style="padding-bottom: 6px;">$R_t$ in Czechia</div>
         <div class = "footer-copyright">&copy; 2020 Jan Netík | source freely available at <a href="https://github.com/netique/R_comparison">github.com/netique/R_comparison</a><br>kindly supported by <a href="https://www.olejann.net/research-against-covid-19/">"Testing, incentives, information: How to mobilize society’s resources against the pandemic"</a>, a TACR GAMA grant at CERGE-EI and Charles University</div>
         <script type="text/x-mathjax-config">
            MathJax.Hub.Config({
             tex2jax: {inlineMath: [["$","$"]]},
             showMathMenu: true
            });
         </script>'
      )
    )
  )