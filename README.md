# Effective reproduction number<br>based on several time series sources<a href='https://netique.shinyapps.io/R_comparison'><img src='r.svg' align="right" height="120" /></a>

Here resides the open-source code for [Shiny](https://shiny.rstudio.com/) application (written in R) showing an estimate of so-called effective reproduction number R(t). The number is normally estimated on daily incidence (i.e. cases tested positive per day), but as the epidemy tracking reaches its limits, this could be rather misleading. In order to avoid relying solely on this time series, we can estimate R(t) on several others, presumably much more robust ones - namely hospitalizations, critical cases and deaths per day.

## License
This program is free software and you can redistribute it and or modify it under the terms of the [GNU GPL 3](https://www.gnu.org/licenses/gpl-3.0.en.html).

## Financial support
This work is supported by the project "Testing, incentives, information: How to mobilize society’s resources against the pandemic" at CERGE-EI and Charles University, under the [TACR GAMA framework](https://www.tacr.cz/en/gama-programme/) and under auspices of the principal investigator [Ole Jann](https://www.olejann.net/).
