## ---- echo = FALSE, results='hide', message = FALSE----------------------
suppressPackageStartupMessages({
  library("dplyr")
  library("ggplot2")
  library("pollstR")
})
knitr::opts_chunk$set(
  fig.path = 'inst/vign-src/figures/',
  comment = "#>",
  error = FALSE,
  tidy = FALSE)

## ----message=FALSE-------------------------------------------------------
library("ggplot2")
library("dplyr")
library("tidyr")

## ------------------------------------------------------------------------
slug <- "2016-general-election-trump-vs-clinton"

## ----elec_2016_polls_notrun, eval=FALSE----------------------------------
#  elec_2016_polls <- pollster_chart_data(slug)

## ---- elec_2016_polls_save, eval=FALSE, warning=FALSE, include=FALSE-----
#  # run this interactively to create the data file
#  elec_2016_polls <- pollster_chart_data(slug)
#  save(elec_2016_polls, file = "vignettes/children/elec_2016_polls.Rdata")

## ----elec_2016_polls_hide, include=FALSE---------------------------------
load("elec_2016_polls.Rdata")

## ----elec_2016_polls_glimpse---------------------------------------------
glimpse(elec_2016_polls)

## ----tidy_elec_2016_polls------------------------------------------------
elec_2016_polls_tidy <-
  elec_2016_polls %>%
  gather(choice, value, one_of("Clinton", "Trump", "Undecided", "Other")) %>%
  mutate(date = start_date +
           difftime(end_date, start_date, units = "days") / 2) %>%
  filter(!is.na(value))
glimpse(elec_2016_polls_tidy)

## ----elect_2016_polls_plot1----------------------------------------------
choice_colours <- c("Trump" = "#9A3E25", "Clinton" = "#156B90", "Other" = "#708259", "Undecided" = "#978F80")
scale_colour_elec_2016 <- function(...) {
  scale_colour_manual(values = choice_colours)
}
scale_fill_elec_2016 <- function(...) {
  scale_fill_manual(values = choice_colours)
}

plot_elec_2016_1 <-
  ggplot(elec_2016_polls_tidy, aes(x = date, y = value, colour = choice)) +
  geom_point() +
  scale_colour_elec_2016() +
  theme_minimal()
plot_elec_2016_1

## ----plot_elec_2016_loess------------------------------------------------
plot_elec_2016_1 +
  geom_smooth(aes(fill = choice), method = "loess") +
  scale_fill_elec_2016()

## ----elec_2016_est_notrun, eval = FALSE----------------------------------
#  elec_2016_est <- pollster_chart(slug)

## ----elec_2016_est_save, eval=FALSE, include=FALSE-----------------------
#  # run this interactively to create the data file
#  elec_2016_est <- pollster_chart(slug)
#  save(elec_2016_est, file = "vignettes/children/elec_2016_est.Rdata")

## ----elec_2016_est_hide, include = FALSE---------------------------------
load("elec_2016_est.Rdata")

## ----elec_2016_est-------------------------------------------------------
glimpse(elec_2016_est)

## ----plot_elec_2016_2----------------------------------------------------
plot_elec_2016_1 +
  geom_line(data = elec_2016_est[["estimates_by_date"]],
            mapping = aes(x = date, y = value, colour = choice))

