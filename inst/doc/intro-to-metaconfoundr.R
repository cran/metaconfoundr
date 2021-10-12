## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 12,
  fig.height = 6
)

library(metafor)

## ----setup--------------------------------------------------------------------
library(metaconfoundr)

# for later examples
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)

metaconfoundr(ipi)

## -----------------------------------------------------------------------------
ipi_wide

metaconfoundr(ipi_wide)

## -----------------------------------------------------------------------------
mc_ipi <- metaconfoundr(ipi)

## ---- fig.width=12------------------------------------------------------------
mc_heatmap(mc_ipi)

## -----------------------------------------------------------------------------
mc_trafficlight(mc_ipi)

## -----------------------------------------------------------------------------
wrap_labeller <- function(x) stringr::str_wrap(x, 10)

mc_heatmap(mc_ipi) + 
  facet_constructs(labeller = as_labeller(wrap_labeller)) + 
  theme_mc() + 
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
    strip.text = element_text(face = "bold")
  )

## -----------------------------------------------------------------------------
mc_trafficlight(mc_ipi) + 
  geom_cochrane() + 
  scale_fill_cochrane() + 
  theme_mc() + 
  guides(x = guide_axis(n.dodge = 3)) # dodge axis text rather than rotate

## -----------------------------------------------------------------------------
mc_heatmap(mc_ipi, sort = TRUE) + 
  theme_mc() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),)

## -----------------------------------------------------------------------------
summary_df <- summarize_control_quality(
  metaconfoundr(ipi),
  Sociodemographics = `Maternal age` & `Race/ethnicity` & `Marital status`,
  Socioeconomics = `SES category` | Insurance & Education,
  "Reproductive Hx" = `Prior pregnancy outcome`
)

summary_df

## ---- fig.width=5-------------------------------------------------------------
mc_heatmap(summary_df) +
  theme_mc() + 
  theme(legend.position = "right") +
  guides(x = guide_axis(n.dodge = 2))

## ---- fig.width=8-------------------------------------------------------------
sort_by_year <- function(.df) {
  .df %>% 
    arrange(desc(year), desc(study)) %>% 
    mutate(study = forcats::fct_inorder(study))
}

forest_plot <- function(.df) {
   .df %>% 
    sort_by_year() %>% 
    # set small weight for missing sample size
    mutate(sample_size = ifelse(is.na(sample_size), 1, sample_size)) %>% 
    ggplot(aes(x = estimate, y = study)) + 
    #  add effect estimates
    geom_point(aes(size = sample_size), shape = 15) +
    geom_errorbarh(aes(xmin = lower_ci, xmax = upper_ci), height = 0) + 
    #  use a log10 transformed scale
    scale_x_log10() + 
    #  use a minumal scale with only vertical grid lines
    theme_minimal(14) +
    theme(
      axis.title.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank()
    ) + 
    labs(
      x = "Odds Ratio",
      size = "Sample Size"
    )
}

fp <- forest_plot(ipi_metaanalysis)

fp

## ---- fig.width=5-------------------------------------------------------------
tl_plot <- 
  mc_ipi %>% 
  summarize_control_quality(
    "Socio-\ndemo-\ngraphics" = `Maternal age` & `Race/ethnicity` & `Marital status`,
    "Socio-\neconomic\nFactors" = `SES category` | Insurance & Education,
    "Repro-\nductive Hx" = `Prior pregnancy outcome`
  ) %>% 
  left_join(ipi_metaanalysis, by = "study") %>% 
  sort_by_year() %>% 
  mutate(variable = stringr::str_wrap(variable, 10)) %>% 
  mc_trafficlight() + 
  geom_cochrane() + 
  scale_fill_cochrane() + 
  theme_mc() + 
  theme(legend.position = "right") +
  guides(x = guide_axis(n.dodge = 2)) + 
  facet_constructs()

tl_plot

## ---- fig.width=10------------------------------------------------------------
library(patchwork)

# forest plot
fp + theme(legend.position = "none") + 
  # traffic light plot
  tl_plot + theme(axis.text.y = element_blank(), legend.position = "none") +
  # make the FP thrice as as wide as the TLP
  plot_layout(widths = c(3, 1))

## -----------------------------------------------------------------------------
library(metafor)
ipi_metaanalysis %>% 
  left_join(summary_df %>% filter(variable == "overall"), by = "study") %>% 
  mutate(se = log(upper_ci) - log(estimate) / 1.96) %>% 
  group_by(control_quality) %>% 
  group_map(~rma(data = .x, yi = estimate, sei = se)) 

## ---- fig.width=5-------------------------------------------------------------
ipi %>%
  metaconfoundr() %>%
  plot_non_confounders(geom = ggplot2::geom_point)

