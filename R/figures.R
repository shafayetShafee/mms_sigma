library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

source(here::here("R/utils.R"))

# Configurations ----------------------------------------------------------
FIG_WIDTH <- 10
FIG_HEIGHT <- 5
FIG_FAMILY <- get_base_family()
FIG_PATH <- here::here("figures/")

CLIP_AT <- 5

# Heatmap for Grids of Direct and Indirect Effects ------------------------
grid <- expand.grid(
  DE = seq(-2, 2, length.out = 400),
  IE = seq(-2, 2, length.out = 400)
) |>
  mutate(
    TE = DE + IE,
    PM = IE / TE,
    L = abs(DE) + abs(IE),
    MMS = if_else(L == 0, NA_real_, abs(IE) / L),
    PM_display = pmin(pmax(PM, -CLIP_AT), CLIP_AT)
  )

grid_long <- grid |>
  select(DE, IE, PM_display, MMS) |>
  rename(PM = PM_display) |>
  pivot_longer(
    cols = c(PM, MMS),
    names_to = "measure",
    values_to = "value"
  ) |>
  mutate(
    measure = factor(
      measure,
      levels = c("PM", "MMS"),
      labels = c("PM", "MMS")
    )
  )

p_pm <- grid_long |>
  filter(measure == "PM") |>
  ggplot(aes(DE, IE, fill = value)) +
  geom_raster(interpolate = TRUE) +
  geom_abline(
    slope = -1,
    intercept = 0,
    linetype = "dashed",
    linewidth = 0.4
  ) +
  scale_fill_stepsn(
    colors = c("#2166ac", "white", "#b2182b"),
    limits = c(-CLIP_AT, CLIP_AT),
    n.breaks = 9,
    name = NULL,
    show.limits = TRUE,
    guide = guide_colorsteps(
      barwidth = unit(0.35, "cm"),
      barheight = unit(8.5, "cm"),
      title.position = "top"
    )
  ) +
  coord_equal(expand = FALSE) +
  facet_wrap(~measure) +
  labs(
    x = "DE",
    y = "IE",
    title = NULL,
    subtitle = NULL
  ) +
  theme_publication(base_size = 16) +
  theme(legend.position = "right")


p_mms <- grid_long |>
  filter(measure == "MMS") |>
  ggplot(aes(DE, IE, fill = value)) +
  geom_raster(interpolate = TRUE) +
  geom_abline(
    slope = -1,
    intercept = 0,
    linetype = "dashed",
    linewidth = 0.4
  ) +
  scale_fill_viridis_c(
    option = "viridis",
    limits = c(0, 1),
    direction = -1,
    breaks = seq(0, 1, 0.2),
    name = NULL,
    guide = guide_colorbar(
      barwidth = unit(0.35, "cm"),
      barheight = unit(8.5, "cm"),
    )
  ) +
  coord_equal(expand = FALSE) +
  facet_wrap(~measure) +
  labs(
    x = "DE",
    y = NULL,
    title = NULL,
    subtitle = NULL
  ) +
  theme_publication(base_size = 16) +
  theme(legend.position = "right")

p_heat <- p_pm + p_mms + plot_layout(ncol = 2, widths = c(1, 1))

p_heat

save_figure(
  plot = p_heat,
  name = "Figure-1",
  width = FIG_WIDTH,
  height = FIG_HEIGHT,
  fig_path = FIG_PATH,
  family = FIG_FAMILY
)


# Trajectory Graph --------------------------------------------------------
DE_FIXED <- 1

# Singularity of PM at TE = 0, i.e. IE = -DE
IE_SINGULAR <- -DE_FIXED

traj <- tibble(
  IE = seq(-2, 2, length.out = 800),
  DE = DE_FIXED,
  TE = DE + IE,
  PM = if_else(abs(TE) > 1e-3, IE / TE, NA_real_),
  L = abs(DE) + abs(IE),
  MMS = abs(IE) / L,
  branch = if_else(IE < IE_SINGULAR, "below", "above")
)

traj_long <- traj |>
  select(IE, branch, PM, MMS) |>
  pivot_longer(
    c(PM, MMS),
    names_to = "measure",
    values_to = "value"
  )

estimator_types <- c("PM", "MMS")

line_types <- c("42", "solid")

line_colors <- c(
  "PM" = "#b2182b",
  "MMS" = "#2166ac"
)

scale_linetype_values <- rlang::set_names(line_types, estimator_types)

p_traj <- traj_long |>
  ggplot(aes(
    IE,
    value,
    linetype = measure,
    color = measure,
    group = interaction(measure, branch)
  )) +
  geom_hline(yintercept = 0, color = "grey70", linewidth = 0.3) +
  geom_hline(yintercept = 1, color = "grey70", linewidth = 0.3) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "grey40") +
  geom_vline(xintercept = IE_SINGULAR, linetype = "dashed", color = "grey40") +
  geom_line(linewidth = 0.8, na.rm = TRUE) +
  coord_cartesian(ylim = c(-5, 5)) +
  scale_x_continuous(breaks = seq(-2, 2, by = 0.5)) +
  scale_y_continuous(breaks = seq(-5, 5, by = 1)) +
  scale_linetype_manual(
    name = "Measure",
    values = scale_linetype_values,
    breaks = estimator_types
  ) +
  scale_color_manual(
    name = "Measure",
    values = line_colors,
    breaks = estimator_types
  ) +
  guides(
    linetype = guide_legend(title = "Measure"),
    color = guide_legend(title = "Measure")
  ) +
  labs(
    title = NULL,
    subtitle = NULL,
    x = "IE",
    y = "Value"
  ) +
  annotate(
    geom = "text",
    x = 1.2,
    y = 3.5,
    label = "Consistent region\nwith PM = MMS",
    size = 4.5
  ) +
  annotate(
    geom = "text",
    x = -1.5,
    y = -3.5,
    label = "Inconsistent region\nPM diverges,\nMMS stays in [0,1]",
    size = 4.5
  ) +
  annotate(
    geom = "text",
    x = IE_SINGULAR - 0.1,
    y = -1,
    label = "TE = 0",
    angle = 90,
    size = 4.5
  ) +
  theme_publication(base_size = 16) +
  theme(
    legend.position = "right",
    legend.key.spacing.y = unit(0.1, "cm"),
    legend.key.height = unit(0.80, "cm"),
    legend.key.width = unit(1.2, "cm"),
    legend.text = element_text(size = rel(0.9))
  )

p_traj

save_figure(
  plot = p_traj,
  name = "Figure-2",
  width = FIG_WIDTH,
  height = FIG_HEIGHT,
  fig_path = FIG_PATH,
  family = FIG_FAMILY
)
