library(ggplot2)
library(ggthemes)


# Table Formatters --------------------------------------------------------
polish_latex_table <- function(
    tbl,
    midrule_gap = "0.8em",
    tabcolsep = "10pt"
) {
  x <- as.character(tbl)

  x <- gsub(
    "\\\\midrule(\\s*\\\\addlinespace\\[[^]]*\\])+",
    "\\\\midrule",
    x
  )

  x <- gsub(
    "\\\\midrule",
    paste0("\\\\midrule\n\\\\addlinespace[", midrule_gap, "]"),
    x
  )

  setlen <- paste0("\\setlength{\\tabcolsep}{", tabcolsep, "}")
  anchor <- if (grepl("\\selectfont", x, fixed = TRUE)) {
    "\\selectfont"
  } else {
    "\\centering"
  }
  x <- sub(anchor, paste0(anchor, "\n", setlen), x, fixed = TRUE)
  stopifnot(grepl("tabcolsep", x, fixed = TRUE))
  x
}


fmt_num <- function(x, digits = TABLE_DIGITS) {
  x <- round(x, digits)
  out <- formatC(abs(x), format = "f", digits = digits, big.mark = ",")
  out <- gsub(",", "\\\\,", out)
  ifelse(x < 0, paste0("$-$", out), out)
}


# Font Family  ------------------------------------------------------------
get_base_family <- function(
    preferred = "Libertinus Serif",
    fallback = "Times"
) {
  families <- unique(systemfonts::system_fonts()$family)
  idx <- which(tolower(families) == tolower(preferred))
  if (length(idx) > 0) families[idx[1]] else fallback
}

BASE_FAMILY <- get_base_family()


# ggplot2 customized theme for publications -------------------------------
theme_publication <- function(
    base_size = 14,
    base_family = BASE_FAMILY,
    axes = c("box", "float")
) {
  axes <- match.arg(axes)

  base <- ggthemes::theme_foundation(
    base_size = base_size,
    base_family = base_family
  ) +
    theme(
      plot.title = element_text(
        face = "bold",
        size = rel(1.2),
        hjust = 0.5,
        margin = margin(0, 0, 20, 0)
      ),
      axis.ticks = element_line(linewidth = 0.5),
      axis.text = element_text(face = "plain"),
      axis.title = element_text(face = "bold", size = rel(0.9)),
      axis.title.y = element_text(angle = 90, vjust = 2),
      axis.title.x = element_text(vjust = -0.2),
      panel.grid = element_blank(),
      panel.spacing.x = unit(0.75, "cm"),
      panel.border = element_rect(linewidth = 1),
      panel.background = element_rect(colour = NA, fill = "white"),
      plot.background = element_rect(colour = NA, fill = "white"),
      plot.margin = unit(c(2, 3, 2, 3), "mm"),
      legend.box = "vertical",
      legend.position = "right",
      legend.direction = "vertical",
      legend.title = element_blank(),
      legend.key = element_rect(colour = NA),
      legend.key.width = unit(0.75, "cm"),
      legend.key.height = unit(1.0, "cm"),
      legend.key.spacing.y = unit(0.3, "cm"),
      legend.text = element_text(size = rel(1.1)),
      strip.text = element_text(face = "bold"),
      strip.background = element_rect(colour = "#f0f0f0", fill = "#f0f0f0")
    )

  if (axes == "float") {
    base <- base +
      theme(
        panel.border = element_blank(),
        axis.line = element_line(linewidth = 0.5)
      )
  }
  base
}

# Axis guides for the floating look: the axis line stops at the first and
# last tick (range-frame style). Guides cannot live inside a theme, so add
# this to each plot.
float_axes <- function() {
  guides(
    x = guide_axis(cap = "both"),
    y = guide_axis(cap = "both")
  )
}


# Saving Figure -----------------------------------------------------------
save_figure <- function(
    plot,
    name,
    width,
    height,
    fig_path,
    family = BASE_FAMILY
) {
  path <- paste0(fig_path, name)

  ggplot2::ggsave(
    filename = paste0(path, ".pdf"),
    plot = plot,
    width = width,
    height = height,
    units = "in",
    dpi = 800,
    device = grDevices::cairo_pdf,
    family = family
  )

  ggplot2::ggsave(
    filename = paste0(path, ".eps"),
    plot = plot,
    device = grDevices::cairo_ps,
    width = width,
    height = height,
    units = "in",
    dpi = 800,
    family = family
  )

  ggplot2::ggsave(
    filename = paste0(path, ".tiff"),
    plot = plot,
    device = grDevices::tiff,
    width = width,
    height = height,
    units = "in",
    dpi = 800,
    type = "cairo",
    family = family,
    symbolfamily = cairoSymbolFont(family = family),
    compression = "lzw"
  )
}
