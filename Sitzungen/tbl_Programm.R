## Load packages ----

library(tidyverse)
library(flextable)
library(quarto)

# Set table back-/foreground ----

ft_theme_light <- theme_colors_flextable(bg = "#ffffff", fg = "#5a5a5a")
ft_theme_dark <- theme_colors_flextable(bg = "#222222", fg = "#ffffff")

# tbl Sitzungsprogramm ----

## Load data ----

Liste_Sitzungen <-
  read.csv("../Sitzungen/AG_BZ_Sitzungsprogramme.csv") |>
  as_tibble() |>
  mutate(Datum = as.Date(Datum)) |>
  filter(!str_starts(Vortragstitel, "\\["))

## Set locale date format to German ----

Sys.setlocale("LC_TIME", "de_DE.UTF-8")

## Function ----

ft_Sitzungsprogramm <- function(Liste_Sitzungen, mode = c("light", "dark")) {
  mode <- match.arg(mode)
  ft_theme <- switch(mode, light = ft_theme_light, dark = ft_theme_dark)
  pause_bg <- switch(mode, light = "grey95", dark = "#333333")

  Liste_Sitzungen |>
    select(!row) |>
    mutate(
      Datum = format(Datum, "%A, %d.%m.%Y"),
      Vortragstitel = ifelse(
        str_detect(
          Vortragstitel,
          regex("diskussion", ignore_case = TRUE)
        ),
        toupper(Vortragstitel),
        Vortragstitel
      )
    ) |>
    as_grouped_data(groups = "Datum") |>
    as_flextable(hide_grouplabel = TRUE) |>
    ft_theme() |> ## early: sets bg/fg + autofit before manual overrides
    delete_part(part = "header") |>
    border_remove() |>
    bold(j = 1, , i = ~ !is.na(Datum), part = "body") |>
    hline(i = ~ !is.na(Datum), part = "body") |>
    height(i = ~ !is.na(Datum), height = 1, unit = "cm", part = "body") |>
    hrule(rule = "exact") |>
    valign(valign = "top", part = "body") |>
    valign(i = ~ !is.na(Datum), valign = "bottom", part = "body") |>
    # Styling für entfallene Vorträge
    style(
      i = ~ str_detect(
        Vortragstitel,
        regex("entfällt|entfallen", ignore_case = TRUE)
      ),
      pr_t = fp_text_default(
        color = "grey", # Grey font color
        strike = TRUE # Strikethrough
      )
    ) |>
    # Hintergrund für Pausen
    bg(
      i = ~ str_detect(Vortragstitel, regex("pause", ignore_case = TRUE)),
      bg = pause_bg,
      part = "body"
    )
}
