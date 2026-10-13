# ============================================================
# SCRIPT NICHT AKTIV, muss ggf. leicht angepasst werden
# Ausgabe der SprecherInnen & Beiräte als flextable-Objekte aus CSV-Tabellen.
# Die CSV-Tabellen müssen im Ordner `Sprecher_Beirat` erstellt werden:
# - tbl Sprecher_Beirat, Spalten "Funktion, Name, Kontakt, Mail, Foto"
# - tbl Liste_Sprecher_Beirat, Spalten "Funktion, Name, von, bis"
# In Sprecher_Beirat.qmd müssen die jeweiligen MD-Tabellen entfernt und ent-
# sprechende Code-Chunks (siehe Ende des Scripts) eingefügt werden.
# ============================================================

## Load packages ----

library(tidyverse)
library(flextable)
library(quarto)

# Set table back-/foreground ----

#source bg/fg
source("../Sitzungen/tbl_Programm.R")

# tbl Sprecher Beirat ----

## Load data ----

Sprecher_Beirat <-
  read.csv("../Sprecher_Beirat/AG_BZ_Sprecher_Beirat.csv")

## Function ----

ft_Sprecher_Beirat <- function(Liste, mode = c("light", "dark")) {
  mode <- match.arg(mode)
  ft_theme <- switch(mode, light = ft_theme_light, dark = ft_theme_dark)
  link_color <- switch(mode, light = "#78c2ad", dark = "#00bc8c") # site's bs-link-color (minty/darkly)

  photo_width <- 1.2

  Liste |>
    flextable() |>
    compose(
      i = ~ Foto != "",
      j = "Foto",
      value = as_paragraph(as_image(
        src = Foto,
        width = photo_width,
        height = photo_width * 4 / 3
      ))
    ) |>
    compose(
      j = "Kontakt",
      value = as_paragraph(
        as_chunk(paste0(Kontakt, "\n")),
        hyperlink_text(
          x = Mail,
          url = paste0("mailto:", Mail),
          props = officer::fp_text(color = link_color, underlined = TRUE)
        )
      )
    ) |>
    delete_columns(j = "Mail") |>
    valign(valign = "top", part = "body") |>
    bold(j = "Name", part = "body") |>
    delete_part(part = "header") |>
    theme_borderless() |>
    ft_theme() # sets bg/fg + autofit LAST; explicit link props above survive this
}

# tbl Liste Sprecher Beirat ----

## Load data ----

Liste_Sprecher <-
  read.csv("../Sprecher_Beirat/AG_BZ_Liste_Sprecher_Beirat.csv") |>
  filter(Funktion == "Sprecher") |>
  select(!Funktion)

Liste_Beirat <-
  read.csv("../Sprecher_Beirat/AG_BZ_Liste_Sprecher_Beirat.csv") |>
  filter(Funktion == "Beirat") |>
  select(!Funktion)

## Function ----

ft_Liste_Sprecher_Beirat <- function(Liste, mode = c("light", "dark")) {
  mode <- match.arg(mode)
  ft_theme <- switch(mode, light = ft_theme_light, dark = ft_theme_dark)

  Liste |>
    flextable() |>
    autofit() |>
    colformat_num(j = 2:3, digits = 0, big.mark = "") |>
    set_header_labels(Name = "") |>
    theme_vanilla() |>
    hline_top(part = "header", border = officer::fp_border(width = 0)) |>
    ft_theme() # sets bg/fg for all cells; leaves theme_vanilla borders intact
}

# Code Chunks für Sprecher_Beirat.qmd ----

# ```{r}
# #| label: Listen & Funktionen laden
# source("../Sprecher_Beirat/tbl_Sprecher_Beirat.R")
# ```

# ```{r}
# #| label: SprecherInnen
# #| renderings: [light, dark]
# Sprecher <-
#   Sprecher_Beirat |>
#   filter(Position == "Sprecher") |>
#   select(Foto, Name, Kontakt, Mail)
# ft_Sprecher_Beirat(Sprecher, mode = "light")
# ft_Sprecher_Beirat(Sprecher, mode = "dark")
# ```

# ```{r}
# #| label: Beirat
# #| renderings: [light, dark]
# Beirat <-
#   Sprecher_Beirat |>
#   filter(Position == "Beirat") |>
#   select(Foto, Name, Kontakt, Mail)
# ft_Sprecher_Beirat(Beirat, mode = "light")
# ft_Sprecher_Beirat(Beirat, mode = "dark")
# ```

# ```{r}
# #| label: Liste SprecherInnen
# #| renderings: [light, dark]
# ft_Liste_Sprecher_Beirat(Liste_Sprecher, mode = "light")
# ft_Liste_Sprecher_Beirat(Liste_Sprecher, mode = "dark")
# ```

# ```{r}
# #| label: Liste Beiräte
# #| renderings: [light, dark]
# ft_Liste_Sprecher_Beirat(Liste_Beirat, mode = "light")
# ft_Liste_Sprecher_Beirat(Liste_Beirat, mode = "dark")
# ```
