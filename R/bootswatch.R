# Bootswatch defaults ---------------------------------------------------------
#
# Mirrors what bslib::bs_themer() does: build the theme for the selected
# Bootswatch, read its values back with bs_get_variables(), and use them as the
# starting values for the sidebar controls. Edits are then applied on top (see
# brand_theme() in theme.R).

DEFAULT_BOOTSWATCH <- "flatly"

# Fonts offered in the dropdowns. The selected Bootswatch's own font is added
# to the list when it isn't one of these (see font_choices()).
base_fonts <- c("Arial", "Figtree", "Inter", "Open Sans")
heading_fonts <- c("Source Sans 3", "Figtree", "Inter")
code_fonts <- c("JetBrains Mono", "Fira Code", "monospace")

font_choices <- function(choices, current) union(choices, current)

# '"Lato", -apple-system, sans-serif' -> "Lato"
# Returns `fallback` for NA, "null", "inherit", or an unresolved var(--...).
first_font <- function(x, fallback = NA_character_) {
  if (length(x) != 1 || is.na(x)) {
    return(fallback)
  }
  x <- trimws(x)
  if (!nzchar(x) || x %in% c("null", "inherit") || grepl("^var\\(", x)) {
    return(fallback)
  }
  trimws(gsub("^\\s*['\"]?([^,'\"]+)['\"]?.*$", "\\1", x))
}

# Any CSS color -> "#RRGGBB" (what <input type="color"> and app.js expect)
css_hex <- function(x, fallback) {
  if (length(x) != 1 || is.na(x)) {
    return(fallback)
  }
  hex <- htmltools::parseCssColors(x, mustWork = FALSE)
  if (length(hex) != 1 || is.na(hex)) {
    return(fallback)
  }
  toupper(substr(hex, 1, 7))
}

# The plain bslib theme for a Bootswatch, with none of the user's edits.
# brand = FALSE keeps a _brand.yml in the working directory from being layered
# over the app's own look or over the values read from the Bootswatch.
bootswatch_theme <- function(bootswatch) {
  args <- list(version = 5, brand = FALSE)
  if (!identical(bootswatch, "default")) {
    args$bootswatch <- bootswatch
  }
  do.call(bslib::bs_theme, args)
}

# bs_get_variables() compiles Sass, so keep results for the life of the process.
.defaults_cache <- new.env(parent = emptyenv())

# Named like the fields of brand_state(), minus `bootswatch`.
bootswatch_defaults <- function(bootswatch) {
  hit <- .defaults_cache[[bootswatch]]
  if (!is.null(hit)) {
    return(hit)
  }

  # "bg"/"fg" are bslib aliases that resolve to the right Sass variable for
  # each Bootswatch, so there is no need to guess body-bg / body-color.
  v <- bslib::bs_get_variables(
    bootswatch_theme(bootswatch),
    c(
      "bg",
      "fg",
      "primary",
      "secondary",
      "font-family-base",
      "headings-font-family",
      "font-family-monospace",
      "font-family-sans-serif"
    )
  )

  # Bootstrap 5 stores some font stacks as CSS variables, e.g.
  # var(--bs-font-sans-serif). Resolve that one to the Sass variable behind it.
  sans <- v[["font-family-sans-serif"]]
  font_value <- function(x) {
    if (!is.na(x) && grepl("font-sans-serif", x, fixed = TRUE)) sans else x
  }

  base <- first_font(font_value(v[["font-family-base"]]), "system-ui")
  out <- list(
    background = css_hex(v[["bg"]], "#FFFFFF"),
    foreground = css_hex(v[["fg"]], "#212529"),
    primary = css_hex(v[["primary"]], "#0D6EFD"),
    secondary = css_hex(v[["secondary"]], "#6C757D"),
    base = base,
    headings = first_font(font_value(v[["headings-font-family"]]), base),
    mono = first_font(v[["font-family-monospace"]], "monospace")
  )
  .defaults_cache[[bootswatch]] <- out
  out
}

# A full brand state holding nothing but a Bootswatch's own values
brand_defaults_state <- function(bootswatch) {
  c(list(bootswatch = bootswatch), bootswatch_defaults(bootswatch))
}
