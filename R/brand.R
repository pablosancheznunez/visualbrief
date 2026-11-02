default_palette <- c("#0072B2", "#D55E00", "#007A5E", "#AA3377", "#7A6A00", "#332288")

#' Define a visual identity for charts and reports
#'
#' A brand is a small set of colours, a font family and an optional logo,
#' used by the plotting functions and [vb_report()].
#'
#' The default palette uses colour-blind-friendly hues (after Okabe and Ito),
#' darkened so that every colour reaches the 3:1 contrast that WCAG 2.2
#' (success criterion 1.4.11) requires for graphical elements on white. Check
#' your own palette with [vb_check_contrast()].
#'
#' @param name Name shown in report headers.
#' @param primary Main colour (headings, accents).
#' @param palette Colours for series and categories, in order.
#' @param background,text Page background and text colours.
#' @param font Font family for charts and reports (e.g. `"sans"`, `"serif"`
#'   or an installed family).
#' @param logo Optional path to a PNG or SVG logo embedded in reports.
#' @return An object of class `vb_brand`.
#' @seealso [vb_brand_yml()] to read a `_brand.yml` file.
#' @export
#' @examples
#' b <- vb_brand("Acme research", primary = "#1F4E5F",
#'               palette = c("#1F4E5F", "#B35900", "#007A5E"))
#' b
vb_brand <- function(name = "visualbrief", primary = palette[1], palette = default_palette,
                     background = "#FFFFFF", text = "#1D1C1A", font = "sans", logo = NULL) {
  cols <- c(primary = primary, background = background, text = text, palette)
  bad <- cols[!vapply(cols, is_colour, TRUE)]
  if (length(bad)) stop("Not valid colours: ", paste(bad, collapse = ", "), call. = FALSE)
  if (length(palette) < 2) stop("`palette` needs at least two colours.", call. = FALSE)
  if (!is.null(logo) && !file.exists(logo)) stop("Logo file not found: ", logo, call. = FALSE)
  structure(list(name = name, primary = hex(primary), palette = hex(palette),
                 background = hex(background), text = hex(text), font = font, logo = logo),
            class = "vb_brand")
}

is_colour <- function(x) {
  is.character(x) && length(x) == 1 && !inherits(try(grDevices::col2rgb(x), silent = TRUE), "try-error")
}

hex <- function(x) {
  m <- grDevices::col2rgb(x)
  grDevices::rgb(m[1, ], m[2, ], m[3, ], maxColorValue = 255)
}

#' @export
print.vb_brand <- function(x, ...) {
  cat("<vb_brand> ", x$name, "\n", sep = "")
  cat("  primary    ", x$primary, "\n  palette    ", paste(x$palette, collapse = " "),
      "\n  background ", x$background, "   text ", x$text, "\n  font       ", x$font, "\n", sep = "")
  if (!is.null(x$logo)) cat("  logo       ", x$logo, "\n", sep = "")
  invisible(x)
}

#' Read a brand from a `_brand.yml` file
#'
#' Reads the colours, base font and logo of a
#' [brand.yml](https://posit-dev.github.io/brand-yml/) file, the format shared
#' by Quarto and Shiny, so one identity serves charts, reports and websites.
#' Needs the 'yaml' package.
#'
#' @param path Path to the `_brand.yml` file.
#' @return A `vb_brand`.
#' @export
#' @examples
#' f <- tempfile(fileext = ".yml")
#' writeLines(c(
#'   "meta:",
#'   "  name: Acme research",
#'   "color:",
#'   "  palette:",
#'   "    navy: \"#1F4E5F\"",
#'   "    rust: \"#B35900\"",
#'   "    pine: \"#007A5E\"",
#'   "  primary: navy",
#'   "  background: \"#FFFFFF\"",
#'   "  foreground: \"#1D1C1A\""
#' ), f)
#' if (requireNamespace("yaml", quietly = TRUE)) vb_brand_yml(f)
#' unlink(f)
vb_brand_yml <- function(path) {
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("Reading _brand.yml needs the 'yaml' package: install.packages(\"yaml\").", call. = FALSE)
  }
  y <- yaml::read_yaml(path)
  pal <- unlist(y$color$palette %||% list())
  resolve <- function(v) if (is.null(v)) NULL else if (v %in% names(pal)) pal[[v]] else v
  named <- unlist(lapply(c("primary", "secondary", "tertiary", "success", "info", "warning", "danger"),
                         function(k) resolve(y$color[[k]])))
  palette <- unique(c(named, unname(pal)))
  if (length(palette) < 2) palette <- unique(c(palette, default_palette))
  name <- y$meta$name
  if (is.list(name)) name <- name$full %||% name$short
  font <- y$typography$base
  if (is.list(font)) font <- font$family
  logo <- y$logo
  if (is.list(logo)) logo <- logo$medium %||% logo$small %||% logo$large
  if (!is.null(logo)) logo <- file.path(dirname(path), logo)
  if (!is.null(logo) && !file.exists(logo)) logo <- NULL
  vb_brand(name = name %||% "visualbrief", primary = resolve(y$color$primary) %||% palette[1],
           palette = palette, background = resolve(y$color$background) %||% "#FFFFFF",
           text = resolve(y$color$foreground) %||% "#1D1C1A", font = font %||% "sans", logo = logo)
}

`%||%` <- function(a, b) if (is.null(a)) b else a

luminance <- function(col) {
  rgb <- grDevices::col2rgb(col) / 255
  rgb <- ifelse(rgb <= 0.03928, rgb / 12.92, ((rgb + 0.055) / 1.055)^2.4)
  as.numeric(c(0.2126, 0.7152, 0.0722) %*% rgb)
}

#' Contrast ratio between two colours
#'
#' WCAG 2.2 contrast ratio, from 1 (no contrast) to 21 (black on white).
#'
#' @param a,b Colours.
#' @return A number.
#' @export
#' @examples
#' vb_contrast("#0072B2", "white")
vb_contrast <- function(a, b) {
  la <- luminance(a)
  lb <- luminance(b)
  (max(la, lb) + 0.05) / (min(la, lb) + 0.05)
}

#' Check a brand against WCAG contrast requirements
#'
#' Text must reach 4.5:1 against the background (WCAG 2.2, criterion 1.4.3)
#' and graphical elements such as lines, bars and points 3:1 (criterion
#' 1.4.11). Colours that fail are hard to see for many readers, including
#' people with low vision.
#'
#' @param brand A `vb_brand`.
#' @return A data frame with one row per colour: its role, contrast ratio
#'   against the background, the ratio required and whether it passes.
#' @export
#' @examples
#' vb_check_contrast(vb_brand())
#' vb_check_contrast(vb_brand(palette = c("#E69F00", "#56B4E9", "#0072B2")))
vb_check_contrast <- function(brand = vb_brand()) {
  cols <- c(text = brand$text, primary = brand$primary,
            stats::setNames(brand$palette, paste0("palette_", seq_along(brand$palette))))
  required <- ifelse(names(cols) == "text", 4.5, 3)
  ratio <- vapply(cols, vb_contrast, numeric(1), b = brand$background)
  data.frame(role = names(cols), colour = unname(cols), background = brand$background,
             ratio = round(unname(ratio), 2), required = required, passes = unname(ratio) >= required,
             stringsAsFactors = FALSE)
}
