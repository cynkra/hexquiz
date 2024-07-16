#' Generate random package author list
#'
#' One of the author is the maintainer
#'
#' @param pkg Package to search for in r-unviverse API.
#'
#' @keywords internal
#' @return list containing package name, authors and maintainer.
extract_rand_pkg_authors <- function(pkg) {
  # Get descr
  tmp <- get_pkg_from_runiverse(pkg)
  if (is.null(tmp$Author)) {
    return(tmp)
  }

  # Find 3 indexes excluding the maintainer
  authors <- process_pkg_authors(tmp$Author)
  if (length(authors) == 1) {
    return(NULL)
  }

  maintainer <- process_pkg_maintainer(tmp$Maintainer)
  maintainer_id <- which(grepl(maintainer, authors) == TRUE)
  seq_sample <- setdiff(1:length(authors), maintainer_id)
  sel <- if (length(authors) == 2) {
    c(seq_sample, maintainer_id)
  } else {
    c(sample(seq_sample, min(3, length(seq_sample))), maintainer_id)
  }

  # Return the random choices including the maintainer
  res <- authors[sample(sel, length(sel))]
  list(
    # Return only family + given name
    package = pkg,
    authors = res,
    maintainer = maintainer
  )
}

#' Get package info on r-universe
#'
#' @param pkg Package name as string.
#'
#' @keywords internal
#' @return A list of metadata.
#' @importFrom jsonlite fromJSON
get_pkg_from_runiverse <- function(pkg) {
  tmp <- try(fromJSON(sprintf("https://api.cran.dev/%s", pkg)), silent = TRUE)
  if (inherits(tmp, "try-error")) {
    return(NULL)
  }
  universe <- strsplit(tmp$home, sprintf("/%s$", pkg))[[1]][1]
  url <- sprintf("%s/api/packages/%s", universe, pkg)
  fromJSON(url)
}

process_pkg_maintainer <- function(el) {
  strsplit(el, " <")[[1]][1]
}

#' Only returns given and family name of a package author
#'
#' @param authors Vector of authors returned by the desc package.
#'
#' @keywords internal
#' @return character vector
process_pkg_authors <- function(authors) {
  tmp <- strsplit(authors, ",\\n")[[1]]
  if (length(tmp) > 1) {
    # Exclude cph and fnd
    to_exclude <- grep("(fnd|^http)", tmp)
    if (length(to_exclude)) tmp <- tmp[-to_exclude]
  }

  vapply(tmp, \(author) {
    strsplit(author, " \\[")[[1]][1]
  }, FUN.VALUE = character(1), USE.NAMES = FALSE)
}

#' Remove NULL element from list
#' @keywords internal
drop_nulls <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}

#' Get package names from GitHub
#'
#' @param url Location to read from.
#' @keywords internal
#' @return Vector package names.
#' @importFrom gh gh
get_pkgs_names <- function(endpoint = "/repos/rstudio/hex-stickers/git/trees/main?recursive=1") {
  res <- gh(sprintf("GET %s", endpoint))
  sub(
    "PNG/",
    "",
    grep("PNG/", vapply(res$tree, `[[`, "", "path"), value = TRUE)
  ) |>
    split_pkgs_names() |>
    exclude_incorrect_pkgs()
}

split_pkgs_names <- function(pkgs) {
  vapply(pkgs, \(pkg) {
    strsplit(pkg, "\\.")[[1]][1]
  }, FUN.VALUE = character(1), USE.NAMES = FALSE)
}

exclude_incorrect_pkgs <- function(pkgs) {
  pkgs[-c(
    grep("googlesheets", pkgs), # googlesheets4
    grep("great_table", pkgs), # gt
    grep("pipe", pkgs), # magrittr
    grep("plotnine", pkgs), # Python
    grep("rray", pkgs), # TBD
    grep("RStudio", pkgs), # Not a package
    grep("siuba", pkgs), # Python
    grep("webr", pkgs)
  )]
}

#' Blur image from selected list of packages
#'
#' @param pkgs Vector of package names.
#' @param path Path to images.
#' @keywords internal
#' @return Side effect: create blurred png files of input images.
#' @importFrom magick image_read_svg image_write image_blur
blur_pkgs_images <- function(pkgs, path = "inst/www/blurred") {
  if (!dir.exists(path)) dir.create(path)

  lapply(pkgs, \(pkg) {
    hex <- image_read_svg(sprintf("https://raw.githubusercontent.com/rstudio/hex-stickers/main/SVG/%s.svg", pkg))
    image_write(
      image_blur(hex, 100, 100),
      path = sprintf("%s/%s.png", path, pkg),
      format = "png"
    )
  })
}

#' Get description files
#'
#' That's actually just a subset of the description.
#' This function is used for pre-processing data and not intended
#' to be used by the app directly. The app uses the descriptions file
#' located in the path parameter.
#'
#' @param pkgs Vector of package names to write the description.
#' Default to \link{get_pkgs_names}.
#' @keywords internal
#' @importFrom parallel mclapply detectCores
get_descriptions <- function(pkgs = get_pkgs_names()) {
  descs <- drop_nulls(mclapply(pkgs, extract_rand_pkg_authors, mc.cores = detectCores() / 2))
  pad_authors(descs)
}

#' Write description file
#'
#' Save the description file to disk. Where the number of possible of authors is
#' less than four, the author list should be padded with random authors from
#' other packages.
#'
#' @param descs Vector of package names to write the description. Default to \link{get_descriptions}.
#' @param path Path to save descriptions list to.
#' @keywords internal
write_descriptions <- function(
    descs = get_descriptions(),
    path = "inst/www/descriptions.rds") {
  saveRDS(descs, file = path)
}

#' Update description file
#'
#' Read the current description and append description from the
#' input packages. Write back the same file.
#'
#' @param pkgs Vector of package names search for.
#' @param path Path to save descriptions list to.
#' @keywords internal
update_descriptions <- function(pkgs, path = "inst/www/descriptions.rds") {
  stopifnot(file.exists(path))
  desc <- readRDS(path)
  new <- get_descriptions(pkgs)
  write_descriptions(c(desc, new), path)
}

#' Update package description and images
#'
#' Query GitHub for package names. Compare
#' to local package folder and update only elements
#' that are missing in the local folder. Images are blurred and description
#' file is modified to add the new packages if any.
#'
#' @param path Path where current images are located
#'
#' @keywords internal
check_and_update_pkgs_hex <- function(path = "inst/www/blurred") {
  new <- get_pkgs_names()
  current <- sub(".png", "", list.files(path))

  # auto update
  to_update <- setdiff(new, current)
  if (length(to_update)) {
    # Handle descriptions
    descs <- get_descriptions(to_update)
    message(sprintf("Need to update: %s", paste(to_update, collapse = ", ")))
    if (!dir.exists(path)) {
      write_descriptions(descs)
    } else {
      update_descriptions(descs)
    }

    # Process images
    lapply(
      vapply(descs, `[[`, "", "package"),
      blur_pkgs_images,
      path = path
    )
  } else {
    message("Nothing to update")
  }
}

#' Pad a list with random authors
#'
#' Some packages do not have four authors in the DESCRIPTION file that can be
#' used for the quiz. This function checks the number of authors for a given
#' DESCRIPTION file and pads the the number of authors so that it always
#' equals four.
#'
#' @param descs list of package description files
#'
#' @keywords internal
pad_authors <- function(descs) {
  all_authors <- lapply(descs, function(x) c(x$authors, x$maintainer)) |>
    unlist() |>
    unique()

  purrr::map(descs, function(x) {
    num_authors <- length(x$authors)

    if (num_authors < 4) {
      additional_authors <- sample(all_authors, 4 - num_authors, replace = FALSE)
      x$authors <- c(x$authors, additional_authors)
    }
    return(x)
  })
}
