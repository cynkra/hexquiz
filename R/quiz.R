quiz <- function(path = "inst/www/descriptions.rds") {
  # NOTE: from time to time it is good to renew the list of
  # images within "www/blurred" to update any new hex logo.
  # You need to call check_and_update_pkgs_hex()
  packages <- readRDS(path)
  return(sample(packages, 1)[[1]])
}
