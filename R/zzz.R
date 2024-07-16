.onLoad <- function(libname, pkgname) {
  addResourcePath(
    "www",
    system.file(
      "www",
      package = "hexquiz"
    )
  )
}

.onUnload <- function(libname, pkgname) {
  removeResourcePath("www")
}
