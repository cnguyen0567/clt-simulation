# Install package ==============================================================
required_packages <- c(
  "moments",
  "nortest",
  "fBasics",
  "ggplot2",
  "gt",
  "scales",
  "dplyr"
)

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(
      pkg,
      repos = "https://cloud.r-project.org"
    )
  }
}