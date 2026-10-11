# Install package ==============================================================
required_packages <- c(
  "moments",
  "nortest",
  "fBasics"
)

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(
      pkg,
      repos = "https://cloud.r-project.org"
    )
  }
}