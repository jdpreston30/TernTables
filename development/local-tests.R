# Fix PATH so TeX binaries are available (needed in VS Code R terminal)
Sys.setenv(PATH = paste("/usr/local/bin", Sys.getenv("PATH"), sep = ":"))

# If you want to build and run the whole vignette locally
source("development/run-vignette/run-vignette.R")

# If you want to build the manual document locally, run the fillowing in the terminal (non-R)
Rscript -e "devtools::build_manual(pkg = '.', path = 'development/manual')"