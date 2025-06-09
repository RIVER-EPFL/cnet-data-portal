install.packages("renv")
install.packages("readr")
library(renv)
# Install dependencies from renv.lock file
renv::restore(prompt=FALSE)

# Install additional packages for discharge tool
install.packages(c("pracma", "gridExtra", "signal"), repos="https://cran.r-project.org")

# Requires a root user to install
hrbrthemes::import_roboto_condensed()
