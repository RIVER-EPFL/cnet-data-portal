install.packages("renv")
install.packages("readr")
library(renv)
# Install dependencies from renv.lock file
renv::restore(prompt=FALSE)

# Requires a root user to install
hrbrthemes::import_roboto_condensed()
