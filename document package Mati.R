setwd(here::here())
devtools::document()
setwd("..")

devtools::install("PArankmi")

devtools::install_git("https://github.com/matifuentes2/PArankmi")
