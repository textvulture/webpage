library(blogdown)
blogdown::install_hugo(force = TRUE)

setwd("/Users/bson3/Documents/GitHub/webpage/")
rmarkdown::render_site(encoding = 'UTF-8') # when the changes are not reflected
blogdown::serve_site()