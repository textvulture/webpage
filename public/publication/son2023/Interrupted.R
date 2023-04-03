# interrupted time series analysis

## reference document: https://cran.r-project.org/web/packages/CausalImpact/vignettes/CausalImpact.html

### example video: https://www.youtube.com/watch?v=GTgZfCltMm8&ab_channel=BigThingsConference

# Need to figure out how to customize the plot.

if(! "CausalImpact" %in% installed.packages()){
  install.packages("CausalImpact")
}
library(CausalImpact)
library(haven) # for reading Dta data.
library(tidyverse)

############################

# model <- CausalImpact(data, pre.period, post.period)

# loading the dislike data

Rdislike <- read_dta("/Users/bson3/Google Drive/international political economy/kpop/reddit/data/Rdislike.dta")


# pre.period <- as.Date(c("2019-04-06", "2020-03-15"))
# post.period <- as.Date(c("2020-03-16", "2020-12-02"))

pre.period <- c(1, 52)  
post.period <- c(53, 89)
  
# Rdislike %>%
#  mutate(Rdate = as.Date(rdate, "%d-%m-%y")) %>%
#  mutate(y = as.numeric(n_comment)) %>%
#  select(Rdate, y) -> causalimpact 

Rdislike %>%
  mutate(log_com = log(n_comment)) %>%
  dplyr::select(log_com)-> temp

ci <-  CausalImpact(temp, pre.period, post.period) 
ci2 <-  CausalImpact(temp, pre.period, post.period, alpha=0.1) 

summary(ci)

plot <- plot(ci,
             c("pointwise", "cumulative")) 

plot2 <-  plot(ci2,
       c("pointwise"))






# the example in the document
