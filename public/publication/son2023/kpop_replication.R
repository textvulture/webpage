# Replication codes
# paper title: "Foreign Pop-Culture and Backlash"
# Journal of Cultural Economics (DOI: https://link.springer.com/article/10.1007/s10824-023-09475-w)
# this file created: 03-25-2023
# Byunghwan Son (bson3@gmu.edu; https://ben-son.netlify.app/)
# Data to be downloaded: https://ben-son.netlify.app/publication/son2023/


# packages

library(dplyr)
library(kableExtra)
library(ggplot2)
library(modelsummary)
library(gt)
library(ggrepel)
library(readr)
library(fixest)
library(ggiplot)
library(coefplot)

## Fig 1

load("Rdislike.Rdata")
Rdislike %>%
  mutate(Rdate = as.Date(rdate, "%d-%m-%y")) %>%
  ggplot(aes(x=Rdate,
             y=n_comment)) +
  geom_line(aes(color='darkgreen')) +
  geom_smooth(aes(color='darkblue'),
              alpha=0.25) +
  geom_bar(aes(x=Rdate,
               y=n_post,
               color='red'),
           stat="identity",
           fill='red',
           alpha=0.75) +
  geom_vline(xintercept = as.numeric(as.Date("2020-03-25")), 
             linetype="dashed", 
             color = "orange", 
             size=0.5,
             alpha=0.85) +
  annotate("text", 
           label="Pandemic News Surge", 
           x = as.Date("2020-03-15"), # tricky how this thing works
           y = 300,
           size=5,
           family="serif",
           color='orange') +
  labs(x="Date",
       y="",
       color="") +   # the killer one!!!!!!
  theme_minimal() +
  guides(color = guide_legend(override.aes = list(fill = "white"))) +
  scale_color_manual(labels = c("Smoothed", "Comments", "Posts"),
                     values = c('darkgreen', 'darkblue', 'red')) +
  scale_x_date(date_breaks = "2 month", date_labels = "%m-%y") +
  theme(
    legend.position = c(0.2, 0.8),
    legend.direction = "vertical",
    legend.text=element_text(size=11),
    axis.text=element_text(size=11),
    axis.title=element_text(size=11))



## fig 2

load("trend.Rdata")
load("news.Rdata")

ggplot(NULL, aes(x=Rdate)) +
  geom_line(data=trend,
            aes(y=cumul10,
                color='darkblue'),
            size=2) +
  geom_histogram(data = news, 
                 aes(x=Rdate,
                     color='grey',
                     size=NA),
                 alpha=0.5) +
  theme_minimal() +
  guides(color=guide_legend(" ")) +
  scale_color_manual(labels = c("Google Trend", "News"),
                     values = c('darkblue', 'grey')) +
  scale_x_date(date_breaks = "2 month", date_labels = "%m-%y") +
  theme(
    legend.position = c(0.1, 0.9),
    legend.direction = "vertical",
    legend.text=element_text(size=10),
    axis.text=element_text(size=10),
    axis.title=element_text(size=10)) +
  labs(x="Date",
       y="") 

## table 1 

load("Rdislike2.Rdata")

dislike_dd <-
  lm(log_com ~ log_com_t1 + treat + n_fan + n_stan + n_korea +  t + t3, 
     # korea is NA, says singularity 
     data=Rdislike2) 

dislike_dd2 <-
  lm(log_com ~ treat + n_fan + n_stan + n_korea +  t + t3, 
     # korea is NA, says singularity 
     data=Rdislike2) 

dislike_dd3 <-
  lm(log_com ~ log_com_t1 + log_com_t2 +treat + n_fan + n_stan + n_korea +  t + t3, 
     # korea is NA, says singularity 
     data=Rdislike2) 


interrupt <- list("benchmark" = dislike_dd,
                  "W/O LDV" = dislike_dd2,
                  "Two LDVs" = dislike_dd3)



relable <- c("log_com_t1" = "ln(comments)$_{t-1}$",
             "log_com_t2" = "ln(comments)$_{t-2}$",
             "treat" = "treatment",
             "n_fan" = "fan mentions",
             "n_stan" = "stan mentions",
             "n_korea" = "Korea mentions",
             "t" = "time",
             "t3" = "time since the treatment")

modelsummary(interrupt,
             escape = F,
             # statistic = NULL, 
             # estimate = "{estimate} ({std.error}) {stars}", statistic=NULL, # how to make SE side by side
             vcov = "robust", # robust SE
             coef_map = relable,
             coef_omit = "[^log_com_t1 | # ^ means NOT, so actually 'keep'
                 log_com_t2 |
                 treat |
                 n_fan |
                 n_stan |
             n_korea |
             t |
             t3]",
             stars =  c('*' = .1, '**' = .05),
             fmt = 3,
             title = "Interrupted Time-Series Analysis, r/WeHateKpop",
             gof_omit = 'DF|R2 Adj.|Std.Errors|F',
             notes="The dependent variable is the daily number of comments in the Subreddit. The results for the month dummy variables are abbreviated to spare space.",
             threeparttable = T) %>%
  add_header_above(c(" ", "(1)", "(2)", "(3)"))



## Fig 3 

## Bayesian Causal Impact for r/WeHateKpop.
## reference document: https://cran.r-project.org/web/packages/CausalImpact/vignettes/CausalImpact.html

if(! "CausalImpact" %in% installed.packages()){
  install.packages("CausalImpact")
}
library(CausalImpact)


pre.period <- c(1, 52)  
post.period <- c(53, 89)

Rdislike %>%
  mutate(log_com = log(n_comment)) %>%
  dplyr::select(log_com, korea, fan, stan, n_post)-> temp

ci <-  CausalImpact(temp, pre.period, post.period) 

plot(ci,
     c("pointwise", "cumulative")) 


## figure 4 and table 2 -- export

load("did_data.Rdata")


# For Regression Table

baseline <- lm(lncom ~ treat1*k,
               data = did_data)  

benchmark <- lm(lncom ~ treat1*k +
                  lncom_all +
                  lnpost_all +
                  llncul +
                  factor(month2), 
                data = did_data)    
summary(benchmark)

sub <- lm(lncom ~ treat1*k +
            lncom_all +
            lnpost_all +
            llncul +
            lnsub + 
            factor(month2), 
          data = did_data)    
summary(sub)

wfe <- lm(lncom ~ treat1*k +
            lncom_all +
            lnpost_all +
            llncul +
            factor(month2) + 
            factor(week2),
          data = did_data) 

summary(wfe)

nb <- glm.nb(dailycomment ~ treat1*k +
               lncom_all +
               lnpost_all +
               llncul +
               factor(month2),
             data = did_data)
summary(nb)


did_data %>%
  drop_na(rdate) %>% 
  drop_na(topic) %>%
  drop_na(dailypost) %>%
  ggplot(aes(y=lncom,
             x=date)) +
  geom_smooth(aes(color=topic,
                  fill=topic),
              alpha=0.5, se=FALSE)  +
  coord_cartesian(ylim=c(2.5, 5)) +
  geom_vline(xintercept = as.numeric(as.Date("2020-03-25")), 
             linetype="dashed", 
             color = "orange", 
             size=0.75,
             alpha=0.85) +
  annotate("text", 
           label="Pandemic News Surge", 
           x = as.Date("2020-03-15"), # tricky how this thing works
           y = 4.5,
           size=5,
           family="serif",
           color='orange') +
  theme_minimal() +
  labs(y="ln(total daily comments)",
       x = "date") +
  theme(legend.position = c(0.9, 0.1))

did <- list("Baseline" = baseline,
            "Benchmark" = benchmark,
            "Week FE" = wfe,
            "subscribers" = sub,
            "N.B." = nb)

relabel <- c("treat1" = "Treatment",
             "k" = "Kpop",
             "treat1:k" = "Treatment $\\times$ Kpop",
             "lncom_all" = "ln(No. of total comments)",
             "lnpost_all" = "ln(No. of total posts)",
             "llncul" = "ln(No. of cumul. comments)$_{t-1}$",
             "lnsub" = "ln(No. of subscribers)")

fe <- data.frame("Coefficients" = "Month Fixed Effect",
                 "Baseline" = "$\\checkmark$",
                 "Benchmark" = "$\\checkmark$",
                 "Week FE" = "$\\checkmark$",
                 "subscribers" = "$\\checkmark$",
                 "N.B." = "$\\checkmark$")	

modelsummary(did,
             escape = F,
             # statistic = NULL, 
             # estimate = "{estimate} ({std.error}) {stars}", # how to make SE side by side
             vcov = "robust", # robust SE
             coef_map = relabel,
             coef_omit = "factor",
             stars =  c('*' = .1, '**' = .05),
             fmt = 3,
             title = 'Difference-in-Difference Model, r/Cringetopia',
             gof_omit = 'DF|R2 Adj.|Std.Errors|F',
             add_rows = fe,
             notes = "\\\\justifying \\\\footnotesize OLS estimates with robust standard errors in the parantheses. The dependent variable is the daily number of comments (logged) in r/Cringetopia. The result for the month dummy variables are abbreviated to spare space.",
             threeparttable = T) %>%
  add_header_above(c(" ", "(1)", "(2)", "(3)", "(4)", "(5)"))


## figure 5


did_data3 <- did_data %>%
  mutate(treated = ifelse(k==1 & treat1==1, 1, 0))

event = lm(lncom ~ k*monthF +
             lncom_all +
             lnpost_all +
             llncul , 
           data = did_data)  


did_data %>%   mutate(period = week2 - 63) -> did_data3

est_did = feols(lncom ~ lncom_all +
                  lnpost_all +
                  llncul + i(period, k, ref=0) | k+week2, did_data3)



ggiplot(est_did, geom_style = 'ribbon', color='orange',
        main=" ", alpha=0.5, pt.pch=0.5) +
  theme_minimal() +
  coord_cartesian(ylim=c(-10, 20), xlim=c(-40, 40)) + 
  labs(x="Weeks, before and after the treatment",
       y="Treatment Effect (95% CI)") 


## figure 6

did_data %>%
  mutate(
    date = as.Date(rdate, "%d-%m-%y"),
    treatment = as_factor(treat1),
    #    llncul = lag(lncul, 1),
    monthF = factor(month2),
    Kpop = as_factor(topic),
    Q = as_factor(q)
  ) %>%
  mutate(
    treatment_10 = ifelse(date > "2020-01-15", 1, 0),
    treatment_4 = ifelse(date > "2020-02-26", 1, 0),
    treatment_3 = ifelse(date > "2020-03-04", 1, 0),
    treatment_2 = ifelse(date > "2020-03-11", 1, 0),
    treatment_1 = ifelse(date > "2020-03-18", 1, 0),
    treatment0 = ifelse(date > "2020-03-25", 1, 0),
    treatment1 = ifelse(date > "2020-04-01", 1, 0),
    treatment2 = ifelse(date > "2020-04-08", 1, 0),
    treatment3 = ifelse(date > "2020-04-15", 1, 0),
    treatment4 = ifelse(date > "2020-04-22", 1, 0),
    treatment10 = ifelse(date > "2020-06-03", 1, 0)
  ) -> did_data2

summary(benchmark <- lm(lncom ~ treatment*k +
                          lncom_all +
                          lnpost_all +
                          llncul +
                          factor(month2), 
                        data = did_data2)    
)

a0 <- lm(lncom ~ tr*k + lncom_all + lnpost_all +
           llncul + factor(month2), data = did_data2 %>% 
           mutate(tr = treatment_10)) 

a1 <- lm(lncom ~ tr*k + lncom_all + lnpost_all +
           llncul + factor(month2), data = did_data2 %>% 
           mutate(tr = treatment_4)) 
a2 <- lm(lncom ~ tr*k + lncom_all + lnpost_all +
           llncul + factor(month2), data = did_data2 %>% 
           mutate(tr = treatment_3)) 
a3 <- lm(lncom ~ tr*k + lncom_all + lnpost_all +
           llncul + factor(month2), data = did_data2 %>% 
           mutate(tr = treatment_2)) 
a4 <- lm(lncom ~ tr*k + lncom_all + lnpost_all +
           llncul + factor(month2), data = did_data2 %>% 
           mutate(tr = treatment_1)) 

a5 <- lm(lncom ~ tr*k + lncom_all + lnpost_all +
           llncul + factor(month2), data = did_data2 %>% 
           mutate(tr = treatment0)) 

a6 <- lm(lncom ~ tr*k + lncom_all + lnpost_all +
           llncul + factor(month2), data = did_data2 %>% 
           mutate(tr = treatment1)) 
a7 <- lm(lncom ~ tr*k + lncom_all + lnpost_all +
           llncul + factor(month2), data = did_data2 %>% 
           mutate(tr = treatment2)) 
a8 <- lm(lncom ~ tr*k + lncom_all + lnpost_all +
           llncul + factor(month2), data = did_data2 %>% 
           mutate(tr = treatment3)) 
a9 <- lm(lncom ~ tr*k + lncom_all + lnpost_all +
           llncul + factor(month2), data = did_data2 %>% 
           mutate(tr = treatment4)) 

a10 <- lm(lncom ~ tr*k + lncom_all + lnpost_all +
            llncul + factor(month2), data = did_data2 %>% 
            mutate(tr = treatment10))




multiplot(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10,
          #          predictors="tr:k",
          title = " ",
          innerCI = 0,
          lwdOuter = 1.5,
          alpha = 0.75,
          xlab = 'Coefficient of treat X K-pop',
          ylab = 'treatment timing',
          sort=c("alphabetical"),
          horizontal = T,
          by="Model", 
          coefficients = "tr:k") + 
  theme_minimal() + 
  theme(
    legend.title = element_blank(),
    legend.position = "none",
    axis.text=element_text(size=10),
    axis.title=element_text(size=10)) +
  scale_color_manual(values = c("green", "blue", "blue", "blue", "blue", "red", "blue", "blue", "blue", "blue", "green")) +
  scale_y_discrete(labels = c("t-10", "t-4", "t-3", "t-2", "t-1", "benchmark", "t+1", "t+2", "t+3", "t+4", "t+10"))
