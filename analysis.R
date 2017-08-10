#.libPaths("D:/R/win-library/3.2")


library(tidyverse) # for data munging
library(haven) # import and export 'SPSS', 'Stata' and 'SAS' Files
library(readxl) # import excel files
library(magrittr)
library(stringr)
library(ez) # for SPSS-style anovas. 
library(broom) # for collecting regressions

#### Load data
d_placement <- read_csv("data/data1.csv")  %>%
  filter(environmentCondition_VS_1to4_ES_5 %in% c(1,5)) 
d_pointing <- read_csv("data/data2.csv") %>%
  filter(environmentCondition_VS_1to4_ES_5 %in% c(1,5)) 

#### Data checks
d_pointing %>% 
  select(subjCode) %>% 
  unique() %>% 
  nrow()

d_placement %>% 
  select(subjCode) %>% 
  unique() %>% 
  nrow()

#### Tidy data
d_pointing %<>% 
  mutate(env_cond = ifelse(environmentCondition_VS_1to4_ES_5 == 1, "VS", "ES"))

d_pointing %<>% mutate(gender = ifelse(Gender_Male_1_Female_2 == 1, "male", "female"))

#### Run analysis
d_pointing$absoluteError_withoutOutliers <- as.numeric(d_pointing$absoluteError_withoutOutliers)
mean(is.na(d_pointing$absoluteError_withoutOutliers))

d_pointing$latency_withoutOutliers <- as.numeric(d_pointing$latency_withoutOutliers)
mean(is.na(d_pointing$latency_withoutOutliers))

### Inferential statistics
d_model <- d_pointing %>%
  filter(!is.na(latency_withoutOutliers)) %>%
  select(subjCode, latency_withoutOutliers, env_cond, currentPositionNumber, targetNumber)

corridor <- c(1, 2, 2, 3, 3, 4, 4)
d_model$corridor_distance <- abs(corridor[d_model$currentPositionNumber] - 
                                   corridor[d_model$targetNumber])
qplot(d_model$corridor_distance, binwidth = 1)  

filter(d_model, subjCode == 47) %>%
  group_by(corridor_distance) %>%
  summarise(n=n())

###
d_model$corridor_distance_factor <- factor(d_model$corridor_distance) # treated as discrete, inferred due to df.
mod <- ezANOVA(data = d_model, 
               dv = latency_withoutOutliers, 
               wid = subjCode, 
               between = env_cond, 
               within = corridor_distance_factor)
print(mod)

lm.beta <- function (MOD) {
  b <- summary(MOD)$coef[-1, 1]
  sx <- sd(MOD$model[[-1]])
  sy <- sd(MOD$model[[1]])
  beta <- b * sx/sy
  return(beta)
}

fit_model <- function (data_frame) {
  lm.r <- lm(latency_withoutOutliers ~ corridor_distance, data = data_frame)
  res <- tidy(lm.r)
  res[3,] <- NA
  res$term[3] <- "corridor_distance_standarizedBeta"
  res$estimate[3] <- lm.beta(lm.r)
  return(res)
}

mods <- d_model %>%
  group_by(subjCode, env_cond) %>%
  do(
   fit_model(.)
  ) %>%
  select(-std.error, -statistic, -p.value) %>%
  spread(term, estimate) %>%
  rename(intercept = `(Intercept)`)

mod_mean <- mods %>%
  group_by(env_cond) %>%
  summarise(intercept = mean(intercept)/1000, 
            corridor_distance = mean(corridor_distance)/1000, 
            corridor_distance_standarizedBeta = mean(corridor_distance_standarizedBeta), 
            se_dist = sd(corridor_distance) / sqrt(n()))

ggplot(mods %>% mutate(intercept=intercept/1000, corridor_distance=corridor_distance/1000), aes(x = 0, y = intercept, xend = 3, 
                                                                                                yend = intercept + 3*corridor_distance, 
                                                                                                col = env_cond)) + 
  geom_segment(lty = 2) + 
  scale_y_continuous(breaks=c(4,6,8,10,12,14,16,18))+
  geom_segment(data = mod_mean,  lty = 1, size = 2) + 
  scale_color_manual(values = c("orange","blue"))

###
mod_mean <- mods %>%
  group_by(env_cond) %>%
  summarise(intercept = mean(intercept),
            se_dist = sd(corridor_distance_standarizedBeta) / sqrt(n()), 
            corridor_distance_standarizedBeta = mean(corridor_distance_standarizedBeta))

ggplot(mod_mean, aes(x = env_cond, y = corridor_distance_standarizedBeta, fill = env_cond)) + 
  geom_bar(stat = "identity") + 
  geom_linerange(aes(ymin = corridor_distance_standarizedBeta - se_dist, 
                     ymax = corridor_distance_standarizedBeta + se_dist)) + 
  scale_fill_manual(values = c("orange","blue"))

###
es_coef_t <- t.test(mods$corridor_distance_standarizedBeta[mods$env_cond == "ES"])
es_coef_t

vs_coef_t <- t.test(mods$corridor_distance_standarizedBeta[mods$env_cond == "VS"])
as.numeric(vs_coef_t$estimate)

both_coef_t <- t.test(mods$corridor_distance_standarizedBeta[mods$env_cond == "VS"],
                      mods$corridor_distance_standarizedBeta[mods$env_cond == "ES"])
both_coef_t
