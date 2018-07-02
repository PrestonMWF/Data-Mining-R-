library(caret)
library(tidyverse)

data("GermanCredit")

training_data <- GermanCredit %>%
  select(Amount, Duration, Age)

train_lm <- lm(Amount ~., data = training_data)

model_sampler <- function(df_return){
    data_split <- createDataPartition(y = training_data$Amount, 
                                    p = .632, 
                                    list = F)
  
    train <- training_data[data_split,]
    test <- training_data[-data_split,]
    
    train_lm <- lm(Amount ~ ., data = train)
    
    model_summary <- data.frame(
      t(coef(train_lm)),
      R_squared = summary(train_lm)$r.squared,
      Holdout_R2 = cor(test$Amount, predict(train_lm, newdata = test)) ^ 2) %>%
      mutate(R2_percent_change = round((Holdout_R2 - R_squared) / R_squared * 100, 3)) %>%
      rename(Intercept = X.Intercept.)
    
    confidence_int <- data.frame(t(confint(train_lm))) %>%
      rownames_to_column(var = "Confidence_level") %>%
      rename(Intercept = X.Intercept.)
    
    if (df_return == "confint"){
      return(confidence_int)
    } else if (df_return == "model") {
      return(model_summary)
    }
}

#histogram all values using melt- facet plot
#density plot with both
set.seed(1017)
lm_samples <- as.data.frame(
  do.call(rbind, replicate(1000, model_sampler(df_return = "model"),
                           simplify = F))) %>%
  mutate(Sample = seq(training_data$Amount)) %>%
  select(Sample, everything())

set.seed(1017)
test <- as.data.frame(
  do.call(rbind, replicate(1000, model_sampler(df_return = "confint"),
                           simplify = F))) %>%
  mutate(Sample = rep(seq(training_data$Amount), each = 2)) %>%
  select(Sample, everything())

#coef means vs single model
lm_samples %>%
  select(-Sample, -R_squared, -Holdout_R2, -R2_percent_change) %>%
  map(mean) %>%
  unlist() %>%
  data.frame() %>%
  rownames_to_column(var = "coefficient") %>%
  rename(coef_mean = ".") %>%
  mutate(model_coef = coef(train_lm),
         difference = round(coef_mean - model_coef, 3),
         percent_diff = round(difference / model_coef * 100, 3))

#coef sd
lm_samples %>%
  select(-Sample) %>%
  map(sd) %>%
  unlist() %>%
  data.frame() %>%
  rownames_to_column(var = "coefficient") %>%
  rename(coefficient_sd = ".")
