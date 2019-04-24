library(tidyverse)
library(corrplot)
library(rpart)
library(caret)
library(randomForest)
library(nnet)
library(circular)
library(rattle)

theme_set(
  theme_minimal() +
    theme(strip.text.x = element_text(size = 14))
)

full_trees <- read.csv("fulltrees.csv")

set.seed(314)
data_split <- sample(x = c(1:50000), size = 50000, replace = F)

sampled_trees <- full_trees %>%
  slice(data_split) %>%
  mutate(Id = 1:50000,
         Cover_Type = as.factor(Cover_Type),
         Aspect = abs(Aspect - mean(as.circular(Aspect))),
         Ammenities_distance = (Horizontal_Distance_To_Fire_Points +
                                  Horizontal_Distance_To_Hydrology +
                                  Horizontal_Distance_To_Roadways) / 3,
         Hydrol_delta = Horizontal_Distance_To_Hydrology - Vertical_Distance_To_Hydrology) %>%
  select(Id, everything())

set.seed(314)
train_split <- createDataPartition(y = sampled_trees$Cover_Type, p = .7, list = F)

training <- sampled_trees %>%
  slice(train_split)

holdout <- sampled_trees %>%
  slice(-train_split)

soil_gather <- sampled_trees %>%
  select(1, 17:55) %>%
  gather(key = "Soil", value = "counts", -Id) %>%
  filter(counts == 1) %>%
  select(-counts)

wilderness_gather <- sampled_trees %>%
  select(Id, Wilderness_Area1, Wilderness_Area2, Wilderness_Area3, Wilderness_Area4) %>%
  gather(key = "Wilderness", value = "counts", -Id) %>%
  filter(counts == 1) %>%
  full_join(soil_gather) %>%
  select(-counts)

trees_transform <- sampled_trees %>%
  select(1:11, 56:58) %>%
  left_join(wilderness_gather) %>%
  mutate(Cover_Type = case_when(
    Cover_Type == 1 ~ "Spruce/Fir",
    Cover_Type == 2 ~ "Lodgepole_Pine",
    Cover_Type == 3 ~ "Ponderosa_Pine",
    Cover_Type == 4 ~ "Cottonwood/Willow",
    Cover_Type == 5 ~ "Aspen",
    Cover_Type == 6 ~ "Douglas-fir",
    Cover_Type == 7 ~ "Krummholz"))

trees_transform <- trees_transform %>%
  mutate(Soil = ifelse(is.na(Soil), "unknown", Soil))

#write.csv(trees_transform, "transformed_trees.csv", row.names = F)

str(trees_transform)

#univariate
quickplot(trees_transform$Elevation)

quickplot(trees_transform$Aspect)

quickplot(trees_transform$Horizontal_Distance_To_Hydrology)

quickplot(trees_transform$Vertical_Distance_To_Hydrology)

quickplot(trees_transform$Horizontal_Distance_To_Roadways)

trees_transform %>%
  select(Hillshade_9am, Hillshade_Noon, Hillshade_3pm) %>%
  gather(key = "time", value = "shade") %>%
  mutate(time = factor(time, levels = c("Hillshade_9am", 
                                        "Hillshade_Noon", 
                                        "Hillshade_3pm"))) %>%
  ggplot(aes(shade, fill = time)) +
  geom_histogram(bins = 35, show.legend = F) +
  facet_wrap(facets = "time")

quickplot(trees_transform$Horizontal_Distance_To_Fire_Points)

trees_transform %>%
  count(Cover_Type) %>%
  mutate(Cover_Type = reorder(Cover_Type, n)) %>%
  ggplot(aes(Cover_Type, n, fill = Cover_Type)) +
  geom_col(show.legend = F) +
  coord_flip() +
  labs(title = "Outcome variable cover type is heavily skewed towards Lodgepole Pine",
       x = NULL,
       y = NULL)

trees_transform %>%
  ggplot(aes(Wilderness, fill = Wilderness)) +
  geom_bar(show.legend = F) +
  labs(title = "Area 1 is most predominant feature; Area 2 has very few cases")

trees_transform %>%
  count(Soil) %>%
  mutate(Soil = reorder(Soil, n)) %>%
  ggplot(aes(Soil, n)) +
  geom_col(fill = "dodgerblue2") +
  coord_flip() +
  labs(title = "Soil types very uneven with 29 being largest class",
       x = NULL)

#bivariate
trees_transform %>%
  mutate(Cover_Type = reorder(Cover_Type, Elevation, median)) %>%
  ggplot(aes(Cover_Type, Elevation, fill = Cover_Type)) +
  geom_boxplot(show.legend = F, outlier.alpha = .15, outlier.color = "dodgerblue2") +
  coord_flip() +
  labs(title = "Boxplot for cover type vs elevation- Plot informs details on heights where trees grow",
       x = NULL)

trees_transform %>%
  mutate(Cover_Type = reorder(Cover_Type, Ammenities_distance, median)) %>%
  ggplot(aes(Cover_Type, Ammenities_distance, fill = Cover_Type)) +
  geom_boxplot(show.legend = F, outlier.alpha = .15, outlier.color = "dodgerblue2") +
  coord_flip() +
  labs(title = "Boxplot for cover type vs Ammenities_distance- Plot informs details on average distance to an ammenity",
       x = NULL)

trees_transform %>%
  mutate(Cover_Type = reorder(Cover_Type, Aspect, median)) %>%
  ggplot(aes(Cover_Type, Aspect, fill = Cover_Type)) +
  geom_boxplot(show.legend = F) +
  coord_flip() +
  labs(title = "Boxplot for cover type vs aspect- Plot informs details on compass direction where trees grow",
       x = NULL)

trees_transform %>%
  mutate(Cover_Type = reorder(Cover_Type, Slope, median)) %>%
  ggplot(aes(Cover_Type, Slope, fill = Cover_Type)) +
  geom_boxplot(show.legend = F) +
  coord_flip() +
  labs(title = "Boxplot for cover type vs slope- Plot informs details on compass direction where trees grow",
       x = NULL)

trees_transform %>%
  select(Elevation, Aspect, Slope, Cover_Type) %>%
  gather(key = "metric", value = "values", -Cover_Type) %>%
  arrange(metric, values) %>%
  ggplot(aes(Cover_Type, values, fill = Cover_Type)) +
  geom_boxplot(show.legend = F) +
  facet_wrap(facets = "metric", scales = "free") +
  coord_flip() +
  labs(title = "Aspect, Elevation, and Slope metrics for different cover types",
       x = NULL)

trees_transform %>%
  select(Horizontal_Distance_To_Hydrology,
         Horizontal_Distance_To_Roadways,
         Horizontal_Distance_To_Fire_Points,
         Vertical_Distance_To_Hydrology,
         Cover_Type) %>%
  gather(key = "metric", value = "values", -Cover_Type) %>%
  ggplot(aes(Cover_Type, values, fill = Cover_Type)) +
  geom_boxplot(show.legend = F) +
  facet_wrap(facets = "metric", scales = "free") +
  coord_flip() +
  labs(title = "Distance metrics for cover types- Vertical looks homogenous but others look varied",
       x = NULL)

trees_transform %>%
  select(Hillshade_9am, Hillshade_Noon, Hillshade_3pm, Cover_Type) %>%
  gather(key = "time", value = "values", -Cover_Type) %>%
  mutate(time = factor(time, levels = c("Hillshade_9am", 
                                        "Hillshade_Noon", 
                                        "Hillshade_3pm"))) %>%
  ggplot(aes(Cover_Type, values, fill = Cover_Type)) +
  geom_boxplot(show.legend = F) +
  facet_wrap(facets = "time", scales = "free") +
  coord_flip() +
  labs(title = "Colour metrics by time of day for cover types- shades look homogenous across cover types",
       x = NULL)

trees_transform %>%
  mutate(Cover_Type = reorder(Cover_Type, ammenities_distance, median)) %>%
  ggplot(aes(Cover_Type, ammenities_distance, fill = Cover_Type)) +
  geom_boxplot(show.legend = F) +
  coord_flip() +
  labs(title = "Boxplot for cover type vs average distance to ammenities",
       x = NULL)

trees_transform %>%
  mutate(Cover_Type = reorder(Cover_Type, Hydrol_delta, median)) %>%
  ggplot(aes(Cover_Type, Hydrol_delta, fill = Cover_Type)) +
  geom_boxplot(show.legend = F) +
  coord_flip() +
  labs(title = "Boxplot for cover type vs hydrology delta",
       x = NULL)

trees_transform %>%
  count(Cover_Type, Wilderness) %>%
  ggplot(aes(Cover_Type, Wilderness, size = n, colour = n)) +
  geom_point() +
  scale_colour_gradient(low = "deepskyblue", high = "darkorange") +
  guides(size = F) +
  labs(title = "Cover type vs wilderness type- not all areas represented",
       x = NULL,
       y = NULL)

trees_transform %>%
  count(Cover_Type, Soil) %>%
  ggplot(aes(Cover_Type, Soil, size = n, colour = n)) +
  geom_point() +
  scale_colour_gradient(low = "deepskyblue", high = "darkorange") +
  guides(size = F) +
  labs(title = "Cover type vs wilderness type- not all areas represented",
       x = NULL,
       y = NULL)

trees_transform %>%
  ggplot(aes(Horizontal_Distance_To_Hydrology, 
             Vertical_Distance_To_Hydrology, colour = Cover_Type)) +
  geom_jitter(alpha = .4, size = 2) +
  facet_wrap(facets = "aspect_direction")

#corplot 
trees_transform %>%
  select(-Id, -Cover_Type, -Wilderness, -Soil) %>%
  rename(Elev. = Elevation,
         H.Hydro = Horizontal_Distance_To_Hydrology,
         V.Hydro = Vertical_Distance_To_Hydrology,
         H.Road = Horizontal_Distance_To_Roadways,
         AM = Hillshade_9am,
         Noon = Hillshade_Noon,
         PM = Hillshade_3pm,
         Fire = Horizontal_Distance_To_Fire_Points,
         Ammen. = Ammenities_distance,
         Hydro = Hydrol_delta) %>%
  cor() %>%
  corrplot.mixed(title = "Correlation plot for Forestry Data- Output shows mix of correlations", 
                 mar = c(0, 0, 2, 0))

#modeling work
set.seed(314)
tree_rpart <- rpart(formula = Cover_Type ~., data = training)
fancyRpartPlot(tree_rpart)

#rf variable importance plot
as.data.frame(tree_rpart$variable.importance) %>%
  rownames_to_column(var = "variable") %>%
  rename(importance = "tree_rpart$variable.importance") %>%
  mutate(variable = reorder(variable, importance)) %>%
  top_n(10, importance) %>%
  ggplot(aes(variable, importance)) +
  geom_col(fill = "dodgerblue2") +
  coord_flip() +
  labs(title = "Rpart variable importance plot- Elevation most important feature",
       x = NULL)

confusionMatrix(training$Cover_Type, 
                predict(tree_rpart, training, type = "class"))

(82 + 87 + 73 + 87 + 80 + 85 + 92) / 7

confusionMatrix(holdout$Cover_Type, 
                predict(tree_rpart, holdout, type = "class"))

(82 + 87 + 72 + 86 + 80 + 86 + 91) / 7

set.seed(314)
tree_multinom <- multinom(formula = Cover_Type ~., data = training)
confusionMatrix(training$Cover_Type, 
                predict(tree_multinom, training, type = "class"))

confusionMatrix(holdout$Cover_Type, 
                predict(tree_multinom, holdout, type = "class"))

(84 + 88 + 78 + 88 + 83 + 79 + 92) / 7

(84 + 88 + 76 + 88 + 83 + 77 + 92) / 7

set.seed(314)
tree_rf <- randomForest(Cover_Type ~., data = training)
confusionMatrix(training$Cover_Type, 
                predict(tree_rf, training, type = "class"))

(98 + 97 + 99 + 99 + 99 + 97 + 100) / 7

confusionMatrix(holdout$Cover_Type, 
                predict(tree_rf, holdout, type = "class"))

#balanced accuracy- metrics from rf confusion matrix above
(93 + 94 + 91 + 95 + 93 + 89 + 97) / 7

getmode <- function(v) {
  uniqv <- unique(v)
  if(length(uniqv)!=length(v)){
    uniqv[which.max(tabulate(match(v, uniqv)))]  
  }else{
    sample(v,1)
  }
}

#ensemble train and test compare
training <- training %>%
  mutate(tree = predict(tree_rpart, training, type = "class"),
         multinom = predict(tree_multinom, training, type = "class"),
         rf = predict(tree_rf, training, type = "class"))

training <- training %>%
  mutate(ensemble = apply(training[,59:61], 1, getmode))

confusionMatrix(training$Cover_Type, training$ensemble)

(89 + 91 + 86 + 92 + 90 + 91 + 95) / 7

holdout <- holdout %>%
  mutate(tree = predict(tree_rpart, holdout, type = "class"),
         multinom = predict(tree_multinom, holdout, type = "class"),
         rf = predict(tree_rf, holdout, type = "class"))

holdout <- holdout %>%
  mutate(ensemble = apply(holdout[,59:61], 1, getmode))

confusionMatrix(holdout$Cover_Type, holdout$ensemble)

(88 + 90 + 83 + 92 + 87 + 86 + 95) / 7

#only 1.6% ties where no model has same class
holdout %>%
  mutate(class_votes = ifelse(rf != multinom & 
                             rf != tree &
                             tree != multinom, "mixed vote", "some agreement")) %>%
  count(class_votes) %>%
  mutate(percent = round(n / sum(n), 3) * 100)

#rf variable importance plot
as.data.frame(tree_rf$importance) %>%
  rownames_to_column(var = "variable") %>%
  mutate(variable = reorder(variable, MeanDecreaseGini)) %>%
  top_n(10, MeanDecreaseGini) %>%
  ggplot(aes(variable, MeanDecreaseGini)) +
  geom_col(fill = "dodgerblue2") +
  coord_flip() +
  labs(title = "Random Forest variable importance plot- Elevation most important feature",
       x = NULL)
