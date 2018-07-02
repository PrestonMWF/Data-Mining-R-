library(caret)
library(tidyverse)
library(knitr)
library(kableExtra)

theme_set(
  theme_bw()
)

custom_kable <- function(x){
  kable(x, format = "html") %>%
    kable_styling(bootstrap_options = "striped")
}

data("GermanCredit")

str(GermanCredit)

clustering_set <- GermanCredit %>%
  select(Duration, Age, Amount, InstallmentRatePercentage, 
         ResidenceDuration, NumberExistingCredits)

visual_df <- clustering_set %>%
  gather(key = "variable")

variable_mean <- visual_df %>%
  group_by(variable) %>%
  summarise(mean = mean(value))

variable_mean %>%
  custom_kable()

visual_df %>%
  ggplot(aes(value, fill = variable)) +
  geom_histogram(bins = 33, show.legend = F) +
  geom_vline(data = variable_mean, aes(xintercept = mean),
             alpha = .4, size = 1.3 , colour = "dodgerblue2") +
  facet_wrap(facets = "variable", scales = "free") +
  labs(title = "Clustering data for German Credit customer segmentation- includes only numeric variables",
       y = NULL,
       x = NULL,
       caption = "Source: German Credit Data")

#clustering_set <- as.data.frame(scale(clustering_set, center = T, scale = T))
set.seed(1017)
data_split <- sample(x = nrow(clustering_set), size = 650, replace = F)

training <- clustering_set %>%
  slice(data_split)

testing <- clustering_set %>%
  slice(-data_split)

#compare train and test numbers

cluster_collect <- function(return, training_data, test_data, cluster_n, n_start, seed){
  set.seed(seed = seed)
  train_cluster <- kmeans(training_data, centers = cluster_n, nstart = n_start)
  test_cluster <- kmeans(test_data, centers = train_cluster$centers, nstart = n_start)
  
  vaf <- data.frame(cluster = cluster_n,
             train_VAF = 1 - train_cluster$tot.withinss / train_cluster$totss,
             test_VAF = 1 - test_cluster$tot.withinss / test_cluster$totss) %>%
    mutate(total_diff = round(test_VAF - train_VAF, 3),
           percent_diff = round(total_diff / train_VAF * 100, 3))
  
  train_size <- train_cluster$cluster
  test_size <- test_cluster$cluster
  
  train_means <- train_cluster$centers
  test_means <- test_cluster$centers
  
  if (return == "vaf") {
    return(vaf)
  } else if (return == "test") {
    return(test_size)
  }
}

vaf_compare <- as.data.frame(
  t(sapply(2:10, function(x) cluster_collect(seed = 1017,
                                             return = "vaf",
                                              training_data = training, 
                                              test_data = testing,
                                              cluster_n = x, 
                                              n_start = 100)))
  )

vaf_compare <- as.data.frame(apply(vaf_compare, 2, as.numeric))

vaf_compare %>%
  custom_kable()

vaf_compare %>%
  select(-total_diff, -percent_diff) %>%
  gather(key = data_set, value = vaf, train_VAF:test_VAF) %>%
  mutate(data_set = factor(data_set, levels = c("train_VAF", "test_VAF"))) %>%
  ggplot(aes(cluster, vaf, colour = data_set)) +
  geom_line(size = 1.3) +
  scale_y_continuous(breaks = seq(0, 1, .05)) +
  scale_x_continuous(breaks = seq(2, 10, 1)) +
  geom_vline(xintercept = 4, alpha = .3, colour = "blueviolet", size = 1.3) +
  geom_vline(xintercept = 6, alpha = .3, colour = "blueviolet", size = 1.3) +
  annotate("rect", xmin = 4, xmax = 6,
           ymin = .67, ymax = 1, 
           alpha = .1, fill = "blueviolet") +
  scale_color_manual(values = c("royalblue2", "darkorange")) +
  labs(title = "Variance Accounted For (VAF) Screeplot for training and test clusters",
       subtitle = "Training and test remain close providing assurance that cluster groups are reasonable; Selecting 4 to 6 clusters seems appropriate",
       y = "VAF")

comparison_df <- as.data.frame(sapply(1:10, function(x) cluster_collect(seed = 1017,
                                                            return = "train",
                                                            training_data = training, 
                                                            test_data = testing,
                                                            cluster_n = x, 
                                                            n_start = 100))) %>%
  mutate(data_set = "training", 
         F1_scores = princomp(training)$scores[,1],
         F2_scores = princomp(training)$scores[,2]) %>%
  select(data_set, F1_scores, F2_scores, everything()) %>%
  select(-V1)

comparison_df <- comparison_df %>%
  bind_rows(
    as.data.frame(sapply(1:10, function(x) cluster_collect(seed = 1017,
                                                           return = "test",
                                                         training_data = training, 
                                                         test_data = testing,
                                                         cluster_n = x, 
                                                         n_start = 100))) %>%
  mutate(data_set = "testing", 
         F1_scores = princomp(testing)$scores[,1] * -1,
         F2_scores = princomp(testing)$scores[,2] * -1) %>%
  select(data_set, F1_scores, F2_scores, everything()) %>%
  select(-V1)
)

names(comparison_df) <- gsub(pattern = "V", replacement = "cluster_", 
                             x = names(comparison_df))

#cluster size comparison- test vs train
#use spread to get proportions to graph; include regression lines and R2

cluster_compare <- comparison_df %>%
  gather(key = "cluster", value = "value", -data_set, -F1_scores, -F2_scores) %>%
  group_by(data_set, cluster, value) %>%
  count() %>%
  mutate(count = n,
         clust_percent = ifelse(data_set == "training", n / 650, n / 350),
         clust_percent = round(clust_percent, 4) * 100) %>%
  ungroup() %>%
  select(-n, -value)

#table comparing test and train for 2,3
cluster_compare %>%
  filter(cluster == "cluster_2" | cluster == "cluster_3") %>%
  arrange(cluster) %>%
  custom_kable()

values <- cluster_compare %>%
  select(-count) %>%
  mutate(row_id = 1:n()) %>%
  ungroup() %>% 
  spread(key = "data_set", value = "clust_percent") %>%
  select(-row_id) %>%
  filter(is.na(training) == F)

cluster_compare %>%
  select(-count) %>%
  mutate(row_id = 1:n()) %>%
  ungroup() %>% 
  spread(key = "data_set", value = "clust_percent") %>%
  select(-row_id) %>%
  filter(is.na(training) == T) %>%
  select(-training) %>%
  bind_cols(values) %>%
  select(cluster, training, testing) %>%
  mutate(cluster = factor(cluster, levels = names(comparison_df)[4:12])) %>%
  ggplot(aes(training, testing)) +
  geom_point(aes(colour = cluster), size = 5, alpha = .5) +
  geom_smooth(method = "lm", size = 1.3, se = F, colour = "darkgray") +
  facet_wrap(facets = "cluster", scales = "free") +
  labs(title = "Train and test cluster group size percentage comparison",
       subtitle = "Both sets show very similiar group size proportions providing evidence that test clustering split is sound",
       x = "training group percentages",
       y = "testing group percentages",
       caption = "Source: German Credit Data")

#pca facets with group 2 to 10 for test and train; comment on relative sizes
comparison_df %>%
  gather(key = "cluster", value = "value", -data_set, -F1_scores, -F2_scores) %>%
  mutate(data_set = factor(data_set, levels = c("training", "testing")), 
         value = as.factor(value),
         cluster = factor(cluster, levels = names(comparison_df)[4:12])) %>%
  ggplot(aes(F1_scores, F2_scores, colour = value)) +
  geom_jitter(alpha = .5) +
  facet_grid(cluster ~ data_set, scales = "free") +
  labs(title = "Train and test cluster groups visualized using PCA",
       subtitle = "Both sets show very similiar group size and shape providing further evidence that test clustering split is sound",
       x = "PCA 1",
       y = "PCA 2",
       caption = "Source: German Credit Data")

comparison_df %>%
  gather(key = "cluster", value = "value", -data_set, -F1_scores, -F2_scores) %>%
  mutate(value = as.factor(value)) %>%
  filter(cluster == "cluster_4") %>%
  ggplot(aes(F1_scores, F2_scores, colour = value)) +
  geom_jitter(size = 2, alpha = .4) +
  facet_wrap(facets = "data_set") +
  labs(title = "Train and test cluster groups visualized using PCA- Focus on Cluster 4",
       subtitle = "Both sets show very similiar group size and shape providing further evidence that test clustering split is sound",
       x = "PCA 1",
       y = "PCA 2",
       caption = "Source: German Credit Data")

#analyzing four clusters; means and naming, facet of means across groups for training
#do sd across each mean to show variability of group
set.seed(1017)
segments_train <- kmeans(clustering_set, centers = 6, nstart = 100)
segments_test <- kmeans(testing, centers = segments_train$centers, nstart = 100)

rbind(train = table(segments_train$cluster),
      test = table(segments_test$cluster)) %>%
  custom_kable()

rbind(train = prop.table(table(segments_train$cluster)),
      test = prop.table(table(segments_test$cluster))) %>%
  custom_kable()


#komeans
fun.okc.2 <- function (data = data, nclust = nclust, lnorm = lnorm, tolerance = tolerance) 
{
  M = nrow(data)
  N = ncol(data)
  K = nclust
  niterations = 50
  #    datanorm = apply(data, 2, fun.normalize)
  datanorm = scale(data)
  S = matrix(sample(c(0, 1), M * K, replace = TRUE), M, K)
  S = cbind(S, rep(1, M))
  W = matrix(runif(N * K), K, N)
  W = rbind(W, rep(0, N))
  sse = rep(0, niterations)
  oprevse = exp(70)
  opercentse = 1
  i = 1
  while ((i <= niterations) & (opercentse > tolerance)) {
    for (k in 1:K) {
      sminusk = S[, -k]
      wminusk = W[-k, ]
      s = as.matrix(S[, k])
      w = t(as.matrix(W[k, ]))
      dstar = datanorm - sminusk %*% wminusk
      prevse = exp(70)
      percentse = 1
      l = 1
      while ((l <= niterations) & (percentse > tolerance)) {
        for (m in 1:N) {
          if (lnorm == 2) {
            w[1, m] = mean(dstar[s == 1, m], na.rm = TRUE)
          }
          if (lnorm == 1) {
            w[1, m] = median(dstar[s == 1, m], na.rm = TRUE)
          }
        }
        for (m in 1:M) {
          if (lnorm == 2) {
            ss1 = sum((dstar[m, ] - w[1, ])^2, na.rm = TRUE)
            ss0 = sum((dstar[m, ])^2, na.rm = TRUE)
          }
          if (lnorm == 1) {
            ss1 = sum(abs(dstar[m, ] - w[1, ]), na.rm = TRUE)
            ss0 = sum(abs(dstar[m, ]), na.rm = TRUE)
          }
          if (ss1 <= ss0) {
            s[m, 1] = 1
          }
          if (ss1 > ss0) {
            s[m, 1] = 0
          }
        }
        if (sum(s) == 0) {
          s[sample(1:length(s), 2)] = 1
        }
        if (lnorm == 2) {
          se = sum((dstar - s %*% w)^2, na.rm = TRUE)
        }
        if (lnorm == 1) {
          se = sum(abs(dstar - s %*% w), na.rm = TRUE)
        }
        percentse = 1 - se/prevse
        prevse = se
        l = l + 1
      }
      S[, k] = as.vector(s)
      W[k, ] = as.vector(w)
    }
    if (lnorm == 2) 
      sse[i] = sum((datanorm - S %*% W)^2, na.rm = TRUE)/sum((datanorm - 
                                                                mean(datanorm, na.rm = TRUE))^2, na.rm = TRUE)
    if (lnorm == 1) 
      sse[i] = sum(abs(datanorm - S %*% W), na.rm = TRUE)/sum(abs(datanorm - 
                                                                    median(datanorm, na.rm = TRUE)), na.rm = TRUE)
    if (lnorm == 2) {
      ose = sum((datanorm - S %*% W)^2, na.rm = TRUE)
    }
    if (lnorm == 1) {
      ose = sum(abs(datanorm - S %*% W), na.rm = TRUE)
    }
    opercentse = (oprevse - ose)/oprevse
    oprevse = ose
    i = i + 1
  }
  if (lnorm == 2) 
    vaf = cor(as.vector(datanorm), as.vector(S %*% W), use = "complete.obs")^2
  if (lnorm == 1) 
    vaf = 1 - sse[i - 1]
  rrr = list(Data = data, Normalized.Data = datanorm, Tolerance = tolerance, 
             Groups = S[, 1:K], Centroids = round(W[1:K, ], 2), SSE.Percent = sse[1:i - 
                                                                                    1], VAF = vaf)
  
  
  return(rrr)
}

komeans <- function (data = data, nclust = nclust, lnorm = lnorm, nloops = nloops, tolerance = tolerance, seed = seed) 
{
  prevsse = 100
  set.seed(seed)
  for (i in 1:nloops) {
    z = fun.okc.2(data = data, nclust = nclust, lnorm = lnorm, 
                  tolerance = tolerance)
    if (z$SSE.Percent[length(z$SSE.Percent[z$SSE.Percent >  0])] < prevsse) {
      prevsse = z$SSE.Percent[length(z$SSE.Percent[z$SSE.Percent >  0])]
      ind = i
      z.old = z
    }
  }
  return(list(data = z.old$Data, Normalized.Data = z.old$Normalized.Data, 
              Group = z.old$Group %*% as.matrix(2^(0:(nclust-1)) ), Centroids = z.old$Centroids, Tolerance = z.old$Tolerance, 
              SSE.Pecent = z.old$SSE.Percent, VAF = z.old$VAF, iteration = ind, 
              seed = seed))
}

#10- you can use a classification model to predict what segment they would be in
#Assuming mailing list or other customer details  data, use model to predict clusters
#recruit in correct proportions to original clusters or some mix suited to business need


#function development
cluster_collect <- function(return, training_data, test_data, cluster_n, n_start, seed){
  set.seed(seed = seed)
  train_cluster <- kmeans(training_data, centers = cluster_n, nstart = n_start)
  test_cluster <- kmeans(test_data, centers = train_cluster$centers, nstart = n_start)
  
  vaf <- data.frame(cluster = cluster_n,
                    train_VAF = 1 - train_cluster$tot.withinss / train_cluster$totss,
                    test_VAF = 1 - test_cluster$tot.withinss / test_cluster$totss) %>%
    mutate(total_diff = round(test_VAF - train_VAF, 3),
           percent_diff = round(total_diff / train_VAF * 100, 3))
  
  train_size <- train_cluster$cluster
  test_size <- test_cluster$cluster
  
  train_means <- train_cluster$centers
  test_means <- test_cluster$centers
  
  while (return == "vaf") {return(vaf)}
  while (return == "test") {return(test_size)}
  while (return == "train") {return(test_size)}
  if (return != "vaf" | return != "test" | return != "train"){
    warning("Value specified is not part of function- Try: vaf, train, or test")}
}
