library(cluster)

library(factoextra)
library(NbClust)
library(mclust)
library(ClusterR)

options(scipen = 0)

### Scaling for other clustering methods, Mixture models don't need scaling.

final_pitchers_df <- as.data.frame(final_pitchers)

row.names(final_pitchers_df) <- final_pitchers_df$pitcher

pitcher_row_names <- final_pitchers_df$pitcher

final_pitchers_df <- final_pitchers_df[, -which(names(final_pitchers_df) == "pitcher")]

rownames(final_pitchers_df) <- pitcher_row_names

pct_vars_to_scale <- c("ff_num_thrown", "sl_num_thrown", "fc_num_thrown",
                       "si_num_thrown", "ch_num_thrown", "cu_num_thrown")

release_x_vars_to_scale <- c("ff_release_pos_x", "sl_release_pos_x", "fc_release_pos_x",
                             "si_release_pos_x", "ch_release_pos_x", "cu_release_pos_x")

release_z_vars_to_scale <- c("ff_release_pos_z", "sl_release_pos_z", "fc_release_pos_z",
                             "si_release_pos_z", "ch_release_pos_z", "cu_release_pos_z")

release_extension_vars_to_scale <- c("ff_release_extension", "sl_release_extension", "fc_release_extension",
                                     "si_release_extension", "ch_release_extension", "cu_release_extension")

speed_vars_to_scale <- c("ff_avg_speed", "sl_avg_speed", "fc_avg_speed",
                             "si_avg_speed", "ch_avg_speed", "cu_avg_speed")

spin_vars_to_scale <- c("ff_avg_spin", "sl_avg_spin", "fc_avg_spin",
                        "si_avg_spin", "ch_avg_spin", "cu_avg_spin")

scaled_df <- final_pitchers_df %>%
  mutate(across(all_of(pct_vars_to_scale), scale),
         across(all_of(release_x_vars_to_scale), scale),
         across(all_of(release_z_vars_to_scale), scale),
         across(all_of(release_extension_vars_to_scale), scale),
         across(all_of(speed_vars_to_scale), scale),
         across(all_of(spin_vars_to_scale), scale))



scaled_df = as.data.frame(scale(final_pitchers_df))

### K Means

model <- kmeans(final_pitchers_df, centers = 2)


### Elbow method

fviz_nbclust(scaled_df, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2) + # add line for better visualisation
  labs(subtitle = "Elbow method") # add subtitle

### Silhoutte method

fviz_nbclust(final_pitchers_df, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")


### Gap Method

set.seed(42)
fviz_nbclust(scaled_df, kmeans,
             nstart = 25,
             method = "gap_stat",
             nboot = 500 # reduce it for lower computation time (but less precise results)
) +
  labs(subtitle = "Gap statistic method")

### DB SCAN
library(fpc)
library(dbscan)

eps_plot = kNNdistplot(scaled_df, k=30)
abline(h = 4.8, lty = 2)

db_model <- dbscan(scaled_df, minPts = 30, eps = 2.8)
db_model

hdb <- hdbscan(scaled_df, minPts = 4)
hdb
plot(scaled_df)

### Mixture model, method used in modeling.

clPairs(scaled_df, pitcher_row_names)


### Silhouette plot to view the greatest jump in interpretable cluster numbers.
### i.e. 36 wasn't the min bic but had the biggest jump for a lower amount of clusters.
pitchersMClustBIC <- mclustBIC(final_pitchers_df, G = 1:100)
plot(pitchersMClustBIC)

plot(pitchersMClustBIC, G = 5:100, ylim = c(-400000,-200000), legendArgs = list(x = "bottomright", ncol = 5))
abline(v = 36, col = "red", lwd = 2)

mclust_model <- Mclust(final_pitchers_df, modelNames = "VEI", G = 36)

cluster_assignments <- mclust_model$classification

final_pitchers_df$cluster <- cluster_assignments

final_pitchers_df <- rownames_to_column(final_pitchers_df, var = "id")

final_pitchers

final_pitchers_df <- final_pitchers_df %>%
  mutate(id = as.integer(id)) %>%
  right_join(all_height %>% select(player_id, full_name, pitch_hand_code, height), by = c("id" = "player_id"))

require('dendextend')
require('GGally')
require('rpart')
require('rpart.plot')

tree_model <- rpart(cluster_assignments ~ ., data = final_pitchers_df)

rpart.plot(tree_model)


hierarchical_structure <- hclust(as.dist(mclust_model$uncertainty))

