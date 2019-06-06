centroids <- here::here("data/processed",
                        "centroids.csv") %>%
    read.csv()
centroids <- centroids %>% slice(match(countries, country))
distances <- geosphere::distm(cbind(centroids$lon,
                                    centroids$lat))
distances <- as.matrix(distances)
distvec <- distances[lower.tri(distances)]
idx <- mRIIDS:::lower_tri_idx(nrow(distances))
n_from <- centroids$pop[idx[, 1]]
n_to <- centroids$pop[idx[, 2]]
flowmat <- mRIIDS:::flow_matrix(distvec,
                                n_from,
                                n_to,
                                place_names = centroids$country,
                                model = "gravity",
                                mobility)
