create_user_item_matrix <- function(
  dataframe,
  col_users,
  col_items,
  col_ratings
) {
    n <- dim(unique(dataframe[col_users])[1])
    m <- dim(unique(dataframe[col_items])[1])

    user_matrix <- matrix(-10000, n, m)

    ids_users <- create_id(dataframe[col_users])
    ids_items <- create_id(dataframe[col_items])

    for (i in seq_len(nrow(dataframe))) {
        user_matrix[
            ids_users[[dataframe[i, col_users]]],
            ids_items[[dataframe[i, col_items]]]
        ] <- dataframe[i, col_ratings]
    }

    list(user_matrix, ids_users, ids_items)
}

create_id <- function(series) {
    ids <- new.env(hash = TRUE, parent = emptyenv())
    keys <- unique(series)

    for (i in seq_len(nrow(keys))) {
        ids[[keys[i, 1]]] <- i
    }

    ids
}

cos_dist_all <- function(vector1, vector2) {
    t(vector1) %*% vector2 / norm(vector1, type = "2") / norm(vector2, type = "2")
}

cos_dist_rated <- function(vector1, vector2) {
    v1_2 <- 0
    v2_2 <- 0
    v12 <- 0
    for (i in seq_along(vector1)) {
        if (vector1[i] >= 0 && vector2[i] >= 0) {
            v1_2 <- v1_2 + vector1[i] * vector1[i]
            v2_2 <- v2_2 + vector2[i] * vector2[i]
            v12 <- v12 + vector1[i] * vector2[i]
        }
    }
    v12 / sqrt(v1_2) / sqrt(v2_2)
}

calculate_sim <- function(mat, target_ind, rated = FALSE) {
    sim <- matrix(0, nrow = 1, ncol = nrow(mat))

    if (rated) {
        dist <- cos_dist_rated
    } else {
        dist <- cos_dist_all
    }

    for (i in seq_len(nrow(mat))) {
        sim[i] <- dist(mat[target_ind, ], mat[i, ])
    }

    sim
}

calibrate_rates <- function(rates, sim) {
    calibrated_rates <- rates
    for (i in seq_len(nrow(rates))) {
        calibrated_rates[i, ] <- calibrated_rates[i, ] * sim[i]
    }

    calibrated_rates
}

fit_col_filtering <- function(rates, target_ind, rated = FALSE) {
    calculate_sim(rates, target_ind, rated)
}

predict_col_filtering <- function(rates, sim, target_ind) {
    cal_rates <- calibrate_rates(rates, sim)

    similar_users_ind <- choose_k_best(sim, 3)
    pred_rates <- matrix(0, nrow = 1, ncol = ncol(cal_rates))

    for (i in seq_len(ncol(cal_rates))) {
        for (j in similar_users_ind) {
            pred_rates[i] <- pred_rates[i] + cal_rates[j, i]
        }
    }
    for (j in seq_len(ncol(cal_rates))) {
        if (cal_rates[target_ind, j] >= 0) {
            pred_rates[j] <- 0
        }
    }

    pred_rates
}

fit_baseline <- function(user_rating_matrix) {
    items_means <- vector("numeric", ncol(user_rating_matrix))
    for (j in seq_len(ncol(user_rating_matrix))) {
        count <- 0
        sum <- .0
        for (i in seq_len(nrow(user_rating_matrix))) {
            if (user_rating_matrix[i, j] != -10000) {
                count <- count + 1
                sum <- sum + user_rating_matrix[i, j]
            }
        }
        if (count != 0) {
            items_means[j] <- sum / count
        } else {
            items_means[j] <- 0
        }
    }

    for (i in seq_len(nrow(user_rating_matrix))) {
        for (j in seq_len(ncol(user_rating_matrix))) {
            if (user_rating_matrix[i, j] == -10000) {
                user_rating_matrix[i, j] <- items_means[j]
            }
        }
    }

    user_rating_matrix
}

predict_baseline <- function(model, user_rating_matrix, user, user_map) {
    mask <- user_rating_matrix != -10000
    user_id <- user_map[[user]]

    model[mask] <- -10000
    predictions <- model[user_id, ]

    predictions
}

choose_k_best <- function(row, k) {
    index <- numeric(k)

    row <- matrix(append(row, seq_along(row)), nrow = 2, ncol = length(row), TRUE)
    row <- row[, order(row[1, ], decreasing = TRUE)]

    for (i in seq_len(k)) {
        index[i] <- row[2, i]
    }

    index
}

train_test_split <- function(data, p) {
    train <- data
    test <- matrix(0, nrow(data), ncol(data))
    for (i in seq_len(nrow(data))) {
        for (j in seq_len(ncol(data))) {
            if (data[i, j] != -10000 && runif(1) < p) {
                test[i, j] <- data[i, j]
                train[i, j] <- -10000
            }
        }
    }

    list(train, test)
}

eval_mae <- function(matrix_pred, matrix_val) {
    error <- 0
    count <- 0
    for (i in seq_len(nrow(matrix_pred))) {
        for (j in seq_len(ncol(matrix_pred))) {
            if (matrix_val[i, j] != -10000) {
                error <- error + abs(matrix_val[i, j] - matrix_pred[i, j])
                count <- count + 1
            }
        }
    }

    error / count
}
