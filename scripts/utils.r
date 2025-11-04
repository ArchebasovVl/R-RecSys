create_user_item_matrix <- function(
    dataframe,
    col_users,
    col_items,
    col_ratings
) {
    n <- dim(unique(dataframe[col_users])[1])
    m <- dim(unique(dataframe[col_items])[1])

    user_matrix <- matrix(0, n, m)

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

fit_baseline <- function(user_rating_matrix) {
    items_means <- vector("numeric", ncol(user_rating_matrix))
    for (j in seq_len(ncol(user_rating_matrix))) {
        count <- 0
        sum <- .0
        for (i in seq_len(nrow(user_rating_matrix))) {
            if (user_rating_matrix[i, j] != 0) {
                count <- count + 1
                sum <- sum + user_rating_matrix[i, j]
            }
        }
        items_means[j] <- sum / count
    }

    for (i in seq_len(nrow(user_rating_matrix))) {
        for (j in seq_len(ncol(user_rating_matrix))) {
            if (user_rating_matrix[i, j] == 0) {
                user_rating_matrix[i, j] <- items_means[j]
            }
        }
    }

    user_rating_matrix
}

predict_baseline <- function(model, user_rating_matrix, user, user_map) {
    mask <- user_rating_matrix == 0
    user_id <- user_map[[user]]

    model[mask] <- 0
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