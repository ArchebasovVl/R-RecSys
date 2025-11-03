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
