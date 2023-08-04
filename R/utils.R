#'
#' This function back-transforms a variable in df2 to the original scale of
#' the same variable in df1. Assumes that variable was mean-centered and
#' scaled by the standard deviation.
#'
#' @param df1 A data frame containing the original unscaled data.
#' @param df2 A data frame containing the scaled data.
#' @param v The name of the variable to back-transform in df1.
#' @param v2 The name of the variable to create or update in df2.
#'
#' @return A data frame with the back-transformed variable.
#'
#' @examples
#' df1 <- data.frame(x = rnorm(100, 10, 2))
#' df2 <- data.frame(x = scale(df1$x))
#' df2 <- og_scale(df1, df2, "x")
#'
#' @export
og_scale <- function(df1, df2, v = NULL, v2 = NULL) {
    if (!is.null(v2) && !v2 %in% names(df2)) {
        stop("v2 is not in df2")
    } else if (is.null(v2) && !v %in% names(df2) &&
        !v %in% names(df1)) {
        stop("v is not in df2 or df1")
    }
    # back transform v in df2 to the original scale of v in df1
    if (!is.null(v2)) {
        df2 <- df2 |>
            dplyr::mutate(
                !!dplyr::sym(v2) := !!dplyr::sym(v2) * stats::sd(df1[[v]]) +
                    mean(df1[[v]])
            )
    } else {
        df2 <- df2 |>
            dplyr::mutate(
                !!dplyr::sym(v) := !!dplyr::sym(v) * stats::sd(df1[[v]]) +
                    mean(df1[[v]])
            )
    }
    return(dplyr::as_tibble(df2))
}


#' Check if a data frame is a valid grid
#'
#' This function checks if a data frame is a valid grid, which is defined as a
#' data frame where the number of rows is equal to the product of the number
#' of unique values in each column.
#'
#' #' Thanks to Mattan S. Ben-Shachar
#' https://gist.github.com/mattansb/b1f1e636286003b6c45353b2d2f7d1d7
#'
#' @param df A data frame to check
#' @return A logical value indicating whether the data frame is a valid grid
#' @examples
#' df <- data.frame(x = c(1, 2), y = c(3, 4))
#' .is_grid(df)
#' # [1] TRUE
#'
#' df2 <- data.frame(x = c(1, 2), y = c(3, 3))
#' .is_grid(df2)
#' # [1] FALSE
#'
.is_grid <- function(df) {
    unq <- lapply(df, unique)
    if (prod(sapply(unq, length)) != nrow(df)) {
        return(FALSE)
    }
    df2 <- do.call(expand.grid, args = unq)
    df2$..1 <- 1
    res <- merge(df, df2, by = colnames(df), all = TRUE)
    return(sum(res$..1) == sum(df2$..1))
}


#' Match a data frame to a grid
#'
#' This function matches a data frame to a grid, which is defined as a
#' data frame where each column contains a unique set of values.
#' The function returns a modified version of the data frame where each
#' value in each column is replaced with the closest matching value in the
#' corresponding column of the grid.
#'
#' Thanks to Mattan S. Ben-Shachar
#' https://gist.github.com/mattansb/b1f1e636286003b6c45353b2d2f7d1d7
#'
#' @param grid A data frame representing the grid to match to
#' @param data A data frame to match to the grid
#' @return A modified version of the data frame where each value in each
#' column is replaced with the closest matching value in the corresponding
#' column of the grid.
#'
#' @export
match_grid <- function(grid, data) {
    # 1. Duplicate grid
    dm1_grid2 <- grid
    # 2. Remove from duplicate fixed columns and columns not in data
    is_fixed <- sapply(dm1_grid2, insight::has_single_value)
    dm1_grid2 <- dm1_grid2[!is_fixed]
    dm1_grid2 <- dm1_grid2[intersect(colnames(dm1_grid2), colnames(data))]
    # 3. Test if grid
    stopifnot(.is_grid(dm1_grid2))


    # 4. For each col in grid, get the unique values
    unqs <- lapply(dm1_grid2, unique)
    to_char <- sapply(unqs, function(x) is.character(x) || is.factor(x) || is.logical(x))
    unqs[to_char] <- lapply(unqs[to_char], as.character)

    # 5. For each row in data:
    for (i in seq_len(nrow(data))) {
        for (j in names(unqs)) {
            #   For each column
            if (is.factor(unqs[[j]]) || is.logical(unqs[[j]]) || is.character(unqs[[j]])) {
                # If (logical, factor, char), match exactly. If not matched, return NA.
                jidx <- which(as.character(data[[j]][i]) == as.character(unqs[[j]]))
                if (length(jidx) == 0L) {
                    data[[j]][i] <- NA
                    next
                }
            } else {
                # If numeric, find closest value in grid values
                jidx <- which.min(abs(data[[j]][i] - unqs[[j]]))
            }
            data[[j]][i] <- unqs[[j]][jidx]
        }
    }

    # 6. Add back columns from (2).
    fixed_cols <- names(is_fixed)[is_fixed]
    data[, fixed_cols] <- grid[1, fixed_cols]

    return(data)
}
