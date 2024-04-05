source("logic_terms.R")

# data prep ======

# convenience function to fill down columns
fill_down_columns <- function(df, cols) {
  for (col in cols)
    df <-
      df |>
      dplyr::mutate(..fill = cumsum(!is.na(!!sym(col)))) |>
      dplyr::mutate(dplyr::across(!!col, ~.x[1]), .by = ..fill) |>
      dplyr::select(-"..fill")
  return(df)
}

# prepare classes
prepare_classes <- function(classes) {
  classes <- classes |>
    dplyr::mutate(
      class = stringr::str_remove_all(.data$class, "[ \\r\\n]"),
      inactive = !is.na(.data$inactive) & .data$inactive,
      path_class = .data$class,
      .after = "class"
    )
  # include 4000/5000 cross-listed classes also with the 4000 listing only
  classes_4000 <- classes |>
    dplyr::filter(stringr::str_detect(.data$class, "4\\d{3}/5\\d{3}")) |>
    dplyr::mutate(
      path_class = stringr::str_remove(.data$class, "/5\\d{3}")
    )

  # finalize
  classes |>
    dplyr::bind_rows(
      classes_4000 |> dplyr::anti_join(classes, by = "path_class")
    ) |>
    # make sure every class only exists once
    dplyr::slice_head(n = 1L, by = "path_class") |>
    # figure out level
    dplyr::mutate(
      program = stringr::str_extract(.data$class, "^[^0-9]+"),
      number = stringr::str_extract(.data$class, "\\d+") |> as.integer(),
      upper_division = !is.na(.data$number) & .data$number >= 3000 & .data$number < 5000
    )
}

# prepare schedule for paths
prepare_schedule <- function(schedule) {
  # safety checks
  schedule |>
    dplyr::mutate(
      class = stringr::str_remove_all(class, "[ \\r\\n]"),
      canceled = !is.na(.data$canceled) & .data$canceled,
      confirmed = !is.na(.data$confirmed) & .data$confirmed
    ) |>
    # remove deleted and canceled records
    dplyr::filter(is.na(.data$deleted), !.data$canceled)
}

# prepare path recommmendation classes
prepare_path_recommendations <- function(paths) {
  paths |>
    fill_down_columns(c("path", "category")) |>
    dplyr::mutate(
      category_description = .data$category_description[1],
      rec_min = .data$rec_min[1],
      .by = c("path", "category")
    ) |>
    dplyr::mutate(row = dplyr::row_number(), .before = 1L) |>
    # category info
    dplyr::mutate(
      n = stringr::str_sub(rec_min, 1, 1),
      category_info = dplyr::case_when(
        is.na(.data$n) ~ .data$category,
        stringr::str_detect(.data$n, "\\d") ~ sprintf("%s (at least %s recommended)", .data$category, .data$n),
        TRUE ~ sprintf("%s (take %s)", .data$category, .data$rec_min)
      ),
      .after = "category"
    )
}

# get path specific list of classes
prepare_path_classes <- function(paths, selected_path, classes) {

  path_classes <-
    paths |>
    dplyr::filter(path == !!selected_path) |>
    dplyr::rename("path_class" = "class") |>
    dplyr::left_join(classes, by = "path_class")

  available_classes <-
    # combine all classes
    dplyr::bind_rows(
      path_classes,
      classes |>
        dplyr::filter(.data$upper_division) |>
        dplyr::anti_join(path_classes, by = c("path_class")) |>
        dplyr::filter(.data$program == "GEOL") |>
        dplyr::mutate(category_info = "Other GEOL upper division classes"),
      classes |>
        dplyr::filter(.data$upper_division) |>
        dplyr::anti_join(path_classes, by = c("path_class")) |>
        dplyr::filter(.data$program != "GEOL") |>
        dplyr::mutate(category_info = "Other upper division classes")
    ) |>
    dplyr::slice_head(n = 1, by = "class") |>
    dplyr::mutate(
      row = dplyr::row_number()
    )

  return(available_classes)
}

combine_path_classes_with_schedule <- function(path_classes, schedule, selected_terms) {
  schedule_wide <-
    schedule |>
    dplyr::filter(term %in% !!selected_terms) |>
    dplyr::mutate(info = "yes") |>
    dplyr::slice_head(n = 1L, by = c("term", "class")) |>
    tidyr::pivot_wider(id_cols = c(class), names_from = term, values_from = info)

  path_classes |>
    dplyr::left_join(schedule_wide, by = "class") |>
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(!!selected_terms),
        ~dplyr::case_when(
          !is.na(.x) ~ .x,
          .data$program != "GEOL" ~ "?",
          TRUE ~ "no")
      )
    )
}

prepare_path_classes_table_columns <- function(path_classes) {
  path_classes |>
    dplyr::mutate(
      Category =
        ifelse(
          !is.na(category_description),
          sprintf("%s<br/><i>%s</i>",
                  htmltools::htmlEscape(category_info),
                  htmltools::htmlEscape(category_description)),
          htmltools::htmlEscape(category_info)
        ),
      Class = sprintf("%s(%s)", class, credits),
      Title = title,
      `Relevance for this path` = reason,
    ) |>
    dplyr::select("row", "Category", "Class", "Title", "Relevance for this path", dplyr::matches(get_term_regexp()))
}

