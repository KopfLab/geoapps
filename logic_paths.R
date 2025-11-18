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
      confirmed = !is.na(.data$confirmed) & .data$confirmed,
      room = .data$room |> stringr::str_remove("\\.\\d+$")
    ) |>
    # remove deleted and canceled records
    dplyr::filter(is.na(.data$deleted), !.data$canceled)
}

# prepare path recommmendation classes
prepare_path_recommendations <- function(paths) {
  paths |>
    fill_down_columns(c("path_id", "category")) |>
    dplyr::mutate(
      path = .data$path[1],
      url = .data$url[1],
      .by = c("path_id")
    ) |>
    dplyr::mutate(
      category_description = .data$category_description[1],
      rec_min = .data$rec_min[1],
      .by = c("path_id", "category")
    ) |>
    dplyr::mutate(row = dplyr::row_number(), .before = 1L) |>
    # path with ID
    dplyr::mutate(
      path_w_id = sprintf("%s (%s)", .data$path, .data$path_id)
    ) |>
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

# get all path classes
prepare_all_path_classes <- function(paths, classes) {
  paths |>
  dplyr::rename("path_class" = "class") |>
  dplyr::left_join(classes, by = "path_class") |>
  dplyr::mutate(
    .by = c("path_id", "category"),
    total = dplyr::n()
  ) |>
  dplyr::mutate(
    recommendation = ifelse(.data$n == "a", "take", sprintf("%s/%d", .data$n, .data$total))
  ) |>
  dplyr::mutate(
    .by = "class",
    major_category = 
    if (any(!is.na(.data$category) & stringr::str_detect(.data$category, "[Ss]trongly"))) "Strongly recommended in some paths"
    else if (any(!is.na(.data$category_info) & stringr::str_detect(.data$category_info, stringr::fixed("recommended")))) "Recommended in some paths"
    else if (!is.na(.data$program[1]) && .data$program[1] == "GEOL") "Other GEOL upper division classes"
    else "Other upper division classes"
  ) |>
  dplyr::select("program", "class", "title", "credits", "path_id", "major_category", "recommendation") |>
  tidyr::pivot_wider(names_from = path_id, values_from = recommendation) |>
  dplyr::arrange(dplyr::desc(.data$major_category), dplyr::desc(.data$program == "GEOL"), .data$program, .data$class) 
}

# get path specific list of classes
prepare_path_classes <- function(paths, selected_path, classes) {

  path_classes <-
    paths |>
    dplyr::filter(path_w_id == !!selected_path) |>
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
    dplyr::mutate(
      info = "yes",
      term = factor(.data$term, levels = !!selected_terms)
    ) |>
    dplyr::arrange(.data$term) |>
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

prepare_all_paths_table_columns <- function(path_classes) {
  path_classes |>
    dplyr::mutate(
      row = dplyr::row_number(),
      Category = major_category,
      Class = sprintf("%s(%s)", class, credits),
      Title = title,
      .before = 1L
    ) |>
    dplyr::select(-"program", -"class", -"credits", -"title", -"major_category")
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

