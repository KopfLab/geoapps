# data functions ======

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

# complete timings
complete_semester_availabilities <- function(df) {
  df |>
    dplyr::mutate(
      dplyr::across(
        dplyr::matches("^(Spring|Fall|Summer) ?\\d+"),
        ~dplyr::case_when(
          is.na(.x) | nchar(.x) == 0 ~ "?",
          stringr::str_detect(.x, "n|N|no|No") ~ "no",
          TRUE ~ .x
        )
      )
    )
}

# prepare path recommmendation classes
prep_path_recommendations <- function(paths) {
  paths |>
    fill_down_columns(c("path", "category")) |>
    dplyr::mutate(
      category_description = .data$category_description[1],
      rec_min = .data$rec_min[1],
      .by = c("path", "category")
    )
}

# get all timeslot options
get_all_timeslots <- function(df) {
  df |>
    tidyr::pivot_longer(cols = dplyr::matches("^(Spring|Fall|Summer) ?\\d+")) |>
    dplyr::filter(!.data$value %in% c("?", "no")) |>
    dplyr::pull(.data$value) |>
    unique()
}

# get unique classes
get_unique_classes <- function(classes) {
  # make sure every class only exists once
  classes |> dplyr::filter(dplyr::row_number() == 1L, .by = "class")
}

# get ud classes
get_classes_above_level <- function(classes, level) {
  classes |>
    dplyr::mutate(
      program = stringr::str_extract(.data$class, "^[^0-9]+"),
      number = readr::parse_number(.data$class)
    ) |>
    dplyr::filter(!is.na(.data$number) & number >= !!level) |>
    dplyr::arrange(.data$program, .data$number)
}

# get path specific list of classes
get_path_classes <- function(path, paths, classes) {
  path_classes <-
    paths |>
    dplyr::filter(path == !!path) |>
    dplyr::left_join(classes, by = "class")

  available_classes <-
    # combine all classes
    dplyr::bind_rows(
      path_classes,
      classes |>
        dplyr::anti_join(path_classes, by = "class") |>
        dplyr::filter(.data$program == "GEOL") |>
        dplyr::mutate(category = "other GEOL upper division classes"),
      classes |>
        dplyr::anti_join(path_classes, by = "class") |>
        dplyr::filter(.data$program != "GEOL") |>
        dplyr::mutate(category = "other upper division classes")
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
    ) |>
    dplyr::select(-"n")


  return(available_classes)
}

# ui helper functions =====

get_classes_teaching_info <- function(df) {
  ts <- df |> get_all_timeslots()
  c(
    "?" = "lightgray",
    "no" = "lightpink",
    rep("lightgreen", length(ts)) |> rlang::set_names(ts)
  )
}


get_classes_teaching_info_placeholder <- function() {
  c(`?` = "lightgray", no = "lightpink", `MWF10-11` = "lightgreen",
    `MW2-4pm` = "lightgreen", `TTh10-11:30` = "lightgreen", `MWF9-10` = "lightgreen",
    `MWF1-2:30` = "lightgreen", `TTh2-3` = "lightgreen", `MWF11-12` = "lightgreen"
  )
}
