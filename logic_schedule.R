# terms =======

get_current_term <- function() {
  month <- lubridate::month(lubridate::now())
  year <- lubridate::year(lubridate::now())
  season <- if (month >= 8) "Fall" else if (month >= 5) "Summer" else "Spring"
  return(paste(season, year))
}

find_term <- function(terms, current_term = get_current_term(), years_shift) {
  term <- which(levels(terms) == current_term) + years_shift * 3
  if (term < 1) term <- 1
  if (term > length(levels(terms))) term <- length(levels(terms))
  return(levels(terms)[term])
}

filter_terms <- function(terms, start_term, end_term) {
  terms[terms >= start_term & terms <= end_term] |> droplevels()
}

# get available terms based on first term current term (based on current year) and number of years past current
get_available_terms <- function(first_term, n_years_past_current = 5) {
  current_term <- get_current_term()
  terms <-
    paste(c("Spring", "Summer", "Fall"),
          rep(readr::parse_number(first_term):(readr::parse_number(current_term) + n_years_past_current + 1), each = 3)) |>
    forcats::fct_inorder(ordered = TRUE)
  available_terms <- filter_terms(terms, first_term, find_term(terms, current_term, n_years_past_current))
  return(available_terms)
}

# combine schedule
combine_schedule <- function(schedule, not_teaching, instructors, classes, selected_terms, new_line = "\n") {

  # safety checks
  stopifnot(
    "`schedule` needs to be a data frame" = is.data.frame(schedule),
    "`not_teaching` needs to be a data frame" = is.data.frame(not_teaching),
    "`instructor` needs to be a data frame" = is.data.frame(instructors),
    "`classes` needs to be a data frame" = is.data.frame(classes),
    "`selected_terms` needs to be a factor" = is.factor(selected_terms)
  )
  current_term <- get_current_term()

  # combine all info
  schedule_combined <-
    dplyr::bind_rows(
      # actual schedule
      schedule |> dplyr::filter(!.data$deleted),
      # not teaching
      not_teaching |> dplyr::mutate(class = "XXXX0000"),
      # future classes
      dplyr::tibble(
        term = as.character(!!selected_terms),
        class = "XXXX9999",
        instructor_id = "other"
      )
    ) |>
    # filter for selected terms
    dplyr::filter(.data$term %in% !!selected_terms) |>
    # info + factorize
    dplyr::mutate(
      term = factor(.data$term, levels = levels(!!selected_terms)),
      class = factor(.data$class, levels = levels(classes$class)),
      info =
        dplyr::case_when(
          .data$canceled ~ "canceled",
          # all information (bt default)
          #TRUE ~ sprintf("%s %s-%s\n%s %s\n%s students", days, start_time, end_time, building, room, enrollment)
          TRUE ~ sprintf("%s %s-%s", days, start_time, end_time)
        )
    ) |>
    droplevels() |>
    # make sure to account for same instructor teaching multiple sections of the same class (e.g. in 1030)
    dplyr::summarize(info = paste(info, collapse = new_line), .by = c(term, class, subtitle, instructor_id)) |>
    # pivot wider
    tidyr::pivot_wider(id_cols = c(class, subtitle, instructor_id), names_from = term, values_from = info) |>
    # remove the future classes placeholder
    dplyr::filter(.data$class != "XXXX9999") |>
    # fill in past classes' NAs with no
    dplyr::mutate(across(dplyr::any_of(selected_terms[selected_terms <= current_term]), ~ifelse(!is.na(.x), .x, "no"))) |>
    dplyr::mutate(across(dplyr::any_of(selected_terms[selected_terms > current_term]),
                  ~if (any(!is.na(.x))) { ifelse(!is.na(.x), .x, "no") } else { "?" }), .by = instructor_id) |>
    # remove the not teaching placeholder
    dplyr::filter(.data$class != "XXXX0000") |>
    # add instructor information
    dplyr::left_join(
      instructors |>
        dplyr::mutate(
          instructor =
            sprintf("%s %s", first_name, last_name) |>
            paste0(ifelse(inactive, " (inactive)", ""))
        ) |>
        dplyr::arrange(.data$first_name) |>
        dplyr::mutate(instructor = forcats::as_factor(instructor)) |>
        dplyr::select("instructor_id", "instructor"),
      by = "instructor_id"
    ) |>
    # add class details
    dplyr::left_join(
      classes |> dplyr::select("class", "title", "credits", "inactive"),
      by = "class"
    ) |>
    dplyr::mutate(
      full_title =
        sprintf("%s (%.0f) - %s", class, credits, title) |>
        paste0(ifelse(!is.na(subtitle), sprintf(": %s", subtitle), "")) |>
        paste0(ifelse(inactive, " (inactive)", ""))
    ) |>
    # select which columns
    dplyr::select(class, full_title, instructor, dplyr::everything()) |>
    # arrange
    dplyr::arrange(.data$class, .data$full_title, .data$instructor)

  return(schedule_combined)
}

