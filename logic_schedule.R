# terms =======

get_term_regexp <- function() {
  return("(Spring|Summer|Fall) \\d{4}")
}

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

drop_summers <- function(terms) {
  terms[!stringr::str_detect(as.character(terms), "Summer")] |> droplevels()
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
combine_schedule <- function(
    schedule, not_teaching, instructors, classes, available_terms, selected_terms,
    separator = "\n", recognized_reasons = c(),
    include_section_nr = TRUE,
    include_day_time = TRUE,
    include_location = TRUE,
    include_enrollment = TRUE) {

  # safety checks
  stopifnot(
    "`schedule` needs to be a data frame" = is.data.frame(schedule),
    "`not_teaching` needs to be a data frame" = is.data.frame(not_teaching),
    "`instructor` needs to be a data frame" = is.data.frame(instructors),
    "`classes` needs to be a data frame" = is.data.frame(classes),
    "`selected_terms` needs to be a factor" = is.factor(selected_terms)
  )
  current_term <- get_current_term()

  # recognize reasons
  fill_empty <- function(values, allow_question_mark = FALSE) {
    dplyr::case_when(
      # no values at all and question mark is allowed
      allow_question_mark & all(is.na(values)) ~ "?",
      # some values are set, use them
      !is.na(values) ~ values,
      # if any remaining values are part of the recognized reasons, substitute those
      any(values %in% recognized_reasons) ~ values[values %in% recognized_reasons][1],
      # else put in "no"
      TRUE ~ "no"
    )
  }

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
          .data$class == "XXXX0000" & !is.na(.data$reason) ~ .data$reason,
          !include_section_nr && !include_day_time && !include_location && !include_enrollment ~ "yes",
          # all information
          TRUE ~ paste(
            "\n",
            if (include_section_nr) sprintf("Section #%s", section),
            if (include_day_time) sprintf("%s %s-%s", days, start_time, end_time),
            if (include_location) sprintf("%s %s", building, room),
            if (include_enrollment) ifelse(!is.na(enrollment), sprintf("%s students", enrollment), ""),
            sep = separator
          ) |>
            stringr::str_replace_all("(\\n){2,}", "\n") |>
            stringr::str_remove_all("(^\\n|\\n$)")
        )
    ) |>
    droplevels() |>
    # make sure to account for same instructor teaching multiple sections of the same class (e.g. in 1030)
    dplyr::summarize(info = paste(info, collapse = separator), .by = c(term, class, subtitle, instructor_id)) |>
    # pivot wider
    tidyr::pivot_wider(id_cols = c(class, subtitle, instructor_id), names_from = term, values_from = info) |>
    # remove the future classes placeholder
    dplyr::filter(.data$class != "XXXX9999") |>
    # for each instructor fill in past and future values
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(available_terms[available_terms <= current_term]),
        ~fill_empty(.x)
      ),
      dplyr::across(
        dplyr::any_of(available_terms[available_terms > current_term]),
        ~fill_empty(.x, allow_question_mark = TRUE)
      ),
      .by = instructor_id
    ) |>
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
        dplyr::mutate(instructor = forcats::as_factor(instructor) |>
                        # move these levels to the end
                        # FIXME: these are hard-coded at the moment
                        forcats::fct_relevel("Other Instructor", "Postdoctoral Scholar", "Graduate Student", after = Inf)) |>
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

