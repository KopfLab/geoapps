# data prep ====

prepare_classes <- function(classes) {
  classes |>
    # not teaching placeholder
    dplyr::bind_rows(dplyr::tibble(class = "XXXX0000", title = "not teaching placeholder")) |>
    # future classes placeholder
    dplyr::bind_rows(dplyr::tibble(class = "XXXX9999", title = "future classes placeholder")) |>
    dplyr::mutate(
      class = stringr::str_remove_all(.data$class, "[ \\r\\n]"),
      inactive = !is.na(.data$inactive) & .data$inactive,
      class = forcats::as_factor(.data$class)
    )
}

prepare_instructors <- function(instructors) {
  instructors |>
    dplyr::mutate(
      instructor_id = stringr::str_remove_all(.data$instructor_id, "[ \\r\\n]"),
      inactive = !is.na(.data$inactive) & .data$inactive,
      full_name = sprintf("%s %s", .data$first_name, .data$last_name)
    )
}

prepare_not_teaching <- function(not_teaching) {
  not_teaching |>
    # keep track of index (for editing purposes)
    dplyr::mutate(
      idx = dplyr::row_number(),
      .before = 1L
    ) |>
    # only get the latest not teaching definition if there are multiple for a semester
    dplyr::filter(
      .by = c("instructor_id", "term"),
      dplyr::row_number() == dplyr::n()
    ) |>
    # remove deleted records
    dplyr::filter(is.na(.data$deleted))
}

prepare_schedule <- function(schedule) {
  # safety checks
  schedule |>
    # keep track of index (for editing purposes)
    dplyr::mutate(
      idx = dplyr::row_number(),
      .before = 1L
    ) |>
    # note that this is an experimental function in tidyr
    tidyr::separate_longer_delim("instructor_id", delim = ",") |>
    dplyr::mutate(
      class = stringr::str_remove_all(class, "[ \\r\\n]"),
      instructor_id = stringr::str_remove_all(instructor_id, "[ \\r\\n]"),
      canceled = !is.na(.data$canceled) & .data$canceled,
      confirmed = !is.na(.data$confirmed) & .data$confirmed
    ) |>
    dplyr::mutate(
      instructor_id = ifelse(!is.na(.data$instructor_id) & nchar(.data$instructor_id) > 0,
                             .data$instructor_id, "none")
    ) |>
    # remove deleted records
    dplyr::filter(is.na(.data$deleted))
}

prepare_rooms <- function(schedule) {
  schedule |>
    dplyr::select("building", "room") |>
    dplyr::distinct() |>
    dplyr::filter(!is.na(.data$building), !is.na(.data$room)) |>
    dplyr::filter(.data$building == "BESC") |>
    dplyr::mutate(label = paste(.data$building, .data$room)) |>
    dplyr::pull("label") |> sort()
}

prepare_teaching_times <- function(schedule) {
  schedule |>
    dplyr::select("days", "start_time", "end_time") |>
    dplyr::distinct() |>
    dplyr::filter(!is.na(.data$days), !is.na(.data$start_time), !is.na(.data$end_time)) |>
    dplyr::mutate(
      days = factor(stringr::str_to_upper(.data$days)) |> forcats::fct_relevel("MWF", "TTH", "MW", "WF"),
      time = as.POSIXct(paste0(.data$start_time, "m"), format = "%I:%M%p")
    ) |>
    dplyr::arrange(.data$days, .data$time) |>
    dplyr::mutate(label = sprintf("%s: %s-%s", .data$days, .data$start_time, .data$end_time)) |>
    dplyr::pull("label")
}

# FIXME: timezone hardcoded for this application
get_datetime <- function(tz = "America/Denver") {
  lubridate::now() |>
    lubridate::with_tz(tz) |>
    lubridate::force_tz("UTC")
}

# terms =======

get_term_regexp <- function() {
  return("(Spring|Summer|Fall) \\d{4}")
}

check_terms <- function(terms) {
  stringr::str_detect(terms, get_term_regexp())
}

get_current_term <- function() {
  month <- lubridate::month(lubridate::now())
  year <- lubridate::year(lubridate::now())
  season <- if (month >= 8) "Fall" else if (month >= 5) "Summer" else "Spring"
  return(paste(season, year))
}

get_past_or_future_term <- function(reference_term = get_current_term(), years_shift = 0, season = get_term_season(reference_term)) {
  paste(season, get_term_year(reference_term) + years_shift)
}

get_term_year <- function(term) {
  term |> stringr::str_extract("\\d{4}") |> as.integer()
}

get_term_season <- function(term) {
  term |> stringr::str_extract("Spring|Summer|Fall") |>
    factor(levels = c("Spring", "Summer", "Fall"), ordered = TRUE)
}

# calculates the difference between two terms
calculate_term_difference <- function(term1, term2) {
  years_diff <- get_term_year(term2) - get_term_year(term1)
  seasons_diff <- as.integer(get_term_season(term2)) - as.integer(get_term_season(term1))
  return(years_diff * 3 + seasons_diff)
}

# check if a term is after another one
is_term_after <- function(terms, after = get_current_term(), include_equal = FALSE) {
  if (include_equal)
    calculate_term_difference(after, terms) >= 0
  else
    calculate_term_difference(after, terms) > 0
}
# check if a term is before another one
is_term_before <- function(terms, before = get_current_term(), include_equal = FALSE) {
  if (include_equal)
    calculate_term_difference(before, terms) <= 0
  else
    calculate_term_difference(before, terms) < 0
}

# filter terms depending on start/end
filter_terms <- function(terms, start_term = NULL, end_term = NULL, inclusive = TRUE) {
  # saftey checks
  stopifnot(
    "`terms` need to be valid terms" = terms |> check_terms() |> all(),
    "`start_term` needs to be a valid term if provided" = is.null(start_term) || check_terms(start_term),
    "`end_term` needs to be a valid term if provided" = is.null(end_term) || check_terms(end_term)
  )

  if (!is.null(start_term))
    terms <- terms[is_term_after(terms, after = start_term, include_equal = inclusive)]

  if (!is.null(end_term))
    terms <- terms[is_term_before(terms, before = end_term, include_equal = inclusive)]

  return(terms)
}

# get available terms based on first term and current term (based on current year) and number of years past current
get_available_terms <- function(first_term, n_years_past_current = 5) {
  current_term <- get_current_term()
  available_terms <-
    paste(
      c("Spring", "Summer", "Fall"),
      rep(get_term_year(first_term):(get_term_year(current_term) + n_years_past_current + 1), each = 3)
    ) |>
    filter_terms(
      start_term = first_term,
      end_term = get_past_or_future_term(years_shift = n_years_past_current)
    )
  return(available_terms)
}

# drop summers
drop_summers <- function(terms) {
  terms[get_term_season(terms) != "Summer"]
}

# sort terms (past, current, future)
get_sorted_terms <- function(terms) {
  current_term <- get_current_term()
  past <- filter_terms(terms, end_term = current_term, inclusive = FALSE) |> as.list()
  future <- filter_terms(terms, start_term = current_term, inclusive = FALSE) |> as.list()
  c(
    if (length(past) > 0) list(Past = past),
    if (any(calculate_term_difference(terms, current_term) == 0)) list(Current = list(current_term)),
    if (length(future) > 0) list(Future = future)
  )
}

# schedule ========

# quick check for practical nonempty strings
string_not_empty <- function(s) {
  return(!is.na(s) & !stringr::str_detect(s, "^[\\s\\r\\n]*$"))
}
# helper to replace non empty strings with default
string_not_empty_or_default <- function(s, default = "?") {
  ifelse(string_not_empty(s), s, default)
}

# combine information
combine_information <- function(
    section, days, start_time, end_time, building, room, enrollment, enrollment_cap, confirmed,
    include_section_nr, include_day_time, include_location, include_enrollment,
    unconfirmed_tags = c("<i><u>", "</u></i>")) {

  if (length(section) == 0) return(character(0))

  # start empty
  section_info <- day_time_info <- location_info <- enrollment_info <- rep("", length(section))

  # section info
  if (include_section_nr) {
    section_info <- sprintf("#%s", string_not_empty_or_default(section, "?"))
    if (include_day_time || include_location || include_enrollment)
      section_info <- paste0(section_info, ": ")
  }

  # day time info
  if (include_day_time) {
    day_time_info <-
      dplyr::case_when(
        string_not_empty(days) & string_not_empty(start_time) & string_not_empty(end_time) ~
          sprintf("%s %s-%s", days, start_time, end_time),
        string_not_empty(days) & !string_not_empty(start_time) & !string_not_empty(end_time) ~
          sprintf("%s ?", days),
        !string_not_empty(days) & !string_not_empty(start_time) & !string_not_empty(end_time) ~
          "?",
        TRUE ~ sprintf(
          "%s %s-%s", string_not_empty_or_default(days, "?"),
          string_not_empty_or_default(start_time, "?"), string_not_empty_or_default(end_time, "?"))
      )
  }

  # location info
  if (include_location) {
    location_info <- sprintf(
      "in %s%s", string_not_empty_or_default(building, "?"),
      string_not_empty_or_default(room, "?"))
    if (include_day_time) location_info <- paste0(" ", location_info)
  }

  # enrollment info
  if (include_enrollment) {
    enrollment_info <-
      dplyr::case_when(
        string_not_empty(enrollment) & string_not_empty(enrollment_cap) ~
          sprintf("%s / %s students", enrollment, enrollment_cap),
        string_not_empty(enrollment) ~ sprintf("%s students", enrollment),
        string_not_empty(enrollment_cap) ~ sprintf("max %s students", enrollment_cap),
        TRUE ~ "room max"
      )
    if (include_day_time || include_location) enrollment_info <- paste0(", ", enrollment_info)
  }

  info <- paste0(section_info, day_time_info, location_info, enrollment_info)
  # if all just placeholders, mark as "yes"
  info <- ifelse(stringr::str_detect(info, "^([?#:, ]|in|room max)*$"), "yes", info)
  # wrap info in italics if not confirmed
  info <- ifelse(!confirmed, paste0(unconfirmed_tags[1], info, unconfirmed_tags[2]), info)
  #return
  return(info)
}

# combine schedule
# @param selected_terms the terms to include (will be listed in the order listed in the selected_terms vector)
# @param instructor_schedule if provided, lists all classes ever taught by the faculty (no matter what term) and includes other faculty that taught those classes in the listing
combine_schedule <- function(
    schedule, not_teaching, instructors, classes, selected_terms,
    separator = "\n", recognized_reasons = c(),
    include_section_nr = TRUE,
    include_day_time = TRUE,
    include_location = TRUE,
    include_enrollment = TRUE,
    instructor_schedule = NULL
    ) {

  # safety checks
  stopifnot(
    "`schedule` needs to be a data frame" = is.data.frame(schedule),
    "`not_teaching` needs to be a data frame" = is.data.frame(not_teaching),
    "`instructor` needs to be a data frame" = is.data.frame(instructors),
    "`classes` needs to be a data frame" = is.data.frame(classes),
    "`selected_terms` needs to be a character vector" = is.character(selected_terms)
  )

  # combine all info
  schedule_combined <-
    dplyr::bind_rows(
      # actual schedule (except deleted records)
      schedule |>
        dplyr::filter(is.na(.data$deleted)) |>
        dplyr::select(-"created", -"updated", -"deleted"),
      # not teaching (except deleted records)
      not_teaching |>
        dplyr::filter(is.na(.data$deleted)) |>
        dplyr::select(-"created", -"deleted") |>
        dplyr::mutate(class = "XXXX0000"),
      # inactive (to flag as NOT teaching)
      instructors |>
        dplyr::filter(.data$inactive) |>
        tidyr::crossing(term = !!selected_terms) |>
        dplyr::mutate(class = "XXXX0000"),
      # future classes placeholder
      dplyr::tibble(
        term = !!selected_terms,
        class = "XXXX9999",
        instructor_id = "other"
      )
    )

  # filter for selected terms (or everything for the selected instructor)
  if (!is.null(instructor_schedule)) {
    schedule_combined <- schedule_combined |>
      dplyr::filter(.data$term %in% !!selected_terms | .data$instructor_id == !!instructor_schedule)
  } else {
    schedule_combined <- schedule_combined |>
      dplyr::filter(.data$term %in% !!selected_terms)
  }

  # fill empty
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

  # factorize and add info
  current_term <- get_current_term()
  schedule_combined <- schedule_combined |>
    dplyr::mutate(
      class = factor(.data$class, levels = levels(classes$class)),
      info =
        dplyr::case_when(
          .data$canceled ~ "canceled",
          .data$class == "XXXX0000" & !is.na(.data$reason) ~ .data$reason,
          !include_section_nr && !include_day_time && !include_location && !include_enrollment ~ "yes",
          TRUE ~ combine_information(
            .data$section, .data$days, .data$start_time, .data$end_time, .data$building, .data$room, .data$enrollment, .data$enrollment_cap, .data$confirmed,
            include_section_nr, include_day_time, include_location, include_enrollment)
        )
    ) |>
    droplevels() |>
    # make sure to account for same instructor teaching multiple sections of the same class (e.g. in 1030)
    dplyr::summarize(info = paste(info, collapse = separator), .by = c(term, class, subtitle, instructor_id)) |>
    # pivot wider (sort by terms first to make sure pivoted columns have the right order)
    dplyr::arrange(get_term_year(term), get_term_season(term)) |>
    tidyr::pivot_wider(id_cols = c(class, subtitle, instructor_id), names_from = term, values_from = info) |>
    # remove the future classes placeholder
    dplyr::filter(.data$class != "XXXX9999") |>
    # for each instructor fill in past and future values
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(selected_terms[is_term_before(selected_terms, before = current_term, include_equal = TRUE)]),
        ~fill_empty(.x)
      ),
      dplyr::across(
        dplyr::any_of(selected_terms[is_term_after(selected_terms, after = current_term)]),
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
                        forcats::fct_relevel("Other Instructor", "Postdoctoral Scholar", "Graduate Student", "Noone Assigned", after = Inf)) |>
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
    )

  # focus on instructor_schedule if provided
  if (!is.null(instructor_schedule)) {
    schedule_combined <- schedule_combined |>
      dplyr::filter(.by = c("class", "full_title"), any(.data$instructor_id == !!instructor_schedule))
  }

  # return
  schedule_combined |>
    # select which columns
    dplyr::select("class", "full_title", "instructor_id", "instructor", !!!selected_terms) |>
    # arrange
    dplyr::arrange(.data$class, .data$full_title, .data$instructor) |>
    dplyr::mutate(row = dplyr::row_number(), .before = 1L)
}

