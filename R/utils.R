utils_write_csv_safe <- function(df, path) {
  try(utils_dir_create_if_missing(dirname(path)), silent = TRUE)
  try(utils_base_write_csv(df, path), silent = TRUE)
}

utils_dir_create_if_missing <- function(dir_path) {
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  }
}

utils_base_write_csv <- function(df, path) {
  if (is.null(df)) return(invisible(NULL))
  try(utils::write.table(df, file = path, sep = ",", row.names = FALSE, col.names = TRUE, qmethod = "double"), silent = TRUE)
}

add_strikezone_9box <- function(p, xmin = -0.83, xmax = 0.83, ymin = 1.5, ymax = 3.5) {
  p +
    ggplot2::annotate("rect", xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                      fill = NA, color = "black", linewidth = 0.4)
}

make_player7_2d_drift_plot <- function(drift_data) {
  if (nrow(drift_data) == 0) {
    stop("2D-Drift data is empty.")
  }

  name_cols <- c("Last & first names", "Last & First Names", "last_and_first_names")
  player_col <- intersect(names(drift_data), name_cols)
  if (length(player_col) == 0) {
    stop("Could not find a 'Last & first names' style column in 2D-Drift sheet.")
  }
  player_col <- player_col[1]

  player_df <- drift_data[drift_data[[player_col]] == "Player 7", , drop = FALSE]
  if (nrow(player_df) == 0) {
    stop("No 2D-Drift rows found for Player 7.")
  }

  wanted_tests <- c(
    "5 JUMPS SINGLE LEG RIGHT 2D DRIFT",
    "5 JUMPS SINGLE LEG LEFT 2D DRIFT"
  )

  player_df <- player_df[player_df$Test %in% wanted_tests, , drop = FALSE]
  if (nrow(player_df) == 0) {
    stop("Player 7 has no matching 2D-Drift tests.")
  }

  if (!all(c("WalkingPointX", "WalkingPointY") %in% names(player_df))) {
    stop("Expected WalkingPointX and WalkingPointY columns in 2D-Drift sheet.")
  }

  jump_col_candidates <- c("#", "Jump", "JumpNumber", "Jump_Number")
  jump_col <- intersect(names(player_df), jump_col_candidates)

  if (length(jump_col) > 0) {
    player_df$jump_num <- as.integer(player_df[[jump_col[1]]])
  } else {
    time_like <- intersect(c("Time", "Date", "DateTime", "Timestamp"), names(player_df))
    if (length(time_like) > 0) {
      player_df <- dplyr::group_by(player_df, Test)
      player_df <- dplyr::arrange(player_df, dplyr::across(dplyr::all_of(time_like)), .by_group = TRUE)
      player_df <- dplyr::mutate(player_df, jump_num = dplyr::row_number())
      player_df <- dplyr::ungroup(player_df)
    } else {
      player_df <- dplyr::group_by(player_df, Test)
      player_df <- dplyr::mutate(player_df, jump_num = dplyr::row_number())
      player_df <- dplyr::ungroup(player_df)
    }
  }

  # Ensure integer jump numbers
  player_df$jump_num <- as.integer(player_df$jump_num)

  # Keep only rows with valid coordinates and jump numbers
  player_df <- dplyr::filter(
    player_df,
    !is.na(WalkingPointX),
    !is.na(WalkingPointY),
    !is.na(jump_num)
  )

  # Sort within each Test by any available time column and jump number,
  # then remove duplicate (Test, jump_num) pairs keeping the first row
  time_like <- intersect(c("Time", "Date", "DateTime", "Timestamp"), names(player_df))
  if (length(time_like) > 0) {
    player_df <- player_df |>
      dplyr::arrange(Test, dplyr::across(dplyr::all_of(time_like)), jump_num)
  } else {
    player_df <- player_df |>
      dplyr::arrange(Test, jump_num)
  }

  player_df <- player_df |>
    dplyr::group_by(Test, jump_num) |>
    dplyr::slice_head(n = 1) |>
    dplyr::ungroup()

  # Keep only jumps 1–5 for plotting (origin added later as 0)
  player_df <- dplyr::filter(player_df, jump_num %in% 1:5)

  player_df <- dplyr::mutate(
    player_df,
    leg = dplyr::case_when(
      grepl("RIGHT", Test, ignore.case = TRUE) ~ "RIGHT",
      grepl("LEFT", Test, ignore.case = TRUE) ~ "LEFT",
      TRUE ~ "UNKNOWN"
    )
  )

  origin_rows <- player_df |>
    dplyr::distinct(Test, leg) |>
    dplyr::mutate(
      WalkingPointX = 0,
      WalkingPointY = 0,
      jump_num = 0
    )

  plot_df <- dplyr::bind_rows(origin_rows, player_df) |>
    dplyr::arrange(Test, jump_num)

  x_lim <- max(abs(plot_df$WalkingPointX), na.rm = TRUE)
  y_lim <- max(abs(plot_df$WalkingPointY), na.rm = TRUE)
  lim <- max(x_lim, y_lim)
  if (!is.finite(lim) || lim == 0) {
    lim <- 1
  }

  ggplot2::ggplot(plot_df, ggplot2::aes(x = WalkingPointX, y = WalkingPointY)) +
    ggplot2::geom_path(ggplot2::aes(group = Test, color = leg), alpha = 0.7) +
    ggplot2::geom_point(ggplot2::aes(color = leg), size = 2) +
    ggplot2::geom_text(
      data = subset(plot_df, jump_num > 0),
      ggplot2::aes(label = jump_num),
      vjust = -0.8,
      size = 3
    ) +
    ggplot2::facet_wrap(~leg, nrow = 1) +
    ggplot2::coord_equal(xlim = c(-lim, lim), ylim = c(-lim, lim)) +
    ggplot2::labs(
      title = "Player 7 - 2D Drift Jumps",
      x = "WalkingPointX",
      y = "WalkingPointY",
      color = "Leg"
    ) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom"
    )
}

