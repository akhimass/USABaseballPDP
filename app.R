library(shiny)
library(htmltools)
library(readxl)
library(dplyr)
library(ggplot2)
library(gt)
library(scales)
library(janitor)

addResourcePath("outputs", "outputs")
source("R/utils.R")

# ---- Pitcher table helpers (same logic as Rmd) ----
make_pitcher_tables <- function(trackman_raw) {
  pitcher_df <- trackman_raw %>% filter(Pitcher == "Pitcher 2")
  if (nrow(pitcher_df) == 0) return(list(gt1 = NULL, gt2 = NULL, gt3 = NULL))

  pitcher_df <- pitcher_df %>% mutate(
    pitch_type = case_when(
      !is.na(TaggedPitchType) & TaggedPitchType != "" ~ TaggedPitchType,
      !is.na(AutoPitchType) & AutoPitchType != "" ~ AutoPitchType,
      TRUE ~ "Unknown"
    )
  )
  total_pitches <- nrow(pitcher_df)
  valid_calls <- unique(stats::na.omit(trackman_raw$PitchCall))
  strike_calls_full <- c("StrikeCalled", "StrikeSwinging", "FoulBall", "FoulBallNotFieldable", "FoulBallFieldable", "InPlay")
  swings_calls_full <- c("StrikeSwinging", "FoulBall", "FoulBallNotFieldable", "FoulBallFieldable", "InPlay")
  strike_calls <- intersect(strike_calls_full, valid_calls)
  swings_calls <- intersect(swings_calls_full, valid_calls)
  if (length(strike_calls) == 0) strike_calls <- c("StrikeCalled", "StrikeSwinging", "FoulBallNotFieldable", "InPlay")
  if (length(swings_calls) == 0) swings_calls <- c("StrikeSwinging", "FoulBallNotFieldable", "InPlay")

  strike_pitches <- sum(pitcher_df$PitchCall %in% strike_calls, na.rm = TRUE)
  swings <- sum(pitcher_df$PitchCall %in% swings_calls, na.rm = TRUE)
  whiffs <- sum(pitcher_df$PitchCall == "StrikeSwinging", na.rm = TRUE)
  strike_pct <- ifelse(total_pitches > 0, strike_pitches / total_pitches, NA_real_)
  swing_pct <- ifelse(total_pitches > 0, swings / total_pitches, NA_real_)
  whiff_pct <- ifelse(swings > 0, whiffs / swings, NA_real_)

  pitch_mix_df <- pitcher_df %>% filter(!is.na(pitch_type), pitch_type != "") %>%
    count(pitch_type, name = "n") %>% mutate(pct = n / sum(n), pitch_type = stats::reorder(pitch_type, pct))
  velo_spin_by_type <- pitcher_df %>% group_by(pitch_type) %>% summarise(
    pitches = n(), avg_velo = mean(RelSpeed, na.rm = TRUE), max_velo = max(RelSpeed, na.rm = TRUE),
    avg_spin = mean(SpinRate, na.rm = TRUE), .groups = "drop"
  )

  gt1 <- tibble::tibble(
    Metric = c("Total pitches", "Strike%", "Swing%", "Whiff%"),
    Value = c(as.character(total_pitches), scales::percent(strike_pct, 0.1), scales::percent(swing_pct, 0.1),
              ifelse(is.na(whiff_pct), "N/A", scales::percent(whiff_pct, 0.1)))
  ) %>% gt() %>% cols_label(Metric = "Metric", Value = "Value") %>% tab_header(title = "Summary") %>%
    tab_options(table.font.size = px(10), data_row.padding = px(3), heading.padding = px(3))

  pct_match <- if (nrow(pitch_mix_df) > 0) pitch_mix_df %>% as.data.frame() %>% select(pitch_type, pct) else NULL
  velo_spin_gt <- velo_spin_by_type %>% mutate(
    `Pitch Mix %` = if (!is.null(pct_match)) {
      mix_joined <- left_join(velo_spin_by_type, pct_match, by = "pitch_type")
      scales::percent(replace(mix_joined$pct, is.na(mix_joined$pct), 0), 0.1)
    } else NA_character_,
    `Avg Velo` = round(avg_velo, 1), `Max Velo` = round(max_velo, 1), `Avg Spin` = round(avg_spin, 0)
  ) %>% select(`Pitch Type` = pitch_type, `Pitch Mix %`, `Pitches` = pitches, `Avg Velo`, `Max Velo`, `Avg Spin`)
  gt2 <- velo_spin_gt %>% gt() %>% tab_header(title = "Pitch Type (Velo, Spin)") %>%
    tab_options(table.font.size = px(10), data_row.padding = px(3), heading.padding = px(3))

  pitch_call_tbl <- pitcher_df %>% count(PitchCall, name = "Count") %>%
    mutate(Percent = Count / sum(Count), Percent = scales::percent(Percent, 0.1)) %>% arrange(desc(Count))
  gt3 <- if (nrow(pitch_call_tbl) > 0) {
    pitch_call_tbl %>% gt() %>% cols_label(PitchCall = "Pitch Call", Count = "Count", Percent = "Percent") %>%
      tab_header(title = "PitchCall Distribution") %>%
      tab_options(table.font.size = px(10), data_row.padding = px(3), heading.padding = px(3))
  } else NULL

  list(gt1 = gt1, gt2 = gt2, gt3 = gt3, pitcher_df = pitcher_df, pitch_mix_df = pitch_mix_df)
}

# ---- Hitter table helpers ----
make_hitter_tables <- function(trackman_raw) {
  hitter_df <- trackman_raw %>% filter(Batter == "Hitter 5")
  if (nrow(hitter_df) == 0) return(list(gt1 = NULL, gt2 = NULL, hitter_df = NULL, bb_df = NULL, swings_calls = character(0)))

  valid_calls <- unique(stats::na.omit(trackman_raw$PitchCall))
  strike_calls_full <- c("StrikeCalled", "StrikeSwinging", "FoulBall", "FoulBallNotFieldable", "FoulBallFieldable", "InPlay")
  swings_calls_full <- c("StrikeSwinging", "FoulBall", "FoulBallNotFieldable", "FoulBallFieldable", "InPlay")
  strike_calls <- intersect(strike_calls_full, valid_calls)
  swings_calls <- intersect(swings_calls_full, valid_calls)
  if (length(strike_calls) == 0) strike_calls <- c("StrikeCalled", "StrikeSwinging", "FoulBallNotFieldable", "InPlay")
  if (length(swings_calls) == 0) swings_calls <- c("StrikeSwinging", "FoulBallNotFieldable", "InPlay")

  total_pitches <- nrow(hitter_df)
  strike_pitches <- sum(hitter_df$PitchCall %in% strike_calls, na.rm = TRUE)
  swings <- sum(hitter_df$PitchCall %in% swings_calls, na.rm = TRUE)
  whiffs <- sum(hitter_df$PitchCall == "StrikeSwinging", na.rm = TRUE)
  in_play <- sum(hitter_df$PitchCall == "InPlay", na.rm = TRUE)
  ball_calls <- c("BallCalled"); if ("BallinDirt" %in% valid_calls) ball_calls <- union(ball_calls, "BallinDirt")
  ball_pitches <- sum(hitter_df$PitchCall %in% ball_calls, na.rm = TRUE)
  ball_pct <- ifelse(total_pitches > 0, ball_pitches / total_pitches, NA_real_)
  strike_pct <- ifelse(total_pitches > 0, strike_pitches / total_pitches, NA_real_)
  swing_pct <- ifelse(total_pitches > 0, swings / total_pitches, NA_real_)
  whiff_pct <- ifelse(swings > 0, whiffs / swings, NA_real_)

  bb_df <- hitter_df %>% filter(!is.na(ExitSpeed))
  avg_ev <- if (nrow(bb_df) > 0) mean(bb_df$ExitSpeed, na.rm = TRUE) else NA_real_
  max_ev <- if (nrow(bb_df) > 0) max(bb_df$ExitSpeed, na.rm = TRUE) else NA_real_
  avg_la <- if (nrow(bb_df) > 0) mean(bb_df$Angle, na.rm = TRUE) else NA_real_
  p90_ev <- if (nrow(bb_df) > 0) as.numeric(stats::quantile(bb_df$ExitSpeed, 0.9, na.rm = TRUE, type = 7)) else NA_real_

  overall_tbl <- tibble::tibble(
    Metric = c("Total pitches seen", "Ball%", "Strike%", "Swing%", "Whiff%", "InPlay count",
               "Avg ExitSpeed (batted balls)", "Max ExitSpeed (batted balls)", "Avg Launch Angle (batted balls)", "90th percentile ExitSpeed"),
    Value = c(as.character(total_pitches), scales::percent(ball_pct, 0.1), scales::percent(strike_pct, 0.1),
              scales::percent(swing_pct, 0.1), ifelse(is.na(whiff_pct), "N/A", scales::percent(whiff_pct, 0.1)),
              as.character(in_play), ifelse(is.na(avg_ev), "N/A", as.character(round(avg_ev, 1))),
              ifelse(is.na(max_ev), "N/A", as.character(round(max_ev, 1))), ifelse(is.na(avg_la), "N/A", as.character(round(avg_la, 1))),
              ifelse(is.na(p90_ev), "N/A", as.character(round(p90_ev, 1))))
  )
  gt1 <- overall_tbl %>% gt() %>% cols_label(Metric = "Metric", Value = "Value") %>%
    tab_header(title = "Plate Discipline & Outcomes") %>%
    tab_options(table.font.size = px(10), data_row.padding = px(3), heading.padding = px(3))

  results_tbl <- bb_df %>% mutate(
    result_key = case_when(
      !is.na(PlayResult) & !is.na(TaggedHitType) ~ paste(PlayResult, "-", TaggedHitType),
      !is.na(PlayResult) ~ PlayResult, !is.na(TaggedHitType) ~ TaggedHitType, TRUE ~ "Unknown"
    )
  ) %>% count(result_key, name = "n") %>% mutate(pct = n / sum(n)) %>% arrange(desc(n))
  gt2 <- if (nrow(results_tbl) > 0) {
    results_tbl %>% mutate(`Percent` = scales::percent(pct, 0.1)) %>%
      select(`Result` = result_key, `Count` = n, `Percent`) %>% gt() %>%
      tab_header(title = "Results Breakdown") %>%
      tab_options(table.font.size = px(10), data_row.padding = px(3), heading.padding = px(3))
  } else {
    tibble(Note = "No batted-ball results") %>% gt() %>% tab_header(title = "Results Breakdown")
  }

  if (nrow(bb_df) > 0) {
    ev_threshold <- stats::quantile(bb_df$ExitSpeed, 0.9, na.rm = TRUE, type = 7)
    bb_df <- bb_df %>% mutate(is_top_ev = ExitSpeed >= ev_threshold)
  }
  swings_df <- hitter_df %>% filter(PitchCall %in% swings_calls)
  if (nrow(swings_df) > 0 && "PlateLocSide" %in% names(swings_df) && "PlateLocHeight" %in% names(swings_df)) {
    swings_df <- swings_df %>% mutate(
      PlateLocSide = suppressWarnings(as.numeric(PlateLocSide)),
      PlateLocHeight = suppressWarnings(as.numeric(PlateLocHeight))
    )
  }
  list(gt1 = gt1, gt2 = gt2, hitter_df = hitter_df, bb_df = bb_df, swings_df = swings_df,
       avg_ev = avg_ev, max_ev = max_ev, p90_ev = p90_ev)
}

# ---- UI ----
ui <- fluidPage(
  titlePanel("PDP Reports Viewer"),
  sidebarLayout(
    sidebarPanel(
      textInput("excel_path", "Excel file path", value = "/Users/akhichappidi/PDP/2025 PDP Data Exercise.xlsx", width = "100%"),
      actionButton("run_reports", "Run Reports", class = "btn-primary"),
      hr(), strong("Status:"), verbatimTextOutput("status", placeholder = TRUE)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Pitcher 2 Report (App View)",
          tags$div(style = "margin-bottom:16px;", htmlOutput("pitcher_summary_tbl")),
          tags$div(style = "margin-bottom:16px;", htmlOutput("pitcher_pitchtype_tbl")),
          tags$div(style = "margin-bottom:16px;", htmlOutput("pitcher_pitchcall_tbl")),
          tags$div(style = "margin-bottom:16px;", plotOutput("pitch_mix_plot", height = "260px")),
          tags$div(style = "margin-bottom:16px;", plotOutput("velo_plot", height = "260px")),
          tags$div(style = "margin-bottom:16px;", plotOutput("movement_plot", height = "260px")),
          tags$div(style = "margin-bottom:16px;", plotOutput("location_plot", height = "340px"))
        ),
        tabPanel("Hitter 5 Report (App View)",
          tags$div(style = "margin-bottom:16px;", htmlOutput("hitter_summary_tbl")),
          tags$div(style = "margin-bottom:16px;", htmlOutput("hitter_results_tbl")),
          tags$div(style = "margin-bottom:16px;", plotOutput("ev_la_plot", height = "260px")),
          tags$div(style = "margin-bottom:16px;", plotOutput("ev_summary_plot", height = "260px")),
          tags$div(style = "margin-bottom:16px;", plotOutput("spray_plot", height = "260px")),
          tags$div(style = "margin-bottom:16px;", plotOutput("swing_map_plot", height = "340px"))
        ),
        tabPanel("Player 7 2D Drift",
          br(), uiOutput("drift_png_ui"), br(),
          downloadButton("download_drift_png", "Download PNG"),
          downloadButton("download_drift_pdf", "Download PDF")
        ),
        tabPanel("Downloads",
          tags$div(style = "margin-bottom:12px;", downloadButton("download_pitcher_pdf", "Download Pitcher 2 PDF")),
          tags$div(style = "margin-bottom:12px;", downloadButton("download_hitter_pdf", "Download Hitter 5 PDF"))
        )
      )
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  status_val <- reactiveVal("Idle.")
  trackman_val <- reactiveVal(NULL)
  drift_val <- reactiveVal(NULL)

  observeEvent(input$run_reports, {
    excel_path <- input$excel_path
    status_val("Running pipeline...")
    tryCatch({
      source("run_all.R")
      if (!exists("run_pipeline")) stop("run_pipeline() not found after sourcing run_all.R")
      run_pipeline(excel_path)
      trackman_val(readxl::read_excel(excel_path, sheet = "Trackman Data") %>% janitor::remove_empty("cols"))
      drift_val(suppressWarnings(tryCatch(
        readxl::read_excel(excel_path, sheet = "2D-Drift") %>% janitor::remove_empty("cols"),
        error = function(e) NULL
      )))
      status_val("Done.")
    }, error = function(e) {
      status_val(paste("Error:", e$message))
      trackman_val(NULL)
    })
  })

  output$status <- renderText(status_val())

  # Pitcher tables
  pitcher_data <- reactive({
    df <- trackman_val()
    if (is.null(df)) return(NULL)
    make_pitcher_tables(df)
  })

  output$pitcher_summary_tbl <- renderUI({
    d <- pitcher_data(); req(d, !is.null(d$gt1))
    htmltools::HTML(gt::as_raw_html(d$gt1))
  })
  output$pitcher_pitchtype_tbl <- renderUI({
    d <- pitcher_data(); req(d, !is.null(d$gt2))
    htmltools::HTML(gt::as_raw_html(d$gt2))
  })
  output$pitcher_pitchcall_tbl <- renderUI({
    d <- pitcher_data(); req(d, !is.null(d$gt3))
    htmltools::HTML(gt::as_raw_html(d$gt3))
  })

  output$pitch_mix_plot <- renderPlot({
    d <- pitcher_data()
    if (is.null(d)) return(ggplot() + theme_void() + labs(title = "Run reports to load data."))
    if (is.null(d$pitch_mix_df) || nrow(d$pitch_mix_df) == 0) return(ggplot() + theme_void() + labs(title = "No pitch mix data."))
    ggplot(d$pitch_mix_df, aes(x = pitch_type, y = pct)) + geom_col(fill = "#2c7fb8") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + coord_flip() +
      labs(title = "Pitch Mix", x = "Pitch Type", y = "Usage %") + theme_minimal(base_size = 10) + theme(legend.position = "none")
  })
  output$velo_plot <- renderPlot({
    d <- pitcher_data()
    if (is.null(d) || is.null(d$pitcher_df)) return(ggplot() + theme_void() + labs(title = "Run reports to load data."))
    ggplot(d$pitcher_df, aes(x = pitch_type, y = RelSpeed, fill = pitch_type)) + geom_boxplot(outlier.alpha = 0.4) +
      labs(title = "Velocity", x = "Pitch Type", y = "Velocity (mph)") + coord_flip() +
      theme_minimal(base_size = 10) + theme(legend.position = "none")
  })
  output$movement_plot <- renderPlot({
    d <- pitcher_data()
    if (is.null(d) || is.null(d$pitcher_df)) return(ggplot() + theme_void() + labs(title = "Run reports to load data."))
    ggplot(d$pitcher_df, aes(x = HorzBreak, y = InducedVertBreak, color = pitch_type)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey70") +
      geom_vline(xintercept = 0, linetype = "dashed", color = "grey70") +
      geom_point(alpha = 0.7, size = 2) + coord_equal() +
      labs(title = "Movement (IVB vs HB)", x = "Horizontal Break (in)", y = "Induced Vertical Break (in)", color = "Pitch Type") +
      theme_minimal(base_size = 10) + theme(legend.position = "bottom", legend.direction = "horizontal")
  })
  output$location_plot <- renderPlot({
    d <- pitcher_data()
    if (is.null(d) || is.null(d$pitcher_df)) return(ggplot() + theme_void() + labs(title = "Run reports to load data."))
    pd <- d$pitcher_df %>% mutate(PlateLocSide = suppressWarnings(as.numeric(PlateLocSide)), PlateLocHeight = suppressWarnings(as.numeric(PlateLocHeight))) %>%
      filter(!is.na(PlateLocSide), !is.na(PlateLocHeight))
    if (nrow(pd) == 0) return(ggplot() + theme_void() + labs(title = "Pitch Location (No data)"))
    base_p <- ggplot(pd, aes(x = PlateLocSide, y = PlateLocHeight, color = pitch_type)) + geom_point(alpha = 0.6, size = 2)
    base_p <- add_strikezone_9box(base_p, -0.83, 0.83, 1.5, 3.5)
    base_p + coord_equal(xlim = c(-1.5, 1.5), ylim = c(0, 5)) +
      scale_x_continuous(breaks = seq(-1.5, 1.5, 0.5), labels = scales::number_format(accuracy = 0.1)) +
      scale_y_continuous(breaks = 0:5, labels = scales::number_format(accuracy = 0.1)) +
      labs(title = "Pitch Location", subtitle = paste0("n = ", nrow(pd)), x = "PlateLocSide (in)", y = "PlateLocHeight (in)", color = "Pitch Type") +
      theme_minimal(base_size = 10) + theme(legend.position = "bottom", legend.direction = "horizontal")
  })

  # Hitter tables and plots
  hitter_data <- reactive({
    df <- trackman_val()
    if (is.null(df)) return(NULL)
    make_hitter_tables(df)
  })

  output$hitter_summary_tbl <- renderUI({
    d <- hitter_data(); req(d, !is.null(d$gt1))
    htmltools::HTML(gt::as_raw_html(d$gt1))
  })
  output$hitter_results_tbl <- renderUI({
    d <- hitter_data(); req(d, !is.null(d$gt2))
    htmltools::HTML(gt::as_raw_html(d$gt2))
  })

  output$ev_la_plot <- renderPlot({
    d <- hitter_data()
    if (is.null(d)) return(ggplot() + theme_void() + labs(title = "Run reports to load data."))
    if (is.null(d$bb_df) || nrow(d$bb_df) == 0) return(ggplot() + theme_void() + labs(title = "No batted-ball data"))
    ggplot(d$bb_df, aes(x = Angle, y = ExitSpeed, color = is_top_ev)) + geom_point(alpha = 0.7, size = 2) +
      scale_color_manual(values = c("FALSE" = "#2c7fb8", "TRUE" = "#d73027"), labels = c("Other", "Top EV (90th+)"), name = "") +
      labs(title = "EV vs Launch Angle", x = "Launch Angle (deg)", y = "Exit Velocity (mph)") +
      theme_minimal(base_size = 10) + theme(legend.position = "bottom", legend.direction = "horizontal")
  })
  output$ev_summary_plot <- renderPlot({
    d <- hitter_data()
    if (is.null(d)) return(ggplot() + theme_void() + labs(title = "Run reports to load data."))
    if (is.null(d$bb_df) || nrow(d$bb_df) == 0) return(ggplot() + theme_void() + labs(title = "Exit Velocity (Batted Balls)", subtitle = "No data"))
    sub <- sprintf("n = %d | avg = %.1f | max = %.1f | p90 = %.1f", nrow(d$bb_df), d$avg_ev, d$max_ev, d$p90_ev)
    ggplot(d$bb_df, aes(x = 1, y = ExitSpeed)) + geom_boxplot(fill = "#2c7fb8", alpha = 0.7, outlier.alpha = 0) +
      geom_jitter(width = 0.15, height = 0, alpha = 0.6, size = 2) +
      labs(title = "Exit Velocity (Batted Balls)", subtitle = sub, x = "", y = "Exit Speed (mph)") +
      theme_minimal(base_size = 10) + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  })
  output$spray_plot <- renderPlot({
    d <- hitter_data()
    if (is.null(d)) return(ggplot() + theme_void() + labs(title = "Run reports to load data."))
    if (is.null(d$bb_df) || !"Direction" %in% names(d$bb_df) || nrow(d$bb_df) == 0) return(ggplot() + theme_void() + labs(title = "Spray (Direction not available)"))
    ggplot(d$bb_df, aes(x = Direction, y = ExitSpeed, color = PlayResult)) + geom_point(alpha = 0.7, size = 2) +
      labs(title = "Spray", x = "Direction (deg)", y = "Exit Speed (mph)", color = "Play Result") +
      theme_minimal(base_size = 10) + theme(legend.position = "bottom", legend.direction = "horizontal")
  })
  output$swing_map_plot <- renderPlot({
    d <- hitter_data()
    if (is.null(d)) return(ggplot() + theme_void() + labs(title = "Run reports to load data."))
    sd <- d$swings_df
    if (is.null(sd) || nrow(sd) == 0 || !"PlateLocSide" %in% names(sd) || !"PlateLocHeight" %in% names(sd) || sum(!is.na(sd$PlateLocSide) & !is.na(sd$PlateLocHeight)) == 0)
      return(ggplot() + theme_void() + labs(title = "Swing Map (insufficient data)"))
    p <- ggplot(sd, aes(x = PlateLocSide, y = PlateLocHeight)) + stat_bin2d(bins = 25) +
      scale_fill_gradient(low = "#f7fbff", high = "#08306b", name = "Swings") +
      coord_equal(xlim = c(-1.5, 1.5), ylim = c(0, 5)) +
      labs(title = "Swing Map", x = "PlateLocSide", y = "PlateLocHeight") + theme_minimal(base_size = 10) + theme(legend.position = "right")
    add_strikezone_9box(p, -0.83, 0.83, 1.5, 3.5)
  })

  output$drift_png_ui <- renderUI({
    path <- "outputs/Player7_2DDrift.png"
    req(file.exists(path))
    tags$div(style = "max-width: 100%; overflow: hidden;",
      tags$img(src = path, alt = "Player 7 2D Drift", style = "max-width: 100%; height: auto; display: block;"))
  })

  output$download_pitcher_pdf <- downloadHandler(
    filename = function() "Pitcher2_Pitching_Report.pdf",
    content = function(file) { req(file.exists("outputs/Pitcher2_Pitching_Report.pdf")); file.copy("outputs/Pitcher2_Pitching_Report.pdf", file, overwrite = TRUE) }
  )
  output$download_hitter_pdf <- downloadHandler(
    filename = function() "Hitter5_Hitting_Report.pdf",
    content = function(file) { req(file.exists("outputs/Hitter5_Hitting_Report.pdf")); file.copy("outputs/Hitter5_Hitting_Report.pdf", file, overwrite = TRUE) }
  )
  output$download_drift_png <- downloadHandler(
    filename = function() "Player7_2DDrift.png",
    content = function(file) { req(file.exists("outputs/Player7_2DDrift.png")); file.copy("outputs/Player7_2DDrift.png", file, overwrite = TRUE) }
  )
  output$download_drift_pdf <- downloadHandler(
    filename = function() "Player7_2DDrift.pdf",
    content = function(file) { req(file.exists("outputs/Player7_2DDrift.pdf")); file.copy("outputs/Player7_2DDrift.pdf", file, overwrite = TRUE) }
  )
}

shinyApp(ui = ui, server = server)
