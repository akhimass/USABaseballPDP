# USA Baseball PDP — Reports & Shiny App

This project generates **scouting reports** (PDF/HTML) and an **R Shiny app** for viewing Pitcher 2, Hitter 5, and Player 7 2D Drift from Trackman data in an Excel workbook.

---

## Requirements

- **R** (4.x recommended)
- R packages: `readxl`, `dplyr`, `tidyr`, `ggplot2`, `stringr`, `patchwork`, `gt`, `scales`, `janitor`, `rmarkdown`, `shiny`, `htmltools`

The pipeline script will attempt to install missing packages when you run it.

For **PDF report** generation, a LaTeX engine (e.g. [TinyTeX](https://yihui.org/tinytex/)) is required. If LaTeX is not available, only HTML reports are produced.

---

## Data

- **Input:** One Excel file with at least two sheets:
  - **Trackman Data** — pitch-level data (Pitcher, Batter, PitchCall, RelSpeed, SpinRate, ExitSpeed, Angle, PlateLocSide, PlateLocHeight, etc.)
  - **2D-Drift** — jump test data for Player 7

- Default path used in the app and Rmd params: `2025 PDP Data Exercise.xlsx` (adjust in the Shiny sidebar or in the Rmd `params` if your file is elsewhere).

---

## 1. Report pipeline (PDF & HTML)

Generate all reports and outputs from the project root:

```bash
Rscript run_all.R
```

Or from R:

```r
source("run_all.R")
run_pipeline("/path/to/your/data.xlsx")
```

**What it does:**

1. Reads the Excel **Trackman Data** and **2D-Drift** sheets (and optionally writes CSVs to `outputs/`).
2. Renders **Pitcher 2** and **Hitter 5** R Markdown reports:
   - **PDF** (one-page layout): summary tables + 2×2 visuals (if LaTeX is available).
   - **HTML**: same content, written to `outputs/`.
3. Builds **Player 7 2D Drift** plot and saves `Player7_2DDrift.png` and `Player7_2DDrift.pdf` in `outputs/`.

**Outputs (all under `outputs/`):**

| File | Description |
|------|-------------|
| `Pitcher2_Pitching_Report.pdf` | One-page pitcher scouting report (summary tables + Pitch Mix, Velocity, Movement, Location). |
| `Pitcher2_Pitching_Report.html` | Same report in HTML. |
| `Hitter5_Hitting_Report.pdf` | One-page hitter scouting report (summary tables + EV/LA, EV summary, Spray, Swing Map). |
| `Hitter5_Hitting_Report.html` | Same report in HTML. |
| `Player7_2DDrift.png` / `.pdf` | 2D Drift jump plot for Player 7. |
| `outputs/tmp/*.png` | Table/plot images used when rendering HTML. |
| `trackman_data.csv`, `2d_drift.csv` | Optional CSVs exported from the Excel sheets. |

---

## 2. Reports — content and layout

### Pitcher 2 report

- **Summary table:** Total pitches, Strike%, Swing%, Whiff%.
- **Pitch type table:** Pitch mix %, count, avg/max velocity, avg spin by pitch type.
- **Visuals (2×2):** Pitch Mix (bar), Velocity (boxplot), Movement (IVB vs horizontal break), Pitch Location (strike zone).

### Hitter 5 report

- **Plate Discipline & Outcomes:** Total pitches, Ball%, Strike%, Swing%, Whiff%, InPlay count, and batted-ball EV/LA stats (avg, max, 90th %).
- **Results Breakdown:** Count and % by result (e.g. PlayResult / TaggedHitType).
- **Visuals (2×2):** EV vs Launch Angle, Exit Velocity (box + jitter), Spray, Swing Map (plate location heatmap).

PDFs use compact headers and spacing so the report fits on one page when possible; table and figure sizes are tuned for readability.

---

## 3. R Shiny app

The Shiny app shows the **same report content** (tables and plots) **one-by-one** in the browser — no embedded PDF/HTML, so you get full-width tables and stacked plots with clear spacing.

**Run the app from the project root:**

```bash
R -e "shiny::runApp('.')"
```

Or in RStudio: open `app.R` and click **Run App**.

### App tabs

1. **Pitcher 2 Report (App View)**  
   - Summary table (gt)  
   - Pitch type table (gt)  
   - PitchCall distribution table (gt), if present  
   - Plots stacked vertically: Pitch Mix → Velocity → Movement → Location  

2. **Hitter 5 Report (App View)**  
   - Plate Discipline & Outcomes table (gt)  
   - Results Breakdown table (gt)  
   - Plots stacked vertically: EV vs LA → EV summary (box + jitter) → Spray → Swing Map  

3. **Player 7 2D Drift**  
   - Displays `outputs/Player7_2DDrift.png` (scaled to fit)  
   - Download buttons for PNG and PDF  

4. **Downloads**  
   - Download Pitcher 2 PDF  
   - Download Hitter 5 PDF  

### How the app gets data

- Click **Run Reports** in the sidebar to run the full pipeline (same as `run_all.R`) and reload the Excel file.
- The app then reads **Trackman Data** (and 2D-Drift for the drift tab) from that Excel path and builds tables and plots in memory.
- Tables and plots are built with the same logic as the Rmd reports (Pitcher 2, Hitter 5, strike/ball/swing definitions, etc.).

---

## Project structure

```
PDP/
├── README.md                 # This file
├── run_all.R                 # Pipeline: read Excel → render reports → 2D Drift
├── app.R                     # Shiny app (one-by-one tables & plots)
├── 2025 PDP Data Exercise.xlsx   # Default Excel input (or your own path)
├── R/
│   └── utils.R               # Helpers (e.g. add_strikezone_9box, 2D drift plot)
├── reports/
│   ├── pitcher2_report.Rmd   # Pitcher 2 PDF/HTML
│   ├── pitcher2_header.tex   # LaTeX header for pitcher PDF
│   ├── hitter5_report.Rmd    # Hitter 5 PDF/HTML
│   └── hitter5_header.tex    # LaTeX header for hitter PDF
├── outputs/                  # Generated reports and artifacts
│   ├── Pitcher2_Pitching_Report.pdf
│   ├── Pitcher2_Pitching_Report.html
│   ├── Hitter5_Hitting_Report.pdf
│   ├── Hitter5_Hitting_Report.html
│   ├── Player7_2DDrift.png
│   ├── Player7_2DDrift.pdf
│   └── tmp/                  # Table/plot images used in HTML
└── img/                      # Optional assets (e.g. logos; not used in app)
```

---

## Quick start

```bash
# 1. Generate all reports and outputs
Rscript run_all.R

# 2. Launch the Shiny app (optional)
R -e "shiny::runApp('.')"
```

Then open the **Pitcher 2** and **Hitter 5** tabs in the app to view tables and plots one-by-one, or open the PDF/HTML files in `outputs/` for the formatted reports.
