# ==============================================================================
# Script: app.R ("Audiometry Calculator 2026")
# ==============================================================================
library(shiny)
library(dplyr)

# ------------------------------------------------------------------------------
# PART 1: CLINICAL CALCULATION ENGINE CORE
# ------------------------------------------------------------------------------

classify_who_grade <- function(pta) {
  if (is.na(pta)) return("N/A")
  if (pta < 20)  return("0. No impairment (<20 dB)")
  if (pta < 35)  return("1. Mild (20–34 dB)")
  if (pta < 50)  return("2. Moderate (35–49 dB)")
  if (pta < 65)  return("3. Moderately severe (50–64 dB)")
  if (pta < 80)  return("4. Severe (65–79 dB)")
  if (pta < 95)  return("5. Profound (80–94 dB)")
  return("6. Complete (>=95 dB)")
}

get_band_mean <- function(db_vec, freq_vec, target_freqs) {
  vals <- db_vec[freq_vec %in% target_freqs]
  if (length(vals) == 0 || all(is.na(vals))) return(NA_real_)
  return(mean(vals, na.rm = TRUE))
}

compute_detailed_patient_report <- function(clinical_data, target_id, target_date) {
  
  patient_data <- clinical_data %>% 
    filter(patient_id == target_id & session_date == target_date)
  
  if (nrow(patient_data) == 0) return(NULL)
  
  re_data <- patient_data %>% filter(ear == "RE")
  le_data <- patient_data %>% filter(ear == "LE")
  
  # --- AIR CONDUCTION AVERAGES ---
  ac_conv_re <- get_band_mean(re_data$ac_db, re_data$frequency, c(500, 1000, 2000, 4000))
  ac_conv_le <- get_band_mean(le_data$ac_db, le_data$frequency, c(500, 1000, 2000, 4000))
  
  ac_low_re  <- get_band_mean(re_data$ac_db, re_data$frequency, c(125, 250, 500))
  ac_low_le  <- get_band_mean(le_data$ac_db, le_data$frequency, c(125, 250, 500))
  
  ac_high_re <- get_band_mean(re_data$ac_db, re_data$frequency, c(4000, 6000, 8000))
  ac_high_le <- get_band_mean(le_data$ac_db, le_data$frequency, c(4000, 6000, 8000))
  
  ac_cent_re <- get_band_mean(re_data$ac_db, re_data$frequency, c(1000, 1500, 2000, 3000))
  ac_cent_le <- get_band_mean(le_data$ac_db, le_data$frequency, c(1000, 1500, 2000, 3000))
  
  ac_uhf_re  <- get_band_mean(re_data$ac_db, re_data$frequency, c(10000, 12000, 14000, 16000))
  ac_uhf_le  <- get_band_mean(le_data$ac_db, le_data$frequency, c(10000, 12000, 14000, 16000))
  
  # --- BONE CONDUCTION AVERAGES ---
  bc_conv_re <- get_band_mean(re_data$bc_db, re_data$frequency, c(500, 1000, 2000, 4000))
  bc_conv_le <- get_band_mean(le_data$bc_db, le_data$frequency, c(500, 1000, 2000, 4000))
  
  bc_low_re  <- get_band_mean(re_data$bc_db, re_data$frequency, c(250, 500, 750))
  bc_low_le  <- get_band_mean(le_data$bc_db, le_data$frequency, c(250, 500, 750))
  
  bc_high_re <- get_band_mean(re_data$bc_db, re_data$frequency, c(4000, 6000, 8000))
  bc_high_le <- get_band_mean(le_data$bc_db, le_data$frequency, c(4000, 6000, 8000))
  
  bc_cent_re <- get_band_mean(re_data$bc_db, re_data$frequency, c(1000, 1500, 2000, 3000))
  bc_cent_le <- get_band_mean(le_data$bc_db, le_data$frequency, c(1000, 1500, 2000, 3000))

  # --- AIR-BONE GAP (ABG) ---
  gap_conv_re <- ac_conv_re - bc_conv_re
  gap_conv_le <- ac_conv_le - bc_conv_le
  
  gap_low_re  <- ac_low_re - bc_low_re
  gap_low_le  <- ac_low_le - bc_low_le
  
  gap_high_re <- ac_high_re - bc_high_re
  gap_high_le <- ac_high_le - bc_high_le
  
  gap_cent_re <- ac_cent_re - bc_cent_re
  gap_cent_le <- ac_cent_le - bc_cent_le

  # --- LOSS PERCENTAGES & BINAURAL METRICS ---
  loss_conv_re <- max(0, (ac_conv_re - 25) * 1.5)
  loss_conv_le <- max(0, (ac_conv_le - 25) * 1.5)
  
  ama_pta_re <- get_band_mean(re_data$ac_db, re_data$frequency, c(500, 1000, 2000, 3000))
  ama_pta_le <- get_band_mean(le_data$ac_db, le_data$frequency, c(500, 1000, 2000, 3000))
  loss_ama_re <- max(0, (ama_pta_re - 25) * 1.5)
  loss_ama_le <- max(0, (ama_pta_le - 25) * 1.5)
  
  ore_pta_re <- get_band_mean(re_data$ac_db, re_data$frequency, c(500, 1000, 2000, 4000, 6000))
  ore_pta_le <- get_band_mean(le_data$ac_db, le_data$frequency, c(500, 1000, 2000, 4000, 6000))
  loss_ore_re <- max(0, (ore_pta_re - 25) * 1.5)
  loss_ore_le <- max(0, (ore_pta_le - 25) * 1.5)
  
  better_ear_conv <- min(ac_conv_re, ac_conv_le)
  binaural_loss_who <- max(0, (better_ear_conv - 25) * 1.5)
  
  binaural_loss_ama <- ((5 * min(loss_ama_re, loss_ama_le)) + max(loss_ama_re, loss_ama_le)) / 6
  binaural_loss_ore <- ((7 * min(loss_ore_re, loss_ore_le)) + max(loss_ore_re, loss_ore_le)) / 8
  
  aud_index <- 100 - ((ac_conv_re + ac_conv_le) / 2) * 0.4
  gross_ben  <- abs(ac_conv_re - ac_conv_le) * 1.2
  bbi_calc   <- (gross_ben / 16) * 100
  bhi_calc   <- (binaural_loss_ama * 1.5) + (bbi_calc * 0.7)

  # --- COCHLEO-CONSTRUCTIVE RATIOS ---
  ratio_air_hb_re <- ac_high_re / ac_low_re
  ratio_air_hb_le <- ac_high_le / ac_low_le
  ratio_air_bh_re <- ac_low_re / ac_high_re
  ratio_air_bh_le <- ac_low_le / ac_high_re
  
  ratio_bone_hb_re <- bc_high_re / bc_low_re
  ratio_bone_hb_le <- bc_high_le / bc_low_le
  ratio_bone_bh_re <- bc_low_re / bc_high_re
  ratio_bone_bh_le <- bc_low_le / bc_high_re

  ratio_gap_hb_re  <- gap_high_re / gap_low_re
  ratio_gap_hb_le  <- gap_high_le / gap_low_le
  ratio_gap_bh_re  <- gap_low_re / gap_high_re
  ratio_gap_bh_le  <- gap_low_le / gap_high_re
  
  ratio_air_uhfh_re <- ac_uhf_re / ac_high_re
  ratio_air_uhfh_le <- ac_uhf_le / ac_high_le

  # --- DYNAMIC RANGE AVERAGES ---
  re_data <- re_data %>% mutate(dr = ucl_db - ac_db)
  le_data <- le_data %>% mutate(dr = ucl_db - ac_db)
  
  dr_low_re  <- get_band_mean(re_data$dr, re_data$frequency, c(125, 250, 500))
  dr_low_le  <- get_band_mean(le_data$dr, le_data$frequency, c(125, 250, 500))
  
  dr_conv_re <- get_band_mean(re_data$dr, re_data$frequency, c(500, 1000, 2000, 4000))
  dr_conv_le <- get_band_mean(le_data$dr, le_data$frequency, c(500, 1000, 2000, 4000))
  
  dr_cent_re <- get_band_mean(re_data$dr, re_data$frequency, c(1000, 1500, 2000, 3000))
  dr_cent_le <- get_band_mean(le_data$dr, le_data$frequency, c(1000, 1500, 2000, 3000))
  
  dr_high_re <- get_band_mean(re_data$dr, re_data$frequency, c(4000, 6000, 8000))
  dr_high_le <- get_band_mean(le_data$dr, le_data$frequency, c(4000, 6000, 8000))
  
  dr_uhf_re  <- get_band_mean(re_data$dr, re_data$frequency, c(10000, 12000, 14000, 16000))
  dr_uhf_le  <- get_band_mean(le_data$dr, le_data$frequency, c(10000, 12000, 14000, 16000))
  
  dr_b1_re   <- get_band_mean(re_data$dr, re_data$frequency, c(125, 250, 500))
  dr_b1_le   <- get_band_mean(le_data$dr, le_data$frequency, c(125, 250, 500))
  
  dr_b2_re   <- get_band_mean(re_data$dr, re_data$frequency, c(750, 1000, 1500, 2000))
  dr_b2_le   <- get_band_mean(le_data$dr, le_data$frequency, c(750, 1000, 1500, 2000))
  
  dr_b3_re   <- get_band_mean(re_data$dr, re_data$frequency, c(3000, 4000, 6000))
  dr_b3_le   <- get_band_mean(le_data$dr, le_data$frequency, c(3000, 4000, 6000))
  
  dr_b4_re   <- get_band_mean(re_data$dr, re_data$frequency, c(8000, 9000, 10000))
  dr_b4_le   <- get_band_mean(le_data$dr, le_data$frequency, c(8000, 9000, 10000))

  report_text <- sprintf("
======================================================================
                    Audiometry Calculator 2026
                               R Environment
======================================================================
PATIENT ID: %s | SESSION DATE: %s
======================================================================

[1] MONAURAL TONAL AVERAGES (AIR CONDUCTION - AC):
  • RE Conversational (WHO 500-4000 Hz): %.2f dB -> %s
  • LE Conversational (WHO 500-4000 Hz): %.2f dB -> %s
  • Low Frequency Average:		RE: %.2f dB   | LE: %.2f dB
  • High Frequency Average:		RE: %.2f dB   | LE: %.2f dB
  • Central Frequency Average:		RE: %.2f dB   | LE: %.2f dB
  • Ultra-High Frequency Average (UHF):	RE: %.2f dB   | LE: %.2f dB

======================================================================
[2] MONAURAL TONAL AVERAGES (BONE CONDUCTION - BC):
  • RE Conversational Bone (500-4000 Hz):	 %.2f dB
  • LE Conversational Bone (500-4000 Hz):	 %.2f dB
  • Low Frequency Bone Average (250-750 Hz):	RE: %.2f dB  | LE: %.2f dB
  • High Frequency Bone Average (4k-8k Hz):	RE: %.2f dB  | LE: %.2f dB
  • Central Frequency Bone Average:		RE: %.2f dB  | LE: %.2f dB

======================================================================
[3] AIR-BONE GAP ANALYSIS (ABG):
  • Mean Conversational ABG:		RE: %.2f dB   | LE: %.2f dB
  • Low Frequency ABG (250-750 Hz):	RE: %.2f dB   | LE: %.2f dB
  • High Frequency ABG (4k-6k Hz):	RE: %.2f dB   | LE: %.2f dB
  • Central Frequency ABG:		RE: %.2f dB   | LE: %.2f dB

======================================================================
[4] MONAURAL HEARING LOSS PERCENTAGES:
  • Conversational Monaural Loss:	RE: %.2f %%    | LE: %.2f %%
  • AMA Criterium Monaural Loss:	RE: %.2f %% | LE: %.2f %%
  • Oregon Criterium Monaural Loss:	RE: %.2f %%    | LE: %.2f %%

======================================================================
[5] BINAURAL HEARING LOSS PERCENTAGES AND PARAMETERS:
  • TOTAL BINAURAL LOSS (WHO 5:1):		 %.2f %% -> WHO: %s
  • AMA Criterium Binaural Loss (5:1):	 %.2f %%
  • Oregon Criterium Binaural Loss (7:1):		 %.2f %%
  • Audibility Index (AI):			 %.1f %%
  • Gross Binaural Benefit (Reb):		 %.2f dB
  • Binaural Benefit Index (BBI):		 %.1f %%
  • Gross Binaural Interference:		 0.00 dB
  • Binaural Interference Index (BII):	 0.0 %%
  • BINAURAL HANDICAP INDEX (BHI):	 %.2f %%

======================================================================
[6] COCHLEO-CONSTRUCTIVE CONFIGURATION FREQUENCY RATIOS:
  • Air Ratio (High / Low Frequencies):	RE: %.2f   | LE: %.2f
  • Air Ratio (Low / High Frequencies):	RE: %.2f   | LE: %.2f
  • Bone Ratio (High / Low Frequencies):	RE: %.2f   | LE: %.2f
  • Bone Ratio (Low / High Frequencies):	RE: %.2f   | LE: %.2f
  • Gap Ratio (High / Low Frequencies):	RE: %.2f   | LE: %.2f
  • Gap Ratio (Low / High Frequencies):	RE: %.2f   | LE: %.2f
  • Air Ratio (Ultra-High / High Frequencies):	RE: %.2f   | LE: %.2f

======================================================================
[7] DYNAMIC RANGE AVERAGES:
  • Dynamic Range - Low Frequencies:	RE: %.2f dB | LE: %.2f dB
  • Dynamic Range - Conversational:	RE: %.2f dB | LE: %.2f dB
  • Dynamic Range - Central Frequencies:	RE: %.2f dB | LE: %.2f dB
  • Dynamic Range - High Frequencies:	RE: %.2f dB | LE: %.2f dB
  • Dynamic Range - Ultra-High Frequencies:	RE: %.2f dB | LE: %.2f dB

  • Specific Band 125-500 Hz:	RE: %.2f dB | LE: %.2f dB
  • Specific Band 750-2000 Hz:	RE: %.2f dB | LE: %.2f dB
  • Structured Band 3000-6000 Hz:	RE: %.2f dB | LE: %.2f dB
  • Structured Band 8000-10000 Hz:	RE: %.2f dB | LE: %.2f dB
======================================================================
", 
  target_id, target_date,
  ac_conv_re, classify_who_grade(ac_conv_re), ac_conv_le, classify_who_grade(ac_conv_le),
  ac_low_re, ac_low_le, ac_high_re, ac_high_le, ac_cent_re, ac_cent_le, ac_uhf_re, ac_uhf_le,
  bc_conv_re, bc_conv_le, bc_low_re, bc_low_le, bc_high_re, bc_high_le, bc_cent_re, bc_cent_le,
  gap_conv_re, gap_conv_le, gap_low_re, gap_low_le, gap_high_re, gap_high_le, gap_cent_re, gap_cent_le,
  loss_conv_re, loss_conv_le, loss_ama_re, loss_ama_le, loss_ore_re, loss_ore_le,
  binaural_loss_who, classify_who_grade(better_ear_conv), binaural_loss_ama, binaural_loss_ore,
  aud_index, gross_ben, bbi_calc, bhi_calc,
  ratio_air_hb_re, ratio_air_hb_le, ratio_air_bh_re, ratio_air_bh_le,
  ratio_bone_hb_re, ratio_bone_hb_le, ratio_bone_bh_re, ratio_bone_bh_le,
  ratio_gap_hb_re, ratio_gap_hb_le, ratio_gap_bh_re, ratio_gap_bh_le,
  ratio_air_uhfh_re, ratio_air_uhfh_le,
  dr_low_re, dr_low_le, dr_conv_re, dr_conv_le, dr_cent_re, dr_cent_le, dr_high_re, dr_high_le, dr_uhf_re, dr_uhf_le,
  dr_b1_re, dr_b1_le, dr_b2_re, dr_b2_le, dr_b3_re, dr_b3_le, dr_b4_re, dr_b4_le
  )
  
  return(list(text_report = report_text, plot_data = patient_data))
}

# ------------------------------------------------------------------------------
# PART 2: SHINY USER INTERFACE (UI)
# ------------------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Advanced Audiometry Analytics Platform 2026"),
  
  sidebarLayout(
    sidebarPanel(
      h4("1. Data Source Selection"),
      fileInput("uploaded_file", "Choose Audiometry CSV File:",
                accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
      
      hr(),
      h4("2. Patient & Session Selection"),
      uiOutput("patient_selector"),
      uiOutput("date_selector"),
      
      br(),
      actionButton("calculate", "Execute Clinical Calculation", class = "btn-primary", width = "100%")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Consolidated Analytical Report", 
                 verbatimTextOutput("report_output")
        )
      )
    )
  )
)

# ------------------------------------------------------------------------------
# PART 3: SHINY SERVER LOGIC
# ------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  raw_data <- reactive({
    file_info <- input$uploaded_file
    if (is.null(file_info)) {
      return(NULL)
    }
    
    first_line <- readLines(file_info$datapath, n = 1)
    detected_sep <- if (grepl(";", first_line)) ";" else ","
    
    read.csv(file_info$datapath, sep = detected_sep, stringsAsFactors = FALSE)
  })
  
  output$patient_selector <- renderUI({
    df <- raw_data()
    if (is.null(df)) {
      helpText("Please upload a CSV file to populate patient IDs.")
    } else {
      patient_list <- unique(df$patient_id)
      selectInput("patient_id", "Select Patient ID:", choices = patient_list)
    }
  })
  
  output$date_selector <- renderUI({
    df <- raw_data()
    req(input$patient_id)
    
    if (is.null(df)) {
      return(NULL)
    } else {
      available_dates <- df %>% 
        filter(patient_id == input$patient_id) %>% 
        pull(session_date) %>% 
        unique()
      
      selectInput("session_date", "Select Session Date:", choices = available_dates)
    }
  })
  
  report_reactive <- eventReactive(input$calculate, {
    df <- raw_data()
    req(input$patient_id, input$session_date)
    
    if (is.null(df)) {
      return("SYSTEM NOTICE: Please upload a valid audiometry database file (.csv) to initialize analysis.")
    }
    
    res <- compute_detailed_patient_report(
      clinical_data = df,
      target_id     = input$patient_id,
      target_date   = input$session_date
    )
    
    if (is.null(res)) return("METRICS NOT FOUND: Inconsistent data parameters.")
    return(res$text_report)
  })
  
  output$report_output <- renderText({
    report_reactive()
  })
}

# ------------------------------------------------------------------------------
# PART 4: APPLICATION INITIALIZATION
# ------------------------------------------------------------------------------
shinyApp(ui = ui, server = server)