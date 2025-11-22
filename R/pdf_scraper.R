#### ---- PACKAGES ----
suppressPackageStartupMessages({
  library(shiny)
  library(data.table)
  library(rhandsontable)
  library(shinyjs)
  library(shinythemes)
  library(httr)
  library(pdftools)
  library(dplyr)
  library(stringr)
  library(purrr)
  library(fs)
})

#### ---- TABLE EXTRACTION ----

# Detect the approximate top of table
find_table_top <- function(df_words) {
  numeric_rows <- df_words %>% filter(str_detect(text, "\\d+\\.\\d+|\\d+"))
  if (nrow(numeric_rows) == 0) return(130)
  min_top <- min(numeric_rows$top)
  max(min_top - 20, 40)
}

# Group words into rows by y coordinate
group_rows_by_y <- function(df_words, y_tol = 6) {
  df_words <- df_words %>% arrange(top, x)
  groups <- list()
  current <- list()
  current_y <- NULL
  
  for (i in seq_len(nrow(df_words))) {
    w <- df_words[i, ]
    if (is.null(current_y)) current_y <- w$top
    if (abs(w$top - current_y) > y_tol) {
      groups <- append(groups, list(bind_rows(current)))
      current <- list()
      current_y <- w$top
    }
    current <- append(current, list(w))
  }
  if (length(current) > 0) groups <- append(groups, list(bind_rows(current)))
  groups
}

# Extract table rows from grouped words 
extract_table_rows <- function(groups) {
  rows_list <- list()
  
  for (g in groups) {
    g <- g %>% arrange(x)
    
   
    texts <- g$text
    combined_text <- c()
    i <- 1
    while (i <= length(texts)) {
      txt <- texts[i]
      # Merge with next if both are numeric-like
      while (i < length(texts) && grepl("^\\d+$", txt) && grepl("^\\d+$", texts[i+1])) {
        txt <- paste0(txt, texts[i+1])
        i <- i + 1
      }
      combined_text <- c(combined_text, txt)
      i <- i + 1
    }
    
    line <- paste(combined_text, collapse = " ")
    
    # Skip header lines / short lines
    if (str_detect(line, "^(Other|Previous|Days|Average|Today|Price|Rs)") || nchar(line) < 5) next
    
    # Match numbers with commas or decimals
    nums <- str_extract_all(line, "\\d{1,3}(?:,\\d{3})*(?:\\.\\d+)?|n\\.a\\.")[[1]]
    
    if (length(nums) > 0) {
      prefix <- str_split(line, nums[1], simplify = TRUE)[1] %>% str_trim()
      
      unit_pattern <- "(Rs\\.?/?\\s*Kg|Rs\\.?/?\\s*\\w+)"
      if (str_detect(prefix, unit_pattern)) {
        item  <- str_replace(prefix, unit_pattern, "") %>% str_trim()
        unit  <- str_extract(prefix, unit_pattern) %>% str_trim()
        row <- c(item, unit, nums)
      } else {
        row <- c(prefix, nums)
      }
    } else {
      row <- c(line)
    }
    
    rows_list <- append(rows_list, list(row))
  }
  
  if (length(rows_list) == 0) return(NULL)
  
  max_len <- max(lengths(rows_list))
  rows_list <- lapply(rows_list, function(r) c(r, rep("", max_len - length(r))))
  
  do.call(rbind, rows_list)
}

#### ---- UI ----
ui <- fluidPage(
  theme = shinytheme("flatly"),
  useShinyjs(),
  
  tags$head(
    tags$link(
      rel = "stylesheet",
      href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.3/font/bootstrap-icons.css"
    ),
    tags$style(HTML("
      .icon-red { color: #E74C3C; font-size: 1.3em; }
      .icon-blue { color: #3498DB; font-size: 1.3em; }
      .icon-green { color: #2ECC71; font-size: 1.3em; }
      .icon-gray { color: #7F8C8D; font-size: 1.3em; }
    "))
  ),
  
  titlePanel(HTML("<i class='bi bi-file-earmark-text icon-gray'></i> PDF Table Extractor")),
  
  sidebarLayout(
    sidebarPanel(
      textInput("downloadURL", "Enter PDF URL:", value = ""),
      textInput("pages", "Enter Pages (comma-separated):", value = ""),
      actionButton(
        "downloadButton", 
        HTML("<i class='bi bi-file-earmark-arrow-down icon-red'></i> Download PDF")
      ),
      br(), br(),
      actionButton(
        "scrapeButton", 
        HTML("<i class='bi bi-table icon-blue'></i> Extract Tables")
      ),
      br(), br(),
      downloadButton(
        "fileDownload", 
        label = HTML("<i class='bi bi-download icon-green'></i> Download CSV"),
        icon = NULL
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          HTML("<i class='bi bi-file-earmark-pdf icon-gray'></i> PDF Viewer"), 
          uiOutput("pdfViewer")
        ),
        tabPanel(
          HTML("<i class='bi bi-grid-3x3-gap icon-gray'></i> Extracted Table"), 
          rHandsontableOutput("hot")
        )
      )
    )
  )
)

#### ---- SERVER ----
server <- function(input, output, session) {
  
  pdf <- reactiveValues(pdf_folder = NULL, pdfPath = NULL)
  current <- reactiveValues(current = NULL)
  
  safe_GET <- safely(httr::GET)
  
  assign_extension <- function(response) {
    if (is.null(response$result)) return("no response")
    if (response$result$status_code == 200) {
      con_type <- response$result$headers$`content-type`
      if (grepl("pdf", con_type)) "pdf"
      else if (grepl("zip", con_type)) "zip"
      else if (grepl("html", con_type)) "html"
      else "other"
    } else response$result$status_code
  }
  
  #### ---- Download PDF ----
  observeEvent(input$downloadButton, {
    req(input$downloadURL)
    showModal(modalDialog(HTML("<i class='bi bi-hourglass-split icon-gray'></i> Downloading PDF..."), footer = NULL))
    
    url <- input$downloadURL
    x <- safe_GET(url)
    ext <- assign_extension(x)
    
    if (ext != "pdf") {
      removeModal()
      showNotification("This link does not return a valid PDF!", type = "error")
      req(FALSE)
    }
    
    pdf$pdf_folder <- tempfile(pattern = "pdfs_")
    dir.create(pdf$pdf_folder, showWarnings = FALSE)
    temp <- file.path(pdf$pdf_folder, "document.pdf")
    download.file(url, temp, mode = "wb", quiet = TRUE)
    
    addResourcePath("pdfs", pdf$pdf_folder)
    pdf$pdfPath <- temp
    
    removeModal()
    showNotification("PDF downloaded successfully!", type = "message")
  })
  
  #### ---- Show PDF in Viewer ----
  output$pdfViewer <- renderUI({
    req(pdf$pdfPath)
    tags$iframe(
      style = "width:100%;height:700px;border:none;",
      src = "pdfs/document.pdf"
    )
  })
  
  #### ---- Extract Tables ----
  observeEvent(input$scrapeButton, {
    req(pdf$pdfPath)
    showModal(modalDialog(HTML("<i class='bi bi-hourglass-split icon-gray'></i> Extracting tables..."), footer = NULL))
    
    pages <- if (nzchar(input$pages)) {
      as.integer(unlist(strsplit(input$pages, ",")))
    } else seq_len(pdf_length(pdf$pdfPath))
    
    all_pages <- list()
    
    for (pg in pages) {
      df_words <- pdf_data(pdf$pdfPath)[[pg]] %>%
        rename(x = x, top = y, text = text)
      
      top_cut <- find_table_top(df_words)
      df_words_crop <- df_words %>% filter(top >= top_cut)
      
      groups <- group_rows_by_y(df_words_crop)
      table_mat <- extract_table_rows(groups)
      
      if (!is.null(table_mat)) {
        df <- as.data.frame(table_mat, stringsAsFactors = FALSE)
        df$page <- pg
        all_pages[[length(all_pages) + 1]] <- df
      }
    }
    
    if (length(all_pages) == 0) {
      removeModal()
      showNotification("No tables found on the specified pages.", type = "warning")
      return(NULL)
    }
    
    combined_df <- do.call(rbind, all_pages)
    current$current <- combined_df
    
    removeModal()
    showNotification("Tables extracted successfully!", type = "message")
  })
  
  #### ---- Show Extracted Table ----
  output$hot <- renderRHandsontable({
    req(current$current)
    rhandsontable(current$current, useTypes = FALSE) |>
      hot_table(highlightCol = TRUE, highlightRow = TRUE)
  })
  
  #### ---- Download CSV ----
  output$fileDownload <- downloadHandler(
    filename = function() paste0("scraped_data.csv"),
    content = function(file) write.csv(current$current, file, row.names = FALSE)
  )
}

#### ---- RUN APP ----
shinyApp(ui, server)

