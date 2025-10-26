#### ---- pkg ----
suppressPackageStartupMessages({
  library(shiny)
  library(data.table)
  library(rhandsontable)
  library(shinyjs)
  library(shinythemes)
  library(httr)
  library(purrr)
  library(tabulapdf)
})

#### ---- UI ----
ui <- fluidPage(
  theme = shinytheme("flatly"),
  useShinyjs(),
  
  # Bootstrap Icons CDN
  tags$head(
    tags$link(
      rel = "stylesheet",
      href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.3/font/bootstrap-icons.css"
    ),
    # Custom colors for icons
    tags$style(HTML("
      .icon-red { color: #E74C3C; font-size: 1.3em; }       /* Download PDF */
      .icon-blue { color: #3498DB; font-size: 1.3em; }      /* Extract Tables */
      .icon-green { color: #2ECC71; font-size: 1.3em; }     /* Download CSV */
      .icon-gray { color: #7F8C8D; font-size: 1.3em; }      /* Tabs */
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
        label = HTML("<i class='bi bi-download icon-green'></i> Download CSV")
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
  
  safe_GET <- safely(GET)
  
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
      showNotification("❌ This link does not return a valid PDF!", type = "error")
      req(FALSE)
    }
    
    # Temporary folder
    pdf$pdf_folder <- tempfile(pattern = "pdfs_")
    dir.create(pdf$pdf_folder, showWarnings = FALSE)
    
    # Save PDF
    temp <- file.path(pdf$pdf_folder, "document.pdf")
    download.file(url, temp, mode = "wb", quiet = TRUE)
    
    # Make folder web-accessible
    addResourcePath("pdfs", pdf$pdf_folder)
    pdf$pdfPath <- temp
    
    removeModal()
    showNotification("✅ PDF downloaded successfully!", type = "message")
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
    } else NULL
    
    tables_list <- extract_tables(pdf$pdfPath, pages = pages)
    
    if (length(tables_list) == 0) {
      removeModal()
      showNotification("⚠️ No tables found on the specified pages.", type = "warning")
      return(NULL)
    }
    
    max_cols <- max(sapply(tables_list, ncol))
    tables_fixed <- lapply(tables_list, function(tbl) {
      tbl <- as.data.frame(tbl, stringsAsFactors = FALSE)
      if (ncol(tbl) < max_cols) tbl[(ncol(tbl) + 1):max_cols] <- NA
      names(tbl) <- paste0("V", seq_len(max_cols))
      tbl
    })
    
    combined_df <- do.call(rbind, tables_fixed)
    current$current <- combined_df
    
    removeModal()
    showNotification("✅ Tables extracted successfully!", type = "message")
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

#### ---- Run App ----
shinyApp(ui, server)
