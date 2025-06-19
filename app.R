library(shiny)
library(shinydashboard)
library(DBI)
library(RPostgres)
library(tidyverse)
library(plotly)
library(DT)
library(viridis)
library(scales)
library(lubridate)

# Mematikan scientific notation
options(scipen = 999)

# Fungsi koneksi database
db_connect <- function() {
  dbConnect(
    Postgres(),
    dbname   = Sys.getenv("DB_NAME", "railway"),
    host     = Sys.getenv("DB_HOST", "turntable.proxy.rlwy.ne"),
    port     = as.integer(Sys.getenv("DB_PORT", 30325)),
    user     = Sys.getenv("DB_USER", "postgres"),
    password = Sys.getenv("DB_PASSWORD", "zFsimXXGRVzLkcMiShuvxOSXLSULnfCG")
  )
}

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Dashboard Analisis Produk & Ulasan", titleWidth = 350),
  dashboardSidebar(width = 350,
                   sidebarMenu(
                     menuItem("📊 Ringkasan Utama", tabName = "ringkasan", icon = icon("dashboard")),
                     menuItem("⭐ Analisis Ulasan", tabName = "ulasan", icon = icon("star")),
                     menuItem("📦 Wawasan Produk", tabName = "produk", icon = icon("box")),
                     menuItem("💰 Analisis Keuangan", tabName = "keuangan", icon = icon("chart-line")),
                     menuItem("🏪 Analisis Pemasok", tabName = "pemasok", icon = icon("store")),
                     menuItem("📋 Tabel Data", tabName = "data", icon = icon("table"))
                   ),
                   hr(),
                   div(style = "padding: 15px;",
                       h4("🔍 Filter Data", style = "color: #3c8dbc; margin-bottom: 15px;"),
                       selectInput("kategoriFilter", "Pilih Kategori:", 
                                   choices = NULL, selected = "Semua"),
                       selectInput("segmentasiFilter", "Pilih Segmentasi:", 
                                   choices = NULL, selected = "Semua"),
                       selectInput("kategoriUlasanFilter", "Kategori Ulasan:", 
                                   choices = NULL, selected = "Semua"),
                       dateRangeInput("rentangTanggal", "Periode Ulasan:",
                                      start = "2024-01-01", end = "2024-12-31"),
                       sliderInput("minBintang", "Rating Minimum:", 
                                   value = 1, min = 1, max = 5, step = 1)
                   )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f8f9fa;
        }
        .box {
          border-radius: 10px;
          box-shadow: 0 3px 12px rgba(0,0,0,0.15);
          border-top: 3px solid #3c8dbc;
        }
        .value-box {
          border-radius: 10px;
          box-shadow: 0 3px 12px rgba(0,0,0,0.15);
        }
        .navbar-brand {
          font-weight: bold;
        }
        .sidebar-menu > li > a {
          font-size: 14px;
        }
      "))
    ),
    tabItems(
      # Tab Ringkasan Utama
      tabItem(tabName = "ringkasan",
              fluidRow(
                valueBoxOutput("totalProduk", width = 3),
                valueBoxOutput("totalUlasan", width = 3),
                valueBoxOutput("rataRating", width = 3),
                valueBoxOutput("totalKeuntungan", width = 3)
              ),
              fluidRow(
                box(title = "📈 Tren Keuntungan Bulanan", width = 6, status = "primary",
                    solidHeader = TRUE, plotlyOutput("trenKeuntungan", height = 350)),
                box(title = "🎯 Matriks Performa Produk", width = 6, status = "success",
                    solidHeader = TRUE, plotlyOutput("matriksPerforma", height = 350))
              ),
              fluidRow(
                box(title = "🏆 Top 5 Kategori Berdasarkan Nilai Produk", width = 6, status = "warning",
                    solidHeader = TRUE, plotlyOutput("topKategori", height = 300)),
                box(title = "📊 Distribusi Segmentasi Produk", width = 6, status = "info",
                    solidHeader = TRUE, plotlyOutput("distribusiSegmen", height = 300))
              )
      ),
      
      # Tab Analisis Ulasan  
      tabItem(tabName = "ulasan",
              fluidRow(
                box(title = "⭐ Distribusi Rating Bintang", width = 6, status = "primary",
                    solidHeader = TRUE, plotlyOutput("distribusiRating", height = 350)),
                box(title = "📅 Perkembangan Ulasan dari Waktu ke Waktu", width = 6, status = "success",
                    solidHeader = TRUE, plotlyOutput("ulasanWaktu", height = 350))
              ),
              fluidRow(
                box(title = "🔍 Analisis Kategori Ulasan", width = 6, status = "warning",
                    solidHeader = TRUE, plotlyOutput("kategoriUlasan", height = 350)),
                box(title = "📊 Rating vs Kategori Produk", width = 6, status = "info",
                    solidHeader = TRUE, plotlyOutput("ratingKategori", height = 350))
              ),
              fluidRow(
                box(title = "💬 Detail Ulasan Terbaru", width = 12, status = "primary",
                    solidHeader = TRUE, DTOutput("ulasanTerbaru"))
              )
      ),
      
      # Tab Wawasan Produk
      tabItem(tabName = "produk",
              fluidRow(
                box(title = "🎯 Nilai Produk vs Potensi Penjualan", width = 6, status = "primary",
                    solidHeader = TRUE, plotlyOutput("nilaiPotensi", height = 400)),
                box(title = "📦 Harga vs Margin Laba per Kategori", width = 6, status = "success",
                    solidHeader = TRUE, plotlyOutput("hargaMargin", height = 400))
              ),
              fluidRow(
                box(title = "🔝 10 Produk Terbaik Berdasarkan Estimasi Keuntungan", width = 12, status = "warning",
                    solidHeader = TRUE, DTOutput("produkTerbaik"))
              )
      ),
      
      # Tab Analisis Keuangan
      tabItem(tabName = "keuangan",
              fluidRow(
                valueBoxOutput("totalPendapatan", width = 3),
                valueBoxOutput("rataMargin", width = 3),
                valueBoxOutput("keuntunganTertinggi", width = 3),
                valueBoxOutput("produkTerlaris", width = 3)
              ),
              fluidRow(
                box(title = "💹 Margin Laba Berdasarkan Segmentasi", width = 6, status = "primary",
                    solidHeader = TRUE, plotlyOutput("marginSegmen", height = 350)),
                box(title = "💰 Analisis Harga vs Estimasi Keuntungan", width = 6, status = "success",
                    solidHeader = TRUE, plotlyOutput("hargaKeuntungan", height = 350))
              ),
              fluidRow(
                box(title = "📊 Heatmap Performa Keuangan per Kategori", width = 12, status = "info",
                    solidHeader = TRUE, plotlyOutput("heatmapKeuangan", height = 400))
              )
      ),
      
      # Tab Analisis Pemasok
      tabItem(tabName = "pemasok",
              fluidRow(
                box(title = "🏪 Performa Pemasok - Jumlah Produk", width = 6, status = "primary",
                    solidHeader = TRUE, plotlyOutput("pemasokProduk", height = 350)),
                box(title = "💼 Pemasok dengan Rating Tertinggi", width = 6, status = "success",
                    solidHeader = TRUE, plotlyOutput("pemasokRating", height = 350))
              ),
              fluidRow(
                box(title = "💰 Total Keuntungan per Pemasok", width = 6, status = "warning",
                    solidHeader = TRUE, plotlyOutput("pemasokKeuntungan", height = 350)),
                box(title = "🎯 Analisis Potensi Penjualan per Pemasok", width = 6, status = "info",
                    solidHeader = TRUE, plotlyOutput("pemasokPotensi", height = 350))
              )
      ),
      
      # Tab Data
      tabItem(tabName = "data",
              fluidRow(
                box(title = "📋 Dataset Lengkap Produk & Ulasan", width = 12, status = "primary",
                    solidHeader = TRUE, DTOutput("tabelData"))
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Data dari database
  data_lengkap <- reactive({
    con <- db_connect()
    on.exit(dbDisconnect(con), add = TRUE)
    
    # Sesuaikan nama tabel dengan yang ada di database Anda
    query <- "SELECT * FROM gabungan_data_produk_dan_review"
    dbGetQuery(con, query)
  })
  
  # Update pilihan filter berdasarkan data dari DB
  observe({
    df <- data_lengkap()
    if(nrow(df) > 0) {
      updateSelectInput(session, "kategoriFilter", 
                        choices = c("Semua", unique(df$kategori)), 
                        selected = "Semua")
      updateSelectInput(session, "segmentasiFilter", 
                        choices = c("Semua", unique(df$segmentasi_produk)), 
                        selected = "Semua")
      updateSelectInput(session, "kategoriUlasanFilter", 
                        choices = c("Semua", unique(df$kategori_ulasan)), 
                        selected = "Semua")
    }
  })
  
  # Data terfilter
  data_terfilter <- reactive({
    df <- data_lengkap()
    if(nrow(df) == 0) return(df)
    
    # Konversi tanggal
    df$tanggal_ulasan <- as.Date(df$tanggal_ulasan)
    
    # Filter berdasarkan input
    if(input$kategoriFilter != "Semua") {
      df <- df %>% filter(kategori == input$kategoriFilter)
    }
    if(input$segmentasiFilter != "Semua") {
      df <- df %>% filter(segmentasi_produk == input$segmentasiFilter)
    }
    if(input$kategoriUlasanFilter != "Semua") {
      df <- df %>% filter(kategori_ulasan == input$kategoriUlasanFilter)
    }
    
    # Filter tanggal dan rating
    df <- df %>% 
      filter(tanggal_ulasan >= input$rentangTanggal[1] & 
               tanggal_ulasan <= input$rentangTanggal[2]) %>%
      filter(bintang_ulasan >= input$minBintang)
    
    return(df)
  })
  
  # Value Boxes - Ringkasan
  output$totalProduk <- renderValueBox({
    total <- length(unique(data_terfilter()$id_produk))
    valueBox(
      value = total,
      subtitle = "Total Produk Unik",
      icon = icon("box"),
      color = "blue"
    )
  })
  
  output$totalUlasan <- renderValueBox({
    total <- nrow(data_terfilter())
    valueBox(
      value = total,
      subtitle = "Total Ulasan",
      icon = icon("comments"),
      color = "green"
    )
  })
  
  output$rataRating <- renderValueBox({
    rata <- round(mean(data_terfilter()$bintang_ulasan, na.rm = TRUE), 2)
    valueBox(
      value = rata,
      subtitle = "Rata-rata Rating",
      icon = icon("star"),
      color = "yellow"
    )
  })
  
  output$totalKeuntungan <- renderValueBox({
    total <- sum(data_terfilter()$estimasi_keuntungan, na.rm = TRUE)
    valueBox(
      value = paste0("Rp ", format(total, big.mark = ".", decimal.mark = ",")),
      subtitle = "Total Est. Keuntungan",
      icon = icon("money-bill-wave"),
      color = "red"
    )
  })
  
  # Value Boxes - Keuangan
  output$totalPendapatan <- renderValueBox({
    total <- sum(data_terfilter()$nilai_produk, na.rm = TRUE)
    valueBox(
      value = paste0("Rp ", format(total, big.mark = ".", decimal.mark = ",")),
      subtitle = "Total Nilai Produk",
      icon = icon("chart-line"),
      color = "green"
    )
  })
  
  output$rataMargin <- renderValueBox({
    rata <- round(mean(data_terfilter()$margin_laba, na.rm = TRUE), 2)
    valueBox(
      value = paste0(rata, "%"),
      subtitle = "Rata-rata Margin Laba",
      icon = icon("percentage"),
      color = "blue"
    )
  })
  
  output$keuntunganTertinggi <- renderValueBox({
    tertinggi <- max(data_terfilter()$estimasi_keuntungan, na.rm = TRUE)
    valueBox(
      value = paste0("Rp ", format(tertinggi, big.mark = ".", decimal.mark = ",")),
      subtitle = "Keuntungan Tertinggi",
      icon = icon("trophy"),
      color = "yellow"
    )
  })
  
  output$produkTerlaris <- renderValueBox({
    df <- data_terfilter()
    if(nrow(df) > 0) {
      terlaris <- df %>% 
        filter(potensi_penjualan == max(potensi_penjualan, na.rm = TRUE)) %>%
        slice(1) %>%
        pull(potensi_penjualan)
    } else {
      terlaris <- 0
    }
    valueBox(
      value = format(terlaris, big.mark = ".", decimal.mark = ","),
      subtitle = "Potensi Penjualan Tertinggi",
      icon = icon("fire"),
      color = "red"
    )
  })
  
  # Plot: Tren Keuntungan Bulanan
  output$trenKeuntungan <- renderPlotly({
    df <- data_terfilter()
    if(nrow(df) == 0) return(plotly_empty())
    
    data_bulanan <- df %>%
      mutate(bulan = floor_date(tanggal_ulasan, "month")) %>%
      group_by(bulan) %>%
      summarise(total_keuntungan = sum(estimasi_keuntungan, na.rm = TRUE)) %>%
      arrange(bulan)
    
    p <- ggplot(data_bulanan, aes(x = bulan, y = total_keuntungan)) +
      geom_line(color = "#3c8dbc", size = 1.2) +
      geom_point(color = "#3c8dbc", size = 3) +
      scale_y_continuous(labels = comma_format(prefix = "Rp ")) +
      labs(x = "", y = "") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  # Plot: Matriks Performa
  output$matriksPerforma <- renderPlotly({
    df <- data_terfilter()
    if(nrow(df) == 0) return(plotly_empty())
    
    p <- ggplot(df, aes(x = nilai_produk, y = potensi_penjualan, 
                        color = bintang_ulasan, size = estimasi_keuntungan)) +
      geom_point(alpha = 0.7) +
      scale_color_viridis_c(name = "Rating") +
      scale_size_continuous(name = "Keuntungan") +
      labs(x = "", y = "") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Plot: Top Kategori
  output$topKategori <- renderPlotly({
    df <- data_terfilter()
    if(nrow(df) == 0) return(plotly_empty())
    
    data_kategori <- df %>%
      group_by(kategori) %>%
      summarise(total_nilai = sum(nilai_produk, na.rm = TRUE)) %>%
      arrange(desc(total_nilai)) %>%
      slice_head(n = 5)
    
    p <- ggplot(data_kategori, aes(x = reorder(kategori, total_nilai), y = total_nilai)) +
      geom_col(fill = "#f39c12", alpha = 0.8) +
      coord_flip() +
      scale_y_continuous(labels = comma_format(prefix = "Rp ")) +
      labs(x = "", y = "") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Plot: Distribusi Segmen
  output$distribusiSegmen <- renderPlotly({
    df <- data_terfilter()
    if(nrow(df) == 0) return(plotly_empty())
    
    data_segmen <- df %>%
      group_by(segmentasi_produk) %>%
      summarise(jumlah = n()) %>%
      mutate(persentase = jumlah / sum(jumlah) * 100)
    
    p <- ggplot(data_segmen, aes(x = "", y = persentase, fill = segmentasi_produk)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      scale_fill_viridis_d() +
      theme_void() +
      theme(legend.position = "bottom")
    
    ggplotly(p)
  })
  
  # Plot: Distribusi Rating
  output$distribusiRating <- renderPlotly({
    df <- data_terfilter()
    if(nrow(df) == 0) return(plotly_empty())
    
    data_rating <- df %>%
      group_by(bintang_ulasan) %>%
      summarise(jumlah = n())
    
    p <- ggplot(data_rating, aes(x = factor(bintang_ulasan), y = jumlah)) +
      geom_col(fill = "#3c8dbc", alpha = 0.8) +
      labs(x = "Rating Bintang", y = "Jumlah Ulasan") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Plot: Ulasan dari Waktu ke Waktu
  output$ulasanWaktu <- renderPlotly({
    df <- data_terfilter()
    if(nrow(df) == 0) return(plotly_empty())
    
    data_harian <- df %>%
      group_by(tanggal_ulasan) %>%
      summarise(jumlah_ulasan = n()) %>%
      arrange(tanggal_ulasan)
    
    p <- ggplot(data_harian, aes(x = tanggal_ulasan, y = jumlah_ulasan)) +
      geom_line(color = "#18bc9c", size = 1) +
      geom_point(color = "#18bc9c", size = 2) +
      labs(x = "", y = "") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Plot: Kategori Ulasan
  output$kategoriUlasan <- renderPlotly({
    df <- data_terfilter()
    if(nrow(df) == 0) return(plotly_empty())
    
    data_kat_ulasan <- df %>%
      group_by(kategori_ulasan) %>%
      summarise(jumlah = n(), rata_rating = mean(bintang_ulasan, na.rm = TRUE))
    
    p <- ggplot(data_kat_ulasan, aes(x = kategori_ulasan, y = jumlah, fill = rata_rating)) +
      geom_col() +
      scale_fill_viridis_c(name = "Rata-rata\nRating") +
      labs(x = "", y = "") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # Plot: Rating vs Kategori
  output$ratingKategori <- renderPlotly({
    df <- data_terfilter()
    if(nrow(df) == 0) return(plotly_empty())
    
    p <- ggplot(df, aes(x = kategori, y = bintang_ulasan, fill = kategori)) +
      geom_boxplot(alpha = 0.7) +
      scale_fill_viridis_d() +
      labs(x = "", y = "Rating Bintang") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none")
    
    ggplotly(p)
  })
  
  # Plot: Nilai vs Potensi
  output$nilaiPotensi <- renderPlotly({
    df <- data_terfilter()
    if(nrow(df) == 0) return(plotly_empty())
    
    p <- ggplot(df, aes(x = nilai_produk, y = potensi_penjualan, color = kategori)) +
      geom_point(size = 3, alpha = 0.7) +
      scale_color_viridis_d() +
      labs(x = "Nilai Produk", y = "Potensi Penjualan") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Plot: Harga vs Margin
  output$hargaMargin <- renderPlotly({
    df <- data_terfilter()
    if(nrow(df) == 0) return(plotly_empty())
    
    p <- ggplot(df, aes(x = harga, y = margin_laba, color = segmentasi_produk)) +
      geom_point(size = 3, alpha = 0.7) +
      scale_color_viridis_d() +
      labs(x = "Harga", y = "Margin Laba (%)") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Plot: Margin per Segmen
  output$marginSegmen <- renderPlotly({
    df <- data_terfilter()
    if(nrow(df) == 0) return(plotly_empty())
    
    p <- ggplot(df, aes(x = segmentasi_produk, y = margin_laba, fill = segmentasi_produk)) +
      geom_boxplot(alpha = 0.7) +
      scale_fill_viridis_d() +
      labs(x = "", y = "Margin Laba (%)") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  # Plot: Harga vs Keuntungan
  output$hargaKeuntungan <- renderPlotly({
    df <- data_terfilter()
    if(nrow(df) == 0) return(plotly_empty())
    
    p <- ggplot(df, aes(x = harga, y = estimasi_keuntungan, size = bintang_ulasan)) +
      geom_point(alpha = 0.6, color = "#18bc9c") +
      scale_size_continuous(name = "Rating") +
      labs(x = "Harga", y = "Estimasi Keuntungan") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Plot: Heatmap Keuangan
  output$heatmapKeuangan <- renderPlotly({
    df <- data_terfilter()
    if(nrow(df) == 0) return(plotly_empty())
    
    data_heatmap <- df %>%
      group_by(kategori, segmentasi_produk) %>%
      summarise(
        rata_margin = mean(margin_laba, na.rm = TRUE),
        total_keuntungan = sum(estimasi_keuntungan, na.rm = TRUE),
        .groups = 'drop'
      )
    
    p <- ggplot(data_heatmap, aes(x = segmentasi_produk, y = kategori, fill = rata_margin)) +
      geom_tile() +
      scale_fill_viridis_c(name = "Rata-rata\nMargin (%)") +
      labs(x = "", y = "") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # Plot: Pemasok - Jumlah Produk
  output$pemasokProduk <- renderPlotly({
    df <- data_terfilter()
    if(nrow(df) == 0) return(plotly_empty())
    
    data_pemasok <- df %>%
      group_by(id_pemasok) %>%
      summarise(jumlah_produk = n_distinct(id_produk)) %>%
      arrange(desc(jumlah_produk)) %>%
      slice_head(n = 10)
    
    p <- ggplot(data_pemasok, aes(x = reorder(factor(id_pemasok), jumlah_produk), y = jumlah_produk)) +
      geom_col(fill = "#3c8dbc", alpha = 0.8) +
      coord_flip() +
      labs(x = "ID Pemasok", y = "Jumlah Produk") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Plot: Pemasok Rating
  output$pemasokRating <- renderPlotly({
    df <- data_terfilter()
    if(nrow(df) == 0) return(plotly_empty())
    
    data_pemasok_rating <- df %>%
      group_by(id_pemasok) %>%
      summarise(rata_rating = mean(bintang_ulasan, na.rm = TRUE)) %>%
      arrange(desc(rata_rating)) %>%
      slice_head(n = 10)
    
    p <- ggplot(data_pemasok_rating, aes(x = reorder(factor(id_pemasok), rata_rating), y = rata_rating)) +
      geom_col(fill = "#f39c12", alpha = 0.8) +
      coord_flip() +
      labs(x = "ID Pemasok", y = "Rata-rata Rating") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Plot: Pemasok Keuntungan
  output$pemasokKeuntungan <- renderPlotly({
    df <- data_terfilter()
    if(nrow(df) == 0) return(plotly_empty())
    
    data_pemasok_profit <- df %>%
      group_by(id_pemasok) %>%
      summarise(total_keuntungan = sum(estimasi_keuntungan, na.rm = TRUE)) %>%
      arrange(desc(total_keuntungan)) %>%
      slice_head(n = 10)
    
    p <- ggplot(data_pemasok_profit, aes(x = reorder(factor(id_pemasok), total_keuntungan), y = total_keuntungan)) +
      geom_col(fill = "#18bc9c", alpha = 0.8) +
      coord_flip() +
      scale_y_continuous(labels = comma_format(prefix = "Rp ")) +
      labs(x = "ID Pemasok", y = "Total Keuntungan") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Plot: Pemasok Potensi
  output$pemasokPotensi <- renderPlotly({
    df <- data_terfilter()
    if(nrow(df) == 0) return(plotly_empty())
    
    data_pemasok_potensi <- df %>%
      group_by(id_pemasok) %>%
      summarise(total_potensi = sum(potensi_penjualan, na.rm = TRUE)) %>%
      arrange(desc(total_potensi)) %>%
      slice_head(n = 10)
    
    p <- ggplot(data_pemasok_potensi, aes(x = reorder(factor(id_pemasok), total_potensi), y = total_potensi)) +
      geom_col(fill = "#e74c3c", alpha = 0.8) +
      coord_flip() +
      scale_y_continuous(labels = comma_format()) +
      labs(x = "ID Pemasok", y = "Total Potensi Penjualan") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Tabel: Ulasan Terbaru
  output$ulasanTerbaru <- renderDT({
    df <- data_terfilter()
    if(nrow(df) == 0) return(datatable(data.frame()))
    
    tabel_ulasan <- df %>%
      arrange(desc(tanggal_ulasan)) %>%
      select(nama_produk, kategori, bintang_ulasan, tanggal_ulasan, ulasan, kategori_ulasan) %>%
      slice_head(n = 50)
    
    datatable(tabel_ulasan,
              options = list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE),
              colnames = c("Nama Produk", "Kategori", "Rating", "Tanggal", "Ulasan", "Kategori Ulasan"))
  })
  
  # Tabel: Produk Terbaik
  output$produkTerbaik <- renderDT({
    df <- data_terfilter()
    if(nrow(df) == 0) return(datatable(data.frame()))
    
    tabel_produk <- df %>%
      group_by(id_produk, nama_produk, kategori, segmentasi_produk) %>%
      summarise(
        rata_rating = round(mean(bintang_ulasan, na.rm = TRUE), 2),
        total_ulasan = n(),
        harga = first(harga),
        estimasi_keuntungan = first(estimasi_keuntungan),
        margin_laba = first(margin_laba),
        potensi_penjualan = first(potensi_penjualan),
        .groups = 'drop'
      ) %>%
      arrange(desc(estimasi_keuntungan)) %>%
      slice_head(n = 10) %>%
      mutate(
        harga = paste0("Rp ", format(harga, big.mark = ".", decimal.mark = ",")),
        estimasi_keuntungan = paste0("Rp ", format(estimasi_keuntungan, big.mark = ".", decimal.mark = ",")),
        margin_laba = paste0(margin_laba, "%"),
        potensi_penjualan = format(potensi_penjualan, big.mark = ".", decimal.mark = ",")
      )
    
    datatable(tabel_produk,
              options = list(pageLength = 10, scrollX = TRUE, autoWidth = TRUE),
              colnames = c("ID Produk", "Nama Produk", "Kategori", "Segmentasi", "Rata Rating", 
                           "Total Ulasan", "Harga", "Est. Keuntungan", "Margin Laba", "Potensi Penjualan"))
  })
  
  # Tabel: Data Lengkap
  output$tabelData <- renderDT({
    df <- data_terfilter()
    if(nrow(df) == 0) return(datatable(data.frame()))
    
    # Format kolom keuangan untuk tampilan yang lebih baik
    df_display <- df %>%
      mutate(
        harga = paste0("Rp ", format(harga, big.mark = ".", decimal.mark = ",")),
        nilai_produk = paste0("Rp ", format(nilai_produk, big.mark = ".", decimal.mark = ",")),
        estimasi_keuntungan = paste0("Rp ", format(estimasi_keuntungan, big.mark = ".", decimal.mark = ",")),
        margin_laba = paste0(margin_laba, "%"),
        potensi_penjualan = format(potensi_penjualan, big.mark = ".", decimal.mark = ",")
      )
    
    datatable(df_display,
              options = list(pageLength = 15, scrollX = TRUE, autoWidth = TRUE),
              colnames = c("ID Pemasok", "ID Produk", "Nama Produk", "Kategori", "Segmentasi", 
                           "ID Pelanggan", "ID Ulasan", "Tanggal Ulasan", "Ulasan", "Rating", 
                           "Kategori Ulasan", "Harga", "Nilai Produk", "Potensi Penjualan", 
                           "Est. Keuntungan", "Margin Laba"))
  })
  
  # Fungsi helper untuk plot kosong
  plotly_empty <- function() {
    plotly::plot_ly() %>%
      plotly::add_annotations(
        text = "Tidak ada data untuk ditampilkan",
        x = 0.5, y = 0.5,
        xref = "paper", yref = "paper",
        showarrow = FALSE,
        font = list(size = 16, color = "gray")
      ) %>%
      plotly::layout(
        xaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE),
        yaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)
      )
  }
}

# Jalankan aplikasi
shinyApp(ui = ui, server = server)