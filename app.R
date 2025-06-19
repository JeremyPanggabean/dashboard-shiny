library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(dplyr)

# Data dalam bahasa Inggris (sesuai contoh Anda)
product_data <- data.frame(
  id_product = c(5011, 5022, 5033, 5044, 5055, 5066, 5077, 5088, 5099, 51010),
  product_name = c("Office Chair", "Coffee Maker", "Document Scanner", "Desk Mat", 
                   "Tablet Stand", "Drawer Unit", "Bath Towels", "External SSD", 
                   "Computer Speakers", "Gaming Keyboard"),
  category = c("Furniture", "Home & Kitchen", "Electronics", "Accessories", "Accessories",
               "Furniture", "Home & Kitchen", "Electronics", "Electronics", "Electronics"),
  price = c(445.01, 937.29, 940.02, 76.11, 388.17, 457.31, 710.31, 522.82, 20.36, 776.20),
  segment = c("Mid Range", "Luxury", "Luxury", "Budget", "Mid Range", 
              "Mid Range", "Premium", "Premium", "Budget", "Premium"),
  rating = c(4, 1, 1, 1, 1, 1, 3, 2, 2, 2),
  profit_margin = c(35.06, 29.98, 25.00, 49.93, 49.98, 34.99, 29.99, 25.06, 24.56, 24.99)
)

# UI dalam bahasa Indonesia
ui <- dashboardPage(
  dashboardHeader(
    title = "Dashboard Analisis Penjualan Produk dan Ulasan",
    titleWidth = 300
  ),
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Ringkasan", tabName = "summary", icon = icon("chart-pie")),
      menuItem("Analisis Kategori", tabName = "category", icon = icon("th-list")),
      menuItem("Data Produk", tabName = "data", icon = icon("table"))
    ),
    
    hr(),
    
    selectInput(
      "categoryFilter",
      "Pilih Kategori:",
      choices = c("Semua", unique(product_data$category)),
      selected = "Semua"
    ),
    
    sliderInput(
      "priceRange",
      "Rentang Harga:",
      min = floor(min(product_data$price)),
      max = ceiling(max(product_data$price)),
      value = c(floor(min(product_data$price)), ceiling(max(product_data$price)))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Tab Ringkasan
      tabItem(
        tabName = "summary",
        fluidRow(
          valueBoxOutput("totalRevenueBox"),
          valueBoxOutput("avgPriceBox"),
          valueBoxOutput("avgRatingBox")
        ),
        
        fluidRow(
          box(
            title = "Harga Rata-Rata per Kategori",
            width = 6,
            plotlyOutput("avgPricePlot")
          ),
          
          box(
            title = "Distribusi Segmentasi Produk",
            width = 6,
            plotlyOutput("segmentPlot")
          )
        )
      ),
      
      # Tab Analisis Kategori
      tabItem(
        tabName = "category",
        fluidRow(
          box(
            title = "Jumlah Produk per Kategori",
            width = 12,
            plotlyOutput("categoryCountPlot")
          )
        ),
        
        fluidRow(
          box(
            title = "Margin Laba per Produk",
            width = 12,
            plotlyOutput("profitMarginPlot")
          )
        )
      ),
      
      # Tab Data Produk
      tabItem(
        tabName = "data",
        fluidRow(
          box(
            title = "Tabel Data Produk",
            width = 12,
            DTOutput("productTable")
          )
        )
      )
    )
  )
)

# Server logic
server <- function(input, output) {
  # Filter data berdasarkan input
  filtered_data <- reactive({
    data <- product_data
    
    # Filter kategori
    if (input$categoryFilter != "Semua") {
      data <- data %>% filter(category == input$categoryFilter)
    }
    
    # Filter harga
    data <- data %>% filter(price >= input$priceRange[1], price <= input$priceRange[2])
    
    data
  })
  
  # Value boxes
  output$totalRevenueBox <- renderValueBox({
    total <- sum(filtered_data()$price)
    valueBox(
      paste0("$", format(round(total, 2), big.mark = ",")),
      "Total Pendapatan",
      icon = icon("money-bill-wave"),
      color = "green"
    )
  })
  
  output$avgPriceBox <- renderValueBox({
    avg <- mean(filtered_data()$price)
    valueBox(
      paste0("$", format(round(avg, 2), big.mark = ",")),
      "Harga Rata-Rata",
      icon = icon("tags"),
      color = "blue"
    )
  })
  
  output$avgRatingBox <- renderValueBox({
    avg <- mean(filtered_data()$rating)
    valueBox(
      round(avg, 1),
      "Rating Rata-Rata",
      icon = icon("star"),
      color = "yellow"
    )
  })
  
  # Plots
  output$avgPricePlot <- renderPlotly({
    data <- filtered_data() %>%
      group_by(category) %>%
      summarise(avg_price = mean(price))
    
    plot_ly(
      data,
      x = ~category,
      y = ~avg_price,
      type = "bar",
      marker = list(color = "#3B82F6")
    ) %>%
      layout(
        xaxis = list(title = "Kategori"),
        yaxis = list(title = "Harga Rata-Rata ($)")
      )
  })
  
  output$segmentPlot <- renderPlotly({
    data <- filtered_data() %>%
      group_by(segment) %>%
      summarise(count = n())
    
    plot_ly(
      data,
      labels = ~segment,
      values = ~count,
      type = "pie",
      marker = list(colors = c("#3B82F6", "#10B981", "#F59E0B"))
    )
  })
  
  output$categoryCountPlot <- renderPlotly({
    data <- filtered_data() %>%
      group_by(category) %>%
      summarise(count = n())
    
    plot_ly(
      data,
      x = ~category,
      y = ~count,
      type = "bar",
      marker = list(color = "#10B981")
    ) %>%
      layout(
        xaxis = list(title = "Kategori"),
        yaxis = list(title = "Jumlah Produk")
      )
  })
  
  output$profitMarginPlot <- renderPlotly({
    plot_ly(
      filtered_data(),
      x = ~product_name,
      y = ~profit_margin,
      type = "bar",
      marker = list(color = "#F59E0B")
    ) %>%
      layout(
        xaxis = list(title = "Produk"),
        yaxis = list(title = "Margin Laba (%)")
      )
  })
  
  # Data table
  output$productTable <- renderDT({
    datatable(
      filtered_data(),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        language = list(
          url = '//cdn.datatables.net/plug-ins/1.10.24/i18n/Indonesian.json'
        )
      )
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)