library(shiny)
library(ggplot2)
library(plotly)
library(readr)
library(dplyr)

# 读取季度和月度数据
quarter_data_path <- "data/data_by_quarter.csv"
month_data_path <- "data/data_by_month.csv"
quarter_data <- read_csv(quarter_data_path, show_col_types = FALSE)
month_data <- read_csv(month_data_path, show_col_types = FALSE)


month_data$Date <- as.Date(month_data$Date, format = "%Y/%m/%d")

# 预处理季度和月度数据
quarter_data$Year <- as.numeric(substr(quarter_data$QuarterLabel, 3, 6))
quarter_data$Quarter <- substr(quarter_data$QuarterLabel, 1, 2)
month_data$Year <- lubridate::year(month_data$Date)
month_data$Month <- lubridate::month(month_data$Date)

# 定义变量分类，同时包含原始数据和变化率（change）数据
categories_quarter <- list(
  PPI = c("PR_All", "PR_Landed", "PR_NL_ALL", "PR_NL_CCR", "PR_NL_RCR", "PR_NL_OCR"),
  Materials = c("Cement", "SteelBar", "Granite", "ConcretingSand", "ReadyMixConcrete"),
  Financial = c("Index", "Bill1Yr", "Bond2Yr", "Bond5Yr", "Bond10Yr", "Rate")
)

categories_month <- list(
  Materials = c("Cement", "SteelBar", "Granite", "ConcretingSand", "ReadyMixConcrete"),
  Financial = c("Index", "Bill1Yr", "Bond2Yr", "Bond5Yr", "Bond10Yr", "Rate")
)

categories_changes_quarter <- lapply(categories_quarter, function(x) paste0("%Change_", x))
categories_changes_month <- lapply(categories_month, function(x) paste0("%Change_", x))

# UI定义
ui <- fluidPage(
  titlePanel("Dynamic Visualization with Enhanced Filters"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("viewMode", "View by:", choices = c("Quarter" = "quarter", "Month" = "month")),
      uiOutput("categorySelect"),
      uiOutput("varSelectUI"),
      uiOutput("typeSelectUI"),
      uiOutput("timeSelectUI")  # 根据选择动态显示季度还是月份选择器
    ),
    mainPanel(
      plotlyOutput("linePlot"),
      width = 8
    )
  )
)



# Server逻辑
server <- function(input, output, session) {
  # 根据查看模式动态更新时间选择器UI
  output$timeSelectUI <- renderUI({
    if (!is.null(input$viewMode) && input$viewMode == "quarter") {
      tagList(
        fluidRow(
          column(8, sliderInput("startYear", "Start Year:",
                                min = min(quarter_data$Year), max = max(quarter_data$Year), value = min(quarter_data$Year), step = 1)),
          column(4, uiOutput("startQuarterUI"))
        ),
        fluidRow(
          column(8, uiOutput("endYearUI")),
          column(4, uiOutput("endQuarterUI"))
        )
      )
    } else {
      tagList(
        fluidRow(
          column(8, sliderInput("startYear", "Start Year:",
                                min = min(month_data$Year), max = max(month_data$Year), value = min(month_data$Year), step = 1)),
          column(4, uiOutput("startMonthUI"))
        ),
        fluidRow(
          column(8, uiOutput("endYearUI")),
          column(4, uiOutput("endMonthUI"))
        )
      )
    }
  })
  
  output$startQuarterUI <- renderUI({
    startYear <- input$startYear
    if (!is.null(startYear) && startYear == 2005) {
      selectInput("startQuarter", "Start Quarter:", choices = c("Q3", "Q4"))
    } else {
      selectInput("startQuarter", "Start Quarter:", choices = c("Q1", "Q2", "Q3", "Q4"))
    }
  })
  
  output$startMonthUI <- renderUI({
    startYear <- input$startYear
    if (!is.null(startYear) && startYear == 2005) {
      selectInput("startMonth", "Start Month:", choices = month.abb[7:12])
    } else {
      selectInput("startMonth", "Start Month:", choices = month.abb)
    }
  })
  
  output$endQuarterUI <- renderUI({
    endYear <- input$endYear
    if (!is.null(endYear) && endYear == 2005) {
      selectInput("endQuarter", "End Quarter:", choices = c("Q3", "Q4"), selected = "Q4")
    } else {
      selectInput("endQuarter", "End Quarter:", choices = c("Q1", "Q2", "Q3", "Q4"), selected = "Q4")
    }
  })
  
  output$endMonthUI <- renderUI({
    endYear <- input$endYear
    if (!is.null(endYear) && endYear == 2005) {
      selectInput("endMonth", "End Month:", choices = month.abb[7:12], selected = "Dec")
    } else {
      selectInput("endMonth", "End Month:", choices = month.abb, selected = "Dec")
    }
  })
  
  output$endYearUI <- renderUI({
    startYear <- input$startYear
    if (!is.null(startYear)) {
      sliderInput("endYear", "End Year:", min = startYear, max = max(quarter_data$Year), value = max(quarter_data$Year), step = 1)
    } else {
      sliderInput("endYear", "End Year:", min = min(quarter_data$Year), max = max(quarter_data$Year), value = max(quarter_data$Year), step = 1)
    }
  })
  
  
  
  
  
  # 根据所选分类更新数据类型选择UI
  output$typeSelectUI <- renderUI({
    selectInput("dataType", "Data Type:", choices = c("Original", "Change"), selected = "Original")
  })
  
  # 根据所选分类和数据类型动态更新变量选择UI
  output$varSelectUI <- renderUI({
    if (input$dataType == "Original") {
      if (input$viewMode == "quarter") {
        selectInput("yvar", "Choose a variable:", choices = categories_quarter[[input$category]])
      } else {
        selectInput("yvar", "Choose a variable:", choices = categories_month[[input$category]])
      }
    } else {
      if (input$viewMode == "quarter") {
        selectInput("yvar", "Choose a variable:", choices = categories_changes_quarter[[input$category]])
      } else {
        selectInput("yvar", "Choose a variable:", choices = categories_changes_month[[input$category]])
      }
    }
  })
  output$categorySelect <- renderUI({
    choices <- if (input$viewMode == "quarter") {
      c("PPI","Materials", "Financial")
    } else {
      c("Materials", "Financial")
    }
    
    selectInput("category", "Choose a category:", choices = choices)
  })
  # 根据输入渲染交互式图表
  output$linePlot <- renderPlotly({
    req(input$yvar) # 确保有变量被选中
    
    # 根据查看模式选择数据集和时间格式化方法
    data <- if(input$viewMode == "quarter") {
      quarter_data
    } else {
      month_data
    }
    
    filtered_data <- data %>%
      filter(Year >= input$startYear & Year <= input$endYear) %>%
      filter(if (input$viewMode == "quarter") {
        (Year > input$startYear | (Year == input$startYear & Quarter >= input$startQuarter)) &
          (Year < input$endYear | (Year == input$endYear & Quarter <= input$endQuarter))
      } else {
        (Year > input$startYear | (Year == input$startYear & Month >= match(input$startMonth, month.abb))) &
          (Year < input$endYear | (Year == input$endYear & Month <= match(input$endMonth, month.abb)))
      })
    
    
    # 绘制图形
    p <- ggplot(filtered_data, aes(x = Date, y = .data[[input$yvar]])) +
      geom_line() +
      labs(title = paste("Trend of", input$yvar, "over Time"), x = "Date", y = input$yvar) +
      theme_minimal()
    
    ggplotly(p)
  })
}

shinyApp(ui = ui, server = server)