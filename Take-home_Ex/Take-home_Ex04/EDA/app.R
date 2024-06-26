library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(dplyr)
library(readr)
library(ggcorrplot)
library(lubridate)
library(viridis)

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
  Index = c("Index"),
  Financial =  c("Bill1Yr", "Bond2Yr", "Bond5Yr", "Bond10Yr", "Rate")
)

categories_month <- list(
  Materials = c("Cement", "SteelBar", "Granite", "ConcretingSand", "ReadyMixConcrete"),
  Index = c("Index"),
  Financial =  c("Bill1Yr", "Bond2Yr", "Bond5Yr", "Bond10Yr", "Rate")
)

categories_changes_quarter <- lapply(names(categories_quarter), function(category) {
  if (category != "Financial") {
    paste0("%Change_", categories_quarter[[category]])
  } else {
    paste0("Change_", categories_quarter[[category]])
  }
})

# 对于月度数据
categories_changes_month <- lapply(names(categories_month), function(category) {
  if (category != "Financial") {
    paste0("%Change_", categories_month[[category]])
  } else {
    paste0("Change_", categories_month[[category]])
  }
})

# 将生成的列表转换回以类别为名的列表结构
categories_changes_quarter <- setNames(categories_changes_quarter, names(categories_quarter))
categories_changes_month <- setNames(categories_changes_month, names(categories_month))

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "EDA", titleWidth = 300), # Adjust title width
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Analysis", tabName = "data_analysis", icon = icon("bar-chart-o"), startExpanded = TRUE,
               menuSubItem("Trend", tabName = "trend"),
               menuSubItem("Distribution", tabName = "distribution"),
               menuSubItem("Autocorrelation", tabName = "autocorrelation"),
               menuSubItem("Crosscorrelation", tabName = "crosscorrelation")
      )
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(
        HTML("
          .box-title {
            font-weight: bold;
            font-size: 16px;
            color: #333;
          }
          .box-content {
            padding: 10px;
            background-color: #f8f8f8;
            border-radius: 5px;
            box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
          }
          .plot-container {
            padding: 10px;
            background-color: #fff;
            border-radius: 5px;
            box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
          }
        ")
      )
    ),
    tabItems(
      # Trend tab content
      tabItem(tabName = "trend",
              fluidRow(
                box(
                  title = span("Filters", class = "box-title"),
                  solidHeader = TRUE,
                  status = "primary",
                  width = 4,
                  radioButtons("viewMode", "View by:", choices = c("Quarter" = "quarter", "Month" = "month")),
                  uiOutput("categorySelect"),
                  uiOutput("varSelectUI"),
                  uiOutput("typeSelectUI"),
                  uiOutput("timeSelectUI")
                ),
                box(
                  title = span("Line Plot", class = "box-title"),
                  solidHeader = TRUE,
                  status = "primary",
                  width = 8,
                  div(class = "plot-container", plotlyOutput("linePlot"))
                )
              )
      ),
      # Distribution tab content
      tabItem(tabName = "distribution",
              fluidRow(
                box(
                  title = span("Filters", class = "box-title"),
                  solidHeader = TRUE,
                  status = "primary",
                  width = 4,
                  uiOutput("categorySelectDist"),
                  uiOutput("varSelectUIDist"),
                  uiOutput("typeSelectDistUI"),
                  uiOutput("timeSelectUIDist"),
                  sliderInput("binCount", "Number of Bins:", value = 15, min = 5, max = 50, step = 1)
                ),
                box(
                  title = span("Histogram", class = "box-title"),
                  solidHeader = TRUE,
                  status = "primary",
                  width = 8,
                  div(class = "plot-container", plotlyOutput("histogramPlot"))
                )
              )
      ),
      # Autocorrelation tab content
      tabItem(tabName = "autocorrelation",
              fluidRow(
                box(
                  title = span("Filters", class = "box-title"),
                  solidHeader = TRUE,
                  status = "primary",
                  width = 4,
                  h3("Autocorrelation"),
                  radioButtons("viewModeAuto", "View by:", choices = c("Year" = "year", "Quarter" = "quarter", "Month" = "month")),
                  uiOutput("categorySelectAuto"),
                  uiOutput("varSelectAutoUI"),
                  uiOutput("typeSelectAutoUI")
                ),
                box(
                  title = span("Boxplot", class = "box-title"),
                  solidHeader = TRUE,
                  status = "primary",
                  width = 8,
                  div(class = "plot-container", plotlyOutput("boxPlot"))
                )
              )
      ),
      # Crosscorrelation tab content
      tabItem(tabName = "crosscorrelation",
              fluidRow(
                box(
                  title = span("Variable Selection", class = "box-title"),
                  solidHeader = TRUE,
                  status = "primary",
                  width = 4,
                  uiOutput("varSelectCorrUI"),
                  br()
                ),
                box(
                  title = span("Correlation Matrix", class = "box-title"),
                  solidHeader = TRUE,
                  status = "primary",
                  width = 8,
                  div(class = "plot-container", plotOutput("corrPlot", height = "600px"))
                )
              )
      )
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
      c("PPI","Materials", "Index","Financial")
    } else {
      c("Materials", "Index","Financial")
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
    
    # 美化图表
    p <- ggplot(filtered_data, aes(x = Date, y = .data[[input$yvar]])) +
      geom_line(size = 1.2, color = "#3366CC") + # 加粗线条并使用深蓝色
      labs(title = paste("Trend of", input$yvar, "over Time"), x = "Date", y = input$yvar, 
           title.size = 18, title.color = "#333333") + # 调整标题大小和颜色
      theme_minimal() +
      theme(
        plot.title = element_text(family = "Arial"), # 设置标题字体
        axis.title = element_text(family = "Arial", size = 14), # 设置坐标轴标题字体和大小
        axis.text = element_text(family = "Arial", size = 12), # 设置坐标轴刻度标签字体和大小
        panel.grid.major = element_line(color = "#D3D3D3", linewidth = 0.5), # 设置网格线颜色和宽度
        panel.grid.minor = element_blank(), # 移除次要网格线
        panel.background = element_rect(fill = "#F8F8F8") # 设置背景颜色
      )
    
    ggplotly(p)
  })
  
  
  
  #Distribution
  output$categorySelectDist <- renderUI({
    choices <- c("PPI", "Materials","Index", "Financial")
    selectInput("categoryDist", "Choose a category:", choices = choices)
  })
  
  # Variable selection UI component based on category
  output$varSelectUIDist <- renderUI({
    selectInput("varDist", "Choose a variable:", choices = categories_quarter[[input$categoryDist]])
  })
  
  # Year range selection UI component, adjusted based on the category
  output$timeSelectUIDist <- renderUI({
    # Adjusted to use only the quarter_data for year range as an example.
    # Modify this logic if month_data is also needed for other categories.
    sliderInput("yearRange", "Select Year Range:", min = min(quarter_data$Year), max = max(quarter_data$Year), value = c(min(quarter_data$Year), max(quarter_data$Year)))
  })
  
  # Histogram plot generation based on selected category and variable
  output$histogramPlot <- renderPlotly({
    req(input$varDist, input$binCount)
    
    data <- if(input$categoryDist == "PPI") {
      quarter_data
    } else {
      month_data
    }
    
    filtered_data <- data %>%
      filter(Year >= input$yearRange[1] & Year <= input$yearRange[2]) %>%
      select(.data[[input$varDist]])
    
    plot <- ggplot(filtered_data, aes_string(x = input$varDist)) +
      geom_histogram(bins = input$binCount, fill = "#6D9EC1", color = "white") +
      geom_vline(aes(xintercept = mean(.data[[input$varDist]], na.rm = TRUE)), color = "#E46726", linetype = "dashed") +
      geom_vline(aes(xintercept = median(.data[[input$varDist]], na.rm = TRUE)), color = "gold", linetype = "dashed") +
      labs(title = paste("Distribution of", input$varDist), x = input$varDist, y = "Frequency") +
      theme_minimal()
    
    # 转换为plotly对象
    p <- ggplotly(plot)
    
    # 添加注释
    p <- p %>% layout(annotations = list(
      list(
        text = "Mean",
        x = mean(filtered_data[[input$varDist]], na.rm = TRUE),
        y = 0.85,  # 调整位置远离图形顶端
        xref = "x",
        yref = "paper",
        showarrow = FALSE,
        font = list(color = "#E46726", size = 10)  # 调整字体大小
      ),
      list(
        text = "Median",
        x = median(filtered_data[[input$varDist]], na.rm = TRUE),
        y = 0.70,  # 调整位置远离图形顶端和另一个标签
        xref = "x",
        yref = "paper",
        showarrow = FALSE,
        font = list(color = "gold", size = 10)  # 调整字体大小
      )
    ))
    
    p
  })
  
  
  # Autocorrelation部分
  output$typeSelectAutoUI <- renderUI({
    selectInput("dataTypeAuto", "Data Type:", choices = c("Original", "Change"), selected = "Original")
  })
  
  output$categorySelectAuto <- renderUI({
    choices <- if (input$viewModeAuto == "quarter") {
      c("PPI","Materials", "Index","Financial")
    } else if (input$viewModeAuto == "month") {
      c("Materials", "Index","Financial")
    } else {
      c("PPI", "Materials","Index", "Financial")
    }
    selectInput("categoryAuto", "Choose a category:", choices = choices)
  })
  
  output$varSelectAutoUI <- renderUI({
    if (input$dataTypeAuto == "Original") {
      if (input$viewModeAuto == "quarter") {
        selectInput("yvarAuto", "Choose a variable:", choices = categories_quarter[[input$categoryAuto]])
      } else if (input$viewModeAuto == "month") {
        selectInput("yvarAuto", "Choose a variable:", choices = categories_month[[input$categoryAuto]])
      } else {
        if (input$categoryAuto == "PPI") {
          selectInput("yvarAuto", "Choose a variable:", choices = categories_quarter[[input$categoryAuto]])
        } else {
          selectInput("yvarAuto", "Choose a variable:", choices = categories_month[[input$categoryAuto]])
        }
      }
    } else {
      if (input$viewModeAuto == "quarter") {
        selectInput("yvarAuto", "Choose a variable:", choices = categories_changes_quarter[[input$categoryAuto]])
      } else if (input$viewModeAuto == "month") {
        selectInput("yvarAuto", "Choose a variable:", choices = categories_changes_month[[input$categoryAuto]])
      } else {
        if (input$categoryAuto == "PPI") {
          selectInput("yvarAuto", "Choose a variable:", choices = categories_changes_quarter[[input$categoryAuto]])
        } else {
          selectInput("yvarAuto", "Choose a variable:", choices = categories_changes_month[[input$categoryAuto]])
        }
      }
    }
  })
  
  output$boxPlot <- renderPlotly({
    req(input$yvarAuto)
    
    data <- if(input$viewModeAuto == "quarter") {
      quarter_data
    } else if (input$viewModeAuto == "month") {
      month_data
    } else {
      if (input$categoryAuto == "PPI") {
        quarter_data %>%
          mutate(Date = as.Date(paste0(Year, "-", (as.numeric(Quarter) - 1) * 3 + 1, "-01"), "%Y-%m-%d"))
      } else {
        month_data
      }
    }
    
    x_axis <- if (input$viewModeAuto == "year") {
      "Year"
    } else if (input$viewModeAuto == "month") {
      "Month"
    } else {
      "Quarter"
    }
    viridis_colors <- viridis_pal()(5)
    p <- ggplot(data, aes(x = .data[[x_axis]], y = .data[[input$yvarAuto]])) +
      geom_boxplot(fill = "darkblue") +
      stat_summary(fun = mean, geom = "line", aes(group = 1), color = "gold") +
      labs(title = paste("Boxplot and Mean of", input$yvarAuto, "over Time"), x = x_axis, y = input$yvarAuto) +
      theme_minimal()
    
    # 当查看模式为"month"时，自定义x轴标签为月份缩写
    if(input$viewModeAuto == "month") {
      p <- p + scale_x_continuous(breaks = 1:12, labels = month.abb)
    }
    
    ggplotly(p)
  })
  
  
  # Crosscorrelation部分
  output$varSelectCorrUI <- renderUI({
    all_vars <- c(categories_quarter$PPI, categories_quarter$Materials, categories_quarter$Financial)
    
    tagList(
      div(
        
        lapply(c("PPI", "Materials", "Index","Financial"), function(category) {
          div(
            style = "border: 0px solid #ccc; padding: 0px;", # Add border and padding for each category
            h4(category, style = "margin-bottom: 1px;"), # Add space below category title
            div(
              style = "display: flex; flex-direction: column;", # Use flex layout for variables
              checkboxGroupInput(paste0("corrVars_", category), NULL, choices = categories_quarter[[category]],
                                 selected = categories_quarter[[category]], inline = FALSE) # Set inline = FALSE for vertical layout
            )
          )
        })
      ),
      div(
        style = "text-align: center; margin-top: 0px;", # Center the action buttons and add space above them
        actionButton("selectAll", "Select All", style = "margin-right: 10px;"), # Add space between buttons
        actionButton("deselectAll", "Deselect All")
      )
    )
  })
  
  
  output$corrPlot <- renderPlot({
    selected_vars <- unlist(lapply(c("PPI", "Materials","Index", "Financial"), function(category) {
      input[[paste0("corrVars_", category)]]
    }))
    
    if (length(selected_vars) < 2) return(NULL) # 确保至少有两个变量被选中
    
    corr_data <- quarter_data[, selected_vars, drop = FALSE]
    corr_matrix <- cor(corr_data, use = "pairwise.complete.obs")
    
    # Enhance the plot aesthetics here
    p <- ggcorrplot(corr_matrix, hc.order = TRUE, type = "lower", outline.col = "white",
                    lab = TRUE, lab_size = 3.5,
                    colors = c("#6D9EC1", "white", "#E46726"), # use your own color palette
                    ggtheme = theme_minimal()) +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
            axis.text.y = element_text(size = 12))
    
    print(p)
  }, width = 600, height = 600) # Control the size of the output plot here # Control the size of the output plot here
  
  
  
  observeEvent(input$selectAll, {
    updateCheckboxGroupInput(session, "corrVars_PPI", choices = categories_quarter$PPI, selected = categories_quarter$PPI)
    updateCheckboxGroupInput(session, "corrVars_Materials", choices = categories_quarter$Materials, selected = categories_quarter$Materials)
    updateCheckboxGroupInput(session, "corrVars_Index", choices = categories_quarter$Index, selected = categories_quarter$Index)
    updateCheckboxGroupInput(session, "corrVars_Financial", choices = categories_quarter$Financial, selected = categories_quarter$Financial)
  })
  
  observeEvent(input$deselectAll, {
    updateCheckboxGroupInput(session, "corrVars_PPI", choices = categories_quarter$PPI, selected = NULL)
    updateCheckboxGroupInput(session, "corrVars_Materials", choices = categories_quarter$Materials, selected = NULL)
    updateCheckboxGroupInput(session, "corrVars_Index", choices = categories_quarter$Index, selected = NULL)
    updateCheckboxGroupInput(session, "corrVars_Financial", choices = categories_quarter$Financial, selected = NULL)
  })
}
shinyApp(ui = ui, server = server)
