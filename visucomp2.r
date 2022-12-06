library(ggplot2)
library(shiny)
library(shinydashboard)
library(dplyr)


cropdata = read.csv("crop_production.csv")

cropdata = na.omit(cropdata)



ui <- dashboardPage(
  dashboardHeader(title=" Crop Production "),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Univariate Analysis", tabName = "singlevarmenu"),
      menuItem("Multivariate Analysis", tabName = "multivarmenu"),
      menuItem("Time Series Analysis", tabName = "timevarmenu"),
      menuItem("State-wise data", tabName = "statemenu"),
      menuItem("Crop Production Data", tabName = "diagmenu")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("singlevarmenu",
              tabsetPanel(
                tabPanel("Numeric Variables",
                         selectInput("numvar", "Numeric Variable", colnames(cropdata)[c(6,7)]),
                         h4("Summary Statistics"),
                         tableOutput("summstat"),
                         h4("Plots"),
                         plotOutput("boxplot")
                )
              )),
      tabItem("multivarmenu",
              tabsetPanel(
                tabPanel("Numeric-Numeric Analysis",
                         h3("Analyse two Numeric Variables"),
                         selectInput("numvar1", "Variable 1", colnames(cropdata)[c(6,7)]),
                         selectInput("numvar2", "Variable", colnames(cropdata)[c(6,7)]),
                         plotOutput("scatplot"),
                         actionButton("actSmooth", "Fit Regression Line")
                         )
              )),
      tabItem("diagmenu",
              tabsetPanel(
                tabPanel("Data Head",
                         tableOutput("tablehead")
                ),
                tabPanel("Data Select",
                         checkboxGroupInput("selvars", "Add/Remove Data Variables", colnames(cropdata)),
                         textInput("selrows", "Select Rows (vector expression)", "1:15"),
                         tableOutput("tableselect")
                )
              )),
      tabItem("timevarmenu",
              tabsetPanel(
                tabPanel(" Time Series Plot ",
                         selectInput("tvar", "Crop", unique(cropdata$Crop)),
                         plotOutput("lineplot")
                )
      )),
      tabItem("statemenu",
              tabsetPanel(
                tabPanel("States",
                         selectInput("svar1", "States", unique(cropdata$State_Name)),
                         selectInput("svar2", "Variable", colnames(cropdata)[c(6,7)]),
                         plotOutput("barplot")
                )
              ))
  )  
))
                



server <- function(input,output){
  
  rv = reactiveValues()
  
  output$summstat = renderTable({
    var = input$numvar
    s = summary(cropdata[,var])
    df = data.frame(Min=s[[1]], Median=s[[3]], Mean=s[[4]], Max=s[[6]])
    df
  })
  
  output$boxplot = renderPlot({
    var = input$numvar
    v = cropdata[[var]]
    quartiles <- quantile(v, probs = c(0.25, 0.75))
    IQR = IQR(v)
    lower = quartiles[1] - 1.5*IQR
    upper = quartiles[2] + 1.5*IQR
    v = as.data.frame(v)
    data_no = subset(v, v > lower & v < upper)
    ggplot(data=data_no) + 
      geom_boxplot(aes(x=v))
  })
  
 
  output$scatplot = renderPlot({
    var1 = input$numvar1
    var2 = input$numvar2
    g = ggplot(data=cropdata, mapping=aes_string(x=var1,y=var2)) + geom_point(color="red") +
      ggtitle(paste0("Scatter-Plot of ", var1, " vs. ", var2))
    if(rv$line) {
      g = g + geom_smooth(method='lm')
    }
    g
  })
  
  observeEvent(input$numvar2, {
    rv$line = 0
  })
  
  observeEvent(input$actSmooth, {
    rv$line = 1
  })
  
  output$tablehead = renderTable({
    cropdata[1:10,]
  })
  
  output$tableselect = renderTable({
    vars = input$selvars
    dat = cbind(data.frame(S.no=1:nrow(cropdata)), cropdata[,vars])
    rows = eval(parse(text=input$selrows))
    dat[rows,]
  })
  
  output$lineplot = renderPlot({
    var = input$tvar
    d = cropdata[cropdata$Crop==var,]
    df = summarise(group_by(d, Crop_Year),
                   tot_prod = sum(Production))
    head(df)
    ggplot(data=df, aes(x = Crop_Year, y = tot_prod)) +
      geom_line(color = 'red', size = 2) + geom_point() +
      xlab("Year") + ylab("Production")
  })
  
  output$barplot = renderPlot({
    var1 = input$svar1
    var2 = input$svar2
    d = cropdata[cropdata$State_Name==var1,]
    d = d[, c('District_Name','Area','Production')]
    # df = summarise(group_by(d, District_Name),
    #                across(everything(), sum), .groups = 'drop')
    df=d%>%group_by(District_Name)%>%summarise(across(everything(),sum),.groups='drop')
    print(df)
    df=df[,c('District_Name',var2)]
    ggplot(df, aes_string(x = 'District_Name', y= colnames(df)[c(2)], fill = "''")) +
      geom_bar(stat = 'identity', width = 0.8) + 
      scale_fill_manual(values=c('#790252')) + 
      theme(axis.text.x = element_text(angle = 90))
  })
}


## main()
shinyApp(ui,server)

  
  
