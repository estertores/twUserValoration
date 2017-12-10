library(shiny)
library(shinyjs)
library(shinycssloaders)
library(knitr)
library(timevis)

source("keys.R")
source("connect.R")

val1 <- new("valoration", frequency=0.1, repercussionRT=5, repercussionFAV=10, lambda=105, mu=158)
val2 <- new("valoration", frequency=2, repercussionRT=25, repercussionFAV=100, lambda=693, mu=1000)

ui <- fluidPage(
  shinyjs::useShinyjs(),
  h1("Twitter User Valoration"),
  
  fluidRow(
    column(width = 4,
           textInput(inputId = "twitteruser",
                     label = "add twitter profile:",
                     value = "estertores"),
           
           actionButton(inputId = "loadUserButton", 
                        label = "load user"),
           
           shinyjs::hidden(actionButton(inputId = "loadTimeLineButton", 
                                        label = "load timeline")),
           hr(),
           h4("valoration1 params"),
           tags$style(HTML("#valoration1Params, #valoration2Params {font-size:10px;height:100px;padding:1px}")),
           wellPanel(id="valoration1Params", fluidRow(
             tags$style("#val1_FrqInput, #val1_LambdaInput, #val1_RTInput, #val1_FAVInput, #val1_MuInput
                        {font-size:10px;height:20px;}"),
             column(width = 4, 
                    textInput(inputId = "val1_FrqInput", label = "freq", value = val1@frequency),
                    textInput(inputId = "val1_LambdaInput", label = "lambda", value = val1@lambda)),
             column(width = 4,
                    textInput(inputId = "val1_RTInput", label = "rt", value = val1@repercussionRT),
                    textInput(inputId = "val1_MuInput", label = "mu", value = val1@mu)),
             column(width = 4,
                    textInput(inputId = "val1_FAVInput", label = "fav", value = val1@repercussionFAV))
           )),

           h4("valoration2 params"),
           wellPanel(id="valoration2Params", fluidRow(
             tags$style("#val2_FrqInput, #val2_LambdaInput, #val2_RTInput, #val2_FAVInput, #val2_MuInput
                        {font-size:10px;height:20px;}"),
             column(width = 4, 
                    textInput(inputId = "val2_FrqInput", label = "freq", value = val2@frequency),
                    textInput(inputId = "val2_LambdaInput", label = "lambda", value = val2@lambda)),
             column(width = 4,
                    textInput(inputId = "val2_RTInput", label = "rt", value = val2@repercussionRT),
                    textInput(inputId = "val2_MuInput", label = "mu", value = val2@mu)),
             column(width = 4,
                    textInput(inputId = "val2_FAVInput", label = "fav", value = val2@repercussionFAV))
           ))
    ),
    
    column(width = 8,
           tabsetPanel(
             tabPanel(title="user",
                      column(width = 12,
                             h3("user info"),
                             column(width = 6,
                                    withSpinner( htmlOutput("userInfo") )),
                             column(width = 6,  
                                    shinyjs::hidden(sliderInput("simulateFollowersSlider", label = "simulate number of followers", 
                                                                min = 0, max=100, value = 50)))),
                      column(width = 12,
                             h3("valoration"),
                             withSpinner( tableOutput("valoration") ))
                             # timevisOutput("timelinePlot") )
             ),
             tabPanel(title="all users")
           )
    )
  ))

timeLine <- list()

server <- function(input, output, session) {
  
  userInput <- eventReactive(input$loadUserButton, {
    try( expr = { getUser( input$twitteruser ) }, silent = T)
  }, ignoreNULL = FALSE, ignoreInit = T)

  output$userInfo <- renderUI({
    user <- userInput()
    if( isS4(user) ){
      shinyjs::show("loadTimeLineButton")
      shinyjs::show("simulateFollowersSlider")
      data <- paste0("<ul> <li><b>name: </b>", user$name,"</li>")
      data <- paste0(data, "<li><b>created: </b>", user$created,"</li>")
      data <- paste0(data, "<li><b>tweets: </b>", user$statusesCount,"</li>")
      data <- paste0(data, "<li><b>followers: </b>", user$followersCount,"</li>")
      data <- paste0(data, "<li><b>friends: </b>", user$friendsCount,"</li></ul>")
      updateSliderInput(session, inputId = "simulateFollowersSlider", value = user$followersCount,
                        min = 0, max = 5*user$followersCount)
      HTML(data)
    }else{
      shinyjs::hide("loadTimeLineButton")
      shinyjs::hide("simulateFollowersSlider")
      HTML( "<b>NOT FOUND!</b>" )
    }
  })

  timeLineInput <- eventReactive(input$loadTimeLineButton, {
    tl <- GetTimeLine( input$twitteruser )
    timeLine[[ length(timeLine)+1 ]] <<- tl
    T
  }, ignoreNULL = FALSE, ignoreInit = T)

  output$valoration <- renderTable({
    if( timeLineInput() ){
      cat(paste("caca length(timeLine): ", length(timeLine), "\n"))
      val1@frequency <- as.numeric(input$val1_FrqInput)
      val1@repercussionRT <- as.numeric(input$val1_RTInput)
      val1@repercussionFAV <- as.numeric(input$val1_FAVInput)
      val1@lambda <- as.numeric(input$val1_LambdaInput)
      val1@mu <- as.numeric(input$val1_MuInput)
  
      val2@frequency <- as.numeric(input$val2_FrqInput)
      val2@repercussionRT <- as.numeric(input$val2_RTInput)
      val2@repercussionFAV <- as.numeric(input$val2_FAVInput)
      val2@lambda <- as.numeric(input$val2_LambdaInput)
      val2@mu <- as.numeric(input$val2_MuInput)
  
      rep1 <- GetRepercussionIndex(timeLine[[length(timeLine)]], val1)
      rep2 <- GetRepercussionIndex(timeLine[[length(timeLine)]], val2)
  
      followersCount <- input$simulateFollowersSlider
  
      f1v1 <- f1(followersCount, val1@lambda)
      f1v2 <- f1(followersCount, val2@lambda)
  
      f2v1 <- f2(followersCount, val1@mu)
      f2v2 <- f2(followersCount, val2@mu)
  
      df <- data.frame( rep = c(rep1, rep2),
                        exp = c(f1v1, f1v2),
                        arctan = c(f2v1, f2v2),
                        V_exp = 100*c(rep1*f1v1, rep2*f1v2),
                        V_arctan = 100*c(rep1*f2v1, rep2*f2v2))
      rownames(df) <- c("val1", "val2")
  
      df$rep <- sprintf('%0.9f', df$rep)
      df$exp <- sprintf('%0.5f', df$exp)
      df$arctan <- sprintf('%0.5f', df$arctan)
      df$V_exp <- sprintf('%.2f', df$V_exp)
      df$V_arctan <- sprintf('%.2f', df$V_arctan)
      df
    }
  }, rownames = T)
  
  # output$timelinePlot <- renderTimevis({
  #   if(timeLineInput()){
  #     timevis(GetTimeLineSummary(timeLine[[length(timeLine)]]))
  #   }
  # })
}

# Create Shiny app ----
shinyApp(ui, server)