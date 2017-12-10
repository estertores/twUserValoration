library(shiny)
library(shinyjs)
library(shinycssloaders)
library(knitr)
library(DT)

source("keys.R")
source("connect.R")

val1 <- new("valoration", frequency=0.1, repercussionRT=5, repercussionFAV=10, lambda=105, mu=158)
val2 <- new("valoration", frequency=2, repercussionRT=25, repercussionFAV=100, lambda=693, mu=1000)

setClass("data",
         slots = c(timeLine="list", 
                   username="character", 
                   followers="numeric"))

ui <- fluidPage(
  shinyjs::useShinyjs(),
  h1("Twitter User Valoration"),
  
  fluidRow(
    column(width = 4,
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
           
           hr(),
            
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
                        textInput(inputId = "twitteruser",
                                  label = "add twitter profile:",
                                  value = "estertores"),
                        
                        actionButton(inputId = "loadUserButton", 
                                     label = "load user"),
                        
                        shinyjs::hidden(actionButton(inputId = "loadTimeLineButton", 
                                                     label = "load timeline"))),
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
             tabPanel(title="all users",
                      column(width = 12,
                             DT::dataTableOutput("allLoadedUsersTable")))
           )
    )
  ))

loadedData <- list()

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
    user <- userInput()
    loadedData[[ length(loadedData)+1 ]] <<- new("data", 
                                                 timeLine = tl, 
                                                 username = input$twitteruser, 
                                                 followers= user$followersCount)
    T
  }, ignoreNULL = FALSE, ignoreInit = T)

  output$valoration <- renderTable({
    if( timeLineInput() ){

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
  
      rep1 <- GetRepercussionIndex(loadedData[[length(loadedData)]]@timeLine, val1)
      rep2 <- GetRepercussionIndex(loadedData[[length(loadedData)]]@timeLine, val2)
  
      followersCount <- input$simulateFollowersSlider
  
      f1v1 <- f1(followersCount, val1@lambda)
      f1v2 <- f1(followersCount, val2@lambda)
  
      f2v1 <- f2(followersCount, val1@mu)
      f2v2 <- f2(followersCount, val2@mu)
  
      df <- data.frame( rep = c(rep1, rep2),
                        F1 = c(f1v1, f1v2),
                        F2 = c(f2v1, f2v2),
                        hitScoreF1 = 100*c(rep1*f1v1, rep2*f1v2),
                        hitScoreF2 = 100*c(rep1*f2v1, rep2*f2v2))
      rownames(df) <- c("val1", "val2")
  
      df$rep <- sprintf('%0.9f', df$rep)
      df$F1 <- sprintf('%0.5f', df$F1)
      df$F2 <- sprintf('%0.5f', df$F2)
      df$hitScoreF1 <- sprintf('%.2f', df$hitScoreF1)
      df$hitScoreF2 <- sprintf('%.2f', df$hitScoreF2)
      df
    }
  }, rownames = T)
  
  output$allLoadedUsersTable <- DT::renderDataTable({
    if( timeLineInput() ){

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
      
      hitScoreV1F1AllUsers <- sapply(loadedData, function(x){ 
        GetRepercussionIndex(x@timeLine, val1) * f1(x@followers, val1@lambda) * 100
      })
      hitScoreV1F2AllUsers <- sapply(loadedData, function(x){ 
        GetRepercussionIndex(x@timeLine, val1) * f2(x@followers, val1@mu) * 100
      })
      
      hitScoreV2F1AllUsers <- sapply(loadedData, function(x){ 
        GetRepercussionIndex(x@timeLine, val2) * f1(x@followers, val2@lambda) * 100
      })
      hitScoreV2F2AllUsers <- sapply(loadedData, function(x){ 
        GetRepercussionIndex(x@timeLine, val2) * f2(x@followers, val2@mu) * 100
      })
      
      followersAllUsers <- sapply(loadedData, function(x) x@followers)
      namesAllUsers <- sapply(loadedData, function(x) x@username)
      
      df <- data.frame( followers = followersAllUsers,
                        hitScoreVal1F1 = hitScoreV1F1AllUsers,
                        hitScoreVal1F2 = hitScoreV1F2AllUsers,
                        hitScoreVal2F1 = hitScoreV2F1AllUsers,
                        hitScoreVal2F2 = hitScoreV2F2AllUsers)
      rownames(df) <- namesAllUsers
      
      df$followers <- sprintf('%d', df$followers)
      df$hitScoreVal1F1 <- sprintf('%0.2f', df$hitScoreVal1F1)
      df$hitScoreVal1F2 <- sprintf('%0.2f', df$hitScoreVal1F2)
      df$hitScoreVal2F1 <- sprintf('%0.2f', df$hitScoreVal2F1)
      df$hitScoreVal2F2 <- sprintf('%0.2f', df$hitScoreVal2F2)

      sketch = htmltools::withTags(table(
        class = 'display',
        thead(
          tr(
            th(rowspan = 1, ''),
            th(colspan = 4, 'HitScore')
          ),
          tr(
            th(rowspan = 1, ''),
            th(colspan = 2, 'Valoration1'),
            th(colspan = 2, 'Valoration2')
          ),
          tr(
            th(rowspan = 1, 'Followers'),
            th(colspan = 1, 'f1'),
            th(colspan = 1, 'f2'),
            th(colspan = 1, 'f1'),
            th(colspan = 1, 'f2')
          )
        )
      ))
      DT::datatable(df)
    }
  })
}

# Create Shiny app ----
shinyApp(ui, server)