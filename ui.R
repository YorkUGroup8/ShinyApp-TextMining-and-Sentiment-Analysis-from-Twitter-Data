#--- UI ---#
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)


shinyUI <- dashboardPagePlus(
  skin = "blue-light",
  
  #- Dashboard Title
  dashboardHeader(title = span(tagList(icon("twitter"), "Explore Text Mining Technique")), titleWidth = 700),
  
  
  #- Left Menu
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Word-Cloud", tabName = "cloud", icon = icon("cloud")),
      menuItem("Sentimental Analysis", tabName = "sentimental", icon = icon("smile face"))
    )
  ),
  
  dashboardBody(
    
    #- Remove error messages
    tags$style(
      type="text/css",
      ".shiny-output-error { visibility: hidden; }",
      ".shiny-output-error:before { visibility: hidden; }"
    ),
    setShadow("box"),
    
    tabItems(
      #- First TAB
      tabItem(
        tabName = "home",
        fluidRow(
          widgetUserBox(
            title = "Welcome to Group 8 (Text Mining)",
            subtitle = "Joshua, Amin Samnani and Jad",
            width = 7,
            type = 2,
            src = "image/text-mining-icon.png",
            color = "aqua-active",
            align = "center",
            
            socialButton(
              url = "https://github.com/XXXXx",
              type = "github"
            ),
            
            closable = FALSE,
            footer = "This Interface was developed by York University Student, 
                      in order to estimulate researches in text mining area."
          )
        )
      ),
      #- second TAB
      tabItem(
        tabName = "cloud",
        fluidRow(
          box(
            sliderInput("wordfreq",
                        "Select the minimum frequency of word:",
                        min = 1,  max = 50, value = 10),
            hr(),
            sliderInput("maxword",
                        "Select the maximum number of words:",
                        min = 1,  max = 300,  value = 100),
            checkboxInput("random","Random Order?", FALSE),
            radioButtons("color", "Select the word cloud color theme", c("Accent", "Dark"), selected = "Accent"),
            actionButton("update","Create word cloud"),
            footer = "Please click [Create Word Cloud] button to prepare Word cloud"
        ),
        box(
          plotOutput("plotwcloud")
        ),
        box(
          wordcloud2Output("plotwcloud2")
        )
      )
      ),
      #- Third TAB
      tabItem(
       tabName = "sentimental",
        fluidRow(
          box(width = 12,
              plotOutput("Bing_lexicon")
          ),
          box(
            width = 12,
            plotOutput("nrc_lexicon")
          ),
          box(
            width = 12,
            plotOutput("nrc_lexicon_scores")
          )
          
        )
      )
    )
  )
)

