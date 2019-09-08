
library(shiny)
library(shinydashboard)
library(gganimate)
library(shinythemes)

options(scipen = 999)

ui <- navbarPage(
  
  "Data Visualization",
  theme = shinytheme("journal"),
  
  tabPanel(
    
    "Home",
    
    tags$head(
      tags$style(HTML('
                      
                     @import url("//fonts.googleapis.com/css?family=Lobster|Cabin:400,700"); 
                      
                      .hero-image {
                      
                      background-color: #ffffff;
                      height: 860px;
                      background-position: center;
                      background-repeat: no-repeat;
                      background-size: cover;
                      margin-left:-15px;
                      margin-right:-15px;
                      margin-top:-25px;
                      
                      
                      }
                      
                      .hero-text {
                      text-align: center;
                      position: absolute;
                      top: 50%;
                      left: 50%;
                      transform: translate(-50%, -50%);
                      color: white;
                      }
                      
                      .font-home {
                      font-size: 4vw;
                      font-family: "Lobster", cursive;
                      font-weight: 500;
                      line-height: 1.1;
                      color: #02151c;
                      }
                      div.transbox {
                      margin: 30px;
                      background-color: #ffffff;
                      border: 1px solid black;
                      opacity: 0.6;
                      filter: alpha(opacity=60); /* For IE8 and earlier */
                      }
                      
                      '))
      ),
    tags$div(class = "hero-image",
             br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
             
             column(12, align="center", offset = 0,
                    
                    tags$div(class = "font-home",
                             "Data Visualization brings the number to life")),
             
             column(6, align="center", offset = 3,
                    br(),
                
                    h1("Ahmad Husain Abdullah"),
                    h2("20 January, 2019"))
            
    )
    
    
      ),
  
  tabPanel(
    
    "Need to Know",
    
    fluidPage(
      br(),
      
      h4("Source: Knaflic, Cole Nussbaumer. 2015. Storytelling with Data: a data visualization guide for business profesional"),
      hr(),
      
      h2("Data Visualization is one of the way to storytelling with data."),
      h4("How you will learn to tell stories with data:"),
      h4("1. Understand the context"),
      h4("2. Choose an appropiate visual display"),
      h4("3. Tell a story"),
      hr(),
      
      fluidRow(
        column(3, 
               
               h3("Understand the context"),
               
               h4("Before you start down the path of data visualization, there are a couple of 
                  questions that you should be able to concisely answer:
                  Who is your audience? What do you need them to know or do?"),
               
               hr(),
               
               h3("Choosing the right visualization software"),
               
               h4("The best visualization software is the one that
                  allows you to make the figures you need."),
               
               p("Source: Wilke, Claus O.",
                 
                 a("Choosing the right visualization software"), 
                 
                 href = "https://serialmentor.com/dataviz/choosing-visualization-software.html", "."),
               
               hr(),
               
               h3("things you have to consider when choosing tools"),
               
               h4("1. Reproducibility and repeatability"),
               
               h4("2. Data exploration versus data presentation"),
               
               h4("3. Separation of content and design")
               
               
               ),
        
        column(5,
               h3("Choosing an effective visual"),
               
               h4("There are many different graphs and other types of visual displays of
                  information, in this summary provides the various plots and charts that
                  are commonly used to visualize data"),
               
               p("Source: Wilke, Claus O.", a("Fundamentals of Data Visualization"), href = "https://serialmentor.com/dataviz/index.html", "."),
               
               selectInput(inputId = "typeviz", 
                           label = "Varians plot",
                           choices = c("Amounts",
                                       "Distributions",
                                       "Relationships")),
               
               br(),
               
               imageOutput(outputId = "image", height = "400px")
               
               ),
        
        column(4,
               h3("Tell a story and making a point"),
               h4("Most data visualization is done for the purpose of communication. 
                 We have an insight about a dataset, and we have a potential audience, 
                 and we would like to convey our insight to our audience. To communicate our 
                 insight successfully, we will have to present the audience with a clear and 
                 exciting story."),
               
               br(), 
               br(),
               br(),
               
               imageOutput("storytelling"))
        ),
        
        hr()

    )
    
  ),
  
  tabPanel(
    
    "Explore",
    sidebarLayout(
      sidebarPanel(width = 3,
                   
                   fileInput(inputId = "FileName",
                             label = "Choose File",
                             multiple = TRUE,
                             accept= ".csv"),
                   
                   radioButtons(inputId = "sep", 
                                label = "Separator",
                                choices = c(Semicolon = ";",
                                            Comma = ",",
                                            Tab = "\t"),
                                selected = ","),
                   
                   uiOutput(outputId = "axisx"),
                   
                   uiOutput(outputId = "axisy"),
                   
                   uiOutput(outputId = "group"),
                   
                   selectInput(inputId = "theme",
                               label = "Choose your theme: ", 
                               choices = c("classic",
                                           "pander",
                                           "minimal",
                                           "linedraw")),
                   
                   selectInput("plot.type",
                               "Choose your Plot Type:",
                               list(boxplot = "boxplot", 
                                    histogram = "histogram", 
                                    density = "density", 
                                    bar = "bar",
                                    point = "point")),
                   
                   p("Created by", a("Ahmad Husain Abdullah", href = "https://www.linkedin.com/in/ahmadhusainabdullah/"), "."),
                   
                   img(src = "http://www.gravatar.com/avatar/faa60428b46ecc30beeef02974e49d47?s=64", width = "70px", height = "70px")
      ),
      
      mainPanel(
        tabsetPanel(
  
                   tabPanel("Plot",
                            br(),br(),
                            box(plotOutput(outputId = "plot", 
                                           width = "800px", 
                                           height = "500px"),
                                width = 12,
                                height = 12)),
                   
                   tabPanel("Data Table",
                            br(),br(),
                            box(
                              dataTableOutput(outputId = "fileinput"), 
                              style = 'overflow-x: auto', 
                              width = 12))
          
        )
      )
    )
  )
  
  
)


server <- function(input, output) {
  dataset <- reactive({
    inFile<-input$FileName
    
    if (is.null(inFile))
      return(NULL)
    
    data <-  read.csv(inFile$datapath, sep = input$sep, header = TRUE)
  })
  
  output$fileinput <- renderDataTable({
    inFile<-input$FileName
    
    if (is.null(inFile))
      return(NULL)
    dataset()
  })
  
  choicesnum <- reactive({
    inFile<-input$FileName
    
    if (is.null(inFile))
      return(NULL)
    dplyr::select_if(dataset(), is.numeric)
  })
  
  choicesfac <- reactive({
    inFile<-input$FileName
    
    if (is.null(inFile))
      return(NULL)
    dplyr::select_if(dataset(), is.factor)
  })
  
  output$axisx <- renderUI(selectizeInput(inputId = "x", 
                                          multiple = TRUE,
                                          label = "Choose your variable: ",
                                          choices = colnames(choicesnum())))

  output$group <- renderUI(selectInput(inputId = "group",
                                       label = "Group by: ",
                                       choices = colnames(choicesfac())))
  
variable <- reactive({
  

  inFile<-input$FileName
  
  if (is.null(inFile))
    return(NULL)
  
  var<-list(data=dataset(),
            x=input$x,
            y=input$y,
            group=input$group
            
  )
})


                  
  output$plot <- renderPlot({
    
    if (is.null(input$x))
      return(h3("Nothing variable has been selected yet"))
    
    varaes<-variable()
    
    #plot types
    plot.type <- switch(input$plot.type,
                        "boxplot" 	= geom_boxplot(),
                        "histogram" =	geom_histogram(alpha=0.75,position="identity"),
                        "density" 	=	geom_density(alpha=.75),
                        "bar" 		=	geom_bar(position="dodge"),
                        "point" = geom_point(size = 7, alpha = 2/3))
    
    theme <- switch(input$theme,
                    "classic" = theme_classic(),
                    "pander" = ggthemes::theme_pander(),
                    "minimal" = theme_minimal(),
                    "linedraw" = theme_linedraw())
    
    if (is.null(varaes)) 
      return(NULL)
    

    
    if (input$plot.type == "boxplot") {
  
      
      g <- ggplot(data = varaes$data,
                  aes_string(
                    x = varaes$group,
                    y = varaes$x[1],
                    fill = varaes$group)
                  ) +
        plot.type +
        geom_point(color='black',
                   alpha=0.5, 
                   position = 'jitter') + 
        theme
      
      print(g)
                  
    }
    
    else if (input$plot.type == "point") {
      
      g <- ggplot(data = varaes$data,
                  aes_string(
                    x = varaes$x[1],
                    y = varaes$x[2],
                    col = varaes$group)
      ) +
        plot.type +
        theme
      
      print(g)
      
    }
    
    else {
      
      g <- ggplot(varaes$data,
                aes_string(
                  x 		= varaes$x[1],
                  fill 	= varaes$group,
                  group 	= varaes$group
                )
              ) + 
        plot.type +
        theme
      
      print(g)

    }

    
  })
  
  
  output$image <- renderImage({

    varians.image <- switch(input$typeviz,
                            "Amounts" = list(src = "www/ranking.jpg.png",
                                             width = 420,
                                             height = 400),
                            "Distributions" = list(src = "www/distributions.png",
                                                   width = 420,
                                                   height = 400),
                            "Relationships" = list(src = "www/correlation.jpg.png",
                                                   width = 420,
                                                   height = 400))
    
    
    print(varians.image)
    
  }, deleteFile = FALSE)
  
  
  output$storytelling <- renderImage({
    

    list(src = "www/storytelling.png")

  }, deleteFile = FALSE)

  

  
}

shinyApp(ui, server)
