#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Northwestern Bot Start"),

    mainPanel(
        selectInput("num", label = "Bot On or Off", choices = c(
            "On",
            "Off"
        )),
        actionButton("update", "Update 1"),
        actionButton("update2", "Update 2"),
        textOutput("up"),

        )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    # Action Button
    reactive(input$num)
    
    observeEvent(input$update2,{
    while(input$num == "On"){twitter_bot(z = 2)
        Sys.sleep(5)
        reactive(input$num)}
        })
    
    output$up <- renderText({print(input$num)})
}

# Run the application 
shinyApp(ui = ui, server = server)
