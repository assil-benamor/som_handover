#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(RMySQL)
library(DBI)


myForm <- shinyreforms::ShinyForm$new(
    "myForm",
    submit = "Check IDP site",
    onSuccess = function(self, input, output) {
        
     
        
        
        site_id <- self$getValue(input, "sid_input")

        code <- dff %>% filter(pcode == site_id) %>% pull(times)
        
        if(code==0){
            output$result <- shiny::renderUI({
                HTML(paste('<font color="green"><b>',
                           paste(" ",site_id,"This site has not been visited before!",sep = "<br/>")),
                     '</b></font>'
                )

            })
        } else if(code==1) {
            output$result <- shiny::renderText({
                HTML(paste('<font color="green"><b>',
                           paste(" ",site_id,paste0("This site has been visited 1 time already! You can conduct up to 3 more surveys!"),sep = "<br/>")),
                     '</b></font>'
                )
            })} else if(code<4) {
            output$result <- shiny::renderText({
               HTML(paste('<font color="green"><b>',
                   paste(" ",site_id,paste0("This site has been visited ",code , " times already! You can conduct up to ",4-code ," more survey(s)!"),sep = "<br/>")),
                   '</b></font>'
               )
            })}
            
            else{
                output$result <- shiny::renderText({
                    HTML(paste('<font color="red"><b>',
                        paste(" ",site_id,paste0("This site has been assesed ",code," times already! Please don't collect data from this site !"),sep = "<br/>")),
                    '</b></font>')
                }) 
            }
            
    },
    
    onError = function(self, input, output) {
        output$result <- shiny::renderText({
            "Form is invalid!"
        })
    },
    shinyreforms::validatedInput(
        shiny::textInput("sid_input", label = "IDP code"),
        helpText="IDP code format should be CCCM-SO-XXXX-XXXX or  CCCM-SO-XXXXXX-XXXX for Banadir region",
        validators = c(
            shinyreforms::ValidatorMinLength(16),
            shinyreforms::ValidatorMaxLength(18),
            shinyreforms::Validator$new(
                test = function(value) {
                    stringr::str_detect(value,"^CCCM-SO[0-9]{4,6}-[0-9]{4}$")
                },
                failMessage = "IDP code format is incorrect"
            ),
            shinyreforms::Validator$new(
                test = function(value) {
                    return(value %in% valid_idp_list )
                    },
                failMessage = "IDP code is wrong"
            )
        )
    )
    
   
)

# Define UI for application that draws a histogram
ui <- shiny::bootstrapPage(
    shinyreforms::shinyReformsPage(  # This adds a dependency on shinyreforms .css
        shiny::fluidPage(
            shiny::tags$h1("DSA 2021 - IDP Sites checker"),
            myForm$ui(),  # <- ShinyForm will be included here!
            # shiny::tags$h4("Code:"),
            shiny::htmlOutput("result")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    conn <- dbConnect(MySQL(), user="user", db="dsa", host="165.232.64.110", password="--lilos@404@Sql__")
    dff <<- dbReadTable(conn, "idp")
    on.exit(dbDisconnect(conn))
    valid_idp_list <<- read.csv("db.csv") %>% pull(pcode)
    myForm$server(input, output)
}

# Run the application 
shinyApp(ui = ui, server = server)
