
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinythemes)

shinyUI(fluidPage(
  fluidPage(theme = shinytheme("journal"),
            tags$head(
              tags$style(HTML("
                              @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
                              
                              h1 {
                              font-family: 'Lobster', cursive;
                              font-weight: 500;
                              line-height: 1.1;
                              color: #48ca3b;
                              }
                              h2 {
                              font-family: 'Lobster', cursive;
                              font-weight: 500;
                              line-height: 1.1;
                              color: gray;
                              }
                              h3 {
                              font-weight: 500;
                              line-height: 1.1;
                              color: blue;
                              }
                              
                              "))
                  ),                
                  
  headerPanel("Monty Hall Game"),
  sidebarLayout(
    sidebarPanel(
      HTML("<table style='width: 100%;'>"),
      HTML("<td style='width: 33%;' align='center' valign='middle'><b>"),
      actionButton('buttonDoor1', "Door 1"),
      HTML("</b></td>"),
      
      HTML("<td style='width: 33%;' align='center' valign='middle'><b>"),
      actionButton('buttonDoor2', "Door 2"),
      HTML("</b></td>"),
      
      HTML("<td style='width: 33%;' align='center' valign='middle'><b>"),
      
      actionButton('buttonDoor3', "Door 3"),
      HTML("</b></td>"),
      HTML("</table>"),
      
      uiOutput('myoutput')
    ),

    # Show a plot of the generated distribution
    mainPanel(
      uiOutput('mainpanel')
    )
  )
)))