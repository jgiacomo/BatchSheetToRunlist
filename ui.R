
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Accium Biosciences - NEC Runlist Generator"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      fileInput('batchSheet', 'Choose the Batch Sheet file',
                accept=c("xlsx","Excel workbook",".xlsx")),
      
      radioButtons("source","Select the source",choices = c("S1","S2"),
                   selected="S1", inline=TRUE),

      fluidRow(
          hr(),
          h4("Define standards and blanks"),
          column(6,
                 textInput("type_OX1",label="OX1"),
                 textInput("typ_OX2",label="OX2"),
                 textInput("typ_C6",label="C6"),
                 textInput("typ_C7",label="C7")
          ),
          
          column(6,
                 textInput("typ_UPGC",label="UPCG"),
                 textInput("typ_Blank",label="Blank"),
                 textInput("typ_C1",label="C1"),
                 textInput("typ_C2",label="C2")
          )
      ),
      
      fluidRow(
          hr(),
          h4("Define measurement groups"),
          column(6,
                 textInput("grp0",label="Group 1"),
                 textInput("grp1",label="Group 2"),
                 textInput("grp2",label="Group 3"),
                 textInput("grp3",label="Group 4")
          ),
          
          column(6,
                 textInput("grp4",label="Group 5"),
                 textInput("grp5",label="Group 6"),
                 textInput("grp6",label="Group 7"),
                 textInput("grp7",label="Group 8")
          )
      )
      
    ),

    # Show a table of the batch samples
    mainPanel(
        verbatimTextOutput('test'),
        tableOutput('batch')
    )
  )
))
