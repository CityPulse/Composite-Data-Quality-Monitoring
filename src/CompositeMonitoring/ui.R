library(shiny)
library(leaflet)
#install.packages("shinyBS")
library(shinyBS)

#simple_ds <- names(simple_datasets)
#combined_ds <- list(paste(names(combined_datasets_data),'<->',names(combined_datasets_metainformation)))
#combined_ds_data <- names(combined_datasets_data)
#combined_ds_data <- combined_datasets_metainformation


timeframe = list(
  "Current"="current",
  "Hourly"="hourly",
  "Daily"="daily"  
)


colorconfig <- c(
  "Full Scale" = "fullscale",
  "Adopted Scale" = "adoptedscale"
)
colorchoice <- c(
  "Red-Yellow-Green" = "RdYlGn",
  "Spektral" = "Spectral"
)
vertsize=135;

shinyUI(navbarPage("CityPulse Composite Monitoring Explorer", id="nav",
                   tabPanel("Interactive map",
                            div(class="outer",
                                tags$head( # Include our custom CSS
                                  includeCSS("styles.css")#,
                                  #includeScript("gomap.js")
                                ),
                                
                                leafletOutput("map", width="100%", height="100%"),
                                
                                # Shiny versions prior to 0.11 should use class="modal" instead.
                                absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                              draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                              width = 700, height = "auto",
                                              fluidPage(
                                                # fluidRow(
                                                #   column(12,
                                                #          selectInput("simple_select", "Simple Datasets:", simple_ds)
                                                #   )),
                                                # fluidRow(
                                                #   column(12,selectInput("combined_select", "Combined Datasets:", combined_ds))
                                                # ),
                                                fluidRow(
                                                   column(12,
                                                         textOutput('qoitext'),                   
                                                         plotOutput("qoiHistFrequency", height = 1.2*vertsize)
                                                  )
                                                ),#fluidRow(
                                                #  column(12,
                                                #         dateRangeInput('dateSelect', 'Chose Time Frame', 
                                                #                        start = min(simple_datasets[[1]]$import_time_stamp), end = max(simple_datasets[[1]]$import_time_stamp), 
                                                #                        min = min(simple_datasets[[1]]$import_time_stamp), max = max(simple_datasets[[1]]$import_time_stamp), 
                                                #                        format = "yyyy-mm-dd", startview = "month", weekstart = 0, 
                                                #                        language = "en", separator = " to "))
                                                #),
                                                fluidRow(
                                                  column(11,DT::dataTableOutput("shown_layer_table"))
                                                  
                                                ),
                                                fluidRow(
                                                           column(6,
                                                                  bsButton("clear_map_button", label = "Clear Map", icon = icon("clear"))
                                                           )
                                                           )
                                              )
                                ),
                                
                                tags$div(id="cite",
                                         'Data compiled from CityPulse ', tags$em('Open Datasets available at'), ' http://www.ict-citypulse.eu/'
                                )
                            )
                   ),
                   tabPanel("Add Dataset",
                            fluidRow(
                              column(3,selectInput("simple_select", "Simple Datasets in Access:",  names(simple_datasets))),
                              column(3,selectInput("simple_select_id", "Id Column:",  NA)),
                              column(3,selectInput("simple_select_time", "Time Column:",  NA)),
                              column(3,selectInput("simple_select_value", "Value Column:",  NA))
                            ),fluidRow(
                              column(6,
                                     dateRangeInput('dateSelect', 'Chose Time Frame', 
                                                    start = min(simple_datasets[[1]]$import_time_stamp), end = max(simple_datasets[[1]]$import_time_stamp), 
                                                    min = min(simple_datasets[[1]]$import_time_stamp), max = max(simple_datasets[[1]]$import_time_stamp), 
                                                    format = "yyyy-mm-dd", startview = "month", weekstart = 0, 
                                                    language = "en", separator = " to ")
                                     ),
                            
                                      column(6,
                                             bsButton("simple_data_button", label = "Add Dataset to Map", icon = icon("plus"))
                                             )
                            ),
                            # Create a new row for the table.
                            fluidRow(
                              DT::dataTableOutput("simple_table")
                            )
                   ),tabPanel("Join Metadata with Dataset",
                              bsButton("combine_data_button", label = "Add Joined Dataset", icon = icon("ban")),
                              
                              fluidRow(
                                column(3,selectInput("combined_select_data", "Combined Datasets:", names(combined_datasets_data))),
                                column(3,selectInput("combined_join_data", "Join Column:",c(NA)))
                              ),
                              fluidRow(
                                column(11,DT::dataTableOutput("combined_datasets_data_table"))
                              ),
                              fluidRow(
                                column(3,selectInput("combined_select_meta", "Combined Metadata:", names(combined_datasets_metainformation))),
                                column(3,selectInput("combined_join_meta", "Join Column:", c(NA)))
                              ),
                              fluidRow(
                                column(11,DT::dataTableOutput("combined_datasets_metainformation"))
                              )

                   ),
                   tabPanel("Configuration",
                            selectInput("colorscale", "Color Scale", colorconfig, selected = 'fullscale'), 
                            selectInput("colorchooser", "Color Choice", colorchoice, selected = 'Red-Yellow-Green')
                   ),

                   tabPanel("Import",
                            sidebarLayout(
                              sidebarPanel(
                                fileInput('file1', 'Choose file to upload',
                                          accept = c(
                                            'text/csv',
                                            'text/comma-separated-values',
                                            'text/tab-separated-values',
                                            'text/plain',
                                            '.csv',
                                            '.tsv'
                                          )
                                ),
                                tags$hr(),
                                checkboxInput('header', 'Header', TRUE),
                                radioButtons('sep', 'Separator',
                                             c(Comma=',',
                                               Semicolon=';',
                                               Tab='\t'),
                                             ','),
                                radioButtons('quote', 'Quote',
                                             c(None='',
                                               'Double Quote'='"',
                                               'Single Quote'="'"),
                                             '"'),
                                tags$hr(),
                                p('If you want a sample .csv or .tsv file to upload,',
                                  'you can first download the sample',
                                  a(href = 'mtcars.csv', 'mtcars.csv'), 'or',
                                  a(href = 'pressure.tsv', 'pressure.tsv'),
                                  'files, and then try uploading them.'
                                )
                              ),
                              mainPanel(
                                tableOutput('contents')
                              )
                            )
                   ),
                   conditionalPanel("false", icon("crosshair"))
))

