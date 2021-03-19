library(shiny, lubridate)
ui<-fluidPage(
  tags$head(
    tags$style(HTML("
      .shiny-output-error-validation {
        color: red;
      }
    "))
  ),
  wellPanel(
  fluidRow(
    column(3),
    column(6,
      h1("photoR", style="text-align:center"),
      p(style="text-align:center", "A web-based version of R code originally provided ",
      a("here", href="https://doi.org/10.1111/2041-210X.13015")),
      p(style="text-align:center", "An R package of `photoR` is available ",
      a("here", href="https://www.github.com/jthinke/photoR")),
      p(style="text-align:center","Last updated on 19 March 2021")
    )
  )
  ),

  br(),

  fluidRow(
  column(3),
  column(6, img(height=300, width=800, src="Lifestages.jpg"))
  ),

  br(),

  fluidRow(
    column(4,
      p("Attendance data must have the following headers: 'YEAR','CAMERA', 'NEST', 'SPP','DATE', and 'MAXN'. The 'date' may also be specified in 'YR', 'MON', and 'DAY' headers.")
    ),
    column(4,
      p("Nest content data must have the following headers: 'YEAR', 'CAMERA', 'NEST', 'SPP', DATE', 'COPULATION','LAY',	'MAXE',	'HATCH',	'MAXC',and 'CRECHE'. The 'date' may also be specified in 'YR', 'MON', and 'DAY' headers.")

  ),
    column(4,
      p("Specify the date format used in the input files.")

  )
  ),

  fluidRow(
    column(4,
      fileInput(inputId="attendance", label="Choose attendance data (.csv format)",
            accept = c(
              "text/csv",
              "text/comma-separated-values,text/plain",
              ".csv"))
    ),
    column(4,
      fileInput(inputId="repro", label="Choose nest content data (.csv format)",
            accept = c(
              "text/csv",
             "text/comma-separated-values,text/plain",
              ".csv"))
    ),
    column(4,
           selectInput(inputId = "date_format",
                       label = "Choose date format",
                       choices = list("None selected", "d/m/y", "m/d/y", "y/m/d", "Create from 'YR', 'MON', & 'DAY' columns"))
  )
  ),
  fluidRow(
    column(6,
      selectInput(inputId="table_type", label="Choose output format",
              choices= list("Raw","CEMP Protocol A6b", "CEMP Protocol A9"), selected="Raw")
    ),

  column(6, downloadButton("downloadData", "Download results"),
         p("Download the table of results generated below.")
  )
),

  br(),

  tableOutput(outputId="result")
)

server<-function(input, output){
#  source('phenology_app.R')
#  source('error_checking.R')
#  source('import_validation.R')
#  library(lubridate)
  reac_func_input<-reactive({
    req(input$attendance)
    req(input$repro)
    req(input$date_format)
    # import, check, and format data based on chosen data format
   if(input$date_format=="Create from 'YR', 'MON', & 'DAY' columns"){
     dat<-read.csv(file=input$attendance$datapath, stringsAsFactors=FALSE, header=TRUE)
     rdat<-read.csv(file=input$repro$datapath, stringsAsFactors=FALSE, header=TRUE)
     # check format
     error.out<-import_validation(dat, rdat)
     validate(
       need(length(error.out) == 0,
            message = paste(error.out, collapse = "\n"))
     )
     # if no error, format dates
     validate(
       need(input$date_format!="None selected",
            message = paste("Select appropriate date format from drop-down menu"))
     )
     dat$DATE<-lubridate::ymd(paste(dat$YR, dat$MON, dat$DAY), tz="GMT")
     rdat$DATE<-lubridate::ymd(paste(rdat$YR, rdat$MON, rdat$DAY), tz="GMT")

    } else {

      if(input$date_format=="m/d/y"){
        Dt<-"%m/%d/%Y"
      }
      if(input$date_format=="d/m/y"){
        Dt<-"%d/%m/%Y"
      }
      if(input$date_format=="y/m/d"){
        Dt<-"%Y/%m/%d"
      }
      dat<-read.csv(file=input$attendance$datapath, stringsAsFactors=FALSE, header=TRUE)
      rdat<-read.csv(file=input$repro$datapath, stringsAsFactors=FALSE, header=TRUE)
      # check format
      error.out<-import_validation(dat, rdat)
      validate(
        need(length(error.out) == 0,
             message = paste(error.out, collapse = "\n"))
      )
      # if no error, format dates
      validate(
        need(input$date_format!="None selected",
             message = paste("Select appropriate date format from drop-down menu"))
      )
      dat$DATE<-as.POSIXct(strptime(dat$DATE, format=Dt), tz="GMT")

      rdat$DATE<-as.POSIXct(strptime(rdat$DATE, format=Dt), tz="GMT")
    }
    #out
    list(dat,rdat)
 })
reac_func_output<-reactive({
  #output$result<-renderTable({
  req(input$table_type)
  dat.list <- reac_func_input()

  # Error checking for basic data formating
  error.out <- error_checking(dat=dat.list[[1]], rdat=dat.list[[2]])

   validate(
     need(length(error.out) == 0,
          message = rbind("Please fix the following errors before further analysis:", paste(error.out, collapse = "\n"))
   )
   )

  out<-phenology_app(dat=dat.list[[1]], rdat=dat.list[[2]], tabletype=input$table_type)
  out
})
output$result<-renderTable({
  req(input$table_type)
  dat.list <- reac_func_output()
})
  output$downloadData <- downloadHandler(
    filename = function() {
     paste(input$table_type, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(reac_func_output(), file, row.names=FALSE)
    }
  )
}
shinyApp(ui=ui, server=server)
