context("Shiny application runs successfully")
library(shiny)

ui <- navbarPage(title = "LOGAN",

             tabPanel("Overview",
                      #wellPanel(
                        tags$h1(tags$strong("LOGAN"), ": Log File Analysis in International Large-scale Assessments"),

                      tags$p(""),
                      tags$style(type="text/css",
                                 ".test_type {color: black;
                           font-size: 10px;
                           font-style: italic;}"
                      ),

                      div(class="test_type",
                          tags$p("\"Logan is the human alter-ego of a superhero called Wolverine. While Wolverine is a primal, comprehensible mutant, Logan
                                    is mysterious and complex. Hey, this sounds like the log files from educational assessments! They can be powerful,
                                    but few people are willing to explore their complexities. This is a perfect name for an R package!\", thoughts of a
                                   postdoc (I swear, she wasn't drunk!)")),

                      tags$p(""),
                      tags$p("Welcome to the LOGAN Shiny app. I am glad you are here and I invite you to take a look at some functionalities of the
                             LOGAN R package (under development). By the way, are you an R user? How are your programming skills? Even if you don't intend to use R, please stay here with
                             me and let's explore the package together."),
                      tags$p("LOGAN aims to enable users to handle the dataset cleaning for conducting specific analyses with
                              the log files from two international educational assessments:", tags$a(href = "http://www.oecd.org/pisa/data/",
                              "Programme of International Student Assessment (PISA)"), " and ",
                             tags$a(href = "http://www.oecd.org/skills/piaac/publicdataandanalysis/", "Programme for International Assessment of Adult
                                   Competencies (PIAAC)."), "The package has been designed in modules to facilitate the user experience when interacting with the R code."),
                      tags$p("Module 0 deals with the preparation of the log file data for each item in the assessment. The remaining modules will enable users
                               to conduct data analysis using specific approaches as illustrated in the figure below. These approaches comprise but are not
                               limited to information on time on task and sequences and occurrence of actions. The next three tabs will show you some functios from Modules 0 to 2."),

                      fluidRow(
                        column(4, offset = 3, tags$img(height = 450,
                                                       width = 500, src = 'ModulesDescriptionv2.png'))),
                      tags$p("Did you like the LOGAN app? Please leave your feedback at", tags$a(href = "https://goo.gl/forms/QqB3a1WMzNF4YPrj2", "LOGAN FEEDBACK"),
                             "or write to d.r.costa@cemo.uio.no. Thank you very much!")
                      ),#)
             tabPanel("Module 0",
                      tags$h1(tags$strong("Module 0"), ": Data preparation"),

                      div(class="test_type",
                          tags$p("\"An item is the heart of an assessment\" (myself, on a inspring flight day)")),

                      tags$p("In an educational assessment, each item has its features and each log file will carry on its specificities. To illustrate this,
                             here we will visit an example of a constructed response item for the PISA 2012 creative problem-solving assessment."),

                      tags$hr(), #horizontal line
                      tags$h2("Climate control item:"),
                      tags$p("This item was analyzed by Greiff, Wustenberg, and Avvisati (2015) and Xu, Fang, Chen, Liu, and Ying (2018). It involves
                             the student's interactions with input variables (i.e., to move the top control, central control, and bottom control sliders;
                             left side of the figure below) and output variables (i.e., temperature, humidity; right side
                             of the figure). The interactivity of the item is related to the clicks on the variables and it is not dynamic, meaning
                             the outcomes do not change by themselves without the student's interaction. The item is considered solved if the diagram is completed correctly."),

                      fluidRow(
                        column(4, offset = 3, tags$img(height = 500,
                                                       width = 700, src = 'CP025Q01.png'))),

                      tags$hr(), #horizontal line
                      tags$h2("Log-file data:"),
                      tags$p("The interactive table below shows the public log-file data from this item. It was downloaded from the",
                             tags$a(href = "http://www.oecd.org/pisa/pisaproducts/database-cbapisa2012.htm", "OECD website.")),

                      fluidRow(
                        column(12,
                        dataTableOutput('table')
                        )
                      ),

                      tags$hr(), #horizontal line
                      tags$h2("Thirteen variables and almost one million entries. What now?"),
                      tags$p("At a first glance, it is not easy to make associations about the log data and the item itself. The first 17 entries of the log file
                             display a student from country \"ARE\" with identification StIDStd= \"04852\" and schoolid=\"0000189\". The variable \"event\" indicates
                             all actions recorded from this student. The figure below intends to illustrate the link between the variables in the log file and the item."),

                      fluidRow(
                        column(4, offset = 3, tags$img(height = 800,
                                                       width = 900, src = 'logFileSPSS.png'))),

                      tags$hr(), #horizontal line
                      tags$h2("Got it! How to start the log file analysis?"),
                      tags$p("After understanding the file, we need to clean the data (I know it is not fun!). To help you in this journey, in the LOGAN R package you can
                             check a summary of the number of event actions (including \"START_ITEM\" and \"END_ITEM\") of the students per country.
                             "),

                      selectInput(inputId = "country0", label = "Summary of students' event actions by:",
                                  choices = as.character(unique(cp025q01.data$cnt)),
                                  selected = "ARE") ,
                      verbatimTextOutput("nActionsbyVar"),

                      tags$p("With this summary, one can see that there is one student at country=\"BGR\", for example, with only one
                             event action. To confirm it, please check student's identification StIDStd= \"0000119\" and schoolid=\"03413\" in the table.
                             For the next step, would you consider this student in your analysis? This decision is up to you. In our example here,
                             this case was dropped as well as those that don't follow the rules below:"),

                      tags$div(
                        tags$ul(
                          tags$li("Having both and unique \"START_ITEM\" and \"END_ITEM\" event actions. Students presenting duplication with this variables were
                                  dropped (example: the students with StIDStd= \"06640\" and schoolid=\"0000261\" from country=\"ARE\" and
                                  StIDStd= \"03413\" and schoolid=\"0000119\" from country=\"BGR\" were excluded using this criterium.)")
                        ),
                        tags$ul(
                          tags$li("Having valid results from the performance variables (\"CP025Q01\" and \"PV1CPRO\" also available at the",
                                  tags$a(href = "http://www.oecd.org/pisa/pisaproducts/database-cbapisa2012.htm", "OECD website"),")")
                        )
                      ),

                      tags$p("After a detailed look at all issues related to the data, you are ready to move on to the next module of the package.
                             Enjoy the experience!"),

                      tags$hr(), #horizontal line
                      tags$h2("References:"),
                      tags$p("Greiff, S., Wustenberg, S., & Avvisati, F. (2015). Computer-generated log-file analyses as a window
                             into students' minds? A showcase study based on the PISA 2012 assessment of problem solving. Computers
                             and Education, 91, 92-105. https://doi.org/10.1016/j.compedu.2015.10.018"),
                      tags$p(
                        "Xu, H., Fang, G., Chen, Y., Liu, J., & Ying, Z. (2018). Latent Class Analysis of Recurrent Events in Problem-Solving Items. Applied Psychological Measurement. https://doi.org/10.1177/0146621617748325")

             ),
             tabPanel("Module 1",
                      tags$h1(tags$strong("Module 1"), ": Time"),

                      div(class="test_type",
                          tags$p("\"You may delay, but time will not\" (not an inspired postdoc thought, just borrowed from Benjamin Franklin)")),

                      tags$p("Time is probably the variable presented in all log-file data in educational assessments. Here we will
                            conduct the analysis of the amount of time students spent on the Climate control item by item
                            performance (CP025Q01=0: incorrect answer; CP025Q01=1: correct answer) and gender  (ST04Q01=1: female; ST04Q01=2: male)."),

                      tags$hr(), #horizontal line
                      radioButtons(inputId = "type1", label = "Analysis of time on task by:",
                                   choices = c("Item Performance" = "item",
                                               "Gender" = "gender"),
                                   selected = "item"),

                        selectInput(inputId = "country1", label = "Select the level of the analysis:",
                                    choices = as.character(unique(cp025q01.complete$IDSELECT)), #include dataset in the LOGAN package!
                                    selected = "Country: ALL") ,
                      verbatimTextOutput("summaryTime"),
                      tags$h4("Distribution of Time on task for the selected country:"),
                      plotOutput("TotbyVar")
             ),
             tabPanel("Module 2",
                      tags$h1(tags$strong("Module 2"), ": Actions - Cognitive related"),

                      div(class="test_type",
                          tags$p("\"Do actions agree with words? There's your measure of reliability. Never confine yourself to the words\"
                                 (clever thoughts from Frank Herbert)")),

                      tags$p("To illustrate how to explore the actions recorded in the log files, we will use the approach describe in
                            Greiff, Wustenberg, and Avvisati (2015). To analyze the Climate control item, they apply the
                             vary-one-thing-at-at-time (VOTAT) strategy
                             which consist of, for each variable (i.e., top control), students have to vary this variable (i.e., put
                             the top control on \"++\"), keep all other variables constant (i.e., put the central and bottom controls
                             on the delta symbol), and click on \"apply\". The operationalization of the VOTAT strategy is given by:"),

                      tags$div(
                        tags$ul(
                          tags$li("VOTAT 1: credit was given to students who applied VOTAT for all input variables.")
                          ),
                        tags$ul(
                          tags$li("VOTAT 2: incorporated four categories for no isolated variation at all (category 0), isolated variation
                                  of one input variable (for example, only the top control), isolated variation of two input variables
                                  (for example, the top and bottom controls), and isolated variation of all three input variables (category 3).")
                        )
                      ),

                      tags$p("In the analysis below, you can investigate how performance outcomes and VOTAT strategies are related by country,
                             item level performance (CP025Q01=0: incorrect answer; CP025Q01=1: correct answer) and problem-solving overall performance
                             (first plausible value, PV1CPRO)"),

                      tags$hr(), #horizontal line
                     # sidebarPanel( #grey box
                      selectInput(inputId = "strateg", label = "Choose a strategy",
                                    choices = c("VOTAT1", "VOTAT2"),
                                    selected = "VOTAT1"),
                      selectInput(inputId = "country2", label = "Analysis by:",
                                  choices = as.character(unique(cp025q01.complete$IDSELECT)), #include dataset in the LOGAN package!
                                  selected = "Country: ALL"),
                     # ),
                      verbatimTextOutput("DescriptStrat"),
                      tags$h4("Relationship between VOTAT and overall performance in PISA 2012 problem solving (percentage within the
                              categorized proficiency score):"),
                      plotOutput("PVbyStrat"),

                     tags$hr(), #horizontal line
                     tags$h2("References:"),
                     tags$p("Greiff, S., Wustenberg, S., & Avvisati, F. (2015). Computer-generated log-file analyses as a window
                            into students' minds? A showcase study based on the PISA 2012 assessment of problem solving. Computers
                            and Education, 91, 92-105. https://doi.org/10.1016/j.compedu.2015.10.018")
             )
  )
server <- function(input, output) { #inside the server function will be run once per end user - connection

  country0 <- reactive(input$country0)
  country1 <- reactive(input$country1)
  country2 <- reactive(input$country2)
  strateg2 <- reactive(input$strateg)

  rv <- reactiveValues(typeAnalysis = "CP025Q01")
  observeEvent(input$type1, {
    if(input$type1 == "item"){rv$typeAnalysis = "CP025Q01"
    } else if (input$type1 == "gender"){rv$typeAnalysis = "ST04Q01"}
  })
  #observeEvent(input$gender, {rv$typeAnalysis = "ST04Q01"})

  #Module 0:
  output$table <- renderDataTable(cp025q01.data[,1:13], options = list(lengthMenu = c(5, 10, 15, 20), pageLength = 5))
  output$nActionsbyVar <- renderPrint({
    LOGAN::RangeNumberActionsbyVar(data = cp025q01.data[cp025q01.data$cnt == country0(), ],
                            id.var = NewID, var.group = cnt)
  })

  #Module 1:
  output$summaryTime <- renderPrint({
      LOGAN::SummaryTOTbyVar(data = cp025q01.complete[cp025q01.complete$IDSELECT == country1(), ], tot.var = "CP025Q01.TOT", performance.item = rv$typeAnalysis)
    #summary(cp025q01.complete[cp025q01.complete$IDSELECT == country1(), "CP025Q01.TOT"], na.rm = TRUE)
  })

  output$TotbyVar <- renderPlot({
    PlotTimeonTaskbyVar(data = cp025q01.complete[cp025q01.complete$IDSELECT == country1(), ], tot.var = "CP025Q01.TOT", performance.item = rv$typeAnalysis,
                        namexlab = "Time on task (in minutes)", nameylab = "Density")
  })

  #Module 2:
  output$DescriptStrat <- renderPrint({
  LOGAN::DescriptiveStrategy(data=cp025q01.complete[cp025q01.complete$IDSELECT == country2(), ], strategy.var = strateg2(),
                             performance.item="CP025Q01", performance.test= "PV1CPRO")
  })

  output$PVbyStrat <- renderPlot({
    cp025q01.complete$categ <- ifelse(cp025q01.complete$PV1CPRO <= 423, "Level 1 or below",
                                     ifelse(cp025q01.complete$PV1CPRO <= 488, "Level 2",
                                            ifelse(cp025q01.complete$PV1CPRO <= 553, "Level 3", "Level 4 or above")))
    dataplot <- cp025q01.complete[cp025q01.complete$IDSELECT == country2(), c(strateg2(), "categ")]

    dataplot[,1] <- as.factor(dataplot[,1])
    dataplot[,2] <- as.factor(dataplot[,2])
    names(dataplot)[1] <- "VOTAT"

        PlotStrategybyCatPerformance(data = dataplot, strategy.var = VOTAT,
                                 categ.var = categ, namexlab = "Problem solving - Proficiency levels",
                                 nameylab = "")
  })
}

test_that("Shiny runs", expect_s3_class(shinyApp(ui, server = server), "shiny.appobj"))