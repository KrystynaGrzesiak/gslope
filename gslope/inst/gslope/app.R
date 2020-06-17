
library(shiny)
library(shinyWidgets)
library(shinythemes)

ui <- fluidPage(theme = shinytheme("superhero"),

        tags$img(src = "logo.png", width = 140, style="float:right"),
        tags$h1("gslope"),
        tags$h4("Graphical SLOPE"),



        tabsetPanel(type = "tabs",

          tabPanel("Start",
                   sidebarPanel(
                     span("bibliography", style="font-size:20px;
                                              color:pink;
                                              text-align:center"),
                     hr(),
                     uiOutput("link"),
                     h5("Michał Makowski,"),
                     h4('Precision matrix estimation in Gaussian graphical models'),
                     hr(),
                     span("Enjoy!", style="font-size:20px;
                                              color:pink;
                                              text-align:center")
                   ),

                   mainPanel(
                     p(span('Bonjour!', br(), 'Dowiecie się dzisiaj kilku wspaniałych
                        rzeczy o modelach grafowych, w szczególności o',
                      strong('grafowym SLOPE,'),
                      'a także poznacie świeżutki pakiet',
                      strong('gslope'),
                      'i zobaczycie, jak go używać.'), style="font-size:20px; text-align:center"),
                      p('Z tą wiedzą świat stoi
                      przed wami otworem!', style="font-size:25px; text-align:center")
                   )
          ),

          tabPanel("gModels - intuition",
                   h4("A is conditionally independent of B given S.",
                      style="text-align:center"),
                   HTML('<center><img src="graph_image2.png" width="400"></center>'),
                   helpText("source: Statistical Learning with Sparsity",
                            style = "color:pink; text-align:center"),
                   hr(),
                   h4("Gaussian Graphical Model can be selected by the
                      estimation of a precision matrix.",
                      style="text-align:center"),
                   HTML('<center><img src="graph_image.png" width="500"></center>'),
                   helpText("source: Statistical Learning with Sparsity",
                            style = "color:pink; text-align:center")
          ),

          tabPanel("SLOPE",
                   h4("Lambda",
                      style="text-align:center"),
                   h5("Benjamini-Hochberg's method",
                      style="text-align:center"),
                   HTML('<center><img src="lambda.png" width="400"></center>'),
                   hr(),
                   h4("SLOPE",
                      style="text-align:center"),
                   HTML('<center><img src="slope.png" width="400"></center>'),
                   h5("where S denotes a sample covariance matrix and",
                      style="text-align:center"),
                   HTML('<center><img src="lambda_term.png" width="200"></center>'),
                   helpText('source: "Precision matrix estimation in Gaussian graphical models"',
                            style = "color:pink; text-align:center")
          ),

          tabPanel("Example usage",
            sidebarPanel(
              numericInput("num_n", label = h3("Enter n"), value = 3),
              numericInput("num_p", label = h3("Enter p"), value = 2),
              numericInput("num_thresh", label = h3("Enter threshold"), value = 0.01)
            ),

            mainPanel(
              hr(),
              fluidRow(
                column(2, helpText("n:")),
                column(3, verbatimTextOutput("value_n"))
              ),
              fluidRow(
                column(2, helpText("Cov_p:")),
                column(12, verbatimTextOutput("value_p"))
              ),

              fluidRow(
                column(3, helpText("Theoretical precision matrix:")),
                column(12, verbatimTextOutput("prec"))
              ),

              fluidRow(
                column(2, helpText("Sample covariance:")),
                column(12, verbatimTextOutput("sample_cov"))
              ),


              fluidRow(
                column(3, helpText("Estimated precision matrix:")),
                column(12, verbatimTextOutput("prec_est"))
              )
            )
          ),
          tabPanel("gslope output",
                   sidebarPanel(
                     selectInput("lst", label = h3("Select output"),
                                 choices = list("estimated precision matrix" = 1,
                                                "estimated covariance matrix" = 2,
                                                "scaled precision matrix" = 3,
                                                "lambda" = 4,
                                                "iterations" = 5,
                                                "graph" = 6,
                                                "clusters" = 7),
                                 selected = 1)
                   ),

                   mainPanel(
                     hr(),
                     fluidRow(column(12, verbatimTextOutput("gslope_lst")))
                   )
          ),

          tabPanel("Matrix plot",
            sidebarPanel(
              selectInput("data", label = h3("Select data"),
                          choices = list("generated" = 1, "mtcars" = 2, "frets" = 3),
                          selected = 1)
            ),

            mainPanel(
              hr(),
              fluidRow(column(12, plotOutput("plot_prec")))
            )
          ),

          tabPanel("Graphs",
            sidebarPanel(
              selectInput("data_graph", label = h3("Select data"),
                          choices = list("generated" = 1, "mtcars" = 2, "frets" = 3),
                          selected = 1)
            ),

            mainPanel(
              hr(),
              fluidRow(column(12, plotOutput("plot_graph")))
            )
          )
        )
      )

server <- function(input, output) {

  library(gslope)
  library(glasso)
  library(mvtnorm)
  library(boot)


  create_Gamma = function(p){
    sapply(1:p, function(i){
      sapply(1:p, function(j){
        min(i,j)
      })
    })
  }


  generate_X = function(n, Gamma){
    rmvnorm(n, rep(0, nrow(Gamma)), Gamma)
  }


  url = a("Statistical learning with Sparsity",
          href = "https://web.stanford.edu/~hastie/StatLearnSparsity/")


  output$link <- renderUI({
    tagList("The clever book:", url)
  })


  n <- reactive({input$num_n})
  p <- reactive({input$num_p})
  thr <- reactive({input$num_thresh})
  Gamma <- reactive({create_Gamma(p())})

  X <- reactive({
    generate_X(n(), Gamma())
  })

  X_scaled <- reactive({
    scale(X())
  })

  gslope_X <- reactive({gslope(X(), scaled = TRUE, threshold = thr())})

  output$value_n <- renderPrint({ n() })
  output$value_p <- renderPrint({ Gamma() })


  output$prec <- renderPrint({
    solve(Gamma())
  })


  output$sample_cov <- renderPrint({
    round(cov(X()),4)
  })


  output$prec_est <- renderPrint({
    round(gslope_X()[[3]], 4)
  })

  # output$prec_est <- renderPrint({
  #   # round(gslope_X()[[1]], 4)
  #   g = glasso(cov(X()), 1)[[2]]
  #   g = -cov2cor(g)
  #   g[abs(g)<thr()] = 0
  #   g
  # })


  output$gslope_lst <- renderPrint({
    gslope(X(), scaled = TRUE, threshold = thr())[[as.integer(input$lst)]]
  })


  output$plot_prec <- renderPlot({
    if(input$data == 1){
      g_X = gslope(X(), scaled=TRUE, threshold = thr())
      plot(g_X, plt = "scaled_precision")
    }
    if(input$data == 2)
      plot(gslope(scale(mtcars), scaled=TRUE), plt = "scaled_precision")
    if(input$data_graph == 3)
      plot(gslope(frets), plt = "scaled_precision")
  })


  output$plot_graph <- renderPlot({
    if(input$data_graph == 1)
      graph_plot(gslope(X(), scaled=TRUE, threshold = thr()))
    if(input$data_graph == 2)
      graph_plot(gslope(scale(mtcars), scaled=TRUE))
    if(input$data_graph == 3)
      graph_plot(gslope(frets))
  })

}

# Run the application
shinyApp(ui = ui, server = server)
