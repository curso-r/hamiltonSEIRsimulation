#' top_ui UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_top_ui_ui <- function(id){
  ns <- NS(id)
  tagList(
    bs4Dash::bs4DashPage(
      sidebar_collapsed = TRUE,
      sidebar_mini = FALSE,
      body = bs4Dash::bs4DashBody(
        hamiltonThemes::use_bs4Dash_distill_theme(),
        br(),
        fluidRow(
          bs4Dash::column(
            width = 4,
            sliderInput(
              ns("R0"),
              "The average number of people infected before an exposed person recovers at the start of the epidemic",
              0,
              10,
              3.6,
              step=0.1
            )
          ),
          bs4Dash::column(
            width = 4,
            sliderInput(
              ns("E"),
              "Average time that a newly infected person spends as an asymptomatic spreader before becoming symptomatic",
              0,
              20,
              6.6,
              step=0.2,
              post = " days"
            )
          ),
          bs4Dash::column(
            width = 4,
            sliderInput(
              ns("I"),
              HTML("Average time that a symptomatic person spends before recovering<br><br>"),
              0,
              20,
              7.4,
              step=0.2,
              post = " days"
            )
          )
        )
        ,
        fluidRow(
          bs4Dash::column(
            width = 3,
            numericInput(
              ns("pop"),
              "Initial number of susceptible people, S compartment",
              value = 4.9E6
            )
          ),
          bs4Dash::column(
            width = 3,
            numericInput(
              inputId = ns("exp"),
              label = "Initial number of asymptomatic spreaders, E compartment",
              value = 0
            )
          ),
          bs4Dash::column(
            width = 3,
            numericInput(
              inputId = ns("inf"),
              label = "Initial number of symptomatic spreaders, I compartment",
              value = 1
            )
          ),
          bs4Dash::column(
            width = 3,
            numericInput(
              inputId = ns("rec"),
              label = "Initial number of recovered (i.e. immune) people, R compartment",
              value = 0
            )
          )
        ),
        hr(),
        fluidRow(
          column(
            width =  12,
            bs4Dash::bs4TabCard(
              title = "",
              id = "tabcard",
              closable = FALSE,
              collapsible = FALSE,
              width = 12,
              bs4Dash::bs4TabPanel(
                tabName = "Spread",
                plotOutput(ns("plot")) %>% hamiltonThemes::distill_load_spinner(),
                radioButtons(ns("yscale"), "Y axis scale:",
                             choices = list("Linear" = "linear","Log10" = "log"), inline=TRUE)
              ),
              bs4Dash::bs4TabPanel(
                tabName = "Days to epidemic's end",
                plotOutput(ns("plot2")) %>% hamiltonThemes::distill_load_spinner(),
                uiOutput(ns("Report"))
              ),
              bs4Dash::bs4TabPanel(
                tabName = "About",
                div(style = "color: black;", get_about_text())
              )
            )
          )
        )
      ),
      footer = hamiltonThemes:::bs4dash_distill_footer()
    )
  )
  
}
    
#' top_ui Server Function
#'
#' @noRd 
mod_top_ui_server <- function(input, output, session){
  ns <- session$ns
 
  real <- 200 # number of simulation
  
  realisation <- reactive({
    
    ##### General setup
    compt <-  c('Susceptible', 
                'Exposed', 
                'Infectious', 
                'Recovered') # SEIR compartment
    
    
    # equal mean holding times for E and I compartments (mean(E+I) = 14days)
    mean_holding_times <-  c(input$E, input$I)  # mean holding times at compartment E and I
    total_holding_times <-  sum(mean_holding_times)
    beta <-  input$R0 / c(total_holding_times, total_holding_times)
    
    ##### Assign initial conditions
    N <-  matrix(0, nrow = 1, ncol = length(compt)) # a matrix to store number of people in each compartment
    
    N[1,1] <- input$pop  # number of susceptible people (population of ROI)
    N[1,2] <- input$exp   # number of exposed people
    N[1,3] <- input$inf   # number of infected people
    N[1,4] <- input$rec
    
    # initialise simulation condition
    t <- 0
    dt <- 1 # time increment in days
    t_phase <- Inf
    N_phases <-  1 # number of phases (e.g. intervention at t=t*)
    
    ##### Run simulation
    realisation <- list(Time = list(), S = list(), E = list(), I = list(), R = list()) # a list of lists to store results
    for (r in 1:real) {
      for(i in 1:N_phases){
        if (i == 1) { # for first phase of the epimedic
          res <- seir_model(t_phase, t, dt, N, mean_holding_times, beta)
        }
        else {
          # TODO: pass results from the first phase with different infection parameters
        }
        
      }
      # rename the column names to appropriate compartments and convert it to a data frame
      res <-  as.data.frame(res) %>% 
        dplyr::rename_at(dplyr::vars(paste0(c(rep("V", length(compt))), 2:5)), ~compt)

      # save the results as a list of vectors
      realisation$Time[[r]] <- res$Time
      realisation$S[[r]] <- res$Susceptible
      realisation$E[[r]] <- res$Exposed
      realisation$I[[r]] <- res$Infectious
      realisation$R[[r]] <- res$Recovered
    }
    
    realisation
    
  })
  

  output$plot <- renderPlot({
    # convert to matrix (NB: because each realisation has different length, fill the gap with NAs)
    extractS = sapply(realisation()$S, `length<-`, max(lengths(realisation()$S)))
    extractE = sapply(realisation()$E, `length<-`, max(lengths(realisation()$E)))
    extractI = sapply(realisation()$I, `length<-`, max(lengths(realisation()$I)))
    extractR = sapply(realisation()$R, `length<-`, max(lengths(realisation()$R)))
    
    # calculate mean across rows
    Time = rowMeans(sapply(realisation()$Time, `length<-`, max(lengths(realisation()$S))), na.rm = TRUE)
    meanS = rowMeans(extractS, na.rm = TRUE)
    meanE = rowMeans(extractE, na.rm = TRUE)
    meanI = rowMeans(extractI, na.rm = TRUE)
    meanR = rowMeans(extractR, na.rm = TRUE)
    
    # calculate percentile confidence intervals
    low = 0.025 # lower percentile
    upp = 0.975 # upper percentile
    quantilesS.lower = apply(extractS, 1, stats::quantile, low, na.rm = TRUE)
    quantilesS.upper = apply(extractS, 1, stats::quantile, upp, na.rm = TRUE)
    quantilesE.lower = apply(extractE, 1, stats::quantile, low, na.rm = TRUE)
    quantilesE.upper = apply(extractE, 1, stats::quantile, upp, na.rm = TRUE)
    quantilesI.lower = apply(extractI, 1, stats::quantile, low, na.rm = TRUE)
    quantilesI.upper = apply(extractI, 1, stats::quantile, upp, na.rm = TRUE)
    quantilesR.lower = apply(extractR, 1, stats::quantile, low, na.rm = TRUE)
    quantilesR.upper = apply(extractR, 1, stats::quantile, upp, na.rm = TRUE)
    
    # convert the mean values into a data frame
    final = data.frame(
      Time = Time, 
      Susceptible = meanS, 
      Exposed = meanE, 
      Infectious = meanI, 
      Recovered = meanR
    )
    
    
    plt1 <- ggplot2::ggplot(final, ggplot2::aes(Time)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = quantilesS.lower, ymax = quantilesS.upper), alpha = 0.3, fill = "black") +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = quantilesE.lower, ymax = quantilesE.upper), alpha = 0.3, fill = "purple") +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = quantilesI.lower, ymax = quantilesI.upper), alpha = 0.3, fill = "red") +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = quantilesR.lower, ymax = quantilesR.upper), alpha = 0.3, fill = "blue") +
      
      ggplot2::geom_line(ggplot2::aes(y = Susceptible, color = "A_black")) +  # S
      ggplot2::geom_line(ggplot2::aes(y = Exposed, color = "B_purple")) +     # E
      ggplot2::geom_line(ggplot2::aes(y = Infectious, color = "C_red")) +     # I
      ggplot2::geom_line(ggplot2::aes(y = Recovered, color = "D_blue")) +     # R
      
      ggplot2::labs(x = "time [day]", y = "Number of people", colour = "") +
      ggplot2::scale_colour_manual(
        name = "Comp.",
        values = c(A_black = "black", B_purple = "purple", C_red = "red", D_blue = "blue"),
        labels = c("S", "E", "I", "R")
      ) +
      ggplot2::scale_x_continuous(expand = c(0, 0)) +
      ggplot2::scale_y_continuous(expand = c(0, 0), labels = scientific_10) + 
      ggplot2::theme(legend.position = c(0.9, 0.5), plot.title = ggplot2::element_text(size = 10, face = "bold"))
    
    plt2 = plt1 + 
      ggplot2::scale_y_log10(limits = c(1E-2, NA), breaks = scales::trans_breaks("log10", function(x) 10^x), labels = scales::trans_format("log10", scales::math_format())) +
      ggplot2::labs(x = "time [day]", y = "Number of people (logscale)", colour = "") +
      ggplot2::theme(legend.position = "none")
    
    if(input$yscale == "linear"){
      
      final_plot <- plt1
      
    } else {
      
      final_plot <- plt2
      
    }
    
    final_plot
    
  })
  
  d <- reactive({
    
    x <- c()
    
    for(i in 1:real){
      
      x[i] <- max(unlist(realisation()$Time[i]))
      
    }
    
    as.data.frame(x)
    
  })
  
  
  
  
  
  output$plot2 <- renderPlot({
    
    ggplot2::ggplot(d(), ggplot2::aes(x)) +
      ggplot2::geom_histogram(ggplot2::aes(x, fill = ..count.., colour = x), show.legend = FALSE) +
      ggplot2::theme_minimal() +
      ggplot2::labs(x = "Days", y = "Number of future scenarios") +
      ggplot2::ggtitle("Possible future scenarios") 
  })
  
  output$Report <- renderUI({

    fit = survival::survfit(survival::Surv(d()$x)~1)
    df <- data.frame(V1 = fit$time, 'V2' = fit$surv)
    x <- df %>% dplyr::filter(V2 > 0.49 & V2 < 0.6)
    y <- df %>% dplyr::filter(V2 < 0.059 & V2 > 0.01)
    x <- max(x$V1)
    y <- min(y$V1)

    
    div(
      style = "color: black;",
      paste0("There is a 50% chance that COVID-19 will be gone by day ", x, " and 95% chance that it will be gone by day ",y,".")
    )
    
  })
}
    
## To be copied in the UI
# mod_top_ui_ui("top_ui_ui_1")
    
## To be copied in the server
# callModule(mod_top_ui_server, "top_ui_ui_1")
 
