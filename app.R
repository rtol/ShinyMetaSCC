# Shiny Social Cost of Carbon

library(shiny)
library(plotrix)

# Define UI for user input
ui <- fluidPage(
  titlePanel(
  HTML("<h2 style='text-decoration: underline;'>A Meta-Analysis of the Social Cost of Carbon</h2>"),
  ),
  sidebarLayout(
    sidebarPanel(
      helpText("What do you want to show?"),
      radioButtons("prtp", "Pure Rate of Time Preference",
                   choices = list("0" = 1, "1" = 2,"2" = 3, "3" = 4, "all" = 5), selected = 5),
      helpText("What is the utility discount rate?"),
      radioButtons("weight", "Weight",
              choices = list("Quality" = 1, "Author" = 2,"Paper" = 3, "None" = 4), selected = 1),
      helpText("How should the observations be weighted?"),
      sliderInput("leviathan_sldr", "Leviathan Tax", min = 856, max = 1426, value = 1141),
      helpText("Observations above the Leviathan Tax are discounted."),
      sliderInput("atpay_sldr", "Ability to pay", min = 5707, max = 9511, value = 7609),
      helpText("Observations above the maximum ability to pay are ignored."),
      sliderInput("sYear_sldr", "Start Year", min = 1982, max = 2021, value = 1982, sep = ""),
      sliderInput("eYear_sldr", "End Year", min = 1982, max = 2021, value = 2021, sep = ""),
      helpText("Restrict the sample."),
      radioButtons("tax", "Tax or not",
                   choices = list("with carbon tax" = 1, "without carbon tax" = 2,"both" = 3), selected = 3),
      helpText("Is the social cost of carbon imposed as a carbon tax?"),
      radioButtons("author", "Authors",
                   choices = list("Chris Hope" = 1, "Bill Nordhaus" = 2,"Richard Tol" = 3, "Rick van der Ploeg" = 4, "all" = 5), selected = 5),
      helpText("Restrict the sample to particular authors?"),
      tags$a("Click here for more information", href="https://doi.org/10.1038/s41558-023-01680-x")
    ),
    mainPanel(
      h4(textOutput("SCCw")),
      plotOutput("weighted_hist"),
      plotOutput("xy_plot"),
      h5(textOutput("key")),
      h5(textOutput("key1")),
      h5(textOutput("key2")),
      h5(textOutput("key3")),
      #h4(textOutput("SCC")),
      #plotOutput("histogram")
    )
  )
)

# Define server to generate output
server <- function(input, output) {
  
  resultsout <- reactive({
    
    impdata <-  read.csv(file="socialcostcarbon.csv", header=TRUE, sep=",")
    #all the data/observations used
    year <- impdata[1:5905,1]
    scc0 <- impdata[1:5905,6]
    censor <- impdata[1:5905,2]
    # adapting the censor data be used as sliders
    a <- 1 - (input$leviathan_sldr/(input$leviathan_sldr-input$atpay_sldr))
    b <- 1/(input$leviathan_sldr-input$atpay_sldr)
    censor <- a + b*scc0
    censor[censor > 1] <- 1
    censor[censor < 0] <- 0
    quality <- impdata[1:5905,3]
    author <- impdata[1:5905,4]
    paper <- impdata[1:5905,5]
    cdr <- impdata[1:5905,7]
    prtp <- impdata[1:5905,8]
    prtp[prtp == "NA"] <- -100
    equity <- impdata[1:5905,9]
    uncertainty <- impdata[1:5905,10]
    hope <- impdata[1:5905,11]
    nordhaus <- impdata[1:5905,12]
    tol <- impdata[1:5905,13]
    ploeg <- impdata[1:5905,14]
    tax <- impdata[1:5905,15]
    
    #Tax or not buttons
    if (input$tax == "1") {
      scc <- tax*scc0
    }
    else if (input$tax == "2") {
      scc <- (1-tax)*scc0
    }
    else if (input$tax == "3") {
      scc <- scc0
    }
    else {
      scc <- scc0
    }
    
    #Sliders to determine the years shown
    scc <- scc*(year >= input$sYear_sldr & year <= input$eYear_sldr)
    
    #PRTP buttons
    if (input$prtp == "1") {
      aux <- prtp == 0
      aux[is.na(aux)] <- FALSE #removes 'NA' from results
      scc <- aux*scc
    }
    else if (input$prtp == "2") {
      aux <- prtp ==1
      aux[is.na(aux)] <- FALSE #removes 'NA' from results
      scc <- aux*scc
    }
    else if (input$prtp == "3") {
      aux <- prtp == 2
      aux[is.na(aux)] <- FALSE #removes 'NA' from results
      scc <- aux*scc
    }
    else if (input$prtp == "4") {
      aux <- prtp == 3
      aux[is.na(aux)] <- FALSE #removes 'NA' from results
      scc <- aux*scc
    }
    else if (input$prtp == "5") {
      scc <- scc
    }
    else {
      scc <- scc
    }
   
    #Author buttons
    if (input$author == "1") {
      scc <- hope*scc
    }
    else if (input$author == "2") {
      scc <- nordhaus*scc
    }
    else if (input$author == "3") {
      scc <- tol*scc
    }
    else if (input$author == "4") {
      scc <- ploeg*scc
    }
    else if (input$author == "5") {
      scc <- scc
    }
    else {
      scc <- scc
    }
    
    #Weight buttons
    if (input$weight == "1") {
      weight <- censor*quality
    }
    else if (input$weight == "2") {
      weight <- censor*author
    }
    else if (input$weight == "3") {
      weight <- censor*paper
    }
    else {
      weight <- censor
    }
    
    resultsout <- cbind(scc,year,censor,weight)
  })
  
  #plotting the unweighted histogram
  output$histogram <- renderPlot({
    par(bg = "beige") #background colour
    req(resultsout())
    aux <- resultsout()[1:5905,1] #social cost of carbon
    aux3 <- resultsout()[1:5905,3] #censor
    aux <- aux[aux != 0 & aux3 != 0] #filtering out the data that equals 0
    custom_error <- "Error: Not enough data to create histogram." #replaces the error message with a notification
    tryCatch(
      expr = {
        hist(aux, col = "orange", main = "Social cost of carbon", xlab = "Dollar per tonne of carbon")
      },
      error = function(e) {
        showNotification(custom_error, type = "error")
      }
    )
  })
  
  #plotting the weighted histogram
  output$weighted_hist <- renderPlot({
    par(bg = "beige") #background colour
    req(resultsout())
    aux <- resultsout()[1:5905,1] #social cost of carbon
    aux3 <- resultsout()[1:5905,3] #censor
    aux5 <- aux[aux != 0 & aux3 != 0] #filters out zeros
    
    weight <- resultsout()[1:5905,4]
    weight <- weight[aux != 0 & aux3 != 0] #filters out zeros
    
    minscc <- min(aux5)
    maxscc <- max(aux5)
    n2 <- sum(weight)
    wmean <- sum(aux5 * weight) / n2 #finds the weighted mean
    wvar <- sum(weight * (aux5 - wmean)^2) / n2 #calculates the weighted variance
    beta <- sqrt(6*wvar)/pi
    
    mu <- wmean - beta*exp(1) 
    
    u <- unique(aux5)
    tab <- tabulate(match(aux5, u))
    mu <- u[tab == max(tab)]
    
    grid <- seq(minscc,maxscc,abs(minscc/8))
    z <- (grid + 4*abs(minscc/8) - mu)/beta
    gumbel <- exp(-(z+exp(-z)))/beta
    gumbel <- gumbel / sum(gumbel) #* abs(minscc/2)
    
    custom_error <- "Error: Not enough data to create histogram." #replaces error message with a notification
    tryCatch(
      expr = {
    weighted.hist(aux5, weight, breaks = grid, col = "orange", main = "Social cost of carbon", xlab = "Dollar per Tonne of Carbon", freq = FALSE)
    #axis(1, at = grid, las=2)
    lines(grid, gumbel, col = 2, lwd = 2)
      },
    error = function(e) {
      showNotification(custom_error,type = "error")
    }
    )
  })
  
  #plotting the xy plot
  output$xy_plot <- renderPlot({
    par(bg = "lightblue") #background colour
    req(resultsout())
    aux3 <- resultsout()[1:5905,3] #censor
    aux <- resultsout()[1:5905,1] #social cost of carbon
    aux1 <- aux[aux != 0 & aux3 != 0] #filters out zeros
    aux2 <- resultsout()[1:5905,2] #years
    uyears <- unique(aux2) #represents the individual years
    aux2 <- aux2[aux != 0 & aux3 != 0] #filters out zeros
    n <- length(uyears)
    meanscc <- 0 #defining to be used later
    upper <- 0
    lower <- 0
    for (i in 1:n) {
      meanscc[i] = mean(aux[aux2 == uyears[i] & aux != 0 & aux3 != 0 ]) #calculates the mean and filters out unwanted data
      aux8 <- aux[aux2 == uyears[i] & aux != 0 & aux3 != 0 ] #filters out unwanted data
      n_xy <- length(aux8) #calculates the length
      sd_xy <- sd(aux8) #calculates the standard deviation
      sem_xy <- sd_xy / sqrt(n_xy) #calculates the standard error of the mean
      upper[i] <- meanscc[i] + (2*sem_xy) #turns the standard error of the mean into points that can be plotted
      lower[i] <- meanscc[i] - (2*sem_xy)
     }
    
    custom_error <- "Error: Not enough data to create graph." #replaces error message with notification
    tryCatch(
      expr = {
    plot(aux2,aux1, type = "p", col = "blue", pch = 16, cex = 0.75, main = "Social cost of carbon", xlab = "Years", ylab = "Dollar per Tonne of Carbon")
        lines(uyears,upper, type = "p", col = "orange", pch = 18, cex = 1) #plots the upper limit of the sem 
        lines(uyears,lower, type = "p", col = "orange", pch = 18, cex = 1) #plots the lower limit of the sem
        lines(uyears, meanscc, type = "p", col = "brown", pch = 15, cex = 1.25) #plots the average
    },
    error = function(e) {
    showNotification(custom_error, type = "error")
  }
  )
  })
  
  #text1 the average for the unweighted scc 
  output$SCC <- renderText({
    aux4 <- resultsout()[1:5905,1] #social cost of carbon
    aux3 <- resultsout()[1:5905,3] #censor
    aux4 <- aux4[aux4 != 0 & aux3 != 0] #filters out zeros
    average <- mean(aux4) #finds the average
    
    n1 <- length(aux4) 
    sd <- sd(aux4) #calculates the standard deviation
    sem <- sd / sqrt(n1) #calculates the standard error of the mean
    
    SCC <- paste("Social cost of carbon - unweighted: $",format(average,digits=2),"(s.e. ",format(sem, digits=2),")/tC")
  })
  
  #text2 the average for the weighted scc
  output$SCCw <- renderText({
    aux5 <- resultsout()[1:5905,1] #social cost of carbon
    aux6 <- resultsout()[1:5905,4] #weight
    aux6 <- aux6[aux5 != 0] #filters out zeros
    aux5 <- aux5[aux5 != 0]
    weighted_mean <- sum(aux5 * aux6) / sum(aux6) #finds the weighted mean
    
    n2 <- sum(aux6)
    variance <- sum(aux6 * (aux5 - weighted_mean)^2) / sum(aux6) #calculates the weighted variance
    wsem <- sqrt(variance / n2) #calculates the standard weighted error of the mean
    
    SCCw <- paste("Weighted average: $",format(weighted_mean,digits=2),"(s.e. ",format(wsem, digits=2),")/tC")
    })
  
  #text3 the key for the xy plot
  output$key1 <- renderText({
    key1 <- "Key- Blue circles = all data points"
  }) 
  output$key2 <- renderText({
    key2 <- "Brown squares = averages for each year"
  }) 
  output$key3 <- renderText({
    key3 <- "Orange diamonds = upper and lower bounds of the 95% confidence interval around the mean"
  }) 
}

# Run the application 
shinyApp(ui = ui, server = server)