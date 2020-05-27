library(shiny)


rsconnect::setAccountInfo(name='xxxx',
                          token='xxx',
                          secret='xxx')

##### SERVER #####

# Define server logic for random distribution application
server <- function(input, output) {
  
  #Generate Acoustic deterrent devices result?
  #LFC - Low Frequency Cetaceans
  #MFC - Mid Frequency Cetaceans
  #HFC - High Frequency Cetaceans
  #PP - Phocid Pinnipeds
  #OP - Otariid Pinnipeds
  #SELT - SEL Threshold (provided)
  #PKT - PK Threshold (provided)
  #PTSIT - PTS Isopleth to threshold (meters)
  #PTSPKTIT - PTS PK Isopleth to threshold (meters)
  #SL - Source Level (Single shot SEL) (input)
  #SLPK - Source Level (PL SPL) (input)
  #ADB - Adjustment db
  #NP - 10 log (number of pulses)
  #Px - Propagation (xLogR) (input)
  #P1 - Parameter 1
  #P2 - Parameter 2
  #P3 - Parameter 3
  #P4 - Parameter 4
  #a - Weighting Function Parameter 1
  #b - Weighting Function Parameter 2
  #f1 - Weighting Function Parameter 3
  #f2 - Weighting Function Parameter 4
  #C - Weighting Function Parameter 5
  #WFA - Weighting Factor Adjustment (input)
  #NP24h - Number of pulses in 24-h period
  #NP1h - Number of pulses in 1-h period
  #AD - Activity duration (hours) within 24 h period
  
  #a = 1
  #b = 2
  #f1 = 0.2
  #f2 = 19
  #C = 0.13
  #SELT = 183
  #PKT = 219
  
  Hearing_Group_Names = c('Low-Frequency Cetaceans', 'Mid-Frequency Cetaceans',
                          'High-Frequency Cetaceans', 'Phocid Pinnipeds', 'Otariid Pinnipeds')
  A = c(1, 1.6, 1.8, 1, 2)
  B = c(2, 2, 2, 2, 2)
  F1 = c(0.2, 8.8, 12, 1.9, 0.94)
  F2 = c(19, 110, 140, 30, 25)
  CC = c(0.13, 1.2, 1.36, 0.75, 0.64)
  SELTC = c(183, 185, 155, 185, 203)
  PKTC = c(219, 230, 202, 218, 232)
  
  
  PTSIT = function(SL, AD, NP1h, Px, WFA,v){
    P1 = (WFA/F1[v])^(2*A[v])
    P2 = (1 + (WFA/F1[v])^2)^A[v]
    P3 = (1 + (WFA/F2[v])^2)^B[v]
    P4 = P1/(P2*P3)
    ADB = log10(P4)*10 + CC[v]
    NP24h = NP1h*AD
    NP = log10(NP24h)*10
    PTS_output = 10^(((SL + ADB) + NP - SELTC[v] )/ Px)
    res = PTS_output
  }
  
  PTSPKTIT = function(SLPK, Px, v){
    if(SLPK > PKTC[v]){
      round(10^((SLPK - PKTC[v])/Px),2)
    } else {
      "NA"
    }
  }
  
  
  #SEL Threshold
  #output$PTSIT = renderText({
    #Get inputs
   # SL = input$sourcelevel
    #AD = input$activityduration
    #NP1h = input$numberofpulses1h
    #Px = input$propagationxLogR
    #WFA = input$weightingfactoradjustment
    #res = round(PTSIT(SL, AD, NP1h, Px, WFA),4)
  #})
  result = c()
  output$PTSIT = renderTable({
    #Get inputs
    SL = input$sourcelevel
    AD = input$activityduration
    NP1h = input$numberofpulses1h
    Px = input$propagationxLogR
    WFA = input$weightingfactoradjustment
    
    for (v in c(1:5)){
      res = round(PTSIT(SL, AD, NP1h, Px, WFA, v),4)
      result[v] = res
    }
    df = data.frame(result)
    rownames(df) = Hearing_Group_Names
    return(df)
  }, rownames =  TRUE, colnames = FALSE)
  
  
  result2 = c()
  
  output$PTSPTIT = renderTable({
    #Get inputs
    SLPK = input$sourcelevelPK
    Px = input$propagationxLogR
    for (v in c(1:5)){
      res = PTSPKTIT(SLPK, Px, v)
      
      result2[v] = res
    }
    df2 = data.frame(result2)
    rownames(df2) = Hearing_Group_Names
    return(df2)
  }, rownames = TRUE, colnames = FALSE)
  
  counting = 0
  output$Resulting = renderText({
    SLPK = input$sourcelevelPK
    Px = input$propagationxLogR
    SL = input$sourcelevel
    AD = input$activityduration
    NP1h = input$numberofpulses1h
    Px = input$propagationxLogR
    WFA = input$weightingfactoradjustment
    
    for (v in c(1:5)){
      res = PTSPKTIT(SLPK, Px, v)
      res2 = PTSIT(SL, AD, NP1h, Px, WFA, v)
      if (res > 5){
        counting = counting + 1
      }
      if (res2 > 5){
        counting = counting + 1
      }
      
    }
    if (counting == 10){
      resulting = "Pass, device meets the requirement and you can download the certificate"
    } else {
      resulting = "Fail, device doesn't meet the criteria and cannot be used"
    }
    return(resulting)
  })
}

##### UI #####

ui <- shinyUI(fluidPage(
  
  titlePanel("Demo - Impulsive Stat"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput('sourcelevel','Source Level (Single Shot SEL)',150),
      numericInput('sourcelevelPK','Source Level (PK SPL)',225),
      sliderInput('activityduration','Activity Duration (hours) within 24-h period',min=0.1,max=24,value=2,step=0.1),
      sliderInput('numberofpulses1h','Number of pulses in 1-h period',min=100,max=1000,value=240,step=10),
      sliderInput('propagationxLogR','Propagation (xLogR)',min=1,max=100,value=15,step=1),
      sliderInput('weightingfactoradjustment','Weighting Factor Adjustment (kHz)',min=0.1,max=10,value=2,step=0.1),
      hr()
      
    ),
    
    mainPanel(
      h4(p('PTS Isopleth to threshold (meters)')),
      tableOutput("PTSIT"),
      #hr(),
      h4(p('PTS PK Isopleth to threshold (meters)')),
      tableOutput("PTSPTIT"),
      hr(),
      h4(p('Result')),
      textOutput("Resulting"),
      hr(),
      p('Assumptions'),
      h6(p('1. The demo is for Impulsive Stationary sources assuming pulse duration and repetition rate is not available. Hence, Single Shot SEL is used to determine isopleth.')),
      h6(p('2. Methodology assumes that the criterion to evaluate device eligibility for permit is based on  minimum safe distance.  We have assumed  5m as minimum safe distance.')),
      h6(p('3. A devise is said to be passed, if both PK isopleth and PTS isopleth are more than 5m, for all five types of mammals.')),
      h6(p('4. The calculations are based on our understanding of excel tool. We acknowledge that actual results can be different. We will study methodology in detail once the project begins.')),
      )
    )
  )  
)

##### Run #####
shinyApp(ui = ui, server = server)
