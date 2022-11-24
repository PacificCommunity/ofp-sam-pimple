#--------------------------------------------------------------
# PIMPLE
# Performance Indicators and Management Procedures Explorer
# Main app
# Updated for SC17 (July 2021)
# Updated for SMD 2022 (Started March 2022)
# Updated for MOC 2022 (October 2022)

# Copyright 2020 OFP SPC MSE Team. Distributed under the GPL 3
# Maintainer: Finlay Scott, OFP SPC
# Soundtrack: People and Industry by Warrington-Runcorn New Town Development Plan
#             Time Turns Into Space by Deliquescent Crystals

#--------------------------------------------------------------

library(shiny)
library(ggplot2)
library(RColorBrewer)
library(markdown)
library(data.table)
library(tidyr)
library(DT)

source("funcs.R")
source("plots.R")

# Load the indicator data for the reference sets - including Kobe and Majuro data
ref_files <- load("data/WCPFC_2022_reference_results.Rdata")
rob_files <- load("data/WCPFC_2022_robustness_results.Rdata")

# Which HCRs do we want to show?
# Include the Brian variants
#hcrrefs <- sort(unique(periodqs$hcrref))
#hcrnames <- sort(unique(periodqs$hcrname))
# Not the Brian variants
hcrrefs <- unique(periodqs$hcrref)[1:5]
hcrnames <- unique(periodqs$hcrname)[1:5]

# Only keep HCRs we want
periodqs <- periodqs[hcrref %in% hcrrefs]
worms <- worms[hcrref %in% hcrrefs]
yearqs <- yearqs[hcrref %in% hcrrefs]
hcr_shape <- hcr_shape[hcrref %in% hcrrefs]
hcr_points <- hcr_points[hcrref %in% hcrrefs]
scaler <- scaler[hcrref %in% hcrrefs]
scaler_diff <- scaler_diff[hcrref %in% hcrrefs]

#--------------------------------------------------------------------------------------------------

# Check catch stability - why are minimums same as median for medium and long term
# Because negative - because the minimum stability is based on 5% but whisker is 2.5%
# periodqs[piname == "PI 6: Catch stability" & area == "total" & metric == "relative catch stability" & period == "Medium",]

# Additional data processing

# General plotting parameters
short_term <- worms[period == "Short", sort(unique(year))]
medium_term <- worms[period == "Medium", sort(unique(year))]
long_term <- worms[period == "Long", sort(unique(year))]

last_plot_year <- max(long_term)
first_plot_year <- 1990

# Quantiles - extract calculated percentiles from data object
qnames <- colnames(periodqs)[!(colnames(periodqs) %in% c("area", "msectrl", "metric", "period", "pi", "piname", "hcrname", "hcrref", "area_name", "X50.", "set"))]
qnames <- substring(qnames,2) # Drop the X
qnames <- substring(qnames,1,nchar(qnames)-1)
quantiles <- as.numeric(qnames)
# and the last character (a dot)
inner_percentiles <- quantiles[c(2,length(quantiles)-1)]
outer_percentiles <- quantiles[c(1,length(quantiles))]
quantiles <- quantiles / 100

# Trim out years for tight time series plots
yearqs <- yearqs[year %in% first_plot_year:last_plot_year]
worms <- worms[year %in% first_plot_year:last_plot_year]

# For the worms - same worms for all plots
nworms <-  min(7,length(unique(worms$iter)))
set.seed(95)
wormiters <- sample(unique(worms$iter), nworms)

# Careful with these - they are only used for plotting lines, NOT for calculating the indicators
lrp <- 0.2
trp1 <- 0.5
trp2 <- unlist(yearqs[piname == "SB/SBF=0" & msectrl == "Z2" & area == "all" & year == 2012, "X50."]) # 0.4545289
#trp3 <- 0.41 # Les' two-eyed TRP
trp3 <- 0.39 # Equilibrium baseline conditions

# Find common iters between HCRs - used for HCR plots so we can directly compare
# Some iters dropped due to errors
common_iters <- hcr_points[, unique(iter)]

# Should already be done in the PI calculation script
majuro_summary_tabs <- lapply(majuro_summary_tabs, function(x) x[order(x$HCR),])
kobe_summary_tabs <- lapply(kobe_summary_tabs, function(x) x[order(x$HCR),])

# Get names for the main PI selector for Compare Performance tab
# i.e. not used in the more detailed analysis
#unique(periodqs$piname)
pis_list <- c("PI 1: Prob. above LRP",
              "PI 3: Catch\n(relative to 2013-2015)",
              "PI 4: P&L CPUE\n(relative to 2001-2004)",
              "PI 4: PS CPUE\n(relative to 2012)",
              "PI 6: Catch stability",
              "PI 7: Effort stability\n(PS in areas 6,7,8 only)",
              "PI 8: Proximity to SB/SBF=0 (target)",
              "Effort\n(relative to reference period)",
              "SB/SBF=0",
              "SB/SBF=0 relative to target")
piselector <- as.list(pis_list)
# Cut off second line if exists for display
pis_text <- unlist(lapply(strsplit(pis_list,"\n"),'[',1))
names(piselector) <- pis_text

## Make a factor of pinmaes with relative effort last
#allpinames <- sort(unique(periodqs$piname))
#where <- which(allpinames=="Effort relative to reference period")
#allpinames[where:(length(allpinames)-1)] <- allpinames[(where+1):length(allpinames)]
#allpinames[length(allpinames)] <- "Effort relative to reference period"
## Reorder piname in periodqs
##periodqs$piname <- factor(periodqs$piname, levels=allpinames)

# -------------------------------------------
# General settings for app

# Whole thing is 12 units wide
main_panel_width <- 10
side_panel_width <- 12 - main_panel_width 

# Get ranges of short, medium and long to make labels with
short <- range(short_term)
medium <- range(medium_term)
long <- range(long_term)

# Text notes for the app
shorttext <- paste(short, collapse="-")
mediumtext <- paste(medium, collapse="-")
longtext <- paste(long, collapse="-")
yearrangetext <- paste("Short-term is: ", shorttext, ", medium-term is: ", mediumtext, " and long-term is: ", longtext,".",sep="")
pi47text <- "Note that PIs 7 and 'effort relative to reference period' are for fisheries managed through effort limits only, e.g. pole and line fisheries and purse seine fisheries."
pi36text <- "The grouping for PIs 3 and 6 can be selected with the drop down menu on the left."
biotext <- "PIs 1, 8 and SB/SBF=0 are calculated over all model areas."
relcatchtext <- "Note that the catches are relative to the average catch in that area grouping in the years 2013-2015."
barchartplottext <- "The height of each bar shows the median expected value. Note that the bar charts do not show any uncertainty which can be important (see the box plots)."
boxplottext <- paste0("For box plots the box contains the ", inner_percentiles[2] - inner_percentiles[1],"th percentile, the whiskers show the ", outer_percentiles[2] - outer_percentiles[1], "th percentile and the horizontal line is the median. The wider the range, the less certain we are about the expected value.")
tabletext <- paste0("The tables show the median indicator values in each time period. The values inside the parentheses are the ", outer_percentiles[2] - outer_percentiles[1] ,"th percentile range.")
timeseriesplottext <- paste0("The outer ribbons show the ", outer_percentiles[2] - outer_percentiles[1], "th percentile range and the inner ribbons show the ", inner_percentiles[2] - inner_percentiles[1], "th percentile range. The dashed, black line is the median value.")
timeseriesplottext2 <-  "The dashed vertical lines show, from left, the start of the MSE evaluation with transient period assumptions, the start of the HCR operating with the short-, medium- and long-term periods."
stabtext <- "Note that the stability can only be compared between time periods, not between areas or area groups, i.e. it is the relative stability in that area."
sbsbf02012text <- "On the SB/SBF=0 plot, the lower dashed line is the Limit Reference Point and the upper dashed line is the target SB/SBF=0 (where the target is determined as equilibrium stock status under baseline fishing levels)."

#--------------------------------------------------------------------------------------------------

# Fake place holder app
#ui <- fluidPage(
#    sidebarPanel(width=side_panel_width,
#      br(),
#      img(src = "spc.png", height = 60),
#      br(),
#      br(),
#      tags$html(
#        tags$h1("PIMPLE"),
#        tags$p("Performance Indicators and Management Procedures expLorEr"),
#        tags$footer(
#          tags$p("version 1.0.0 Tarantula Deadly Cargo"),
#          tags$p("Copyright 2021 OFP SPC MSE Team."),
#          tags$p("Distributed under the GPL 3")
#        )
#      )
#    ),
#    mainPanel(width=main_panel_width,
#      tags$style(type="text/css", "body {padding-top: 70px;}"), 
#      h1("The PIMPLE address has been updated."),
#      br(),
#      p("For the results to be discussed at WCPFC 19 go to:"),
#      a(href="https://ofp-sam.shinyapps.io/PIMPLE_WCPFC19/", "https://ofp-sam.shinyapps.io/PIMPLE_WCPFC19/"),
#      br(),
#      br(),
#      p("For the results used at the Science Management Dialogue Meeting 2022 go to:"),
#      a(href="https://ofp-sam.shinyapps.io/PIMPLE2022/", "https://ofp-sam.shinyapps.io/PIMPLE2022/") 
#    )
#)





# The actual app!

# Navbarpage inside a fluidpage?
# Pretty nasty but it means we get the power of the navparPage and can have common side panel
ui <- fluidPage(id="top",
  tags$head(includeHTML("google-analytics.html")),  # google analytics
  #titlePanel("Performance Indicators and Management Procedures Explorer"),
  sidebarLayout(
    sidebarPanel(width=side_panel_width,
      br(),
      img(src = "spc.png", height = 60),
      br(),
      br(),
      #-----------------------------------------------------------------------
      # A shit-tonne of conditional panels to make the sidebar do what we want
      #-----------------------------------------------------------------------
      conditionalPanel(condition="input.nvp == 'about'",
        tags$html(
          tags$h1("PIMPLE"),
          tags$p("Performance Indicators and Management Procedures expLorEr"),
          tags$footer(
            tags$p("version 1.0.1 Tarantula Deadly Cargo"),
            tags$p("Copyright 2021 OFP SPC MSE Team."),
            tags$p("Distributed under the GPL 3")
          )
      )), # End of about condition
      # HCR selection - can select multiples
      # Only for the main Compare MPs tab, the MPs tab and SOME of the explorePIs tabs
      conditionalPanel(condition="input.nvp == 'compareMPs' || (input.nvp == 'explorePIs' && (input.pitab == 'pi3' || input.pitab == 'pi6' || input.pitab == 'vulnb' || input.pitab == 'relcpuepl')) || input.nvp == 'robustness' || input.nvp == 'mps' || input.nvp == 'mixpis'",
        checkboxGroupInput(inputId = "hcrchoice", label="SKJ HCR selection", selected = unique(periodqs$hcrref), choiceNames = as.character(unique(periodqs$hcrname)), choiceValues = unique(periodqs$hcrref))
        #checkboxGroupInput(inputId = "hcrchoice", label="SKJ HCR selection", selected = hcrrefs, choiceNames = hcrnames, choiceValues = hcrrefs)
      ),
      # PI choice - only shown in the compare PIs tab
      conditionalPanel(condition="input.nvp == 'compareMPs' || input.nvp == 'robustness'",
        checkboxGroupInput(inputId = "pichoice", label="PI selection",choices = piselector, selected=sort(pis_list))
      ),
      # Catch grouping choice (all, PS in 678, PL in 1234) - show in compare MPs 
      #conditionalPanel(condition="(input.nvp == 'compareMPs' || (input.nvp == 'explorePIs' && (input.pitab == 'pi3' || input.pitab == 'pi6')))",
      conditionalPanel(condition="input.nvp == 'compareMPs' || input.nvp == 'robustness'",
        selectInput(inputId = "catchareachoice", label="Catch grouping (PIs 3 & 6 only)", choices = list("All areas"="total", "Purse seines in areas 6,7 & 8"="ps678", "Pole & line in areas 1,2,3 & 4" = "pl_jp"), selected="total")
      ),
      # Selecting catch by area 
      conditionalPanel(condition="input.nvp == 'explorePIs' && (input.pitab== 'pi3' || input.pitab== 'pi6')",
        checkboxGroupInput(inputId = "areachoice", label="Area selection",choices = list("All areas" = "total", "Purse seines in areas 6,7 & 8" = "ps678", "Pole & line in areas 1,2,3 & 4" = "pl_jp", "Area 1" = "1", "Area 2" = "2", "Area 3"="3", "Area 4"="4","Area 5"="5","Area 6"="6","Area 7"="7","Area 8"="8" ), selected="total")
      ),
      
      # Select plot type by bar, box or time
      conditionalPanel(condition="(input.nvp == 'explorePIs' && (input.pitab == 'pi3' || input.pitab == 'vulnb' || input.pitab == 'relcpuepl')) || (input.nvp == 'mixpis' && (input.mixpisid == 'mixss' || input.mixpisid == 'mixcatch')) ",
        radioButtons(inputId = "plotchoicebarboxtime", label="Plot selection",choices = list("Bar chart" = "median_bar", "Box plot" ="box", "Time series" = "time"), selected="box")
      ),
      # Show spaghetti on the time series plots - only show when you get time series plots
      conditionalPanel(condition="(input.nvp == 'explorePIs' && (input.pitab=='pi3' || input.pitab=='vulnb' || input.pitab=='relcpuepl')) || (input.nvp == 'compareMPs' && input.comptab == 'timeseries')",
        checkboxInput("showspag", "Show trajectories", value=FALSE) 
      ),
      # In Management Procedures tab, show the points and trajectories
      # Change false to true to show performance options (can hide from users)
      #conditionalPanel(condition="((input.nvp == 'mps') && true)",
      conditionalPanel(condition="((input.nvp == 'mps') && false)",
        p("HCR performance options"),
        radioButtons(inputId="hcrperformance", label="HCR performance options", choices=list("Show nothing" = "shownothing", "Show selection of points" = "showpoints", "Show iter paths" = "showpaths"), selected="shownothing"), 
        conditionalPanel(condition="input.hcrperformance == 'showpaths'",
          numericInput(inputId='hcrperfiter', label="Iter to show path for", value=1, min=1, max=length(common_iters), step=1)
        )
      ),
      # Kobe plot HCR selection - one at a time only
      #conditionalPanel(condition="input.nvp == 'majurokobeplot'",
      conditionalPanel(condition="input.nvp == 'explorePIs' & input.pitab == 'majurokobeplot'",
        radioButtons(inputId = "hcrchoicekobe", label="HCR selection", selected = unique(periodqs$hcrref)[1], choiceNames = as.character(unique(periodqs$hcrname)), choiceValues = unique(periodqs$hcrref)),
        radioButtons(inputId = "majurokobe", label="Majuro or Kobe plot", selected = "Majuro", choiceNames = c("Majuro", "Kobe"), choiceValues = c("Majuro", "Kobe"))
      ),
      
      # Select plot type by bar or box (Note - need to include the NVP input as the the pitab input still has value even if not seen)
      #conditionalPanel(condition="(input.nvp == 'explorePIs') && (input.pitab== 'pi6') || (input.nvp == 'mixpis' && (input.mixpisid == 'mixss' || input.mixpisid == 'mixcatch'))",
      conditionalPanel(condition="(input.nvp == 'explorePIs') && (input.pitab== 'pi6')",
        radioButtons(inputId = "plotchoicebarbox", label="Plot selection",choices = list("Bar chart" = "median_bar", "Box plot" ="box"), selected="box")
      ),
      
      # Stability or variability
      conditionalPanel(condition="input.nvp == 'explorePIs' && input.pitab== 'pi6'",
        radioButtons(inputId = "stabvarchoice", label="Stability or variability",choices = list("Stability" = "stability", "Variability" ="variability"), selected="stability")
      ),
      
      # In Robustness tab choose the robustness set
      conditionalPanel(condition="input.nvp == 'robustness'",
        selectInput(inputId = "robustset", label = "Robustness set", 
          choices = list("Hyperstability" = "robust_hyperstability", "Low recruitment" = "robust_low_rec_10yrs", "Effort creep" = "robust_effort_creep"),
          selected = "robust_hyperstability") 
      )
      
      
    ), # End of sidebarPanel
    
    #---------------------------------------------
    # Main panel
    #---------------------------------------------
    mainPanel(width=main_panel_width,
      tags$style(type="text/css", "body {padding-top: 70px;}"), # padding - as we use fixed-top for position, applies to all tabs
      navbarPage(id="nvp",
        collapsible=TRUE,  # Should help if using small screens like tablets
        windowTitle="PIMPLE",
        position="fixed-top",
        title="Performance Indicators and Management Procedures Explorer",
        
        #--------------------------------------------
        # Introduction page 
        #--------------------------------------------
        
        tabPanel("Introduction", value="intro",
          # How to use PIMPLE - Add to top
          fluidRow(column(8, 
            includeMarkdown("introtext/introduction.md")
          )),
          fluidRow(column(8, 
            includeMarkdown("introtext/assumptions.md")
          )),
          fluidRow(column(8, 
            includeMarkdown("introtext/background.md")
          )),
          fluidRow(
            column(4, 
              includeMarkdown("introtext/barcharttext.md")
            ),
            column(8,
              plotOutput("demobarchart")
            )
          ),
          fluidRow(
            column(4, 
              includeMarkdown("introtext/boxplottext.md")
            ),
            column(8,
              plotOutput("demoboxplot")
            )
          ),
          fluidRow(
            column(4, 
              includeMarkdown("introtext/timeseriestext.md")
            ),
            column(8,
              plotOutput("demotimeseriesplot")
            )
          )
        ), # End of Intro tab

        #----------------------------------------------------------------------------
        # Comparing performance across all indicators
        #----------------------------------------------------------------------------
        tabPanel("Compare performance", value="compareMPs",
          tabsetPanel(id="comptab",
                      
            tabPanel("Box plots", value="box",
              fluidRow(column(12,
                p(boxplottext),
                plotOutput("plot_box_comparehcr", height="auto") 
              )),
              fluidRow(column(12,
                p(yearrangetext),
                p(pi47text),
                p(biotext),
                p(pi36text),
                p(sbsbf02012text)
              ))
            ),
            
            tabPanel("Bar charts", value="bar",
              fluidRow(column(12,
                p(barchartplottext),
                plotOutput("plot_bar_comparehcr", height="auto") 
              )),
              fluidRow(column(12,
                p(yearrangetext),
                p(pi47text),
                p(biotext),
                p(pi36text),
                p(sbsbf02012text)
              ))
            ),
            
            tabPanel("Time series plots", value="timeseries",
              fluidRow(column(12,
                p("Note that not all indicators have time series plots."),
                p(timeseriesplottext),
                p(timeseriesplottext2),
                plotOutput("plot_timeseries_comparehcr", height="auto") # height is variable
              )),
              fluidRow(column(12,
                p(pi47text),
                p(biotext),
                p(pi36text),
                p(sbsbf02012text)
              ))
            ),
            tabPanel("Table", value="bigtable",
              tags$span(title=paste0("Median indicator values. The values inside the parentheses are the ", outer_percentiles[2] - outer_percentiles[1], "th percentile range."),
                p(tabletext),
                tableOutput("table_pis_short"),
                tableOutput("table_pis_medium"),
                tableOutput("table_pis_long"),
                p(yearrangetext),
                p(pi47text),
                p(biotext),
                p(pi36text)
              )
            )
          )
        ), # End of Compare PIs tab
        #----------------------------------------------------------------------------
        # Tabs for exploring specific indicators in more detail
        #----------------------------------------------------------------------------
        tabPanel("Other SKJ indicators", value="explorePIs",
          tabsetPanel(id="pitab",

            # *** PI 3: Catch based ones ***
            tabPanel("PI 3: Relative catches by area",value="pi3",
              column(12, fluidRow(
                plotOutput("plot_pi3", height="auto"), # Nice  - height is auto - seems to given by the height in renderOutput()
                p(relcatchtext),
                p(yearrangetext)
              ))
            ),
            # *** PI 6: Catch stability ***
            tabPanel("PI 6: Catch stability by area",value="pi6",
              column(12, fluidRow(
                p("It is possible to see the variability instead of the stability using the checkbox on the left."),
                plotOutput("plot_pi6", height="auto"), # Nice  - height is auto - seems to given by the height in renderOutput()
                p(relcatchtext),
                p(yearrangetext),
                p(stabtext)
              ))
            ), # End of PI6 Catch stability tab
            # *** Vulnerable biomass
            # Separate panels for areas 1 - 4
            tabPanel("Vulnerable biomass under pole and line fisheries", value="vulnb",
              column(12, 
                p("Vulnerable biomass is the biomass that is exposed to a particular fishery through a combination of the biomass at size and the fishery selectivity. These plots show the vulnerable biomass to the pole and line fisheries in model areas 1 - 4."),
                radioButtons(inputId = "vbscale", label="Same scale?",choiceNames=c("Yes", "No"), choiceValues=c("fixed", "free"), selected="fixed", inline=TRUE),
                fluidRow(
                  plotOutput("plot_vulnb",  height="800px"),
                  p(yearrangetext)
                )
              )
            ), # End of vulnerable biomass
            ## *** Pole and line CPUE
            ## Separate panels for areas 1 - 4
            #tabPanel("Relative CPUE of pole and line fisheries", value="relcpuepl",
            #  column(12, 
            #    p("The CPUE of the pole and line fisheries in model areas 1 - 4 relative to the period 2001-2004."),
            #    fluidRow(
            #      plotOutput("plot_relcpuepl",  height="800px"),
            #      p(yearrangetext)
            #    )
            #  )
            #), # End of pole and line CPUE
           
            
            # *** Kobe and Majuro
            tabPanel(title="Majuro and Kobe plots", value="majurokobeplot",
              fluidRow(column(12, 
                p("Majuro or Kobe plots for a single HCR (chosen from the input menu on the left) in each time period." ),
                p(paste(yearrangetext, "The historical period covers 2000-2018."),sep=" "),
                p("The contour colours show the approximate probability of being in that area. Each coloured band represents a 25% chance, e.g. there is a 25% chance of being in the blue zone and a 25% chance of being in the yellow zone etc. A random sample of points are shown as an illustration."),
                p("The percentage of points falling in each plot quadrant is also shown in the small white box. This represents the chance of being in that quadrant in that time period. A table of the percentages for all HCRs is next to the plot.")
              )),
              fluidRow(column(6, plotOutput("plot_kobe_ptables_short")),
                       column(6, tableOutput("table_kobesummary_short"))),
              fluidRow(column(6, plotOutput("plot_kobe_ptables_medium")),
                       column(6, tableOutput("table_kobesummary_medium"))),
              fluidRow(column(6, plotOutput("plot_kobe_ptables_long")),
                       column(6, tableOutput("table_kobesummary_long"))),
              fluidRow(column(6, plotOutput("plot_kobe_ptables_hist")),
                       column(6, tableOutput("table_kobesummary_hist")))
            ) # End of Majuro Kobe tab
          )                                
        ), # End of Other SKJ indicators tab                                
        
        #------------------------------------------------------
        # Robustness
        #------------------------------------------------------
        tabPanel("Robustness set", value="robustness",
          #fluidRow(
          #  h3("Coming soon...")
          #)
          
          fluidRow(
            column(12,
            p("The robustness set includes more extreme, but still plausible, uncertainties than the reference set of operating models."),
            p("There are currently three robustness set scenarios: increased hyperstability; lower than average future recruitment; increased effort creep in the purse seine fisheries."),
            p("The results from the robustness evaluations should not be compared to the reference set evaluations as the operating models are different."),
            p("Instead, the relative performance of the candidate HCRs should be compared across the robustness set scenarios."),
            p("Note that for the 'low recruitment' robustness scenario, the low recruitment affects both the SB and the SBF0 so that SB/SBF0 may not be as affected as expected.")
          )),
          fluidRow(column(12,
            p(boxplottext),
            plotOutput("plot_box_robust", height="auto") 
          )),
          fluidRow(column(12,
            p(yearrangetext),
            p(pi47text),
            p(biotext),
            p(pi36text),
            p(sbsbf02012text)
          ))
          
          
        ), # End of Robustness tab
        
        #------------------------------------------
        # The Management Procedures
        #------------------------------------------
        tabPanel(title="SKJ management procedures", value="mps",
          #column(12,
          #  fluidRow(            
          #    p("Currently all the candidate skipjack management procedures have the same estimation method (an 8-region MULTIFAN-CL stock assessment model)."),
          #    p("This means that we are only comparing the performance of the HCRs. However, this may not always be the case."),
          #    p("The current HCRs use a value of estimated depletion (SB/SBF=0) to set a multiplier. This multipler is applied to the catch or effort in 2012 for each fishery to set a new catch or effort limit for the next time period."),
          #    plotOutput("plot_hcrshape",  height="600px"),
          #  )
          #),
          # Can wrap each element in a fluidRow if necessary
          fluidRow(
              p("Currently all the candidate skipjack management procedures have the same estimation method (an 8-region MULTIFAN-CL stock assessment model)."),
              p("This means that we are only comparing the performance of the HCRs. However, this may not always be the case."),
              p("The current HCRs use a value of estimated depletion (SB/SBF=0) to set a multiplier. This multipiler is applied to the catch or effort the reference period (see Introduction tab) for each fishery to set a new catch or effort limit for the next time period."),
            column(6, h3("HCR shapes"),
                      plotOutput("plot_hcrshape",  height="600px")),
            column(6, h3("HCR parameters"),
                      DTOutput("hcr_table"))
          ),
          
          
          fluidRow(
            h3("HCR output"),
            p(paste0("The distribution of HCR output in each management period. The box and whiskers show the ", inner_percentiles[2] - inner_percentiles[1],  "th and ",outer_percentiles[2] - outer_percentiles[1], "th percentiles respectively.")),
            p("Note that the minimum y-limit is not fixed at 0."),
            plotOutput("plot_hcr_op",  height="600px")
          ),
          fluidRow(
            h3("Change in HCR output"),
            p(paste0("The distribution of absolute change in HCR output in each management period. The box and whiskers show the ", inner_percentiles[2] - inner_percentiles[1],  "th and ",outer_percentiles[2] - outer_percentiles[1], "th percentiles respectively.")),
            p("This plot therefore shows the variability in the HCR output. The large the value, the more the output changes between management periods."),
            plotOutput("plot_hcr_op_diff",  height="600px")
          )#,
          #fluidRow(
          #  DTOutput("hcr_table")
          #)
        ), # End of MPs tab
        
        #------------------------------------------------------
        # About
        #------------------------------------------------------
        tabPanel("About", value="about",
          fluidRow(column(8, 
            spc_about()
          )),
          fluidRow(column(8, 
            includeMarkdown("introtext/news.md")
          ))
        ) # End of About
      ) # End of navbarPage()
    ) # End of mainPanel()
  ) # End of sidebarLayout()
) # End of fluidPage()


#-------------------------------------------------
# Server function
#-------------------------------------------------

server <- function(input, output, session) {
  
  # Plot settings
  height_per_pi <- 300
  height_per_area <- 300
  no_facets_row <- 2
  no_mixfacets_row <- 3 # For mixed fishery indicators

  #-------------------------------------------------------------------
  # Make the checkbox inputs match each other
  observeEvent(input$hcrchoice_pitab, {
    newsel <- input$hcrchoice_pitab
    updateCheckboxGroupInput(session, "hcrchoice", 
      choiceNames = as.character(unique(periodqs$hcrname)), choiceValues = unique(periodqs$hcrref),
      selected = newsel)
  })

  observeEvent(input$hcrchoice, {
    newsel <- input$hcrchoice
    updateCheckboxGroupInput(session, "hcrchoice_pitab", 
      choiceNames = as.character(unique(periodqs$hcrname)), choiceValues = unique(periodqs$hcrref),
      selected = newsel)
  })
  #-------------------------------------------------------------------
  # Intro plots
  demo_hcr_choices <- c("HCR 1 (+-10%)", "HCR 6 (+-10%)")
  
  output$demobarchart <- renderPlot({
    # Demo bar plot
    pi_choices <- c("pi3")
    metric_choices <- c("relative catch")
    area_choices <- "total"
    dat <- dplyr::filter(periodqs, period != "Rest" & pi %in% pi_choices & metric %in% metric_choices & area %in% area_choices)
    dat$hcrname <- as.character(dat$hcrname)
    dat$hcrref <- as.character(dat$hcrref)
    p <- barboxplot(dat=dat, hcr_choices=demo_hcr_choices, plot_type="median_bar", quantiles=quantiles)
    #p <- p + ggplot2::ylim(0,NA)
    p <- p + ggplot2::ylab("Value") + ggplot2::xlab("Time period")
    return(p)
  })

  output$demoboxplot <- renderPlot({
    # Demo bar plot
    pi_choices <- c("pi3")
    metric_choices <- c("relative catch")
    area_choices <- "total"
    dat <- dplyr::filter(periodqs, period != "Rest" & pi %in% pi_choices & metric %in% metric_choices & area %in% area_choices)
    dat$hcrname <- as.character(dat$hcrname)
    dat$hcrref <- as.character(dat$hcrref)
    p <- barboxplot(dat=dat, hcr_choices=demo_hcr_choices, plot_type="box", quantiles=quantiles)
    #p <- p + ggplot2::ylim(0,NA)
    p <- p + ggplot2::ylab("Value") + ggplot2::xlab("Time period")
    return(p)
  })

  output$demotimeseriesplot <- renderPlot({
    # Demo time series plot
    pi_choices <- c("pi3")
    metric_choices <- c("relative catch")
    area_choices <- "total"
    dat <- dplyr::filter(yearqs, pi %in% pi_choices & metric %in% metric_choices & area %in% area_choices)
    wormdat <- dplyr::filter(worms, pi %in% pi_choices & metric %in% metric_choices & area %in% area_choices)
    p <- time_series_plot(dat=dat, hcr_choices=demo_hcr_choices, wormdat=wormdat, last_plot_year=last_plot_year, short_term = short_term, medium_term = medium_term, long_term = long_term, show_spaghetti=FALSE, outer_percentile_range = outer_percentiles, inner_percentile_range = inner_percentiles)
    #p <- p + ggplot2::ylim(c(0,NA))
    # Axes limits set here or have tight?
    p <- p + ggplot2::scale_x_continuous(expand = c(0, 0))
    p <- p + ggplot2::ylab("PI 3: Catch (rel. to 2013-2015)")
    p <- p + facet_grid(piname ~ hcrref, scales="free")#, ncol=1)
    # Size of labels etc
    p <- p + theme(axis.text=element_text(size=16), axis.title=element_text(size=16), strip.text=element_text(size=16), legend.text=element_text(size=16))
    return(p)
  })

  #-------------------------------------------------------------------
  # Main comparison plots

  # Bar or box plot - facetting on PI
  plot_barbox_comparehcr <- function(plot_type="median_bar"){
    rPlot <- renderPlot({
      
      hcr_choices <- input$hcrchoice
      pi_choices <- input$pichoice
      if((length(hcr_choices) < 1) | (length(pi_choices) < 1)){
        return()
      }
      # There are a maximum of length(pis_list) panels (currently 10)
      # pi1, pi3, pi4 (x2), pi6, pi7, pi8, relative effort, sbsbf0, sbsbf0 relative to TRP
      # (see pis_list above)
      # They have different groupings based on metric and area
      # pi1: metric = SBSBF0, area = 1-8, all
      # pi3: area = 1-8, total, ps678, pl_jp
      # pi4: area = ps678x, 1 - 4, pl_jp
      # pi6: area = as pi3 and metric catch stability / relative catch stability (not in piname so need additional subset)
      # pi7: area = ps678x
      # pi8: area = all
      # sbsbsf0: area = 1-8, all
      catch_area_choice <- input$catchareachoice
      
      # Subset out the data you want to plot based on user options
      # Need to be careful as PI 4 has six areas - PL 1-4 (for indiv P&L) pl_jp and ps678x
      # Gets tricky with areas for PI 3 and 4
      # For 3 we want one of pl_jp, ps678 or total - given by catchareachoice
      # For 4 we want both of pl_jp and ps678x - pl_jp is the pain
      dat <- periodqs[period != "Rest" &
             metric != "catch stability" &
             piname %in% pi_choices &
             area %in% c("all", catch_area_choice, "pl_jp", "ps678x") &
             # Drop areas not in catch_area_choice from PI 3 and 6 only  (to deal with pl_jp)
             !(piname == "PI 3: Catch\n(relative to 2013-2015)" & !(area %in% catch_area_choice)) &
             !(piname == "PI 6: Catch stability" & !(area %in% catch_area_choice))]
      
      p <- barboxplot(dat=dat, hcr_choices=hcr_choices, plot_type=plot_type, quantiles=quantiles)
      # Do we fix axis at 0? Probably
      p <- p + ggplot2::ylim(0,NA)
      # Coord cartesian to zoom
      #p <- p + coord_cartesian(ylim=c(0.5,NA)) # Fixes barcharts - all of them at 0.5
      # How to get different ones
      p <- p + ggplot2::ylab("Value") + ggplot2::xlab("Time period")
      # Add LRP and TRP lines
      # Only if SB/SBF=0 is in dat
      if ("SB/SBF=0" %in% pi_choices){
        p <- p + ggplot2::geom_hline(data=data.frame(yint=lrp, piname="SB/SBF=0"), ggplot2::aes(yintercept=yint), linetype=2)
        p <- p + ggplot2::geom_hline(data=data.frame(yint=trp3, piname="SB/SBF=0"), ggplot2::aes(yintercept=yint), linetype=2)
      }
      # Add 1.0 line for relative to TRP plot
      if ("SB/SBF=0 relative to target" %in% pi_choices){
        p <- p + ggplot2::geom_hline(data=data.frame(yint=1.0, piname="SB/SBF=0 relative to target"), ggplot2::aes(yintercept=yint), linetype=2)
      }
      # Must have a probability of at least 0.8 above LRP
      if ("PI 1: Prob. above LRP" %in% pi_choices){
        p <- p + ggplot2::geom_hline(data=data.frame(yint=0.8,piname="PI 1: Prob. above LRP"), ggplot2::aes(yintercept=yint), linetype=2)
      }
      # Add 1.0 line for relative effort
      if ("Effort\n(relative to reference period)" %in% pi_choices){
        p <- p + ggplot2::geom_hline(data=data.frame(yint=1.0, piname="Effort\n(relative to reference period)"), ggplot2::aes(yintercept=yint), linetype=2)
      }
      # Add 1.0 line for relative CPUE PL
      if ("PI 4: P&L CPUE\n(relative to 2001-2004)" %in% pi_choices){
        p <- p + ggplot2::geom_hline(data=data.frame(yint=1.0, piname="PI 4: P&L CPUE\n(relative to 2001-2004)"), ggplot2::aes(yintercept=yint), linetype=2)
      }
      # Add 1.0 line for relative CPUE PL
      if ("PI 4: PS CPUE\n(relative to 2012)" %in% pi_choices){
        p <- p + ggplot2::geom_hline(data=data.frame(yint=1.0, piname="PI 4: PS CPUE\n(relative to 2012)"), ggplot2::aes(yintercept=yint), linetype=2)
      }
      #p <- p + ggplot2::facet_wrap(~piname, scales="free", ncol=no_facets_row)
      # Why do I have to make the wrap a factor when it already is?
      p <- p + ggplot2::facet_wrap(~factor(piname, levels=pis_list), scales="free", ncol=no_facets_row)
      return(p)
    },
      height=function(){
        return(max(height_per_pi*1.5, (height_per_pi * ceiling(length(input$pichoice) / no_facets_row))))
      }
    )
    return(rPlot)
  }

  output$plot_bar_comparehcr <- plot_barbox_comparehcr(plot_type="median_bar")
  output$plot_box_comparehcr <- plot_barbox_comparehcr(plot_type="box")

  # Time series comparisons - just three plots
  #pinames_ts <- c("SB/SBF=0", "PI 3: Catch (rel. to 2013-2015)" ,"PI 4: Relative PS CPUE")
  pinames_ts <- c("SB/SBF=0", "PI 3: Catch\n(relative to 2013-2015)", "PI 4: P&L CPUE\n(relative to 2001-2004)" ,"PI 4: PS CPUE\n(relative to 2012)")#, "Effort\n(relative to reference period)")
  
  
  # pis to plot time series of
  
  # Try facetting rather than plotting one on top of the other
  output$plot_timeseries_comparehcr <- renderPlot({

    show_spaghetti <- input$showspag
    hcr_choices <- input$hcrchoice
    # Only these ones allowed
    pi_choices <- input$pichoice
    pi_choices <- pi_choices[pi_choices %in% pinames_ts]
    if((length(hcr_choices) < 1) | (length(pi_choices) < 1)){
      return()
    }

    catch_area_choice <- input$catchareachoice
    
    #dat <- subset(yearqs, area %in% c("all", catch_area_choice, "ps678x") & piname %in% pi_choices & metric != "catch stability")
    #wormdat <- subset(worms, area %in% c("all", catch_area_choice, "ps678x") & piname %in% pi_choices & metric != "catch stability" & iter %in% wormiters)
    
    
    dat <- yearqs[metric != "catch stability" &
            piname %in% pi_choices &
            area %in% c("all", catch_area_choice, "pl_jp", "ps678x") &
            # Drop areas not in catch_area_choice from PI 3 and 6 only  (to deal with pl_jp)
            !(piname == "PI 3: Catch\n(relative to 2013-2015)" & !(area %in% catch_area_choice)) &
            !(piname == "PI 6: Catch stability" & !(area %in% catch_area_choice))]
      
    wormdat <- worms[metric != "catch stability" &
            piname %in% pi_choices &
            area %in% c("all", catch_area_choice, "pl_jp", "ps678x") &
            # Drop areas not in catch_area_choice from PI 3 and 6 only  (to deal with pl_jp)
            !(piname == "PI 3: Catch\n(relative to 2013-2015)" & !(area %in% catch_area_choice)) &
            !(piname == "PI 6: Catch stability" & !(area %in% catch_area_choice))]

    p <- time_series_plot(dat=dat, hcr_choices=hcr_choices, wormdat=wormdat, last_plot_year=last_plot_year, short_term = short_term, medium_term = medium_term, long_term = long_term, show_spaghetti=show_spaghetti, outer_percentile_range = outer_percentiles, inner_percentile_range = inner_percentiles)
    # Facet by PI
    p <- p + facet_grid(factor(piname, levels=pis_list) ~ hcrref, scales="free")#, ncol=1)
    #p <- p + ylab("Catch")
    p <- p + ggplot2::ylim(c(0,NA))
    # Axes limits set here or have tight?
    p <- p + ggplot2::scale_x_continuous(expand = c(0, 0))
    p <- p + ggplot2::ylab("Value")
    # Add LRP and TRP if SB/SBF=0 is plotted
    if ("SB/SBF=0" %in% pi_choices){
      p <- p + ggplot2::geom_hline(data=data.frame(yint=lrp,piname="SB/SBF=0"), ggplot2::aes(yintercept=yint), linetype=2)
      #p <- p + ggplot2::geom_hline(data=data.frame(yint=trp,piname="SB/SBF=0"), ggplot2::aes(yintercept=yint), linetype=2)
      #p <- p + ggplot2::geom_hline(data=data.frame(yint=trp2, piname="SB/SBF=0"), ggplot2::aes(yintercept=yint), linetype=2)
      p <- p + ggplot2::geom_hline(data=data.frame(yint=trp3, piname="SB/SBF=0"), ggplot2::aes(yintercept=yint), linetype=2)
    }
      # Add 1.0 line for relative CPUE PL
      if ("PI 4: P&L CPUE\n(relative to 2001-2004)" %in% pi_choices){
        p <- p + ggplot2::geom_hline(data=data.frame(yint=1.0, piname="PI 4: P&L CPUE\n(relative to 2001-2004)"), ggplot2::aes(yintercept=yint), linetype=2)
      }
      # Add 1.0 line for relative CPUE PL
      if ("PI 4: PS CPUE\n(relative to 2012)" %in% pi_choices){
        p <- p + ggplot2::geom_hline(data=data.frame(yint=1.0, piname="PI 4: PS CPUE\n(relative to 2012)"), ggplot2::aes(yintercept=yint), linetype=2)
      }
    
    
    # Size of labels etc
    p <- p + theme(axis.text=element_text(size=16), axis.title=element_text(size=16), strip.text=element_text(size=16), legend.text=element_text(size=16))
    return(p)
  }, height=function(){max(height_per_pi*1.5, (height_per_pi * length(input$pichoice[input$pichoice %in% pinames_ts])))})

  get_pi_table <- function(period_choice="Short"){
    hcr_choices <- input$hcrchoice
    pi_choices <- input$pichoice
    catch_area_choice <- input$catchareachoice
    if((length(hcr_choices) < 1) | (length(pi_choices) < 1)){
      return()
    }
    catch_area_choice <- input$catchareachoice
    #dat <- subset(periodqs, hcrref %in% hcr_choices & period == period_choice & area %in% c("all", catch_area_choice, "ps678x") & piname %in% pi_choices & metric != "catch stability")
    # See bar box compare for notes on this horrendous subset
    dat <- periodqs[period == period_choice &
           metric != "catch stability" &
           piname %in% pi_choices &
           area %in% c("all", catch_area_choice, "pl_jp", "ps678x") &
           # Drop areas not in catch_area_choice from PI 3 and 6 only  (to deal with pl_jp)
           !(piname == "PI 3: Catch\n(relative to 2013-2015)" & !(area %in% catch_area_choice)) &
           !(piname == "PI 6: Catch stability" & !(area %in% catch_area_choice))]
    tabdat <- pitable(dat, percentile_range = outer_percentiles)
    return(tabdat)
  }

  output$table_pis_short <- renderTable({
      tabdat <- get_pi_table(period_choice="Short")
    },
    rownames = FALSE,
    caption= "Performance indicators in the short-term",
    auto=TRUE
  )

  output$table_pis_medium <- renderTable({
      tabdat <- get_pi_table(period_choice="Medium")
    },
    rownames = FALSE,
    caption= "Performance indicators in the medium-term",
    auto=TRUE
  )

  output$table_pis_long <- renderTable({
      tabdat <- get_pi_table(period_choice="Long")
    },
    rownames = FALSE,
    caption= "Performance indicators in the long-term",
    auto=TRUE
  )

  #-------------------------------------------------------------------
  # Individual PI plots

  # For exploring the catches in different regions
  output$plot_pi3 <- renderPlot({
    # If no HCRs chosen just leave
    hcr_choices <- input$hcrchoice
    if(length(hcr_choices) < 1){
      return()
    }

    plot_choice <- input$plotchoicebarboxtime
    area_choice <- input$areachoice
    if(length(area_choice) < 1){
      return()
    }
    # Choose if relative to year X
    catch_rel_choice <- "relative catch"
    ylabel <- "PI 3: Catch (rel. to 2013-2015)"

    if (plot_choice %in% c("median_bar","box")){
      dat <- subset(periodqs, period != "Rest" & pi=="pi3" & area %in% area_choice & metric == catch_rel_choice) 
      p <- barboxplot(dat=dat, hcr_choices=hcr_choices, plot_type=plot_choice, quantiles=quantiles)
      p <- p + ggplot2::ylab(ylabel)
      #p <- p + ggplot2::ylim(c(0,NA))
      p <- p + ggplot2::facet_wrap(~area_name, ncol=no_facets_row)
    }

    if(plot_choice == "time"){
      show_spaghetti <- input$showspag
      dat <- subset(yearqs, pi=="pi3" & area %in% area_choice & metric == catch_rel_choice) 
      wormdat <- subset(worms, pi=="pi3" & area %in% area_choice & metric == catch_rel_choice & iter %in% wormiters) 
      p <- time_series_plot(dat=dat, hcr_choices=hcr_choices, wormdat=wormdat, last_plot_year=last_plot_year, short_term = short_term, medium_term = medium_term, long_term = long_term, show_spaghetti=show_spaghetti, outer_percentile_range = outer_percentiles, inner_percentile_range = inner_percentiles)
      p <- p + facet_grid(area_name ~ hcrref, scales="free")#, ncol=1)
      p <- p + ggplot2::ylab(ylabel)
      #p <- p + ggplot2:: ylim(c(0,NA))
      # Axes limits set here or have tight?
      p <- p + ggplot2::scale_x_continuous(expand = c(0, 0))
      # Size of labels etc
      p <- p + theme(axis.text=element_text(size=16), axis.title=element_text(size=16), strip.text=element_text(size=16), legend.text=element_text(size=16))
    }
    return(p)
  }, height=function(){
    # Height of each facet is a little complicated
    if(input$plotchoicebarboxtime=="time"){return(max(height_per_area*1.5, (height_per_area * length(input$areachoice))))}
    if(input$plotchoicebarboxtime %in% c("median_bar","box")){return(max(height_per_area*1.5, (height_per_area * ceiling(length(input$areachoice) / no_facets_row))))}
  })
  
  # For exploring the vulnerable biomass to pole and line in different regions
  output$plot_vulnb <- renderPlot({
    # If no HCRs chosen just leave
    hcr_choices <- input$hcrchoice
    if(length(hcr_choices) < 1){
      return()
    }
    vbscale <- input$vbscale
    plot_choice <- input$plotchoicebarboxtime
    ylabel <- "Vulnerable biomass to pole and line fisheries"

    if (plot_choice %in% c("median_bar","box")){
      dat <- subset(periodqs, period != "Rest" & pi=="vb") 
      p <- barboxplot(dat=dat, hcr_choices=hcr_choices, plot_type=plot_choice, quantiles=quantiles)
      p <- p + ylab(ylabel)
      #p <- p + ylim(c(0,NA))
      p <- p + facet_wrap(~area_name, ncol=no_facets_row, scales=vbscale)
    }

    if(plot_choice == "time"){
      show_spaghetti <- input$showspag
      dat <- subset(yearqs, pi=="vb") 
      wormdat <- subset(worms, pi=="vb" & iter %in% wormiters) 
      p <- time_series_plot(dat=dat, hcr_choices=hcr_choices, wormdat=wormdat, last_plot_year=last_plot_year, short_term = short_term, medium_term = medium_term, long_term = long_term, show_spaghetti=show_spaghetti, outer_percentile_range = outer_percentiles, inner_percentile_range = inner_percentiles)
      p <- p + facet_grid(area_name ~ hcrref, scales=vbscale)#, ncol=1)
      p <- p + ylab(ylabel)
      #p <- p +  ylim(c(0,NA))
      # Axes limits set here or have tight?
      p <- p + scale_x_continuous(expand = c(0, 0))
      # Size of labels etc
      p <- p + theme(axis.text=element_text(size=16), axis.title=element_text(size=16), strip.text=element_text(size=16), legend.text=element_text(size=16))
    }
    return(p)
  })
  
  output$plot_relcpuepl <- renderPlot({
    # If no HCRs chosen just leave
    hcr_choices <- input$hcrchoice
    if(length(hcr_choices) < 1){
      return()
    }
    #vbscale <- input$vbscale
    plot_choice <- input$plotchoicebarboxtime
    ylabel <- "Relative CPUE of pole and line fisheries"

    if (plot_choice %in% c("median_bar","box")){
      #dat <- subset(periodqs, period != "Rest" & pi=="pi4" & piname =="PI 4: Relative P&L CPUE") 
      dat <- subset(periodqs, period != "Rest" & pi=="pi4" & piname =="PI 4: P&L CPUE\n(relative to 2001-2004)") 
      p <- barboxplot(dat=dat, hcr_choices=hcr_choices, plot_type=plot_choice, quantiles=quantiles)
      p <- p + ylab(ylabel)
      p <- p + ylim(c(0,NA))
      p <- p + facet_wrap(~area_name, ncol=no_facets_row, scales="free")
    }

    if(plot_choice == "time"){
      show_spaghetti <- input$showspag
      #dat <- subset(yearqs, pi=="pi4" & piname=="PI 4: Relative P&L CPUE") 
      dat <- subset(yearqs, pi=="pi4" & piname=="PI 4: P&L CPUE (relative to\n2001-2004)") 
      #wormdat <- subset(worms, pi=="pi4" & piname=="PI 4: Relative P&L CPUE" & iter %in% wormiters) 
      wormdat <- subset(worms, pi=="pi4" & piname=="PI 4: P&L CPUE (relative to\n2001-2004)" & iter %in% wormiters) 
      p <- time_series_plot(dat=dat, hcr_choices=hcr_choices, wormdat=wormdat, last_plot_year=last_plot_year, short_term = short_term, medium_term = medium_term, long_term = long_term, show_spaghetti=show_spaghetti, outer_percentile_range = outer_percentiles, inner_percentile_range = inner_percentiles)
      p <- p + facet_grid(area_name ~ hcrref, scales="free")#, ncol=1)
      p <- p + ylab(ylabel)
      p <- p +  ylim(c(0,NA))
      # Axes limits set here or have tight?
      p <- p + scale_x_continuous(expand = c(0, 0))
      # Size of labels etc
      p <- p + theme(axis.text=element_text(size=16), axis.title=element_text(size=16), strip.text=element_text(size=16), legend.text=element_text(size=16))
    }
    return(p)
  })


  output$plot_pi6 <- renderPlot({
    # If no HCRs chosen just leave
    hcr_choices <- input$hcrchoice
    if(length(hcr_choices) < 1){
      return()
    }
    plot_choice <- input$plotchoicebarbox
    area_choice <- input$areachoice
    # Need additional option of stability or variability
    if(length(area_choice) < 1){
      return()
    }

    # Options:
    #   bar or box - handled in the plot_choice
    #   stab or variability
    stabvar_choice <- input$stabvarchoice
    metric_choice <-  paste("relative catch", stabvar_choice, sep=" ") # or stability
    ylabel <- paste("PI 6: ",   paste0(toupper(substr(stabvar_choice, 1, 1)), substr(stabvar_choice, 2, nchar(stabvar_choice))), " of relative catch", sep="")
    dat <- subset(periodqs, period != "Rest" & pi=="pi6" & area %in% area_choice & metric == metric_choice) 
    p <- barboxplot(dat=dat, hcr_choices=hcr_choices, plot_type=plot_choice, quantiles=quantiles)
    p <- p + ggplot2::ylab(ylabel)
    #p <- p + ggplot2::ylim(c(0,NA))
    p <- p + ggplot2::facet_wrap(~area_name, ncol=no_facets_row)
    return(p)

  }, height=function(){
    return(max(height_per_area*1.5, (height_per_area * ceiling(length(input$areachoice) / no_facets_row))))
  })

  # Bar and box plot 
  # PI 7: Effort variability and stability
  plot_barbox_pi7varstab <- function(plot_type="median_bar", metric_choice="relative effort variability", ylab="PI 7: Relative effort variability"){#}, ylim=c(0,NA)){
    rPlot <- renderPlot({
      hcr_choices <- input$hcrchoice
      if(length(hcr_choices) < 1){
        return()
      }
      # Choose if relative to year X
      dat <- subset(periodqs, period != "Rest" & pi=="pi7" & metric==metric_choice) 
      p <- barboxplot(dat=dat, hcr_choices=hcr_choices, plot_type=plot_type, quantiles=quantiles)
      p <- p + ggplot2::ylab(ylab)
      #p <- p + ggplot2::ylim(ylim)
      return(p)
    })
    return(rPlot)
  }

  output$plot_bar_pi7var <- plot_barbox_pi7varstab(plot_type="median_bar", metric_choice="relative effort variability", ylab="PI 7: Variability of relative effort")#, ylim=c(0,NA))
  output$plot_box_pi7var <- plot_barbox_pi7varstab(plot_type="box", metric_choice="relative effort variability", ylab="PI 7: Variability of relative effort")#, ylim=c(0,NA))
  output$plot_bar_pi7stab <- plot_barbox_pi7varstab(plot_type="median_bar", metric_choice="relative effort stability", ylab="PI 7: Stability")#, ylim=c(0,1))
  output$plot_box_pi7stab <- plot_barbox_pi7varstab(plot_type="box", metric_choice="relative effort stability", ylab="PI 7: Stability")#, ylim=c(0,1))

  # Plot the HCR shapes and the bits that were active
  output$plot_hcrshape <- renderPlot({
    # Able to choose which HCRs
    hcr_choices <- input$hcrchoice
    if(length(hcr_choices) < 1){
      return()
    }

    # Secret HCR performance options
    if(input$hcrperformance=="shownothing"){
      showpoints <- FALSE
      showpaths <- FALSE
    }
    hcr_points_sub <- hcr_points
    if(input$hcrperformance=="showpoints"){
      showpoints <- TRUE
      showpaths <- FALSE
      nhcriters <- min(10, length(common_iters))
      hcriters <- sample(common_iters, nhcriters)
      hcr_points_sub <- subset(hcr_points, iter %in% hcriters)
    }
    if(input$hcrperformance=="showpaths"){
      showpoints <- TRUE
      showpaths <- TRUE
      hcriters <- input$hcrperfiter
      hcr_points_sub <- subset(hcr_points, iter %in% hcriters)
    }
    p <- hcr_plot(hcr_choices=hcr_choices, hcr_shape=hcr_shape, hcr_points=hcr_points_sub, lrp=lrp, trp=trp3, add_points=showpoints, add_path=showpaths)
    p <- p + ylab("Catch or effort multiplier")
    return(p)
  })
  
  # Kobe / Majuro stuff
  majuro_kobe_table <- function(period="Short", period_label="Short-term"){
    outfunc <- renderTable({
      plot_choice <- input$majurokobe
      if(plot_choice == "Kobe"){
        dat <- kobe_summary_tabs[[period]]
      }
      else {
        dat <- majuro_summary_tabs[[period]]
      }
      return(dat)
    },
    rownames = FALSE,
    caption = paste(period_label, " summary. Table values show the percentage (%) of observations in each plot quadrant.", sep=""),
      auto=TRUE)
    return(outfunc)
  }

  output$table_kobesummary_short <- majuro_kobe_table(period="Short", period_label="Short-term")
  output$table_kobesummary_medium <- majuro_kobe_table(period="Medium", period_label="Medium-term")
  output$table_kobesummary_long <- majuro_kobe_table(period="Long", period_label="Long-term")
  output$table_kobesummary_hist <- majuro_kobe_table(period="Historical", period_label="Historical")

  majuro_kobe_plot <- function(period){
    out <- renderPlot({
      hcr_choice <- input$hcrchoicekobe
      plot_choice <- input$majurokobe
      if(plot_choice == "Kobe"){
        dat <- kobe_ptables_indiv[[hcr_choice]]
      }
      else {
        dat <- majuro_ptables_indiv[[hcr_choice]]
      }
      grid::grid.draw(dat[[period]])
    })
    return(out)
  }

  output$plot_kobe_ptables_hist <- majuro_kobe_plot(period="Historical")
  output$plot_kobe_ptables_long <- majuro_kobe_plot(period="Long")
  output$plot_kobe_ptables_medium <- majuro_kobe_plot(period="Medium")
  output$plot_kobe_ptables_short <- majuro_kobe_plot(period="Short")
  
  # Checking distribution of scaler in first year
  #pdat <- hcrresultsdf[year == 2021] 
  #p <- ggplot(pdat, aes(x=scaler)) + geom_histogram() + facet_wrap(~msectrl)
  
  # Robustness plots
  output$plot_box_robust <- renderPlot({
      hcr_choices <- input$hcrchoice
      pi_choices <- input$pichoice
      if((length(hcr_choices) < 1) | (length(pi_choices) < 1)){
        return()
      }
      # There are a maximum of length(pis_list) panels (currently 10)
      # pi1, pi3, pi4 (x2), pi6, pi7, pi8, relative effort, sbsbf0, sbsbf0 relative to TRP
      # (see pis_list above)
      # They have different groupings based on metric and area
      # pi1: metric = SBSBF0, area = 1-8, all
      # pi3: area = 1-8, total, ps678, pl_jp
      # pi4: area = ps678x, 1 - 4, pl_jp
      # pi6: area = as pi3 and metric catch stability / relative catch stability (not in piname so need additional subset)
      # pi7: area = ps678x
      # pi8: area = all
      # sbsbsf0: area = 1-8, all
      catch_area_choice <- input$catchareachoice
      
      # Subset out the data you want to plot based on user options
      # Need to be careful as PI 4 has six areas - PL 1-4 (for indiv P&L) pl_jp and ps678x
      # Gets tricky with areas for PI 3 and 4
      # For 3 we want one of pl_jp, ps678 or total - given by catchareachoice
      # For 4 we want both of pl_jp and ps678x - pl_jp is the pain
      dat <- periodqs_robust[set == input$robustset & period != "Rest" &
             metric != "catch stability" &
             piname %in% pi_choices &
             area %in% c("all", catch_area_choice, "pl_jp", "ps678x") &
             # Drop areas not in catch_area_choice from PI 3 and 6 only  (to deal with pl_jp)
             !(piname == "PI 3: Catch\n(relative to 2013-2015)" & !(area %in% catch_area_choice)) &
             !(piname == "PI 6: Catch stability" & !(area %in% catch_area_choice))]
      
      p <- barboxplot(dat=dat, hcr_choices=hcr_choices, plot_type="box", quantiles=quantiles)
      # Do we fix axis at 0? Probably
      p <- p + ggplot2::ylim(0,NA)
      # Coord cartesian to zoom
      #p <- p + coord_cartesian(ylim=c(0.5,NA)) # Fixes barcharts - all of them at 0.5
      # How to get different ones
      p <- p + ggplot2::ylab("Value") + ggplot2::xlab("Time period")
      # Add LRP and TRP lines
      # Only if SB/SBF=0 is in dat
      if ("SB/SBF=0" %in% pi_choices){
        p <- p + ggplot2::geom_hline(data=data.frame(yint=lrp, piname="SB/SBF=0"), ggplot2::aes(yintercept=yint), linetype=2)
        p <- p + ggplot2::geom_hline(data=data.frame(yint=trp3, piname="SB/SBF=0"), ggplot2::aes(yintercept=yint), linetype=2)
      }
      # Add 1.0 line for relative to TRP plot
      if ("SB/SBF=0 relative to target" %in% pi_choices){
        p <- p + ggplot2::geom_hline(data=data.frame(yint=1.0, piname="SB/SBF=0 relative to target"), ggplot2::aes(yintercept=yint), linetype=2)
      }
      # Must have a probability of at least 0.8 above LRP
      if ("PI 1: Prob. above LRP" %in% pi_choices){
        p <- p + ggplot2::geom_hline(data=data.frame(yint=0.8,piname="PI 1: Prob. above LRP"), ggplot2::aes(yintercept=yint), linetype=2)
      }
      # Add 1.0 line for relative effort
      if ("Effort\n(relative to reference period)" %in% pi_choices){
        p <- p + ggplot2::geom_hline(data=data.frame(yint=1.0, piname="Effort\n(relative to reference period)"), ggplot2::aes(yintercept=yint), linetype=2)
      }
      # Add 1.0 line for relative CPUE PL
      if ("PI 4: P&L CPUE\n(relative to 2001-2004)" %in% pi_choices){
        p <- p + ggplot2::geom_hline(data=data.frame(yint=1.0, piname="PI 4: P&L CPUE\n(relative to 2001-2004)"), ggplot2::aes(yintercept=yint), linetype=2)
      }
      # Add 1.0 line for relative CPUE PL
      if ("PI 4: PS CPUE\n(relative to 2012)" %in% pi_choices){
        p <- p + ggplot2::geom_hline(data=data.frame(yint=1.0, piname="PI 4: PS CPUE\n(relative to 2012)"), ggplot2::aes(yintercept=yint), linetype=2)
      }
      #p <- p + ggplot2::facet_wrap(~piname, scales="free", ncol=no_facets_row)
      # Why do I have to make the wrap a factor when it already is?
      p <- p + ggplot2::facet_wrap(~factor(piname, levels=pis_list), scales="free", ncol=no_facets_row)
      return(p)
    },
    height=function(){
      return(max(height_per_pi*1.5, (height_per_pi * ceiling(length(input$pichoice) / no_facets_row))))
    }
  )
    
    
    
    
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # HCR output analysis plots
  hcr_op_plots <- function(type="op"){
    hcr_choices <- input$hcrchoice
    if(length(hcr_choices) < 1){
      return()
    }
    if(type=="op"){
      pdat <- subset(scaler, hcrref %in% hcr_choices)
      ylabel <- ("Catch or effort multiplier")
      minq_col <- paste0("X",min(quantiles)*100,".")
      miny <- min(pdat[,..minq_col]) * 0.9
      ylims <- c(miny, NA)
    }
    if(type=="diff"){
      pdat <- subset(scaler_diff, hcrref %in% hcr_choices)
      ylabel <- ("Absolute change in catch or effort multiplier")
      ylims <- c(0, NA)
    }
    
    all_hcr_names <- sort(unique(scaler$hcrref))
    hcr_cols <- get_hcr_colours(hcr_names=all_hcr_names, chosen_hcr_names=hcr_choices)
    quantiles_text <- paste0("X", quantiles * 100, ".")
    ymin <- quantiles_text[1]
    ymax <- quantiles_text[4]
    lower <- quantiles_text[2]
    upper <- quantiles_text[3]
    
    p <- ggplot(pdat, aes(x=as.factor(year)))
    p <- p + geom_boxplot(aes_string(ymin=ymin, ymax=ymax, lower=lower, upper=upper, middle="X50.", fill="hcrref"), stat="identity", width=0.7)
    p <- p + ylab(ylabel)
    p <- p + xlab("Management period start year")
    p <- p + scale_fill_manual(values=hcr_cols)
    p <- p + ylim(ylims)
    p <- p + theme_bw()
    p <- p + theme(axis.text=element_text(size=16), axis.title=element_text(size=16), strip.text=element_text(size=16), legend.text=element_text(size=16))
    p <- p + theme(legend.position="bottom", legend.title=element_blank(), axis.text.x = element_text(angle = 45, vjust=0.5))
    return(p)
  }
  
  output$plot_hcr_op <- renderPlot({
    return(hcr_op_plots(type="op"))
  })
  
  output$plot_hcr_op_diff <- renderPlot({
    return(hcr_op_plots(type="diff"))
  })
  
  output$hcr_table <- renderDT({
    hcr_tab <- get_hcr_param_table()
    # Able to choose which HCRs
    hcr_choices <- input$hcrchoice
    if(length(hcr_choices) < 1){
      return()
    }
    
    hcr_tab <- hcr_tab[hcr_tab$hcr_ref %in% hcr_choices,]
    
    #all_hcr_names <- unique(hcr_tab$hcr_ref)
    all_hcr_names <- unique(periodqs$hcrref) # Get from periodqs because we may have subset outs we don't want
    hcr_cols <- get_hcr_colours(hcr_names=all_hcr_names, chosen_hcr_names=hcr_choices)
    
    # Syntax to not show a column is horrible. But cannot drop column as needed to colour the rows
    hcr_tabDT <- datatable(hcr_tab,
                   options=list(
                     pageLength=length(hcr_choices)*2,
                     dom='t',
                     columnDefs=list(list(visible=FALSE, targets=0)), # Don't show first column
                     ordering=F), # Remove ordering arrows
                   rownames=FALSE,
                   ) 
    hcr_tabDT <- formatStyle(hcr_tabDT, columns="hcr_ref", target='row', color=styleEqual(hcr_choices, hcr_cols), fontWeight = 'bold')
    return(hcr_tabDT)
  })

} # end of server

# Run the app
shinyApp(ui, server)
