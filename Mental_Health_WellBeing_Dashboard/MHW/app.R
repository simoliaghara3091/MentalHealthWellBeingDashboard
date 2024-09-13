#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readxl)
library(tidyverse)
library(DT)
library(dplyr)
library(ggplot2)
library(plotly)
library(shinythemes)

source("Data.R")

ui <- navbarPage(
  "Mental Health WellBeing Dashboard By Simoli Aghara",
  tabPanel("About Section",
           fluidPage(
             theme = shinytheme("cerulean"),
             fluidRow(
               column(12,
                      h2("Introduction", style = "font-size: 18px;"),
                      p("Welcome to the Mental Health WellBeing Dashboard. This dashboard provides an overview of mental health and wellbeing statistics.
                        It shed's light on the factors that contribute to or mitigate negative mental health feelings. 
                        Furthermore it helps contribute valuable insights to the broader conversation surrounding mental health. 
                        Thus, helpin in fostering a deeper understanding of the intricate interplay between surroundings and mental 
                        well-being as this relationship holds profound significance, as it is crucial for fostering self-awareness among individuals, 
                        aiding medical professionals in tailoring treatments."),
                      h3("Why is it imprtant to learn about Mental Health" , style = "font-size: 18px;"),
                      p("Mental health includes our emotional, psychological, and social well-being. It affects 
                      how we think, feel, and act. It also helps determine how we handle stress, relate to others,
                      and make healthy choices. Mental health is important at every stage of life, 
                      from childhood and adolescence through adulthood."),
                      h4("About the Data", style = "font-size: 18px;"),
                      p("The data used in this dashboard has been obtained from reputable sources including the Centers for Disease Control and Prevention (CDC) and the National Health Interview Survey (NHIS) website."),
                      p("The information presented in the data table is derived from responses provided by the civilian noninstitutionalized population."),
                      h5("Survey Questions", style = "font-size: 16px;"),
                      HTML("<p>The interview questions asked to the respondents include:</p>
                           <ol>
                             <li>How often do you feel worried, nervous, or anxious? Would you say daily, weekly, monthly, a few times a year, or never? (Response recorded as Anx_Freq)</li>
                             <li>Thinking about the last time you felt worried, nervous, or anxious, how would you describe the level of these feelings? Would you say a little, a lot, or somewhere in between? (Response recorded as Anx_Level)</li>
                             <li>During the past 12 months, did you take prescription medication for these feelings? (Response recorded as Anx_meds)</li>
                             <li>During the past 12 months, did you receive counseling or therapy from a mental health professional such as a psychiatrist, psychologist, psychiatric nurse, or clinical social worker? (Response recorded as Anx_Therapy)</li>
                           </ol>"
                      ),
                      h6("Demographic Information", style = "font-size: 18px;"),
                      p("In addition to mental health-related questions, demographic information including age, gender, race, education, employment, and marital status is collected from the respondents."),
                      h6("Data Sources", style = "font-size: 18px;"),
                      p(" Centers for Disease Control and Prevention. (2018, August 27). NHIS-Adult Summary Health Statistics. Centers for Disease Control and Prevention. https://wwwn.cdc.gov/NHISDataQueryTool/SHS_adult/index.html"),
                      p(" Technical Notes for Interactive Summary Health Statistics — 2019–2022: National Health Interview Survey https://wwwn.cdc.gov/NHISDataQueryTool/SHS_adult/SHS_Tech_Notes.pdf"),
                      p(" Centers for Disease Control and Prevention. (2023, April 6). NHIS - data, questionnaires and related documentation. Centers for Disease Control and Prevention. https://www.cdc.gov/nchs/nhis/data-questionnaires-documentation.html")
                      
                      
               )
             )
           )
           
  ),
  tabPanel("Table",
           theme = shinytheme("cerulean"),
           dataTableOutput("table")
  ),
  tabPanel("Anxiety Trends over the years",
           fluidPage(
             theme = shinytheme("cerulean"),
             fluidRow(
               column(12,
                      h2("Anxiety Trends"),
                      p("All of us at sometime have felt anxious over something or the other and sometimes
                        someone might have felt it on a regular baises. It is important to understand in these
                        times how it has changed over the years. The visualization below shows trends over the 
                        years 2019 - 2022 for frequency and intensity in the feeling of anxiety."),
                      
                      plotlyOutput("trend_anx_freq_Plot"),
                      p("Based on the visualization above we can say that people who have never and sometimes felt anxious 
                        has lessend as years go by. On the other hand people who have felt it daily, weekly or monthly has almost 
                        remained the same. Which according to me is not a good sign as there haven't been any
                        change and people have neither improved nor gotten worse."),
                      
                      plotlyOutput("trend_anx_level_Plot"),
                      p("Based on the visualization above we can say that people whose intensity of feeling was lot
                        has remained same over the years. Those whose intensity was less has reduced over the years.
                        This indicates that with time this is becoming more and more serious and more people
                        are suffering from it."),
                      p("This indicates that we need to seriously consider this issue and need to work to 
                        reduce the number of people suffering from anxiety.")
               )
             )
           )
  ),
  tabPanel("Correlations",
           theme = shinytheme("cerulean"),
           p("Based on the previous visualization and its insight that more and more people have been 
           suffering from anxiety. We now need to find the correlations between how factors such as
           education, employment status and martial status and those changes. The following visualizations will help answer questions 
           such as is it a particular level of education that causes anxiety the most, or it can be seen within full
             time employees because they have work stress, or part time because they do not have enpugh work, 
             and different marital status as well."),
           tabsetPanel(
             tabPanel("Education",
                      fluidPage(
                        fluidRow(
                          column(12,
                                 h2("About", style = "font-size: 16px;"),
                                 p("The visualization below gives insight on how different levels of education has 
                                 affected the frequency and intensity of feelings of nervousness, anxiousness, and worry.
                                 It sheds light on such that does one particular level of education cause more anxiety within people."),
                                 selectInput("yearInput", "Select Year", choices = unique(Data$Year)),
                                 h3("Education VS Intensity and Frequency in Anxiety", style = "font-size: 16px;"),
                                 plotlyOutput("anxiety_freq_education_Plot"),
                                 plotlyOutput("anxiety_level_education_Plot")
                          )
                        )
                      )
             )
           ),
           tabsetPanel(
             tabPanel("Employment Status",
                      fluidPage(
                        fluidRow(
                          column(12,
                                 h2("About", style = "font-size: 16px;"),
                                 p("The visualization below gives insight on how different employment status affected the
                                   frequency and intensity of feelings. It helps answer questions such as people 
                                   with full time jobs feel more anxious or people with part time jobs feel more
                                   anxious."),
                                 selectInput("yearInput1", "Select Year", choices = unique(Data$Year)),
                                 h3("Employment Status VS Intensity and Frequency in Anxiety", style = "font-size: 16px;"),
                                 plotlyOutput("anxiety_freq_empstatus_Plot"),
                                 plotlyOutput("anxiety_level_empstatus_Plot")
                          )
                        )
                      )
             )
           ),
           tabsetPanel(
             tabPanel("Maritial Status",
                      fluidPage(
                        fluidRow(
                          column(12,
                                 h2("About", style = "font-size: 16px;"),
                                 p("The visualization below gives insight on how different maritial status affected the
                                   frequency and intensity of feelings. It shows how different relationship status
                                   affects your mental health and also highlights the message that it is vitally
                                   important for you to select the correct partner."),
                                 selectInput("yearInput2", "Select Year", choices = unique(Data$Year)),
                                 h3("Maritial Status VS Intensity and Frequency in Anxiety", style = "font-size: 16px;"),
                                 plotlyOutput("anxiety_freq_maritial_Plot"),
                                 plotlyOutput("anxiety_level_maritial_Plot")
                          )
                        )
                      )
             )
           ),
           p("All in all the above visualizations highlight some of the reasons behind the changes
             in the feelings of anxiety within United States. It also shows how different life choices
             affect their mental health.")
  ),
  tabPanel("Yes - Therapy, No - Meds",
           theme = shinytheme("cerulean"),
           p("Previously we explored the reason behind the changes, now we are moving on to which treatment
             is the most successful within different people such as people within different age groups,
             gender, and racial groups. Below we explore people going through only therapy and not medications
             for the last twelve months."),
           tabsetPanel(
             tabPanel("Age",
                      fluidPage(
                        fluidRow(
                          column(12,
                                 h2("About", style = "font-size: 16px;"),
                                 p("The visualization below gives insight on which is a better treatment plan for 
                                     people of different ages. The visualizations below show whether therapy or medicine 
                                     is more successful for people in different age groups."),
                                 selectInput("yearInput3", "Select Year", choices = unique(Data$Year)),
                                 h3("Yes - Therapy, No - Medicine", style = "font-size: 16px;"),
                                 p("This visualization shows how the frequency and level of people suffering 
             from anxiety changed after they went through therapy but not medication."),
                                 plotlyOutput("anxietyfreqPlot"),
                                 plotlyOutput("anxietylevelPlot")
                          )
                        )
                      )
             )
           ),
           tabsetPanel(
             tabPanel("Gender",
                      fluidPage(
                        fluidRow(
                          column(12,
                                 h2("About", style = "font-size: 16px;"),
                                 p("The visualization below gives insight on which is a better treatment plan for 
                                     people of different gender. The visualizations below show whether therapy or medicine 
                                     is more successful for people in different genders."),
                                 selectInput("yearInput4", "Select Year", choices = unique(Data$Year)),
                                 h3("Yes - Therapy, No - Medicine", style = "font-size: 16px;"),
                                 p("This visualization shows how the frequency and level of people suffering 
             from anxiety changed after they went through therapy but not medication."),
                                 plotlyOutput("anxietyfreq_gender_Plot"),
                                 plotlyOutput("anxietylevel_gender_Plot")
                          )
                        )
                      )
             )
           ),
           tabsetPanel(
             tabPanel("Race",
                      fluidPage(
                        fluidRow(
                          column(12,
                                 h2("About", style = "font-size: 16px;"),
                                 p("The visualization below gives insight on which is a better treatment plan for 
                                     people of different race. The visualizations below show whether therapy or medicine 
                                     is more successful for people in different race groups."),
                                 selectInput("yearInput5", "Select Year", choices = unique(Data$Year)),
                                 h3("Yes - Therapy, No - Medicine", style = "font-size: 16px;"),
                                 p("This visualization shows how the frequency and level of people suffering 
             from anxiety changed after they went through therapy but not medication."),
                                 plotlyOutput("anxietyfreq_race_Plot"),
                                 plotlyOutput("anxietylevel_race_Plot")
                          )
                        )
                      )
             )
           ),
           p("All in all this page gave insights on where therapy works better. An example of 
           this would be within the age group is that reducing the intensity of anxiousness
             feelings therapy works better in the age group of 18 - 29.")
  ),
  tabPanel("No - Therapy, Yes - Meds",
           theme = shinytheme("cerulean"),
           p("Previously we explored the reason behind the changes, now we are moving on to which treatment
             is the most successful within different people such as people within different age groups,
             gender, and racial groups. Below we explore people going through only medications and not therapy
             for the last twelve months."),
           tabsetPanel(
             tabPanel("Age",
                      fluidPage(
                        fluidRow(
                          column(12,
                                 h2("About", style = "font-size: 16px;"),
                                 p("The visualization below gives insight on which is a better treatment plan for 
                                     people of different ages. The visualizations below show whether therapy or medicine 
                                     is more successful for people in different age groups."),
                                 selectInput("yearInput6", "Select Year", choices = unique(Data$Year)),
                                 h3("No - Therapy, Yes - Medicine", style = "font-size: 16px;"),
                                 p("This visualization shows how the frequency and level of people suffering 
             from anxiety changed after they went through medication but not therapy."),
                                 plotlyOutput("anxietyfreq1Plot"),
                                 plotlyOutput("anxietylevel1Plot")
                          )
                        )
                      )
             )
           ),
           tabsetPanel(
             tabPanel("Gender",
                      fluidPage(
                        fluidRow(
                          column(12,
                                 h2("About", style = "font-size: 16px;"),
                                 p("The visualization below gives insight on which is a better treatment plan for 
                                     people of different gender. The visualizations below show whether therapy or medicine 
                                     is more successful for people in different genders."),
                                 selectInput("yearInput7", "Select Year", choices = unique(Data$Year)),
                                 h3("No - Therapy, Yes - Medicine", style = "font-size: 16px;"),
                                 p("This visualization shows how the frequency and level of people suffering 
             from anxiety changed after they went through medication but not therapy."),
                                 plotlyOutput("anxietyfreq_gender1_Plot"),
                                 plotlyOutput("anxietylevel_gender1_Plot")
                          )
                        )
                      )
             )
           ),
           tabsetPanel(
             tabPanel("Race",
                      fluidPage(
                        fluidRow(
                          column(12,
                                 h2("About", style = "font-size: 16px;"),
                                 p("The visualization below gives insight on which is a better treatment plan for 
                                     people of different race. The visualizations below show whether therapy or medicine 
                                     is more successful for people in different race groups."),
                                 selectInput("yearInput8", "Select Year", choices = unique(Data$Year)),
                                 h3("No - Therapy, Yes - Medicine", style = "font-size: 16px;"),
                                 p("This visualization shows how the frequency and level of people suffering 
             from anxiety changed after they went through medication but not therapy."),
                                 plotlyOutput("anxietyfreq_race1_Plot"),
                                 plotlyOutput("anxietylevel_race1_Plot")
                          )
                        )
                      )
             )
           ),
           p("This gave us insights in which case medication works better than therapy. It might also
             help people make decisions such as do they want to opt for only medications to resolve their
             feelings or want to go through therapy.")
           
  ),
  tabPanel("Yes - Therapy, Yes - Meds",
           theme = shinytheme("cerulean"),
           p("Previously we explored the reason behind the changes, and then which is more successful
             therapy or medicine within different age, race and gender groups. Below we look at visualizations
             for people who went through both therapy and medicine in the last twelve months and how they
             have changed."),
           tabsetPanel(
             tabPanel("Age",
                      fluidPage(
                        fluidRow(
                          column(12,
                                 h2("About", style = "font-size: 16px;"),
                                 p("The visualization below gives insight on which is a better treatment plan for 
                                     people of different ages. The visualizations below show whether therapy or medicine 
                                     is more successful for people in different age groups."),
                                 selectInput("yearInput9", "Select Year", choices = unique(Data$Year)),
                                 h3("Yes - Therapy, Yes - Medicine", style = "font-size: 16px;"),
                                 p("This visualization shows how the frequency and level of people suffering 
             from anxiety changed after they went through medication but not therapy."),
                                 plotlyOutput("anxietyfreq2Plot"),
                                 plotlyOutput("anxietylevel2Plot")
                          )
                        )
                      )
             )
           ),
           tabsetPanel(
             tabPanel("Gender",
                      fluidPage(
                        fluidRow(
                          column(12,
                                 h2("About", style = "font-size: 16px;"),
                                 p("The visualization below gives insight on which is a better treatment plan for 
                                     people of different gender. The visualizations below show whether therapy or medicine 
                                     is more successful for people in different genders."),
                                 selectInput("yearInput10", "Select Year", choices = unique(Data$Year)),
                                 h3("Yes - Therapy, Yes - Medicine", style = "font-size: 16px;"),
                                 p("This visualization shows how the frequency and level of people suffering 
             from anxiety changed after they went through medication but not therapy."),
                                 plotlyOutput("anxietyfreq_gender2_Plot"),
                                 plotlyOutput("anxietylevel_gender2_Plot")
                          )
                        )
                      )
             )
           ),
           tabsetPanel(
             tabPanel("Race",
                      fluidPage(
                        fluidRow(
                          column(12,
                                 h2("About", style = "font-size: 16px;"),
                                 p("The visualization below gives insight on which is a better treatment plan for 
                                     people of different race. The visualizations below show whether therapy or medicine 
                                     is more successful for people in different race groups."),
                                 selectInput("yearInput11", "Select Year", choices = unique(Data$Year)),
                                 h3("Yes - Therapy, Yes - Medicine", style = "font-size: 16px;"),
                                 p("This visualization shows how the frequency and level of people suffering 
             from anxiety changed after they went through medication but not therapy."),
                                 plotlyOutput("anxietyfreq_race2_Plot"),
                                 plotlyOutput("anxietylevel_race2_Plot")
                          )
                        )
                      )
             )
           ),
           p("This gave us insights such as if you go through both are you more 
             successful in understanding your feelings then opting to go through only one option.")
  ),
  tabPanel("No - Therapy, No - Meds",
           theme = shinytheme("cerulean"),
           p("Previously we explored the reason behind the changes, and then which is more successful
             therapy or medicine within different age, race and gender groups. Below we look at visualizations
             for people who went through neither therapy nor medicine in the last twelve months and how they
             have changed."),
           tabsetPanel(
             tabPanel("Age",
                      fluidPage(
                        fluidRow(
                          column(12,
                                 h2("About", style = "font-size: 16px;"),
                                 p("The visualization below gives insight on which is a better treatment plan for 
                                     people of different ages. The visualizations below show whether therapy or medicine 
                                     is more successful for people in different age groups."),
                                 selectInput("yearInput12", "Select Year", choices = unique(Data$Year)),
                                 h3("No - Therapy, No - Medicine", style = "font-size: 16px;"),
                                 p("This visualization shows how the frequency and level of people suffering 
             from anxiety changed after they went through medication but not therapy."),
                                 plotlyOutput("anxietyfreq3Plot"),
                                 plotlyOutput("anxietylevel3Plot")
                          )
                        )
                      )
             )
           ),
           tabsetPanel(
             tabPanel("Gender",
                      fluidPage(
                        fluidRow(
                          column(12,
                                 h2("About", style = "font-size: 16px;"),
                                 p("The visualization below gives insight on which is a better treatment plan for 
                                     people of different gender. The visualizations below show whether therapy or medicine 
                                     is more successful for people in different genders."),
                                 selectInput("yearInput13", "Select Year", choices = unique(Data$Year)),
                                 h3("No - Therapy, No - Medicine", style = "font-size: 16px;"),
                                 p("This visualization shows how the frequency and level of people suffering 
             from anxiety changed after they went through medication but not therapy."),
                                 plotlyOutput("anxietyfreq_gender3_Plot"),
                                 plotlyOutput("anxietylevel_gender3_Plot")
                          )
                        )
                      )
             )
           ),
           tabsetPanel(
             tabPanel("Race",
                      fluidPage(
                        fluidRow(
                          column(12,
                                 h2("About", style = "font-size: 16px;"),
                                 p("The visualization below gives insight on which is a better treatment plan for 
                                     people of different race. The visualizations below show whether therapy or medicine 
                                     is more successful for people in different race groups."),
                                 selectInput("yearInput14", "Select Year", choices = unique(Data$Year)),
                                 h3("No - Therapy, No - Medicine", style = "font-size: 16px;"),
                                 p("This visualization shows how the frequency and level of people suffering 
             from anxiety changed after they went through medication but not therapy."),
                                 plotlyOutput("anxietyfreq_race3_Plot"),
                                 plotlyOutput("anxietylevel_race3_Plot")
                          )
                        )
                      )
             )
           ),
           p("This gave us insights such as if you go through both are you more 
             successful in understanding your feelings then opting to go through only one option.") 
  )
  
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  output$table <- renderDataTable({
    return(Data)
  })
  output$trend_anx_freq_Plot <- renderPlotly({
    filtered_data1 <- Data %>% select(Year, Anx_Freq) %>% group_by(Anx_Freq, Year) %>%
      mutate(number = n())
    
    # Create ggplot object
    p <- ggplot(filtered_data1, aes(x = Year, y = number, color = Anx_Freq)) +
      geom_line() +
      geom_point() + 
      scale_color_brewer(palette = "Set3") + 
      labs(title = paste("Frequency Feelings Trend"),
           x = "Year", y = "Count")
    
    # Convert ggplot object to plotly
    ggplotly(p)
  })
  
  output$trend_anx_level_Plot <- renderPlotly({
    filtered_data2 <- Data %>% select(Year, Anx_Level) %>% group_by(Anx_Level, Year) %>%
      mutate(number = n())
    
    # Create ggplot
    p <- ggplot(filtered_data2, aes(x = Year, y = number, color = Anx_Level)) +
      geom_line() +
      geom_point() +
      scale_color_brewer(palette = "Set2") + 
      labs(title = paste("Intensity of Feelings Trend"),
           x = "Year", y = "Count")
    
    # Convert ggplot object to plotly
    ggplotly(p)
    
  })
  
  output$anxiety_freq_education_Plot <- renderPlotly({
    filtered_data27 <- subset(Data, Year == input$yearInput)
    
    # Remove rows with missing values
    filtered_data27 <- na.omit(filtered_data27)
    
    # Calculate count for each level of Anx_Level
    count_data27 <- table(as.factor(filtered_data27$Education), filtered_data27$Anx_Freq)
    
    # Convert table to dataframe
    count_data27 <- as.data.frame(count_data27)
    names(count_data27) <- c("Education", "Anx_Freq", "Count")
    
    # Define bubble size based on count
    count_data27$Size <- sqrt(count_data27$Count)
    
    count_data27$Education_Group <- cut(
      as.numeric(count_data27$Education),
      breaks = c(0, 4, 6, 8, 10, 12),
      labels = c("Grade 1-11", "High School", "Some College", "Bachelor's Degree", "Advanced Degree"),
      include.lowest = TRUE
    )
    
    # Aggregate counts by Education_Group and Anx_Level
    aggregated_data <- aggregate(Count ~ Education_Group + Anx_Freq, data = count_data27, FUN = sum)
    
    # Plot as heatmap
    p <- ggplot(aggregated_data, aes(x = Education_Group, y = Anx_Freq, fill = Count)) +
      geom_tile() +
      scale_fill_gradient2(low = "lightsalmon", mid = "indianred1", high = "brown4", midpoint = median(aggregated_data$Count)) +  # Adjust color gradient for more sensitivity
      labs(title = "Frequency of Anxiety by Education",
           x = "Education Group", y = "Frequency") +
      theme_minimal()
    # Convert ggplot to plotly
    ggplotly(p)
  })
  
  output$anxiety_level_education_Plot <- renderPlotly({
    filtered_data28 <- subset(Data, Year == input$yearInput)
    
    # Remove rows with missing values
    filtered_data28 <- na.omit(filtered_data28)
    
    count_data28 <- table(as.factor(filtered_data28$Education), filtered_data28$Anx_Level)
    
    # Convert table to dataframe
    count_data28 <- as.data.frame(count_data28)
    names(count_data28) <- c("Education", "Anx_Level", "Count")
    
    # Define bubble size based on count
    count_data28$Size <- sqrt(count_data28$Count)
    
    count_data28$Education_Group1 <- cut(
      as.numeric(count_data28$Education),
      breaks = c(0, 4, 6, 8, 10, 12),
      labels = c("Grade 1-11", "High School", "Some College", "Bachelor's Degree", "Advanced Degree"),
      include.lowest = TRUE
    )
    
    # Aggregate counts by Education_Group and Anx_Level
    aggregated_data2 <- aggregate(Count ~ Education_Group1 + Anx_Level, data = count_data28, FUN = sum)

    # Plot as heatmap
    p <- ggplot(aggregated_data2, aes(x = Education_Group1, y = Anx_Level, fill = Count)) +
      geom_tile() +
      scale_fill_gradient2(low = "lightyellow", mid = "orange", high = "brown", midpoint = median(aggregated_data2$Count)) +  # Adjust color gradient for more sensitivity
      labs(title = "Intensity of Anxiety by Education",
           x = "Education Group", y = "Intensity") +
      theme_minimal()
    # Convert ggplot to plotly
    ggplotly(p)
  })
  
  output$anxiety_freq_empstatus_Plot <- renderPlotly({
    filtered_data29 <- subset(Data, Year == input$yearInput1)
    
    # Remove rows with missing values
    filtered_data29 <- na.omit(filtered_data29)
    
    count_data29 <- table(as.factor(filtered_data29$Emp_Status), filtered_data29$Anx_Freq)
    
    # Convert table to dataframe
    count_data29 <- as.data.frame(count_data29)
    names(count_data29) <- c("Emp_Status", "Anx_Freq", "Count")
    
    # Define bubble size based on count
    count_data29$Size <- sqrt(count_data29$Count)
    
    # Plot as heatmap
    p <- ggplot(count_data29, aes(x = Emp_Status, y = Anx_Freq, fill = Count)) +
      geom_tile() +
      scale_fill_gradient2(low = "lightsalmon", mid = "indianred1", high = "brown4", midpoint = median(count_data29$Count)) +  # Adjust color gradient for more sensitivity
      labs(title = "Frequency of Anxiety by Employment",
           x = "Employmnet Status", y = "Frequency") +
      theme_minimal()
    
    # Convert ggplot to plotly
    ggplotly(p)
  })
  
  output$anxiety_level_empstatus_Plot <- renderPlotly({
    filtered_data30 <- subset(Data, Year == input$yearInput1)
    
    # Remove rows with missing values
    filtered_data30 <- na.omit(filtered_data30)
    
    count_data30 <- table(as.factor(filtered_data30$Emp_Status), filtered_data30$Anx_Level)
    
    # Convert table to dataframe
    count_data30 <- as.data.frame(count_data30)
    names(count_data30) <- c("Emp_Status", "Anx_Level", "Count")
    
    # Define bubble size based on count
    count_data30$Size <- sqrt(count_data30$Count)
    
    # Plot as heatmap
    p <- ggplot(count_data30, aes(x = Emp_Status, y = Anx_Level, fill = Count)) +
      geom_tile() +
      scale_fill_gradient2(low = "lightyellow", mid = "orange", high = "brown", midpoint = median(count_data30$Count)) +  # Adjust color gradient for more sensitivity
      labs(title = "Intensity of Anxiety by Employment",
           x = "Employmnet Status", y = "Intensity") +
      theme_minimal()
    
    # Convert ggplot to plotly
    ggplotly(p)
  })
  
  output$anxiety_freq_maritial_Plot <- renderPlotly({
    filtered_data31 <- subset(Data, Year == input$yearInput2)
    
    # Remove rows with missing values
    filtered_data31 <- na.omit(filtered_data31)
    
    count_data31 <- table(as.factor(filtered_data31$Maritial_Status), filtered_data31$Anx_Freq)
    
    # Convert table to dataframe
    count_data31 <- as.data.frame(count_data31)
    names(count_data31) <- c("Maritial_Status", "Anx_Freq", "Count")
    
    # Define bubble size based on count
    count_data31$Size <- sqrt(count_data31$Count)
    
    p <- ggplot(count_data31, aes(x = Maritial_Status, y = Anx_Freq, fill = Count)) +
      geom_tile() +
      scale_fill_gradient2(low = "lightsalmon", mid = "indianred1", high = "brown4", midpoint = median(count_data31$Count)) +  # Adjust color gradient for more sensitivity
      labs(title = "Frrequency of Anxiety by Maritial Status",
           x = "Maritial Status", y = "Frequency") +
      theme_minimal()
    
    # Convert ggplot to plotly
    ggplotly(p)
  })
  
  output$anxiety_level_maritial_Plot <- renderPlotly({
    filtered_data32 <- subset(Data, Year == input$yearInput2)
    
    # Remove rows with missing values
    filtered_data32 <- na.omit(filtered_data32)
    
    count_data32 <- table(as.factor(filtered_data32$Maritial_Status), filtered_data32$Anx_Level)
    
    # Convert table to dataframe
    count_data32 <- as.data.frame(count_data32)
    names(count_data32) <- c("Maritial_Status", "Anx_Level", "Count")
    
    # Define bubble size based on count
    count_data32$Size <- sqrt(count_data32$Count)
    
    p <- ggplot(count_data32, aes(x = Maritial_Status, y = Anx_Level, fill = Count)) +
      geom_tile() +
      scale_fill_gradient2(low = "lightyellow", mid = "orange", high = "brown", midpoint = median(count_data32$Count)) +  # Adjust color gradient for more sensitivity
      labs(title = "Intensity of Anxiety by Maritial Status",
           x = "Maritial Status", y = "Intensity") +
      theme_minimal()
    
    # Convert ggplot to plotly
    ggplotly(p)
  })
  
  output$anxietyfreqPlot <- renderPlotly({
    filtered_data3 <- subset(Data, Year == input$yearInput3 & Anx_Therapy == "Yes" & Anx_Meds == "No")
    
    # Remove rows with missing values
    filtered_data3 <- na.omit(filtered_data3)
    
    # Calculate count of each category within each age group
    count_data3 <- filtered_data3 %>%
      group_by(Age, Anx_Freq) %>%
      summarise(count = n()) %>%
      ungroup()
    
    # Calculate total count within each age group
    total_count3 <- count_data3 %>%
      group_by(Age) %>%
      summarise(total = sum(count)) %>%
      ungroup()
    
    # Calculate percentage within each age group
    count_data3 <- count_data3 %>%
      left_join(total_count3, by = "Age") %>%
      mutate(percentage = count / total * 100)
    
    # Plot
    p <- ggplot(count_data3, aes(x = Age, y = percentage, fill = Anx_Freq)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Frequency of Anxiety by Age",
           x = "Age", y = "Percentage") +
      scale_color_brewer(palette = "Set3")
    
    # Convert ggplot to plotly
    ggplotly(p)
  })
  
  output$anxietylevelPlot <- renderPlotly({
    filtered_data4 <- subset(Data, Year == input$yearInput3 & Anx_Therapy == "Yes" & Anx_Meds == "No")
    
    # Remove rows with missing values
    filtered_data4 <- na.omit(filtered_data4)
    
    # Calculate count of each category within each age group
    count_data4 <- filtered_data4 %>%
      group_by(Age, Anx_Level) %>%
      summarise(count = n()) %>%
      ungroup()
    
    # Calculate total count within each age group
    total_count4 <- count_data4 %>%
      group_by(Age) %>%
      summarise(total = sum(count)) %>%
      ungroup()
    
    # Calculate percentage within each age group
    count_data4 <- count_data4 %>%
      left_join(total_count4, by = "Age") %>%
      mutate(percentage = count / total * 100)
    
    # Plot
    p <- ggplot(count_data4, aes(x = Age, y = percentage, fill = Anx_Level)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Intensity of Anxiety by Age",
           x = "Age Group", y = "Percentage") +
      scale_color_brewer(palette = "Set2") 
    # Convert ggplot to plotly
    ggplotly(p)
  })
  
  output$anxietyfreq_gender_Plot <- renderPlotly({
    filtered_data5 <- subset(Data, Year == input$yearInput4 & Anx_Therapy == "Yes" & Anx_Meds == "No")
    
    # Remove rows with missing values
    filtered_data5 <- na.omit(filtered_data5)
    
    # Calculate count of each category within each gender group
    count_data5 <- filtered_data5 %>%
      group_by(Gender, Anx_Freq) %>%
      summarise(count = n()) %>%
      ungroup()
    
    # Calculate total count within each gender group
    total_count5 <- count_data5 %>%
      group_by(Gender) %>%
      summarise(total = sum(count)) %>%
      ungroup()
    
    # Calculate percentage within each gender group
    count_data5 <- count_data5 %>%
      left_join(total_count5, by = "Gender") %>%
      mutate(percentage = count / total * 100)
    # Plot
    p <- ggplot(count_data5, aes(x = Gender, y = percentage,  fill = Anx_Freq)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Frequency of Anxiety by Gender",
           x = "Gender", y = "Percentage") +
      scale_color_brewer(palette = "Set3") 
    
    # Convert ggplot to plotly
    ggplotly(p)
  })
  output$anxietylevel_gender_Plot <- renderPlotly({
    filtered_data6 <- subset(Data, Year == input$yearInput4 & Anx_Therapy == "Yes" & Anx_Meds == "No")
    
    # Remove rows with missing values
    filtered_data6 <- na.omit(filtered_data6)
    
    # Calculate count of each category within each gender group
    count_data6 <- filtered_data6 %>%
      group_by(Gender, Anx_Level) %>%
      summarise(count = n()) %>%
      ungroup()
    
    # Calculate total count within each gender group
    total_count6 <- count_data6 %>%
      group_by(Gender) %>%
      summarise(total = sum(count)) %>%
      ungroup()
    
    # Calculate percentage within each group group
    count_data6 <- count_data6 %>%
      left_join(total_count6, by = "Gender") %>%
      mutate(percentage = count / total * 100)
    
    # Plot
    p <- ggplot(count_data6, aes(x = Gender, y = percentage, fill = Anx_Level)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Intensity of Anxiety by Gender",
           x = "Gender", y = "Percentage") +
      scale_color_brewer(palette = "Set2") 
    
    # Convert ggplot to plotly
    ggplotly(p)
  })
  
  output$anxietyfreq_race_Plot <- renderPlotly({
    filtered_data7 <- subset(Data, Year == input$yearInput5 & Anx_Therapy == "Yes" & Anx_Meds == "No")
    
    # Remove rows with missing values
    filtered_data7 <- na.omit(filtered_data7)
    
    # Calculate count of each category within each race group
    count_data7 <- filtered_data7 %>%
      group_by(Race, Anx_Freq) %>%
      summarise(count = n()) %>%
      ungroup()
    
    # Calculate total count within each race group
    total_count7 <- count_data7 %>%
      group_by(Race) %>%
      summarise(total = sum(count)) %>%
      ungroup()
    
    # Calculate percentage within each race group
    count_data7 <- count_data7 %>%
      left_join(total_count7, by = "Race") %>%
      mutate(percentage = count / total * 100)
    
    # Plot
    p <- ggplot(count_data7, aes(x = Race, y = percentage, fill = Anx_Freq)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Frequency of Anxiety by Race",
           x = "Race", y = "Percentage") +
      scale_color_brewer(palette = "Set3")
    
    # Convert ggplot to plotly
    ggplotly(p)
  })
  output$anxietylevel_race_Plot <- renderPlotly({
    filtered_data8 <- subset(Data, Year == input$yearInput5 & Anx_Therapy == "Yes" & Anx_Meds == "No")
    
    # Remove rows with missing values
    filtered_data8 <- na.omit(filtered_data8)
    
    # Calculate count of each category within each race group
    count_data8 <- filtered_data8 %>%
      group_by(Race, Anx_Level) %>%
      summarise(count = n()) %>%
      ungroup()
    
    # Calculate total count within each race group
    total_count8 <- count_data8 %>%
      group_by(Race) %>%
      summarise(total = sum(count)) %>%
      ungroup()
    
    # Calculate percentage within each race group
    count_data8 <- count_data8 %>%
      left_join(total_count8, by = "Race") %>%
      mutate(percentage = count / total * 100)
    
    # Plot
    p <- ggplot(count_data8, aes(x = Race, y = percentage, fill = Anx_Level)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Intensity of Anxiety by Race",
           x = "Race", y = "Percentage") +
      scale_color_brewer(palette = "Set2") 
    
    # Convert ggplot to plotly
    ggplotly(p)
  })
  
  # Render line graph for frequency of anxiety by age group
  output$anxietyfreq1Plot <- renderPlotly({
    filtered_data9 <- subset(Data, Year == input$yearInput6 & Anx_Therapy == "No" & Anx_Meds == "Yes")
    
    # Remove rows with missing values
    filtered_data9 <- na.omit(filtered_data9)
    
    # Calculate count for each level of Anx_Freq
    count_data9 <- count(filtered_data9, Age, Anx_Freq) %>% 
      mutate(Percentage = prop.table(n) * 100)
    
    # Plot
    p <- ggplot(count_data9, aes(x = Age, y = Percentage, group = Anx_Freq, color = Anx_Freq)) +
      geom_line() +
      labs(title = "Frequency of Anxiety by Age",
           x = "Age", y = "Percentage") +
      scale_color_brewer(palette = "Set3") 
    
    # Convert ggplot to plotly
    ggplotly(p)
  })
  
  # Render line graph for intensity of anxiety by age group
  output$anxietylevel1Plot <- renderPlotly({
    filtered_data10 <- subset(Data, Year == input$yearInput6 & Anx_Therapy == "No" & Anx_Meds == "Yes")
    
    # Remove rows with missing values
    filtered_data10 <- na.omit(filtered_data10)
    
    # Calculate count for each level of Anx_Level
    count_data10 <- count(filtered_data10, Age, Anx_Level) %>% 
      mutate(Percentage = prop.table(n) * 100)
    
    # Plot
    p <- ggplot(count_data10, aes(x = Age, y = Percentage, group = Anx_Level, color = Anx_Level)) +
      geom_line() +
      labs(title = "Intensity of Anxiety by Age ",
           x = "Age", y = "Percentage") +
      scale_color_brewer(palette = "Set2") 
    # Convert ggplot to plotly
    ggplotly(p)
  })
  
  # Render line graph for frequency of anxiety by gender
  output$anxietyfreq_gender1_Plot <- renderPlotly({
    filtered_data11 <- subset(Data, Year == input$yearInput7 & Anx_Therapy == "No" & Anx_Meds == "Yes")
    
    # Remove rows with missing values
    filtered_data11 <- na.omit(filtered_data11)
    
    # Calculate count for each level of Anx_Freq
    count_data11 <- count(filtered_data11, Gender, Anx_Freq) %>% 
      mutate(Percentage = prop.table(n) * 100)
    
    # Plot
    p <- ggplot(count_data11, aes(x = Gender, y = Percentage, group = Anx_Freq, color = Anx_Freq)) +
      geom_line() +
      labs(title = "Frequency of Anxiety by Gender",
           x = "Gender" , y = "Percentage") +
      scale_color_brewer(palette = "Set3") 
    
    # Convert ggplot to plotly
    ggplotly(p)
  })
  
  # Render line graph for intensity of anxiety by gender
  output$anxietylevel_gender1_Plot <- renderPlotly({
    filtered_data12 <- subset(Data, Year == input$yearInput7 & Anx_Therapy == "No" & Anx_Meds == "Yes")
    
    # Remove rows with missing values
    filtered_data12 <- na.omit(filtered_data12)
    
    # Calculate count for each level of Anx_Level
    count_data12 <- count(filtered_data12, Gender, Anx_Level) %>% 
      mutate(Percentage = prop.table(n) * 100)
    
    # Plot
    p <- ggplot(count_data12, aes(x = Gender, y = Percentage, group = Anx_Level, color = Anx_Level)) +
      geom_line() +
      labs(title = "Intensity of Anxiety by Gender",
           x = "Gender", y = "Percentage") +
      scale_color_brewer(palette = "Set2") 
    
    # Convert ggplot to plotly
    ggplotly(p)
  })
  
  # Render line graph for frequency of anxiety by race
  output$anxietyfreq_race1_Plot <- renderPlotly({
    filtered_data13 <- subset(Data, Year == input$yearInput8 & Anx_Therapy == "No" & Anx_Meds == "Yes")
    
    # Remove rows with missing values
    filtered_data13 <- na.omit(filtered_data13)
    
    # Calculate count for each level of Anx_Freq
    count_data13 <- count(filtered_data13, Race, Anx_Freq) %>% 
      mutate(Percentage = prop.table(n) * 100)
    
    # Plot
    p <- ggplot(count_data13, aes(x = Race, y = Percentage, group = Anx_Freq, color = Anx_Freq)) +
      geom_line() +
      labs(title = "Frequency of Anxiety by Race",
           x = "Race", y = "Percentage") +
      scale_color_brewer(palette = "Set3") 
    
    # Convert ggplot to plotly
    ggplotly(p)
  })
  
  # Render line graph for intensity of anxiety by race
  output$anxietylevel_race1_Plot <- renderPlotly({
    filtered_data14 <- subset(Data, Year == input$yearInput8 & Anx_Therapy == "No" & Anx_Meds == "Yes")
    
    # Remove rows with missing values
    filtered_data14 <- na.omit(filtered_data14)
    
    # Calculate count for each level of Anx_Level
    count_data14 <- count(filtered_data14, Race, Anx_Level) %>% 
      mutate(Percentage = prop.table(n) * 100)
    
    # Plot
    p <- ggplot(count_data14, aes(x = Race, y = Percentage, group = Anx_Level, color = Anx_Level)) +
      geom_line() +
      labs(title = "Intensity of Anxiety by Race",
           x = "Race", y = "Percentage") +
      scale_color_brewer(palette = "Set2") 
    
    # Convert ggplot to plotly
    ggplotly(p)
  })
  
  # Render scatter plot for frequency of anxiety by age group
  output$anxietyfreq2Plot <- renderPlotly({
    filtered_data15 <- subset(Data, Year == input$yearInput9 & Anx_Therapy == "Yes" & Anx_Meds == "Yes")
    
    # Remove rows with missing values
    filtered_data15 <- na.omit(filtered_data15)
    
    # Calculate count for each level of Anx_Freq
    count_data15 <- table(as.factor(filtered_data15$Age), filtered_data15$Anx_Freq)
    
    # Convert table to dataframe
    count_data15 <- as.data.frame(count_data15)
    names(count_data15) <- c("Age", "Anx_Freq", "Count")
    
    # Plot
    p <- ggplot(count_data15, aes(x = Age, y = Count, color = Anx_Freq)) +
      geom_point(size = 3) +
      labs(title = "Frequency of Anxiety by Age",
           x = "Age", y = "Count") +
      scale_color_brewer(palette = "Set3") 
    # Convert ggplot to plotly
    ggplotly(p)
  })
  
  # Render scatter plot for intensity of anxiety by age group
  output$anxietylevel2Plot <- renderPlotly({
    filtered_data16 <- subset(Data, Year == input$yearInput9 & Anx_Therapy == "Yes" & Anx_Meds == "Yes")
    
    # Remove rows with missing values
    filtered_data16 <- na.omit(filtered_data16)
    
    # Calculate count for each level of Anx_Level
    count_data16 <- table(as.factor(filtered_data16$Age), filtered_data16$Anx_Level)
    
    # Convert table to dataframe
    count_data16 <- as.data.frame(count_data16)
    names(count_data16) <- c("Age", "Anx_Level", "Count")
    
    # Plot
    p <- ggplot(count_data16, aes(x = Age, y = Count, color = Anx_Level)) +
      geom_point(size = 3) +
      labs(title = "Intensity of Anxiety by Age",
           x = "Age", y = "Count") +
      scale_color_brewer(palette = "Set2") 
    # Convert ggplot to plotly
    ggplotly(p)
  })
  
  # Render scatter plot for frequency of anxiety by gender
  output$anxietyfreq_gender2_Plot <- renderPlotly({
    filtered_data17 <- subset(Data, Year == input$yearInput10 & Anx_Therapy == "Yes" & Anx_Meds == "Yes")
    
    # Remove rows with missing values
    filtered_data17 <- na.omit(filtered_data17)
    
    # Calculate count for each level of Anx_Freq
    count_data17 <- table(as.factor(filtered_data17$Gender), filtered_data17$Anx_Freq)
    
    # Convert table to dataframe
    count_data17 <- as.data.frame(count_data17)
    names(count_data17) <- c("Gender", "Anx_Freq", "Count")
    
    # Plot
    p <- ggplot(count_data17, aes(x = Gender, y = Count, color = Anx_Freq)) +
      geom_point(size = 3) +
      labs(title = "Frequency of Anxiety by Gender",
           x = "Gender", y = "Count") +
      scale_color_brewer(palette = "Set3")
    # Convert ggplot to plotly
    ggplotly(p)
  })
  
  # Render scatter plot for intensity of anxiety by gender
  output$anxietylevel_gender2_Plot <- renderPlotly({
    filtered_data18 <- subset(Data, Year == input$yearInput10 & Anx_Therapy == "Yes" & Anx_Meds == "Yes")
    
    # Remove rows with missing values
    filtered_data18 <- na.omit(filtered_data18)
    
    # Calculate count for each level of Anx_Level
    count_data18 <- table(as.factor(filtered_data18$Gender), filtered_data18$Anx_Level)
    
    # Convert table to dataframe
    count_data18 <- as.data.frame(count_data18)
    names(count_data18) <- c("Gender", "Anx_Level", "Count")
    
    # Plot
    p <- ggplot(count_data18, aes(x = Gender, y = Count, color = Anx_Level)) +
      geom_point(size = 3) +
      labs(title = "Intensity of Anxiety by Gender",
           x = "Gender", y = "Count") +
      scale_color_brewer(palette = "Set2") 
    # Convert ggplot to plotly
    ggplotly(p)
  })
  
  # Render scatter plot for frequency of anxiety by race
  output$anxietyfreq_race2_Plot <- renderPlotly({
    filtered_data19 <- subset(Data, Year == input$yearInput11 & Anx_Therapy == "Yes" & Anx_Meds == "Yes")
    
    # Remove rows with missing values
    filtered_data19 <- na.omit(filtered_data19)
    
    # Calculate count for each level of Anx_Freq
    count_data19 <- table(as.factor(filtered_data19$Race), filtered_data19$Anx_Freq)
    
    # Convert table to dataframe
    count_data19 <- as.data.frame(count_data19)
    names(count_data19) <- c("Race", "Anx_Freq", "Count")
    
    # Plot
    p <- ggplot(count_data19, aes(x = Race, y = Count, color = Anx_Freq)) +
      geom_point(size = 3) +
      labs(title = "Frequency of Anxiety by Race",
           x = "Race", y = "Count") +
      scale_color_brewer(palette = "Set3")
    
    # Convert ggplot to plotly
    ggplotly(p)
  })
  
  # Render scatter plot for intensity of anxiety by race
  output$anxietylevel_race2_Plot <- renderPlotly({
    filtered_data20 <- subset(Data, Year == input$yearInput11 & Anx_Therapy == "Yes" & Anx_Meds == "Yes")
    
    # Remove rows with missing values
    filtered_data20 <- na.omit(filtered_data20)
    
    # Calculate count for each level of Anx_Level
    count_data20 <- table(as.factor(filtered_data20$Race), filtered_data20$Anx_Level)
    
    # Convert table to dataframe
    count_data20 <- as.data.frame(count_data20)
    names(count_data20) <- c("Race", "Anx_Level", "Count")
    
    # Plot
    p <- ggplot(count_data20, aes(x = Race, y = Count, color = Anx_Level)) +
      geom_point(size = 3) +
      labs(title = "Intensity of Anxiety by Race",
           x = "Race", y = "Count") +
      scale_color_brewer(palette = "Set2") 
    
    # Convert ggplot to plotly
    ggplotly(p)
  })
  
  output$anxietyfreq3Plot <- renderPlotly({
    filtered_data21 <- subset(Data, Year == input$yearInput12 & Anx_Therapy == "No" & Anx_Meds == "No")
    
    # Remove rows with missing values
    filtered_data21 <- na.omit(filtered_data21)
    
    # Calculate count for each level of Anx_Freq
    count_data21 <- table(as.factor(filtered_data21$Age), filtered_data21$Anx_Freq)
    
    # Convert table to dataframe
    count_data21 <- as.data.frame(count_data21)
    names(count_data21) <- c("Age", "Anx_Freq", "Count")
    
    # Define bubble size based on count
    count_data21$Size <- sqrt(count_data21$Count)
    
    # Plot
    p <- ggplot(count_data21, aes(x = Age, y = Anx_Freq, size = Size, color = Anx_Freq)) +
      geom_point() +
      scale_size_continuous(range = c(3, 10)) +  # Adjust bubble size range as needed
      labs(title = "Frequency of Anxiety by Age",
           x = "Age", y = "Frequency") +
      scale_color_brewer(palette = "Set3") 
    
    # Convert ggplot to plotly
    ggplotly(p)
  })
  
  # Render bubble chart for intensity of anxiety by age
  output$anxietylevel3Plot <- renderPlotly({
    filtered_data22 <- subset(Data, Year == input$yearInput12 & Anx_Therapy == "No" & Anx_Meds == "No")
    
    # Remove rows with missing values
    filtered_data22 <- na.omit(filtered_data22)
    
    # Calculate count for each level of Anx_Level
    count_data22 <- table(as.factor(filtered_data22$Age), filtered_data22$Anx_Level)
    
    # Convert table to dataframe
    count_data22 <- as.data.frame(count_data22)
    names(count_data22) <- c("Age", "Anx_Level", "Count")
    
    # Define bubble size based on count
    count_data22$Size <- sqrt(count_data22$Count)
    
    # Plot
    p <- ggplot(count_data22, aes(x = Age, y = Anx_Level, size = Size, color = Anx_Level)) +
      geom_point() +
      scale_size_continuous(range = c(3, 10)) +  # Adjust bubble size range as needed
      labs(title = "Intensity of Anxiety by Age",
           x = "Age", y = "Intensity") +
      scale_color_brewer(palette = "Set2") 
    # Convert ggplot to plotly
    ggplotly(p)
  })
  
  output$anxietyfreq_gender3_Plot <- renderPlotly({
    filtered_data23 <- subset(Data, Year == input$yearInput13 & Anx_Therapy == "No" & Anx_Meds == "No")
    
    # Remove rows with missing values
    filtered_data23 <- na.omit(filtered_data23)
    
    # Calculate count for each level of Anx_Freq
    count_data23 <- table(as.factor(filtered_data23$Gender), filtered_data23$Anx_Freq)
    
    # Convert table to dataframe
    count_data23 <- as.data.frame(count_data23)
    names(count_data23) <- c("Gender", "Anx_Freq", "Count")
    
    # Define bubble size based on count
    count_data23$Size <- sqrt(count_data23$Count)
    
    # Plot
    p <- ggplot(count_data23, aes(x = Gender, y = Anx_Freq, size = Size, color = Anx_Freq)) +
      geom_point() +
      scale_size_continuous(range = c(3, 10)) +  # Adjust bubble size range as needed
      labs(title = "Frequency of Anxiety by Gender",
           x = "Gender", y = "Frequency") +
      scale_color_brewer(palette = "Set3") 
    
    # Convert ggplot to plotly
    ggplotly(p)
  })
  
  
  # Render bubble chart for intensity of anxiety by gender
  output$anxietylevel_gender3_Plot <- renderPlotly({
    filtered_data24 <- subset(Data, Year == input$yearInput13 & Anx_Therapy == "No" & Anx_Meds == "No")
    
    # Remove rows with missing values
    filtered_data24 <- na.omit(filtered_data24)
    
    # Calculate count for each level of Anx_Level
    count_data24 <- table(as.factor(filtered_data24$Gender), filtered_data24$Anx_Level)
    
    # Convert table to dataframe
    count_data24 <- as.data.frame(count_data24)
    names(count_data24) <- c("Gender", "Anx_Level", "Count")
    
    # Define bubble size based on count
    count_data24$Size <- sqrt(count_data24$Count)
    
    # Plot
    p <- ggplot(count_data24, aes(x = Gender, y = Anx_Level, size = Size, color = Anx_Level)) +
      geom_point() +
      scale_size_continuous(range = c(3, 10)) +  # Adjust bubble size range as needed
      labs(title = "Intensity of Anxiety by Gender",
           x = "Gender", y = "Intensity") +
      scale_color_brewer(palette = "Set2") 
    
    # Convert ggplot to plotly
    ggplotly(p)
  })
  
  output$anxietyfreq_race3_Plot <- renderPlotly({
    filtered_data25 <- subset(Data, Year == input$yearInput14 & Anx_Therapy == "No" & Anx_Meds == "No")
    
    # Remove rows with missing values
    filtered_data25 <- na.omit(filtered_data25)
    
    # Calculate count for each level of Anx_Freq
    count_data25 <- table(as.factor(filtered_data25$Race), filtered_data25$Anx_Freq)
    
    # Convert table to dataframe
    count_data25 <- as.data.frame(count_data25)
    names(count_data25) <- c("Race", "Anx_Freq", "Count")
    
    # Define bubble size based on count
    count_data25$Size <- sqrt(count_data25$Count)
    
    # Plot
    p <- ggplot(count_data25, aes(x = Race, y = Anx_Freq, size = Size, color = Anx_Freq)) +
      geom_point() +
      scale_size_continuous(range = c(3, 10)) +  # Adjust bubble size range as needed
      labs(title = "Frequency of Anxiety by Race",
           x = "Race", y = "Frequency") +
      scale_color_brewer(palette = "Set3") 
    
    # Convert ggplot to plotly
    ggplotly(p)
  })
  
  # Render bubble chart for intensity of anxiety by race
  output$anxietylevel_race3_Plot <- renderPlotly({
    filtered_data26 <- subset(Data, Year == input$yearInput14 & Anx_Therapy == "No" & Anx_Meds == "No")
    
    # Remove rows with missing values
    filtered_data26 <- na.omit(filtered_data26)
    
    # Calculate count for each level of Anx_Level
    count_data26 <- table(as.factor(filtered_data26$Race), filtered_data26$Anx_Level)
    
    # Convert table to dataframe
    count_data26 <- as.data.frame(count_data26)
    names(count_data26) <- c("Race", "Anx_Level", "Count")
    
    # Define bubble size based on count
    count_data26$Size <- sqrt(count_data26$Count)
    
    # Plot
    p <- ggplot(count_data26, aes(x = Race, y = Anx_Level, size = Size, color = Anx_Level)) +
      geom_point() +
      scale_size_continuous(range = c(3, 10)) +  # Adjust bubble size range as needed
      labs(title = "Intensity of Anxiety by Race",
           x = "Race", y = "Intensity") +
      scale_color_brewer(palette = "Set2") 
    
    # Convert ggplot to plotly
    ggplotly(p)
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
