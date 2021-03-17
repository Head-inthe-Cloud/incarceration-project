library(tidyverse)
library(shiny)
library(ggplot2)
# Data input
source("analysis.r")


# Server
server <- function(input, output) {
  #Intro
  
  url_1 <- a("Data Source (Incarceration Data)", href="https://catalog.data.gov/dataset/criminal-justice-data-book")
  output$inc <- renderUI({
    tagList("Incarceration Data Link:", url_1)
  })
  
  url_2 <- a("Data Source (Income Data)", href="https://www.kaggle.com/goldenoakresearch/us-household-income-stats-geo-locations?select=kaggle_income.csv")
  output$income <- renderUI({
    tagList("Income Data Link:", url_2)
  })
  
  url_3 <- a("gh Pages", href="https://info201b-wi21.github.io/project-Incarceration-LabBB-4/index.html")
  output$ghpages <- renderUI({
    tagList("Exploratory Report:", url_3)
  })
  
  #Summary
  output$box<- renderPlot({
    summary_plot <- ggplot(data = modified_income_df,aes(x=.data[[input$value_choice]])) + geom_boxplot(outlier.colour="black", outlier.shape=16,
                                                                                                        outlier.size=2, notch=FALSE) +
      theme(plot.title = element_text(size = 20, face = "bold"),
            axis.text.x = element_text(size = 15, vjust=0.5),
            axis.text.y = element_text(size = 15, vjust=0.5),
            axis.title = element_text(size = 15, vjust=0.5, face = "bold"))
    
    summary_plot
  })
  output$sum <- renderText({
    ret <- paste("This box graph represents the distribution of", paste(input$value_choice,"s",sep=''),"found in our income database.")
    ret <- paste(ret,"Median of")
    referenced_col <- modified_income_df[[input$value_choice]]
    ret <- paste(ret, input$value_choice,"was", paste(round(median(referenced_col), digits = 2),'.', sep = ''))
    quartiles = quantile(referenced_col,type=3)
    third_quartile <- quartiles[4]
    first_quartile <- quartiles[2]
    ret <- paste(ret, "The third quartile of the data is", paste(third_quartile,",",sep=""), "the first quartile is", paste(first_quartile, '.', sep = ''))
    iqr <- third_quartile-first_quartile
    ret <- paste(ret, "The IQR of the data is conseuqently",paste(iqr, '.',sep=''))
    min <- round(which.min(abs(referenced_col-(first_quartile - 1.5 * iqr))), digits = 2)
    max <- round(which.min(abs(referenced_col-(third_quartile + 1.5 * iqr))), digits = 2)
    ret <- paste(ret, "Due to the IQR, the minimum of the data is", referenced_col[min], "and the maximum of the data is", paste(referenced_col[max], '.\n\n',sep=''))
    ret <- paste(ret,"The standard deviation of the values is", round(sd(referenced_col),digits = 2)," and the mean was", paste(round(mean(referenced_col),digits = 2),'.',sep=''))
    num_outliers <- modified_income_df %>% filter(modified_income_df[[input$value_choice]] > referenced_col[max]) %>% select(input$value_choice) %>% nrow()
    num_outliers <- num_outliers + modified_income_df %>% filter(modified_income_df[[input$value_choice]] < referenced_col[min]) %>% select(input$value_choice) %>% nrow()
    
    if (num_outliers == 1){
      ret <- paste(ret, "There was one outlier in the data set.")
    }
    else
    {
      ret <- paste(ret, "There were",num_outliers, "outliers in the data set.")
    }
    return(ret)
  })
  output$norm <- renderPlot({
    summary_plot_norm <- ggplot(modified_income_df, aes(x=.data[[input$value_choice]])) +
                          geom_histogram(binwidth=5000, colour="black", fill="white") +
      geom_density() + # Overlay with transparent density plot
      geom_vline(aes(xintercept=mean(.data[[input$value_choice]], na.rm=T)),   # Ignore NA values for mean
                 color="red", linetype="dashed", size=1) +
      theme(plot.title = element_text(size = 20, face = "bold"),
            axis.text.x = element_text(size = 15, vjust=0.5),
            axis.text.y = element_text(size = 15, vjust=0.5),
            axis.title = element_text(size = 15, vjust=0.5, face = "bold"))
    
    summary_plot_norm
  })
  output$dev <- renderText({
    ret <- "We decided to use median household incomes for most of our analysis as medians are less suscpetible to fluctuation and outliers. 
            As you can tell from the standard deviations of income across counties, there is a large range of standard deviations, some counties,
            such as King County, have a large deviation of incomes, which could severely affect its mean. As a result, to keep these deviations 
            from affecting our analysis, we decided to focus on medians, rather than a county's mean income."
    ret
  })
  #Q1
  
    output$income_sentence_time_plot <- renderPlot({
    
      time_inc_df <- time_inc_df %>%
        na.omit() %>% 
        top_n(n = as.numeric(input$choice), wt = APS_AVG_SENT)
      
      income_sentence_time_plot <- ggplot(time_inc_df, aes(x = APS_AVG_SENT, y = Median)) + 
        geom_point(color = "firebrick", size = 2.5) +
        geom_smooth(method="lm", color = "Darkgreen", se=F) +
        labs(
          title = "Median Incomes vs Average Sentence Time in Counties",
          x = "Average Sentence Time in Years",
          y = "Median Income"
        ) +
        theme_classic()+
        theme(plot.title = element_text(size = 18, face = "bold"),
              axis.text.x = element_text(size = 15, vjust=0.5),
              axis.text.y = element_text(size = 15, vjust=0.5),
              axis.title = element_text(size = 15, vjust=0.5, face = "bold"))

    
    return(income_sentence_time_plot)
  })
    
  output$time_table <- renderDataTable({
      
    time_table <- time_inc_df %>%
      na.omit() %>% 
      top_n(n = as.numeric(input$choice), wt = APS_AVG_SENT)
      
      time_table[, "county"] <- str_to_title(time_table$county)
      
      return(time_table)
  })
  
  output$correlation <- renderText({
    
    time_inc_df <- time_inc_df %>%
      na.omit() %>% 
      top_n(n = as.numeric(input$choice), wt = APS_AVG_SENT)
    
    correlation <- cor(time_inc_df$APS_AVG_SENT,
                       time_inc_df$Median)
    
    correlation <- paste("Correlation Coefficient:", correlation)
    
    return(correlation)
  })
  
  #Q2
  
  output$arrest_prison_plot <- renderPlot({
    
    if(!input$checkbox_outlier){
      arrest_vs_prison_data <- arrest_vs_prison_data %>%
        filter(county != "ferry")
    }
    
    arrest_vs_prison_data <- arrest_vs_prison_data %>%
      filter(income_median >= input$income_slider2[1] & 
               income_median <= input$income_slider2[2])
    
    
    if(input$checkbox_stddev){
      arrest_prison_by_median_plot <- ggplot(data = arrest_vs_prison_data,
                                             mapping = aes(x = income_median, 
                                                           y = prison_to_arrest_rate)) +
        geom_point(color = "firebrick", aes(size = income_sd)) +
        # scale_color_distiller(palette = "YlOrRd", direction = 1) +
        geom_smooth(method="lm", color = "Darkgreen", se=F) +
        labs(
          title = "WA county prison to arrest rate by income median",
          x = "Income Median (USD)",
          y = "Prison-Arrest Rate"
        ) +
        theme_classic()+
        theme(plot.title = element_text(size = 20, face = "bold"),
              axis.text.x = element_text(size = 15, vjust=0.5),
              axis.text.y = element_text(size = 15, vjust=0.5),
              axis.title = element_text(size = 15, vjust=0.5, face = "bold"))
    } else {
      arrest_prison_by_median_plot <- ggplot(data = arrest_vs_prison_data,
                                             mapping = aes(x = income_median, 
                                                           y = prison_to_arrest_rate)) +
        geom_point(color = "firebrick", size = 3) +
        geom_smooth(method="lm", color = "Darkgreen", se=F) +
        labs(
          title = "WA county prison to arrest rate by income median",
          x = "Income Median (USD)",
          y = "Prison-Arrest Rate"
        ) +
        theme_classic()+
        theme(plot.title = element_text(size = 20, face = "bold"),
              axis.text.x = element_text(size = 15, vjust=0.5),
              axis.text.y = element_text(size = 15, vjust=0.5),
              axis.title = element_text(size = 15, vjust=0.5, face = "bold"))
    }
  
    
    
    return(arrest_prison_by_median_plot)
  })
   
  output$arrest_prison_table <- renderDataTable({
    
    if(!input$checkbox_outlier){
      arrest_vs_prison_data <- arrest_vs_prison_data %>%
        filter(county != "ferry")
    }
    
    arrest_vs_prison_data <- arrest_vs_prison_data %>%
      filter(income_median >= input$income_slider2[1] & 
               income_median <= input$income_slider2[2])
    
    arrest_prison_table <- arrest_vs_prison_data %>% 
      select(county, prison_to_arrest_rate, income_median, income_sd) %>%
      arrange(desc(prison_to_arrest_rate))
    
    arrest_prison_table[, "county"] <- str_to_title(arrest_prison_table$county)
    
    return(arrest_prison_table)
  })
  
  output$cor <- renderText({
    if(!input$checkbox_outlier){
      arrest_vs_prison_data <- arrest_vs_prison_data %>%
        filter(county != "ferry")
    }
    arrest_vs_prison_data <- arrest_vs_prison_data %>%
      filter(income_median >= input$income_slider2[1] & 
               income_median <= input$income_slider2[2])
    
    correlation <- cor(arrest_vs_prison_data$prison_to_arrest_rate,
                       arrest_vs_prison_data$income_median)
    
    correlation <- paste("Correlation Coefficient:", correlation)
    
    return(correlation)
  })
  
  #Q3

  output$income_crime_proportion_plot <- renderPlot({
    
    county_crime_proportion <- county_crime_proportion %>%
      select(County, Median, Stdev, crime_type = input$crime_choice) %>%
      filter(Median < 300000)
    
    income_crime_proportion_plot <- ggplot(county_crime_proportion, aes(x = Median, y = crime_type)) + 
      geom_point(color = "firebrick", aes(size = Stdev)) +
      labs(
        title = "Median Incomes vs Proportion of Arrests in Counties",
        x = "Median Income",
        y = "Proportion of Arrests"
      ) +
      theme_classic() +
        theme(plot.title = element_text(size = 20, face = "bold"),
              axis.text.x = element_text(size = 15, vjust=0.5),
              axis.text.y = element_text(size = 15, vjust=0.5),
              axis.title = element_text(size = 15, vjust=0.5, face = "bold"))
    
    return(income_crime_proportion_plot)
  })
  
  output$cor_q3 <- renderText({
    county_crime_proportion <- county_crime_proportion %>%
      select(County, Median, crime_type = input$crime_choice)
    
    correlation <- cor(county_crime_proportion$Median,
                       county_crime_proportion$crime_type)
    
    correlation <- paste("Correlation Coefficient:", correlation)
    
    return(correlation)
  })
  
}