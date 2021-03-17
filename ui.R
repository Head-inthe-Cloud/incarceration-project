source("global.R")

library(shinythemes)

# Pages


page_one <- tabPanel(
  title="Intro",
  h2("Data Introduction"),
  h4("Patrick Cheng, Erica Gordy, William Siauw, Joel Yang"),
  p(),
  p("Our group is choosing to focus on Incarceration in the State of Washington, 
  specifically we will be focusing on economic status and geographic location as 
  they relate to incarceration rate. In the United States, on average, every 1 in 100 
  adults are incarcerated (Bertram). The United States incarcerates the most people world wide, 
  with 1 in every 5 people incarcerated around the world being in the US. Given that the US 
  incarceration rate is so high, it can be imagined that there might be certain factors 
  contributing to this. We will work to analyze data sets that pertain to incarceration rates 
  in different socio-economic areas, as well as looking at crimes in counties across the Washington State. 
  A 2015 report found that “people ages 27-42 had a median annual income of $19,185 prior to 
  incarceration, a figure that is 41 percent less than non-incarcerated people of a similar 
  age”(Levere). Not many studies have been done regarding this topic so it will be 
  interesting to see what our research into these data sets will uncover. We are also interested 
  in looking at the particular crimes committed in geographic locations and see how they compare 
  to different income communities. For example, how do affluent counties' crimes compare to 
  crimes committed in low income counties? We are interested to see the crimes committed in 
  different counties and look at how they vary in the State of Washington. This will give us a deeper 
  understanding of incarceration in the Washington State and how different communities may be affected by it."),
  p(uiOutput("inc")),
  p(uiOutput("income")),
  p(uiOutput("ghpages")),
  
  
  h3("Statistics from Prison Policy Initiative"),
  img("Distribution of annual incomes for people in local jails", src="https://static.prisonpolicy.org/images/jail_nobail_income.png"),
  p(),
  p(),
  img("Lower income communities have higher probation rates", src="https://static.prisonpolicy.org/images/MA_probation_rates_by_income.png?v=1480949928")
  
  
)


page_two <- tabPanel(
  title="Summary Analysis",    
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "value_choice", 
                  h3("Data Selection"), 
                  choices = features,
                  selected = 1)
    ),
    
    mainPanel(
      tabPanel("Box", plotOutput("box")),
      p(textOutput(outputId = "sum")),
      tabPanel("Normal", plotOutput("norm")),
      p(textOutput(outputId = "dev"))
    )
  )
)

page_three <- tabPanel(
  title="Sentence Time",
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "choice", 
                  h3("Data Selection"), 
                  choices = list("highest 10 avg sentence time" = 10, 
                                 "lowest 10 avg sentence time" = -10),
                  selected = 1),
      p("The graph shows the relationship between sentence times and income in counties. The user has 
        the option to view either the top 10 highest average sentence times or the 10 lowest average 
        sentence times. The data table gives the county names, the average sentence time, and Median incomes."),
      p("The correlation coefficient changes when the user chooses each graph. For the lowest 10 averages
      sentence times the correlation coefficient is .38927. This indicates a positive relationship 
      between average sentence time and median income. For the highest 10 average sentence times the 
      correlation coefficient is -.0646 This indicates a slightly negative relationship between 
      average sentence time and median income.")
    ),
    mainPanel(
      plotOutput(outputId = "income_sentence_time_plot"),
      h4(textOutput(outputId = "correlation")),
      p(""),
      dataTableOutput(outputId = "time_table")
    )
  )
)
 
page_four <- tabPanel(
  title="Prison/Arrest Rate",
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "income_slider2", label = h3("Median Income"), min = 20000, max = 85000,
                  value = c(20000, 85000)),
      
      checkboxInput(inputId = "checkbox_outlier", label = h5("Include Outlier (Ferry County)"), value = FALSE),
      checkboxInput(inputId = "checkbox_stddev", label = h5("Show Standard Deviation"), value = FALSE),
      
      h4("How is the relationship between the rate of people getting arrested and the rate of people being sentenced to prison affected by economic status in the state of Washington"),
      p("When an individual is arrested, it doesn't always mean he or she is going to jail or prison because it depends on the crime committed, and if the individual has the financial ability to pay the fine or bail."),
      p("In this case, we expect counties with better economic status (estimated by income median) to have a smaller prison-arrest ratio -- the number of prison admissions compared to number of arrests -- because people in richer counties are more likely to have the ability to pay the fine or bail instead of serving their sentence time in prison."),
      p("The rate of people getting arrested is the number of total people arrested in a year divided by total population of the county. The rate of prison sentence is the number of total prison admission in a year divided by total population of the county."),
      p("The relationship between the arrest rate and prison sentence rate in a certain county is calculated by dividing the total number of prison admission in a year by total number of people arrested in that year. Then I compare the result to the median income of the county."),
      
      h4("Conclusion"),
      p("The correlation coefficient between prison-arrest rate and income median is -0.3678. It indicates a moderate negative relationship between the prison-arrest rate and the median income."),
      p("However, Ferry County appears to be an outlier with a exceptionally high prison-arrest rate, which skews the result to the negative side. After removing the outlier, the new correlation coefficient is -0.1621, which is less negative than the previous result. This means in a certain county, the higher the income median is, the less likely one is going to be sentenced to prison after being arrested.")
    ),
    
    mainPanel(
      plotOutput(outputId = "arrest_prison_plot"),
      h4(textOutput(outputId = "cor")),
      p(""),
      h3("Data table"),
      dataTableOutput(outputId = "arrest_prison_table")
    )
  )
)

page_five <- tabPanel(
  title="Analysing by Crime Types",
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "crime_choice", 
                  h3("Type of Arrest"), 
                  choices = list("Assault",
                                 "Burglary",
                                 "Destruction of Property",
                                 "Drug Violation",
                                 "Forced Sex",
                                 "Group B",
                                 "Theft",
                                 "Violation of No Contact",
                                 "Weapons Violation"
                                 ),
                  selected = 1),
      
      h4("How does the type of arrest made relate to the geographic location of the arrest and the economic status of the area?"),
      p("Not only did we want to focus on incarcerated populations, we wanted to see what factors led to them. Overpolicing and arrests in different classes of counties appeared to be a good lead. By \"economic status of the area\", we referred to the mean income of each county, which, in hindsight, may have inflated the incomes of richer counties. For the \"type of crime\", we used the crimes listed in the codebook, filtering out crimes that had a negligibly low proportion in comparison to other crimes."),
      p("However, as we found out later, each category of arrest was very broad, leading us to realize we didn't know what to expect in terms of the correlation between the class of an area and the \"type\" of that crime."),
      p("We created a plot for each relevant arrest type. The proportion of arrests is measured by the amount of the given types of arrests in each county divided by the total number of arrests for that county. The mean income is just that: the average income for each county."),
      
      h4("Conclusion"),
      p("As mentioned before, counties were ranked based on median income rather than mean income, which may be influcence by outliers. \"Group B violations\" were by far the most common crime for every county, but what exactly is a \"Group B violation\"? It turns out that category is extremely broad. According to the code book, Group B crimes include \"bad checks, curfew/loitering/vagrancy violations, disorderly conduct, driving under the influence, drunkenness, family offenses-nonviolent, liquor law violations, peeping tom, runaway, trespass of real property, all other offenses that are not Group A offenses\". Normally we would wipe this from the plot altogether, but the crimes that are included in this list are relevant to our ultimate question of overpolicing in certain areas, putting us stuck between a rock and a hard place."),
      p("If there is anything to take away from these plots, it would be that the arrest rates for theft appear to be consistently higher in lower income counties, but even that is overshadowed by the fact that theft covers everything from embezzlement to simple larceny. Ultimately, we believe not that these data are misinformative, but rather that they are ambiguous."),
    ),
    
    mainPanel(
      plotOutput(outputId = "income_crime_proportion_plot"),
      h4(textOutput(outputId = "cor_q3")),
    )
  )
)



# UI
ui <- navbarPage(
  theme = shinytheme("superhero"),
  "WA Incarceration and Income",
  page_one,
  page_two,
  page_three,
  page_four,
  page_five
)