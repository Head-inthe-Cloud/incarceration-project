# (Project domain)  --- Incarceration rate
Authors: Patrick Cheng, Erica Gordy, William Siauw, Joel Yang
Group number: BB-4

## (problem domain)
 Our group is choosing to focus on Incarceration in the United States, specifically we will be focusing on economic status and geographic location as they relate to incarceration rate. In the United States, on average, every 1 in 100 adults are incarcerated (Bertram). The United States incarcerates the most people world wide, with 1 in every 5 people incarcerated around the world being in the US. Given that the US incarceration rate is so high, it can be imagined that there might be certain factors contributing to this.
	We will work to analyze data sets that pertain to incarceration rates in different socio-economic areas, as well as looking at crimes in cities across the US. A 2015 report found that people ages 27-42 had a median annual income of $19,185 prior to incarceration, a figure that is 41 percent less than non-incarcerated people of a similar age. Not many studies have been done regarding this topic so it will be interesting to see what our research into these data sets will uncover. We are also interested in looking at the particular crimes committed in geographic locations and see how they compare to different income communities. For example, how do affluent neighborhood crimes compare to crimes committed in low income communities? We are interested to see the crimes committed in different cities and look at how they vary across the United States. This will give us a deeper understanding of incarceration in the US and how different communities may be affected by it.

https://www.prisonpolicy.org/blog/2020/01/16/percent-incarcerated/

https://prosperitynow.org/blog/new-pipeline-poverty-prison

## (analysis questions)
1. Are people of different economic status punished by law differently?
    - The law determines a range for the sentence time depending on the crime as well as the criminal history of the accused. However, it is also influenced by the prosecutor, defense attorneys and judges. We believe the length of prison sentence time, which will be analyzed in our datasets, could reflect the degree of punishment and its relation to race.
2. How are the rates of people arrested and the rates of people who actually are sentected and go to prison affected by economic status? How do they compare to each other?
   - Arrest does not always lead to incarceration depending on the crime as well as the judgement of the police officer and that of the law system. We believe this could be a factor that leads to the high proportion of African Americans in the incarcerated population. The amount of money one has, or their race, can affect if they actually go to prison. For example, the ability to pay bail.
3. How does the economic status of residents in a given city correlate with the incarceration rate?
   - If any of these questions directly connect to our domain, this is the one. We are focusing on these two variables because while we are well aware that minorities disproportionately make up a large percentage of the incarcerated, we would also like to see how class plays such a role, as not much research has been done on those two specific factors.
4. How does the type of crime committed relate to the geographic location of the crime and the economic status of the area?
   - We do not mean to imply that a certain crime tend to happen in certain areas, but we would like to highlight the difference in blue-collar vs. white-collar crimes so that we can better make the connection to our second question, which discusses bail.


## (data sets)

Your project will need to analyze at least two different data of your choosing related to your problem domain. The project will analyze a "mash-up" of these data sets, considering the relationship between them. While your final project will only need to make use of two data sets, for this proposal you should identify at least three potential data sets (in case one ends up not working out well)!

- The two data sets need to be different from each other. This means that they have different features-"traffic in 2016" and "traffic in 2017" are not different data sets (even if they are in different files).

However, the two data sets should be related so that they can be considered together in some way-your analysis will need to consider (in part) a single combined ("joined") set of data For example, perhaps you want to consider the relationship between Uber Rates and Taxi Rates (to see the impacts of worker exploitation); these would be different data sets but within a related domain. Look for data sets that are about similar or related topics.

**Pro tip: while not required, it's easy to mash up data sets if they both have geographic features (such as cities, states, countries, or lat/lng coordinates)-which also lends itself to cool interactive map visualizations! Time series data (e.g., yearly data) also make for easy combining.**

- Your data sets do not need to be "Big Data", but should be of sufficient size to do some interesting analysis. **When combined, we expect your analyzed data to have at least 100 observations and at least 5 unique features.** Please check with your instructor or TA if you're unsure about whether your data sets are of appropriate scope!

GitHub cannot handle data files of more than 50mb. If you want to use something larger, you'll need to manually break it up into multiple files or find a different place to "store" it and access it from.

- Your data can be loaded from a .csv file (or more than 1!), a web API (or more than 1!), or some other data source (or more than 1!). If you use a web API, be careful that you can actually get sufficient data from it with a small number of queries; we do not use for-loops in R!

Beyond the above requirements, the choice of data is entirely up to you.

Your proposal must include a list of at least three (3) potential data sets for your project (though you'll only end up needing to use 2 of those 3), as a separate section from your problem domain description and analysis questions. For each data set, you must include the following information:

- What is this a data set? "Homeless rates in Seattle by month" is a fine description
- Where did you download the data set (what is the web URL that we can follow to access it ourselves)?
- How was the data collected or generated? Be sure to identify who collected the data (which are not necessarily the same people that host the data)?
- Which of your analysis questions will this data set support you in answering?

[Crime and Incarceration in the US](https://www.kaggle.com/christophercorrea/prisoners-and-crime-in-united-states?select=ucr_by_state.csv)

- This data set is a general national overview of crime & incarceration
- This was found on Kaggle, link is above
- This data was collected by the Bureau of Justice
- This set will mainly help us in answering analysis question 4.

[Crime in Los Angeles](https://www.kaggle.com/cityofLA/crime-in-los-angeles?select=Crime_Data_2010_2017.csv)

- This data set is a overview of crime in LA from 2010 to 2017
- This was found on Kaggle, link is above
- This data was collected by the city of LA
- This set will mainly help us in answering analysis question 4.

[Crime in Boston](https://www.kaggle.com/AnalyzeBoston/crimes-in-boston?select=crime.csv)

- This data set is an overview of crime in Boston, MA
- This was found on Kaggle, link is above
- This data was collected by Analyze Boston
- This set will mainly help us in answering analysis question 4.

[US Household Income Statistics](https://www.kaggle.com/goldenoakresearch/us-household-income-stats-geo-locations?select=kaggle_income.csv)

- This data set is an overview of income by city
- This was found on Kaggle, link is above
- This data was collected by Golden Oak Research group
- This set should help us answer all four questions.

**Additionally**, you will need to download each of the data sets, placing them in the provided data/ folder of your project repository. If your data is only available through a Web API rather than as a data file, you do not need to download it.


