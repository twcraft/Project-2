Project 2
================
Tiffany Craft
2023-10-11

- [Nobel Prize API Vignette](#nobel-prize-api-vignette)
- [Required Packages](#required-packages)
- [Functions to Contact API](#functions-to-contact-api)
  - [`getEnglish`](#getenglish)
  - [`prizes`](#prizes)
  - [`laureates`](#laureates)
- [Exploratory Data Analysis (EDA)](#exploratory-data-analysis-eda)
- [Wrap-Up](#wrap-up)

# Nobel Prize API Vignette

This document is a vignette to show how to read and summarizes data from
an API. I will be interacting with the [Nobel Prize
API](https://www.nobelprize.org/about/developer-zone-2/) and writing
functions to contact the API and then do a basic exploratory data
anaylsis with the data I pull.

# Required Packages

The following packages are required for running the functions I craeted
to interact with the API: - `httr`: a user-friendly interface for
interacting with web-based APIs - `jsonlite`: a simple and fast parser
for working with JSON in R - `tidyverse`: helps to transform and better
present data in R - `scales`: customizes axies and legend labels for
plots - `treemapify`: provides ggplot2 geoms for drawing treemaps -
`ggfittext`: provides ggplot2 geoms to fit text into a box

To get started, a user will need to install and load these packages like
so:

``` r
library(httr)
library(jsonlite)
library(tidyverse)
library(scales)
library(treemapify)
library(ggfittext)
```

# Functions to Contact API

The Nobel Prize API contains information about the Nobel Prizes awarded
beginning from 1901 to present day, as well as informatino about all the
Nobel Laureates. It is free to use and does not require an API key.

Below I describe the three functions I wrote to interact with and
organize the data retrieved from the API.

## `getEnglish`

I wrote a helper function called `getEnglish` to make the data returned
from my endpoint functions easier to filter. For several variables
returned by the API, columns included lists of the same data in three
different languages. I needed a way to search only through the English
data for matching user inputs.

``` r
getEnglish <- function(df, column){
  ###
  # This functions unnests a given column with data in multiple languages and then
  # replaces the column with a variable for only the English data.
  ###
  
  # Unnest a single column into separate columns
  df %>% unnest_wider(column, names_repair = "unique_quiet") %>%
    # remove the columns for languages other then English
    subset(select = -c(no, se)) %>%
    # rename the new English column with the original column name
    rename(!!sym(column) := en)
}
```

## `prizes`

I wrote a function called `prizes` to interact with the `Nobel Prizes`
endpoints of the Nobel Prize API. It returns a data frame with key
details about Nobel Prizes awarded in past years, including the year it
was awarded, the category of the award, the prize amount, and the names
of the laureates who received the award. The user can input two
variables to filter the data by the award category and/or the year the
prize was awarded.

``` r
prizes <- function(awardCategory = "all", year = "all"){
  ###
  # This functions returns a data frame with metadata on Nobel Prizes awarded in the past.
  # It can also return those columns for a certain category and/or year.
  ###
  
  # Get the award data from the Nobel Prizes endpoint.
  outputAPI <- fromJSON(
      "https://api.nobelprize.org/2.1/nobelPrizes?limit=1000&sort=asc"
      )
  
  # Select only the Nobel Prizes data from the JSON output and get only the English
  # data for category..
  output <- outputAPI$nobelPrizes %>%
    getEnglish("category") %>%
    getEnglish("categoryFullName")
  
  # If award category does not equal "all", check if it is a category name.
  if (awardCategory != "all"){
    
    # If award category is in the category column, subset output for just that row.
    if (awardCategory %in% output$category){
      output <- output %>%
        filter(category == awardCategory) %>%
        select(awardYear, category, categoryFullName, prizeAmount, laureates)
        
    }
    # Otherwise, throw an informative error.
    else {
      message <- paste("ERROR: Argument for award category was not found in the",
                       "category column. Try award category('all')",
                       "to find the category you're looking for.")
      stop(message)
    }
  }
  # Do nothing but return selected columns if the award category value equals "all".
  else {
    output <- output %>%
      select(awardYear, category, categoryFullName, prizeAmount, laureates)
  }
  
  # If year does not equal "all", check if it is an award year.
  if (year != "all"){
    
    # If year is in the award year column, subset output for just that row.
    if (year %in% output$awardYear){
      output <- output %>%
        filter(awardYear == year) %>%
        select(awardYear, category, categoryFullName, prizeAmount, laureates)
        
    }
    # Otherwise, throw an informative error.
    else {
      message <- paste("ERROR: Argument for year was not found in the",
                       "award year column. Try year('all') to",
                       "find the year you're looking for.")
      stop(message)
    }
  }
  # Do nothing but return selected columns if the year value equals "all".
  else {
    output <- output %>%
      select(awardYear, category, categoryFullName, prizeAmount, laureates)
  }
  
  # Return the output data frame.
  return(output)
}
```

## `laureates`

I wrote a function called `laureates` to interact with the `laureates`
endpoints of the Nobel Prize API. It returns a data frame with key
details about Nobel Prize winning laureates, including their name and ID
number, gender, year they won the award and the category they won in,
and their location at birth and now. The user can input two variables to
filter the data by the laureates’ country of birth or now and the
laureates’ gender.

``` r
laureates <- function(country = "all", laureateGender = "both"){
  ###
  # This functions returns a data frame with metadata on Nobel Prize winning laureates.
  # It can also return those columns for a certain country and/or gender.
  ###
  
  # Get the award data from the laureates endpoint.
  outputAPI <- fromJSON(
      "http://api.nobelprize.org/2.1/laureates?limit=1000"
      )
  
  # Select only the Nobel Prizes data from the JSON output and get only the English
  # data for category, location, and name variables.
  output <- outputAPI$laureates %>%
    unnest_wider(nobelPrizes, names_repair = "unique_quiet") %>%
    getEnglish("category") %>%
    unnest_wider(birth) %>%
    unnest_wider(place) %>%
    getEnglish("city") %>%
      rename("birthCity" = "city") %>%
    getEnglish("country") %>%
      rename("birthCountry" = "country") %>%
    getEnglish("cityNow") %>%
    getEnglish("countryNow") %>%
    getEnglish("continent") %>%
    getEnglish("locationString") %>%
    getEnglish("fullName") %>%
    mutate(awardYear = sapply(awardYear, toString)) %>%
    mutate(category = sapply(category, toString))

  # If country does not equal "all", check if it is a birth country or country now.
  if (country != "all"){
    
    # If country is in the birth country column, subset output for just that row.
    if (country %in% output$birthCountry){
      output <- output %>%
        filter(birthCountry == country) %>%
        select(id, fullName, gender, awardYear, category, birthCity, birthCountry,
               cityNow, countryNow, continent,locationString)
    }
    # If country is in the country now column, subset output for just that row.
    else if (country %in% output$countryNow){
      output <- output %>%
        filter(countryNow == country) %>%
        select(id, fullName, gender, awardYear, category, birthCity, birthCountry,
               cityNow, countryNow, continent,locationString)
    }
    # Otherwise, throw an informative error.
    else {
      message <- paste("ERROR: Argument for country was not found in either",
                       "the birth country or country now columns. Try country('all') to",
                       "find the country you're looking for.")
      stop(message)
    }
  }
  # Do nothing but return selected columns if the country value equals "all".
  else {
      output <- output %>%
        select(id, fullName, gender, awardYear, category, birthCity, birthCountry,
               cityNow, countryNow, continent,locationString)
  }
  
  # If laureate gender does not equal "both", check if it is a gender.
  if (laureateGender != "both"){
    
    # If laureate gender is in the gender column, subset output for just that row.
    if (laureateGender %in% output$gender){
      output <- output %>%
        filter(gender == laureateGender) %>%
        select(id, fullName, gender, awardYear, category, birthCity, birthCountry,
               cityNow, countryNow, continent,locationString)
    }
    # Otherwise, throw an informative error.
    else {
      message <- paste("ERROR: Argument for laureate's gender was not found in the",
                       "gender column. Try genders('both') to",
                       "find the gender you're looking for.")
      stop(message)
    }
  }
  # Do nothing but return selected columns if the laureate gender value equals "both".
  else {
      output <- output %>%
        select(id, fullName, gender, awardYear, category, birthCity, birthCountry,
               cityNow, countryNow, continent,locationString)
  }
  
  # Return the output data frame.
  return(output)
}
```

# Exploratory Data Analysis (EDA)

To begin my data analysis, I wanted to investigate the following
question: How does the number of Nobel Prize winners differ between male
and female laureates? For example, are there perhaps noticeable
differences between categories? Could there be a relationship between
the gender of laureates and the countries they are from?

I started by pulling the data on all the winning laureates by calling
`laureates()` and then created a one-way contingency table to compare
the total winners based on gender.

``` r
# Get data on all Nobel Prize laureates
winners <- laureates()

# Create a one-way contingency table of gender
table(winners$gender)
```

    ## 
    ## female   male 
    ##     64    901

As I expected, there have been significantly more male Nobel Prize
winners than female winners. To continue my investigation, I decided to
create a new, two-way contingency table using the same data to see
whether there might be a relationship between Nobel Prize winners’
gender and their country of birth based on the assumption that women
might receive more opportunities that could lead to their winning a
Nobel Prize if they grow up in certain countries.

``` r
# Create a two-way contigency table of gender vs. country
table(winners$gender, winners$birthCountry)
```

    ##         
    ##          Argentina Australia Austria Austria-Hungary Austrian Empire Bavaria
    ##   female         0         1       1               1               1       0
    ##   male           4         9      15              12               3       1
    ##         
    ##          Belgian Congo Belgium Bosnia Brazil British India
    ##   female             0       0      0      0             0
    ##   male               1       9      1      1             2
    ##         
    ##          British Mandate of Palestine British Protectorate of Palestine
    ##   female                            1                                 0
    ##   male                              4                                 1
    ##         
    ##          British West Indies Bulgaria Burma Canada Chile China Colombia Costa Rica
    ##   female                   0        0     1      2     1     1        0          0
    ##   male                     1        1     0     19     1    10        2          1
    ##         
    ##          Crete Cyprus Czechoslovakia Denmark East Friesland East Timor Egypt Ethiopia
    ##   female     0      0              0       1              0          0     1        0
    ##   male       1      1              1      11              1          2     5        1
    ##         
    ##          Faroe Islands (Denmark) Finland France Free City of Danzig French Algeria
    ##   female                       0       0      6                   0              0
    ##   male                         1       2     52                   1              2
    ##         
    ##          French protectorate of Tunisia German-occupied Poland Germany Gold Coast
    ##   female                              0                      0       3          0
    ##   male                                1                      1      77          1
    ##         
    ##          Guadeloupe Island Guatemala Hesse-Kassel Hungary Iceland India Iran Iraq
    ##   female                 0         1            0       1       0     0    2    1
    ##   male                   1         1            1       8       1    10    0    0
    ##         
    ##          Ireland Italy Japan Java, Dutch East Indies Kenya Korea Lebanon Liberia
    ##   female       0     2     0                       0     1     0       0       2
    ##   male         5    16    28                       1     0     2       1       0
    ##         
    ##          Lithuania Luxembourg Madagascar Mecklenburg Mexico Morocco New Zealand
    ##   female         0          0          0           0      0       0           0
    ##   male           1          2          1           1      3       1           3
    ##         
    ##          Nigeria Northern Ireland Norway Ottoman Empire Pakistan Persia Peru
    ##   female       0                2      1              1        1      1    0
    ##   male         1                3     12              1        0      0    1
    ##         
    ##          Philippines Poland Portugal Prussia Romania Russia Russian Empire
    ##   female           1      2        0       0       1      0              1
    ##   male             0      7        2      13       3     18             15
    ##         
    ##          Saint Lucia Schleswig Scotland South Africa Southern Rhodesia Spain Sweden
    ##   female           0         0        0            1                 0     0      2
    ##   male             1         2       11            8                 1     7     28
    ##         
    ##          Switzerland Taiwan the Netherlands Tibet Trinidad and Tobago Turkey Tuscany
    ##   female           0      0               0     0                   0      0       0
    ##   male            19      1              19     1                   1      2       1
    ##         
    ##          Ukraine United Kingdom USA USSR Venezuela Vietnam West Germany Württemberg
    ##   female       1              0  17    0         0       0            0           0
    ##   male         0             89 272    7         1       1            5           1
    ##         
    ##          Yemen
    ##   female     1
    ##   male       0

The table shows some interesting information, including which countries
have had the most past Nobel Prize winners. I am not too surprised to
see that the United States has not only the most total winners but also
the most female winners. Some interesting observations are that many
countries that have a lot of total winners, such as Japan and the United
Kingdom, actually have no female winners. Whereas, others countries,
such as Iran and Iraq, have only had female Nobel Prize winners.

Since the contingency table comparing gender and birth country is rather
large, I decided to narrow in on only the countries that have female
winners. I pulled some new data on all the winning female laureates by
calling `laureates(laureateGender = "female")`. I then created a bar
graph to plot the number of female Nobel Prize winners based on country.

``` r
# Get data on all female laureates
femaleWinners <- laureates(laureateGender = "female")

# Group female laureates by country of birth and get a total count per country
femaleByCountry <- femaleWinners %>%
  group_by(birthCountry) %>%
  summarise(count = n())

# Create a bar graph of country vs. count
plot1 <- ggplot(femaleByCountry, aes(x = birthCountry, y =count)) + 
  # Add a bar graph layer and fill color by country
  geom_bar(aes(fill = birthCountry), stat = "identity") +
  # Add axis labels and title
  labs(x = "Birth Country", y = "Count", title ="Female Nobel Prize Winners by Country") +
  # Add legend for countries
  scale_fill_discrete(name = "Birth Country") +
  # Adjust the x-axis labels to better fit the plot
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Plot the resulting bar graph
plot1
```

![](README_files/figure-gfm/plot1-1.png)<!-- -->

This looks better, but it is a bit difficult to read because of the
number of countries included. I decided to try plotting the same data in
a different format using the treemaps package for ggplot in hopes that
it would provide a clearer visual representation of the number of female
Nobel Prize laureates per country.

``` r
# Create a treemap of country vs. count
plot2 <- ggplot(femaleByCountry, aes(area = count, fill = count, label = birthCountry)) +
  # Add a treemap layer
  geom_treemap() +
  # Adjust the labels in each tile to better fit the plot
  geom_treemap_text(fontface = "italic", colour = "white", place = "centre", grow = TRUE)

# Plot the resulting treemap
plot2
```

![](README_files/figure-gfm/plot2-1.png)<!-- -->

The treemap is nice because it helps to really highlight which countries
have the most female Nobel Prize Winners based on the size of their
individual tiles, clearly showing that the United States has the most
female laureates followed by France.

Still, since we could already see from the earlier bar graph that there
are clearly a lot of countries that have just one female Nobel Prize
winner, I thought we could further narrow down the countries with more
female laureates by sorting the data for only those that have two or
more winners.

``` r
# Filter the data for only countries with at least two or more past female laureates
twoPlusFemales <- femaleByCountry %>%
  filter(count > 1)

# Create a bar graph of country vs. count
plot3 <- ggplot(twoPlusFemales, aes(x = birthCountry, y =count)) + 
  # Add a bar graph layer and fill color by country
  geom_bar(aes(fill = birthCountry), stat = "identity") +
  # Add axis labels and title
  labs(x = "Birth Country", y = "Count", title ="Countries with Two or More Female Winners") +
  # Add legend for countries
  scale_fill_discrete(name = "Birth Country") +
  # Adjust the x-axis labels to better fit the plot
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Plot the resulting bar graph
plot3
```

![](README_files/figure-gfm/plot3-1.png)<!-- -->

Based on this new graph, we can see that the majority of countries with
two or more female Nobel Prize winners are located in either Europe or
North America, with the exception of Iran and Liberia.

This brings me to back to my earlier observation that Iran and Iraq had
only female laureates but no male laureates. To further investigate this
information, I called `laureates("Iran)` and `laureates("Iraq")` to pull
data for these countries only.

``` r
laureates("Iran")
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":["id"],"name":[1],"type":["chr"],"align":["left"]},{"label":["fullName"],"name":[2],"type":["chr"],"align":["left"]},{"label":["gender"],"name":[3],"type":["chr"],"align":["left"]},{"label":["awardYear"],"name":[4],"type":["chr"],"align":["left"]},{"label":["category"],"name":[5],"type":["chr"],"align":["left"]},{"label":["birthCity"],"name":[6],"type":["chr"],"align":["left"]},{"label":["birthCountry"],"name":[7],"type":["chr"],"align":["left"]},{"label":["cityNow"],"name":[8],"type":["chr"],"align":["left"]},{"label":["countryNow"],"name":[9],"type":["chr"],"align":["left"]},{"label":["continent"],"name":[10],"type":["chr"],"align":["left"]},{"label":["locationString"],"name":[11],"type":["chr"],"align":["left"]}],"data":[{"1":"1033","2":"Narges Mohammadi","3":"female","4":"2023","5":"Peace","6":"Zanjan","7":"Iran","8":"Zanjan","9":"Iran","10":"Asia","11":"Zanjan, Iran"},{"1":"773","2":"Shirin Ebadi","3":"female","4":"2003","5":"Peace","6":"Hamadan","7":"Iran","8":"Hamadan","9":"Iran","10":"Asia","11":"Hamadan, Iran"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

``` r
laureates("Iraq")
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":["id"],"name":[1],"type":["chr"],"align":["left"]},{"label":["fullName"],"name":[2],"type":["chr"],"align":["left"]},{"label":["gender"],"name":[3],"type":["chr"],"align":["left"]},{"label":["awardYear"],"name":[4],"type":["chr"],"align":["left"]},{"label":["category"],"name":[5],"type":["chr"],"align":["left"]},{"label":["birthCity"],"name":[6],"type":["chr"],"align":["left"]},{"label":["birthCountry"],"name":[7],"type":["chr"],"align":["left"]},{"label":["cityNow"],"name":[8],"type":["chr"],"align":["left"]},{"label":["countryNow"],"name":[9],"type":["chr"],"align":["left"]},{"label":["continent"],"name":[10],"type":["chr"],"align":["left"]},{"label":["locationString"],"name":[11],"type":["chr"],"align":["left"]}],"data":[{"1":"967","2":"Nadia Murad Basee Taha","3":"female","4":"2018","5":"Peace","6":"Kojo","7":"Iraq","8":"Kojo","9":"Iraq","10":"Asia","11":"Kojo, Iraq"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

The data shows that all three female laureates from these countries were
winners of The Nobel Peace Prize. This led me to wonder whether female
Nobel Prize winners have been awarded more prizes in certain categories
over others.

To further investigate this, I started by creating a one-way contingency
table comparing how many awards female laureates have received in each
of the six categories using the previous data I pulled from calling
`laureates(laureateGender = "female")`.

``` r
# Create a one-way contingency table of category for female laureates
table(femaleWinners$category)
```

    ## 
    ##              Chemistry      Economic Sciences             Literature 
    ##                      7                      3                     17 
    ##                  Peace                Physics     Physics, Chemistry 
    ##                     19                      4                      1 
    ## Physiology or Medicine 
    ##                     13

The table shows that female Nobel Prize winners have been awarded the
most prizes in the categories of Peace, Physiology or Medicine, and
Literature, but have received fewer prizes in the scientific categories.

This observation then led me to wonder whether the monetary prize amount
awarded to Nobel Prize winners differs among categories, which could
lead to female and male laureates receiving different average prize
amounts.To further investigate this, I first called `prizes()` to pull
data on all the Nobel Prizes given out over the years.

I then found numerical summary data for the prize amount as grouped by
the different Nobel Prize categories.

``` r
# Get data for all Nobel Prizes awarded
nobelPrizes <- prizes()

# Find measures of center and spread for prize amount grouped by variable: category
nobelPrizes %>% group_by(category) %>%
  summarise(avgAmt = mean(prizeAmount), medAmt = median(prizeAmount),
            minAmt = min(prizeAmount), maxAmt = max(prizeAmount),
            varAmt = var(prizeAmount))
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":["category"],"name":[1],"type":["chr"],"align":["left"]},{"label":["avgAmt"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["medAmt"],"name":[3],"type":["int"],"align":["right"]},{"label":["minAmt"],"name":[4],"type":["int"],"align":["right"]},{"label":["maxAmt"],"name":[5],"type":["int"],"align":["right"]},{"label":["varAmt"],"name":[6],"type":["dbl"],"align":["right"]}],"data":[{"1":"Chemistry","2":"2683968","3":"257220","4":"114935","5":"11000000","6":"1.468271e+13"},{"1":"Economic Sciences","2":"5795564","3":"7400000","4":"375000","5":"11000000","6":"1.533133e+13"},{"1":"Literature","2":"2683968","3":"257220","4":"114935","5":"11000000","6":"1.468271e+13"},{"1":"Peace","2":"2683968","3":"257220","4":"114935","5":"11000000","6":"1.468271e+13"},{"1":"Physics","2":"2683968","3":"257220","4":"114935","5":"11000000","6":"1.468271e+13"},{"1":"Physiology or Medicine","2":"2683968","3":"257220","4":"114935","5":"11000000","6":"1.468271e+13"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

Using this same data, I also created some boxplots for a better visual
representation of the numerical summaries.

``` r
# Create a box plot for numeric variable prize amount across categorical variable category
plot4 <- ggplot(nobelPrizes, aes(x = prizeAmount, fill = category)) +
  # Add box plot layer
  geom_boxplot() +
  # Add axis label and title
  labs(x = "Prize Amount Awarded", title = "Boxplot of Prize Amount Across Category") +
  # Add theme to remove y-axis labels and tick marks
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  # Change x-axis labels from scientific notation
  scale_x_continuous(labels = comma)

# Plot resulting box plots
plot4
```

![](README_files/figure-gfm/plot4-1.png)<!-- -->

Based on the table and box plots, we can see that the prize amount
generally does not vary by category, except for the category of Economic
Sciences. Some additional internet research revealed that this is
because the Nobel Prize in the category of Economic Sciences was not
established until 1968. The box plot for Economic Sciences does reveal
another interesting observation, however. The median and interquartile
range for this category is higher than the others, which made me wonder
if the Nobel Prize prize award amount has increased steadily through the
years since it first began in 1901. Thus, I decided to investigate this
by finding the numerical summary data for the prize amount as grouped by
award year.

``` r
# Find measures of center and spread for prize amount grouped by variable: awardYear
nobelPrizes %>% group_by(awardYear) %>%
  summarise(avgAmt = mean(prizeAmount), medAmt = median(prizeAmount),
            minAmt = min(prizeAmount), maxAmt = max(prizeAmount), varAmt = var(prizeAmount))
```

<div data-pagedtable="false">

<script data-pagedtable-source type="application/json">
{"columns":[{"label":["awardYear"],"name":[1],"type":["chr"],"align":["left"]},{"label":["avgAmt"],"name":[2],"type":["dbl"],"align":["right"]},{"label":["medAmt"],"name":[3],"type":["dbl"],"align":["right"]},{"label":["minAmt"],"name":[4],"type":["int"],"align":["right"]},{"label":["maxAmt"],"name":[5],"type":["int"],"align":["right"]},{"label":["varAmt"],"name":[6],"type":["dbl"],"align":["right"]}],"data":[{"1":"1901","2":"150782","3":"150782","4":"150782","5":"150782","6":"0"},{"1":"1902","2":"141847","3":"141847","4":"141847","5":"141847","6":"0"},{"1":"1903","2":"141358","3":"141358","4":"141358","5":"141358","6":"0"},{"1":"1904","2":"140859","3":"140859","4":"140859","5":"140859","6":"0"},{"1":"1905","2":"138089","3":"138089","4":"138089","5":"138089","6":"0"},{"1":"1906","2":"138536","3":"138536","4":"138536","5":"138536","6":"0"},{"1":"1907","2":"138796","3":"138796","4":"138796","5":"138796","6":"0"},{"1":"1908","2":"139800","3":"139800","4":"139800","5":"139800","6":"0"},{"1":"1909","2":"139800","3":"139800","4":"139800","5":"139800","6":"0"},{"1":"1910","2":"140703","3":"140703","4":"140703","5":"140703","6":"0"},{"1":"1911","2":"140695","3":"140695","4":"140695","5":"140695","6":"0"},{"1":"1912","2":"140476","3":"140476","4":"140476","5":"140476","6":"0"},{"1":"1913","2":"143010","3":"143010","4":"143010","5":"143010","6":"0"},{"1":"1914","2":"146900","3":"146900","4":"146900","5":"146900","6":"0"},{"1":"1915","2":"149223","3":"149223","4":"149223","5":"149223","6":"0"},{"1":"1916","2":"131793","3":"131793","4":"131793","5":"131793","6":"0"},{"1":"1917","2":"133823","3":"133823","4":"133823","5":"133823","6":"0"},{"1":"1918","2":"138198","3":"138198","4":"138198","5":"138198","6":"0"},{"1":"1919","2":"133127","3":"133127","4":"133127","5":"133127","6":"0"},{"1":"1920","2":"134100","3":"134100","4":"134100","5":"134100","6":"0"},{"1":"1921","2":"121573","3":"121573","4":"121573","5":"121573","6":"0"},{"1":"1922","2":"122483","3":"122483","4":"122483","5":"122483","6":"0"},{"1":"1923","2":"114935","3":"114935","4":"114935","5":"114935","6":"0"},{"1":"1924","2":"116719","3":"116719","4":"116719","5":"116719","6":"0"},{"1":"1925","2":"118165","3":"118165","4":"118165","5":"118165","6":"0"},{"1":"1926","2":"116960","3":"116960","4":"116960","5":"116960","6":"0"},{"1":"1927","2":"126501","3":"126501","4":"126501","5":"126501","6":"0"},{"1":"1928","2":"156939","3":"156939","4":"156939","5":"156939","6":"0"},{"1":"1929","2":"172760","3":"172760","4":"172760","5":"172760","6":"0"},{"1":"1930","2":"172947","3":"172947","4":"172947","5":"172947","6":"0"},{"1":"1931","2":"173206","3":"173206","4":"173206","5":"173206","6":"0"},{"1":"1932","2":"171753","3":"171753","4":"171753","5":"171753","6":"0"},{"1":"1933","2":"170332","3":"170332","4":"170332","5":"170332","6":"0"},{"1":"1934","2":"162608","3":"162608","4":"162608","5":"162608","6":"0"},{"1":"1935","2":"159917","3":"159917","4":"159917","5":"159917","6":"0"},{"1":"1936","2":"159850","3":"159850","4":"159850","5":"159850","6":"0"},{"1":"1937","2":"158463","3":"158463","4":"158463","5":"158463","6":"0"},{"1":"1938","2":"155077","3":"155077","4":"155077","5":"155077","6":"0"},{"1":"1939","2":"148822","3":"148822","4":"148822","5":"148822","6":"0"},{"1":"1940","2":"138570","3":"138570","4":"138570","5":"138570","6":"0"},{"1":"1941","2":"131496","3":"131496","4":"131496","5":"131496","6":"0"},{"1":"1942","2":"131891","3":"131891","4":"131891","5":"131891","6":"0"},{"1":"1943","2":"123691","3":"123691","4":"123691","5":"123691","6":"0"},{"1":"1944","2":"121841","3":"121841","4":"121841","5":"121841","6":"0"},{"1":"1945","2":"121333","3":"121333","4":"121333","5":"121333","6":"0"},{"1":"1946","2":"121524","3":"121524","4":"121524","5":"121524","6":"0"},{"1":"1947","2":"146115","3":"146115","4":"146115","5":"146115","6":"0"},{"1":"1948","2":"159773","3":"159773","4":"159773","5":"159773","6":"0"},{"1":"1949","2":"156290","3":"156290","4":"156290","5":"156290","6":"0"},{"1":"1950","2":"164304","3":"164304","4":"164304","5":"164304","6":"0"},{"1":"1951","2":"167612","3":"167612","4":"167612","5":"167612","6":"0"},{"1":"1952","2":"171135","3":"171135","4":"171135","5":"171135","6":"0"},{"1":"1953","2":"175293","3":"175293","4":"175293","5":"175293","6":"0"},{"1":"1954","2":"181647","3":"181647","4":"181647","5":"181647","6":"0"},{"1":"1955","2":"190214","3":"190214","4":"190214","5":"190214","6":"0"},{"1":"1956","2":"200123","3":"200123","4":"200123","5":"200123","6":"0"},{"1":"1957","2":"208629","3":"208629","4":"208629","5":"208629","6":"0"},{"1":"1958","2":"214559","3":"214559","4":"214559","5":"214559","6":"0"},{"1":"1959","2":"220678","3":"220678","4":"220678","5":"220678","6":"0"},{"1":"1960","2":"225987","3":"225987","4":"225987","5":"225987","6":"0"},{"1":"1961","2":"250233","3":"250233","4":"250233","5":"250233","6":"0"},{"1":"1962","2":"257220","3":"257220","4":"257220","5":"257220","6":"0"},{"1":"1963","2":"265000","3":"265000","4":"265000","5":"265000","6":"0"},{"1":"1964","2":"273000","3":"273000","4":"273000","5":"273000","6":"0"},{"1":"1965","2":"282000","3":"282000","4":"282000","5":"282000","6":"0"},{"1":"1966","2":"300000","3":"300000","4":"300000","5":"300000","6":"0"},{"1":"1967","2":"320000","3":"320000","4":"320000","5":"320000","6":"0"},{"1":"1968","2":"350000","3":"350000","4":"350000","5":"350000","6":"0"},{"1":"1969","2":"375000","3":"375000","4":"375000","5":"375000","6":"0"},{"1":"1970","2":"400000","3":"400000","4":"400000","5":"400000","6":"0"},{"1":"1971","2":"450000","3":"450000","4":"450000","5":"450000","6":"0"},{"1":"1972","2":"480000","3":"480000","4":"480000","5":"480000","6":"0"},{"1":"1973","2":"510000","3":"510000","4":"510000","5":"510000","6":"0"},{"1":"1974","2":"550000","3":"550000","4":"550000","5":"550000","6":"0"},{"1":"1975","2":"630000","3":"630000","4":"630000","5":"630000","6":"0"},{"1":"1976","2":"681000","3":"681000","4":"681000","5":"681000","6":"0"},{"1":"1977","2":"700000","3":"700000","4":"700000","5":"700000","6":"0"},{"1":"1978","2":"725000","3":"725000","4":"725000","5":"725000","6":"0"},{"1":"1979","2":"800000","3":"800000","4":"800000","5":"800000","6":"0"},{"1":"1980","2":"880000","3":"880000","4":"880000","5":"880000","6":"0"},{"1":"1981","2":"1000000","3":"1000000","4":"1000000","5":"1000000","6":"0"},{"1":"1982","2":"1150000","3":"1150000","4":"1150000","5":"1150000","6":"0"},{"1":"1983","2":"1500000","3":"1500000","4":"1500000","5":"1500000","6":"0"},{"1":"1984","2":"1650000","3":"1650000","4":"1650000","5":"1650000","6":"0"},{"1":"1985","2":"1800000","3":"1800000","4":"1800000","5":"1800000","6":"0"},{"1":"1986","2":"2000000","3":"2000000","4":"2000000","5":"2000000","6":"0"},{"1":"1987","2":"2175000","3":"2175000","4":"2175000","5":"2175000","6":"0"},{"1":"1988","2":"2500000","3":"2500000","4":"2500000","5":"2500000","6":"0"},{"1":"1989","2":"3000000","3":"3000000","4":"3000000","5":"3000000","6":"0"},{"1":"1990","2":"4000000","3":"4000000","4":"4000000","5":"4000000","6":"0"},{"1":"1991","2":"6000000","3":"6000000","4":"6000000","5":"6000000","6":"0"},{"1":"1992","2":"6500000","3":"6500000","4":"6500000","5":"6500000","6":"0"},{"1":"1993","2":"6700000","3":"6700000","4":"6700000","5":"6700000","6":"0"},{"1":"1994","2":"7000000","3":"7000000","4":"7000000","5":"7000000","6":"0"},{"1":"1995","2":"7200000","3":"7200000","4":"7200000","5":"7200000","6":"0"},{"1":"1996","2":"7400000","3":"7400000","4":"7400000","5":"7400000","6":"0"},{"1":"1997","2":"7500000","3":"7500000","4":"7500000","5":"7500000","6":"0"},{"1":"1998","2":"7600000","3":"7600000","4":"7600000","5":"7600000","6":"0"},{"1":"1999","2":"7900000","3":"7900000","4":"7900000","5":"7900000","6":"0"},{"1":"2000","2":"9000000","3":"9000000","4":"9000000","5":"9000000","6":"0"},{"1":"2001","2":"10000000","3":"10000000","4":"10000000","5":"10000000","6":"0"},{"1":"2002","2":"10000000","3":"10000000","4":"10000000","5":"10000000","6":"0"},{"1":"2003","2":"10000000","3":"10000000","4":"10000000","5":"10000000","6":"0"},{"1":"2004","2":"10000000","3":"10000000","4":"10000000","5":"10000000","6":"0"},{"1":"2005","2":"10000000","3":"10000000","4":"10000000","5":"10000000","6":"0"},{"1":"2006","2":"10000000","3":"10000000","4":"10000000","5":"10000000","6":"0"},{"1":"2007","2":"10000000","3":"10000000","4":"10000000","5":"10000000","6":"0"},{"1":"2008","2":"10000000","3":"10000000","4":"10000000","5":"10000000","6":"0"},{"1":"2009","2":"10000000","3":"10000000","4":"10000000","5":"10000000","6":"0"},{"1":"2010","2":"10000000","3":"10000000","4":"10000000","5":"10000000","6":"0"},{"1":"2011","2":"10000000","3":"10000000","4":"10000000","5":"10000000","6":"0"},{"1":"2012","2":"8000000","3":"8000000","4":"8000000","5":"8000000","6":"0"},{"1":"2013","2":"8000000","3":"8000000","4":"8000000","5":"8000000","6":"0"},{"1":"2014","2":"8000000","3":"8000000","4":"8000000","5":"8000000","6":"0"},{"1":"2015","2":"8000000","3":"8000000","4":"8000000","5":"8000000","6":"0"},{"1":"2016","2":"8000000","3":"8000000","4":"8000000","5":"8000000","6":"0"},{"1":"2017","2":"9000000","3":"9000000","4":"9000000","5":"9000000","6":"0"},{"1":"2018","2":"9000000","3":"9000000","4":"9000000","5":"9000000","6":"0"},{"1":"2019","2":"9000000","3":"9000000","4":"9000000","5":"9000000","6":"0"},{"1":"2020","2":"10000000","3":"10000000","4":"10000000","5":"10000000","6":"0"},{"1":"2021","2":"10000000","3":"10000000","4":"10000000","5":"10000000","6":"0"},{"1":"2022","2":"10000000","3":"10000000","4":"10000000","5":"10000000","6":"0"},{"1":"2023","2":"11000000","3":"11000000","4":"11000000","5":"11000000","6":"0"}],"options":{"columns":{"min":{},"max":[10]},"rows":{"min":[10],"max":[10]},"pages":{}}}
  </script>

</div>

I also used this same data to create a scatter plot looking at changes
over the years chronologically.

``` r
# Create a scatter plot for numeric variable prize amount across categorical variable award year
plot5 <- ggplot(nobelPrizes, aes(x = awardYear, y = prizeAmount, color = prizeAmount)) +
  # Add scatter plot later
  geom_point() +
  # Add axis labels and title
  labs(x = "Award Year", y = "Prize Amount Awarded", 
       title = "Scatterplot of Award Year and Prize Amount") +
  # Adjust x-axis for labels increasing in increements of 10
  scale_x_discrete(breaks = seq(1900, 2030, by = 10)) +
  # Change y-axis labels from scientific notation
  scale_y_continuous(labels = comma) +
  # Remove legend
  theme(legend.position="none") +
  # Add color gradient from blue to red based on amount
  scale_color_gradient(low="blue", high="red")

# Plot resulting scatter plot
plot5
```

![](README_files/figure-gfm/plot5-1.png)<!-- -->

The scatter plot shows that while the prize amount awarded has increased
since the Nobel Prize awards were established, this increase has not
been at a steady and continuous rate. In fact, there have been years
where the award amount actually decreased from the year before. This is
interesting to note and makes me wonder how the prize amount is
calculated, though the data I have does not allow me to explore this any
further at this time.

# Wrap-Up

To summarize what I did in this vignette, I wrote functions to interact
with the Nobel Prize API’s endpoints, retrieved some data, and explored
it using tables, numerical summaries, and data visualization through a
variety of different plots. Some of my findings were not too surprising,
such as there being a significantly greater number of male versus female
laureates, and the United States having both the greatest total number
of past Nobel Prize winners and greatest number of female laureates. I
also discovered some interesting new information, such as how female
Nobel Prize winners have generally won a lot more in certain categories
but are still far behind in the scientific categories.
