# temp
---
title: "Spatial Autocorrelation Tutorial"
author: "Geog 418"
date: "`r format(Sys.time(), '%B %d, %Y')`"
output: pdf_document
#output: 
#  bookdown::html_document2:
#    number_sections: false
#   fig_caption: true
#    global_numbering: true 
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
#####
## For this lab you will need the following libraries: 
##knitr, tmap, spdep, raster, shinyjs, e1071, sf


dir <- "C:/Users/chrisbone/OneDrive - University of Victoria/Courses/Geog 418 Spatial Analysis/2024/Assignment3"
setwd(dir)
knitr::opts_knit$set(root.dir = dir)
```

## Introduction

Spatial autocorrelation is an essential concept to grasp for anyone working with spatial data. A measure of spatial autocorrelation will describe the degree of similarity between observations at spatial locations. This means that to measure spatial autocorrelation, we need two key things: observations (e.g. data values or attributes of some sort) and locations (e.g. points, polygons, or raster cells) for those observations.

I often find that using simplified extreme examples is a good way to grasp a complex concept, so let’s think about houses in a city for a moment. If we want to determine whether attributes of houses are spatially autocorrelated, we must first pick an attribute that can be measured. Let’s say, household income. If you were to sample four adjacent households and they all had incomes around \$50,000, it would seem somewhat obvious that these measurements are similar (correlated) to one another spatially, because they are taken within the same area. If you sampled another four households in the same neighbourhood and then determined that for the entire city, the median income was around \$50,000, your results would be influenced by positive spatial autocorrelation, meaning that nearby observations of income are similar to one other, or clustered. If you examined the same variable of household income from four adjacent houses, where one house has an income of \$25,000, the next \$50,000, the next $85,000, and the last \$100,000, the observations at these nearby houses would be an example of negative spatial autocorrelation, i.e., nearby observations of income are different from one another.

The finer details of determining spatial autocorrelation can be a bit more complicated, for example, how do we define “near to one another”? Also, how can we tell if any patterns of spatial autocorrelation are statistically significant? In the old days, geographers would work these things out by hand using complicated formulas, but today I will walk you through figuring all of this out using the ‘R’ statistical language.

We will work through examples of two different types of spatial autocorrelation: global and local. Global spatial autocorrelation looks at the big picture and will tell you about the entire dataset, while local spatial autocorrelation focuses in on variability within the data set. By the end of this tutorial you will be able to test for spatial autocorrelation in different ways, and we will do this by analyzing Canadian census data for 2016.

Since its beginning in 1871, Canada’s national census has provided government leaders, businesses, and everyday citizens vital information on the Canadian society [1]. This decennial survey offers a snapshot of the country’s population characteristics, and is used to make important decisions on governmental representation, assignment of social services, and studies on housing, health, or transportation needs [2]. Whether on a national or municipal scale, the ability to address problems and identify solutions is invaluable to expand our societal understanding in all avenues of study. In the field of geography, census information is perhaps even more appreciated, as spatial analysts are able to manipulate, test, and visualize data patterns across time and space to answer a plethora of questions.

Our earlier example was a qualitative assessment based on a few extreme values, but in order to actually determine spatial autocorrelation in a quantitative way, we need to use various measures and equations, some of which can seem fairly complicated. Just as an example, one of the most common ways to test for global spatial autocorrelation involves calculating the global Moran’s I, for which the equation looks like this:

$$
I = \frac{\sum_{i=1}^n\sum_{j=1}^nW_{i,j}(x_i - \bar{x})(x_j - \bar{x})}{(\sum_{i=1}^n\sum_{j=1}^nW_{i,j})\sum_{i=1}^n(x_i - \bar{x})^2}
$$

I will talk a bit more about this formula later, but the point here is that none of us would choose to do this equation by hand; or in an excel spreadsheet for that matter more than once. This is where ‘R’ really saves the day, because it allows us to use pre-made functions within various packages to make many of these complex calculations for us.

Today we will use the packages knitr, rgdal, tmap, spdep, raster, shinyjs, and e1071 [3-9]. You can install and load these packages with the following code:

```{r Libraries, echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE}
#Install packages if not already installed:
#install.packages("knitr")
# install.packages("rgdal")
#install.packages("tmap")
#install.packages("spdep")
#install.packages("raster")
#install.packages("shinyjs")
#install.packages("e1071")

#Load in libraries:
library(knitr)
library(tmap)
library(spdep)
library(raster)
library(shinyjs)
library(e1071)
library(sf)

```

You will notice that the code I have posted contains hashtags followed by lines of code. Hashtags are our way of telling ‘R’ to ignore whatever comes after this point, so we can write comments about the code itself; or in this case, to allow R to skip a line we might not need. The lines that are commented out will install a package if it has never been installed before, and will only need to be ran once. After that you can use only the library commands that are not commented out.

To start, you will want to read the shapefile for the census boundaries into ‘R’ as a spatial polygons data frame and the census data into a dataframe.

```{r Read in data, echo=TRUE, eval=TRUE, warning=FALSE}
#From the working dir read in the csv
csv <- read.csv("./ucgsJQnBVLvP_data.csv") 

#Data source is the working dir (where the layer is), layer is the name of the file (without .shp)
shp <- st_read("lda_000b16a_e.shp") 

```

Next, we want to clean up our data and make it easier to use. First we will create a vector of the column names so we understand what columns refer to what data. Then, we will remove any unwanted rows and merge the result with our spatial polygon data frame, and finally, we will subset to only the city of interest, for this analysis 'Kamloops'. The last step is to turn any absolute count data into a rate for mapping and analysis.

```{r Clean data, echo=TRUE, eval=TRUE, warning=FALSE}
#New column names
cols <- c("GEO UID", "Province code", "Province name", "CD code",
        "CD name", "DA name", "Population", "Land area", 
        "Median total income", "Income Sample Size", "French Knowledge", 
        "Language Sample Size")

#Apply those names to dataframe
colnames(csv) <- cols

#Add column to count number of ID charactors
csv$len <- nchar(csv$`GEO UID`)

#Remove IDs with less than 8 numbers
csv_clean <- subset(csv, csv$len == 8)

#Merge spatial and aspatial data
census_DAs <- merge(shp, csv_clean, 
                    by.x = "DAUID", 
                    by.y = "GEO UID", 
                    all.x = TRUE)

#Subset for Kamloops
Municp <- subset(census_DAs, census_DAs$CMANAME == "Kamloops")

#Convert to rate
Municp$PercFrench <- (Municp$`French Knowledge`/Municp$`Language Sample Size`)*100
```

Before we can start to analyze our data, we need to be sure that the data we are looking at is relevant. Often, missing data in the form of NA or 0 values can change the results of an analysis. To make sure that the polygons we are looking at actually contain values for our variables of interest. To do this we can remove any polygon that contains an NA value for either median total income or knowledge of French.

```{r NA Remove, echo=TRUE, eval=TRUE, warning=FALSE}
#Remove Income NA
Income_noNA <- Municp[which(!is.na(Municp$`Median total income`)),]

#Remove French NA
French_noNA <- Municp[which(!is.na(Municp$PercFrench)),]
```

Next, we will take a closer look at the two variables we are interested in: Median total income and Percentage of respondents with French language knowledge. We will look at some descriptive stats (Table \@ref(tab:DescriptiveStats)), and do a final check for NA values in the data.

```{r DescriptiveStats, echo=TRUE, eval=TRUE, warning=FALSE}
#Calculate descriptive stats for Income
meanIncome <- mean(Income_noNA$`Median total income`)
stdevIncome <- sd(Income_noNA$`Median total income`)
skewIncome <- skewness(Income_noNA$`Median total income`)

#Calculate descriptive stats for French
meanFrench <- mean(French_noNA$PercFrench)
stdevFrench <- sd(French_noNA$PercFrench)
skewFrench <- skewness(French_noNA$PercFrench)

#Create dataframe for display in table
data <- data.frame(Variable = c("Income", "French Language"),
                   Mean = c(round(meanIncome,2), round(meanFrench,2)),
                   StandardDeviation = c(round(stdevIncome,2), round(stdevFrench,2)),
                   Skewness = c(round(skewIncome,2), round(skewFrench,2)))

#Produce table
kable(data, caption = paste0("Descriptive statistics for selected ", 2016, " census variables"))
```

It is always a good idea to visualize your data when possible. We can map out the variable to get a good idea of how variables change across the study area (Figure \@ref(fig:StudyArea)).

```{r StudyArea, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap="Kamloops census dissemination areas showing median total income (left) and percentage of respondants with knowledge of french (right)."}
#Choose a pallete
# tmaptools::palette_explorer() #Tool for selecting pallettes

#Map median Income
map_Income <- tm_shape(Income_noNA) + 
  tm_polygons(col = "Median total income", 
              title = "Median total income", 
              style = "jenks", 
              palette = "BuGn", n = 6,
              border.alpha = 0,
              colorNA = "grey") +
  tm_layout(legend.position = c("RIGHT", "TOP"))

#Map French Knowledge
map_French <- tm_shape(French_noNA) + 
  tm_polygons(col = "PercFrench", 
              title = "Percentage with \n French Knowledge", 
              style = "jenks", 
              palette = "BuGn", n = 6,
              border.alpha = 0,
              colorNA = "grey") +
  tm_layout(legend.position = c("RIGHT", "TOP"))

#Print maps side by side
tmap_arrange(map_Income, map_French, ncol = 2, nrow = 1)
```

## Neighbourhood matrix

Now that we have our variables, we need a quantitative way of determining which areas are, and are not, neighbours. The creation of a spatial weights matrix is a key component of testing for spatial autocorrelation, as it denotes the degree of closeness between observations by defining who the neighbours are for each observation [10]. There are different ways to do this, but here I will introduce two types of weighting schemes; rook weights and queen weights. If you are familiar with how the game of chess is played, these weighting schemes make much more sense. Rooks are a chess piece that can move vertically and horizontally on the board where the sides of the squares touch one another, but not diagonally where the squares are only connected by corners. Therefore, a rook weighting scheme only includes neighbours where the adjacent polygon sides overlap, but ignores polygons connected only at corners. Conversely, a queen in chess can move in all directions, including diagonal movements connected only by corners. Therefore, a queen weighting scheme includes all polygons connected to the initial polygon, even if only connected by a corner. It stands to reason that by including more types of connections, there will be more polygons to assess as neighbours with this scheme versus the rook weighting. Contiguity matrices like these are commonly used when dealing with data in vector format and are considered a geometric as opposed to geostatistical approach to neighbour selection [11].

The code to create a list of neighbours in R is very simple thanks to the poly2nb() function in the ‘spdep’ package. If we want to change from default queen weighting to rook weighting in our selection, we simply change the ‘queen = TRUE’ to ‘queen = FALSE’.

```{r Neighbours, echo=TRUE, eval=TRUE, warning=FALSE}

#Income Neighbours - Queens weight
Income.nb <- poly2nb(Income_noNA)
# Use st_coordinates to get the coordinates
Income.net <- nb2lines(Income.nb, coords=st_coordinates(Income_noNA))


#Income Neighbours - Rooks weight
Income.nb2 <- poly2nb(Income_noNA, queen = FALSE)
Income.net2 <- nb2lines(Income.nb2, coords=st_coordinates(Income_noNA))
crs(Income.net2) <- crs(Income_noNA)

#French Neighbours - Queens weight
French.nb <- poly2nb(French_noNA)
French.net <- nb2lines(French.nb, coords=st_coordinates(French_noNA))
crs(French.net) <- crs(French_noNA)

#French Neighbours - Rooks weight
French.nb2 <- poly2nb(French_noNA, queen = FALSE)
French.net2 <- nb2lines(French.nb2, coords=st_coordinates(French_noNA))
crs(French.net2) <- crs(French_noNA)

```

By printing outputs from each neighbour list, we can see that the differences in links between queen weighting and rook weighting. However, it is nice to be able to visualize these links, and see where the differences occur between schemes. By making slight adjustments to the mapping code above, we can produce other maps showing the connections between neighbours for different weight schemes (Figure \@ref(fig:Neighboursmap)).

```{r Neighboursmap, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap="Kamloops census dissemination areas showing median total income neighbours queens weight (left)  rooks weight (middle) and the combination of the two (right)."}

#Make queens map
IncomeQueen <- tm_shape(Income_noNA) + tm_borders(col='lightgrey') + 
              tm_shape(Income.net) + tm_lines(col='red')

#Make rooks map
IncomeRook <- tm_shape(Income_noNA) + tm_borders(col='lightgrey') + 
              tm_shape(Income.net2) + tm_lines(col='yellow', lwd = 2)

#Make combined map
IncomeBoth <- tm_shape(Income_noNA) + tm_borders(col='lightgrey') + 
               tm_shape(Income.net) + tm_lines(col='blue', lwd = 2) +
               tm_shape(Income.net2) + tm_lines(col='yellow', lwd = 2)

#Print maps in a three pane figure
tmap_arrange(IncomeQueen, IncomeRook, IncomeBoth, ncol = 3, nrow = 1)

```

This tutorial will make use of the Queen’s case for determining the neighbours of the ith observation. It has been suggested that when dealing with irregular polygons, using the Queen’s case is recommended in order to account for any possible inaccuracies in the data, such as rounding errors [12]. R provides several options for applying weights to the identified neighbours. Weights are defined by “style” (ie. type), and can include “B”, “W”, and “C”. The B weights matrix is the most basic of the three, as it employs a binary weighting scheme, whereby each neighbour is given a weight of 1, and all other polygons are given a weight of 0 (see figures above). A W weights matrix employs a row standardized weighting scheme, with each neighbour given equal weights that sum to 1 [11]. Comparatively, a C weights matrix is a globally standardized method of weighting, with all neighbours given equal weight across the entire study area [13].

Creating a weights matrix in R uses the “nb2listw” function from the “spdep” library. We can apply this function to the vri.nb variable created above, as it contains all of the neighbour links to which we want to assign weights. Additionally, if there are any polygons in our file with zero neighbour links, we still want the program to run. Therefore, we define “zero.policy” as equal to “TRUE”, which assigns weights vectors of zero length for regions with no neighbours [13]. Subsequently, we can print off our list of weights matrices (“print.listw”) in order to assess the distribution of weights for each observation (i) and its neighbours (j). The example of code below is using a weights matrix of type W. You can read more about the different styles of spatial weighting [here](https://r-spatial.github.io/spdep/reference/nb2listw.html).


```{r Final weights, echo=TRUE, eval=TRUE, warning=FALSE}
#Create Income weights matrix
Income.lw <- nb2listw(Income.nb, zero.policy = TRUE, style = "W")

#Create French weights matrix
French.lw <- nb2listw(French.nb, zero.policy = TRUE, style = "W")

head(Income.lw[["weights"]])[c(1:3)]

```


## Global Moran’s I

Now that we have determined how to choose and weight our neighbours, we can calculate the Global Moran’s I statistic. This method of testing for spatial autocorrelation looks across the entire study area for every location simultaneously [14]. As stated at the beginning of this tutorial, the equation for this statistic is

$$
I = \frac{\sum_{i=1}^n\sum_{j=1}^nW_{i,j}(x_i - \bar{x})(x_j - \bar{x})}{(\sum_{i=1}^n\sum_{j=1}^nW_{i,j})\sum_{i=1}^n(x_i - \bar{x})^2}
$$

Here, if $x$ is the variable being assessed, $x_i$ is the variable value at a point of interest (i) and $x_j$ represents a neighbour to $x_i$ (here determined by the queen weighting scheme). The spatial weighting applied to the weighting matrix $W_{i,j}$ is multiplied by both the differences of $x_i$ and the mean value of variable $x$, and $x_j$ and the mean value of variable $x$.

The denominator in this case is used to standardize our values, and therefore relatively high values of I correspond with positive spatial autocorrelation, and relatively low values of I correspond with negative spatial autocorrelation. Remember that the global Moran’s I statistic provides an indication of how spatially autocorrelated our data is over the entire dataset, thus representing a spatial pattern at the global scale [15].

Thankfully, Instead of typing the entire calculation out, we can use the moran.test() function found in the ‘spdep’ package.

```{r Global Morans I, echo=TRUE, eval=TRUE, warning=FALSE}
#Calculate Global Moran's I for Income
miIncome <- moran.test(Income_noNA$`Median total income`, Income.lw, zero.policy = TRUE)

#Extract Global Moran's I results for Income
mIIncome <- miIncome$estimate[[1]]
eIIncome <- miIncome$estimate[[2]]
varIncome <- miIncome$estimate[[3]]

#Calculate Global Moran's I for French
miFrench <- moran.test(French_noNA$PercFrench, French.lw, zero.policy = TRUE)

#Extract Global Moran's I results for French
mIFrench <- miFrench$estimate[[1]]
eIFrench <- miFrench$estimate[[2]]
varFrench <- miFrench$estimate[[3]]
```

This test will give us some useful values, such as our expected Moran’s I value for a completely random distribution (eI), our calculated Moran’s I (mI), and the variance (var). For the first example of queen weighted median income, our Moran’s I value was `r round(mIIncome, 4)` while the expected random distribution value was `r round(eIIncome, 4)`, telling us that the overall trend of the region is positive spatial autocorrelation. If we change the variable in the code above to percentage of respondents with French language knowledge, notice that the Moran’s I value is slightly lower (`r round(mIFrench, 4)`) but the expected random value remains very similar  (`r round(eIFrench, 4)`). This is because we are looking at a different variable while using the same weighting scheme, with slight differences in the NA polygons removed. We can conclude that both median income and French language knowledge show positive global spatial autocorrelation, and that income shows more positive autocorrelation relative to French knowledge.

We may be interested to see where our observed pattern lies compared to potential levels of positive or negative spatial autocorrelation, given our weighting scheme. We can create the following function, and use it to calculate the possible range or global Moran’s I values for each weighting scheme, with the minimum value showing perfect negative spatial autocorrelation and the maximum value showing perfect positive spatial autocorrelation.

```{r Global Morans Range, echo=TRUE, eval=TRUE, warning=FALSE}
#Function to calculate the range of global Moran's I
moran.range <- function(lw) {
  wmat <- listw2mat(lw)
  return(range(eigen((wmat + t(wmat))/2)$values))
}

#Calculate the range for the Income variable
range <- moran.range(Income.lw)
minRange <- range[1]
maxRange <- range[2]
```

Here the calculated range for income is `r round(minRange, 4)` to `r round(maxRange, 4)`, which shows that even though our calculated Moran’s I value of `r round(mIIncome, 4)` for income showed positive spatial autocorrelation, there is still quite a way to go before similar polygons were perfectly clustered. 

However, we can still go a step further and figure out whether these patterns are statistically significant. To do so, we can use a Z-test. Here our null hypothesis is that the distribution is completely random, and the alternate hypothesis is that the distribution is non-random. Using an $\alpha$ value of 0.05, if our Z-score falls above or below 1.96, we can say accept the alternate hypothesis with 95% certainty. A value greater than +1.96 would imply a positive spatial autocorrelation, and a value less than -1.96 would imply a negative spatial autocorrelation.

We can calculate a Z-test using the following code:

```{r Global Morans ZScore, echo=TRUE, eval=TRUE, warning=FALSE}
#Calculate z-test for Income
zIncome <- (mIIncome - eIIncome) / (sqrt(varIncome))

#Calculate z-test for French
zFrench <- (mIFrench - eIFrench) / (sqrt(varFrench))
```

The zscores for both variable confirm that for both median income (`r round(zIncome, 4)`) and french knowledge (`r round(zFrench, 4)`) there is strong evidence of significant spatial autocorrelation.

## Local spatial autocorrelation

If we want to take a closer look at regional differences for a variable of interest within the watershed, we can do that using a Local Moran’s I calculation. Instead of the Global Moran’s I, which calculated one mI, eI, var, for the entire region, here we will calculate these values for every single polygon (which we referred to earlier as xi) in our spatial data frame. Not only that, but we can calculate the Z-score and p-value for each polygon as well! This approach is highly useful in that it enables SAC “hotspots” to be identified and points to the influence of specific locations on the Global Moran’s I statistic [14].

The calculation for Local Moran’s I has many of the same features as our global calculation, although arranged in a different way.

$$
I_i = \frac{x_i - \bar{x}}{S_i^2}\sum{_{j=1}^n}W_{i,j}(x_j - \bar{x})\space \space where \space \space S_i^2 = \frac{\sum_{i=1}^n (x_i - \bar{x})^2}{n-1} 
$$

Again, instead of typing out these calculations, we can use the localmoran() function to deal with all of the messy calculations for us, as long as we input our variable and weighting scheme.


```{r Local Morans I, echo=TRUE, eval=TRUE, warning=FALSE}
#Calculate LISA test for Income
lisa.testIncome <- localmoran(Income_noNA$`Median total income`, Income.lw)

#Extract LISA test results for Income
Income_noNA$Ii <- lisa.testIncome[,1]
Income_noNA$E.Ii<- lisa.testIncome[,2]
Income_noNA$Var.Ii<- lisa.testIncome[,3]
Income_noNA$Z.Ii<- lisa.testIncome[,4]
Income_noNA$P<- lisa.testIncome[,5]

#Calculate LISA test for Income
lisa.testFrench <- localmoran(French_noNA$PercFrench, French.lw)

#Extract LISA test results for Income
French_noNA$Ii <- lisa.testFrench [,1]
French_noNA$E.Ii<- lisa.testFrench [,2]
French_noNA$Var.Ii<- lisa.testFrench [,3]
French_noNA$Z.Ii<- lisa.testFrench [,4]
French_noNA$P<- lisa.testFrench [,5]
```


Now going back to our basic mapping template we can visualize some of these results to understand what this test is doing (Figure \@ref(fig:MappingLocalMoransI)).


```{r MappingLocalMoransI, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap="Kamloops census dissemination areas showing LISA z-scores for median total income (left) and percentage of respondants with knowledge of french (right)."}
#Map LISA z-scores for Income
map_LISA_Income <- tm_shape(Income_noNA) +
  tm_polygons(col = "Z.Ii",
              title = "Local Moran's I Z-Scores",
              style = "fixed",
              border.alpha = 0.1,
              midpoint = NA,
              colorNA = NULL,
              breaks = c(min(Income_noNA$Z.Ii),-1.96,1.96,max(Income_noNA$Z.Ii)),
              palette = "-RdBu", n = 3)+
  tm_compass(position=c("left", "top"))+
  tm_scale_bar(position=c("left", "bottom"))+
  tm_legend(position = c("right", "top"))

#Map LISA z-scores for French
map_LISA_French <- tm_shape(French_noNA) +
  tm_polygons(col = "Z.Ii",
              title = "Local Moran's I Z-Scores",
              style = "fixed",
              border.alpha = 0.1,
              midpoint = NA,
              colorNA = NULL,
              breaks = c(min(French_noNA$Z.Ii),-1.96,1.96,max(French_noNA$Z.Ii)),
              palette = "-RdBu", n = 3)+
  tm_compass(position=c("left", "top"))+
  tm_scale_bar(position=c("left", "bottom"))+
  tm_legend(position = c("right", "top"))

#Plot maps in a 2 pane figure
tmap_arrange(map_LISA_Income, map_LISA_French, ncol = 2, nrow = 1)
```

Here we have produced a map of the z-scores for the local spatial autocorrelation test for both variables of interest. Remember, to be statistically significant at $\alpha$ = 0.05 the z-score must fall above or below +1.96. We can set up our breaks at these values to fit as much information as possible into the map. This is a great way to visualize the overall results of our test, and in my opinion, the most informative of the local metrics we calculated.


While these maps are great for visualizing where the data is and getting a rough idea of how many polygons are significantly positively or negatively spatially autocorrelated, it can be even more informative to graph these trends (Figure \@ref(fig:MoransIScatter), \@ref(fig:MoransIScatter2)).

```{r MoransIScatter, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap= "Moran's I scatter plot for median total income."}
#Create Moran's I scatter plot for Income
moran.plot(Income_noNA$`Median total income`, Income.lw, zero.policy=TRUE, spChk=NULL, labels=NULL, xlab="Median Total Income ($)", 
           ylab="Spatially Lagged Median Total Income ($)", quiet=NULL)
```


```{r MoransIScatter2, echo=TRUE, eval=TRUE, warning=FALSE, fig.cap= "Moran's I scatter plot for percentage of respondants with knowledge of french."}
#Create Moran's I scatter plot for French
moran.plot(French_noNA$PercFrench, French.lw, zero.policy=TRUE, spChk=NULL, labels=NULL, xlab="Respondants with knowledge of French (%)", 
           ylab="Spatially Lagged knowledge of French (%)", quiet=NULL)
```


In these plots, the points with diamonds are considered statistically significant, and the regression line shows the overall trend. For both plots we can see that the trend shows positive spatial autocorrelation.


## Summary

In this tutorial we introduced some Canadian census data and used it to test for statistical significance in global and local spatial autocorrelation patterns using variations of Moran’s I calculations and Z-tests. In doing so, we also explored data cleaning methods as well as mapping for spatial data, all using ‘R’ programming. We were able to determine that overall, both median total income and percentage of people with french knowledge show positive global spatial autocorrelation, even though on a local scale many regions within Kamloops do not show statistical significance for autocorrelation. 

Hopefully you found this tutorial helpful!

## References

1. TRU Libraries (2019). “History of the Census”. Retrieved from: https://libguides.tru.ca/censuscanada/history

2. Green, D. A., & Milligan, K. (2010). The Importance of the Long Form Census to Canada. Canadian Public Policy, 36(3), 383-388. doi:10.1353/cpp.2010.0001

3. Yihui Xie (2021). knitr: A General-Purpose Package for Dynamic Report Generation
in R. R package version 1.36.
  
4. Bivand, R., Keitt, T., and Rowlingson, B. (2021). rgdal: Bindings for the
'Geospatial' Data Abstraction Library. R package version 1.5-27.
https://CRAN.R-project.org/package=rgdal

5. Tennekes, M. (2018). “tmap: Thematic Maps in R.” _Journal of Statistical Software_, *84*(6), 1-39. doi: 10.18637/jss.v084.i062.

6. Bivand R., Edzer P., Virgilio G. (2013). Applied spatial data analysis with R, Second edition. Springer, NY. http://www.asdar-book.org/4.

7. Hijmans, R. (2021). raster: Geographic Data Analysis and Modeling. R package
version 3.5-2. https://CRAN.R-project.org/package=raster

8. Attali, D. (2021). shinyjs: Easily Improve the User Experience of Your Shiny Apps
in Seconds. R package version 2.1.0. https://CRAN.R-project.org/package=shinyjs
  
9. Meyer, D., Dimitriadou, E., Hornik, K., Weingessel, A., & Leisch, F. (2021). e1071: Misc Functions of the Department of Statistics, Probability Theory Group (Formerly: E1071), TU Wien. R package version 1.7-9. https://CRAN.R-project.org/package=e1071

10. Li, X., Chen, W. Y., & Hin Ting Cho, F. (2020). 3-D spatial hedonic modelling: Environmental impacts of polluted urban river in a high-rise apartment market. Landscape and Urban Planning, 203, 103883.

11. Getis, A., & Aldstadt, J. (2004). Constructing the spatial weights matrix using a local statistic. Geographical Analysis, 36(2), 90-104.

12. Anselin, L. (2020, October 2). Contiguity-based spatial weights. GeoDa. https://geodacenter.github.io/workbook/4a_contig_weights/lab4a.html.

13. Bivand, Roger S. and Wong, David W. S. (2018) Comparing implementations of global and local indicators of spatial association TEST, 27(3), 716-748. URL https://doi.org/10.1007/s11749-018-0599-x

14. Ping, J. L., Green, C. J., Zartman, R. E., & Bronson, K. F. (2004). Exploring spatial dependence of cotton yield using global and local autocorrelation statistics. Field Crops Research, 89(2), 219-236.

15. Tokarz, R., & Novak, R. J. (2018). Spatial-temporal distribution of anopheles larval habitats in uganda using GIS/remote sensing technologies. Malaria Journal, 17(1), 420-420.
