Project 1: Streaming Services
================
Shariq Mallick
3/20/2021

## The Setup

Using the website Kaggle, multiple datasets pertaining to content found
on different streaming services were collected. Two datasets that had
the most similar data to one another were chosen in order to have the
cleanest joining and most reasonable conclusions to the analysis. One
dataset contains details regarding 50 Netflix TV shows, while the other
contains details regarding Amazon Prime TV shows. This data exploration
will focus on rating, year, and audience age. This data is interesting
to me as an exploration of the range of quality on different services
could inform one’s decision on choosing one service over another.

A limitation of this data is that the Netflix data is specifically the
top 50 most popular shows on Netflix while the prime shows are a wider
range. The prime TV shows dataset contain many of the most popular shows
on Amazon Prime, but it also contains many of the less acclaimed shows
as well. It is expected that shows aimed at younger audiences will have
lower IMDB scores than those aimed at older audiences, and that shows
provided in the Netflix data set will see higher IMDB scores.

``` r
prime<-read.csv("D:/Downloads/Rstuff/Prime.csv")
netflix<-read.csv("D:/Downloads/Rstuff/Netflix.csv")

# Due to a lack of numeric variables, another dataset which includes an extra numeric variable for many of the shows being tested is being used.
tv_shows<-read.csv("D:/Downloads/Rstuff/tv_shows.csv")

glimpse(netflix)
```

    ## Rows: 50
    ## Columns: 5
    ## $ Titles      <chr> "Breaking Bad", "Game of Thrones", "Rick and Morty", "Dark~
    ## $ Year        <int> 2008, 2011, 2013, 2017, 2016, 2005, 2010, 2019, 1994, 2005~
    ## $ Rating      <chr> "18+", "18+", "18+", "16+", "16+", "7+", "16+", "18+", "16~
    ## $ IMDB_Rating <dbl> 9.5, 9.3, 9.2, 8.8, 8.8, 9.2, 9.1, 9.4, 8.9, 8.9, 8.7, 9.3~
    ## $ Netflix     <int> 1, 0, 0, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 1, 1~

``` r
glimpse(prime)
```

    ## Rows: 404
    ## Columns: 8
    ## $ S.no.                   <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14,~
    ## $ Name.of.the.show        <chr> "Pataal Lok", "Upload", "The Marvelous Mrs. Ma~
    ## $ Year.of.release         <int> 2020, 2020, 2017, 2019, 2016, 2019, 2018, 2018~
    ## $ No.of.seasons.available <int> 1, 1, 3, 2, 2, 1, 2, 1, 1, 1, 2, 2, 2, 2, 1, 1~
    ## $ Language                <chr> "Hindi", "English", "English", "Hindi", "Engli~
    ## $ Genre                   <chr> "Drama", "Sci-fi comedy", "Drama, Comedy", "Dr~
    ## $ IMDb.rating             <dbl> 7.5, 8.1, 8.7, 5.3, 8.7, 8.3, 7.5, 8.5, 8.6, 8~
    ## $ Age.of.viewers          <chr> "18+", "16+", "16+", "18+", "18+", "18+", "16+~

These datasets contain some unneeded variables for this exploration.
Therefore, the data must be tidied.

\#Citations for data: Bansal, Shivam. “Netflix Movies and TV Shows.”
Kaggle, 18 Jan. 2021, www.kaggle.com/shivamb/netflix-shows/metadata.
Jauhari, Neelima. “Amazon Prime TV Shows.” Kaggle, 13 Oct. 2020,
www.kaggle.com/nilimajauhari/amazon-prime-tv-shows. Bhatia, Ruchi. “TV
Shows on Netflix, Prime Video, Hulu and Disney+.” Kaggle, 25 May 2020,
www.kaggle.com/ruchi798/tv-shows-on-netflix-prime-video-hulu-and-disney.

## Tidying Up and Joining

``` r
# The tidyverse package contains the needed functions to tidy and visualize the data.
library(tidyverse)

# The prime data contains a redundant variable in s.no, and there are several variables that are not contained in the netflix dataset. These must be removed
tdprime<-prime%>%
  select(-S.no.,-Language, -Genre,-No.of.seasons.available)%>%
# The remaining variables need to be renamed to be the same as the netflix data 
  rename(Titles="Name.of.the.show", Year="Year.of.release", IMDB_Rating="IMDb.rating", Rating="Age.of.viewers")%>%
# Missing values need to be removed as well
  drop_na()

# Creating a new variable that labels these shows as being from Prime video will help with categorization when joining
tdprime$Service<-"Prime"
glimpse(tdprime)
```

    ## Rows: 182
    ## Columns: 5
    ## $ Titles      <chr> "Pataal Lok", "Upload", "The Marvelous Mrs. Maisel", "Four~
    ## $ Year        <int> 2020, 2020, 2017, 2019, 2016, 2019, 2018, 2018, 2019, 2019~
    ## $ IMDB_Rating <dbl> 7.5, 8.1, 8.7, 5.3, 8.7, 8.3, 7.5, 8.5, 8.6, 8.0, 8.0, 8.7~
    ## $ Rating      <chr> "18+", "16+", "16+", "18+", "18+", "18+", "16+", "18+", "1~
    ## $ Service     <chr> "Prime", "Prime", "Prime", "Prime", "Prime", "Prime", "Pri~

The netflix dataset is already tidy except for the netflix variable
which denotes whether a show is a netflix original. This data is
unnecessary, but using pivot\_longer this variable can be used to denote
netflix as the streaming service

``` r
tdnetflix<-netflix%>%
  pivot_longer(cols = Netflix, names_to = "Service", values_to = "Netflix")%>%
# Once the new variable was created, the redundant one could be removed
  select(-Netflix)
glimpse(tdnetflix)
```

    ## Rows: 50
    ## Columns: 5
    ## $ Titles      <chr> "Breaking Bad", "Game of Thrones", "Rick and Morty", "Dark~
    ## $ Year        <int> 2008, 2011, 2013, 2017, 2016, 2005, 2010, 2019, 1994, 2005~
    ## $ Rating      <chr> "18+", "18+", "18+", "16+", "16+", "7+", "16+", "18+", "16~
    ## $ IMDB_Rating <dbl> 9.5, 9.3, 9.2, 8.8, 8.8, 9.2, 9.1, 9.4, 8.9, 8.9, 8.7, 9.3~
    ## $ Service     <chr> "Netflix", "Netflix", "Netflix", "Netflix", "Netflix", "Ne~

``` r
# Now that the datasets are more similar, a full join function can be used to merge them together fully
fulldata<-tdnetflix%>%
  full_join(tdprime)
```

    ## Joining, by = c("Titles", "Year", "Rating", "IMDB_Rating", "Service")

In order to have another numeric variable to test, another dataset
containing information on Rotten Tomatoes scores will be joined as well.
Rotten Tomato scores differ from IMDB ratings as RT scores are a measure
of how many positive vs negative critical review there are, while IMDB
ratings are an average of the scores critics gave out of 10.

``` r
RT<-tv_shows%>%
# First the variables were renamed to be easier to type and match the previous data
  rename(Titles="Title",RT="Rotten.Tomatoes")%>%
# Only the RT variable is necessary, while the Titles serve as the key for the joining
  select(Titles,RT)%>%
# Missing values can be removed since they add no useful information to the data
  drop_na()
# Using a leftjoin ensuress only data that is already relevant is added.
fulldata<- fulldata%>%
  left_join(RT,by="Titles")

glimpse(fulldata)
```

    ## Rows: 233
    ## Columns: 6
    ## $ Titles      <chr> "Breaking Bad", "Game of Thrones", "Rick and Morty", "Dark~
    ## $ Year        <int> 2008, 2011, 2013, 2017, 2016, 2005, 2010, 2019, 1994, 2005~
    ## $ Rating      <chr> "18+", "18+", "18+", "16+", "16+", "7+", "16+", "18+", "16~
    ## $ IMDB_Rating <dbl> 9.5, 9.3, 9.2, 8.8, 8.8, 9.2, 9.1, 9.4, 8.9, 8.9, 8.9, 8.7~
    ## $ Service     <chr> "Netflix", "Netflix", "Netflix", "Netflix", "Netflix", "Ne~
    ## $ RT          <dbl> 0.96, NA, 0.94, 0.94, 0.93, 1.00, 0.78, NA, NA, 0.81, 0.96~

Some cases had to be dropped, specifically cases that did not include
IMDB ratings in the prime video dataset as missing values in crucial
numeric variables would make it difficult to explore the data. Later on,
values that are missing in the RT variable may need to be dropped as
well; however, for now it is possible to work around the missing data.
There are cases of multiple RT scores per title, and this is due to the
presence of multiple seasons.

## Data Exploration

``` r
# Create a list of summary statistics for each numeric variable, starting with RT scores
fulldata%>%
# Select for the relevant variables
  select(-Rating,-Titles)%>%
# The summarize function allows one to list out the different statistics
  summarize(mean_RT=mean(RT, na.rm=T), 
            sd_RT=sd(RT, na.rm=T), 
            min_RT=min(RT,na.rm = T),
            max_RT=max(RT,na.rm=T),
# The n funciton simply lists the number of observations included in the data
            n(),
# The standard error is calculated by dividing the sd by the sqrt of n
            se_RT=sd(RT,na.rm = T)/sqrt(n()))%>%
# The kbl function allows for the creation of a clean table for the statistics
  kbl(caption = "RT score Summary Stats")%>%
# The bootstrap options create specific effects to aid in aesthetics
  kable_styling(bootstrap_options = c("striped", "hover","bordered"))
```

<table class="table table-striped table-hover table-bordered" style="margin-left: auto; margin-right: auto;">
<caption>
RT score Summary Stats
</caption>
<thead>
<tr>
<th style="text-align:right;">
mean\_RT
</th>
<th style="text-align:right;">
sd\_RT
</th>
<th style="text-align:right;">
min\_RT
</th>
<th style="text-align:right;">
max\_RT
</th>
<th style="text-align:right;">
n()
</th>
<th style="text-align:right;">
se\_RT
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
0.8195652
</td>
<td style="text-align:right;">
0.1777856
</td>
<td style="text-align:right;">
0.17
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
233
</td>
<td style="text-align:right;">
0.0116471
</td>
</tr>
</tbody>
</table>

The mean is relatively high for the RT scores, with a mean over 80%.
There is a large amount of variation in this variable though, as seen by
the sd of over 17%, which is approximately one quarter of the mean. The
mean being so high suggests, based on the sample, RT scores are
generally scored highly, with a majority above 50%. This suggests that
there is a disparity between reviewers perception of an above average
score and Rotten Tomato’s. If a reviewer scores above a 5/10, Rotten
Tomatoes takes it as a positive score. Positive and negative scores are
looked at as a binary operator, then the proportion of positive scores
to total number of observations is what outputs the score. However, the
sample is made of data coming from more popular shows, which would be
more likely to score highly. This could be the source of the seeming
disparity in this dataset.

``` r
# Repeat the above with IMDB ratings
fulldata%>%
  select(-Rating,-Titles)%>%
  summarize(mean_IMDB=mean(IMDB_Rating, na.rm=T), 
            sd_IMDB=sd(IMDB_Rating, na.rm=T),
            min_IMDB=min(IMDB_Rating,na.rm = T),
            max_IMDB=max(IMDB_Rating,na.rm=T),
            n=n(),
            se_IMDB=sd(IMDB_Rating,na.rm = T)/sqrt(n()))%>%
  kbl(caption = "IMDB Rating Summary Stats")%>%
  kable_styling(bootstrap_options = c("striped", "hover","bordered"))
```

<table class="table table-striped table-hover table-bordered" style="margin-left: auto; margin-right: auto;">
<caption>
IMDB Rating Summary Stats
</caption>
<thead>
<tr>
<th style="text-align:right;">
mean\_IMDB
</th>
<th style="text-align:right;">
sd\_IMDB
</th>
<th style="text-align:right;">
min\_IMDB
</th>
<th style="text-align:right;">
max\_IMDB
</th>
<th style="text-align:right;">
n
</th>
<th style="text-align:right;">
se\_IMDB
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
7.654506
</td>
<td style="text-align:right;">
1.033528
</td>
<td style="text-align:right;">
3.7
</td>
<td style="text-align:right;">
9.5
</td>
<td style="text-align:right;">
233
</td>
<td style="text-align:right;">
0.0677087
</td>
</tr>
</tbody>
</table>

The mean is lower than the RT scores, which is to be expected based on
the difference in how the scores are calculated. The variation is also
lower, so the scores are less likely to reach the extremes in the IMDB
ratings. This can be seen by the fact that the max is less than 10,
while the RT scores maxed at 100%.

``` r
# Repeat the above with a mutated variable that depicts the ratio of the other two numeric variables
fulldata%>%
    select(-Rating,-Titles)%>%
# The RT values are multiplied by ten to scale them to the IMDB ratings properly since the RT is scored out of 100 while the IMDB scores out of 10.
  mutate(RTDBratio=RT*10/IMDB_Rating)%>%
  summarize(mean_ratio=mean(RTDBratio,na.rm=T),
            sd_ratio=sd(RTDBratio,na.rm=T),
            min_ratio=min(RTDBratio,na.rm = T),
            max_ratio=max(RTDBratio,na.rm = T),
            n(),
            se_ratio=sd(RTDBratio,na.rm = T)/sqrt(n()))%>%
  drop_na()%>%
  kbl(caption = "RT*10/IMDB ratio Summary Stats")%>%
  kable_styling(bootstrap_options = c("striped", "hover","bordered"))
```

<table class="table table-striped table-hover table-bordered" style="margin-left: auto; margin-right: auto;">
<caption>
RT\*10/IMDB ratio Summary Stats
</caption>
<thead>
<tr>
<th style="text-align:right;">
mean\_ratio
</th>
<th style="text-align:right;">
sd\_ratio
</th>
<th style="text-align:right;">
min\_ratio
</th>
<th style="text-align:right;">
max\_ratio
</th>
<th style="text-align:right;">
n()
</th>
<th style="text-align:right;">
se\_ratio
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
1.001567
</td>
<td style="text-align:right;">
0.2013255
</td>
<td style="text-align:right;">
0.2575758
</td>
<td style="text-align:right;">
1.371429
</td>
<td style="text-align:right;">
233
</td>
<td style="text-align:right;">
0.0131893
</td>
</tr>
</tbody>
</table>

Looking at the ratio between these two variables, it is apparent that
the RT score tends to be similar to the IMDB score on average, but the
large sd and especially low minimum suggests that RT scores have a much
larger range. Specifically, an RT score that varies greatly from its
IMDB rating usually is much lower than the IMDB score if it is
significantly different.

``` r
# Repeat the above, but this time group the data by the categorical variables
fulldata %>% 
  select(-Rating,-Titles)%>%
# The ratio between the RT score and IMDB rating is an interesting method of measuring critical reception due to the fundamental differences in how the scores are calculated.
  mutate(RTDBratio=RT*10/IMDB_Rating)%>%
# Grouping by Year and Service allows for conclusions to be made regarding the choice of content on each platform and whether they are motivated to pursue the addition of content with higher mass appeal or critical reception 
  group_by(Year,Service) %>% 
# Remove missing values to avoid errors when computing the summary stats
  drop_na()%>%
# Creating summary statistics for mean and sd of the numeric variables allows for 
  summarize(n=n(),
            mean_RT=mean(RT, na.rm=T), 
            sd_RT=sd(RT, na.rm=T), 
            min_RT=min(RT,na.rm = T),
            max_RT=max(RT,na.rm=T),
            se_RT=sd(RT,na.rm = T)/sqrt(n()),
            mean_IMDB=mean(IMDB_Rating, na.rm=T),
            sd_IMDB=sd(IMDB_Rating, na.rm=T),
            min_IMDB=min(IMDB_Rating,na.rm = T),
            max_IMDB=max(IMDB_Rating,na.rm=T),
            mean_ratio=mean(RTDBratio, na.rm=T),
            se_IMDB=sd(IMDB_Rating,na.rm = T)/sqrt(n()),
            sd_ratio=sd(RTDBratio,na.rm=T),
            min_ratio=min(RTDBratio,na.rm = T),
            max_ratio=max(RTDBratio,na.rm = T),
            se_ratio=sd(RTDBratio,na.rm = T)/sqrt(n()))%>%
# Arrange the data by year in order to directly compare the two platforms more easily
  arrange(Year)%>%
  kbl(caption = "TV Summary Stats Grouped by Year and Service")%>%
  kable_styling(bootstrap_options = c("striped", "hover","bordered"))
```

    ## `summarise()` has grouped output by 'Year'. You can override using the `.groups` argument.

<table class="table table-striped table-hover table-bordered" style="margin-left: auto; margin-right: auto;">
<caption>
TV Summary Stats Grouped by Year and Service
</caption>
<thead>
<tr>
<th style="text-align:right;">
Year
</th>
<th style="text-align:left;">
Service
</th>
<th style="text-align:right;">
n
</th>
<th style="text-align:right;">
mean\_RT
</th>
<th style="text-align:right;">
sd\_RT
</th>
<th style="text-align:right;">
min\_RT
</th>
<th style="text-align:right;">
max\_RT
</th>
<th style="text-align:right;">
se\_RT
</th>
<th style="text-align:right;">
mean\_IMDB
</th>
<th style="text-align:right;">
sd\_IMDB
</th>
<th style="text-align:right;">
min\_IMDB
</th>
<th style="text-align:right;">
max\_IMDB
</th>
<th style="text-align:right;">
mean\_ratio
</th>
<th style="text-align:right;">
se\_IMDB
</th>
<th style="text-align:right;">
sd\_ratio
</th>
<th style="text-align:right;">
min\_ratio
</th>
<th style="text-align:right;">
max\_ratio
</th>
<th style="text-align:right;">
se\_ratio
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
1989
</td>
<td style="text-align:left;">
Netflix
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.8500000
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
0.85
</td>
<td style="text-align:right;">
0.85
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
8.700000
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
8.7
</td>
<td style="text-align:right;">
8.7
</td>
<td style="text-align:right;">
0.9770115
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
0.9770115
</td>
<td style="text-align:right;">
0.9770115
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
1990
</td>
<td style="text-align:left;">
Prime
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.8800000
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
0.88
</td>
<td style="text-align:right;">
0.88
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
8.800000
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
8.8
</td>
<td style="text-align:right;">
8.8
</td>
<td style="text-align:right;">
1.0000000
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
1.0000000
</td>
<td style="text-align:right;">
1.0000000
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
1995
</td>
<td style="text-align:left;">
Prime
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1.0000000
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
8.500000
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
8.5
</td>
<td style="text-align:right;">
8.5
</td>
<td style="text-align:right;">
1.1764706
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
1.1764706
</td>
<td style="text-align:right;">
1.1764706
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
1997
</td>
<td style="text-align:left;">
Netflix
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.8100000
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
0.81
</td>
<td style="text-align:right;">
0.81
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
8.700000
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
8.7
</td>
<td style="text-align:right;">
8.7
</td>
<td style="text-align:right;">
0.9310345
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
0.9310345
</td>
<td style="text-align:right;">
0.9310345
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
1999
</td>
<td style="text-align:left;">
Netflix
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.9200000
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
0.92
</td>
<td style="text-align:right;">
0.92
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
9.200000
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
9.2
</td>
<td style="text-align:right;">
9.2
</td>
<td style="text-align:right;">
1.0000000
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
1.0000000
</td>
<td style="text-align:right;">
1.0000000
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
2001
</td>
<td style="text-align:left;">
Netflix
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.9400000
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
0.94
</td>
<td style="text-align:right;">
0.94
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
9.400000
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
9.4
</td>
<td style="text-align:right;">
9.4
</td>
<td style="text-align:right;">
1.0000000
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
1.0000000
</td>
<td style="text-align:right;">
1.0000000
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
2002
</td>
<td style="text-align:left;">
Netflix
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0.8950000
</td>
<td style="text-align:right;">
0.0636396
</td>
<td style="text-align:right;">
0.85
</td>
<td style="text-align:right;">
0.94
</td>
<td style="text-align:right;">
0.0450000
</td>
<td style="text-align:right;">
9.150000
</td>
<td style="text-align:right;">
0.2121320
</td>
<td style="text-align:right;">
9.0
</td>
<td style="text-align:right;">
9.3
</td>
<td style="text-align:right;">
0.9775986
</td>
<td style="text-align:right;">
0.1500000
</td>
<td style="text-align:right;">
0.0468870
</td>
<td style="text-align:right;">
0.9444444
</td>
<td style="text-align:right;">
1.0107527
</td>
<td style="text-align:right;">
0.0331541
</td>
</tr>
<tr>
<td style="text-align:right;">
2004
</td>
<td style="text-align:left;">
Netflix
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0.8750000
</td>
<td style="text-align:right;">
0.0353553
</td>
<td style="text-align:right;">
0.85
</td>
<td style="text-align:right;">
0.90
</td>
<td style="text-align:right;">
0.0250000
</td>
<td style="text-align:right;">
8.500000
</td>
<td style="text-align:right;">
0.2828427
</td>
<td style="text-align:right;">
8.3
</td>
<td style="text-align:right;">
8.7
</td>
<td style="text-align:right;">
1.0292896
</td>
<td style="text-align:right;">
0.2000000
</td>
<td style="text-align:right;">
0.0073443
</td>
<td style="text-align:right;">
1.0240964
</td>
<td style="text-align:right;">
1.0344828
</td>
<td style="text-align:right;">
0.0051932
</td>
</tr>
<tr>
<td style="text-align:right;">
2004
</td>
<td style="text-align:left;">
Prime
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.4000000
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
0.40
</td>
<td style="text-align:right;">
0.40
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
7.800000
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
7.8
</td>
<td style="text-align:right;">
7.8
</td>
<td style="text-align:right;">
0.5128205
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
0.5128205
</td>
<td style="text-align:right;">
0.5128205
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
2005
</td>
<td style="text-align:left;">
Netflix
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
0.9320000
</td>
<td style="text-align:right;">
0.0725948
</td>
<td style="text-align:right;">
0.81
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
0.0324654
</td>
<td style="text-align:right;">
8.840000
</td>
<td style="text-align:right;">
0.2880972
</td>
<td style="text-align:right;">
8.4
</td>
<td style="text-align:right;">
9.2
</td>
<td style="text-align:right;">
1.0547545
</td>
<td style="text-align:right;">
0.1288410
</td>
<td style="text-align:right;">
0.0815185
</td>
<td style="text-align:right;">
0.9101124
</td>
<td style="text-align:right;">
1.1071429
</td>
<td style="text-align:right;">
0.0364562
</td>
</tr>
<tr>
<td style="text-align:right;">
2005
</td>
<td style="text-align:left;">
Prime
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.8300000
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
0.83
</td>
<td style="text-align:right;">
0.83
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
7.600000
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
7.6
</td>
<td style="text-align:right;">
7.6
</td>
<td style="text-align:right;">
1.0921053
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
1.0921053
</td>
<td style="text-align:right;">
1.0921053
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
2006
</td>
<td style="text-align:left;">
Netflix
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.7200000
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
0.72
</td>
<td style="text-align:right;">
0.72
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
8.600000
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
8.6
</td>
<td style="text-align:right;">
8.6
</td>
<td style="text-align:right;">
0.8372093
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
0.8372093
</td>
<td style="text-align:right;">
0.8372093
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
2006
</td>
<td style="text-align:left;">
Prime
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0.8250000
</td>
<td style="text-align:right;">
0.1484924
</td>
<td style="text-align:right;">
0.72
</td>
<td style="text-align:right;">
0.93
</td>
<td style="text-align:right;">
0.1050000
</td>
<td style="text-align:right;">
8.500000
</td>
<td style="text-align:right;">
0.1414214
</td>
<td style="text-align:right;">
8.4
</td>
<td style="text-align:right;">
8.6
</td>
<td style="text-align:right;">
0.9721761
</td>
<td style="text-align:right;">
0.1000000
</td>
<td style="text-align:right;">
0.1908718
</td>
<td style="text-align:right;">
0.8372093
</td>
<td style="text-align:right;">
1.1071429
</td>
<td style="text-align:right;">
0.1349668
</td>
</tr>
<tr>
<td style="text-align:right;">
2007
</td>
<td style="text-align:left;">
Netflix
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.9400000
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
0.94
</td>
<td style="text-align:right;">
0.94
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
8.600000
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
8.6
</td>
<td style="text-align:right;">
8.6
</td>
<td style="text-align:right;">
1.0930233
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
1.0930233
</td>
<td style="text-align:right;">
1.0930233
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
2007
</td>
<td style="text-align:left;">
Prime
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0.7550000
</td>
<td style="text-align:right;">
0.2616295
</td>
<td style="text-align:right;">
0.57
</td>
<td style="text-align:right;">
0.94
</td>
<td style="text-align:right;">
0.1850000
</td>
<td style="text-align:right;">
8.450000
</td>
<td style="text-align:right;">
0.2121320
</td>
<td style="text-align:right;">
8.3
</td>
<td style="text-align:right;">
8.6
</td>
<td style="text-align:right;">
0.8898851
</td>
<td style="text-align:right;">
0.1500000
</td>
<td style="text-align:right;">
0.2872807
</td>
<td style="text-align:right;">
0.6867470
</td>
<td style="text-align:right;">
1.0930233
</td>
<td style="text-align:right;">
0.2031381
</td>
</tr>
<tr>
<td style="text-align:right;">
2008
</td>
<td style="text-align:left;">
Netflix
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.9600000
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
0.96
</td>
<td style="text-align:right;">
0.96
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
9.500000
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
9.5
</td>
<td style="text-align:right;">
9.5
</td>
<td style="text-align:right;">
1.0105263
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
1.0105263
</td>
<td style="text-align:right;">
1.0105263
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
2009
</td>
<td style="text-align:left;">
Netflix
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
0.9175000
</td>
<td style="text-align:right;">
0.0623832
</td>
<td style="text-align:right;">
0.86
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
0.0311916
</td>
<td style="text-align:right;">
8.650000
</td>
<td style="text-align:right;">
0.3109126
</td>
<td style="text-align:right;">
8.4
</td>
<td style="text-align:right;">
9.1
</td>
<td style="text-align:right;">
1.0598500
</td>
<td style="text-align:right;">
0.1554563
</td>
<td style="text-align:right;">
0.0360143
</td>
<td style="text-align:right;">
1.0238095
</td>
<td style="text-align:right;">
1.0989011
</td>
<td style="text-align:right;">
0.0180072
</td>
</tr>
<tr>
<td style="text-align:right;">
2010
</td>
<td style="text-align:left;">
Netflix
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0.7950000
</td>
<td style="text-align:right;">
0.0212132
</td>
<td style="text-align:right;">
0.78
</td>
<td style="text-align:right;">
0.81
</td>
<td style="text-align:right;">
0.0150000
</td>
<td style="text-align:right;">
8.650000
</td>
<td style="text-align:right;">
0.6363961
</td>
<td style="text-align:right;">
8.2
</td>
<td style="text-align:right;">
9.1
</td>
<td style="text-align:right;">
0.9224739
</td>
<td style="text-align:right;">
0.4500000
</td>
<td style="text-align:right;">
0.0923920
</td>
<td style="text-align:right;">
0.8571429
</td>
<td style="text-align:right;">
0.9878049
</td>
<td style="text-align:right;">
0.0653310
</td>
</tr>
<tr>
<td style="text-align:right;">
2010
</td>
<td style="text-align:left;">
Prime
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0.8950000
</td>
<td style="text-align:right;">
0.0636396
</td>
<td style="text-align:right;">
0.85
</td>
<td style="text-align:right;">
0.94
</td>
<td style="text-align:right;">
0.0450000
</td>
<td style="text-align:right;">
8.000000
</td>
<td style="text-align:right;">
0.4242641
</td>
<td style="text-align:right;">
7.7
</td>
<td style="text-align:right;">
8.3
</td>
<td style="text-align:right;">
1.1182131
</td>
<td style="text-align:right;">
0.3000000
</td>
<td style="text-align:right;">
0.0202473
</td>
<td style="text-align:right;">
1.1038961
</td>
<td style="text-align:right;">
1.1325301
</td>
<td style="text-align:right;">
0.0143170
</td>
</tr>
<tr>
<td style="text-align:right;">
2011
</td>
<td style="text-align:left;">
Netflix
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0.8650000
</td>
<td style="text-align:right;">
0.0494975
</td>
<td style="text-align:right;">
0.83
</td>
<td style="text-align:right;">
0.90
</td>
<td style="text-align:right;">
0.0350000
</td>
<td style="text-align:right;">
8.650000
</td>
<td style="text-align:right;">
0.2121320
</td>
<td style="text-align:right;">
8.5
</td>
<td style="text-align:right;">
8.8
</td>
<td style="text-align:right;">
1.0010027
</td>
<td style="text-align:right;">
0.1500000
</td>
<td style="text-align:right;">
0.0817710
</td>
<td style="text-align:right;">
0.9431818
</td>
<td style="text-align:right;">
1.0588235
</td>
<td style="text-align:right;">
0.0578209
</td>
</tr>
<tr>
<td style="text-align:right;">
2011
</td>
<td style="text-align:left;">
Prime
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
0.8866667
</td>
<td style="text-align:right;">
0.0321455
</td>
<td style="text-align:right;">
0.85
</td>
<td style="text-align:right;">
0.91
</td>
<td style="text-align:right;">
0.0185592
</td>
<td style="text-align:right;">
8.233333
</td>
<td style="text-align:right;">
0.5507571
</td>
<td style="text-align:right;">
7.6
</td>
<td style="text-align:right;">
8.6
</td>
<td style="text-align:right;">
1.0815213
</td>
<td style="text-align:right;">
0.3179797
</td>
<td style="text-align:right;">
0.1063309
</td>
<td style="text-align:right;">
0.9883721
</td>
<td style="text-align:right;">
1.1973684
</td>
<td style="text-align:right;">
0.0613902
</td>
</tr>
<tr>
<td style="text-align:right;">
2012
</td>
<td style="text-align:left;">
Prime
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.7700000
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
0.77
</td>
<td style="text-align:right;">
0.77
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
7.400000
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
7.4
</td>
<td style="text-align:right;">
7.4
</td>
<td style="text-align:right;">
1.0405405
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
1.0405405
</td>
<td style="text-align:right;">
1.0405405
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
2013
</td>
<td style="text-align:left;">
Netflix
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
0.9366667
</td>
<td style="text-align:right;">
0.0186190
</td>
<td style="text-align:right;">
0.92
</td>
<td style="text-align:right;">
0.97
</td>
<td style="text-align:right;">
0.0076012
</td>
<td style="text-align:right;">
8.700000
</td>
<td style="text-align:right;">
0.2966479
</td>
<td style="text-align:right;">
8.4
</td>
<td style="text-align:right;">
9.2
</td>
<td style="text-align:right;">
1.0777680
</td>
<td style="text-align:right;">
0.1211060
</td>
<td style="text-align:right;">
0.0458076
</td>
<td style="text-align:right;">
1.0217391
</td>
<td style="text-align:right;">
1.1547619
</td>
<td style="text-align:right;">
0.0187009
</td>
</tr>
<tr>
<td style="text-align:right;">
2013
</td>
<td style="text-align:left;">
Prime
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
0.7733333
</td>
<td style="text-align:right;">
0.2324507
</td>
<td style="text-align:right;">
0.51
</td>
<td style="text-align:right;">
0.95
</td>
<td style="text-align:right;">
0.1342055
</td>
<td style="text-align:right;">
7.466667
</td>
<td style="text-align:right;">
0.4041452
</td>
<td style="text-align:right;">
7.1
</td>
<td style="text-align:right;">
7.9
</td>
<td style="text-align:right;">
1.0276679
</td>
<td style="text-align:right;">
0.2333333
</td>
<td style="text-align:right;">
0.2686712
</td>
<td style="text-align:right;">
0.7183099
</td>
<td style="text-align:right;">
1.2025316
</td>
<td style="text-align:right;">
0.1551174
</td>
</tr>
<tr>
<td style="text-align:right;">
2014
</td>
<td style="text-align:left;">
Netflix
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
0.9600000
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
0.96
</td>
<td style="text-align:right;">
0.96
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
8.900000
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
8.9
</td>
<td style="text-align:right;">
8.9
</td>
<td style="text-align:right;">
1.0786517
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
1.0786517
</td>
<td style="text-align:right;">
1.0786517
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:right;">
2014
</td>
<td style="text-align:left;">
Prime
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
0.7785714
</td>
<td style="text-align:right;">
0.2316915
</td>
<td style="text-align:right;">
0.27
</td>
<td style="text-align:right;">
0.94
</td>
<td style="text-align:right;">
0.0875712
</td>
<td style="text-align:right;">
7.714286
</td>
<td style="text-align:right;">
0.5843189
</td>
<td style="text-align:right;">
6.6
</td>
<td style="text-align:right;">
8.2
</td>
<td style="text-align:right;">
1.0170303
</td>
<td style="text-align:right;">
0.2208518
</td>
<td style="text-align:right;">
0.3042792
</td>
<td style="text-align:right;">
0.3292683
</td>
<td style="text-align:right;">
1.1666667
</td>
<td style="text-align:right;">
0.1150067
</td>
</tr>
<tr>
<td style="text-align:right;">
2015
</td>
<td style="text-align:left;">
Netflix
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
0.9300000
</td>
<td style="text-align:right;">
0.0336650
</td>
<td style="text-align:right;">
0.89
</td>
<td style="text-align:right;">
0.97
</td>
<td style="text-align:right;">
0.0168325
</td>
<td style="text-align:right;">
8.650000
</td>
<td style="text-align:right;">
0.1290994
</td>
<td style="text-align:right;">
8.5
</td>
<td style="text-align:right;">
8.8
</td>
<td style="text-align:right;">
1.0754890
</td>
<td style="text-align:right;">
0.0645497
</td>
<td style="text-align:right;">
0.0469934
</td>
<td style="text-align:right;">
1.0113636
</td>
<td style="text-align:right;">
1.1149425
</td>
<td style="text-align:right;">
0.0234967
</td>
</tr>
<tr>
<td style="text-align:right;">
2015
</td>
<td style="text-align:left;">
Prime
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
0.7888889
</td>
<td style="text-align:right;">
0.2161275
</td>
<td style="text-align:right;">
0.30
</td>
<td style="text-align:right;">
0.97
</td>
<td style="text-align:right;">
0.0720425
</td>
<td style="text-align:right;">
7.922222
</td>
<td style="text-align:right;">
0.4790036
</td>
<td style="text-align:right;">
6.9
</td>
<td style="text-align:right;">
8.5
</td>
<td style="text-align:right;">
0.9909805
</td>
<td style="text-align:right;">
0.1596679
</td>
<td style="text-align:right;">
0.2523602
</td>
<td style="text-align:right;">
0.4000000
</td>
<td style="text-align:right;">
1.1772152
</td>
<td style="text-align:right;">
0.0841201
</td>
</tr>
<tr>
<td style="text-align:right;">
2016
</td>
<td style="text-align:left;">
Netflix
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
0.9000000
</td>
<td style="text-align:right;">
0.0424264
</td>
<td style="text-align:right;">
0.87
</td>
<td style="text-align:right;">
0.93
</td>
<td style="text-align:right;">
0.0300000
</td>
<td style="text-align:right;">
8.500000
</td>
<td style="text-align:right;">
0.4242641
</td>
<td style="text-align:right;">
8.2
</td>
<td style="text-align:right;">
8.8
</td>
<td style="text-align:right;">
1.0588969
</td>
<td style="text-align:right;">
0.3000000
</td>
<td style="text-align:right;">
0.0029397
</td>
<td style="text-align:right;">
1.0568182
</td>
<td style="text-align:right;">
1.0609756
</td>
<td style="text-align:right;">
0.0020787
</td>
</tr>
<tr>
<td style="text-align:right;">
2016
</td>
<td style="text-align:left;">
Prime
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
0.7292308
</td>
<td style="text-align:right;">
0.3005678
</td>
<td style="text-align:right;">
0.17
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
0.0833625
</td>
<td style="text-align:right;">
7.600000
</td>
<td style="text-align:right;">
0.7118052
</td>
<td style="text-align:right;">
6.5
</td>
<td style="text-align:right;">
8.7
</td>
<td style="text-align:right;">
0.9388496
</td>
<td style="text-align:right;">
0.1974192
</td>
<td style="text-align:right;">
0.3648990
</td>
<td style="text-align:right;">
0.2575758
</td>
<td style="text-align:right;">
1.3714286
</td>
<td style="text-align:right;">
0.1012048
</td>
</tr>
<tr>
<td style="text-align:right;">
2017
</td>
<td style="text-align:left;">
Netflix
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
0.9050000
</td>
<td style="text-align:right;">
0.0665833
</td>
<td style="text-align:right;">
0.81
</td>
<td style="text-align:right;">
0.96
</td>
<td style="text-align:right;">
0.0332916
</td>
<td style="text-align:right;">
8.550000
</td>
<td style="text-align:right;">
0.1914854
</td>
<td style="text-align:right;">
8.4
</td>
<td style="text-align:right;">
8.8
</td>
<td style="text-align:right;">
1.0580200
</td>
<td style="text-align:right;">
0.0957427
</td>
<td style="text-align:right;">
0.0656360
</td>
<td style="text-align:right;">
0.9642857
</td>
<td style="text-align:right;">
1.1162791
</td>
<td style="text-align:right;">
0.0328180
</td>
</tr>
<tr>
<td style="text-align:right;">
2017
</td>
<td style="text-align:left;">
Prime
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
0.7860000
</td>
<td style="text-align:right;">
0.1635848
</td>
<td style="text-align:right;">
0.46
</td>
<td style="text-align:right;">
0.96
</td>
<td style="text-align:right;">
0.0517301
</td>
<td style="text-align:right;">
7.870000
</td>
<td style="text-align:right;">
0.7572611
</td>
<td style="text-align:right;">
6.7
</td>
<td style="text-align:right;">
8.7
</td>
<td style="text-align:right;">
0.9998983
</td>
<td style="text-align:right;">
0.2394670
</td>
<td style="text-align:right;">
0.1913492
</td>
<td style="text-align:right;">
0.5974026
</td>
<td style="text-align:right;">
1.2328767
</td>
<td style="text-align:right;">
0.0605099
</td>
</tr>
<tr>
<td style="text-align:right;">
2018
</td>
<td style="text-align:left;">
Prime
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
0.7633333
</td>
<td style="text-align:right;">
0.2256251
</td>
<td style="text-align:right;">
0.50
</td>
<td style="text-align:right;">
0.98
</td>
<td style="text-align:right;">
0.0921110
</td>
<td style="text-align:right;">
7.416667
</td>
<td style="text-align:right;">
0.3763863
</td>
<td style="text-align:right;">
7.0
</td>
<td style="text-align:right;">
8.1
</td>
<td style="text-align:right;">
1.0286277
</td>
<td style="text-align:right;">
0.1536591
</td>
<td style="text-align:right;">
0.3030403
</td>
<td style="text-align:right;">
0.6849315
</td>
<td style="text-align:right;">
1.3066667
</td>
<td style="text-align:right;">
0.1237157
</td>
</tr>
<tr>
<td style="text-align:right;">
2019
</td>
<td style="text-align:left;">
Netflix
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
0.7900000
</td>
<td style="text-align:right;">
0.1311488
</td>
<td style="text-align:right;">
0.67
</td>
<td style="text-align:right;">
0.93
</td>
<td style="text-align:right;">
0.0757188
</td>
<td style="text-align:right;">
8.300000
</td>
<td style="text-align:right;">
0.3605551
</td>
<td style="text-align:right;">
8.0
</td>
<td style="text-align:right;">
8.7
</td>
<td style="text-align:right;">
0.9495129
</td>
<td style="text-align:right;">
0.2081666
</td>
<td style="text-align:right;">
0.1264474
</td>
<td style="text-align:right;">
0.8170732
</td>
<td style="text-align:right;">
1.0689655
</td>
<td style="text-align:right;">
0.0730044
</td>
</tr>
<tr>
<td style="text-align:right;">
2019
</td>
<td style="text-align:left;">
Prime
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
0.7366667
</td>
<td style="text-align:right;">
0.1636663
</td>
<td style="text-align:right;">
0.57
</td>
<td style="text-align:right;">
1.00
</td>
<td style="text-align:right;">
0.0668165
</td>
<td style="text-align:right;">
7.866667
</td>
<td style="text-align:right;">
0.6153590
</td>
<td style="text-align:right;">
6.9
</td>
<td style="text-align:right;">
8.7
</td>
<td style="text-align:right;">
0.9320757
</td>
<td style="text-align:right;">
0.2512192
</td>
<td style="text-align:right;">
0.1646403
</td>
<td style="text-align:right;">
0.7215190
</td>
<td style="text-align:right;">
1.2195122
</td>
<td style="text-align:right;">
0.0672141
</td>
</tr>
<tr>
<td style="text-align:right;">
2020
</td>
<td style="text-align:left;">
Prime
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
0.7100000
</td>
<td style="text-align:right;">
0.0888819
</td>
<td style="text-align:right;">
0.64
</td>
<td style="text-align:right;">
0.81
</td>
<td style="text-align:right;">
0.0513160
</td>
<td style="text-align:right;">
7.600000
</td>
<td style="text-align:right;">
0.4582576
</td>
<td style="text-align:right;">
7.2
</td>
<td style="text-align:right;">
8.1
</td>
<td style="text-align:right;">
0.9318519
</td>
<td style="text-align:right;">
0.2645751
</td>
<td style="text-align:right;">
0.0596837
</td>
<td style="text-align:right;">
0.8888889
</td>
<td style="text-align:right;">
1.0000000
</td>
<td style="text-align:right;">
0.0344584
</td>
</tr>
</tbody>
</table>

There are many missing data points that exist in this table due to these
years only containing a single observation. Without a second data point,
there is no standard deviation or standard error.

## Visualizing Data

A correlation heatmap that shows the similarity between the RT scores
and the IMDB ratings would be a suitable way of visualizing the ratio
between the two that is displayed by the ratio variables above.

``` r
fulldata_num<-fulldata%>%
# Mutate to include the ratio from above
  mutate(RTDBratio=RT*10/IMDB_Rating)%>%
# Select for numeric variables
  select_if(is.numeric)
# Create heatmap
cor(fulldata_num,use = "pairwise.complete.obs")%>%
# Save as data frame
  as.data.frame()%>%
# Convert rownames to an explicit variable
  rownames_to_column()%>%
# Pivot so that all the correlations appear in the same column
  pivot_longer(-1, names_to = "other_var",values_to = "correlation")%>%
# Use geom_tile
  ggplot(aes(rowname, other_var, fill=correlation)) + geom_tile() + ggtitle("TV Correlation Heatmap")
```

![](Project-1_files/figure-gfm/visuals-1.png)<!-- -->

This heatmap is largely uninformative due to the low number of numeric
variables in this dataset. Interestingly the ratio correlates more
strongly with the RT scores than the IMDB ratings. This is likely due to
the larger degree of variation in the RT scores more heavily affecting
the ratio, or it could simply be due to the fact that the RT scores are
the numerator of the ratio.

``` r
fulldata%>%
# Group by the categorical variables to be depicted/explored
  group_by(Year,Service)%>%
# Use the summarize function to output means for each chosen categorical variable
  summarize(mean_RT=mean(RT, na.rm=T))%>%
# Drop Missing values to avoid gaps or errors in the data
  drop_na()%>%
# Use ggplot to plot the desired variables
  ggplot(aes(x=Year,y=mean_RT,color=Service))+
# Adjust the size to increase visibility
  geom_line(size=1,stat = "summary")+
# Set a theme to customize the background
  theme_linedraw() + 
# Choose a title
  ggtitle("Mean RT Scores Based on Year of Release") +
# Add points to make individual observations more visible
  geom_point() + 
  scale_color_manual(values=c("dark red","blue"))
```

    ## `summarise()` has grouped output by 'Year'. You can override using the `.groups` argument.

    ## No summary function supplied, defaulting to `mean_se()`

![](Project-1_files/figure-gfm/visuals2-1.png)<!-- -->

This line graph depicts the average Rotten Tomatoes score for each
platform based on the year of the shows original release. Netflix has a
more consistent output over time, which suggests that they search for
content that has high audience appeal. Earlier analysis showed that
there was a greater variation in the Prime Video dataset, so it is
likely that this visualization is being skewed negatively for RT scores
for the Prime platform due to imperfect sampling.

``` r
fulldata%>%
# Group by the categorical variables to be depicted/explored
  group_by(Year,Service)%>%
# Use the summarize function to output means for each chosen categorical variable
  summarize(mean_IMDB=mean(IMDB_Rating, na.rm=T))%>%
# Drop Missing values to avoid gaps or errors in the data
  drop_na()%>%
# Use ggplot to plot the desired variables
  ggplot(aes(x=Year,y=mean_IMDB,color=Service))+
# Adjust the size to increase visibility
  geom_line(size=1,stat = "summary")+
# Set a theme to customize the background
  theme_linedraw() + 
# Choose a title
  ggtitle("Mean IMDB Scores based on Year of Release") +
# Add points to make individual observations more visible
  geom_point() + 
  scale_color_manual(values=c("dark red","blue"))
```

    ## `summarise()` has grouped output by 'Year'. You can override using the `.groups` argument.

    ## No summary function supplied, defaulting to `mean_se()`

![](Project-1_files/figure-gfm/visuals3-1.png)<!-- -->

By creating a similar graph for the IMDB variable, a direct comparison
between the two numeric variables can be made. It is clear that IMDB
scores trend significantly lower in the Prime data as compared to the
Netflix data. Prime seems to hold a much higher quantity of content from
a larger range of time, but the quality of this content is less
consistent. Rotten Tomatoes scores seem to have a greater degree of
variation in general as the lowest scores are much lower, while the
highest are much greater.

## Kmeans

``` r
# Use the function kmeans to find 3 clusters
kmeans1 <- fulldata %>%
# Select the numeric variables being tested
  select(RT,IMDB_Rating)%>%
# Drop NA values to avoid errors
  drop_na()%>%
# Use the kmeans function to calculate the data
  kmeans(3)
kmeans1
```

    ## K-means clustering with 3 clusters of sizes 37, 25, 53
    ## 
    ## Cluster means:
    ##          RT IMDB_Rating
    ## 1 0.8037838    8.024324
    ## 2 0.6664000    7.128000
    ## 3 0.9028302    8.726415
    ## 
    ## Clustering vector:
    ##   [1] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 1 1 3 3 3 1 3 3 3 3 3 3 3 3
    ##  [38] 3 3 3 3 1 1 3 1 3 3 2 1 3 2 2 1 2 1 1 2 3 1 1 3 3 3 1 3 1 1 1 1 2 2 2 3 1
    ##  [75] 2 1 1 3 2 2 1 1 1 1 3 1 1 2 1 2 1 2 1 1 2 2 1 2 1 1 2 2 2 1 2 3 3 2 1 2 1
    ## [112] 1 2 3 2
    ## 
    ## Within cluster sum of squares by cluster:
    ## [1] 2.609578 4.135176 4.156894
    ##  (between_SS / total_SS =  80.6 %)
    ## 
    ## Available components:
    ## 
    ## [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
    ## [6] "betweenss"    "size"         "iter"         "ifault"

``` r
# Save cluster assignment as a column in your dataset
kmeansclust <- fulldata %>%
# Select the variables used for the clustering as well as any categorical variable that is to be tested as well
  select(RT,IMDB_Rating,Service)%>%
# Remove missing values
  drop_na() %>%
# Add the cluster data from kmean1 as a factor to the data
  mutate(cluster=as.factor(kmeans1$cluster))

# Make a plot of data colored by final cluster assignment
kmeansclust %>%
# Use color to depict the cluster, and shape to depict the other categorical variable being tested
  ggplot(aes(RT,IMDB_Rating,color = cluster,shape=Service)) +
# A scatterplot is the clearest visual to depict cluster data
  geom_point() +
# Add titles and labels
  ggtitle("Kmeans of IMDB and RT Scores") +
  ylab("IMDB Ratings")+
  xlab("Rotten Tomatoes Scores")+
# Edit colors
  scale_color_manual(values=c("dark red","blue","black"))
```

![](Project-1_files/figure-gfm/kmeans-1.png)<!-- -->

The kmeans cluster visual exhibits the generally positive correlation
between the two numeric variables, but it is clear that low IMDB ratings
can still acheive relatively high Rotten Tomatoes scores. Cluster 3 is
entirely composed of Prime Video shows, and occupies the lower IMDB
values, but the full range of RT scores. The second cluster is
predominantly Prime Video shows with a handful of Netflix shows as well,
with similar trends to cluster 3 regarding the range of RT scores but
about a full point greater IMDB ratings. The first cluster is
predominantly Netflix shows with a handful of Prime shows as well, with
high values in both numeric variables.
