<!--
%\VignetteEngine{knitr::knitr}
%\VignetteIndexEntry{Introduction to pollstR}
-->




# R client for the Huffpost Pollster API

This R package is an interface to the Huffington Post [Pollster API](http://elections.huffingtonpost.com/pollster/api), which provides access to opinion polls collected by the Huffington Post.

The package is released under GPL-2 and the API data it accesses is released under the [Creative Commons Attribution-NonCommercial-ShareAlike 3.0 Unported License](http://creativecommons.org/licenses/by-nc-sa/3.0/deed.en_US).


# API Overview

The Pollster API has two primary data structures: charts and polls.

*Polls* are individual, dated topline results for a single set of candidates in a single race.
The poll data structure consists of (generally) named candidates and percentage support for each, along with additional information (e.g., polling house, sampling frame, sample size, margin of error, state, etc.).

*Charts* aggregate polls for a particular race or topic (e.g., "2012-president" or "obama-job-approval".
A chart reports aggregated survey estimates of support for the candidates in a given race and, possibly, daily estimates of support.
Each chart is named, reports the number of aggregated polls it presents, a last-updated date, and a "slug" field. The "slug" identifies the chart both in the API and on the Pollster website.

In ``pollstR``, there are three functions in that provide access to the opinion polls and model estimates from Huffpost Pollster.

- ``pollstr_charts``: Get a list of all charts and the current model estimates.
- ``pollstr_chart``: Get a single chart along with historical model estimates.
- ``pollstr_polls``: Get opinion poll data.

## Charts

To get a list of all the charts in the API use the function ``pollstr_charts``,

```r
charts <- pollstr_charts()
str(charts)
```

```
#> List of 2
#>  $ charts   :'data.frame':	497 obs. of  9 variables:
#>   ..$ title        : Factor w/ 497 levels "2012 Iowa GOP Primary",..: 1 2 3 4 5 6 7 8 9 10 ...
#>   ..$ slug         : Factor w/ 496 levels "2012-iowa-gop-primary",..: 1 2 3 4 5 6 7 8 9 10 ...
#>   ..$ topic        : Factor w/ 28 levels "2012-president-gop-primary",..: 1 1 1 1 1 1 1 1 2 3 ...
#>   ..$ state        : Factor w/ 53 levels "IA","NH","SC",..: 1 2 3 4 5 6 7 8 8 8 ...
#>   ..$ short_title  : Factor w/ 497 levels "1/3 Iowa Caucus",..: 1 2 3 4 5 6 7 8 9 10 ...
#>   ..$ election_date: Date[1:497], format: NA ...
#>   ..$ poll_count   : int [1:497] 65 55 44 59 10 34 19 258 589 300 ...
#>   ..$ last_updated : POSIXct[1:497], format: "2012-01-02 13:08:44" ...
#>   ..$ url          : Factor w/ 496 levels "http://elections.huffingtonpost.com/pollster/2012-iowa-gop-primary",..: 1 2 3 4 5 6 7 8 9 10 ...
#>  $ estimates:'data.frame':	1245 obs. of  8 variables:
#>   ..$ choice         : chr [1:1245] "Romney" "Paul" "Santorum" "Gingrich" ...
#>   ..$ value          : num [1:1245] 22.5 21.3 15.9 12.6 11.1 8.3 3.7 5.9 0.9 39.6 ...
#>   ..$ lead_confidence: num [1:1245] NA NA NA NA NA NA NA NA NA NA ...
#>   ..$ first_name     : chr [1:1245] "Mitt" "Ron" "Rick" "Newt" ...
#>   ..$ last_name      : chr [1:1245] "Romney" "Paul" "Santorum" "Gingrich" ...
#>   ..$ party          : chr [1:1245] "Rep" "Rep" "Rep" "Rep" ...
#>   ..$ incumbent      : logi [1:1245] FALSE FALSE FALSE FALSE FALSE FALSE ...
#>   ..$ slug           : chr [1:1245] "2012-iowa-gop-primary" "2012-iowa-gop-primary" "2012-iowa-gop-primary" "2012-iowa-gop-primary" ...
#>  - attr(*, "class")= chr "pollstr_charts"
```
This returns a ``list`` with two data frames.
The data frame ``charts`` has data on each chart,
while the data frame ``estimates`` has the current poll-tracking estimates from each chart.

The query can be filtered by state or topic.
For example, to get only charts related to national topics,

```r
us_charts <- pollstr_charts(state = "US")
```

## Chart

To get a particular chart use the function ``pollstr_chart``.
For example, to get the chart for [Barack Obama's Favorable Rating](http://elections.huffingtonpost.com/pollster/obama-favorable-rating), specify its *slug*, ``obama-favorable-rating``.

```r
obama_favorable <- pollstr_chart('obama-favorable-rating')
print(obama_favorable)
```

```
#> Title:       Barack Obama Favorable Rating 
#> Chart Slug:  obama-favorable-rating 
#> Topic:       favorable-ratings 
#> State:       US 
#> Polls:       844 
#> Updated:     1414529680 
#> URL:         http://elections.huffingtonpost.com/pollster/obama-favorable-rating 
#> Estimates:
#>        choice value lead_confidence first_name last_name party incumbent
#> 1   Favorable  44.3              NA         NA        NA    NA        NA
#> 2 Unfavorable  49.3              NA         NA        NA    NA        NA
#> 3   Undecided   4.7              NA         NA        NA    NA        NA
#> 4     Refused   1.5              NA         NA        NA    NA        NA
#> 
#> First 6 (of 2236) daily estimates:
#>        choice value       date
#> 1   Favorable  44.3 2014-10-21
#> 2 Unfavorable  49.3 2014-10-21
#> 3   Favorable  44.3 2014-10-20
#> 4 Unfavorable  49.3 2014-10-20
#> 5   Undecided   4.7 2014-10-20
#> 6     Refused   1.5 2014-10-20
```
The slug can be found from the results of a ``pollstr_charts`` query.
Alternatively the slug is the path of the url of a chart, http://elections.huffingtonpost.com/pollster/obama-favorable-rating.

The historical estimates of the Huffpost Pollster poll-tracking model are contained in the element ``"estimates_by_date"``,

```r
(ggplot(obama_favorable[["estimates_by_date"]], aes(x = date, y = value, color = choice))
 + geom_line())
```

![plot of chunk obama-favorable-chart](inst/vign-src/figures/obama-favorable-chart-1.png) 

## Polls

To get the opinion poll results use the function ``pollstr_polls`.
The polls returned can be filtered by topic, chart, state, or date.

By default, ``pollstr_polls`` only returns 1 page of results (about 10 polls).
To have it return more polls, increase the value of ``max_pages``.
To have it return all polls, set the value of ``max_pages`` to a very high number.
For example, to return all the polls on the favorability of Bararck Obama after March 1, 2014,

```r
obama_favorable_polls <- pollstr_polls(max_pages = 10000, chart = 'obama-favorable-rating', after = "2014-3-1")
str(obama_favorable_polls)	
```

```
#> List of 4
#>  $ polls        :'data.frame':	55 obs. of  9 variables:
#>   ..$ id          : int [1:55] 21021 21151 20893 20905 20884 20791 20790 20686 20590 20492 ...
#>   ..$ pollster    : chr [1:55] "Democracy Corps (D)" "AP-GfK (Web)" "FOX" "YouGov/Economist" ...
#>   ..$ start_date  : Date[1:55], format: "2014-10-16" ...
#>   ..$ end_date    : Date[1:55], format: "2014-10-21" ...
#>   ..$ method      : chr [1:55] "Live Phone" "Internet" "Live Phone" "Internet" ...
#>   ..$ source      : chr [1:55] "http://www.democracycorps.com/attachments/article/990/102114_DCOR_fq_for%20website.pdf" "http://surveys.ap.org/data/GfK/AP-GfK_Poll_October_2014_Topline_Politics.pdf" "http://www.foxnews.com/politics/interactive/2014/10/15/fox-news-polls-us-voters-weigh-in-on-isis-and-ebola/" "http://d25d2506sfb94s.cloudfront.net/cumulus_uploads/document/3ahawzfzp3/econToplines.pdf" ...
#>   ..$ last_updated: POSIXct[1:55], format: "2014-10-24 17:20:07" ...
#>   ..$ partisan    : chr [1:55] "Pollster" "Nonpartisan" "Nonpartisan" "Nonpartisan" ...
#>   ..$ affiliation : chr [1:55] "Dem" "None" "None" "None" ...
#>  $ questions    :'data.frame':	4215 obs. of  14 variables:
#>   ..$ question       : chr [1:4215] "2014 National House Race" "2014 National House Race" "2014 National House Race" "2014 National House Race" ...
#>   ..$ chart          : chr [1:4215] "2014-national-house-race" "2014-national-house-race" "2014-national-house-race" "2014-national-house-race" ...
#>   ..$ topic          : chr [1:4215] "2014-house" "2014-house" "2014-house" "2014-house" ...
#>   ..$ state          : chr [1:4215] "US" "US" "US" "US" ...
#>   ..$ subpopulation  : chr [1:4215] "Likely Voters" "Likely Voters" "Likely Voters" "Likely Voters" ...
#>   ..$ observations   : int [1:4215] 698 698 698 698 698 698 698 698 698 698 ...
#>   ..$ margin_of_error: num [1:4215] -3 -3 -3 -3 -3 -3 -3 -3 -3 -3 ...
#>   ..$ choice         : chr [1:4215] "Democrat" "Republican" "Other" "Undecided" ...
#>   ..$ value          : int [1:4215] 46 46 1 5 1 25 68 7 44 52 ...
#>   ..$ first_name     : chr [1:4215] NA NA NA NA ...
#>   ..$ last_name      : chr [1:4215] NA NA NA NA ...
#>   ..$ party          : chr [1:4215] "Dem" "Rep" NA NA ...
#>   ..$ incumbent      : logi [1:4215] FALSE FALSE NA NA NA NA ...
#>   ..$ id             : int [1:4215] 21021 21021 21021 21021 21021 21021 21021 21021 21021 21021 ...
#>  $ survey_houses:'data.frame':	67 obs. of  3 variables:
#>   ..$ name : chr [1:67] "Greenberg Quinlan Rosner Research (D)" "Gfk" "Anderson Robbins Research" "Shaw & Company Research" ...
#>   ..$ party: chr [1:67] "Dem" "N/A" "Dem" "Rep" ...
#>   ..$ id   : int [1:67] 21021 21151 20893 20893 20905 20884 20884 20791 20790 20686 ...
#>  $ sponsors     :'data.frame':	60 obs. of  3 variables:
#>   ..$ name : chr [1:60] "Associated Press" "FOX News" "Economist" "NBC News" ...
#>   ..$ party: chr [1:60] "N/A" "N/A" "N/A" "N/A" ...
#>   ..$ id   : int [1:60] 21151 20893 20905 20884 20884 20791 20790 20686 20590 20492 ...
#>  - attr(*, "class")= chr "pollstr_polls"
```


# Errors

If you encounter an error when using one of the functions it is likely that there was an error in converting the data as returned by the API into data structures more usable in R.
The conversion code is fragile, and can break when there are changes in APIs, or from weird cases that I didn't anticipate.
So if you encounter an error, try running the function with `convert = FALSE`; this will return the data as returned by the API.
If there is no error, then it is problem with the conversion code in this package.
Using `convert = FALSE`, you will still be able to get the data from the Pollster API, but you will need to do the extra data cleaning yourself.
To get the bug fixed, open a new issue on [github](https://github.com/rOpenGov/pollstR/issues).


<!--  LocalWords:  API APIs github
 -->


# Example: Obama's Job Approval Rating

This section shows how to use ``pollstr`` to create a chart similar to those displayed on the Huffpost Pollster website.
I'll use Obama's job approval rating in this example.

The slug or name of the chart is ``obama-job-approval``, which is derived from the chart's URL , http://elections.huffingtonpost.com/pollster/obama-job-approval.
I'll focus on approval in 2013 in order to reduce the time necessary to run this code.

```r
slug <- "obama-job-approval"
start_date <- as.Date("2013-1-1")
end_date <- as.Date("2014-1-1")
```
For the plot, I'll need both Pollster's model estimates and opinion poll estimates.
I get the Pollster model estimates using ``polster_chart``,

```r
chart <- pollstr_chart(slug)
estimates <- chart[["estimates_by_date"]]

estimates <- estimates[estimates$date >= start_date 
                       & estimates$date < end_date, ]
```
and the opinion poll results,

```r
polls <- pollstr_polls(chart = slug, 
                        after = start_date, 
                        before = end_date,
                        max_pages = 1000000)
```
Note that in ``polster_poll`` I set the ``max_pages`` argument to a very large number in order to download all the polls available.
This may take several minutes.

Before continuing, we will need to clean up the opinion poll data.
First, only keep results from national subpopulations ("Adults", "Likely Voters", "Registered Voters").
This will drop subpopulations like Republicans, Democrats, and Independents.

```r
questions <-
    subset(polls[["questions"]],
           chart == slug
           & subpopulation %in% c("Adults", "Likely Voters", "Registered Voters"))
```
Second, I will need to recode the choices into three categories, "Approve", "Disapprove", and "Undecided".

```r
approvalcat <- c("Approve" = "Approve",
                 "Disapprove" = "Disapprove",
                 "Undecided" = "Undecided",
                 "Neither" = "Undecided",
                 "Refused" = NA,
                 "Neutral" = "Undecided",
                 "Strongly Approve" = "Approve",
                 "Somewhat Approve" = "Approve", 
                 "Somewhat Disapprove" = "Disapprove",
                 "Strongly Disapprove" = "Disapprove")

questions2 <-
    (questions
     %.% mutate(choice = plyr::revalue(choice, approvalcat))
     %.% group_by(id, subpopulation, choice)
     %.% summarise(value = sum(value)))
```

```
#> Warning: %.% is deprecated. Please use %>%
```

```
#> Warning: %.% is deprecated. Please use %>%
```

```
#> Warning: %.% is deprecated. Please use %>%
```
Now merge the question data with the poll metadata,

```r
polldata <- merge(polls$polls, questions2, by = "id")
```

Now, I can plot the opinion poll results along with the Huffpost Pollster trend estimates,

```r
(ggplot()
 + geom_point(data = polldata,
              mapping = aes(y = value, x = end_date, color = choice),
              alpha = 0.3)
 + geom_line(data = estimates,
             mapping = aes(y = value, x = date, color = choice))
 + scale_x_date("date")
 + scale_color_manual(values = c("Approve" = "black", 
                                 "Disapprove" = "red", 
                                 "Undecided" = "blue"))
 )
```

![plot of chunk obama-favorable-chart-2](inst/vign-src/figures/obama-favorable-chart-2-1.png) 

<!--  LocalWords:  Huffpost API Huffington CRAN github devtools str
 -->
<!--  LocalWords:  devools jrnold ggplot obama url aes favorability
 -->
<!--  LocalWords:  Bararck
 -->

<!--  LocalWords:  Huffpost API Huffington CRAN github devtools str
 -->
<!--  LocalWords:  devools jrnold ggplot obama url aes favorability
 -->
<!--  LocalWords:  Bararck
 -->