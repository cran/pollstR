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
#>  $ charts   :'data.frame':	444 obs. of  9 variables:
#>   ..$ title        : chr [1:444] "2012 Iowa GOP Primary" "2012 New Hampshire GOP Primary" "2012 South Carolina GOP Primary" "2012 Florida GOP Primary" ...
#>   ..$ slug         : chr [1:444] "2012-iowa-gop-primary" "2012-new-hampshire-gop-primary" "2012-south-carolina-gop-primary" "2012-florida-gop-primary" ...
#>   ..$ topic        : chr [1:444] "2012-president-gop-primary" "2012-president-gop-primary" "2012-president-gop-primary" "2012-president-gop-primary" ...
#>   ..$ state        : Factor w/ 51 levels "IA","NH","SC",..: 1 2 3 4 5 6 7 8 8 8 ...
#>   ..$ short_title  : chr [1:444] "1/3 Iowa Caucus" "1/10 New Hampshire Primary" "1/21 South Carolina Primary" "1/31 Florida Primary" ...
#>   ..$ election_date: Date[1:444], format: NA ...
#>   ..$ poll_count   : int [1:444] 65 55 44 59 10 34 19 258 589 300 ...
#>   ..$ last_updated : POSIXct[1:444], format: "2012-01-02 13:08:44" ...
#>   ..$ url          : chr [1:444] "http://elections.huffingtonpost.com/pollster/2012-iowa-gop-primary" "http://elections.huffingtonpost.com/pollster/2012-new-hampshire-gop-primary" "http://elections.huffingtonpost.com/pollster/2012-south-carolina-gop-primary" "http://elections.huffingtonpost.com/pollster/2012-florida-gop-primary" ...
#>  $ estimates:'data.frame':	1051 obs. of  8 variables:
#>   ..$ choice         : chr [1:1051] "Romney" "Paul" "Santorum" "Gingrich" ...
#>   ..$ value          : num [1:1051] 22.5 21.3 15.9 12.6 11.1 8.3 3.7 5.9 0.9 39.6 ...
#>   ..$ lead_confidence: num [1:1051] NA NA NA NA NA NA NA NA NA NA ...
#>   ..$ first_name     : chr [1:1051] "Mitt" "Ron" "Rick" "Newt" ...
#>   ..$ last_name      : chr [1:1051] "Romney" "Paul" "Santorum" "Gingrich" ...
#>   ..$ party          : Factor w/ 6 levels "Dem","Gre","ind",..: 6 6 6 6 6 6 6 NA NA 6 ...
#>   ..$ incumbent      : logi [1:1051] FALSE FALSE FALSE FALSE FALSE FALSE ...
#>   ..$ slug           : chr [1:1051] "2012-iowa-gop-primary" "2012-iowa-gop-primary" "2012-iowa-gop-primary" "2012-iowa-gop-primary" ...
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
#> State:       1 
#> Polls:       805 
#> Updated:     1.399e+09 
#> URL:         http://elections.huffingtonpost.com/pollster/obama-favorable-rating 
#> Estimates:
#>             choice value lead_confidence first_name last_name party
#> 1        Favorable  45.6              NA       <NA>      <NA>  <NA>
#> 2      Unfavorable  48.5              NA       <NA>      <NA>  <NA>
#> 3        Undecided   4.1              NA       <NA>      <NA>  <NA>
#> 4          Neutral  12.6              NA       <NA>      <NA>  <NA>
#> 5 Not Heard Enough   0.2              NA       <NA>      <NA>  <NA>
#>   incumbent
#> 1        NA
#> 2        NA
#> 3        NA
#> 4        NA
#> 5        NA
#> 
#> First 6 (of 2127) daily estimates:
#>        choice value       date
#> 1   Undecided   4.1 2014-04-27
#> 2     Neutral  12.6 2014-04-27
#> 3 Unfavorable  48.5 2014-04-27
#> 4   Favorable  45.6 2014-04-27
#> 5   Undecided   4.1 2014-04-21
#> 6 Unfavorable  48.6 2014-04-21
```

The slug can be found from the results of a ``pollstr_charts`` query.
Alternatively the slug is the path of the url of a chart, http://elections.huffingtonpost.com/pollster/obama-favorable-rating.

The historical estimates of the Huffpost Pollster poll-tracking model are contained in the element ``"estimates_by_date"``,

```r
(ggplot(obama_favorable[["estimates_by_date"]], aes(x = date, y = value, color = choice))
 + geom_line())
```

![plot of chunk obama-favorable-chart](figures/obama-favorable-chart.png) 


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
#> List of 2
#>  $ polls    :'data.frame':	16 obs. of  9 variables:
#>   ..$ id           : int [1:16] 19357 19316 19256 19261 19252 19239 19169 19132 19137 19172 ...
#>   ..$ pollster     : Factor w/ 8 levels "NBC/WSJ","YouGov/Economist",..: 1 2 3 2 4 2 2 2 5 6 ...
#>   ..$ start_date   : Date[1:16], format: "2014-04-23" ...
#>   ..$ end_date     : Date[1:16], format: "2014-04-27" ...
#>   ..$ method       : Factor w/ 2 levels "Phone","Internet": 1 2 1 2 1 2 2 2 2 1 ...
#>   ..$ source       : chr [1:16] "http://msnbcmedia.msn.com/i/MSNBC/Sections/A_Politics/14271_APRIL_NBC_WSJ_Poll.pdf" "http://d25d2506sfb94s.cloudfront.net/cumulus_uploads/document/lx2kkwdvcu/econToplines.pdf" "http://www.foxnews.com/politics/interactive/2014/04/21/fox-news-poll-independents-more-likely-to-back-anti-obamacare-candidates"| __truncated__ "http://d25d2506sfb94s.cloudfront.net/cumulus_uploads/document/8cnxcwv20i/econToplines.pdf" ...
#>   ..$ last_updated : POSIXct[1:16], format: "2014-04-30 20:01:58" ...
#>   ..$ survey_houses: chr [1:16] "" "" "" "" ...
#>   ..$ sponsors     : chr [1:16] "" "" "" "" ...
#>  $ questions:'data.frame':	1042 obs. of  14 variables:
#>   ..$ question       : Factor w/ 33 levels "2014 National House Race",..: 1 1 1 2 2 2 2 3 3 3 ...
#>   ..$ chart          : Factor w/ 31 levels "2014-national-house-race",..: 1 1 1 31 31 31 31 21 21 21 ...
#>   ..$ topic          : Factor w/ 7 levels "","2014-house",..: 2 2 2 1 1 1 1 7 7 7 ...
#>   ..$ state          : Factor w/ 1 level "US": 1 1 1 1 1 1 1 1 1 1 ...
#>   ..$ subpopulation  : Factor w/ 12 levels "Registered Voters",..: 1 1 1 2 2 2 2 2 2 2 ...
#>   ..$ observations   : int [1:1042] NA NA NA 1000 1000 1000 1000 1000 1000 1000 ...
#>   ..$ margin_of_error: num [1:1042] NA NA NA 3.1 3.1 3.1 3.1 3.1 3.1 3.1 ...
#>   ..$ choice         : chr [1:1042] "Democrat" "Republican" "Undecided" "Right Direction" ...
#>   ..$ value          : num [1:1042] 45 45 10 27 63 6 4 44 50 6 ...
#>   ..$ first_name     : chr [1:1042] NA NA NA NA ...
#>   ..$ last_name      : chr [1:1042] NA NA NA NA ...
#>   ..$ party          : Factor w/ 3 levels "Dem","ind","Rep": 1 3 NA NA NA NA NA NA NA NA ...
#>   ..$ incumbent      : logi [1:1042] FALSE FALSE NA NA NA NA ...
#>   ..$ id             : int [1:1042] 19357 19357 19357 19357 19357 19357 19357 19357 19357 19357 ...
#>  - attr(*, "class")= chr "pollstr_polls"
```




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

![plot of chunk obama-favorable-chart-2](figures/obama-favorable-chart-2.png) 

```r

```


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