Exercise_5\_trial_1
================

``` r
data_path <- "C:/Users/abhid/Documents/Courses/ORGB_690_People_Analytics/Exercise_5_AM/"
exercise_gs_1 <- read_csv(paste0(data_path,"examiner_gs.csv"))
```

    ## Rows: 52109 Columns: 7
    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (4): examiner_name_last, examiner_name_first, start_date, end_date
    ## dbl (3): examiner_grade, old_pid, new_pid
    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

## 1. Data cleaning

Changed the data by separating the name column into first and last name
using comma deliminator. Assumed that the last name is before the comma
and first name after the comma. This assumption was made based on
familiarity with some Indian names in the list. Looking at the data,
majority of the values associated with the maximum examiner grade is not
available. Therefore removing the rows with null values.

``` r
exercise_gs_1 <- na.omit(exercise_gs_1)
```

## 2. Determine gender

The gender function returned a lot of null values and therefore removing
all the null values. Although the package could identify 10-15% of the
names and therefore the insights derived from these may not be
representative of the entire population

``` r
examiner_names <- exercise_gs_1 %>% 
  distinct(examiner_name_first)

examiner_names_gender <- examiner_names %>% 
  do(results = gender(.$examiner_name_first, method = "ssa")) %>% 
  unnest(cols = c(results), keep_empty = TRUE) %>% 
  select(
    examiner_name_first = name,
    gender
  )

exercise_gs_1 <- exercise_gs_1 %>% 
  left_join(examiner_names_gender, by = "examiner_name_first")
exercise_gs_1 <- na.omit(exercise_gs_1)
```

## 3. Determine race

``` r
examiner_surnames <- exercise_gs_1 %>% 
  select(surname = examiner_name_last) %>% 
  distinct()

examiner_race <- predict_race(voter.file = examiner_surnames, surname.only = T) %>% 
  as_tibble()
```

    ## [1] "Proceeding with surname-only predictions..."

    ## Warning in merge_surnames(voter.file): Probabilities were imputed for 339
    ## surnames that could not be matched to Census list.

``` r
examiner_race <- examiner_race %>% 
  mutate(max_race_p = pmax(pred.asi, pred.bla, pred.his, pred.oth, pred.whi)) %>% 
  mutate(race = case_when(
    max_race_p == pred.asi ~ "Asian",
    max_race_p == pred.bla ~ "black",
    max_race_p == pred.his ~ "Hispanic",
    max_race_p == pred.oth ~ "other",
    max_race_p == pred.whi ~ "white",
    TRUE ~ NA_character_
  ))

examiner_race <- examiner_race %>% 
  select(surname,race)
exercise_gs_1 <- exercise_gs_1 %>% 
  left_join(examiner_race, by = c("examiner_name_last" = "surname"))
```

## 4. Determine tenure

Calculating the number of days an examiner spends within a particular
examiner grade

``` r
exercise_gs_1 <- exercise_gs_1 %>% 
  mutate(start_date = as_date(mdy(start_date)), end_date = as_date(mdy(end_date)))

exercise_gs_1 <- exercise_gs_1 %>%
  mutate(tenure_days = interval(start_date, end_date) %/% days(1))
```

## 5. Determine the average tenure days

Determining the average tenure days for each examiner grade and
averaging the same across gender.

``` r
grade_level_gender_data <- exercise_gs_1 %>% 
  group_by(examiner_grade,gender) %>% 
  summarise(
    people_count = n(),
    start_date = min(start_date, na.rm = TRUE),
    end_date = max(end_date, na.rm = TRUE),
    tenure_days = mean(tenure_days, na.rm = TRUE)
  )
```

    ## `summarise()` has grouped output by 'examiner_grade'. You can override using
    ## the `.groups` argument.

``` r
ggplot(grade_level_gender_data) +
  geom_bar(
    aes(x=as_factor(examiner_grade),y=tenure_days, fill = gender), 
    position = "dodge", stat = "identity"
    ) + 
  xlab("Examiner grade")
```

![](exercise_5_AM_files/figure-gfm/avg%20tenure_gender-1.png)<!-- -->
Conclusion : The average number of days an examiner spends within each
grade before progressing to the next grade is almost similar for both
genders in the lower grades. Whereas for higher grades, females seem to
spend slightly more days in one grade than men before progressing into
the next grade. Also an interesting observation could be made wrt the
earliest start date for the examiners with each grade that is for each
grade earliest male has started either at the same time or earlier than
the earliest females in each grade.

Limitations : The biggest limitation to the above analysis could be
stemmed from three operations on the data. First one stems from assuming
the first and last names. Since this data was not linked with the
app_sample_data, the first and last name based on assuming conventional
format will invite unnecessary errors in identification of gender and
race. Second limitation stems from removing the unavailable data on the
number of days in the final grade. If a person is a new joiner at
earlier grade or is in the highest grade, removing the row associated
with maximum grade might remove an examiner completely which will
produce skewed results. Third one is from removing the data where the
gender could not be determined from first names as adding this filter
removed almost 85% of the data and thus whatever inferences we derive
will not be representative of the entire population.

Assumption : The grades are progressing from lower to higher i.e an
examiner progressing from 5 will have to 7 and that the grades cannot be
skipped.

## 6. Determining avg tenure days across race

Determining the average tenure days for each examiner grade and
averaging the same across race.

``` r
grade_level_race_data <- exercise_gs_1 %>% 
  group_by(examiner_grade,race) %>% 
  summarise(
    start_date = min(start_date, na.rm = TRUE),
    end_date = max(end_date, na.rm = TRUE),
    tenure_days = mean(tenure_days, na.rm = TRUE)
  )
```

    ## `summarise()` has grouped output by 'examiner_grade'. You can override using
    ## the `.groups` argument.

``` r
ggplot(grade_level_race_data) +
  geom_bar(
    aes(x=as_factor(examiner_grade),y=tenure_days, fill = race), 
    position = "dodge", stat = "identity"
    ) + 
  xlab("Examiner grade")
```

![](exercise_5_AM_files/figure-gfm/avg%20tenure_race-1.png)<!-- -->

Conclusion : Based on the observed graph, similar to the gender spread
across examiner grades there is minimal difference between examiners of
various races except for people belonging to black race.

Limitaions : Similar to the ones explained above for gender
