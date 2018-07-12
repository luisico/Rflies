## Install and load packages

## install.packages(c("knitr", "tidyverse", "tidyr", "dplyr", "readxl", "janitor", "survival", "survminer", "data.table"))

library(tidyr)
library(dplyr)
library(readxl)
library(janitor)
library(survival)
library(survminer)
library(data.table)
library(tools)

flies_process = function(inputfile, worksheet = 'Data', skip_unaccounted = FALSE) {
    # Import and transform data
    input = flies_import(inputfile, worksheet, skip_unaccounted)
    data = flies_transform(input)

    # Create datasets
    initial = flies_get_initial(data)
    final =   flies_get_final(data)
    journal = flies_get_journal(data)

    # Create tally and events
    tally = flies_tally(initial, final, journal)
    events = flies_generate_events(journal, tally, skip_unaccounted)

    ## The total number of events should be:
    ##expected_events = tally %>% mutate(events = (dead + escaped + unaccounted)) %>% group_by() %>% summarise_if(is.numeric, sum) %>% select(events)

    ## Export to prism
    flies_export_prism(events, inputfile, worksheet)

    ## Return a list with the tally and the events datasets
    list(tally = tally, events = events)
}

### Import data from Excel
flies_import = function(inputfile, worksheet, skip_unaccounted) {
    read_excel(inputfile, sheet = worksheet)
}

### Transform data (reshape and clean)
flies_transform = function(input) {
    ## - Reshape the date column into rows (removing blanks).
    ## - Identify values marked with ":".
    ## - Split dead and escaped events marked with "+".
    ## - Set correct formats for group, tube, dead, escaped and date columns,
    ## - Calculating day passed per group/tube.

    ## TODO:
    ## - Remove flies0
    ## - Manage initial dates marked with ":"

    input %>%
        gather(date, flies0, -tube, -group, na.rm=T) %>%
        separate(flies0, c("marked", "flies"), ":", remove=T, fill="left") %>%
        separate(flies, c("dead", "escaped"), "[+]", fill="right") %>%
        mutate(group = as.factor(group),
               tube = as.factor(tube),
               dead = as.numeric(dead),
               escaped = as.numeric(escaped),
               date = excel_numeric_to_date(as.numeric(date))) %>%
        group_by(group, tube) %>% mutate(days = as.integer(date - min(date, na.rm=T)))
}

### Initial flies per group/tube.
flies_get_initial =  function(data) {
    data %>% filter(days == 0) %>% select(-marked, -escaped) %>% rename(initial = dead)
}

### Final flies per group/tube.
flies_get_final = function(data) {
    data %>% filter(!is.na(marked)) %>% select(-marked, -escaped) %>% rename(final = dead)
}

### Journal of count of events per day.
flies_get_journal = function(data) {
    data %>% filter(days != 0) %>% filter(is.na(marked)) %>% select(-marked)
}

### Create summary tally
flies_tally = function(initial, final, journal) {
    ## - By group/tube for calculating unaccounted flies per tube
    ## - By group for inspection

    tally = initial %>% select(-date, -days) %>%
        full_join(
            journal %>% summarise(dead = sum(dead, na.rm=T), escaped = sum(escaped, na.rm=T)),
            by = c("group", "tube")
        ) %>%
        full_join(
            final %>% select(-date, -days),
            by = c("group", "tube")
        ) %>%
        mutate_all(~ if_else(is.na(as.numeric(.x)), 0, as.numeric(.x))) %>%
        mutate(unaccounted = abs((final + dead + escaped) - initial))

    bind_rows(
        tally %>% group_by(group) %>% summarise_if(is.numeric, sum),
        tally %>% group_by() %>% summarise_if(is.numeric, sum)
    )
}

### Construct events dataset for use in survival analysis packages
flies_generate_events = function(journal, tally, skip_unaccounted) {
    ## - Expand rows with dead and escaped (censored) events
    ## - Add censored events for flies unaccounted for

    events = bind_rows(
        journal %>%                                       # dead flies: 1
        select(-escaped) %>%
        filter(dead > 0) %>%
        uncount(dead, .id="fly") %>%
        mutate(event=as.integer(1)),

        journal %>%                                       # escape flies: 0
        select(-dead) %>%
        filter(!is.na(escaped)) %>%
        uncount(escaped, .id="fly") %>%
        mutate(event=as.integer(0)),

        tally %>%                                         # unaccounted flies: 0  (-1 if skip_unaccounted=TRUE)
        select(group, unaccounted) %>%
        filter(unaccounted > 0) %>%
        uncount(unaccounted, .id="fly") %>%
        mutate(days=max(journal$days), event=ifelse(skip_unaccounted, -1, 0), date=as.Date(NA))
    )

    events %>% filter(event == 0 | event == 1)
}

### Export to Prism
flies_export_prism = function(events, inputfile, worksheet) {
    prismfile = paste(paste(file_path_sans_ext(inputfile), worksheet, "prism", sep="_"), ".csv", sep="")
    prism = events %>%
        group_by(group) %>%
        mutate(id = 1:n()) %>%
        dcast(group+tube+days+fly+event+id ~ group, value.var="event", fill="") %>%
        select(-group, -tube, -fly, -event, -id)

    write.csv(prism, prismfile)
}

### Create survival object and graph
flies_graph = function(events) {
    ## Create a survival object and fit by group and output the median and CI intervals
    survival = survfit(Surv(days, event) ~ group, data=events)
    surv_median(survival)

    ## Create a pretty graph
    ggsurvplot(survival, data=events, xlab="Days", palette="jco")
    ggsurvplot(survival, data=events, xlab="Days", pval=T, conf.int=T, surv.median.line="hv", palette="jco")

    ## Test for differences
    survdiff(Surv(days, event) ~ group, data=events)
}
