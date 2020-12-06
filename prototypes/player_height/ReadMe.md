# ReadMe

---
How much is player height (and weight) an advantage while serving?

This project aims to: 
* Plot Ace Rates vs. Aces allowed, adding in height as a colour aesthetic
* Data will be collected in the 2010 decade
---

## `notebook_collate_data.R`
* Script that calls all functions from `src` and collates all data necessary to produce analysis.

## `notebook_analysis.R`
* Script that produces data visualizations

## `src`

### scrape_official_atp_height.ipynb & scrape_official_height

* These scripts collect official listed height and weight data for many active and past players.

Acquire all links from a webpage --loop through and collect all the data

https://towardsdatascience.com/quickly-extract-all-links-from-a-web-page-using-javascript-and-the-browser-console-49bb6f48127b

* uses data/raw_data/atp_official_site_urls.csv
* outputs: data/processed_data/official_atp_height_2020.csv

### abbreviated_name.R
 * Function to convert full name to an abbreviated name
 * Ex: Peter Tea --> P. Tea

### process_data.R
* Function to process Raw Tennis Abstract Data into a tidy format
* Function to obtain end-of-yer rankings

## `processed_data`
* end_of_year_atp_rankings.csv: The end of year rankings for each year since 1990







