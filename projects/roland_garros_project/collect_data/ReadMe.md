### Read Me

This repo contains a bunch of raw web scraping and json processing scripts.

`catalogue_player_ids`
* Create data frame of player names and associated player IDs (Roland Garros)
* Note: Need to do the same for Australian Open 

`.\EDA\explore_a_json_file`
* Explore what information is available in a JSON file

`.\EDA\process_a_json_file`
* Exploratory script on writing functions to create tidy datasets
* Tidied functions are added to src `.py` file

`.\scraping\scrape_rolland_garros`
* Scrape roland garros site for tracking data in JSON form

`.\scraping\scrape_australian_open`
* Scrape Australian Open site for tracking data in JSON form

`tidy_all_singles_matches`
* Use functions in src `.py` file to tidy all json files into one .csv file (kind of like what Jeff Sackmann does)
* Create separate files for ATP and WTA matches.

`feature_engineering_in_R.R`
* Some extra feature engineering items, done in R
* Eg: Adding player handedness, point importance, etc