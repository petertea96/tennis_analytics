README

Title: `When Two First Serves are Better than One`
Subtitle: Zverev 2nd serves Project

1. save_oncourt_data.R
- Script queries the onCourt databases and scrapes data in an analyzable format
'atp_fulldata.csv'
- Added columns with direct Pr(overall win on serve) and pr(win overall w/ 2 first serves)
6/14/2020

2. zverev_2019_analysis.py
- Percentile rankings of players for each of their numerical columns
- Produce 'top10.csv' file
- Calculate what Zverev's Prob(win on 2nd serve) percentile would be if
he adopted a two first serves approach (21st -> 66th percentile)
- Find Zverev's worst double fault matches (20 vs. Kecmanovic)
- Calculate difference of Zverev's pr(win on 1st) - pr(win on 2nd)
- Two first serves plot (fivethirtyeight mimic plot)
'atp_2019_two_first_sreves.png'

- !!! decided to scrap percentiles, and instead plot actual probability values
6/14/20
- Decided to keep 'risky serve plot' focused only on 2nd serve rather than
the the overall win prob. Overall win prob masks 2nd serve inefficiencies. 

3. ranking_plots.R
- Percentile ranking for first serve probs, 2nd serve probs and overall probs
- Uses 'top10.csv' produced in (2)
- Plotted raw probabilities instead of ranks
6/14/20

3. zverev_career_stats.py
- Script calculates Zverev's average stats eash year on tour. Purpose is to see how he's fluctuated year-to-year. Second Serve In, in particular saw a drastic dip from 2018 --> 2019


- Calculate double faults / second serve attempt for each Zverev year
- Calculate number of matches played each year 
- Calculate Zverev double faults / second serve in AO 2020,
and for entire atp field.

- Plotly plot of career 2nd serve probabilities (from 2015 - 2020) 
'zverev_history_2nd.html'

- Ditto plot for career 1st serve probabilities
'zverev_history_1st.html'

- Median Filter plot of Zverev's 2nd serve marksmanship from 2018 - 2020
'zverev_recent_2ndin.html'

- Plot Zverev's career two first serve advantages (2019 and 2020 are
 the only seasons with clear advantages)
'zverev_career_two_first_serves.png'

