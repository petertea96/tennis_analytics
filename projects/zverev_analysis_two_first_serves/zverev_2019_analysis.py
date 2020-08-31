# -*- coding: utf-8 -*-
"""
Created on Sat Jun 13 17:19:30 2020

@author: Peter

* Analysis on the 2019 ATP Season
* Calculate player percentiles for serve stats (Consider only players
  who played at least 15 matches)
* --> Consider matches between top 100 ranked players
* Obtain percentiles on some tennis stats
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

################################################
# --> Working directories
import os
print(os.getcwd())
path="C:/Users/Peter/Documents/zverev_analysis/"
os.chdir(path)
################################################

# --> read in data
dat = pd.read_csv('./data/atp_fulldata.csv', encoding='latin1')


# --> look at only the 2019 season
data_2019 = dat[dat.year == 2019]

# --> double faults per second serves
data_2019['df_per_ss'] = data_2019['s_df'] / (data_2019['s_2ndIn'] + data_2019['s_df'])



################################################################################
################################################################################
# Serve stats
# Average stats
data_2019_mean = data_2019.groupby(['server']).mean()

# Add No. of matches played by each player
match_counts = data_2019.groupby(['server']).size()
data_2019_mean['num_matches'] = match_counts.values

# Consider only players who played at least 15 matches?
#data_2019_mean = data_2019_mean[data_2019_mean.num_matches >= 15]

# Consider only players who are ranked in top 100
# Most top players will be playing against top players, so ot shouldn't
# matter who we filter by their opponent
data_2019_mean = data_2019_mean[(data_2019_mean.server_rank <= 100) ]
#data_2019_mean = data_2019_mean[(data_2019_mean.server_rank <= 100) & (data_2019_mean.returner_rank <= 100)]

#--> Get [percentile rankings] on only numeric columns
numerics = ['int16', 'int32', 'int64', 'float16', 'float32', 'float64']
data_2019_mean_numeric = data_2019_mean.select_dtypes(include=numerics)

# --> If we wanted to look at percetile rankings:
#res = data_2019_mean_numeric.rank(pct=True)*100

# --> We'll just use raw numbers
res = data_2019_mean_numeric.copy()

# --> Keep only players ranked in top 10
# How does zverev compare to the top 10?
top10_names = data_2019_mean[data_2019_mean['server_rank'] <=10].index.unique()

top10 = res[res.index.isin(top10_names)]
top10['name'] = top10.index
top10 = top10[['name','pr_1stin', 'pr_2ndin','pr_w1_giv_1in', 
               'pr_w2_giv_2in', 'pr_win_on_1st_serve','pr_win_on_2nd_serve',
                'pr_win_on_serve', 'pr_win_two_first_serves']]
top10.to_csv('./data/top10.csv',
             index = False)
# --> plots in R
################################################################################
################################################################################


################################################################################
################################################################################
# How would Zverev's percentile ranking change if he used two first serves?

# Add in Fake Zverev (to see where his 2nd serve win percentile lands w/ 2 first serves)
fake_dat = data_2019_mean_numeric.copy()
fake_dat.reset_index(inplace = True)

fake_dat.iloc[78, -3] = fake_dat.iloc[78, -4] 


res2 = fake_dat.pr_win_on_2nd_serve.rank(pct=True)*100
res2 = pd.DataFrame(res2)
res2['server'] = fake_dat['server']
pd.set_option('display.max_columns', 500)
res2[res2.server =='Alexander Zverev']


################################################################################
################################################################################

################################################################################
################################################################################
# What are Zverev' worst double fault matches?
zverev = data_2019[data_2019.server == 'Alexander Zverev']
awful = zverev[zverev.s_df >=15]
################################################################################
################################################################################




##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
# 'A Risky serve' plot (same as seen in fivethirtyeight)
##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
risky_adv_dat = data_2019_mean.reset_index()

# Only conider players ranked top 100
risky_adv_dat = risky_adv_dat[risky_adv_dat['server_rank'] <=100]

# --> Who are above the line?
risky_adv_dat[risky_adv_dat['pr_win_on_1st_serve'] >  risky_adv_dat['pr_win_on_2nd_serve']]
# Zverev, FAA, Kyrgios, Querrey

# --> What is Zverev's percentage difference?
tosee = risky_adv_dat[risky_adv_dat.server == 'Alexander Zverev']
tosee['pr_win_on_1st_serve'] - tosee['pr_win_on_2nd_serve']
# 4.4% increase

plt.figure(figsize=(25,15))
fig=plt.figure()
ax=fig.add_axes([0,0,1,1])
ax.scatter(risky_adv_dat['pr_win_on_2nd_serve'],
           risky_adv_dat['pr_win_on_1st_serve'],
           color='r',
           edgecolor = 'black',
           alpha = 0.6)
ax.set_xlabel('Observed Prob(Win on 2nd Serve)', fontweight = 'bold')
ax.set_ylabel('Prob(Win on 2nd with Risky Serve)', fontweight = 'bold')
ax.set_title('Balance of a Risky Serve', 
             fontweight = 'bold',
             fontsize = 16
             )
ax.plot([0, 1], [0, 1], ls="--", c=".3")
ax.set(xlim=(0.37, 0.63), ylim=(0.37, 0.63))


zverev_coord = (risky_adv_dat[risky_adv_dat.server == 'Alexander Zverev']['pr_win_on_2nd_serve'].values- 0.026,
            risky_adv_dat[risky_adv_dat.server == 'Alexander Zverev']['pr_win_on_1st_serve'].values)

faa_coord = (risky_adv_dat[risky_adv_dat.server == 'Felix Auger Aliassime']['pr_win_on_2nd_serve'].values-0.055,
            risky_adv_dat[risky_adv_dat.server == 'Felix Auger Aliassime']['pr_win_on_1st_serve'].values)

kyrgios_coord = (risky_adv_dat[risky_adv_dat.server == 'Nick Kyrgios']['pr_win_on_2nd_serve'].values + 0.0,
            risky_adv_dat[risky_adv_dat.server == 'Nick Kyrgios']['pr_win_on_1st_serve'].values + 0.0085)

querrey_coord = (risky_adv_dat[risky_adv_dat.server == 'Sam Querrey']['pr_win_on_2nd_serve'].values + 0.0,
            risky_adv_dat[risky_adv_dat.server == 'Sam Querrey']['pr_win_on_1st_serve'].values + 0.0085)


#isner_coord = (risky_adv_dat[risky_adv_dat.server == 'John Isner']['actual_2nd_serve_win_percent'].values,
#            risky_adv_dat[risky_adv_dat.server == 'John Isner']['new_strategy'].values)


ax.annotate('Zverev', zverev_coord, fontweight = 'bold',
            fontsize = 9,
            bbox=dict(facecolor='yellow', edgecolor='black', boxstyle='round'))
ax.annotate('Auger-Aliassime', faa_coord,fontweight = 'bold',
            fontsize = 9,
            bbox=dict(facecolor='yellow', edgecolor='black', boxstyle='round'))
ax.annotate('Kyrgios', kyrgios_coord,fontweight = 'bold',
            fontsize = 9,
            bbox=dict(facecolor='yellow', edgecolor='black', boxstyle='round'))
ax.annotate('Querrey', querrey_coord,fontweight = 'bold',
            fontsize = 9,
            bbox=dict(facecolor='yellow', edgecolor='black', boxstyle='round'))
#ax.annotate('Isner', isner_coord)
plt.show()
##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
# 'A Risky serve' plot (same as seen in fivethirtyeight)
##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 

# Zverev's double faults per second serve
np.sum(zverev['s_df']) / np.sum((zverev['s_svpt'] -zverev['s_1stIn'] ))

