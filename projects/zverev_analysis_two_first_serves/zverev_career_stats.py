# -*- coding: utf-8 -*-
"""
Created on Fri Jun 12 20:59:56 2020

@author: Peter
"""

# Script calculates Zverev's average stats eash year on tour
# Purpose is to see how he's fluctuated year-to-year
# Second Serve In, in particular saw a drastic dip from 2018 --> 2019

# --> Load libraries
import pandas as pd
import numpy as np

################################################
# --> Working directories
import os
print(os.getcwd())
path="C:/Users/Peter/Documents/zverev_analysis/"
os.chdir(path)
################################################

# --> read in data
dat = pd.read_csv('./data/atp_fulldata.csv', encoding='latin1')

# --> Data processing steps:
# --> Some player names include parantheses...let's remove the
# parantheses and only have characers in the names
tosee=dat[dat.server.str.find('(') != -1]
dat.server = dat.server.str.replace(r"\(.*\)","")
dat.returner = dat.returner.str.replace(r"\(.*\)","")

# Fix time in data
dat['match_date'] = pd.to_datetime(dat['match_date'], format='%Y-%m-%d')
dat['year'] = pd.DatetimeIndex(dat['match_date']).year 

zverev=dat[dat['server'] == 'Alexander Zverev']
zverev.sort_values(['match_date'], inplace = True)

# Zverev Average stats per year
# !!! This is against ALL players 
# --> Haven't filtered to ensure quality opponent
zverev_by_year = zverev.groupby(['year']).mean().reset_index()

# --> Add number of matches played each year by Zverev
year_counts = zverev.groupby(['year']).size().values
zverev_by_year['num_matches'] = year_counts

# --> Add double faults per second serve attempt
# !!! Is this the correct method?
zverev_double_faults_by_year = zverev.groupby(['year']).s_df.sum() /  (zverev.groupby(['year']).s_2ndIn.sum() + zverev.groupby(['year']).s_df.sum() ).values
zverev_by_year['df_per_ss'] = zverev_double_faults_by_year.values


#--> Calculate zverev df/second serve in 2020 
zverev2020 = zverev[zverev.year == 2020]

np.sum(zverev2020['s_df']) / np.sum((zverev2020['s_svpt'] -zverev2020['s_1stIn'] ))

#--> Atp field df/second serve
np.sum(dat['s_df']) / np.sum((dat['s_svpt'] -dat['s_1stIn'] ))

#--> Averev AO 2020 df /ss
zverev2020AO = zverev2020[zverev2020.tournament_name == 'Australian Open - Melbourne']
np.sum(zverev2020AO['s_df']) / np.sum((zverev2020AO['s_svpt'] -zverev2020AO['s_1stIn'] ))




# --> Only consider Zverev play in 2015 and afterwards
# (year where he regularly played ATP talent)
zverev_to_plot = zverev_by_year[zverev_by_year.year.isin(list(range(2015,2021)))]


###########
### PLOTS
###########

from plotly.offline import plot
import plotly.graph_objects as go

# ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
### Zverev Career 2nd serve statistics
# ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
zverev_career_2nd = go.Figure()
zverev_career_2nd.add_trace(go.Scatter(x=zverev_to_plot['year'],
                            y=zverev_to_plot['pr_2ndin'],
              marker_color = 'green',
              name = 'Prob(2nd Serve In)',
              line_shape = 'spline',
              mode = 'markers+lines',
              marker=dict(line=dict(width=1.5)),
              #name = 'Lower Bound',
              line = dict(width = 1),
              ))
zverev_career_2nd.add_trace(go.Scatter(x=zverev_to_plot['year'],
                            y=zverev_to_plot['pr_win_on_2nd_serve'],
              marker_color = '#FFA500',
              name = 'Prob(Win on 2nd Serve)',
              line_shape = 'spline',
              mode = 'markers+lines',
              marker=dict(line=dict(width=1.5)),
              #name = 'Lower Bound',
              line = dict(width = 1),
              ))
zverev_career_2nd.add_trace(go.Scatter(x=zverev_to_plot['year'],
                            y=zverev_to_plot['pr_w2_giv_2in'],
              marker_color = '#0f52ba',
              name = 'Prob(Win | 2nd Serve In)',
              line_shape = 'spline',
              mode = 'markers+lines',
              marker=dict(line=dict(width=1.5)),
              #name = 'Lower Bound',
              line = dict(width = 1),
              ))
zverev_career_2nd.update_layout(
    showlegend = True,
    barmode = 'overlay',
    title = dict(text = "<b>Zverev's Career <br> Second Serve Performance  </b>", # title of plot,
                    xanchor = 'center',
                    x = 0.5,
                    y = 0.95),
    legend = dict(font = dict(size=12)),
       xaxis_title_text="Year", # xaxis label
       yaxis_title_text='Probability', # yaxis label
       font = dict(size = 16),
       hovermode = 'closest',
       plot_bgcolor = "#FFF",
       xaxis_linecolor = "#BCCCDC",
       hoverlabel=dict(
               bgcolor="white", 
               bordercolor = 'black',
               font_size=14, 
               #font_family="Rockwell"
    )) 
zverev_career_2nd.update_traces(opacity=0.95,
                  hovertemplate='Year: %{x} <br>Probability: %{y}')
plot(zverev_career_2nd, filename = './plots/zverev_history_2nd.html')
# ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
### END Zverev Career 2nd serve statistics
# ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 



# ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
### Zverev Career 1st serve statistics
# ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
zverev_career_1st = go.Figure()
zverev_career_1st.add_trace(go.Scatter(x=zverev_to_plot['year'],
                            y=zverev_to_plot['pr_1stin'],
              marker_color = 'purple',
              name = 'Prob(1st Serve In)',
              line_shape = 'spline',
              mode = 'markers+lines',
              marker=dict(line=dict(width=1.5)),
              #name = 'Lower Bound',
              line = dict(width = 1),
              ))
zverev_career_1st.add_trace(go.Scatter(x=zverev_to_plot['year'],
                            y=zverev_to_plot['pr_w1_giv_1in'],
              marker_color = 'red',
              name = 'Prob(Win | 1st Serve In)',
              line_shape = 'spline',
              mode = 'markers+lines',
              marker=dict(line=dict(width=1.5)),
              #name = 'Lower Bound',
              line = dict(width = 1),
              ))
zverev_career_1st.add_trace(go.Scatter(x=zverev_to_plot['year'],
                            y=zverev_to_plot['pr_win_on_1st_serve'],
              marker_color = 'blue',
              name = 'Prob(Win on 1st Serve)',
              line_shape = 'spline',
              mode = 'markers+lines',
              marker=dict(line=dict(width=1.5)),
              #name = 'Lower Bound',
              line = dict(width = 1),
              ))
zverev_career_1st.update_layout(
    showlegend = True,
    barmode = 'overlay',
    title = dict(text = "<b>Zverev's Career <br> First Serve Performance  </b>", # title of plot,
                    xanchor = 'center',
                    x = 0.5,
                    y = 0.95),
    legend = dict(font = dict(size=12)),
       xaxis_title_text="Year", # xaxis label
       yaxis_title_text='Probability', # yaxis label
       font = dict(size = 16),
       hovermode = 'closest',
       plot_bgcolor = "#FFF",
       xaxis_linecolor = "#BCCCDC",
       hoverlabel=dict(
               bgcolor="white", 
               bordercolor = 'black',
               font_size=14, 
               #font_family="Rockwell"
    )) 
zverev_career_1st.update_traces(opacity=0.95,
                  hovertemplate='Year: %{x} <br>Probability: %{y}')
plot(zverev_career_1st, filename = './plots/zverev_history_1st.html')
# ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
### END Zverev Career 1st serve statistics
# ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 


##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
# Zverev 20182020 Median Filter 2nd serve accuracy (2018 - 2020)
##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
# --> Look at trends of 2nd serve
zverev_past = zverev[zverev.year.isin(list(range(2018,2021)))]
zverev_past.sort_values('match_date', ascending = True, inplace = True)

#godly performance at 2018 Madrid Open
zverev_madrid_2018_19 = zverev_past[(zverev_past.tournament_name == 'Mutua Madrid Open - Madrid') &
                                 (zverev_past.year.isin([2018,2019]))]

# --> Zverev Time series Serve statistics using Median Filter
zverev_past['pr_2ndin_mf'] = zverev_past.pr_2ndin.rolling(window = 5, center = True).median()

# --> add 1 sd confidence band

zverev_past['pr_2ndin_mf_sd'] = zverev_past.pr_2ndin.rolling(window = 5, center = True).std()
zverev_past['pr_2ndin_mf_up'] = zverev_past['pr_2ndin_mf'] + zverev_past['pr_2ndin_mf_sd']
# --> Ensure 1 is the maximum
zverev_past['pr_2ndin_mf_up'] = np.where(zverev_past['pr_2ndin_mf_up'] > 1, 1, zverev_past['pr_2ndin_mf_up'])


zverev_2ndin_accuracy = go.Figure()
# Pr(2nd in)
zverev_2ndin_accuracy.add_trace(go.Scatter(x=zverev_past['match_date'],
                            y=zverev_past['pr_2ndin_mf'] - zverev_past['pr_2ndin_mf_sd'],
              marker_color = '#98FB98',
              mode = 'lines',
              #name = 'Lower Bound',
              line = dict(width = 1),
              name='Lower Bound',
              hovertemplate =
              'Match Date: %{x} <br>Probability: %{y}' 
              ))
zverev_2ndin_accuracy.add_trace(go.Scatter(x=zverev_past['match_date'],
                            y= zverev_past['pr_2ndin_mf_up'],
              marker_color = '#98FB98',
              #name = 'Upper Bound',
              line = dict(width = 1),
              fillcolor = '#98FB98',
              fill = 'tonexty',
              name = 'Upper Bound',
              hovertemplate =
              'Match Date: %{x} <br>Probability: %{y}' 
              ))
zverev_2ndin_accuracy.add_trace(go.Scatter(x=zverev_past['match_date'],
                            y=zverev_past['pr_2ndin_mf'],
              marker_color = '#006400',
              name = 'Pr(2nd in)',
              line_shape = 'spline',
              marker=dict(line=dict(width=2)),
              mode = 'markers',
              hovertemplate =
              'Match Date: %{x} <br>Probability: %{y}' +
              '<br>Tournament: <b>%{text}</b>',
              text = zverev_past['tournament_name'],
              ))

# Add vertical dashed lines to separate years
zverev_2ndin_accuracy.add_shape(
        # Line Vertical
        dict(type="line",
            x0='2018-01-01',
            y0=0.42,
            x1='2018-01-01',
            y1=1,
            line=dict(
                color="grey",
                width=0.5,
                dash="dashdot"
            )))
zverev_2ndin_accuracy.add_shape(
        # Line Vertical
        dict(type="line",
            x0='2019-01-01',
            y0=0.42,
            x1='2019-01-01',
            y1=1,
            line=dict(
                color="grey",
                width=0.5,
                dash="dashdot"
            )))
zverev_2ndin_accuracy.add_shape(
        # Line Vertical
        dict(type="line",
            x0='2020-01-01',
            y0=0.42,
            x1='2020-01-01',
            y1=1,
            line=dict(
                color="grey",
                width=0.5,
                dash="dashdot"
            )))

# Add box around 2018 Madrid Open
zverev_2ndin_accuracy.add_shape(
        # filled Rectangle
            type="rect",
            x0='2018-04-28',
            y0=0.98,
            x1='2018-05-22',
            y1=1.02,
            line=dict(
                color="red",
                width=2,
            )#,
            #fillcolor="LightSkyBlue",
        )

zverev_2ndin_accuracy.update_layout(
    showlegend = False,
    barmode = 'overlay',
    title = dict(text = "<b>Zverev's Career Prob(2nd Serve In)  </b>", # title of plot,
                    xanchor = 'center',
                    x = 0.5,
                    y = 0.95),
       xaxis_title_text="Match Date", # xaxis label
       yaxis_title_text='Probability', # yaxis label
       font = dict(size = 18),
       hovermode = 'closest',
       plot_bgcolor = "#FFF",
       xaxis_linecolor = "#BCCCDC",
       hoverlabel=dict(
               bgcolor="white", 
               bordercolor = 'black',
               font_size=16, 
               #font_family="Rockwell"
    ))
        
plot(zverev_2ndin_accuracy, filename = './plots/zverev_recent_2ndin.html')

##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
# END Zverev 2018-2020 Median Filter 2nd serve accuracy
##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 


# ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
### Zverev Career Two First Serves ('Risky strategy plot') 
# ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
import matplotlib.pyplot as plt
plt.style.use('default')
fig=plt.figure()
ax=fig.add_axes([0,0,1,1])
ax.scatter(zverev_to_plot['pr_win_on_2nd_serve'],
           zverev_to_plot['pr_win_on_1st_serve'],
           color='orange',
           edgecolor = 'black',
           alpha = 0.6)
ax.set_xlabel('Actual Prob(Win on 2nd Serve)', fontweight = 'bold',
              fontsize = 12)
ax.set_ylabel('Two 1st Serves Win Prob.', fontweight = 'bold',
              fontsize=12)
ax.set_title("Zverev's Career Risky Serve Strategy", 
             fontweight = 'bold',
             fontsize = 16
             )
ax.plot([-3, 3], [-3, 3], ls="--", c=".3")
ax.set(xlim=(0.4, 0.6), ylim=(0.4, 0.6))
z2019_coord = (zverev_to_plot[zverev_to_plot.year == 2019]['pr_win_on_2nd_serve'].values + 0.001,
            zverev_to_plot[zverev_to_plot.year == 2019]['pr_win_on_1st_serve'].values + 0.006)
z2020_coord = (zverev_to_plot[zverev_to_plot.year == 2020]['pr_win_on_2nd_serve'].values + 0.001,
            zverev_to_plot[zverev_to_plot.year == 2020]['pr_win_on_1st_serve'].values + 0.006)

ax.annotate('2019', z2019_coord, fontweight = 'bold',
            fontsize = 9,
            bbox=dict(facecolor='lightblue', edgecolor='black', boxstyle='round'))
ax.annotate('2020', z2020_coord, fontweight = 'bold',
            fontsize = 9,
            bbox=dict(facecolor='lightblue', edgecolor='black', boxstyle='round'))

z2018_coord = (zverev_to_plot[zverev_to_plot.year == 2018]['pr_win_on_2nd_serve'].values - 0.012,
            zverev_to_plot[zverev_to_plot.year == 2018]['pr_win_on_1st_serve'].values + 0.006)
ax.annotate('2018', z2018_coord, fontweight = 'bold',
            fontsize = 9,
            bbox=dict(facecolor='lightblue', edgecolor='black', boxstyle='round'))
z2017_coord = (zverev_to_plot[zverev_to_plot.year == 2017]['pr_win_on_2nd_serve'].values + 0,
            zverev_to_plot[zverev_to_plot.year == 2017]['pr_win_on_1st_serve'].values + 0.006)
ax.annotate('2017', z2017_coord, fontweight = 'bold',
            fontsize = 9,
            bbox=dict(facecolor='lightblue', edgecolor='black', boxstyle='round'))
z2016_coord = (zverev_to_plot[zverev_to_plot.year == 2016]['pr_win_on_2nd_serve'].values + 0,
            zverev_to_plot[zverev_to_plot.year == 2016]['pr_win_on_1st_serve'].values + 0.006)
ax.annotate('2016', z2016_coord, fontweight = 'bold',
            fontsize = 9,
            bbox=dict(facecolor='lightblue', edgecolor='black', boxstyle='round'))

z2015_coord = (zverev_to_plot[zverev_to_plot.year == 2015]['pr_win_on_2nd_serve'].values + 0,
            zverev_to_plot[zverev_to_plot.year == 2015]['pr_win_on_1st_serve'].values + 0.006)
ax.annotate('2015', z2015_coord, fontweight = 'bold',
            fontsize = 9,
            bbox=dict(facecolor='lightblue', edgecolor='black', boxstyle='round'))

##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- #####
# END Zverev Career Two First Serves ('Risky strategy plot') 
##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- #####





