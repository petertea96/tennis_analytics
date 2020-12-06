### ReadMe

Prototype code on Sackmann's slam_pointbypoint and then iterate over to MCP


### Expected results
https://www.tennis.com/pro-game/2020/01/analytics-tennis-long-slow-rise-djokovic-federer-moya-andreescu/86128/

* The "most spectacular" long rally points do NOT MATTER... 70% of points played are 4 strokes or less
* In general, Serving W is better than serving down T (in what respect though...faulting? winning the point?). Apparently, you're going to land more serves Wide IN, and you;re going to get more forehands for your serve+1 shot
* Players return better on the Ad. Court than they do on Deuce court
* The #1 Player each year only wins about 55% of the points they play
* Do players go for the trickier shot (Ace down the T) when they're way ahead on the score or when they're way below on the score? 


# Steps 

* Scrape a single match

* Features: 
----------
 	* match_id: Match ID
 	* ElapsedTime
 	* SetNo
 	* Serve Number
 	* Serve Location
 	* Ace/Double fault indicator
 	* Current Score --> Convert to pressure score
 	* Deuce/ Ad. Court
 	* Serve Speed





 * Meta Data:
 ------------
 	* Player Handedness
 	* Surface
 	* 


 For a given tournament,
 * Set column of server name, returner name
 * Break point/game point/ side of court
 * Add handedness
 * Fit a mixed model
 * Add feature of previous serve location?


Some pressure situations are fully nested by side of the court. For example, Break points and Advantage points are ALWAYS played on the Ad. court. On Deuce, the score is always tied. To include the nested "pressure situation" variable nested inside the "court_side" variable, we'll have to include "pressure_Score" as an interaction term. We cannot have pressure_score as a main effect since it is conditional on what side of court we are serving on. We should have a third indicator variable on the level of the nested variable (set it to 0 if we're on Deuce Court). I.e. Pressure score = {0 if on Deuce, 1 if Break}... add others for up or down or Advantage point.
https://stats.stackexchange.com/questions/372257/how-do-you-deal-with-nested-variables-in-a-regression-model




