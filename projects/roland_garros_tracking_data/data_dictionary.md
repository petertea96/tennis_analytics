# Court Vision Data Dictionary

### Data Keys

These are the keys in a JSON file:

`pointsData`: For each point ID, some relevant play-by-play infor
`eventType`: Ex: Mens/Womens singles or doubles
`matchStatus`: Complete status flag
`playersData`: Info on opposing players (name, id, seed, country)
`pointId`: ???
`isMatchComplete`: Another complete status flag
`statsData`: Provides labels for which points are:

* pointsWon
* doubleFault
* secondServePointsWon
* netPoints
* winner
* firstServePointsWon
* convertedBreakPoints
* returnPoints
* aces
* secondServeIn
* firstServeIn
* unforcedError

`courtName`: Name of court. Ex: Philippe Chatrier
`courtId` : Id of playing court
`setsCompleted`: Number of completed sets in match


### To do

* Sort the keys (10 counts as 1 when in character format...)

### Play - by - play

* Score features

`pointId`: Point identifier for set_game_point_serve. Ex: '1_1_6_1'
`set`: set number (ex: 1, 2, 3, ...)
`setNumber`: Ditto
`game`: Game number. Ex: 1, 2, ...
`point`: Point number WITHIN game. Ex: 1, 2, 3,...
`serve`: Serve Number. Ex: 1, 2

`serverId`: Server ID (int/char)
`receiverId`': Returner ID 
`scorerId`: Point Winner ID 
`court`: Serve court side. Ex: AdCourt


* Serve features

`ballSpeedFrench`: Serve speed in KMH 
`ballSpeed`: 
`returnSpeedFrench`: ***
`returnSpeed`: ***
`serveType`: Type of Serve. Ex: Flat, Slice, Pronated
`distanceOutsideCourt`: How much did a ball miss on Fault (Feet)?
`distanceOutsideCourtFrench`: How much did a ball miss on Fault (Metres)?

* How point ended features

`rallyLength`: Length of rally (including serve)
`pointEndType`: How point ended. Ex: Winner, Double Fault, Ace, Unforced/Forced error.
`errorType`: Type of Error. Ex: Net, Wide, Long 
`trappedByNet`: False,
`strokeType`: Last stroke type of rally
`hand`: Last shot of rally
`heightAboveNet`: Ball net clearance at last stroke (feet) Ex: '5.01 Feet'
`heightAboveNetFrench`: Net clearance in metres on LAST STROKE. Ex: '1.53 Metre'
`winnerPlacement`: Ex: 'Cross Court',
`unforcedErrorPlacement`: If point end type is unforced error, what type waws it? Ex: Net, Wide, Long. Lots are missing...
`breakPoint`: Boolean. Is current serve point a break point? 
`breakPointConverted`: Boolean. Was break point converted? 
`runAroundForeHand`: ???

### Unknowns 
`id`: 2
`pointNumber`: 0
`cruciality`: Boolean
`spin`:

### To further explore
`returnSpeed`: Speed in KMH. This always matches with the serve speed.
Sometimes serve speed is missing, but we have return speed. Maybe we can use return speed to impute serve speed?

`returnPlacement`



### Tracking information

trajectoryData: Full tracking data of each stroke, including: hit, peak, net, bounce).

* What is 'erroneousBall' ?


`serveBounceCordinate`: (x,y,z) Serve Bounce
`ballHitCordinate`: (x,y,z) Where ball is struck on last legal shot
`ballBounceCordinate`: (x,y,z) Last ball bounce of rally
`ballPeakCordinate`: 
`serverCordinate`:
`receiverCordinate`:
`ballLastCordinate`:
`ballNetCordinate`:


