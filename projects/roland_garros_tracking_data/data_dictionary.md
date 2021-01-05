# Court Vision Data Dictionary


## Data Keys

These are the keys in a JSON file:


### Match Information

* `eventType`: Ex: Mens/Womens singles
`matchStatus`: Complete status flag
* `playersData`: Info on opposing players (name, id, seed, country)
* `pointId`: ???
* isMatchComplete: Another complete status flag
* `courtName`: Name of court. Ex: Philippe Chatrier
* `courtId` : Id of playing court
* `setsCompleted`: Number of completed sets in match


### Play-by - play information 
`pointsData`: For each point ID, some relevant play-by-play info.
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




### To do

* figure out what server and returner coordinates are

### Play - by - play info in `pointsData`

* Current score features

* `pointId`: Point identifier in format of `set`_`game`_`point`_`serve`. Ex: '1_1_6_1' ==> Set 1, Game 1, Point 6, 1st Serve. Lets are not recorded. 
* `set`: set number (ex: 1, 2, 3, ...)
* `setNumber`: Same as `set`
* `game`: Game number. Ex: 1, 2, ...
* `point`: Point number WITHIN game. Ex: 1, 2, 3,...
* `serve`: Serve Number. Ex: 1, 2

* `serverId`: Server ID (int/char)
* `receiverId`': Returner ID 
* `scorerId`: Point Winner ID 
* `court`: Serve court side. Ex: AdCourt


* Serve features

* `ballSpeedFrench`: Serve speed in KMH 
* `ballSpeed`: Same as `ballSpeedFrench`
* `returnSpeedFrench`: I suspect it's serve speed too.
* `returnSpeed`: Same as `returnSpeedFrench`
* `serveType`: Type of Serve. Ex: Flat, Slice, Pronated
* `distanceOutsideCourt`: How much did a ball miss on Fault (Feet)? Why -- because this variable is only informative when we have a faulty serve.
* `distanceOutsideCourtFrench`: How much did a ball miss on Fault (Metres)?

* How point ended features

* `rallyLength`: Length of rally (including serve)
* `pointEndType`: How point ended. Ex: Winner, Double Fault, Ace, Unforced/Forced error.
* `errorType`: Type of Error. Ex: Net, Wide, Long 
* `trappedByNet`: False,
* `strokeType`: Last stroke type of rally
* `hand`: Last shot of rally
`heightAboveNet`: Ball net clearance at last stroke (feet) Ex: '5.01 Feet'. 
* `heightAboveNetFrench`: Net clearance in metres on LAST STROKE. Ex: '1.53 Metre'. This number matches the last `net` z coordinate in `trajectoryData`
* `winnerPlacement`: Ex: 'Cross Court',
* `unforcedErrorPlacement`: If point end type is unforced error, what type waws it? Ex: Net, Wide, Long. Lots are missing...
* `breakPoint`: Boolean. Is current serve point a break point? 
* `breakPointConverted`: Boolean. Was break point converted? 
* `runAroundForeHand`: ???

### Unknowns 
* `id`: 2
* `pointNumber`: 0
* `cruciality`: Boolean
* `spin`:

### To further explore
`returnSpeed`: Speed in KMH. This always matches with the serve speed.
Sometimes serve speed is missing, but we have return speed. Maybe we can use return speed to impute serve speed?

* `returnPlacement`



### Tracking information

trajectoryData: Full tracking data of each stroke, including: hit, peak, net, bounce).

* What is 'erroneousBall' ?


* `serveBounceCordinate`: (x,y,z) Serve Bounce
* `ballHitCordinate`: (x,y,z) Where ball is struck on last legal shot
* `ballBounceCordinate`: (x,y,z) Last ball bounce of rally
* `ballPeakCordinate`:  Last `peak` coordinates in `trajectoryData`
`serverCordinate`:
`receiverCordinate`:
`ballLastCordinate`:
`ballNetCordinate`:




### Extra things

Here's how infosys codes different events:
CH: "Quad Wheelchair Men's Doubles"
CM: "Quad Wheelchair Men's Singles"
DD: "Women's Doubles "
DF: "Girls' Doubles "
DG: "Boys' Doubles"
DM: "Men's Doubles"
DS: "Women's Singles "
DV: "Men's Legends under 45"
DW: "Men's Legends over 45  "
HD: "Women's Wheelchair Singles "
HF: "Women's Wheelchair Doubles"
HH: "Men's Wheelchair Doubles"
HM: "Men's Wheelchair Singles "
LD: "Women's Legends"
ML: "Men's Legends"
MLD: "Men's Legends Doubles"
MX: "Mixed Doubles "
QD: "Qualifying Women's Singles"
QM: "Qualifying Men's Singles"
SF: "Girls' Singles "
SG: "Boys' Singles "
SM: "Men's Singles "



Get all player Ids:
https://itp.infosys-platforms.com/api/rg/head-to-head/compare/init/eventId/520


More match info (ex: games won, sets, duration)
https://itp.infosys-platforms.com/api/rg/assisted-journalism/matches/year/2020/eventId/520/matchDate/2020-10-11



### Finished product Dictionary

Data dictionary using my own nomenclature


* `point_ID`: Character point ID
* `set_num`: Set Number
* `game_num`: Game Number
* `point_num`: Point number WITHIN a game (always starts at 1)
* `serve_num`: Serve Number (1,2). Lets are not tracked.
* `server_id`: 'Server ID
* `returner_id`: Returner ID
* `point_winner_id`: Point Winner ID (either serverID or returnerID)
* `court_side`: Serve Court ('AdCourt' or 'DeuceCourt')
* `serve_speed_kph`: Serve Speed
* `serve_speed_v2`: Serve speed again
* `serve_type`:'Flat', 'Pronated', 'Slice', 'Unclassified'
* `fault_distance_missed_ft`: If serve was fault, by how much did it miss?
* `fault_distance_missed_m`: Same as above, but in metres.
* `rally_length`: Length of rally (including serve)
* `point_end_type`: How did rally end? Ex: Fault, Unforced/Forced error, Winner
* `error_type`: If point ended in an error, which type? Ex: Net, Wide, Long
* `trapped_by_net`: *I think* Boolean for whether shot was net error.
* `strokeType`: Type of stroke on last shot of rally. Ex: Ground, Passing, Drop. Last shot' means any last shot (winner, error, etc)
* `hand`: Handedness on last shot. 'Last shot' means any last shot (winner, error)
* `last_stroke_net_height_ft`: Ball height at net on last shot of rally
* `last_stroke_net_height_m`: Same as above except in metres
* `winner_placement`: Cross Court or Down the Line
* `unforcedErrorPlacement`: If shot error, what type? Wide, Net, Long. This is the same as `error_type`?
* `is_break_point`: Boolean
* `is_break_point_converted`: Boolean
* `runAroundForeHand`: Boolean
* `is_track_avail`: Boolean. Indicates whether ball tracking data is available for this rally sequence. 
* `serveBounceCordinate`: (x,y,z) coordinates of serve bounce
* `ballHitCordinate`: (x,y,z) coordinates of last location ball was hit
* `ballBounceCordinate`:(x,y,z) coordinates of last ball bounce
* `server_coord`: (x,y,z) coordinates of server at last shot?
* `returner_coord`: (x,y,z) coordinates of returner at last shot?
* `spin_rpm`: Ball spin on last rally shot
* `cruciality`: 
* `returnPlacement`: 
