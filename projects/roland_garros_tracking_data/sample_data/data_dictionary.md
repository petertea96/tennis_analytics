### Data Dictionary

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
* `cruciality`: ???
* `returnPlacement`: ???
