#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""

Functions written along the way to process Roland Garros Tracking data

pd.set_option('display.max_rows', 500)
pd.set_option('display.max_columns', 500)
"""

import pandas as pd
import json

##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 

def categorise_serve_direction(serveBounceCordinate_y):
    '''
    Args:
    -----
    serveBounceCordinate_y [int]
    
    Returns:
    --------
    Character
    
    
    Assumes Serve bounce coordinate is given in metres
    Note: (0,0,0) are the coordinates at the middle of the net.
    Dimension of court: 23.77 m in length (y), and 8.23 m wide (x) -- for single's court
    
    Classifies ball bounce coordinate as: Wide, Body, or T
    '''
    
    if serveBounceCordinate_y == None:
        return None
    
    # Court is 8.23 m wide
    one_third_length = 4.115/3

    # Tenuous at the moment
    # What if a player really miss-hits the ball, and it bounces to the opposite side of the court?
    if ( (serveBounceCordinate_y <= one_third_length) and (serveBounceCordinate_y >= -one_third_length) ):
        serve_dir = 'T'
    elif ((serveBounceCordinate_y < 2*one_third_length) and (serveBounceCordinate_y > one_third_length)  ) or ((serveBounceCordinate_y > -2*one_third_length) and (serveBounceCordinate_y < -one_third_length)  ):
        serve_dir = 'Body'
    elif (serveBounceCordinate_y >= 2*one_third_length) or ( serveBounceCordinate_y <= -2*one_third_length ):
        serve_dir = 'Wide'
    else:
        serve_dir = None
        
        
    return serve_dir



##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
def get_point_level_info(one_point_sequence):
    '''
    Args:
    -----
    one_point_sequence [dict]: Dictionary
    
    Returns:
    --------
    dict of row to append into a dataframe
    
    *******************************************************
    Collects relevant information for a single rally point
    *******************************************************
    
    Notes:
    ------
    For a point sequence in a match, tidy information on relevant stats like serve speed
    or ball coordinates.
    '''
    
    # -- Get Serve Speed
    serve_speed_kph = one_point_sequence['ballSpeedFrench']
    if ( (serve_speed_kph == '0') | ( serve_speed_kph == 'NA' ) ):
        serve_speed_kph = one_point_sequence['returnSpeedFrench']
        
    serve_speed_kph_v2 = one_point_sequence['ballSpeed']
    
    if ( (serve_speed_kph_v2 == '0') | ( serve_speed_kph_v2 == 'NA' ) ):
        serve_speed_kph_v2 = one_point_sequence['returnSpeed']
       
    
    # -- Flag for whether we have tracking data on this point sequence
    is_track_avail = True
    if len(one_point_sequence['trajectoryData']) == 0 :
        is_track_avail = False
    
    # -- Serve Net Clearance
    z_net_serve = None
    if is_track_avail:
        
        try:
            served_ball_loc_net = one_point_sequence['trajectoryData'][2]
            
            if served_ball_loc_net['position'] == 'net':
                z_net_serve = served_ball_loc_net['z']
                
        except IndexError:
            print('Index Error...')
            
    ##########################################################################        
    # -- Add ball location at contact of serve
    x_ball_at_serve = None
    y_ball_at_serve = None
    z_ball_at_serve = None
    
    
    # -- Add max (peak) ball height location of serve
    z_peak_serve = None
    
    if is_track_avail :
        try:
            ball_loc_at_serve = one_point_sequence['trajectoryData'][0]
            
            if ball_loc_at_serve['position'] == 'hit':
                x_ball_at_serve = ball_loc_at_serve['x']
                y_ball_at_serve = ball_loc_at_serve['y']
                z_ball_at_serve = ball_loc_at_serve['z']
                
        except IndexError:
            print('Index Error...')
            
            
    #if is_track_avail :
        try:
            serve_peak = one_point_sequence['trajectoryData'][1]
            
            if serve_peak['position'] == 'peak':
                z_peak_serve = serve_peak['z']
                
        except IndexError:
            print('Index Error...')
            
    ########################################################################## 
            


    
    
    
    # -- Identify whether serve bounce is Body, Wide, or Down the T
    serveBounceCordinate_y = one_point_sequence['serveBounceCordinate']['y']
    
    serve_dir = categorise_serve_direction(serveBounceCordinate_y)
    

    point_dict = dict(
        # Match situation information
        #point_ID_v2 = one_point_sequence['id'],
        point_ID = one_point_sequence['pointId'],
        set_num = one_point_sequence['set'],
        #set_num_v2 = one_point_sequence['setNumber'],
        game_num = one_point_sequence['game'], 
        point_num = one_point_sequence['point'],
        #point_number_v2 = one_point_sequence['pointNumber'],
        serve_num = one_point_sequence['serve'],
        
        # players involved
        server_id = one_point_sequence['serverId'],
        returner_id = one_point_sequence['receiverId'],
        point_winner_id = one_point_sequence['scorerId'],
        court_side = one_point_sequence['court'],
        
        # Serve Stats
        serve_speed_kph = serve_speed_kph,
        serve_speed_kph_v2 = serve_speed_kph_v2,
        serve_type = one_point_sequence['serveType'],
        fault_distance_missed_ft = one_point_sequence['distanceOutsideCourt'],
        fault_distance_missed_m = one_point_sequence['distanceOutsideCourtFrench'],
        #return_placement = one_point_sequence['returnPlacement'],
        x_ball_at_serve = x_ball_at_serve,
        y_ball_at_serve = y_ball_at_serve,
        z_ball_at_serve = z_ball_at_serve,
        
        # How point ended
        rally_length = one_point_sequence['rallyLength'],
        point_end_type = one_point_sequence['pointEndType'],
        error_type = one_point_sequence['errorType'],
        trapped_by_net = one_point_sequence['trappedByNet'],

        strokeType = one_point_sequence['strokeType'],
        hand = one_point_sequence['hand'],
        
        last_stroke_net_height_ft = one_point_sequence['heightAboveNet'],
        last_stroke_net_height_m = one_point_sequence['heightAboveNetFrench'],
        # 0 is ground height...height does not start on top of the net!!!
        
        winner_placement = one_point_sequence['winnerPlacement'],
        unforcedErrorPlacement = one_point_sequence['unforcedErrorPlacement'],
        is_break_point = one_point_sequence['breakPoint'],
        is_break_point_converted = one_point_sequence['breakPointConverted'],
        runAroundForeHand = one_point_sequence['runAroundForeHand'],

        
        # Tracking info
        is_track_avail = is_track_avail,
        
        serveBounceCordinate_x = one_point_sequence['serveBounceCordinate']['x'],
        serveBounceCordinate_y = one_point_sequence['serveBounceCordinate']['y'],
        serveBounceCordinate_z = one_point_sequence['serveBounceCordinate']['z'],
        serve_dir = serve_dir,
        z_net_serve = z_net_serve,
        z_peak_serve = z_peak_serve,
        
        # (initial) Ball coordinate on last shot 
        ballHitCordinate_x = one_point_sequence['ballHitCordinate']['x'],
        ballHitCordinate_y = one_point_sequence['ballHitCordinate']['y'],
        ballHitCordinate_z = one_point_sequence['ballHitCordinate']['z'],
        
        # Ball coordinate on its last bounce of rally
        ballBounceCordinate_x = one_point_sequence['ballBounceCordinate']['x'],
        ballBounceCordinate_y = one_point_sequence['ballBounceCordinate']['y'],
        ballBounceCordinate_z = one_point_sequence['ballBounceCordinate']['z'],
        
        # Server and Returner coordinates
        server_coord_x = one_point_sequence['serverCordinate']['x'],
        server_coord_y = one_point_sequence['serverCordinate']['y'],
        server_coord_z = one_point_sequence['serverCordinate']['z'],
        returner_coord_x = one_point_sequence['receiverCordinate']['x'],
        returner_coord_y = one_point_sequence['receiverCordinate']['y'],
        returner_coord_z = one_point_sequence['receiverCordinate']['z'],
        
        # unknowns
        spin_rpm = one_point_sequence['spin'],
        cruciality = one_point_sequence['cruciality'],
        returnPlacement =  one_point_sequence['returnPlacement']
    )
    
    return point_dict



##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 


def get_match_point_level_info(raw_json_file):
    '''
    Args:
    -----
    one_point_sequence [dict]: Dictionary
    
    Returns:
    --------
    pandas DataFrame
    
    ******************************************
    Iterate over all rally points in a match and create an entire match dataframe
    ******************************************
    Collect all play-by-play information for a match into a pandas DataFrame
    
    '''
    all_tracking_data_dict = raw_json_file['courtVisionData'][0]['pointsData']
    
    data_list = []
    for point_id_key in sorted(all_tracking_data_dict.keys()):
        #print(point_id_key)
        data_list.append( get_point_level_info( all_tracking_data_dict[point_id_key] ) )
    
    match_point_df = pd.DataFrame(data_list)
    
    # Sort Dataframe by Set Number, Game number, Point Number, Serve Number
    match_point_df[['set_num', 'game_num', 'point_num', 'serve_num']] = match_point_df[['set_num', 'game_num', 'point_num', 'serve_num']].astype(int)


    match_point_df.sort_values(by = ['set_num', 'game_num', 'point_num', 'serve_num'], inplace = True)
    
    return match_point_df


##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 



##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 



##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 
##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### ----- ##### 