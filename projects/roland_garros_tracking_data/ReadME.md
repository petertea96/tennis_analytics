### ReadME

This repo contains the Roland Garros tracking data project.

JSON data of tracking data (from RG's Court Vision feature) was saved in early December 2020. 

`json_data`
----------
* Repo of all raw json files


`Data Dictionary`

* point_id [str]: ID of point number. Ex: '1_1_6_1' --> 'set'_'game'_'point_num'_'serve_num' (`pointId`)
* server_id [int]: Server ID (`serverId`)
* returner_id [int]: Returner ID (`receiverId`)
* who_won_id [int]: Point Winner ID (`scorerId`)
* game_num [int]: 
