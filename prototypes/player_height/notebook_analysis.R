# -- Analyze & plot the Height Advantage

setwd("/Users/petertea/tennis_analytics/prototypes/player_height")


# === # === # === # === # === # === # === # === # === # === # === # === #
# === # === # === # === # === # === # === # === # === # === # === # === #
# Identify Which Players to look at
# === # === # === # === # === # === # === # === # === # === # === # === #
# === # === # === # === # === # === # === # === # === # === # === # === #
# -- Include players who played in 50 matches in the 2010s (2010 - 2019)
# -- Include players who reached a top 20 ranking (in the labels?)
# -- Maybe plot all points from players who played 50 matches,
#    but label only the "interesting" names?

atp_filename <- "/Users/petertea/tennis_analytics/prototypes/player_height/data/processed_data/end_of_year_atp_rankings.csv"
atp_rankings <- read.csv(atp_filename)

# -- Which player labels will we display on the graphic?
# -- Players who held an end-of-year ranking of 20
player_ids_to_keep <- atp_rankings %>%
  filter((year >= 2010) & (year <= 2019)) %>%
  filter(rank <= 20) %>%
  select(player) %>%
  .$player


aces_file_name <- '/Users/petertea/tennis_analytics/prototypes/player_height/data/processed_data/processed_atp_aces_data_10_19.csv'


atp_aces <- read.csv(aces_file_name)
