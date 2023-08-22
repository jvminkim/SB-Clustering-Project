require('baseballr')
require('lubridate')
require('RMySQL')

###Overview: Creates dataframe of events in all years and heights from pitchers for all years

### traverse_dates: Loops through statcast_search function to bind together long periods in three day span.

traverse_dates <- function(start, end) {
  start_date <- start
  end_date <- end
  
  current_date <- start_date
  
  final_df <- data.frame()  # Empty dataframe
  
  while (current_date <= end_date - 4) {
    # Calculate the end date for the current 4-day period
    # search_end_date <- min(current_date + days(4), end_date)
    
    # Perform statcast_search and store the result in a temporary dataframe
    temp_df <- statcast_search(start_date = current_date, end_date = current_date + 4)
    
    # Append the temporary dataframe to the final dataframe
    final_df <- rbind(final_df, temp_df)
    
    # Move to the next 4-day period
    #current_date <- search_end_date + days(5)
    current_date <- current_date + days(5)
  }
  
  return(final_df)
}

# Call the function and store the result in the final dataframe
final_dataframe <- traverse_dates()

all_2017 <- traverse_dates(ymd("2017-04-02"), ymd("2017-10-01"))
all_2018 <- traverse_dates(ymd("2018-03-29"), ymd("2018-10-01"))
all_2019 <- traverse_dates(ymd("2019-03-20"), ymd("2019-09-29"))
all_2020 <- traverse_dates(ymd("2020-07-23"), ymd("2020-09-27"))
all_2021 <- traverse_dates(ymd("2021-04-01"), ymd("2021-10-03"))
all_2022 <- traverse_dates(ymd("2022-04-07"), ymd("2022-10-05"))
all_2023 <- traverse_dates(ymd("2023-03-30"), ymd("2023-06-21"))

### all_dat: Contains every event from 2017 - 2023 to use for pitcher clustering.

all_dat <- rbind(all_2017, all_2018, all_2019, all_2020, all_2021, all_2022, all_2023)

### Scraping heights for pitchers from each year, heights not used in modeling.

height_df_2023 <- mlb_sports_players(sport_id = 1, season = 2023) %>%
  filter(primary_position_abbreviation == "P") %>%
  select(full_name, height, primary_position_abbreviation, pitch_hand_code, player_id)

height_df_2022 <- mlb_sports_players(sport_id = 1, season = 2022) %>%
  filter(primary_position_abbreviation == "P") %>%
  select(full_name, height, primary_position_abbreviation, pitch_hand_code, player_id)

height_df_2021 <- mlb_sports_players(sport_id = 1, season = 2021) %>%
  filter(primary_position_abbreviation == "P") %>%
  select(full_name, height, primary_position_abbreviation, pitch_hand_code, player_id)

height_df_2020 <- mlb_sports_players(sport_id = 1, season = 2020) %>%
  filter(primary_position_abbreviation == "P") %>%
  select(full_name, height, primary_position_abbreviation, pitch_hand_code, player_id)

height_df_2019 <- mlb_sports_players(sport_id = 1, season = 2019) %>%
  filter(primary_position_abbreviation == "P") %>%
  select(full_name, height, primary_position_abbreviation, pitch_hand_code, player_id)

height_df_2018 <- mlb_sports_players(sport_id = 1, season = 2018) %>%
  filter(primary_position_abbreviation == "P") %>%
  select(full_name, height, primary_position_abbreviation, pitch_hand_code, player_id)

height_df_2017 <- mlb_sports_players(sport_id = 1, season = 2017) %>%
  filter(primary_position_abbreviation == "P") %>%
  select(full_name, height, primary_position_abbreviation, pitch_hand_code, player_id)

all_height <- rbind(height_df_2017, 
                    height_df_2018, 
                    height_df_2019, 
                    height_df_2020, 
                    height_df_2021, 
                    height_df_2022,
                    height_df_2023) %>%
  unique()


#Converting heights into decimals
all_height$height <- sapply(strsplit(as.character(all_height$height),"'|\""),
                            function(x){as.numeric(x[1]) + (round(as.numeric(x[2])/12,3))})


dbSendQuery(conn, "SET GLOBAL local_infile = true;") 

dbWriteTable(conn, name = "all_statcast", value = all_dat)

