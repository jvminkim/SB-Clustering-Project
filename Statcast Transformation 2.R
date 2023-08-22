library(tidyverse)

###Data transformation script
### Creates dataframes for stolen bases as well as pitcher summaries

### Stolen_events : Dataframe for potential stolen base events for all base out states

stolen_events <- all_2023 %>%
  mutate(on_1b = ifelse(is.na(on_1b), "", on_1b),
         on_2b = ifelse(is.na(on_2b), "", on_2b),
         on_3b = ifelse(is.na(on_3b), "", on_3b),
         BASES = paste(ifelse(on_1b != "",1,0),
                       ifelse(on_2b != "",1,0),
                       ifelse(on_3b != "",1,0), sep = ""), STATE = paste(BASES, outs_when_up),
         runners_state = paste(ifelse(on_1b != "", on_1b, 0),
                               ifelse(on_2b != "", on_2b, 0),
                               ifelse(on_3b != "", on_3b, 0), sep = " ")) %>%
  group_by(game_pk) %>%
  arrange(at_bat_number, pitch_number, .by_group = TRUE) %>%
  mutate(next_pitch_state = dplyr::lead(STATE, n = 1),
         next_base_state = dplyr::lead(BASES, n =1),
         next_runners_state = dplyr::lead(runners_state, n = 1),
         next_out_state = dplyr::lead(outs_when_up, n = 1)) %>%
  filter(BASES != "000") %>%
  filter(STATE != next_base_state)


### Whittles each stolen base by state.
### only_2b : Successful stolen bases on second base attempts  
### only_3b : Succesful stolen bases on third base attempts
### cs_2b : Caught stolen base on second base attempt
### cs_3b : Caught stolen base on third base attempt

only_2b <- stolen_events %>%
  filter(BASES == "100" & next_base_state == "010") %>%
  filter(description != "hit_into_play" & description != "hit_by_pitch" & events != "walk") %>%
  filter(plate_z > 0.3 & plate_x < abs(3) & next_out_state == outs_when_up & description != "swinging_strike_blocked") %>%
  mutate(sb_attempt = 1)

only_3b <- stolen_events %>%
  filter(substr(BASES,2,2) == "1" & substr(next_base_state, 3,3) == "1" & next_base_state != BASES) %>%
  filter(description != "hit_into_play" & description != "hit_by_pitch" & events != "walk" & events != "catcher_interf") %>%
  filter(plate_z > 0.3 & plate_x < abs(3) & next_out_state == outs_when_up & description != "swinging_strike_blocked") %>%
  mutate(sb_attempt = 1)

cs_2b <- stolen_events %>%
  filter(BASES == "100", next_base_state == "000") %>%
  filter(description != "hit_into_play") %>%
  filter(description != "foul" & description != "foul_bunt" & description != "foul_tip") %>%
  filter(!(outs_when_up == 2 & next_out_state == 0 & events == "strikeout")) %>%
  filter(events != "pickoff_1b", events != "pickoff_caught_stealing_2b") %>%
  filter(!grepl("picks off", des)) %>%
  mutate(sb_attempt = 0)

cs_3b <- stolen_events %>%
  filter(substr(BASES,2,2) == "1",
         substr(BASES,3,3) != "1",
         substr(next_base_state ,3,3) == "0",
         next_base_state != BASES,
         description != "hit_into_play",
         events != "walk",
         !(outs_when_up == 2 & next_out_state == 0 & events == "strikeout"),
         outs_when_up != next_out_state,
         !(substr(BASES,1,1) == "0" & substr(next_base_state,1,1) != "0")) %>%
  mutate(sb_attempt = 0)

all_sb_attempts <- rbind(only_2b, only_3b, cs_2b, cs_3b)

sb_2b <- rbind(only_2b, cs_2b)

### Pitcher Clustering Transformation
# Variables: Arm Angle (Release Point) / not included in clustering but can calculate by uncommenting.
#   Pitch Mix (PCT of Each Pitch)
#   Pitch Velocity and Spin
#   Pitch Release Positions


pitchers_info <- all_dat %>%
  drop_na(pitch_type, release_pos_x,release_pos_y, release_pos_z) %>%
  filter((pitch_type == "FF" | pitch_type == "FC" | pitch_type == "SI" |
            pitch_type == "SL" | pitch_type == "CH" | pitch_type == "CU" |
            pitch_type == "ST")) %>%
  group_by(pitcher) %>%
  mutate(pitch_n = n(),
         avg_release_x = mean(release_pos_x),
         avg_release_y = mean(release_pos_y),
         avg_release_z = mean(release_pos_z)) %>%
  inner_join(all_height %>% select(player_id, height) , by = c("pitcher" = "player_id")) %>%
  drop_na(height) %>%
  mutate(#adjacent = avg_release_z - (as.numeric(height) * .7),
         #opposite = abs(avg_release_x),
         #hypotenuse = sqrt(adjacent^2 + opposite^2),
         #angle = (acos((adjacent^2 + hypotenuse^2 - opposite^2)/ (2*(adjacent*hypotenuse))) * (180/pi)),
         #avg_angle = mean(angle),
         pitch_type = ifelse(pitch_type == "ST", "SL", pitch_type)) %>%
  group_by(pitcher, pitch_type) %>%
  mutate( 
         ff_num_thrown = ifelse(pitch_type == "FF", n(), 0)/pitch_n,
         sl_num_thrown = ifelse(pitch_type == "SL", n(), 0)/pitch_n,
         fc_num_thrown = ifelse(pitch_type == "FC", n(), 0)/pitch_n,
         si_num_thrown = ifelse(pitch_type == "SI", n(), 0)/pitch_n,
         ch_num_thrown = ifelse(pitch_type == "CH", n(), 0)/pitch_n,
         cu_num_thrown = ifelse(pitch_type == "CU", n(), 0)/pitch_n,
         ff_release_pos_x = ifelse(pitch_type == "FF", mean(release_pos_x), 0),
         sl_release_pos_x = ifelse(pitch_type == "SL", mean(release_pos_x), 0),
         fc_release_pos_x = ifelse(pitch_type == "FC", mean(release_pos_x), 0),
         si_release_pos_x = ifelse(pitch_type == "SI", mean(release_pos_x), 0),
         ch_release_pos_x = ifelse(pitch_type == "CH", mean(release_pos_x), 0),
         cu_release_pos_x = ifelse(pitch_type == "CU", mean(release_pos_x), 0),
         ff_release_pos_z = ifelse(pitch_type == "FF", mean(release_pos_z), 0),
         sl_release_pos_z = ifelse(pitch_type == "SL", mean(release_pos_z), 0),
         fc_release_pos_z = ifelse(pitch_type == "FC", mean(release_pos_z), 0),
         si_release_pos_z = ifelse(pitch_type == "SI", mean(release_pos_z), 0),
         ch_release_pos_z = ifelse(pitch_type == "CH", mean(release_pos_z), 0),
         cu_release_pos_z = ifelse(pitch_type == "CU", mean(release_pos_z), 0),
         ff_release_extension = ifelse(pitch_type == "FF", mean(release_extension), 0),
         sl_release_extension = ifelse(pitch_type == "SL", mean(release_extension), 0),
         fc_release_extension = ifelse(pitch_type == "FC", mean(release_extension), 0),
         si_release_extension = ifelse(pitch_type == "SI", mean(release_extension), 0),
         ch_release_extension = ifelse(pitch_type == "CH", mean(release_extension), 0),
         cu_release_extension = ifelse(pitch_type == "CU", mean(release_extension), 0),
         ff_avg_speed = ifelse(pitch_type == "FF", mean(release_speed), 0),
         sl_avg_speed = ifelse(pitch_type == "SL", mean(release_speed), 0),
         fc_avg_speed = ifelse(pitch_type == "FC", mean(release_speed), 0),
         si_avg_speed = ifelse(pitch_type == "SI", mean(release_speed), 0),
         ch_avg_speed = ifelse(pitch_type == "CH", mean(release_speed), 0),
         cu_avg_speed = ifelse(pitch_type == "CU", mean(release_speed), 0),
         ff_avg_spin = ifelse(pitch_type == "FF", mean(release_spin_rate), 0),
         sl_avg_spin = ifelse(pitch_type == "SL", mean(release_spin_rate), 0),
         fc_avg_spin = ifelse(pitch_type == "FC", mean(release_spin_rate), 0),
         si_avg_spin = ifelse(pitch_type == "SI", mean(release_spin_rate), 0),
         ch_avg_spin = ifelse(pitch_type == "CH", mean(release_spin_rate), 0),
         cu_avg_spin = ifelse(pitch_type == "CU", mean(release_spin_rate), 0)
      ) %>%
  ungroup() %>%
  select(pitcher, 
         ff_num_thrown, 
         sl_num_thrown,
         fc_num_thrown, 
         si_num_thrown, 
         ch_num_thrown, 
         cu_num_thrown,
         ff_release_pos_x,
         sl_release_pos_x,
         fc_release_pos_x,
         si_release_pos_x,
         ch_release_pos_x,
         cu_release_pos_x,
         ff_release_pos_z,
         sl_release_pos_z,
         fc_release_pos_z,
         si_release_pos_z,
         ch_release_pos_z,
         cu_release_pos_z,
         ff_release_extension,
         sl_release_extension,
         fc_release_extension,
         si_release_extension,
         ch_release_extension,
         cu_release_extension,
         ff_avg_speed,
         sl_avg_speed,
         fc_avg_speed,
         si_avg_speed,
         ch_avg_speed,
         cu_avg_speed,
         ff_avg_spin,
         sl_avg_spin,
         fc_avg_spin,
         si_avg_spin,
         ch_avg_spin,
         cu_avg_spin) %>%
  unique()

pitchers_info[pitchers_info == 0] <- NA

final_pitchers <- pitchers_info %>%
  group_by(pitcher) %>%
  summarize(across(everything(), ~sum(., na.rm = TRUE))) %>%
  unique()

### Final_pitchers: dataframe containing average values for each pitcher

final_pitchers <- pitchers_info %>%
  group_by(pitcher)
  

dbWriteTable(conn, name = "all_pitch_vars", value = pitchers_info)

final_pitchers[is.na(final_pitchers)] <- 0  

