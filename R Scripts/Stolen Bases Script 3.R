library(tidyverse)

stolen_events <- final_dataframe %>%
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

all_2b <- rbind(cs_2b, only_2b)

all_sb_attempts <- rbind(only_2b, only_3b, cs_2b, cs_3b)