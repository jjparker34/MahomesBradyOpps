library(tidyverse)
library(nflfastR)
library(nflplotR)
library(ggimage)

#Finding the Team EPA of Mahomes Superbowl Oppenents vs Brady's

#Mahomes played in the SB in: 2019, 2020, 2022, 2023, 2024
#2019 he played vs SF, 2020 vs TB, 2022 vs PHI, 2023 vs SF,2024 vs PHI

#finding EPA of each oppenent:
szn2019 <- load_pbp(2019)
szn2020 <- load_pbp(2020)
szn2022 <- load_pbp(2022)
szn2023 <- load_pbp(2023)
szn2024 <- load_pbp(2024)

SF2019_Oepa <- szn2019 %>%
  filter(posteam == "SF", !is.na(epa)) %>%  
  summarise(offensive_epa = sum(epa, na.rm = TRUE))
SF2019_Depa <- szn2019 %>%
  filter(defteam == "SF", !is.na(epa)) %>%  
  summarise(defensive_epa = sum(epa, na.rm = TRUE))
SF2019_epa <- szn2019 %>%
  summarise(
    offensive_epa = sum(epa[posteam == "SF"], na.rm = TRUE),
    defensive_epa = sum(epa[defteam == "SF"], na.rm = TRUE)
  )

#2020 TB

TB2020_Oepa <- szn2020 %>% 
  filter(posteam == "TB", !is.na(epa)) %>%  
  summarise(offensive_epa = sum(epa, na.rm = TRUE))
TB2020_Depa <- szn2020 %>% 
  filter(defteam == "TB", !is.na(epa)) %>%  
  summarise(defensive_epa = sum(epa, na.rm = TRUE))
TB2020_epa <- szn2020 %>%
  summarise(
    offensive_epa = sum(epa[posteam == "TB"], na.rm = TRUE),
    defensive_epa = sum(epa[defteam == "TB"], na.rm = TRUE)
  )

#2022 PHI
PHI2022_epa <- szn2022 %>%
  summarise(
    offensive_epa = sum(epa[posteam == "PHI"], na.rm = TRUE),
    defensive_epa = sum(epa[defteam == "PHI"], na.rm = TRUE)
  )

#2023 SF
SF2023_epa <- szn2023 %>%
  summarise(
    offensive_epa = sum(epa[posteam == "SF"], na.rm = TRUE),
    defensive_epa = sum(epa[defteam == "SF"], na.rm = TRUE)
  )

#2024 PHI
PHI2024_epa <- szn2024 %>%
  summarise(
    offensive_epa = sum(epa[posteam == "PHI"], na.rm = TRUE),
    defensive_epa = sum(epa[defteam == "PHI"], na.rm = TRUE)
  )
#--------- Tom Brady's Oppenents EPA ----------------
#Tom Brady played in SBs 2001, 2003, 2004, 2007, 2011, 2014, 2016, 2017, 2018, 2020
#2001 vs. STL, 2003 vs. CAR, 2004 vs. PHI, 2007 vs. NYG, 2011 vs. NYG, 2014 vs. SEA, 2016 vs. ATl, 2017 vs PHI, 2018 vs LAR, 2020 vs KAN

szn2001 <- load_pbp(2001)
szn2003 <- load_pbp(2003)
szn2004 <- load_pbp(2004)
szn2007 <- load_pbp(2007)
szn2011 <- load_pbp(2011)
szn2014 <- load_pbp(2014)
szn2016 <- load_pbp(2016)
szn2017 <- load_pbp(2017)
szn2018 <- load_pbp(2018)
szn2020 <- load_pbp(2020)

#2001 STL
STR2001_epa <- szn2001 %>%
  summarise(
    offensive_epa = sum(epa[posteam == "LA"], na.rm = TRUE),
    defensive_epa = sum(epa[defteam == "LA"], na.rm = TRUE)
  )

CAR2003_epa <- szn2003 %>%
  summarise(
    offensive_epa = sum(epa[posteam == "CAR"], na.rm = TRUE),
    defensive_epa = sum(epa[defteam == "CAR"], na.rm = TRUE)
  )

PHI2004_epa <- szn2004 %>%
  summarise(
    offensive_epa = sum(epa[posteam == "PHI"], na.rm = TRUE),
    defensive_epa = sum(epa[defteam == "PHI"], na.rm = TRUE)
  )
NYG2007_epa <- szn2007 %>%
  summarise(
    offensive_epa = sum(epa[posteam == "NYG"], na.rm = TRUE),
    defensive_epa = sum(epa[defteam == "NYG"], na.rm = TRUE)
  )
NYG2011_epa <- szn2011 %>%
  summarise(
    offensive_epa = sum(epa[posteam == "NYG"], na.rm = TRUE),
    defensive_epa = sum(epa[defteam == "NYG"], na.rm = TRUE)
  )
SEA2014_epa <- szn2014 %>%
  summarise(
    offensive_epa = sum(epa[posteam == "SEA"], na.rm = TRUE),
    defensive_epa = sum(epa[defteam == "SEA"], na.rm = TRUE)
  )
ATL2016_epa <- szn2016 %>%
  summarise(
    offensive_epa = sum(epa[posteam == "ATL"], na.rm = TRUE),
    defensive_epa = sum(epa[defteam == "ATL"], na.rm = TRUE)
  )
PHI2017_epa <- szn2017 %>%
  summarise(
    offensive_epa = sum(epa[posteam == "PHI"], na.rm = TRUE),
    defensive_epa = sum(epa[defteam == "PHI"], na.rm = TRUE)
  )
LAR2018_epa <- szn2018 %>%
  summarise(
    offensive_epa = sum(epa[posteam == "LA"], na.rm = TRUE),
    defensive_epa = sum(epa[defteam == "LA"], na.rm = TRUE)
  )
KC2020_epa <- szn2020 %>%
  summarise(
    offensive_epa = sum(epa[posteam == "KC"], na.rm = TRUE),
    defensive_epa = sum(epa[defteam == "KC"], na.rm = TRUE)
  )
#-----------------------------------plot----------------------
#find each QBs averages:
average_epa <- sb_epa %>%
  group_by(QB) %>%
  summarise(
    Avg_Offensive_EPA = mean(Offensive_EPA, na.rm = TRUE),
    Avg_Defensive_EPA = mean(Defensive_EPA, na.rm = TRUE)
  )

mahomes_off_epa <- round(average_epa$Avg_Offensive_EPA[average_epa$QB == "Mahomes"], 2)
mahomes_def_epa <- round(average_epa$Avg_Defensive_EPA[average_epa$QB == "Mahomes"], 2)
brady_off_epa <- round(average_epa$Avg_Offensive_EPA[average_epa$QB == "Brady"], 2)
brady_def_epa <- round(average_epa$Avg_Defensive_EPA[average_epa$QB == "Brady"], 2)

# Create subtitle text
subtitle_text <- paste0(
  "Mahomes Avg. Opponent EPA || O: ", mahomes_off_epa, ", D: ", mahomes_def_epa,"\n"
  , "Brady Avg. Opponent EPA || O: ", brady_off_epa, ", D: ", brady_def_epa
)

sb_epa <- data.frame(
  Year = c(2019, 2020, 2022, 2023, 2024, 2001, 2003, 2004, 2007, 2011, 2014, 2016, 2017, 2018, 2020),
  Opponent = c("SF", "TB", "PHI", "SF", "PHI", "STL", "CAR", "PHI", "NYG", "NYG", "SEA", "ATL", "PHI", "LAR", "KC"),
  QB = c(rep("Mahomes", 5), rep("Brady", 10)),
  Offensive_EPA = c(
    SF2019_epa$offensive_epa, TB2020_epa$offensive_epa, PHI2022_epa$offensive_epa, SF2023_epa$offensive_epa, PHI2024_epa$offensive_epa,
    STR2001_epa$offensive_epa, CAR2003_epa$offensive_epa, PHI2004_epa$offensive_epa, NYG2007_epa$offensive_epa, 
    NYG2011_epa$offensive_epa, SEA2014_epa$offensive_epa, ATL2016_epa$offensive_epa, PHI2017_epa$offensive_epa, 
    LAR2018_epa$offensive_epa, KC2020_epa$offensive_epa
  ),
  Defensive_EPA = c(
    SF2019_epa$defensive_epa, TB2020_epa$defensive_epa, PHI2022_epa$defensive_epa, SF2023_epa$defensive_epa, PHI2024_epa$defensive_epa,
    STR2001_epa$defensive_epa, CAR2003_epa$defensive_epa, PHI2004_epa$defensive_epa, NYG2007_epa$defensive_epa, 
    NYG2011_epa$defensive_epa, SEA2014_epa$defensive_epa, ATL2016_epa$defensive_epa, PHI2017_epa$defensive_epa, 
    LAR2018_epa$defensive_epa, KC2020_epa$defensive_epa
  )
)
team_logos <- data.frame(
  Opponent = c("SF", "TB", "PHI", "STL", "CAR", "NYG", "SEA", "ATL", "LAR", "KC"),
  Logo_URL = c(
    "https://a.espncdn.com/i/teamlogos/nfl/500/sf.png",   # 49ers
    "https://a.espncdn.com/i/teamlogos/nfl/500/tb.png",   # Buccaneers
    "https://a.espncdn.com/i/teamlogos/nfl/500/phi.png",  # Eagles
    "https://a.espncdn.com/i/teamlogos/nfl/500/lar.png",  # Rams (used STL logo)
    "https://a.espncdn.com/i/teamlogos/nfl/500/car.png",  # Panthers
    "https://a.espncdn.com/i/teamlogos/nfl/500/nyg.png",  # Giants
    "https://a.espncdn.com/i/teamlogos/nfl/500/sea.png",  # Seahawks
    "https://a.espncdn.com/i/teamlogos/nfl/500/atl.png",  # Falcons
    "https://a.espncdn.com/i/teamlogos/nfl/500/lar.png",  # Rams
    "https://a.espncdn.com/i/teamlogos/nfl/500/kc.png"    # Chiefs
  )
)
sb_epa <- sb_epa %>%
  left_join(team_logos, by = "Opponent")
sb_epa <- sb_epa %>%
  mutate(
    Outline_Color = ifelse(QB == "Mahomes", "red", "black"),  # Red outline for Mahomes
    Year_Color = ifelse(QB == "Mahomes", "red", "blue")  # Red year for Mahomes, Blue for Brady
  )

ggplot(sb_epa, aes(x = Offensive_EPA, y = Defensive_EPA)) +
  geom_image(aes(image = Logo_URL.x), size = .15) +  # Plot team logos
  geom_text(aes(label = Year, color = QB), vjust = 1.2, hjust = -1, size = 2.5, weight = 2) +  # Add year labels above logos
  theme_minimal() +
  labs(title = "Super Bowl Opponents' EPA: Mahomes vs. Brady",
       subtitle = subtitle_text,
       x = "Offensive EPA",
       y = "Defensive EPA") +
  scale_y_reverse() +  # Invert Y-axis so stronger defenses appear higher
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +  # Zero line for offense
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  xlim(-60, 270) + ylim(150, -170) +
  scale_color_manual(values = c("Mahomes" = "red", "Brady" = "blue")) +  # Custom colors
  theme(
    plot.subtitle = element_text(size = 7, face = "italic", color = "gray1")  # Change subtitle style
  )
