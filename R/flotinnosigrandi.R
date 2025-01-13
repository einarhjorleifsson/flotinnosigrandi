#lubridate::now()
#library(DBI, lib.loc = "/usr/local/lib/R/site/4.1/x86_64/library")
lubridate::now()
library(ROracle) #, lib.loc = "/usr/local/lib/R/site/4.1/x86_64/library")
library(tidyverse)
library(here)
library(lubridate)
library(sf)
library(omar)
con <- connect_mar()
source("/home/haf/einarhj/ShinyApps/flotinnosigrandi/R/make_trips.R")

# vessels that have landed capelin since 2018, only run occationally
if(FALSE) {
  vessels <-
    omar::vessels_vessels(con) |>
    collect(n = Inf)
  capelin <-
    ln_catch(con) %>%
    filter(date >= to_date("2018-07-01", "YYYY:MM:DD"),
           sid %in% c(31)) %>%
    group_by(vid) %>%
    summarise(catch = sum(catch, na.rm = TRUE),
              last.landing  = max(date, na.rm = TRUE),
              .groups = "drop") %>%
    arrange(desc(catch)) %>%
    collect(n = Inf) %>%
    filter(catch > 0) |>
    left_join(vessels |> select(vid, vessel, uid, uno, cs2 = cs, imo, mmsi2 = mmsi)) |>
    mutate(vessel = case_when(vid == 3743 ~ "LIBAS VL0001ØN",
                              vid == 3744 ~ "Hardhaus VL0009AV",
                              vid == 3745 ~ "Polar Ammassak GR-18-188",
                              TRUE ~ vessel)) |>
    left_join(mar:::stk_mid_vid(con) %>%
                collect(n = Inf),
              multiple = "all") |>
    mutate(mid = case_when(vid == 3743 & is.na(mid) ~ 144307,
                           vid == 3744 & is.na(mid) ~ 144325,
                           vid == 3745 & is.na(mid) ~ 144224,
                           TRUE ~ mid)) |>
    mutate(mid = case_when(vid == 2982 & is.na(mid) ~ 101402,       # Vilhelm Thorsteinsson
                           vid == 2983 & is.na(mid) ~ 141610,       # Börkur
                           vid == 3000 & is.na(mid) ~ 104362,       # Álsey
                           vid == 3015 & is.na(mid) ~ 103872,       # Svanur
                           vid == 3016 & is.na(mid) ~ 101074,       # Suðurey
                           vid == 2730 & is.na(mid) ~ 101119,       # Gullberg
                           TRUE ~ mid)) |>
    mutate(mid = case_when(vid == 3756 ~ 137182,
                           vid == 3757 ~ 140458,
                           vid == 3758 ~ 140470,
                           vid == 3759 ~ 116404,
                           vid == 3760 ~ 116404,
                           vid == 3761 ~ 143357,
                           vid == 3762 ~ 140479,
                           vid == 3763 ~ 140455,
                           vid == 3764 ~ 139624,
                           vid == 3765 ~ 140489,
                           vid == 3766 ~ 140412,
                           vid == 3925 ~ 101715,
                           vid == 3925 ~ 102622,
                           vid == 4727 ~ 102622,
                           TRUE ~ mid)) |>
    arrange(vid) |>
    add_row(vid = 2350, vessel = "Arni Friðriksson",  mid = 101109) |>
    add_row(vid = 1131, vessel = "Bjarni Sæmundsson", mid = 101143) |>
    add_row(vid = 2730, vessel = "Gullberg", mid = 101119) |>
    add_row(vid = 3035, vessel = "Hoffell", mid = 101104) |>
    add_row(vid = 3059, vessel = "Hákon", mid = 149520) |>
    mutate(foreign = ifelse(vid %in% 3700:4999, TRUE, FALSE)) |>
    arrange(foreign, vessel) |>
    filter(vid != 2885)
  capelin |> write_rds(here("data/capelin_vessels.rds"))
}

harbours <- read_rds("/home/haf/einarhj/ShinyApps/flotinnosigrandi/data/harbours.rds")
capelin <- read_rds("/home/haf/einarhj/ShinyApps/flotinnosigrandi/data/capelin_vessels.rds")

mids <-
  capelin %>%
  select(mid) %>%
  drop_na() %>%
  filter(mid > 0) |>
  pull(mid)

trail <-
  stk_trail(con) %>%
  filter(mid %in% mids,
         time >= to_date("2025-01-01", "YYYY:MM:DD"),
         time <= to_date("2028-12-24", "YYYY:MM:DD")) %>%
  collect(n = Inf) %>%
  distinct() %>%
  filter(between(lon, -35, 30),
         between(lat, 50, 79)) %>%
  mutate(speed = ifelse(speed > 12, 12, speed),
         in.harbour = ifelse(!is.na(hid) | !is.na(io), TRUE, FALSE)) %>%
  group_by(mid) %>%
  arrange(time) %>%
  mutate(max.time = max(time, na.rm = TRUE),
         min.time = min(time, na.rm = TRUE)) |>
  #mutate(max.time.in.harbour = max(time[in.harbour])) %>%
  ungroup() %>%
  mutate(days = as.integer(difftime(today(), lubridate::as_date(time), units = "days"))) %>%
  make_trips(harbours = harbours)

trail <-
  trail %>%
  group_by(mid) %>%
  mutate(whacky = ramb::rb_whacky_speed(lon, lat, time)) %>%
  ungroup() %>%
  filter(!whacky)

trail <-
  trail %>%
  filter(trip %in% c(0:3)) # & days <= 91)

trail <-
  trail %>%
  left_join(capelin |>
              select(vid, vessel, mid, foreign),
            by = join_by(mid))

# # fix trip number (should be done upstream)
trip.fix <-
  trail |>
  filter(!is.na(vid)) |>
  group_by(vid) |>
  mutate(min.trip = min(trip, na.rm = TRUE)) |>
  ungroup() |>
  mutate(trip.fix = trip - min.trip) |>
  select(vid, trip, trip.fix) |>
  distinct()

trail <-
  trail %>%
  left_join(trip.fix,
            by = join_by(trip, vid)) |>
  mutate(trip = trip.fix) |>
  select(-trip.fix) |>
  st_as_sf(coords = c("lon", "lat"),
           crs = 4326,
           remove = FALSE)

trail <-
  trail %>%
  select(-c(hid, io, in.harbour, harbour, in.harbour))

max.time <-
  trail |>
  st_drop_geometry() |>
  group_by(vid, foreign) |>
  summarise(time = max(time),
            .groups = "drop") |>
  ungroup() |>
  arrange(time) |>
  filter(!foreign | time >= ymd("2024-11-01"))
# get rid of vessels with max time less than 30 days ago
VID.use <- max.time$vid
trail <-
  trail |>
  filter(vid %in% VID.use)

trail <-
  trail |>
  filter(time >= ymd_hms("2024-12-01 00:00:00"))


trail |> write_rds("/home/haf/einarhj/ShinyApps/flotinnosigrandi/data/flotinnosigrandi.rds")
#system("chmod a+rX data/flotinnosigrandi.rds")

# create a list for selection
tmp <-
  trail %>%
  st_drop_geometry() %>%
  select(mid, vessel, foreign) %>%
  arrange(foreign, vessel) %>%
  distinct() %>%
  drop_na()
i <- order(tmp$foreign, tmp$vessel)
tmp <- tmp[i ,]
vessels <- tmp$mid
names(vessels) <- tmp$vessel
vessels |> write_rds("/home/haf/einarhj/ShinyApps/flotinnosigrandi/data/vessels.rds")
#system("chmod a+rX data/vessels.rds")


