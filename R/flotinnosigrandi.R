lubridate::now()
library(ROracle) #, lib.loc = "/usr/local/lib/R/site/4.1/x86_64/library")
library(tidyverse)
library(arrow)
library(here)
library(lubridate)
library(sf)
library(omar)
con <- connect_mar()
source("/home/haf/einarhj/ShinyApps/flotinnosigrandi/R/make_trips.R")



# Load data --------------------------------------------------------------------
harbours <- read_rds("/home/haf/einarhj/ShinyApps/flotinnosigrandi/data/harbours.rds")
T1 <- (lubridate::today() - months(2)) |> as.character()


vessels <-
  read_parquet("/home/haf/einarhj/stasi/fishydata/data/landings/agf_stations.parquet") |>
  filter(datel >= ymd("2020-07-01"),
         gid_ln %in% c(9, 10)) |>
  inner_join(read_parquet("/home/haf/einarhj/stasi/fishydata/data/landings/agf_catch.parquet") |>
               filter(sid %in% c(30, 31, 34, 36))) |>
  select(vid) |>
  filter(!vid %in% c(1972, 2903)) |>
  add_row(vid = c(1131, 2350)) |> # add bjarni and árni
  distinct() |>
  left_join(read_parquet("/home/haf/einarhj/stasi/fishydata/data/vessels/stk_vessel_match.parquet")) |>
  left_join(read_parquet("/home/haf/einarhj/stasi/fishydata/data/vessels/vessels_iceland.parquet") |>
              select(vid, vessel) |>
              mutate(foreign = ifelse(vid %in% 3700:4999, TRUE, FALSE))) |>
  select(mid, vid, vessel, foreign, d1, d2)



mids <-
  vessels %>%
  select(mid) %>%
  drop_na() %>%
  filter(mid > 0) |>
  pull(mid)

trail <-
  stk_trail(con) %>%
  filter(mid %in% mids,
         rectime >= to_date(T1, "YYYY:MM:DD")) |>
  collect(n = Inf) %>%
  distinct() %>%
  filter(between(lon, -35, 30),
         between(lat, 50, 79)) %>%
  mutate(speed = ifelse(speed > 12, 12, speed),
         in.harbour = ifelse(!is.na(hid) | !is.na(io), TRUE, FALSE)) %>%
  group_by(mid) %>%
  mutate(max.time = max(time, na.rm = TRUE),
         min.time = min(time, na.rm = TRUE)) |>
  ungroup() %>%
  mutate(days = as.integer(difftime(today(), lubridate::as_date(time), units = "days")))

## Create trips ----------------------------------------------------------------
trail <-
  trail |>
  make_trips(harbours = harbours)
## Drop wackies ----------------------------------------------------------------
trail <-
  trail %>%
  group_by(mid) %>%
  mutate(whacky = ramb::rb_whacky_speed(lon, lat, time)) %>%
  ungroup() %>%
  filter(!whacky)
## Only three trips ------------------------------------------------------------
trail <-
  trail %>%
  filter(trip %in% c(0:3))
trail <-
  trail %>%
  left_join(vessels,
            by = join_by(mid, between(time, d1, d2)))

## fix trip number (should be done upstream) -----------------------------------
trip.fix <-
  trail |>
  filter(!is.na(vid)) |>
  group_by(vid) |>
  mutate(min.trip = min(trip, na.rm = TRUE)) |>
  ungroup() |>
  mutate(trip.fix = trip - min.trip) |>
  select(vid, trip, trip.fix) |>
  distinct()

## to spatial ------------------------------------------------------------------
trail <-
  trail %>%
  left_join(trip.fix,
            by = join_by(trip, vid)) |>
  mutate(trip = trip.fix) |>
  select(-trip.fix) |>
  st_as_sf(coords = c("lon", "lat"),
           crs = 4326,
           remove = FALSE) |>
  select(-c(hid, io, in.harbour, harbour, in.harbour))

## get rid of foreign vessels with max time less than 31 days ago --------------
VID.use <-
  trail |>
  st_drop_geometry() |>
  group_by(vid, foreign) |>
  reframe(time = max(time)) |>
  # why the or?
  filter(!foreign | time >= now() - days(31)) |>
  pull(vid)
trail <-
  trail |>
  filter(vid %in% VID.use)

# Save --------------------------------------------------------------------------
## stk
trail |> write_rds("/home/haf/einarhj/ShinyApps/flotinnosigrandi/data/flotinnosigrandi.rds")
## shiny vessel choice
tmp <-
  trail %>%
  st_drop_geometry() %>%
  select(mid, vessel, foreign) %>%
  arrange(foreign, vessel) %>%
  distinct() %>%
  drop_na()
# get the order right, arrange does not respect the Icelandic alphabet
i <- order(tmp$foreign, tmp$vessel)
tmp <- tmp[i ,]
vessels <- tmp$mid
names(vessels) <- tmp$vessel
vessels |> write_rds("/home/haf/einarhj/ShinyApps/flotinnosigrandi/data/vessels.rds")
