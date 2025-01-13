make_trips <- function(d, harbours) {
  d %>%
    st_as_sf(coords = c("lon", "lat"),
             crs = 4326,
             remove = FALSE) %>%
    st_join(harbours %>% select(harbour)) %>%
    st_drop_geometry() %>%
    arrange(mid, desc(time)) %>%
    mutate(harbour = ifelse(lon > 10, "NOR", harbour)) %>%
    mutate(io2 = ifelse(!is.na(harbour), "in", "out")) %>%
    group_by(mid) %>%
    mutate(rownr = 1:n(),
           event = case_when(io2 == "out" & lead(io2) == "in" ~ "outto",
                             io2 == "out" & lead(io2) == "out" ~ "atsea",
                             io2 == "in" & lead(io2) == "out" ~ "into",
                             io2 == "in" & lead(io2) == "in" ~ "in",
                             TRUE ~ NA_character_)) %>%
    #              first record
    mutate(event = case_when(is.na(event) ~ lag(event),
                             TRUE ~ event)) %>%
    mutate(trip = case_when(rownr == 1 ~ 0,
                            event == "in" & lag(event) == "outto" ~ 1,
                            TRUE ~ 0)) %>%
    mutate(trip = cumsum(trip)) %>%
    select(-c(io2, rownr, event)) %>%
    ungroup() %>%
    arrange(mid, time)
}
