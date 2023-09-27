# This is limiting the information loaded into R Shiny.
# I have been running into "oom" errors.

# Loading Packages --------------------------------------------------------
library(shiny)
library(shinyWidgets)
library(tidyverse)
library(sf)
library(plotly)
library(gt)
library(parallel)
library(foreach)
library(doParallel)
tidymodels::tidymodels_prefer()

all_cores <- parallel::detectCores(logical = TRUE)
registerDoParallel(cores = all_cores - 1)




# Reducing Information ----------------------------------------------------
## Census Tracts Loading ----
# Source: https://www.mass.gov/info-details/massgis-data-2020-us-census
tracts <- st_read("data/shapefiles/CENSUS2020TRACTS_POLY.shp") %>% 
  janitor::clean_names() %>% 
  st_transform(
    crs = st_crs("EPSG:26986")
  ) %>% 
  select(c(geoid20))


# Source: https://data.census.gov/table?q=DP05&g=040XX00US25$1400000&tid=ACSDP5Y2021.DP05
acs <- read_csv("data/acs/ACSDP5Y2021.DP05-Data.csv", skip = 1) %>% 
  janitor::clean_names() %>% 
  select(
    c(
      geography,
      estimate_race_total_population,
      estimate_race_total_population_one_race_white,
      estimate_race_total_population_one_race_black_or_african_american,
      estimate_race_total_population_one_race_asian,
      estimate_hispanic_or_latino_and_race_total_population_hispanic_or_latino_of_any_race,
      estimate_race_total_population
    )
  ) %>% 
  mutate(
    estimate_pct_one_race_black_or_african_american = 
      (estimate_race_total_population_one_race_black_or_african_american / 
         estimate_race_total_population) * 100,
    estimate_pct_one_race_white = 
      (estimate_race_total_population_one_race_white / 
         estimate_race_total_population) * 100,
    estimate_pct_one_race_asian = 
      (estimate_race_total_population_one_race_asian / 
         estimate_race_total_population) * 100,
    estimate_pct_any_race_latino_hispanic = 
      (estimate_hispanic_or_latino_and_race_total_population_hispanic_or_latino_of_any_race / 
         estimate_race_total_population) * 100,
    geoid20 = stringr::str_split(geography, "US") %>% map_chr(., 2)
  ) %>% 
  arrange(desc(estimate_pct_one_race_black_or_african_american)) %>% 
  select(
    c(
      geoid20, estimate_pct_one_race_black_or_african_american,
      estimate_pct_one_race_white,
      estimate_pct_one_race_asian,
      estimate_race_total_population,
      estimate_pct_any_race_latino_hispanic
    )
  )


acs_shp_s <- acs %>% 
  inner_join(tracts, by = "geoid20") %>% 
  st_as_sf(
    crs = "EPSG:26986"
  ) %>% 
  rename(
    eaa = estimate_pct_one_race_black_or_african_american,
    ew = estimate_pct_one_race_white,
    ea = estimate_pct_one_race_asian,
    et = estimate_race_total_population,
    elh = estimate_pct_any_race_latino_hispanic
  )

# st_write(acs_shp_s, driver = "ESRI Shapefile", "table_app/data/acs/acs_shp.shp")

acs_shp <- st_read("table_app/data/acs/acs_shp.shp") %>%
  rename(
    estimate_pct_one_race_black_or_african_american = eaa,
    estimate_pct_one_race_white = ew,
    estimate_pct_one_race_asian = ea,
    estimate_race_total_population = et,
    estimate_pct_any_race_latino_hispanic = elh
  )

acs_shp 
  

## Train Routes (MBTA) ----
# Source: https://www.mass.gov/info-details/massgis-data-mbta-rapid-transit
mbta_t_route <- st_read("data/mbta_t/MBTA_ARC.shp") %>% 
  janitor::clean_names() %>% 
  filter(
    line != "SILVER"
  ) %>% 
  st_transform(
    crs = st_crs("EPSG:26986")
  )

# Source: https://www.mass.gov/info-details/massgis-data-trains
mbta_t_commuter_route <- st_read("data/trains/TRAINS_RTE_TRAIN.shp") %>% 
  janitor::clean_names() %>% 
  st_transform(
    crs = st_crs("EPSG:26986")
  ) %>% 
  filter(
    comm_line != "CapeFLYER",
    comm_line != "Wildcat Branch"
  )

mbta_t_combined_route <- mbta_t_commuter_route %>% 
  rename(
    line = comm_line
  ) %>% 
  select(line) %>%
  mutate(
    source = "CR"
  ) %>% 
  rbind(
    mbta_t_route %>% 
      select(line) %>% 
      mutate(
        source = "T"
      )
  ) %>% 
  mutate(
    owner_branch = paste0("MBTA-", source, ": ", line)
  ) %>% 
  rename(
    ob = owner_branch
  )


# st_write(mbta_t_combined_route, driver = "ESRI Shapefile", "data/combined/mbta_t_combined_route.shp")
mbta_t_combined_route <- st_read("data/combined/mbta_t_combined_route.shp") %>% 
  rename(
    owner_branch = ob
  )
mbta_t_combined_route






## Train Stops (MBTA) ----
# Source: https://www.mass.gov/info-details/massgis-data-trains
all_trains_stop <- st_read("data/trains/TRAINS_NODE.shp") %>% 
  janitor::clean_names() %>% 
  st_transform(
    crs = st_crs("EPSG:26986")
  ) %>% 
  mutate(
    station = janitor::make_clean_names(
      station,
      case = "big_camel",
      allow_dupes = TRUE,
      sep_out = " "
    )
  )
# Making minor corrections- adjusting specialty names
all_trains_stop$station[all_trains_stop$station == "Tf Green Airport"] <- "T.F. Green Airport"
all_trains_stop$station[all_trains_stop$station == "Jfk Umass"] <- "JFK/UMass"


# Source: https://www.mass.gov/info-details/massgis-data-mbta-rapid-transit
mbta_t_stop <- st_read("data/mbta_t/MBTA_NODE.shp") %>% 
  janitor::clean_names() %>% 
  filter(
    line != "SILVER"
  ) %>% 
  st_transform(
    crs = st_crs("EPSG:26986")
  ) %>% 
  mutate(
    station = janitor::make_clean_names(
      station,
      case = "big_camel",
      allow_dupes = TRUE,
      sep_out = " "
    )
  )
# Making minor corrections- adjusting specialty names
mbta_t_stop$station[mbta_t_stop$station == "Jfk U Mass"] <- "JFK/UMass"
mbta_t_stop$station[mbta_t_stop$station == "Porter"] <- "Porter Square"
mbta_t_stop$station[mbta_t_stop$station == "Charles Mgh"] <- "Charles MGH"
mbta_t_stop$station[mbta_t_stop$station == "Medford Tufts"] <- "Medford/Tufts"


# Phase 2 Stations from the South Coast Rail Project
# Source: https://cdn.mbta.com/sites/default/files/2020-05/SCR_FullBuild_CorridorMap_2020.pdf
new_stations <- tibble::tribble(
  ~"name", ~"lon", ~"lat",
  "North Easton", 233370, 871500,
  "Easton Village", 232810, 869000,
  "Raynham Place", 235150, 859000,
  "Taunton", 235100, 851900,
  "East Taunton", 235950, 847000,
  "Freetown", 234010, 836000,
  "Battleship Cove", 228160, 828300,
  "Church Street", 246590, 825400,
  "New Bedford", 247790, 821750
) %>% 
  st_as_sf(
    coords = c("lon", "lat"),
    crs = "EPSG:26986"
  )

mbta_t_combined_stop_all <- all_trains_stop %>% 
  filter(
    c_railstat == "Y"
  ) %>% 
  rename(line = line_brnch) %>% 
  select(c(station, line)) %>% 
  mutate(
    source = "CR"
  ) %>% 
  rbind(
    mbta_t_stop %>% 
      select(c(station, line)) %>% 
      mutate(
        source = "T"
      )
  ) %>% 
  rbind(
    new_stations %>% 
      rename(station = name) %>% 
      mutate(
        source = "CR",
        line = "STOUGHTON BRANCH"
      )
  ) 

mbta_t_combined_stop <- mbta_t_combined_stop_all %>%
  distinct(station, .keep_all = TRUE)

# st_write(mbta_t_combined_stop, driver = "ESRI Shapefile", "data/combined/mbta_t_combined_stop.shp")
# st_write(mbta_t_combined_stop_all, driver = "ESRI Shapefile", "data/combined/mbta_t_combined_stop_all.shp")
st_read("data/combined/mbta_t_combined_stop.shp") %>% 
  glimpse()

st_read("data/combined/mbta_t_combined_stop_all.shp") %>% 
  glimpse()

mbta_t_combined_stop %>% 
  filter(source == "T")





## Train Routes (Extensions) ----
# Source: https://www.mass.gov/info-details/massgis-data-municipalities
townssurvey <- st_read("data/townssurvey/TOWNSSURVEY_POLY.shp") %>%
  janitor::clean_names() %>%
  st_transform(
    crs = st_crs("EPSG:26986")
  ) %>%
  select(c(town))


# Source: https://www.mass.gov/info-details/massgis-data-trains
all_trains_route <- st_read("data/trains/TRAINS_ARC.shp") %>% 
  janitor::clean_names() %>% 
  st_transform(
    crs = st_crs("EPSG:26986")
  ) %>% 
  select(c(ownership, state, freight_op, map_one, karr_rt, commrail, shape_len, line_brnch))

# Source: https://bostonopendata-boston.opendata.arcgis.com/datasets/3525b0ee6e6b427f9aab5d0a1d0a1a28/explore
bos_neigh <- st_read("data/boston_neighborhoods/Boston_Neighborhoods.shp") %>% 
  janitor::clean_names() %>% 
  filter(
    name %in% c("Roxbury", "South End")
  ) %>% 
  st_transform(
    crs = st_crs("EPSG:26986")
  )



east_west <- all_trains_route %>% 
  filter(
    line_brnch %in% c("CSX BERKSHIRE SUB", "CSX BOSTON SUB") &
      ownership == "CSX" &
      state == "MA"
  )


vermonter <- st_intersection(
  townssurvey %>% 
    filter(town %in% c(
      "DEERFIELD", "WHATELY",
      "HATFIELD", "NORTHAMPTON",
      "EASTHAMPTON", "HOLYOKE",
      "CHICOPEE", "AGAWAM",
      "SPRINGFIELD"
    )),
  all_trains_route %>% 
    filter(
      # line_brnch %in% c("CSX BERKSHIRE SUB", "CSX BOSTON SUB") &
      ownership %in% c("PAS", "NECR") &
        state == "MA" &
        !is.na(freight_op) &
        map_one == "Y" &
        !is.na(karr_rt) &
        !karr_rt %in% c(30, 35, 45) &
        is.na(commrail) &
        shape_len != 21801.530557 &
        line_brnch %in% c("CONN RIVER MAIN LINE")
    )
) %>% 
  st_cast("MULTILINESTRING")


amherst <- st_intersection(
  townssurvey %>% 
    filter(town %in% c(
      "MONTAGUE", "LEVERETT",
      "SUNDERLAND",
      "DEERFIELD", "AMHERST",
      "BELCHERTOWN", "PALMER"
    )),
  all_trains_route %>% 
    filter(
      # line_brnch %in% c("CSX BERKSHIRE SUB", "CSX BOSTON SUB") &
      ownership %in% c("PAS", "NECR") &
        state == "MA" &
        !is.na(freight_op) &
        map_one == "Y" &
        !is.na(karr_rt) &
        !karr_rt %in% c(30, 35, 45) &
        is.na(commrail) &
        shape_len != 21801.530557 &
        line_brnch %in% c("CENTRAL VERMONT")
    )
) %>% 
  st_cast("MULTILINESTRING")

golden_circle <- amherst %>% 
  rbind(vermonter)


fitchburg_ext <- all_trains_route %>% 
  filter(
    # line_brnch %in% c("CSX BERKSHIRE SUB", "CSX BOSTON SUB") &
    ownership %in% c("PAS", "NECR") &
      line_brnch == "PATRIOT CORRIDOR" &
      state == "MA" &
      !is.na(freight_op) &
      map_one == "Y" &
      !is.na(karr_rt) &
      !karr_rt %in% c(30, 35, 45) &
      is.na(commrail)
  )

streets <- st_read("data/limited_streets/streets.shp") %>% 
  rename(
    functional = functnl,
    street_name = strt_nm,
    shape_leng = shp_lng
  )

expansion <- st_intersection(
  streets %>% 
    filter(
      street_name %in% 
        c("WASHINGTON STREET", "WARREN STREET", "BLUE HILL AVENUE")
    ),
  bos_neigh %>% select(geometry)
) %>%
  st_transform(
    crs = st_crs("EPSG:26986")
  )

expansion_buffer <- expansion %>% 
  st_buffer(dist = 10) %>% 
  st_transform(
    crs = st_crs("EPSG:26986")
  )

expansion_2 <- st_intersection(
  expansion_buffer,
  tracts %>% 
    filter(
      geoid20 %in% c(
        "25025081400",
        "25025081500", "25025081302",
        "25025090400", "25025090300",
        "25025090600", "25025080100"
      )
    ) %>% 
    select(geometry)
) %>%
  st_transform(
    crs = st_crs("EPSG:26986")
  )


expansion_3 <- expansion %>% 
  st_buffer(dist = 10) %>% 
  st_filter(
    expansion_2,
    .predicate = st_disjoint
  )


olx_ext <- expansion_3 %>% 
  st_join(expansion_2) %>% 
  filter(is.na(shape_leng.y)) %>% 
  distinct(functional.x, street_name.x, shape_leng.x, .keep_all = TRUE) %>% 
  mutate(
    owner_branch = "Extension: OLX",
    ext_type = "OLX"
  ) %>% 
  select(c(ext_type, owner_branch)) %>% 
  st_cast("MULTILINESTRING")



all_ext <- golden_circle %>% 
  select(c(line_brnch, ownership)) %>% 
  mutate(
    ext_type = "CentralLoopExt",
    owner_branch = paste0("Extension: Central Loop")
  ) %>% 
  rbind(
    fitchburg_ext %>% 
      select(c(line_brnch, ownership)) %>% 
      mutate(
        ext_type = "FitchburgExt",
        owner_branch = paste0("Extension: Fitchburg Line")
      )
  ) %>% 
  rbind(
    east_west %>% 
      select(c(line_brnch, ownership)) %>% 
      mutate(
        ext_type = "WorcesterExt",
        owner_branch = paste0("Extension: East-West")
      )
  ) %>% 
  select(c(ext_type, owner_branch)) %>% 
  rbind(olx_ext) %>% 
  st_cast("MULTILINESTRING") %>% 
  rename(
    etype = ext_type,
    ob = owner_branch
  )


# st_write(all_ext, driver = "ESRI Shapefile", "data/ext/all_ext.shp")
st_read("data/ext/all_ext.shp") %>% 
  rename(
    ext_type = etype,
    owner_branch = ob
  )


## Aesthetics ----
mbta_t_combined_stop %>% 
  filter(source == "CR") %>% 
  view()

ma_outline <- townssurvey %>% 
  mutate(state = "MA") %>% 
  group_by(state) %>% 
  summarize()

# st_write(ma_outline, driver = "ESRI Shapefile", "data/state_outline/ma_outline.shp")


# Source:
# https://www.rigis.org/datasets/957468e8bb3245e8b3321a7bf3b6d4aa_0/explore?location=41.759128%2C-71.401352%2C9.83
ri_outline <- st_read("data/rhode_island/Municipalities__1997_.shp") %>% 
  janitor::clean_names() %>% 
  st_transform(
    crs = st_crs("EPSG:26986")
  ) %>% 
  mutate(
    state = "RI"
  ) %>% 
  group_by(state) %>% 
  summarize()

# st_write(ri_outline, driver = "ESRI Shapefile", "data/state_outline/ri_outline.shp")




# OLX Problem -------------------------------------------------------------
all_ext <- st_read("data/ext/all_ext.shp") %>% 
  rename(
    ext_type = etype,
    owner_branch = ob
  )

all_ext %>% 
  filter(ext_type == "OLX") %>% 
  distinct(owner_branch) %>% 
  pull()

all_combined_route_buffer <- all_ext %>% 
  mutate(
    source = "EXT"
  ) %>% 
  rename(
    line = ext_type
  ) %>% 
  rbind(mbta_t_combined_route) %>% 
  st_buffer(dist = 10) %>% 
  st_transform(
    crs = st_crs("EPSG:26986")
  )

all_combined_route_buffer %>% 
  filter(line == "OLX") %>% 
  distinct(owner_branch) %>% 
  pull()

all_selection <- all_combined_route_buffer %>% 
  distinct(owner_branch) %>% 
  pull()

all_combined_route_buffer %>%
  group_by(line, owner_branch) %>% 
  summarize() %>% 
  st_cast(
    "MULTIPOLYGON"
  ) %>% 
  view()

all_selection

tracts_all_buffer <- st_intersection(
  tracts,
  all_combined_route_buffer %>%
    filter(
      owner_branch %in% c("MBTA: OLX")
    )
) %>%
    distinct(geoid20) %>%
    pull()



if(length(c("MBTA: OLX")) > 0){
  tracts_all_buffer <- st_intersection(
    tracts,
    all_combined_route_buffer %>%
      filter(
        owner_branch %in% c("MBTA: OLX", "MBTA: ORANGE")
      )
  ) %>%
      distinct(geoid20) %>%
      pull()
  
  summary_table_gt_line(tracts_all_buffer)
  
}else{
  summary_table_gt_ma()
}




# Names of stations that are T/CR Transfers
all_trains_stop %>% view()

mbta_t_combined_stop_c <- all_trains_stop %>% 
  filter(
    c_railstat == "Y"
  ) %>% 
  rename(line = line_brnch) %>% 
  select(c(station, line)) %>% 
  mutate(
    source = "CR"
  ) %>% 
  distinct(station, .keep_all = TRUE) %>% 
  rbind(
    mbta_t_combined_stop_all %>% 
      filter(source == "T") %>% 
      select(c(station, line)) %>% 
      mutate(
        source = "T"
      ) %>% 
      distinct(station, .keep_all = TRUE)
  ) %>% 
  as_tibble() %>% 
  group_by(station) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  filter(n > 1) %>% 
  select(station) %>% 
  pull()


# It would be nice to also have a dimension for how many different lines
# pass through the connection. I can make that adjustment.
mbta_t_combined_stop_c_n <- all_trains_stop %>% 
  filter(
    c_railstat == "Y"
  ) %>% 
  rename(line = line_brnch) %>% 
  select(c(station, line)) %>% 
  mutate(
    source = "CR"
  ) %>% 
  rbind(
    mbta_t_combined_stop_all %>% 
      filter(source == "T") %>% 
      select(c(station, line)) %>% 
      mutate(
        source = "T"
      )
  ) %>% 
  rbind(
    new_stations %>% 
      rename(station = name) %>% 
      mutate(
        source = "CR",
        line = "STOUGHTON BRANCH"
      )
  ) %>% 
  as_tibble() %>% 
  group_by(station, line) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  filter(
    n > 1 &
      station %in% mbta_t_combined_stop_c
  )


mbta_t_combined_stop %>% 
  mutate(
    shape_type = case_when(
      station %in% mbta_t_combined_stop_c ~ "connection",
      ! station %in% mbta_t_combined_stop_c ~ source
    )
  )
