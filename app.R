#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Loading Packages --------------------------------------------------------
library(shiny)
library(shinyWidgets)
library(tidyverse)
library(sf)
library(plotly)
library(gt)
tidymodels::tidymodels_prefer()
# library(janitor)

# Allowing myself to upload a file greater than 5MB to r shiny 
# options(shiny.maxRequestSize=40*1024^2)
# rsconnect::configureApp(appDir = "map_app/", size="xlarge")




# Loading Data ------------------------------------------------------------
# Source: https://data.census.gov/table?q=DP05&g=040XX00US25$1400000&tid=ACSDP5Y2021.DP05
# acs <- read_csv("data/acs/ACSDP5Y2021.DP05-Data.csv", skip = 1) %>% 
#   janitor::clean_names() %>% 
#   select(
#     c(
#       geography,
#       estimate_race_total_population,
#       estimate_race_total_population_one_race_white,
#       estimate_race_total_population_one_race_black_or_african_american,
#       estimate_race_total_population_one_race_asian,
#       estimate_hispanic_or_latino_and_race_total_population_hispanic_or_latino_of_any_race,
#       estimate_race_total_population
#     )
#   ) %>% 
#   mutate(
#     estimate_pct_one_race_black_or_african_american = 
#       (estimate_race_total_population_one_race_black_or_african_american / 
#          estimate_race_total_population) * 100,
#     estimate_pct_one_race_white = 
#       (estimate_race_total_population_one_race_white / 
#          estimate_race_total_population) * 100,
#     estimate_pct_one_race_asian = 
#       (estimate_race_total_population_one_race_asian / 
#          estimate_race_total_population) * 100,
#     estimate_pct_any_race_latino_hispanic = 
#       (estimate_hispanic_or_latino_and_race_total_population_hispanic_or_latino_of_any_race / 
#          estimate_race_total_population) * 100,
#     geoid20 = stringr::str_split(geography, "US") %>% map_chr(., 2)
#   ) %>% 
#   arrange(desc(estimate_pct_one_race_black_or_african_american)) %>% 
#   select(
#     c(
#       geoid20, estimate_pct_one_race_black_or_african_american,
#       estimate_pct_one_race_white,
#       estimate_pct_one_race_asian,
#       estimate_race_total_population,
#       estimate_pct_any_race_latino_hispanic
#     )
#   )


# Source: https://www.mass.gov/info-details/massgis-data-2020-us-census
tracts <- st_read("data/shapefiles/CENSUS2020TRACTS_POLY.shp") %>% 
  janitor::clean_names() %>% 
  st_transform(
    crs = st_crs("EPSG:26986")
  ) %>% 
  select(c(geoid20))

# 
# # Source: https://www.mass.gov/info-details/massgis-data-municipalities
# townssurvey <- st_read("data/townssurvey/TOWNSSURVEY_POLY.shp") %>%
#   janitor::clean_names() %>%
#   st_transform(
#     crs = st_crs("EPSG:26986")
#   ) %>%
#   select(c(town))


# # Source: https://www.mass.gov/info-details/massgis-data-mbta-rapid-transit
# mbta_t_route <- st_read("data/mbta_t/MBTA_ARC.shp") %>% 
#   janitor::clean_names() %>% 
#   filter(
#     line != "SILVER"
#   ) %>% 
#   st_transform(
#     crs = st_crs("EPSG:26986")
#   )


# # Source: https://www.mass.gov/info-details/massgis-data-mbta-rapid-transit
# mbta_t_stop <- st_read("data/mbta_t/MBTA_NODE.shp") %>% 
#   janitor::clean_names() %>% 
#   filter(
#     line != "SILVER"
#   ) %>% 
#   st_transform(
#     crs = st_crs("EPSG:26986")
#   ) %>% 
#   mutate(
#     station = janitor::make_clean_names(
#       station,
#       case = "big_camel",
#       allow_dupes = TRUE,
#       sep_out = " "
#     )
#   )
# # Making minor corrections- adjusting specialty names
# mbta_t_stop$station[mbta_t_stop$station == "Jfk U Mass"] <- "JFK/UMass"
# mbta_t_stop$station[mbta_t_stop$station == "Porter"] <- "Porter Square"
# mbta_t_stop$station[mbta_t_stop$station == "Charles Mgh"] <- "Charles MGH"
# mbta_t_stop$station[mbta_t_stop$station == "Medford Tufts"] <- "Medford/Tufts"


# # Source: https://bostonopendata-boston.opendata.arcgis.com/datasets/3525b0ee6e6b427f9aab5d0a1d0a1a28/explore
# bos_neigh <- st_read("data/boston_neighborhoods/Boston_Neighborhoods.shp") %>% 
#   janitor::clean_names() %>% 
#   filter(
#     name %in% c("Roxbury", "South End")
#   ) %>% 
#   st_transform(
#     crs = st_crs("EPSG:26986")
#   )


####### Need to make a shapefile that contains just those streets. 
####### The OG file is simply too large.
# # Source: https://www.mass.gov/info-details/massgis-data-base-streets
# streets <- st_read("data/limited_streets/streets.shp") %>% 
#   rename(
#     functional = functnl,
#     street_name = strt_nm,
#     shape_leng = shp_lng
#   )


# # Source: https://www.mass.gov/info-details/massgis-data-trains
# mbta_t_commuter_route <- st_read("data/trains/TRAINS_RTE_TRAIN.shp") %>% 
#   janitor::clean_names() %>% 
#   st_transform(
#     crs = st_crs("EPSG:26986")
#   ) %>% 
#   filter(
#     comm_line != "CapeFLYER",
#     comm_line != "Wildcat Branch"
#   )


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


# # Source: https://www.mass.gov/info-details/massgis-data-trains
# all_trains_route <- st_read("data/trains/TRAINS_ARC.shp") %>% 
#   janitor::clean_names() %>% 
#   st_transform(
#     crs = st_crs("EPSG:26986")
#   ) %>% 
#   select(c(ownership, state, freight_op, map_one, karr_rt, commrail, shape_len, line_brnch))


# # Source:
# # https://www.rigis.org/datasets/957468e8bb3245e8b3321a7bf3b6d4aa_0/explore?location=41.759128%2C-71.401352%2C9.83
# ri_outline <- st_read("data/rhode_island/Municipalities__1997_.shp") %>% 
#   janitor::clean_names() %>% 
#   st_transform(
#     crs = st_crs("EPSG:26986")
#   ) %>% 
#   mutate(
#     state = "RI"
#   ) %>% 
#   group_by(state) %>% 
#   summarize()
ri_outline <- st_read("data/state_outline/ri_outline.shp")


# Data Cleaning Help from
# https://stackoverflow.com/questions/42565539/using-strsplit-and-subset-in-dplyr-and-mutate





# Derived Data ------------------------------------------------------------
# acs_shp <- acs %>% 
#   inner_join(tracts, by = "geoid20") %>% 
#   st_as_sf(
#     crs = "EPSG:26986"
#   )
acs_shp <- st_read("data/acs/acs_shp.shp") %>%
  rename(
    estimate_pct_one_race_black_or_african_american = eaa,
    estimate_pct_one_race_white = ew,
    estimate_pct_one_race_asian = ea,
    estimate_race_total_population = et,
    estimate_pct_any_race_latino_hispanic = elh
  )

# ma_outline <- townssurvey %>% 
#   mutate(state = "MA") %>% 
#   group_by(state) %>% 
#   summarize()
ma_outline <- st_read("data/state_outline/ma_outline.shp")

## Determine if I need this
# boston <- townssurvey %>% 
#   filter(town == "BOSTON") %>% 
#   group_by(town) %>% 
#   summarize() %>% 
#   st_transform(
#     crs = st_crs("EPSG:26986")
#   )

# expansion <- st_intersection(
#   streets %>% 
#     filter(
#       street_name %in% 
#         c("WASHINGTON STREET", "WARREN STREET", "BLUE HILL AVENUE")
#     ),
#   bos_neigh %>% select(geometry)
# ) %>%
#   st_transform(
#     crs = st_crs("EPSG:26986")
#   )
# 
# expansion_buffer <- expansion %>% 
#   st_buffer(dist = 10) %>% 
#   st_transform(
#     crs = st_crs("EPSG:26986")
#   )
# 
# expansion_2 <- st_intersection(
#   expansion_buffer,
#   tracts %>% 
#     filter(
#       geoid20 %in% c(
#         "25025081400",
#         "25025081500", "25025081302",
#         "25025090400", "25025090300",
#         "25025090600", "25025080100"
#       )
#     ) %>% 
#     select(geometry)
# ) %>%
#   st_transform(
#     crs = st_crs("EPSG:26986")
#   )
# 
# 
# expansion_3 <- expansion %>% 
#   st_buffer(dist = 10) %>% 
#   st_filter(
#     expansion_2,
#     .predicate = st_disjoint
#   )
# 
# 
# olx_ext <- expansion_3 %>% 
#   st_join(expansion_2) %>% 
#   filter(is.na(shape_leng.y)) %>% 
#   distinct(functional.x, street_name.x, shape_leng.x, .keep_all = TRUE) %>% 
#   mutate(
#     owner_branch = "MBTA: OLX",
#     ext_type = "OLX"
#   ) %>% 
#   select(c(ext_type, owner_branch)) %>% 
#   st_cast("MULTILINESTRING")


## These two are potentially useless
# tracts_of_interest <- st_intersection(
#   tracts,
#   expansion_3
# ) %>% 
#   filter(
#     !geoid20 %in% c(
#       "25025081400",
#       "25025081500", "25025081302",
#       "25025090400", "25025090300",
#       "25025090600", "25025080100",
#       "25025081301", "25025070202",
#       "25025070402", "25025980300"
#     )
#   ) %>% 
#   distinct(geoid20) %>% 
#   pull()
# current_tracts <- st_intersection(
#   tracts,
#   mbta_t_route
# ) %>% 
#   distinct(geoid20) %>% 
#   pull()


# mbta_t_buffer <- mbta_t_route %>% 
#   st_buffer(dist = 10)

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

# east_west <- all_trains_route %>% 
#   filter(
#     line_brnch %in% c("CSX BERKSHIRE SUB", "CSX BOSTON SUB") &
#       ownership == "CSX" &
#       state == "MA"
#   )
# 
# 
# vermonter <- st_intersection(
#   townssurvey %>% 
#     filter(town %in% c(
#       "DEERFIELD", "WHATELY",
#       "HATFIELD", "NORTHAMPTON",
#       "EASTHAMPTON", "HOLYOKE",
#       "CHICOPEE", "AGAWAM",
#       "SPRINGFIELD"
#     )),
#   all_trains_route %>% 
#     filter(
#       # line_brnch %in% c("CSX BERKSHIRE SUB", "CSX BOSTON SUB") &
#       ownership %in% c("PAS", "NECR") &
#         state == "MA" &
#         !is.na(freight_op) &
#         map_one == "Y" &
#         !is.na(karr_rt) &
#         !karr_rt %in% c(30, 35, 45) &
#         is.na(commrail) &
#         shape_len != 21801.530557 &
#         line_brnch %in% c("CONN RIVER MAIN LINE")
#     )
# ) %>% 
#   st_cast("MULTILINESTRING")
# 
# 
# amherst <- st_intersection(
#   townssurvey %>% 
#     filter(town %in% c(
#       "MONTAGUE", "LEVERETT",
#       "SUNDERLAND",
#       "DEERFIELD", "AMHERST",
#       "BELCHERTOWN", "PALMER"
#     )),
#   all_trains_route %>% 
#     filter(
#       # line_brnch %in% c("CSX BERKSHIRE SUB", "CSX BOSTON SUB") &
#       ownership %in% c("PAS", "NECR") &
#         state == "MA" &
#         !is.na(freight_op) &
#         map_one == "Y" &
#         !is.na(karr_rt) &
#         !karr_rt %in% c(30, 35, 45) &
#         is.na(commrail) &
#         shape_len != 21801.530557 &
#         line_brnch %in% c("CENTRAL VERMONT")
#     )
# ) %>% 
#   st_cast("MULTILINESTRING")
# 
# golden_circle <- amherst %>% 
#   rbind(vermonter)
# 
# 
# fitchburg_ext <- all_trains_route %>% 
#   filter(
#     # line_brnch %in% c("CSX BERKSHIRE SUB", "CSX BOSTON SUB") &
#     ownership %in% c("PAS", "NECR") &
#       line_brnch == "PATRIOT CORRIDOR" &
#       state == "MA" &
#       !is.na(freight_op) &
#       map_one == "Y" &
#       !is.na(karr_rt) &
#       !karr_rt %in% c(30, 35, 45) &
#       is.na(commrail)
#   )
# 
# 
# all_ext <- golden_circle %>% 
#   select(c(line_brnch, ownership)) %>% 
#   mutate(
#     ext_type = "CentralLoopExt",
#     owner_branch = paste0(ownership, ": ", line_brnch)
#   ) %>% 
#   rbind(
#     fitchburg_ext %>% 
#       select(c(line_brnch, ownership)) %>% 
#       mutate(
#         ext_type = "FitchburgExt",
#         owner_branch = paste0(ownership, ": ", line_brnch)
#       )
#   ) %>% 
#   rbind(
#     east_west %>% 
#       select(c(line_brnch, ownership)) %>% 
#       mutate(
#         ext_type = "WorcesterExt",
#         owner_branch = paste0(ownership, ": ", line_brnch)
#       )
#   ) %>% 
#   select(c(ext_type, owner_branch)) %>% 
#   rbind(olx_ext) %>% 
#   st_cast("MULTILINESTRING")

all_ext <- st_read("data/ext/all_ext.shp") %>% 
  rename(
    ext_type = etype,
    owner_branch = ob
  )


# mbta_t_combined_route <- mbta_t_commuter_route %>% 
#   rename(
#     line = comm_line
#   ) %>% 
#   select(line) %>%
#   mutate(
#     source = "CR"
#   ) %>% 
#   rbind(
#     mbta_t_route %>% 
#       select(line) %>% 
#       mutate(
#         source = "T"
#       )
#   ) %>% 
#   mutate(
#     owner_branch = paste("MBTA:", line)
#   )

mbta_t_combined_route <- st_read("data/combined/mbta_t_combined_route.shp") %>% 
  rename(
    owner_branch = ob
  )




# mbta_t_combined_stop <- all_trains_stop %>% 
#   filter(
#     c_railstat == "Y"
#   ) %>% 
#   rename(line = line_brnch) %>% 
#   select(c(station, line)) %>% 
#   mutate(
#     source = "CR"
#   ) %>% 
#   rbind(
#     mbta_t_stop %>% 
#       select(c(station, line)) %>% 
#       mutate(
#         source = "T"
#       )
#   ) %>% 
#   rbind(
#     new_stations %>% 
#       rename(station = name) %>% 
#       mutate(
#         source = "CR",
#         line = "STOUGHTON BRANCH"
#       )
#   ) %>% 
#   distinct(station, .keep_all = TRUE)
mbta_t_combined_stop <- st_read("data/combined/mbta_t_combined_stop.shp")
mbta_t_combined_stop_all <- st_read("data/combined/mbta_t_combined_stop_all.shp")

# Names of stations that are T/CR Transfers
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
  rbind(
    new_stations %>% 
      rename(station = name) %>% 
      mutate(
        source = "CR",
        line = "STOUGHTON BRANCH"
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


mbta_t_combined_stop_all <- mbta_t_combined_stop_all %>% 
  mutate(
    systems_served = case_when(
      station %in% mbta_t_combined_stop_c ~ "CR&T",
      ! station %in% mbta_t_combined_stop_c ~ source
    )
  ) %>% 
  distinct(station, systems_served, source, .keep_all = TRUE)

mbta_t_combined_stop <- mbta_t_combined_stop %>% 
  mutate(
  systems_served = case_when(
    station %in% mbta_t_combined_stop_c ~ "CR&T",
    ! station %in% mbta_t_combined_stop_c ~ source
  )
)


# test_tribble <- tibble::tribble(
#   ~"row", ~"value",
#   "FitchburgExt", "one",
#   "CentralLoopExt", "two",
#   "WorcesterExt", "three"
# )



# Aesthetics
#### Update Extension!!
ext_colors <- c(
  "FitchburgExt" = "blue",
  "CentralLoopExt" = "forestgreen",
  "WorcesterExt" = "pink",
  "OLX" = "#8c641f"
)

cr_colors <- c()
cr_lines <- mbta_t_combined_route %>% 
  filter(source == "CR") %>% 
  as_tibble() %>%
  select(line) %>% 
  pull()
for (line in cr_lines){
  cr_colors[line] = "purple"
}


mbta_t_colors <- c(
  "BLUE" = "blue",
  "GREEN" = "green",
  "ORANGE" = "orange",
  "RED" = "red"
)

plot_colors <- append(append(ext_colors, cr_colors), mbta_t_colors)

plot_shapes <- c(
  "T" = 21,
  "CR" = 22,
  "CR&T" = 23
)

labels_dict <- c(
  "Asian" = "estimate_pct_one_race_asian",
  "Black" = "estimate_pct_one_race_black_or_african_american",
  "Hispanic/Latino" = "estimate_pct_any_race_latino_hispanic",
  "White" = "estimate_pct_one_race_white"
)


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
  ) %>% 
  group_by(line, owner_branch) %>% 
  summarize() %>% 
  st_cast(
    "MULTIPOLYGON"
  )

all_selection <- all_combined_route_buffer %>% 
  distinct(owner_branch) %>% 
  arrange(owner_branch) %>% 
  pull()







# Functions ---------------------------------------------------------------
summary_table_gt_line <- function(i_tracts){
  line_demographics_table <- acs_shp %>% 
    filter(geoid20 %in% i_tracts) %>% 
    distinct(geoid20, .keep_all = TRUE) %>% 
    drop_na()
  
  ma_demographics_table <- acs_shp %>% 
    filter(!geoid20 %in% i_tracts) %>% 
    distinct(geoid20, .keep_all = TRUE) %>% 
    drop_na()
  
  # group_name <- paste(mbta_line, "Line")
  summary_table <- tribble(
    ~"Group", ~"Median Asian %", ~"Median Black %",
    ~"Median Hispanic %", ~"Median White %", ~"Total Population",
    "Selected Line(s)", 
    line_demographics_table$estimate_pct_one_race_asian %>% median(), 
    line_demographics_table$estimate_pct_one_race_black_or_african_american %>% median(),
    line_demographics_table$estimate_pct_any_race_latino_hispanic %>% median(),
    line_demographics_table$estimate_pct_one_race_white %>% median(),
    line_demographics_table$estimate_race_total_population %>% sum(),
    "Rest of MA",
    ma_demographics_table$estimate_pct_one_race_asian %>% median(), 
    ma_demographics_table$estimate_pct_one_race_black_or_african_american %>% median(),
    ma_demographics_table$estimate_pct_any_race_latino_hispanic %>% median(),
    ma_demographics_table$estimate_pct_one_race_white %>% median(),
    ma_demographics_table$estimate_race_total_population %>% sum()
  )
  
  pop_tribble <- tribble(
    ~"Population % of MA",
    round((line_demographics_table$estimate_race_total_population %>% sum() /
             ma_demographics_table$estimate_race_total_population %>% sum()) * 100, digits = 2),
  ) %>% 
    mutate(`Population % of MA` = as.character(`Population % of MA`)) %>% 
    rbind(" ")
  
  
  summary_table <- summary_table %>% 
    cbind(pop_tribble) %>% 
    gt() %>% 
    fmt_number(
      columns = contains("Median"),
      decimals = 2
    )
  
  return(summary_table)
}

summary_table_gt_ma <- function(){
  ma_demographics_table <- acs_shp %>% 
    distinct(geoid20, .keep_all = TRUE) %>% 
    drop_na()
  
  # group_name <- paste(mbta_line, "Line")
  summary_table <- tribble(
    ~"Group", ~"Median Asian %", ~"Median Black %",
    ~"Median Hispanic %", ~"Median White %", ~"Total Population",
    "Entirety of MA",
    ma_demographics_table$estimate_pct_one_race_asian %>% median(), 
    ma_demographics_table$estimate_pct_one_race_black_or_african_american %>% median(),
    ma_demographics_table$estimate_pct_any_race_latino_hispanic %>% median(),
    ma_demographics_table$estimate_pct_one_race_white %>% median(),
    ma_demographics_table$estimate_race_total_population %>% sum()
  )
  
  summary_table <- summary_table %>% 
    gt() %>% 
    fmt_number(
      columns = contains("Median"),
      decimals = 2
    )
  
  return(summary_table)
}















































# App ---------------------------------------------------------------------
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Exploring Passenger Rail Options in Massachusetts"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            h3("Adjust Map"),
            selectInput("race",
                         h4("Demographic"),
                         choices = c(
                           # "Asian",
                           # "Black",
                           # "Hispanic/Latino",
                           # "White"
                             "Asian (One Race)" = "Asian",
                             "Black (One Race)" = "Black",
                             "Hispanic/Latino (Any Race)" = "Hispanic/Latino",
                             "White (One Race)" = "White"
                         ),
                         selected = "Asian"
            ),
            checkboxGroupInput("mbtaSystem",
                               h4("Select an Existing System"),
                               choices = list(
                                 "MBTA T" = "T",
                                 "MBTA Commuter Rail" = "CR"),
                               selected = c("T", "CR")
                               ),
                               selected = c("FitchburgExt", "CentralLoopExt", "WorcesterExt"),
            checkboxGroupInput("checkGroupC",
                               h4("Select an Extension Project"),
                               choices = list(
                                 "Fitchburg Line" = "FitchburgExt",
                                 "Central Loop" = "CentralLoopExt",
                                 "East-West" = "WorcesterExt",
                                 "Orange Line Extension (OLX)" = "OLX"
                               ),
                               selected = c("FitchburgExt", "CentralLoopExt", "WorcesterExt", "OLX")
            ),
            sliderInput(
              "xLimits",
              h4("Select Longitude Limits"),
              min = 40000,
              max = 340000,
              step = 5000,
              value = c(40000, 340000)
            ),
            sliderInput(
              "yLimits",
              h4("Select Latitude Limits"),
              min = 785000,
              max = 956500,
              step = 5000,
              value = c(785000, 956500)
            ),
            h3("Numerical Analysis"),
            pickerInput(
              "pickerLines",
              h4("Select Lines"),
              choices = all_selection,
              multiple = TRUE,
              selected = all_selection,
              options = list(`actions-box` = TRUE)
            ),
            selectInput(
              "dissolve",
              h4("Dissolve Census Tract Boundaries"),
              choices = c("True", "False"),
              selected = "True"
            ),
            h3("More Information"),
            a(
              "Explanation of Filters and Data", 
              href = "https://docs.google.com/document/d/1cM-yr9bKee8ABtpNaHm52dJjbE0gH0DvGhaB1DLcorI/edit?usp=sharing"
              )
            # ,
            # checkboxGroupInput("lineOptions",
            #                    h4("Select a Line"),
            #                    choices = output$line_options
            #                    )
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
           h3("Adjust Map"),
           tableOutput("test"),
           # plotlyOutput("distPlot"),
           plotOutput("distPlot"),
           hr(style = "border-top: 1px solid #000000;"),
           # textOutput("distPlot"),
           h3("Numerical Analysis"),
           tableOutput("numbers"),
           # textOutput("texting"),
           # dataTableOutput("length"),
           # plotlyOutput("selectionPlot")
           # tableOutput("printing"),
           plotOutput("selectionPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    display_text <- reactive({paste0("Percentage\n", input$race)})
    column_race <- reactive({labels_dict[input$race]})


    # acs_shp_map_static <- acs_shp %>%
    #       select(c(geoid20, estimate_pct_one_race_asian))
    

    acs_shp_map <- reactive({
      acs_shp %>%
        select(c(!!!column_race(), geoid20)) %>%
        setNames(c("race", "geoid20", "geometry")) %>%
        # rename(
        #   race = !!!column_race()
        # ) %>%
        select(c(geoid20, race, geometry))
    # dplyr::select(!!!column_race()) %>% 
    
    })


    mbta_routes_reactive <- reactive({
      if(("T" %in% input$mbtaSystem) | ("CR" %in% input$mbtaSystem)){
        mbta_t_combined_route %>%
          filter(
            source %in% input$mbtaSystem
          )
      }
    })


    mbta_stops_reactive <- reactive({
      if(("T" %in% input$mbtaSystem) | ("CR" %in% input$mbtaSystem)){
        mbta_t_combined_stop_all %>%
          filter(
            source %in% input$mbtaSystem
          ) %>% 
          distinct(station, source, .keep_all = TRUE)
      }
    })




    # All Maps Share this feature
    base_map <- reactive({
      ggplot() +
        geom_sf(
          data = acs_shp_map(),
          aes(
            geometry = geometry,
            fill = race
          ),
          color = NA
        ) +
        geom_sf(
          data = ma_outline,
          aes(
            geometry = geometry
          ),
          fill = NA,
          show.legend = FALSE
        ) +
        geom_sf(
          data = ri_outline,
          aes(geometry = geometry),
          fill = "grey80",
          color = "black"
        ) +
        labs(
          fill = display_text()
        ) +
        theme_void()
    })


    freight_map_t_cr_reactive <- reactive({
      # plot_stops <- mbta_routes_reactive()
      if("T" %in% input$mbtaSystem | "CR" %in% input$mbtaSystem){
        base_map() +
          geom_sf(
            data = mbta_routes_reactive(),
            aes(
              geometry = geometry,
              color = line,
              group = owner_branch
            ),
            lwd = 1.1,
            show.legend = FALSE
          ) +
          geom_sf(
            data = mbta_stops_reactive() #%>%
            #   filter(
            #     !station %in% mbta_t_combined_stop_c
            # )
            ,
            aes(
              geometry = geometry,
              group = station,
              shape = systems_served
            ),
            color = "black",
            fill = "white",
            show.legend = FALSE
          ) #+
          # geom_sf(
          #   data = mbta_stops_reactive() %>%
          #     filter(
          #         station %in% mbta_t_combined_stop_c
          #     ) %>%
          #     inner_join(
          #       mbta_t_combined_stop_c_n,
          #       by = "station"
          #     ) %>%
          #     mutate(
          #       station_n = paste0(station, ": ", n)
          #     ),
          #   aes(
          #     geometry = geometry,
          #     group = station_n
          #   ),
          #   color = "black",
          #   fill = "white",
          #   show.legend = FALSE,
          #   shape = 23,
          #   size = 2
          # )
      }else{
        base_map()
      }

    })


    all_ext_reactive <- reactive({
      if("FitchburgExt" %in% input$checkGroupC |
         "CentralLoopExt" %in% input$checkGroupC |
         "WorcesterExt" %in% input$checkGroupC |
         "OLX" %in% input$checkGroupC
         ){
        all_ext %>%
          filter(
            ext_type %in% input$checkGroupC
          )
      }
    })


    # Extension Projects
    freight_ext_map_reactive <- reactive({
      if("FitchburgExt" %in% input$checkGroupC |
         "CentralLoopExt" %in% input$checkGroupC |
         "WorcesterExt" %in% input$checkGroupC |
         "OLX" %in% input$checkGroupC
         ){

        freight_map_t_cr_reactive() +
          geom_sf(
            data = all_ext_reactive(),
            aes(
              geometry = geometry,
              group = owner_branch,
              color = ext_type
            ),
            lwd = 1.1,
            show.legend = FALSE
          ) 
      }else{
        freight_map_t_cr_reactive()
      }
    })
    
    
    final_plot_reactive <- reactive({
      if("T" %in% input$mbtaSystem | "CR" %in% input$mbtaSystem){
        freight_ext_map_reactive() +
          scale_fill_gradientn(
            colors = c("white", "orange"),
            na.value = "grey80",
            limits = c(0, 100)
          ) +
          scale_color_manual(
            values = plot_colors
          ) +
          scale_shape_manual(
            values = plot_shapes
          )
      }else{
        freight_ext_map_reactive() +
          scale_fill_gradientn(
            colors = c("white", "orange"),
            na.value = "grey80",
            limits = c(0, 100)
          ) +
          scale_color_manual(
            values = plot_colors
          ) +
          scale_shape_manual(
            values = plot_shapes
          )
      }
      
    })
    


    # output$distPlot <- renderPlotly({
    output$distPlot <- renderPlot({
      final_plot_reactive() +
        coord_sf(
          xlim = c(input$xLimits[1], input$xLimits[2]),
          ylim = c(input$yLimits[1], input$yLimits[2])
        )
      # ggplotly(final_plot_reactive()) %>%
      #     plotly::layout(plot_bgcolor="#aed5e8", showlegend = FALSE)
    })

    
    
    
    
    # line_options <- reactive({
    #   if("T" %in% input$mbtaSystem | "CR" %in% input$mbtaSystem){
    #     mbta_t_combined_route %>%
    #       filter(
    #         source %in% input$mbtaSystem
    #       ) %>%
    #       distinct(line) %>%
    #       pull()
    #   }else{
    #     c()
    #   }
    # })
  

    # output$printing <- renderTable({
    #   if(length(reactive(input$pickerLines)) > 0){
    #     tracts_all_buffer <- reactive({st_intersection(
    #       tracts,
    #       all_combined_route_buffer %>%
    #         filter(
    #           owner_branch %in% reactive(input$pickerLines)
    #         )
    #     ) %>%
    #         distinct(geoid20) %>%
    #         pull()
    #     })
    #     
    #     test_df <- reactive({
    #       all_combined_route_buffer %>%
    #         filter(
    #           owner_branch %in% reactive(input$pickerLines)
    #         )
    #     })
    #     
    #     # print(tracts_all_buffer())
    #     test_df()
    # 
    #   }else{
    #     # print(" ")
    #     mbta_t_combined_stop_c_n
    #   }
    # })
    
    
    output$numbers <- renderTable({
      if(length(input$pickerLines)){
        tracts_all_buffer <- reactive({st_intersection(
          tracts,
          all_combined_route_buffer %>%
            filter(
              owner_branch %in% input$pickerLines
            )
        ) %>%
          as_tibble() %>% 
          distinct(geoid20) %>%
          pull()
        })

        summary_table_gt_line(tracts_all_buffer())

      }else{
        summary_table_gt_ma()
      }
    })


    # output$selectionPlot <- renderPlotly({
    output$selectionPlot <- renderPlot({
      if(length(input$pickerLines) > 0){

        if(input$dissolve == "False"){
          tracts_all_buffer <- reactive({st_intersection(
            tracts,
            all_combined_route_buffer %>%
              filter(
                owner_branch %in% input$pickerLines
              )
          ) %>%
              as_tibble() %>% 
              distinct(geoid20) %>%
              pull()
          })
          
          selection_plot <- reactive({ggplot() +
            geom_sf(
              data = ma_outline,
              aes(geometry = geometry),
              fill = "white",
              color = "black"
            ) +
            geom_sf(
              data = tracts %>%
                filter(
                  geoid20 %in% tracts_all_buffer()
                ),
              aes(geometry = geometry),
              color = "purple",
              fill = "grey80"
            ) +
            theme_void()
          })
          
          selection_plot()

          # ggplotly(selection_plot()) %>%
          #   plotly::layout(plot_bgcolor="#aed5e8", showlegend = FALSE)
        }else{
          tracts_all_buffer <- reactive({st_intersection(
            tracts,
            all_combined_route_buffer %>%
              filter(
                owner_branch %in% input$pickerLines
              )
          ) %>%
              distinct(geoid20) %>%
              pull()
          })

          selection_plot <- reactive({ggplot() +
              geom_sf(
                data = ma_outline,
                aes(geometry = geometry),
                fill = "white",
                color = "black"
              ) +
              geom_sf(
                data = tracts %>%
                  filter(
                    geoid20 %in% tracts_all_buffer()
                  ) %>%
                  mutate(
                    placeholder = "p"
                  ) %>%
                  group_by(placeholder) %>%
                  summarize(),
                aes(geometry = geometry),
                color = "purple",
                fill = "grey80"
              ) +
              theme_void()
          })

          selection_plot()
          # ggplotly(selection_plot()) %>%
          #   plotly::layout(plot_bgcolor="#aed5e8", showlegend = FALSE)
        }

      }else{
        selection_plot <- ggplot() +
          geom_sf(
            data = ma_outline,
            aes(geometry = geometry),
            fill = "white",
            color = "black"
          ) +
          theme_void()

        selection_plot
        # ggplotly(selection_plot) %>%
        #   plotly::layout(plot_bgcolor="#aed5e8", showlegend = FALSE)

      }
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
