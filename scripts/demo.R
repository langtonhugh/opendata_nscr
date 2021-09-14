# Load packages.
library(readr)
library(dplyr)
library(janitor)
library(purrr)
library(haven)
library(forcats)
library(stringr)
library(sf)
library(ggplot2)
library(rvest)
library(jsonlite)
library(cbsodataR)

# Direct downloads =====================================================================

# Direct download (road network).
download.file(url = "https://www.rijkswaterstaat.nl/apps/geoservices/geodata/dmc/nwb-wegen/geogegevens/shapefile/NWB-light/01-12-2020.zip",
              destfile = "data/nwn_light.zip")

# Unzip.
unzip(zipfile = "data/nwn_light.zip", exdir = "data/nwn_light")

# Load.
nwb_sf <- st_read("data/nwn_light/01-12-2020/NWB-Light/nwb-light.shp")

# Plot.
ggplot(data = nwb_sf) +
  geom_sf()

# CSV from Github.
btp_df <- read_csv("https://github.com/langtonhugh/osm_crim/raw/master/data/2020-01-btp-street.csv")
                          
# Make spatial.
btp_sf <- btp_df %>% 
  st_as_sf(coords = c(x = "Longitude", y = "Latitude"), crs = 4326) %>% 
  st_transform(27700)

# Plot.
ggplot(data = btp_sf) +
  geom_sf()

# API direct =====================================================================

# API (TFL).
api_call <- fromJSON(readLines("https://api.tfl.gov.uk/line/jubilee/stoppoints"))

tfl_jub_sf <- api_call %>% 
  select(commonName, lat, lon) %>% 
  st_as_sf(coords = c(x = "lon", y = "lat"), crs = 4326) %>% 
  st_transform(27700)

# Plot.
ggplot(data = tfl_jub_sf) +
  geom_sf()

# Buffer.
tfl_buff_sf <- tfl_jub_sf %>% 
  st_buffer(dist = 50)

# Aggregate.
tfl_jub_sf <- tfl_buff_sf %>% 
  mutate(crimes = lengths(st_intersects(tfl_buff_sf, btp_sf)))

# Plot.
ggplot(data = tfl_jub_sf) +
  geom_sf(mapping = aes(colour = crimes), size = 4)

# API wrapper =====================================================================

# TOC.
cbs_get_catalogs()

# Search.
politie_df <- cbs_search("Politie")

# Get one.
citpol_df <- cbs_get_data("81928NED", Periods = has_substring("JJ"))

# Get nationwide stats for satisfaction with response times (WIP!).
citpol_pd_df <- citpol_df %>% 
  clean_names() %>% 
  zap_label() %>% 
  mutate(regio_s = trimws(regio_s),
         pd      = str_detect(regio_s, "RE")) %>% 
  filter(pd == TRUE) %>% 
  rename(pol_quick_call = komt_niet_snel_als_je_ze_roept_71) %>% 
  select(marges, regio_s, perioden, pol_quick_call) %>% 
  group_split(marges) %>%
  bind_cols() %>%
  clean_names() %>% 
  rename(regio = regio_s_2, 
         perioden = perioden_3,
         pol_quick_call_est = pol_quick_call_8,
         pol_quick_call_ci = pol_quick_call_4) %>% 
  select(regio, perioden, pol_quick_call_est, pol_quick_call_ci) %>% 
  mutate(perioden   = str_remove_all(perioden, "JJ00"),
         regio_naam = fct_recode(regio,
                                 `Northern Netherlands`   = "RE01",
                                 `East Netherlands`       = "RE02",
                                 `Central Netherlands`    = "RE03",
                                 `North Holland`          = "RE04",
                                 `Amsterdam`              = "RE05",
                                 `The Hague`              = "RE06",
                                 `Rotterdam`              = "RE07",
                                 `Zeeland - West Brabant` = "RE08",
                                 `East Brabant`           = "RE09",
                                 `Limburg`                = "RE10"))

# Plot.
ggplot() + 
  geom_ribbon(data = citpol_pd_df,
              mapping = aes(x = perioden, group  = regio_naam,
                            ymax = pol_quick_call_est+pol_quick_call_ci, ymin = pol_quick_call_est-pol_quick_call_ci),
              alpha = 0.1) +
  geom_line(data = citpol_pd_df,
            mapping = aes(x = perioden, y = pol_quick_call_est, group  = regio_naam)) +
  facet_wrap(~regio_naam, nrow = 2) +
  labs(x = NULL, y = NULL, title = "Citizen satisfaction with police response times",
       subtitle = "Percentage (strongly) agree: 'The police don't come quickly when you call them'",
       caption = "No data available in 2018.") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

# Scraping ========================================================================

# Scraping (P2000).
drimble_scrape <- read_html("https://drimble.nl/112/amsterdam/index_p") %>%   # read in page
  html_nodes(".content") %>%        # grab the content table
  html_table() %>%                  # convert to table
  pluck(1) %>%                      # grab relevant info
  row_to_names(row_number = 1) %>%  # create variable name
  clean_names()                     # clean variable names

# Show.
drimble_scrape

