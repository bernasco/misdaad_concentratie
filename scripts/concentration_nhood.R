# concentration_nhood.R

# Load and install required packages if necessary ------------------------------
for(pkg_name_chr in c("here",             # local file references 
                      "tidyverse",        # data manipulation
                      "cbsodataR",        # CBS data API interface 
                      "DescTools",        # Gini
                      "remotes",          # install from outside CRAN
                      "sf")) {            # spatial stuff
  if(!require(pkg_name_chr, character.only = TRUE)) {
    install.packages(pkg_name_chr)
    library(pkg_name_chr)
  }
}

if(!require("lorenzgini", character.only = TRUE)) {
  remotes::install_github("wsteenbeek/lorenzgini", upgrade = "always")
  library(lorenzgini)
}


# Create directories -----------------------------------------------------------

# Directory names (CAPITAL)
DATA              <-  here("data")                 # main data folder
DATA_POPULATION   <-  here("data", "population")   # population
DATA_CRIME        <-  here("data", "crime")        # crime
DATA_GEO          <-  here("data", "geo")          # geometries
DATA_WORK         <-  here("data", "work")         # working data
OUTPUT            <-  here("output")               # all output

# Create these directories
map(c(DATA,  DATA_POPULATION, DATA_CRIME, DATA_GEO, DATA_WORK, OUTPUT), 
    dir.create, showWarnings=FALSE)

# Define constants and settings ------------------------------------------------
set.seed(12345)

# Define ggplot printing functions ---------------------------------------------

# Function definition: store ggplot figure as PNG file
ggsave_png <- function(ggp, output, ...) {
  ggsave(filename = paste(substitute(ggp),".png", sep=""), 
         device = "png", plot = ggp, path = output, 
         limitsize = TRUE, ...)
}

# Function definition: store ggplot figure as SVG file
ggsave_svg <- function(ggp, output, ...) {
  ggsave(filename = paste(substitute(ggp),".svg", sep=""), 
         device = "svg", plot = ggp, path = output, 
         limitsize = TRUE, ...)
}

# Function definition: store ggplot figure as JPG file
ggsave_jpg <- function(ggp, output, ...) {
  ggsave(filename = paste(substitute(ggp),".jpg", sep=""), 
         device = "jpg", plot = ggp, path = output, 
         limitsize = TRUE, dpi = 600, ...)
}

# Download and store population data -------------------------------------------
FILENAME_POPULATION <- "Population85618_2023.csv"
# only download it the file is not locally present
if (!(file.exists(here(DATA_POPULATION, FILENAME_POPULATION)))) {
  # Kerncijfers Wijken en buurten 2023
  cbs_get_data(id = "85618NED",            
               dir = DATA_POPULATION) |>
    # we only need the neighborhood population data  
    dplyr::filter(trimws(SoortRegio_2) == "Buurt") |>
    # we only need neighborhood ID, population and area  
    dplyr::select(nhood_id = WijkenEnBuurten, 
                  population = AantalInwoners_5,
                  area = OppervlakteLand_113) |>
    write.csv(here(DATA_POPULATION, FILENAME_POPULATION))
}

# Download and store crime data ------------------------------------------------
FILENAME_CRIME <- "PolRegCrime47018_2023.csv"
# conditional download
if (!(file.exists(here(DATA_CRIME, FILENAME_CRIME)))) {
  cbs_get_data(id="47018NED", 
               Perioden = has_substring("2023JJ"),          # year 2023 only 
               WijkenEnBuurten = has_substring("BU"),        # nhoods only
               SoortMisdrijf = has_substring("0.0.0"),       # all crimes only
               catalog="Politie", 
               verbose=TRUE, 
               base_url="https://dataderden.cbs.nl",
               dir=DATA_CRIME) |>
    cbs_add_label_columns()|>
    cbs_add_date_column() |>
    # store
    write.csv(here(DATA_CRIME, FILENAME_CRIME))
}

# Download and store geometries-------------------------------------------------

# download map Netherlands 2023
FILENAME_GEOZIP <- "wijkbuurtkaart_2023_v1.zip"
# only download the file if it does not exist locally
if (!(file.exists(here(DATA_GEO, FILENAME_GEOZIP)))) {
  download.file(
    url = file.path("https://download.cbs.nl/regionale-kaarten", FILENAME_GEOZIP),
    destfile = here(DATA_GEO, "wijkbuurtkaart_2023_v1.zip") 
  )
}
# unzip the map
# only unzip if "buurten_2023_v1.prj" exists locally (assuming all other
#   files from the ZIP also exist)
if (!(file.exists(here(DATA_GEO, "buurten_2023_v1.prj")))) {
  unzip(zipfile = here(DATA_GEO, FILENAME_GEOZIP),
        exdir   = here(DATA_GEO),
        junkpaths = TRUE)
}

# # remove the .Zip file
# if (file.exists(here(DATA_GEO, FILENAME_GEOZIP))) {
#   file.remove(here(DATA_GEO, FILENAME_GEOZIP))
# }


# Read and process data --------------------------------------------------------

# read IDs and labels of municipalities
municipality_nhood_df <-
  read.csv(here(DATA_CRIME, "WijkenEnBuurten.csv")) |>  
  dplyr::filter(substring(Key, 1,2) == "GM") |>
  mutate(municipality_id  = trimws(substring(Key, 3, 6)),
         municipality_lab = trimws(Title)) |>
  dplyr::select(municipality_id, municipality_lab) 

# process crime data
crime_2023_df <-
  # read the downloaded crime data file
  read.csv(here(DATA_CRIME, FILENAME_CRIME)) |>
  # rename and convert columns 
  dplyr::mutate(
    nhood_id         = WijkenEnBuurten,
    nhood_lab        = WijkenEnBuurten_label,
    year             = Perioden_label,
    crime            = GeregistreerdeMisdrijven_1,
    municipality_id  = substring(WijkenEnBuurten, 3, 6)
  ) |>
  # remove cases with unknown neighborhood ID
  dplyr::filter(nhood_lab != "Buurt onbekend") |>
  # add municipality labels (names)
  left_join(municipality_nhood_df, by = "municipality_id") |>
  # select required columns
  dplyr::select(year, nhood_id, nhood_lab, crime, 
                municipality_id, municipality_lab)

# read population data
population_2023_df <-
  read.csv(here(DATA_POPULATION, FILENAME_POPULATION))

# read geometry data
FILENAME_MUNICIPALTIES_GEO <- "gemeenten_2023_v1.shp"
geometry_municipalities_sf <- 
  sf::st_read(dsn = file.path(DATA_GEO, FILENAME_MUNICIPALTIES_GEO)) |>
  # exclude bodies of water
  dplyr::filter(H2O == "NEE", GM_NAAM != "Buitenland") |>
  # Create municipality ID (mumeric part)
  mutate(municipality_id = trimws(substring(GM_CODE, 3, 6)))


# merge crime and population data 
crime_population_nhoods_df <-
  dplyr::inner_join(crime_2023_df, population_2023_df, by = "nhood_id") 

# Calculate correlation between population and crime (neighborhood-level)
crime_population_nhoods_df |>
  dplyr::select(crime, population) |>
  cor()

# Scatterplot crime and population (neighborhood-level)
crime_population_nhoods_df  |>
  ggplot() +
  geom_point(aes(x= population, y = crime), size = .5) +
  xlab("Bevolking") + 
  ylab("Geregistreerde misdrijven")

# histogram of neighborhood population
crime_population_nhoods_df |> 
  ggplot() + 
  geom_histogram(aes(x = population), color = "black", fill = "darkgreen") +
  xlab("Aantal inwoners") +
  ylab("Aantal buurten")

# histogram of neighborhood crime
crime_population_nhoods_df |> 
  ggplot() + 
  geom_histogram(aes(x = crime), color = "black", fill = "orange")  +
  xlab("Aantal inwoners") +
  ylab("Aantal geregistreerde misdrijven")



# Calculate Gini's per municipality --------------------------------------------
municipality_gini_df <- 
  crime_population_nhoods_df |>
  dplyr::group_by(municipality_lab, municipality_id) |>
  dplyr::summarize(
    n_nhoods = n(),
    n_crime = sum(crime),
    n_pop = sum(population),
    # unweighted Gini coefficient (based in frequencies)
    gini_crime_unweighted = Gini(crime),
    gini_pop_unweighted = Gini(population),
    # Gini weighted by population
    gini_crime_pop_weighted = Gini(crime, weights = population),
    # municipality-level population (for filtering)
    .groups = "drop"
  ) 

# Calculate correlation between crime and population (municipality-level)
municipality_gini_df |> 
  dplyr::select(n_crime, n_pop) |> 
  cor()

# Scatterplot crime and population (municipality-level)
municipality_gini_df |>
  ggplot() +
  geom_point(aes(x= n_pop, y = n_crime), size = 1) +
  xlab("Bevolking") + 
  ylab("Geregistreerde misdrijven")

# Scatterplot crime and population (municipality-level excluding G4)
municipality_gini_df |>
  filter(n_pop < 300000) |>
  ggplot() +
  geom_point(aes(x= n_pop, y = n_crime), size = 1) +
  xlab("Bevolking") + 
  ylab("Geregistreerde misdrijven")

# Scatterplot crime and gini
municipality_gini_df |>
  filter(n_pop < 300000,
         n_pop > 10000) |>
  ggplot() +
  geom_point(aes(x= n_pop, y = gini_crime_unweighted), size = 1) +
  xlab("Bevolking") + 
  ylab("Gini")

# Count number of neighborhoods per municipality -------------------------------
municipality_nhood_count <- 
  municipality_gini_df |>
  dplyr::mutate(nhoods_cat = case_match(
    n_nhoods,
    1:9  ~ "01-09",
    10:19 ~ "10-19",
    20:49 ~ "20-49",
    50:99 ~ "50-99",
    100:999 ~ "99+") 
  ) |>
  dplyr::group_by(nhoods_cat) |>
  dplyr::summarize(n = n()) |>
  dplyr::mutate(nhoods_cat = as.character(nhoods_cat))
municipality_nhood_count 

write_csv(x = municipality_nhood_count, 
          file = here("output", "municipality_nhood_count.csv"))


# List municipalities with less then 10 neighborhoods --------------------------
municipalities_less10_nhoods <-
  municipality_gini_df |>
  dplyr::filter(n_nhoods < 10) |>
  dplyr::select(municipality_lab) |>
  dplyr::arrange(municipality_lab)
municipalities_less10_nhoods |>
  print(n=Inf)

# List municipalities with less then 5 neighborhoods --------------------------
municipalities_less5_nhoods <-
  municipality_gini_df |>
  dplyr::filter(n_nhoods < 5) |>
  dplyr::select(municipality_lab) |>
  dplyr::arrange(municipality_lab)
municipalities_less5_nhoods |>
  print(n=Inf)


# select a random municipality with at least 10 neighborhoods ------------------
municipality_random_selection <-
  municipality_gini_df |>
  dplyr::filter(n_nhoods >= 10) |>
  dplyr::slice_sample(n = 1)
municipality_random_selection 

# plot and store Lorenz curve for this random municipality ---------------------
municipality_random_lorenz_ggp <-
  municipality_random_selection |>
  dplyr::left_join(crime_population_nhoods_df, by = "municipality_lab") |> 
  dplyr::select(crime) |>
  dplyr::pull() |> 
  lorenzgini::lorenz(prop = FALSE) + 
    xlab("cumulatief % buurten") +
    ylab("cumulatief % misdrijven") +
    ggtitle(paste0("Gemeente ", 
                   municipality_random_selection$municipality_lab,
                   " (Gini = ",
                   round(municipality_random_selection$gini_crime_unweighted, digits = 2),
                   ")"))   +
    geom_hline(aes(yintercept = 80), linetype = "dotted") +
    geom_vline(aes(xintercept = 20), linetype = "dotted")
municipality_random_lorenz_ggp
# store the plot
ggsave_svg(ggp = municipality_random_lorenz_ggp, output = OUTPUT)


# plot and store Lorenz curve for this random municipality ---------------------
plot_lorenz <- function(municipality) {
  if(nrow(municipality) == 1) {
    municipality |>
    dplyr::left_join(crime_population_nhoods_df, by = "municipality_lab") |> 
    dplyr::select(crime) |>
    dplyr::pull() |> 
    lorenzgini::lorenz(prop = FALSE) + 
    xlab("cumulatief % buurten") +
    ylab("cumulatief % misdrijven") +
    ggtitle(paste0("Gemeente ", 
                   municipality$municipality_lab,
                   " (Gini = ",
                   round(municipality$gini_crime_unweighted, digits = 2),
                   ")"))   +
    geom_hline(aes(yintercept = 80), linetype = "dotted") +
    geom_vline(aes(xintercept = 20), linetype = "dotted")
  }
}  

gini_amsterdam_ggp <-
  municipality_gini_df |>
  dplyr::filter(municipality_lab == "Amsterdam") |>
  plot_lorenz()

gini_rotterdam_ggp <-
municipality_gini_df |>
  dplyr::filter(municipality_lab == "Rotterdam") |>
  plot_lorenz()

gini_sgravenhage_ggp <-
municipality_gini_df |>
  dplyr::filter(municipality_lab == "'s-Gravenhage") |>
  plot_lorenz()

gini_utrecht_ggp <-
municipality_gini_df |>
  dplyr::filter(municipality_lab == "Utrecht") |>
  plot_lorenz()

municipality_gini_df |>
  dplyr::filter(municipality_lab == "Eindhoven") |>
  plot_lorenz()

municipality_gini_df |>
  dplyr::filter(municipality_lab == "Almere") |>
  plot_lorenz()


# store the plot
ggsave_svg(ggp = municipality_random_lorenz_ggp, output = OUTPUT)


# merge Gini with the municipality geography -----------------------------------
municipality_gini_sf <-  
  inner_join(geometry_municipalities_sf, 
             municipality_gini_df,
             by = "municipality_id") 

# plot histograms

gini_crime_unweighted_ggp <-
  municipality_gini_df |>
  ggplot() +
  geom_histogram(aes(x = gini_crime_unweighted), 
                 fill = "lightblue", 
                 color = "black",
                 binwidth = .05)  +
  scale_x_continuous(breaks = seq(0,1,.1), limits = c(0,1)) +
  xlab("Gini coëfficiënt") +
  ylab("Aantal gemeenten")
gini_crime_unweighted_ggp 

gini_crime_pop_weighted_ggp <-
  municipality_gini_df |>
  ggplot() +
  geom_histogram(aes(x = gini_crime_pop_weighted), 
                 fill = "orange", 
                 color = "black",
                 binwidth = .05)  +
  scale_x_continuous(breaks = seq(0,1,.1), limits = c(0,1)) +
  xlab("Gini coëfficiënt") +
  ylab("Aantal gemeenten")
gini_crime_pop_weighted_ggp

# plot histograms for municipalities with at least 10 neighborhoods

gini_crime_unweighted_ggp <-
  municipality_gini_df |>
  filter(n_nhoods >= 10) |>
  ggplot() +
  geom_histogram(aes(x = gini_crime_unweighted), 
                 fill = "lightblue", 
                 color = "black",
                 binwidth = .05) +
  scale_x_continuous(breaks = seq(0,1,.1), limits = c(0,1)) +
  xlab("Gini coëfficiënt") +
  ylab("Aantal gemeenten")
gini_crime_unweighted_ggp 

gini_crime_pop_weighted_ggp <-
  municipality_gini_df |>
  filter(n_nhoods >= 10) |>
  ggplot() +
  geom_histogram(aes(x = gini_crime_pop_weighted), 
                 fill = "orange", 
                 color = "black",
                 binwidth = .05)  +
  scale_x_continuous(breaks = seq(0,1,.1), limits = c(0,1)) +
  xlab("Gini coëfficiënt") +
  ylab("Aantal gemeenten")
gini_crime_pop_weighted_ggp

municipality_gini_df |> 
  select(starts_with("gini")) |> 
  filter(gini_crime_unweighted > .20,
         gini_pop_unweighted > .20) |>
  cor()

municipality_gini_df |> 
  filter(gini_crime_unweighted > .20,
         gini_pop_unweighted > .20) |>
  ggplot() + 
  geom_point(aes(x = gini_crime_unweighted,
                 y = gini_pop_unweighted))

municipality_gini_df |>
  filter(gini_crime_unweighted < .35) 


# store histograms
ggsave_svg(ggp = gini_crime_unweighted_ggp, output = OUTPUT)
ggsave_svg(ggp = gini_crime_pop_weighted_ggp, output = OUTPUT)

# correlation between unweighted and population weighted gini
municipality_gini_df |>
  dplyr::select(gini_crime_unweighted, gini_crime_pop_weighted) |> 
  cor()


# Municipalities with lowest Gini (low inequality, weak concentration)
municipality_gini_df |>
  arrange(gini_crime_unweighted) 

# Municipalities with highest Gini (high inequality, strong concentration)
municipality_gini_df |>
  arrange(desc(gini_crime_unweighted)) 

# 20 largest cities

municipality_gini_df |> 
  arrange(desc(n_pop)) |>
  slice_head(n = 20) |>
  select(municipality_lab, gini_crime_unweighted, gini_crime_pop_weighted)



# Map Gini ---------------------------------------------------------------------

# unweighted
municipality_gini_crime_unweighted_ggp <-
  municipality_gini_sf |>
  ggplot() +
  geom_sf(aes(fill = gini_crime_unweighted )) +
  scale_fill_steps2()
municipality_gini_crime_unweighted_ggp

# population weighted
municipality_gini_crime_pop_weighted_ggp <-
  municipality_gini_sf |>
  ggplot() +
  geom_sf(aes(fill = gini_crime_pop_weighted )) +
  scale_fill_steps2()
municipality_gini_crime_pop_weighted_ggp

# store maps
ggsave_svg(ggp = municipality_gini_crime_unweighted_ggp, output = OUTPUT)
ggsave_svg(ggp = municipality_gini_crime_pop_weighted_ggp, output = OUTPUT)

# End of script ----------------------------------------------------------------
sessionInfo()



