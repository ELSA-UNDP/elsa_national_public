# R script to prepare global variables called in the ELSA tool ####

# load packages and prepared data ####
source("R/packages.R")
load("pre_global.RData")

# prepare text for ELSA tool ####
ELSA_text <- readr::read_rds(here::here(".", "elsa_text.rds"))

# set global parameters ####
language <- language
country <- country # "Ecuador"
restorelock <- restorelock

# custom function ####
`%nin%` <- Negate(`%in%`)

if (restorelock & palock) {
  prot_lst <- list("locked", "avail", "restore", "pa_restore")

  names(prot_lst) <- c(
    ELSA_text %>% filter(var == "prot_txt") %>% pull(language),
    ELSA_text %>% filter(var == "nolock_txt") %>% pull(language),
    ELSA_text %>% filter(var == "restlock_txt") %>% pull(language),
    ELSA_text %>% filter(var == "prot_rest_txt") %>% pull(language)
  )
} else {
  if (restorelock) {
    prot_lst <- list("locked", "avail", "restore")

    names(prot_lst) <- c(
      ELSA_text %>% filter(var == "prot_txt") %>% pull(language),
      ELSA_text %>% filter(var == "nolock_txt") %>% pull(language),
      ELSA_text %>% filter(var == "restlock_txt") %>% pull(language)
    )
  } else {
    prot_lst <- list("locked", "avail")

    names(prot_lst) <- c(
      ELSA_text %>% filter(var == "prot_txt") %>% pull(language),
      ELSA_text %>% filter(var == "nolock_txt") %>% pull(language)
    )
  }
}

area_lst <- list("area")

names(area_lst) <- ELSA_text %>%
  dplyr::filter(var == "area") %>%
  dplyr::pull(language)

# Colour Palettes ####
pal.elsa <- tibble(
  colour = c("#4daf4a", "#984ea3", "#377eb8"),
  category = c("Protect", "Restore", "Manage")
)
pal.hm <- c("#440154", "#3B528B", "#21908C", "#5DC863", "#FDE725")
pal.in <- c("#0D0887", "#7E03A8", "#CC4678", "#F89441", "#F0F921")
pal.zone <- "#6afdfa"

# Load spatial data layers ####
# Data layers ####
feat_stack <-
  terra::rast(here::here("data/elsa_inputs", feat_df$feat_name))
# Zones ####
prot_zone <-
  terra::rast(here::here("data/elsa_inputs", zones_df$file_name[1]))
man_zone <-
  terra::rast(here::here("data/elsa_inputs", zones_df$file_name[2]))
rest_zone <-
  terra::rast(here::here("data/elsa_inputs", zones_df$file_name[3]))

zn1 <- terra::rast(zn1)
zn2 <- terra::rast(zn2)
zn3 <- terra::rast(zn3)

# Create Zone file ####
zns <- prioritizr::zones(
  "Protect" = zn1,
  "Restore" = zn2,
  "Manage" = zn3,
  feature_names = names(zn1)
)

# Create data frame for representation calculations ####
feat_stack_raw <- terra::rast(feat_stack_raw)
# Calculate the sum of each layer
layer_sums <- terra::global(feat_stack_raw, sum, na.rm = TRUE)
# Extract the names of each layer
layer_names <- terra::names(feat_stack_raw)

# Create a data frame with layer names and their corresponding sums
overall_raw_df <- data.frame(
  feature = layer_names,
  total_amount = layer_sums[, 1] # First column of the global output
)

rm(layer_sums, layer_names)

# Unwrap data additional data ####
PA <- terra::rast(PA)
if (restorelock) {
  Rest <- terra::rast(Rest)
}
PAN <- terra::rast(PAN)
PA0 <- terra::rast(PA0)
pu1 <- terra::rast(pu1)
pu1_pa <- terra::rast(pu1_pa)
if (restorelock) {
  pu1_rest <- terra::rast(pu1_rest)
}
if (restorelock & palock) {
  pu1_parest <- terra::rast(pu1_parest)
}
pu0 <- terra::rast(pu0)
pu <- terra::rast(pu)

if (restorelock & palock) {
  pu_all <- list(area = list(
    locked = pu1_pa,
    avail = pu1,
    restore = pu1_rest,
    pa_restore = pu1_parest
  ))
} else {
  if (!restorelock) {
    pu_all <- list(area = list(locked = pu1_pa, avail = pu1))
  } else {
    pu_all <- list(area = list(locked = pu1_pa, avail = pu1))
  }
}

# prepare themes information (Biodiversity, climate change mitigation, human well-being) ####
themes <- unique(feat_df$theme)
theme_names <- list()
theme_layers <- list()

for (ii in 1:length(themes)) {
  theme_names[[ii]] <-
    names(feat_stack)[grep(themes[ii], feat_df$theme, ignore.case = T)]
  theme_layers[[ii]] <- feat_stack[[theme_names[[ii]]]]
}

theme_tbl <- tibble(
  theme = themes,
  names = theme_names,
  layers = theme_layers
)
