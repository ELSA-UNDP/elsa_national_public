#!/usr/bin/env Rscript
purrr::walk(
  list.files(
    path = "R",
    pattern = "R",
    full.names = TRUE
  ),
  source,
  echo = FALSE
)
`%nin%` <- Negate(`%in%`)

terra::terraOptions(
  tempdir = here::here(""),
  steps = 4,
  todisk = TRUE
)

terra::tmpFiles(remove = TRUE) # Clean existing temp layers

# Pre_global.R settings ####
# Local Data Only
iso3 <- "ECU" # iso3 code of the country. Needs to match country
country <- "Ecuador"
language <- "en" # es = spanish, en = english
blm <- 0 # boundary length modifier (makes solution more clumped but will increase run time)
palock <- TRUE # lock-in existing protected areas
restorelock <- FALSE # lock-in existing restoration projects
weight_cal <- FALSE # perform weight calibration for prioritization (will increase the run time of this script)

# Load Data Sheet ####
ELSA_df <- readxl::read_xlsx("Ecuador_Master_ELSA_Database.xlsx", sheet = "ELSA_data_stack") %>%
  janitor::clean_names()

# Load Translation File ####
ELSA_text <- readxl::read_xlsx("Translation matrix of the ELSA webtool.xlsx", sheet = "tool_master")

readr::write_rds(ELSA_text, here::here(".", "elsa_text.rds"), compress = "gz")

# Prepare budgets for the three zones (protect, restore, manage) ####
protect_budget <-
  ELSA_df[ELSA_df$groups == "Budgets", "protect"] %>%
  tidyr::drop_na() %>%
  dplyr::pull() %>%
  as.numeric() %>%
  plyr::round_any(1e-4, ceiling)
manage_budget <-
  ELSA_df[ELSA_df$groups == "Budgets", "manage"] %>%
  tidyr::drop_na() %>%
  dplyr::pull() %>%
  as.numeric() %>%
  plyr::round_any(1e-4, ceiling)
restore_budget <-
  ELSA_df[ELSA_df$groups == "Budgets", "restore"] %>%
  tidyr::drop_na() %>%
  dplyr::pull() %>%
  as.numeric() %>%
  plyr::round_any(1e-4, ceiling)

# Prepare feature layer information ####
feat_df <- ELSA_df %>%
  dplyr::slice(1:match("Zones", ELSA_df$groups)) %>%
  dplyr::filter(groups == "Features" & !is.na(protect)) %>%
  dplyr::mutate(
    feat_name = file_name,
    protect = as.numeric(protect),
    manage = as.numeric(manage),
    restore = as.numeric(restore),
    label = case_when(
      language != "en" ~ label_name_translated,
      language == "en" ~ label_name
    ),
    theme = case_when(
      language != "en" ~ label_theme_translated,
      language == "en" ~ label_theme
    ),
    policy_num = ifelse(
      is.na(secondary_policy),
      primary_policy,
      glue::glue("{primary_policy},{secondary_policy}")
    )
  ) %>%
  dplyr::select(
    groups,
    label,
    theme,
    feature_order,
    protect,
    manage,
    restore,
    weight_stakeholder,
    weight_calibration,
    weight_final,
    policy_num,
    policy_short,
    policy_long,
    descriptions = layer_description,
    citation,
    citation_short,
    feat_name
  )

# Prepare zones information ####
zones_df <- ELSA_df %>%
  dplyr::filter(groups == "Zones-restrictions") %>%
  dplyr::select(
    name = if_else(language != "en", "label_name_translated", "label_name"),
    short = policy_short,
    description = layer_description,
    file_name
  )

# Prepare lock-in data information ####
lockin_df <- ELSA_df %>%
  dplyr::filter(ELSA_df$groups == "Lock-in") %>%
  dplyr::select(
    name = if_else(language != "en", "label_name_translated", "label_name"),
    short = policy_short,
    description = layer_description,
    file_name
  )

rm(ELSA_df)

# Prepare data layers ####
# Features
feat_stack <- terra::rast(here::here("data/elsa_inputs", feat_df$feat_name))
# Protected areas
PA <- terra::rast(here::here("data/elsa_inputs", lockin_df$file_name[1]))
# Restoration projects
if (restorelock) {
  Rest <- terra::rast(here::here("data/elsa_inputs/", lockin_df$file_name[2]))
}

# Zones ####
prot_zone <-
  terra::rast(here::here("data/elsa_inputs", zones_df$file_name[1]))
man_zone <-
  terra::rast(here::here("data/elsa_inputs", zones_df$file_name[2]))
rest_zone <-
  terra::rast(here::here("data/elsa_inputs", zones_df$file_name[3]))

# Locked-in constraints ####
# Protected areas (Areas that are in PAs should be 1, everything else NA)
PA <- PA > 0.5 # filter for all areas that are in a PA
PAN <- PA
PAN[PAN == 1] <- NA
PA0 <- PA # create a layer for all areas outside the protected areas
PA0[PA0] <- NA
PA[PA == 0] <- NA

# Restoration areas
if (restorelock) {
  Rest <- Rest > 0
  RestN <- Rest
  RestN[RestN == 1] <- NA
  Rest0 <- Rest
  Rest0[Rest0] <- NA
  Rest[Rest == 0] <- NA
  Rest[PA > 0] <- NA
}

# Planning unit information ####
pu0 <- prot_zone >= 0 # could have used any zone because all zones have 1 where PUs are available for this zone and 0 where not, so would cover all PUs
pu <- pu0
pu[is.na(prot_zone)] <- NA

# Create zones used in prioritization ####
# Protect zone
zone_protect <- prot_zone
zone_protect[zone_protect < 1] <- NA
zone_protect_PA <- ifel(isTRUE(not.na(zone_protect) | not.na(PA)), 1, NA) # make PUs of existing PAs available to select in this zone
if (restorelock) { # if existing restoration projects are locked-in, these need to be excluded from protection zone, otherwise might try to lock those PUs in twice
  zone_protect_Rest <- zone_protect
  zone_protect_Rest[Rest > 0] <- NA
}
if (restorelock & palock) {
  zone_protect_PARest <- zone_protect_Rest
}

# Restore zone
zone_restore <- rest_zone
zone_restore[zone_restore < 1] <- NA
zone_restore_PA <- zone_restore
zone_restore_PA[PA == 1] <- NA # don't allow PUs of existing PAs to be selected in this zone
if (restorelock) {
  zone_restore_Rest <- ifel(isTRUE(not.na(zone_restore) | not.na(Rest)), 1, NA)
}
if (restorelock & palock) {
  zone_restore_PARest <- zone_restore_Rest
  zone_restore_PARest[PA > 0] <- NA
}

# Manage zone
zone_manage <- man_zone
zone_manage[zone_manage < 1] <- NA
zone_manage_PA <- zone_manage
zone_manage_PA[PA == 1] <- NA # don't allow PUs of existing PAs to be selected in this zone
if (restorelock) {
  zone_manage_Rest <- zone_manage
  zone_manage_Rest[Rest > 0] <- NA
}
if (restorelock & palock) {
  zone_manage_PARest <- zone_manage_Rest
  zone_manage_PARest[not.na(PA > 0) | not.na(Rest > 0)] <- NA
}

# Create raster stack of zones layers depending on which areas are locked-in ####
# Nothing locked in
pu1 <- c(zone_protect, zone_restore, zone_manage)

# PAs locke in
pu1_pa <- c(zone_protect_PA, zone_restore_PA, zone_manage_PA)
names(pu1) <- names(pu1_pa) <- c("protect", "restore", "manage")

# Retsoration projects locked in
if (restorelock) {
  pu1_rest <-
    c(zone_protect_Rest, zone_restore_Rest, zone_manage_Rest)
  names(pu1_rest) <- c("protect", "restore", "manage")
}

# PAs and rstoration projects locked in
if (restorelock & palock) {
  pu1_parest <-
    c(
      zone_protect_PARest,
      zone_restore_PARest,
      zone_manage_PARest
    )
  names(pu1_parest) <- c("protect", "restore", "manage")
}

# Create different scenario options that can be selected in the app ####
# locked = lock in PAs, avail = nothing locked-in, restore = only restoration projects locked-in, pa_restore = lock in PAs and restoration projects
if (restorelock & palock) {
  pu_all <- list(area = list(
    locked = pu1_pa,
    avail = pu1,
    restore = pu1_rest,
    pa_restore = pu1_parest
  ))
} else {
  if (!restorelock) {
    pu_all <- list(area = list(
      locked = pu1_pa,
      avail = pu1
    ))
  } else {
    pu_all <- list(area = list(
      locked = pu1_pa,
      avail = pu1
    ))
  }
}

# Impacts setup ####
# Depending of the zone, the feature values are multiplied by an impact score
# This influences whether they are later selected for specific zones in the prioritization
# e.g. Biodiversity features receives a higher impact score in the protect zone than manage zone
protect_impacts <- feat_df$protect
restore_impacts <- feat_df$restore
manage_impacts <- feat_df$manage

impacts <- data.frame(
  Name = feat_df$label,
  Theme = feat_df$theme,
  feature = names(feat_stack),
  Protect = as.numeric(protect_impacts),
  Restore = as.numeric(restore_impacts),
  Manage = as.numeric(manage_impacts)
)

# Prepare feature stack that will be used to define the zones environment for prioritizr ####
zn1 <- feat_stack * impacts[, "Protect"]
zn2 <- feat_stack * impacts[, "Restore"]
zn3 <- feat_stack * impacts[, "Manage"]

# Also create raw feat_stack without impact that will be used for representation calculations
feat_stack_raw <- feat_stack

# Create prioritizr zones environment ####
zns <- prioritizr::zones(
  "Protect" = zn1,
  "Restore" = zn2,
  "Manage" = zn3,
  feature_names = names(zn1)
)

################################################################################
# Weight calibration #####
# The maximum utility objective function does not have targets for features, but
# features can be given weights based on their perceived importance.
# In the ELSA pipeline, weights are determined in stakeholder workshops, but initial
# weights are calculated based on the analysis below.
################################################################################
if (weight_cal) {
  # Planning unit information with cost information per zone (area based, so 1 where selection is possible for a zone, 0 where selection isnt possible)
  pu_temp <- pu_all[["area"]][["locked"]] 

  # Initial prioritizr problem formulation
  prob.ta <- prioritizr::problem(pu_temp, zns) %>%
    prioritizr::add_max_utility_objective(c(
      count_tar(pu0, protect_budget),
      count_tar(pu0, restore_budget),
      count_tar(pu0, manage_budget)
    )) %>%
    prioritizr::add_gurobi_solver(gap = 0.05, threads = 8) %>% # 16 Available on Gurobi Cloud
    prioritizr::add_locked_in_constraints(c(PA, PA0, PA0))

  # Solve conservatioon problem
  s.ta <- solve(prob.ta, force = TRUE)
  
  # Get feature representation
  freq <-
    prioritizr::eval_feature_representation_summary(prob.ta, s.ta)

  
  rep <- rep0 <- tidyr::pivot_wider(freq,
                                    id_cols = feature,
                                    names_from = summary,
                                    values_from = relative_held) |>
    dplyr::select(-c(overall))

  rep[, -1] <- 0


  wgt <-
    as.matrix(matrix(
      rep(0, 3),
      ncol = 3,
      nrow = terra::nlyr(feat_stack)
    ))


  for (ii in 1:terra::nlyr(feat_stack)) {
    wgt2 <- wgt
    
    wgt2[ii, ] <- 1
    
    prob.all <- prob.ta |>
      prioritizr::add_feature_weights(wgt2)

    result <- solve(prob.all, force = TRUE)

    feat_rep <-
      prioritizr::eval_feature_representation_summary(prob.all, result)

    tar <- tidyr::pivot_wider(
      feat_rep,
      id_cols = feature,
      names_from = summary,
      values_from = relative_held
    ) |>
      dplyr::select(-c(overall))
    
    rep[ii, ] <- tar[ii, ]
    
    rm(prob.all, result, feat_rep, tar)
    gc()
  }

  rep[is.na(rep)] <- 0
  
  gc()

  # all groups

  wgt <-
    as.matrix(matrix(
      rep(1, 3),
      ncol = 3,
      nrow = terra::nlyr(feat_stack)
    ))


  prob.all <- prob.ta %>%
    prioritizr::add_feature_weights(wgt)

  result <- solve(prob.all, force = TRUE)

  feat_rep <-
    prioritizr::eval_feature_representation_summary(prob.all, result)

  tar <- tidyr::pivot_wider(
    feat_rep,
    id_cols = feature,
    names_from = summary,
    values_from = relative_held
  ) |>
    dplyr::select(-c(overall))

  tar[is.na(tar)] <- 0

  dd <- tibble::tibble(
    feature = rep$feature,
    max_representation = rowSums(rep[, -1], na.rm = T),
    max_utility = rowSums(tar[, -1], na.rm = T)
  ) |>
    dplyr::mutate(
      delta_mu = max_utility - max_representation,
      delta_mu_perc = (max_utility - max_representation) / max_representation * 100
    )

  summary(dd$delta_mu_perc)

  # calculate addition
  calib <- TRUE
  wgt_scale <- 2
  it <- 1
  wgta <- wgt

  while (calib) {
    adj <-
      wgt_scale - (dd$delta_mu_perc - min(dd$delta_mu_perc)) / (max(dd$delta_mu_perc) - min(dd$delta_mu_perc)) * wgt_scale
    wgtb <- wgta + adj
    
    print(it)
    flush.console()
    
    p1 <- prob.ta |>
      prioritizr::add_feature_weights(wgtb)

    s1 <- solve(p1, force = TRUE)


    f1 <- prioritizr::eval_feature_representation_summary(p1, s1)

    t1 <- tidyr::pivot_wider(
      f1,
      id_cols = feature,
      names_from = summary,
      values_from = relative_held
    ) %>%
      dplyr::select(-c(overall))

    t1[is.na(t1)] <- 0

    dd1 <- tibble::tibble(
      feature = rep$feature,
      max_representation = rowSums(rep[, -1], na.rm = T),
      max_utility = rowSums(t1[, -1], na.rm = T)
    ) |>
      mutate(
        delta_mu = max_utility - max_representation,
        delta_mu_perc = (max_utility - max_representation) / max_representation * 100
      )

    dd1
    
    summary(dd1$delta_mu_perc)

    tt <- tibble::tibble(
      old = dd$delta_mu_perc,
      new = dd1$delta_mu_perc,
      delta = new - old
    )

    delta1 <- max(dd1$delta_mu_perc) - min(dd1$delta_mu_perc)
    delta0 <- max(dd$delta_mu_perc) - min(dd$delta_mu_perc)

    if (delta1 < delta0) {
      wgta <- wgtb
      dd <- dd1
      it <- it + 1
    } else {
      calib <- FALSE
    }
  }

  rm(
    pu_temp,
    prob.ta,
    result,
    freq,
    rep,
    prob.all,
    result,
    feat_rep,
    tar,
    p1,
    s1,
    f1,
    t1,
    dd1,
    ELSA_text
  )

  wgta[, 1] %>% readr::write_rds(glue::glue("wgta_{tolower(iso3)}.rds"), compress = "gz")
} else {
  wgta <- as.numeric(feat_df$weight_calibration)
}

wgta <- as.numeric(feat_df$weight_calibration)

wgts <- tibble::tibble(
  name = feat_df$label,
  theme = feat_df$theme,
  feature = names(feat_stack),
  weight = ifelse(is.na(feat_df$weight_final),
                  5,
                  as.numeric(feat_df$weight_final)),
  policy = feat_df$policy_num
) # Policy Targets

################################################################################
# END Weight calibration
################################################################################

# Process feature theme information ####
#(specific category for a feature, either Biodiversity, Climate Mitigation or Human Well-being)
themes <- unique(feat_df$theme)
theme_names <- list()
theme_layers <- list()

for (ii in 1:length(themes)) {
  theme_names[[ii]] <-
    names(feat_stack)[grep(themes[ii], feat_df$theme, ignore.case = T)]
  theme_layers[[ii]] <- feat_stack[[theme_names[[ii]]]]
}

theme_tbl <- tibble(theme = themes,
                    names = theme_names,
                    layers = theme_layers)

gc()

# Get minimum budget values per each lock-in scenario
default_protect_min_budget <- round(get_coverage(PA, pu), 2)
if (restorelock) {
  default_restore_min_budget <- round(get_coverage(Rest, pu), 2)
} else {
  default_restore_min_budget <- 0
}

# Prepare data that will be used as globals in the app (wrap with terra to later unwrap in global.R) ####
PA <- terra::wrap(PA)
if (restorelock) {
  Rest <- terra::wrap(Rest)
}
PAN <- terra::wrap(PAN)
PA0 <- terra::wrap(PA0)
pu1 <- terra::wrap(pu1)
pu1_pa <- terra::wrap(pu1_pa)
if (restorelock) {
  pu1_rest <- terra::wrap(pu1_rest)
}
if (restorelock & palock) {
  pu1_parest <- terra::wrap(pu1_parest)
}
pu0 <- terra::wrap(pu0)
pu <- terra::wrap(pu)

zn1 <- terra::wrap(zn1)
zn2 <- terra::wrap(zn2)
zn3 <- terra::wrap(zn3)
feat_stack_raw <- terra::wrap(feat_stack_raw)
save.image(here::here("pre_global.RData"), compress = "gzip")

gc()
# terra::tmpFiles(remove = TRUE)
