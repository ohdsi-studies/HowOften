remotes::install_github(repo = "OHDSI/PhenotypeLibrary", ref = "v3.24.0")


# Step 1: get all cohort definition in OHDSI PhenotypeLibrary ----
fullPhenotypeLog <- PhenotypeLibrary::getPhenotypeLog() |>
  dplyr::filter(stringr::str_detect(
    string = cohortNameAtlas,
    pattern = stringr::fixed("[D]"),
    negate = TRUE
  ))

# any overides
cohortsThatShouldBeRemovedBecauseTheySeemToCauseProblems <- c(23, 344)
cohortsThatAreInteresting <- c(30, 33, 43, 61, 142, 235, 236, 240, 248, 251, 253, 329, 334, 372, 373, 375,
                               383, 389, 394, 395, 401, 404, 405, 411,
                               702, 703, 74, 705, 706, 710, 712, 715, 716, 717)
cohortsThatAreDuplicates <- c(1015, 747, 771, 772, 993, 997)
cohortsThatWontAddValue <- c()

fullPhenotypeLog <- fullPhenotypeLog |> 
  dplyr::filter(!cohortId %in% c(cohortsThatShouldBeRemovedBecauseTheySeemToCauseProblems,
                                 cohortsThatAreDuplicates,
                                 cohortsThatWontAddValue))

# Note: HowOften has three types of analysis
## Analysis 1: Use all cohorts in PL that met some criteria as outcome, and use a baseCohort as Target
## Analysis 2: Community proposals
## Analysis 3: Compare incidence of certain outcomes after exposure to drug with and without subset to drugs indications

# Step 2: Identify and flag the cohortIds of the cohorts we want to use in HowOften ----

subsetOfCohorts <- c()
## analysis 1 base cohort. The cohort is 1071
subsetOfCohorts$baseCohort <- fullPhenotypeLog |>
  dplyr::filter(cohortId %in% c(1071)) |>
  dplyr::select(cohortId) |>
  dplyr::mutate(reasonBaseCohort = 1)

## all cohorts that have been accepted to OHDSI PhenotypeLibrary after some review process
subsetOfCohorts$acceptedCohorts <- fullPhenotypeLog |>
  dplyr::filter(stringr::str_length(string = addedVersion) > 0) |>
  dplyr::select(cohortId) |>
  dplyr::mutate(reasonAcceptToOhdsiPl = 1)

## Designated Medical Events
subsetOfCohorts$foundInLibraryOutcomeDme <- fullPhenotypeLog |>
  dplyr::filter(stringr::str_detect(string = toupper(hashTag), pattern = "#DME")) |>
  dplyr::select(cohortId) |>
  dplyr::mutate(reasonDme = 1)

## AESI cohorts built for covide anlaysis. These are mostly imported from the covid aesi studies
subsetOfCohorts$foundInLibraryOutcomeAesi <- fullPhenotypeLog |>
  dplyr::filter(stringr::str_detect(string = toupper(hashTag), pattern = "#AESI")) |>
  dplyr::select(cohortId) |>
  dplyr::mutate(reasonAesi = 1)

# LEGEND studies are imported from Legend Hypertension and LEGEND Diabetes. Unfortunately the cohorts may have duplicated.
subsetOfCohorts$foundInLibraryOutcomeLegend <- fullPhenotypeLog |>
  dplyr::filter(stringr::str_detect(string = toupper(hashTag), pattern = "#LEGEND")) |>
  dplyr::select(cohortId) |>
  dplyr::mutate(reasonLegend = 1)

# All service utilization cohorts.
subsetOfCohorts$foundInVisit <- fullPhenotypeLog |>
  dplyr::filter(stringr::str_detect(string = toupper(hashTag), pattern = "#VISIT")) |>
  dplyr::select(cohortId) |>
  dplyr::mutate(reasonVisit = 1) # debatable

# All cohorts flagged as symptoms.
subsetOfCohorts$foundInSymptoms <- fullPhenotypeLog |>
  dplyr::filter(stringr::str_detect(string = toupper(hashTag), pattern = "#SYMPTOMS")) |>
  dplyr::select(cohortId) |>
  dplyr::mutate(reasonSymptoms = 1)

# All cohorts that were submitted to the OHDSI PhenotypeLibrary on or after August 1st 2023
subsetOfCohorts$recentSubmission <- fullPhenotypeLog |>
  dplyr::filter(createdDate > as.Date('2023-08-01')) |>
  dplyr::select(cohortId) |>
  dplyr::mutate(reasonRecentlyPosted = 1)

# These are hand picked cohorts that were used by Patrick to subset drug exposure cohorts
subsetOfCohorts$libraryIndicationCohorts <- fullPhenotypeLog |>
  dplyr::filter(cohortId %in% c(770,
                                765,
                                71,
                                1032,
                                32,
                                749,
                                861,
                                19,
                                858,
                                860,
                                859,
                                748)) |>
  dplyr::select(cohortId) |>
  dplyr::mutate(reasonAnalysis3Indication = 1)

# These are cohorts that Patrick built for use in Analysis 3
subsetOfCohorts$howOften <- fullPhenotypeLog |>
  dplyr::filter(stringr::str_detect(string = toupper(hashTag), pattern = "#HOWOFTEN")) |>
  dplyr::select(cohortId) |>
  dplyr::mutate(reasonHowOftenAnalysis3 = 1)

# These are cohorts that Patrick built for use in Analysis 3
subsetOfCohorts$jillHardinCohorts <- fullPhenotypeLog |>
  dplyr::filter(cohortId %in% c(134,
                                470,
                                667,
                                690,
                                533,
                                521,
                                591,
                                466)) |>
  dplyr::select(cohortId) |>
  dplyr::mutate(reasonJillHardin = 1)

subsetOfCohorts$thatAreInteresting <- fullPhenotypeLog |> 
  dplyr::filter(cohortId %in% c(cohortsThatAreInteresting)) |>
  dplyr::select(cohortId) |>
  dplyr::mutate(reasonIsInteresting = 1)
## combine

allCohorts <- dplyr::bind_rows(subsetOfCohorts) |>
  dplyr::select(cohortId) |>
  dplyr::distinct() |>
  dplyr::left_join(subsetOfCohorts$baseCohort) |>
  dplyr::left_join(subsetOfCohorts$acceptedCohorts) |>
  dplyr::left_join(subsetOfCohorts$foundInLibraryOutcomeDme) |>
  dplyr::left_join(subsetOfCohorts$foundInLibraryOutcomeAesi) |>
  dplyr::left_join(subsetOfCohorts$foundInLibraryOutcomeLegend) |>
  dplyr::left_join(subsetOfCohorts$recentSubmission) |>
  dplyr::left_join(subsetOfCohorts$libraryIndicationCohorts) |>
  dplyr::left_join(subsetOfCohorts$howOften) |>
  dplyr::left_join(subsetOfCohorts$foundInVisit) |> 
  dplyr::left_join(subsetOfCohorts$foundInSymptoms) |> 
  dplyr::left_join(subsetOfCohorts$jillHardinCohorts) |> 
  dplyr::left_join(subsetOfCohorts$thatAreInteresting) |> 
  tidyr::replace_na(
    replace = list(
      reasonBaseCohort = 0,
      reasonAcceptToOhdsiPl = 0,
      reasonDme = 0,
      reasonAesi = 0,
      reasonLegend = 0,
      reasonRecentlyPosted = 0,
      reasonAnalysis3Indication = 0,
      reasonHowOftenAnalysis3 = 0,
      reasonVisit = 0,
      reasonSymptoms = 0,
      reasonJillHardin = 0,
      reasonIsInteresting = 0
    )
  ) |> 
  dplyr::inner_join(
    fullPhenotypeLog |> 
      dplyr::select(cohortId,
                    cohortName,
                    eventCohort,
                    exitDateOffSet,
                    exitPersistenceWindow,
                    collapseEraPad)
  ) |> 
  dplyr::relocate(cohortId,
                  cohortName,
                  eventCohort,
                  exitDateOffSet,
                  exitPersistenceWindow,
                  collapseEraPad)


# Step 3: Assign clean window ----
## All cohorts get a default clean window ----
allCohorts <- allCohorts |>
  dplyr::mutate(cleanWindow = 0,
                cleanWindowAssigned = 0,
                cleanWindowRule = "") 


## Only event cohorts need custom clean window. ----
## So we will assume that cleanWindowAssigned = 1 for non event cohorts
allCohorts <- allCohorts|>
  dplyr::mutate(
    cleanWindowAssigned = dplyr::if_else(
      condition = !as.logical(eventCohort),
      true = 1,
      false = cleanWindowAssigned
    ),
    cleanWindowRule = dplyr::if_else(
      condition = !as.logical(eventCohort),
      true = "Not an event cohort",
      false = cleanWindowRule
    )
  )
allCohorts |>
  dplyr::group_by(cleanWindowRule, cleanWindowAssigned, eventCohort) |>
  dplyr::summarise(n = dplyr::n())


### We are using a combination of rule and heuristic to assign clean window.

### Rule based: use collapseEraPad  ----
### explore the distribution of collapseEraPad among event cohorts
allCohorts |>
  dplyr::filter(eventCohort == 1) |>
  dplyr::inner_join(fullPhenotypeLog) |>
  dplyr::group_by(collapseEraPad) |>
  dplyr::select(collapseEraPad) |>
  dplyr::summarise(n = dplyr::n()) |>
  dplyr::arrange(collapseEraPad)

# Choice: If collapseEraPad >= 7, then we will use collapseEraPad
allCohorts <- allCohorts |>
  dplyr::mutate(
    cleanWindow = dplyr::if_else(
      condition = (cleanWindowAssigned == 0) & (collapseEraPad > 7),
      true = collapseEraPad,
      false = cleanWindow
    ),
    cleanWindowRule = dplyr::if_else(
      condition = (eventCohort == 1) &
        (cleanWindowAssigned == 0) & (collapseEraPad > 7),
      true = "Has collapse era pad of greater than 7 in definition",
      false = cleanWindowRule
    ),
    cleanWindowAssigned = dplyr::if_else(
      condition = (cleanWindowAssigned == 0) & (collapseEraPad > 7),
      true = 1,
      false = cleanWindowAssigned
    )
  )
allCohorts |>
  dplyr::group_by(cleanWindowRule, cleanWindowAssigned, eventCohort) |>
  dplyr::summarise(n = dplyr::n())

# Choice: If exitDateOffset >= 7 then the cohort exit was probably thoughtfully constructed
allCohorts |>
  dplyr::filter(eventCohort == 1) |>
  dplyr::filter(cleanWindowAssigned == 0) |> 
  dplyr::group_by(exitDateOffSet) |>
  dplyr::select(exitDateOffSet) |>
  dplyr::summarise(n = dplyr::n()) |>
  dplyr::arrange(exitDateOffSet)

allCohorts <- allCohorts |>
  dplyr::mutate(
    cleanWindow = dplyr::if_else(
      condition = (cleanWindowAssigned == 0) & (exitDateOffSet >= 7) & (is.na(exitPersistenceWindow)),
      true = 0, # if exit date has been assigned in cohort definition ,then there is no need for clean window
      false = cleanWindow
    ),
    cleanWindowRule = dplyr::if_else(
      condition = (cleanWindowAssigned == 0) & (exitDateOffSet >= 7) & (is.na(exitPersistenceWindow)),
      true = "Has an exit date strategy in the definition",
      false = cleanWindowRule
    ),
    cleanWindowAssigned = dplyr::if_else(
      condition = (cleanWindowAssigned == 0) & (exitDateOffSet >= 7) & (is.na(exitPersistenceWindow)),
      true = 1,
      false = cleanWindowAssigned
    )
  )
allCohorts |>
  dplyr::group_by(cleanWindowRule, cleanWindowAssigned, eventCohort) |>
  dplyr::summarise(n = dplyr::n())


ids <- allCohorts |> 
  dplyr::arrange(cohortId) |> 
  dplyr::filter(cleanWindowAssigned == 0) |> 
  dplyr::pull(cohortId) |> 
  sort()

### Heuristic based: assign clean window  ----
### (Decided by Azza and Gowtham)
cleanWindow <- c()
cleanWindow$cleanWindow9999 <- c(466, 470, 521, 591, 667, 999, 1071)
cleanWindow$cleanWindow365 <- c(63, 74, 215, 216, 276, 277, 412, 691, 692, 693, 694, 729, 732, 738, 742, 785, 1075, 1080, 1081, 1082, 1083, 1084, 1085, 1086, 1087, 1088, 1089, 1090, 1091)
cleanWindow$cleanWindow183 <- c(1078, 1079)
cleanWindow$cleanWindow180 <- c(218, 727, 737)
cleanWindow$cleanWindow30 <- c(362, 533, 690, 726, 783, 784, 794, 938, 939, 979, 980, 1076, 1077)
cleanWindow$cleanWindow1 <- c(24, 257, 325, 346, 347, 707)

cleanWindowToAssign <-
  dplyr::bind_rows(
    dplyr::tibble(cohortId = cleanWindow$cleanWindow9999,
                  cleanWindow = 9999),
    dplyr::tibble(cohortId = cleanWindow$cleanWindow365,
                  cleanWindow = 365),
    dplyr::tibble(cohortId = cleanWindow$cleanWindow183,
                  cleanWindow = 183),
    dplyr::tibble(cohortId = cleanWindow$cleanWindow180,
                  cleanWindow = 180),
    dplyr::tibble(cohortId = cleanWindow$cleanWindow90,
                  cleanWindow = 90),
    dplyr::tibble(cohortId = cleanWindow$cleanWindow30,
                  cleanWindow = 30),
    dplyr::tibble(cohortId = cleanWindow$cleanWindow14,
                  cleanWindow = 14),
    dplyr::tibble(cohortId = cleanWindow$cleanWindow1,
                  cleanWindow = 1)
  ) |>
  dplyr::group_by(cohortId) |>
  dplyr::summarise(cleanWindow = max(cleanWindow)) |>
  dplyr::ungroup() |> 
  dplyr::mutate(cleanWindowAssigned = 1,
                cleanWindowRule = "Manual")

allCohorts <-
  dplyr::bind_rows(
    allCohorts |>
      dplyr::filter(!cohortId %in% c(cleanWindowToAssign$cohortId)),
    allCohorts |>
      dplyr::select(-cleanWindow, -cleanWindowAssigned, -cleanWindowRule) |>
      dplyr::inner_join(cleanWindowToAssign)
  )


allCohorts |>
  dplyr::group_by(cleanWindowRule, cleanWindowAssigned, eventCohort) |>
  dplyr::summarise(n = dplyr::n())


# Step 4: Create the input format that Chris asked for ----
## Analysis 1 ----
targets <- allCohorts |>
  dplyr::filter(cohortId %in% c(
    allCohorts |>
      dplyr::filter(reasonBaseCohort == 1) |>
      dplyr::pull(cohortId)
  )) |>
  dplyr::inner_join(fullPhenotypeLog) |> 
  dplyr::select(cohortId,
                cohortName) |> 
  dplyr::arrange(cohortId) |> 
  dplyr::rename(cohortDefinitionId = cohortId,
                cohortDefinitionName = cohortName) |> 
  SqlRender::camelCaseToSnakeCaseNames()

outcomes <- allCohorts |>
  dplyr::filter(cohortId %in% c(
    allCohorts |>
      dplyr::filter(reasonBaseCohort == 0) |>
      dplyr::pull(cohortId)
  )) |>
  dplyr::inner_join(fullPhenotypeLog)  |> 
  dplyr::select(cohortId,
                cohortName,
                cleanWindow) |> 
  dplyr::arrange(cohortId) |> 
  dplyr::rename(cohortDefinitionId = cohortId,
                cohortDefinitionName = cohortName) |> 
  SqlRender::camelCaseToSnakeCaseNames()

writexl::write_xlsx(list(targets = targets, outcomes = outcomes), "analysis_specifications/analysis1.xlsx")

## Analysis 2 ----
analysis2InputSpecifications <- readxl::read_excel("analysis_specifications/analysis2InputSpecifications.xlsx")

analysis2CombinationsUnique <- analysis2InputSpecifications |> 
  dplyr::select(from, 
                group) |> 
  dplyr::distinct() |> 
  dplyr::arrange(from, 
                 group) |> 
  dplyr::group_by(from) |> 
  dplyr::mutate(id = dplyr::row_number()) |> 
  dplyr::ungroup()

for (i in (1:nrow(analysis2CombinationsUnique))) {
  combi <- analysis2CombinationsUnique[i,]
  targets <- allCohorts |> 
    dplyr::filter(
      cohortId %in% c(analysis2InputSpecifications |> 
                        dplyr::inner_join(combi) |> 
                        dplyr::pull(tId))
    ) |>
    dplyr::inner_join(fullPhenotypeLog) |> 
    dplyr::select(cohortId,
                  cohortName) |> 
    dplyr::arrange(cohortId) |> 
    dplyr::rename(cohortDefinitionId = cohortId,
                  cohortDefinitionName = cohortName) |> 
    SqlRender::camelCaseToSnakeCaseNames()
  
  outcomes <- allCohorts |> 
    dplyr::filter(
      cohortId %in% c(analysis2InputSpecifications |> 
                        dplyr::inner_join(combi) |> 
                        dplyr::pull(oId))
    ) |>
    dplyr::inner_join(fullPhenotypeLog) |> 
    dplyr::select(cohortId,
                  cohortName,
                  cleanWindow) |> 
    dplyr::arrange(cohortId) |> 
    dplyr::rename(cohortDefinitionId = cohortId,
                  cohortDefinitionName = cohortName) |> 
    SqlRender::camelCaseToSnakeCaseNames()
  
  writexl::write_xlsx(list(targets = targets, outcomes = outcomes), paste0("analysis_specifications/analysis2_",
                                                                           combi$from,
                                                                           "_",
                                                                           combi$id,
                                                                           ".xlsx"))
    
}

## Analysis 3 ----
targets <- allCohorts |>
  dplyr::filter(
    cohortId %in% c(
      subsetOfCohorts$howOften$cohortId,
      subsetOfCohorts$libraryIndicationCohorts$cohortId
    )
  ) |>
  dplyr::inner_join(fullPhenotypeLog) |> 
  dplyr::select(cohortId,
                cohortName) |> 
  dplyr::arrange(cohortId) |> 
  dplyr::rename(cohortDefinitionId = cohortId,
                cohortDefinitionName = cohortName) |> 
  SqlRender::camelCaseToSnakeCaseNames()

outcomes <- allCohorts |> 
  dplyr::filter(
    cohortId %in% c(
      subsetOfCohorts$foundInLibraryOutcomeDme$cohortId,
      subsetOfCohorts$foundInLibraryOutcomeAesi$cohortId,
      subsetOfCohorts$foundInLibraryOutcomeLegend$cohortId
    )
  ) |>
  dplyr::inner_join(fullPhenotypeLog) |> 
  dplyr::select(cohortId,
                cohortName, 
                cleanWindow) |> 
  dplyr::arrange(cohortId) |> 
  dplyr::rename(cohortDefinitionId = cohortId,
                cohortDefinitionName = cohortName) |> 
  SqlRender::camelCaseToSnakeCaseNames()

writexl::write_xlsx(list(targets = targets, outcomes = outcomes), "analysis_specifications/analysis3.xlsx")

