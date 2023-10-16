basePath <-
  rstudioapi::getActiveDocumentContext()$path |> dirname() |> dirname()

listOfCohortIds <- readxl::read_excel(
  path = file.path(basePath,
                   "analysis_specifications",
                   "analysis1.xlsx"),
  sheet = "outcomes"
)

phenotypeLog <-
  PhenotypeLibrary::getPhenotypeLog(cohortIds = listOfCohortIds$cohort_definition_id) |>
  dplyr::arrange(cohortId) |>
  dplyr::select(
    cohortId,
    cohortName,
    numberOfInclusionRules,
    numberOfCohortEntryEvents,
    numberOfDomainsInEntryEvents,
    domainsInEntryEvents,
    domainConditionOccurrence,
    domainMeasurement,
    domainObservation,
    domainVisitOccurrence,
    domainDrugExposure,
    domainProcedureOccurrence,
    domainDeviceExposure,
    domainDrugEra,
    domainConditionEra,
    criteriaLocationVisitTypePrimaryCriteria
  )

conceptSets <-
  PhenotypeLibrary::getPlConceptDefinitionSet(cohortIds = phenotypeLog$cohortId |> unique()) |>
  dplyr::filter(conceptSetUsedInEntryEvent == 1) |>
  dplyr::select(
    uniqueConceptSetId,
    cohortId,
    conceptSetId,
    conceptSetName,
    hasVisit,
    conceptSetExpression,
    conceptSetSql
  ) |>
  dplyr::arrange(cohortId,
                 conceptSetId)


cdmSource <- PrivateScripts::getCdmSource(cdmSources = cdmSources)
connectionDetails <-
  PrivateScripts::createConnectionDetails(cdmSources = cdmSources)
connection <-
  DatabaseConnector::connect(connectionDetails = connectionDetails)

resolvedConcepts <- c()
for (i in (1:nrow(conceptSets))) {
  resolvedSet <- DatabaseConnector::renderTranslateQuerySql(
    connection = connection,
    sql = conceptSets[i, ]$conceptSetSql,
    snakeCaseToCamelCase = TRUE,
    vocabulary_database_schema = cdmSource$vocabDatabaseSchemaFinal
  ) |>
    dplyr::rename(conceptSetId = codesetId)
  
  resolvedConcepts[[i]] <-
    conceptSets[i, ] |>
    dplyr::select(-conceptSetExpression, -conceptSetSql) |>
    dplyr::left_join(resolvedSet,
                     by = "conceptSetId")
}

resolvedConcepts <- dplyr::bind_rows(resolvedConcepts) |>
  dplyr::relocate(cohortId,
                  conceptId) |>
  dplyr::left_join(phenotypeLog |>
                     dplyr::select(cohortId,
                                   cohortName)) |>
  dplyr::relocate(cohortId,
                  conceptId) |>
  dplyr::distinct() |>
  dplyr::rename(cohortEntryEventHasVisitDomain = hasVisit) |>
  dplyr::arrange(cohortId,
                 conceptId)

saveRDS(
  object = resolvedConcepts,
  file = file.path(
    rstudioapi::getActiveDocumentContext()$path |> dirname(),
    "ConceptIds.RDS"
  )
)

# readr::write_excel_csv(
#   x = resolvedConcepts,
#   file = file.path(
#     rstudioapi::getActiveDocumentContext()$path |> dirname(),
#     "ConceptIds.csv"
#   )
)
