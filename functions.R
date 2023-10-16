# Get the unique IDs
.getUniqueCohortIds <- function(fileList) {
  cohortIds <- unique(unlist(lapply(fileList, function(file) {
    targets <- readxl::read_xlsx(file, sheet="targets")
    outcomes <- readxl::read_xlsx(file, sheet="outcomes")
    return (c(targets$cohort_definition_id, outcomes$cohort_definition_id))
  })))
  return (cohortIds)
}

.getUniqueTargetCohortIds <- function(fileList) {
  cohortIds <- unique(unlist(lapply(fileList, function(file) {
    targets <- readxl::read_xlsx(file, sheet="targets")
    return (targets$cohort_definition_id)
  })))
  return (cohortIds)
}


#create analysis spec

.createAnalysisSpecification <- function(jsonFileName, cohortDefinitionShared, cohortGeneratorSpecs, cohortIncidenceSpecs) {
  # Combine across modules -------------------------------------------------------
  analysisSpecifications <- Strategus::createEmptyAnalysisSpecificiations() %>%
    Strategus::addSharedResources(cohortDefinitionShared) %>%
    Strategus::addModuleSpecifications(cohortGeneratorSpecs) %>%
    Strategus::addModuleSpecifications(cohortIncidenceSpecs)

  cat(paste0("Saving file: ", file.path(jsonFileName)))
  ParallelLogger::saveSettingsToJson(analysisSpecifications, file.path(jsonFileName))
}
