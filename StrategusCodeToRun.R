# install the network package
# install.packages('remotes')
# remotes::install_github("OHDSI/Strategus")

library(Strategus)

##=========== START OF INPUTS ==========
keyringName <- "HowOften"
connectionDetailsReference <- "optum_extended_ses"
workDatabaseSchema <- 'scratch_cknoll1'
cdmDatabaseSchema <- 'cdm_optum_extended_ses_v2559'
outputLocation <- 'D:/projects/HowOften/Strategus'
resultsLocation <- 'D:/projects/HowOften/Results'
minCellCount <- 5
cohortTableName <- "howoften_cohort"


##=========== END OF INPUTS ==========
##################################
# DO NOT MODIFY BELOW THIS POINT
##################################

executionSettings <- Strategus::createCdmExecutionSettings(
  connectionDetailsReference = connectionDetailsReference,
  workDatabaseSchema = workDatabaseSchema,
  cdmDatabaseSchema = cdmDatabaseSchema,
  cohortTableNames = CohortGenerator::getCohortTableNames(cohortTable = cohortTableName),
  workFolder = file.path(outputLocation, connectionDetailsReference, "strategusWork"),
  resultsFolder = file.path(outputLocation, connectionDetailsReference, "strategusOutput"),
  minCellCount = minCellCount
)

executeAnalysis <- function(analysisFile, executionSettings, analysisName, outputLocation, resultsLocation, keyringName) {

  analysisSpecifications <- ParallelLogger::loadSettingsFromJson(
    fileName = analysisFile
  )

  Strategus::execute(
    analysisSpecifications = analysisSpecifications,
    executionSettings = executionSettings,
    executionScriptFolder = file.path(outputLocation, connectionDetailsReference, "strategusExecution"),
    keyringName = keyringName
  )

  # copy Results to final location
  resultsDir <- file.path(resultsLocation, analysisName, connectionDetailsReference)

  if (dir.exists(resultsDir)) {
    unlink(resultsDir, recursive = TRUE)
  }
  dir.create(file.path(resultsDir), recursive = TRUE)
  file.copy(file.path(outputLocation, connectionDetailsReference, "strategusOutput"),
            file.path(resultsDir), recursive = TRUE)

  return(NULL)

}

# Step 1 : Execute Azza Analysis
executeAnalysis("howoften_azza.json", executionSettings, "azza", outputLocation, resultsLocation, keyringName)

# Step 2 : Execute Andreas Analysis
executeAnalysis("howoften_andreas.json", executionSettings, "andreas", outputLocation, resultsLocation, keyringName)

# Step 3, Joel Analysis
executeAnalysis("howoften_joel.json", executionSettings, "joel", outputLocation, resultsLocation, keyringName)

# step 4, Evan analysis
executeAnalysis("howoften_evan.json", executionSettings, "evan", outputLocation, resultsLocation, keyringName)

# step 5, gowza analysis
executeAnalysis("howoften_gowza.json", executionSettings, "gowza", outputLocation, resultsLocation, keyringName)

# step 6, overall analysis
executeAnalysis("howoften_overall.json", executionSettings, "overall", outputLocation, resultsLocation, keyringName)

# step 7, george analysis
executeAnalysis("howoften_george.json", executionSettings, "george", outputLocation, resultsLocation, keyringName)



