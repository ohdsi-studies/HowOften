## How Often: An Incidence Analysis for a series of OHDSI Community Submissions

<img src="https://img.shields.io/badge/Study%20Status-Design%20Finalized-brightgreen.svg" alt="Study Status: Design Finalized">

- Analytics use case(s): **Characterization**
- Study type: **Clinical Application**
- Tags: **Incidence**
- Study lead: **George Hripcsak**
- Study lead forums tag: **[hripcsa](https://forums.ohdsi.org/u/hripcsa)**
- Study start date: **August 2023**
- Study end date: **-**
- Protocol: **[HowOften Study Protocol](documents/HowOften protocol v1.0.pdf)**
- Publications: **None**
- Results explorer: **[ShinyApp Explorer](https://results.ohdsi.org/)**

## Overview

How Often is a Large-Scale Characterization analysis to compute incidence for a broad collection of target populations and outcomes across the OHDSI network.  Incidence analyses can be framed as: "Amongst patients who are *insert your favorite target cohort i*, how many patients experienced *insert your favorite outcome j* within *time horizon relative to target start*?", and HowOften aims to systematically apply this analysis to range of target cohorts, outcome cohorts, and time horizons, to address an array of clinical questions that incidence evidence can inform. 

## Getting Started

There are 3 parts to executing the study:  Pre-Configuration (to get base R libraries set up), Keyring Setup and finally Analysis Execution.


### Pre-Configuration

HowOften requires:

- R v4.2 (Preferably 4.2.3)
- DatabaseConnector >= 6.2.3
- Strategus v0.1.0

DatabaseConnector may have challenges to updating if already installed as a Package. Either update the package in a R CLI (outside of RStudio) or use `unloadNamespace()` to remove DatabaseConnector from memory.

### Keyring Setup

This repository provides a [keyringSetup.R](keyringSetup.R) script that provides initialization scripts to set up your R environment and register connection details as `connection refs` for use in Strategus.

**Part 1** is to ensure your environment has 2 environment variables: 
- STRATEGUS_KEYRING_PASSWORD: used to unlock your Strategus keyring.
- INSTANTIATED_MODULES_FOLDER: a shared folder location that is used to download and cache Strategus Modules.

```
install.packages("keyring")

if (Sys.getenv("STRATEGUS_KEYRING_PASSWORD") == "") {
  # set keyring password by adding STRATEGUS_KEYRING_PASSWORD='sos' to renviron
  usethis::edit_r_environ()
  # then add STRATEGUS_KEYRING_PASSWORD='yourPassword', save and close
  # Restart your R Session to confirm it worked
  stop("Please add STRATEGUS_KEYRING_PASSWORD='yourPassword' to your .Renviron file
       via usethis::edit_r_environ() as instructed, save and then restart R session")
}

if (Sys.getenv("INSTANTIATED_MODULES_FOLDER") == "") {
  # set a env var to a path to cache Strategus modules
  usethis::edit_r_environ()
  # then add INSTANTIATED_MODULES_FOLDER='path/to/module/cache', save and close
  # Restart your R Session to confirm it worked
  stop("Please add INSTANTIATED_MODULES_FOLDER='{path to module cache folder}' to your .Renviron file
       via usethis::edit_r_environ() as instructed, save and then restart R session")
}
```

**Part 2** is where you will instantiate your connection details in memory, and test connectivity:

```
# Provide your environment specific values ------
connectionDetails <- NULL # fetch/create your own connection details here
connectionDetailsReference <- "mYDatasourceKey" # short abbreviation that describes these connection details

# test the connection
conn <- DatabaseConnector::connect(connectionDetails)
DatabaseConnector::disconnect(conn)
```

In the above, you will assign connectionDetails in your r environment through your own script.

**Part 3** is to create your Keyring if it does not exist:

```
# Create the keyring if it does not exist.
allKeyrings <- keyring::keyring_list()
if (!(keyringName %in% allKeyrings$keyring)) {
  keyring::keyring_create(keyring = keyringName, password = Sys.getenv("STRATEGUS_KEYRING_PASSWORD"))
} else {
  stop("Keyring already exists. You do not need to create it again.")
}
```

**Part 4** will store your connection details into your keyring:

```
# excecute this for each connectionDetails/ConnectionDetailsReference you are going to use
Strategus::storeConnectionDetails(
  connectionDetails = connectionDetails,
  connectionDetailsReference = connectionDetailsReference,
  keyringName = keyringName
)
```

### Executing the Analysis

The [StrategusCodeToRun.R](StrategusCodeToRun.R) contains the script that will perform the execution of the 7 individual analyes in HowOften:

**Part 1** sets up variables that will be used as input to execution:

```
##=========== START OF INPUTS ==========
keyringName <- "HowOften"
connectionDetailsReference <- "yourCdmRef"
workDatabaseSchema <- 'writable_schema'
cdmDatabaseSchema <- 'cdm_schema'
outputLocation <- '{path/to/Strategus/Output}'
resultsLocation <- '{path/to/Strategus/Results}'
minCellCount <- 5 # set this to a value where you want to censor small cells
cohortTableName <- "howoften_cohort"
```

Note: the outputLocation will be reused between analysis exeuctions to cache cohort generation info.   Each analysis execution will copy from the `outputLocation` to the `resultsLocation` under the directory dedicated to the individual studies.  The `resultsLocation` folder will be zipped and submitted for inclusion in the ShinyApp viewer.

**Part 2** sets up execution settings and creates the helper function to execute the analysis and copy results to the result folder:

```
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

```

**Part 3** executes the individual HowOften analyses:

```
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

```

### Submitting Results

Once the analyses executions are complete (not all are required to be completed, some analyses are very large and may not be completed in time for the OHDSI Symposium), the Results folder is zipped and submitted to an FTP location for processing.  Results that are properlly submitted and formatted will be uploaded to OHDSI servers and will be available on [results.ohdsi.org](https://results.ohdsi.org/)

