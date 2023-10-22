#######################################################################
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
#                                                                     #
# NOTE: HowOftenResultsTableCreation.R must be run BEFORE this script #
#                                                                     #
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!#
#######################################################################
# Code for uploading results to the OHDSI HowOften results schemas
# Notes on this script:
# 1. The folder structure below the "resultsFolderRoot" must be:
#   |+-- Results
#   |   +-- <analysis> (i.e. "overall")
#   |     +-- <database> (i.e. "truven_mdcd")
#   |       +-- strategusOutput
#   |         +-- <StrategusModule>
# 2. The list of <analysis> folders (see above) matches the individual
#    HowOften analyses. These analyses are stored in the vector
#    howOftenAnalyses. This script assumes that all of the howOftenAnalyses
#    have a folder in the results even if no results were calcuated.
# 3. You may choose to filter the <database> list by using the 
#    databaseFilterList vector below. If the vector is empty, the script
#    will assume to upload all database results found in the <database>
#    subdirectories
# 4. There will exist a folder per <StrategusModule> executed by the study
#    and this script will upload them if a "done" file is found in the
#    individual <StrategusModule> folder.

# Set this to root of the results
resultsFolderRoot <- 'D:/projects/HowOften'

# Set this to c() if not using the analysis filtering.
# This will then upload all analyses results found in each
# analysis directory
howOftenAnalysesFilterList <- c(
  "andreas",
  "azza",
  "evan",
  "george",
  "gowza",
  "joel",
  "overall"
)

# Set this to c() if not using the database filtering.
# This will then upload all database results found 
# in each analysis directory
databaseFilterList <- c(
  # "truven_ccae",
  # "truven_mdcd",
  # "truven_mdcr",
  # "iqvia_pharmetrics_plus",
  # "optum_extended_ses",
  # "optum_ehr",
  # "iqvia_amb_emr",
  # "CUIMC OMOP 2023q3r1",
  # "ims_australia_lpd",
  # "ims_france",
  # "ims_germany",
  # "jmdc"
)

# Traverse results to obtain list of results for upload ------------------------
dfResultsFolders <- data.frame()
# Get the list of analyses directories in the results
analysesInResults <- list.dirs(
  path = file.path(resultsFolderRoot, "Results"),
  recursive = FALSE,
  full.names = FALSE
)
if (length(howOftenAnalysesFilterList) > 0) {
  howOftenAnalysesFiltered <- intersect(
    x = analysesInResults, 
    y = howOftenAnalysesFilterList
  )
  if (length(howOftenAnalysesFiltered) != length(howOftenAnalysesFilterList)) {
    message <- paste0(
      "You requested to filter results to the following analyses:",
      paste(howOftenAnalysesFilterList, collapse = ","),
      " but ", 
      file.path(resultsFolderRoot, "Results"),
      " only contained: ",
      paste(howOftenAnalysesFiltered)
    )
    stop(message)
  }
  analysesInResults <- howOftenAnalysesFiltered
}

# Get the list of databases in the analysis results
for (analysis in analysesInResults) {
  databaseResultsFolder <- file.path(resultsFolderRoot, "Results", analysis)
  databaseFoldersInResults <- list.dirs(
    path = databaseResultsFolder,
    recursive = FALSE,
    full.names = FALSE
  ) 
  if (length(databaseFilterList) > 0) {
    databaseFoldersInResultsFiltered <- intersect(
      x = databaseFoldersInResults, 
      y = databaseFilterList
    )
    # If we are filtering the database list and we didn't find
    # one of the databases, stop the upload process.
    if (length(databaseFoldersInResultsFiltered) != length(databaseFilterList)) {
      message <- paste0(
        "You requested to filter results to the following databases:",
        paste(databaseFilterList, collapse = ","),
        " but ", 
        databaseResultsFolder,
        " only contained: ",
        paste(databaseFoldersInResultsFiltered)
      )
      stop(message)
    }
    # Assign to the filtered database list
    databaseFoldersInResults <- databaseFoldersInResultsFiltered
  }
  # Add to the data.frame that contains the list of results to upload
  dfResultsFolders <- rbind(
    dfResultsFolders,
    data.frame(
      analysis = analysis,
      database = databaseFoldersInResults,
      strategusResultsFolder = file.path(resultsFolderRoot, "Results", analysis, databaseFoldersInResults, "strategusOutput")
    )
  )
}

if (nrow(dfResultsFolders) == 0) {
  stop("No results to upload.")
}

# Connect to the database ------------------------------------------------------
resultsDatabaseConnectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = "postgresql",
  server = Sys.getenv("OHDSI_RESULTS_DB"),
  user = Sys.getenv("OHDSI_HO_USER"),
  password = Sys.getenv("OHDSI_HO_PASSWORD")
)
connection <- DatabaseConnector::connect(connectionDetails = resultsDatabaseConnectionDetails)

# Upload results -----------------
isModuleComplete <- function(moduleFolder) {
  doneFileFound <- (length(list.files(path = moduleFolder, pattern = "done")) > 0)
  isDatabaseMetaDataFolder <- basename(moduleFolder) == "DatabaseMetaData"
  return(doneFileFound || isDatabaseMetaDataFolder)
}

deletePriorCIResults <- function(con, schemaName, databaseId) {
  message("- Removing CI Results from ", schemaName, " for databaseId [", databaseId, "]")
  sql <- "DELETE FROM @schema.ci_incidence_summary WHERE database_id = '@database_id'"
  DatabaseConnector::renderTranslateExecuteSql(
    connection = con,
    sql = sql,
    schema = schemaName,
    database_id = databaseId,
    progressBar = FALSE,
    reportOverallTime = FALSE
  )
  
}

# Setup logging ----------------------------------------------------------------
ParallelLogger::clearLoggers()
ParallelLogger::addDefaultFileLogger(
  fileName = file.path(file.path(resultsFolderRoot, "Results"), "upload-log.txt"),
  name = "RESULTS_FILE_LOGGER"
)
ParallelLogger::addDefaultErrorReportLogger(
  fileName = file.path(file.path(resultsFolderRoot, "Results"), 'upload-errorReport.txt'),
  name = "RESULTS_ERROR_LOGGER"
)

tryCatch({
  for (i in 1:nrow(dfResultsFolders)) {
    resultFolder <- dfResultsFolders[i,]
    message("Loading results for analysis: ", resultFolder$analysis, ", database: ", resultFolder$database, " in ", resultFolder$strategusResultsFolder)
    resultsDatabaseSchema <- paste0("howoften_", resultFolder$analysis)
    moduleFolders <- list.dirs(path = resultFolder$strategusResultsFolder, recursive = FALSE)
    for (moduleFolder in moduleFolders) {
      moduleName <- basename(moduleFolder)
      if (!isModuleComplete(moduleFolder)) {
        warning("Module ", moduleName, " did not complete. Skipping upload")
      } else {
        message("- Uploading results for module ", moduleName)
        rdmsFile <- file.path(moduleFolder, "resultsDataModelSpecification.csv")
        if (!file.exists(rdmsFile)) {
          stop("resultsDataModelSpecification.csv not found in ", resumoduleFolderltsFolder)
        } else {
          if (grepl("CohortIncidence", moduleName)) {
            #read DB ID
            databaseMeta <- CohortGenerator::readCsv(file=file.path(
              resultFolder$strategusResultsFolder,
              "DatabaseMetaData/database_meta_data.csv"))
            databaseId <- databaseMeta$databaseId
            deletePriorCIResults(connection, resultsDatabaseSchema, databaseId = databaseId)
          }
          specification <- CohortGenerator::readCsv(file = rdmsFile)
          runCheckAndFixCommands = grepl("CohortDiagnostics", moduleName)
          ResultModelManager::uploadResults(
            connection = connection,
            schema = resultsDatabaseSchema,
            resultsFolder = moduleFolder,
            purgeSiteDataBeforeUploading = TRUE,
            databaseIdentifierFile = file.path(
              resultFolder$strategusResultsFolder,
              "DatabaseMetaData/database_meta_data.csv"
            ),
            runCheckAndFixCommands = runCheckAndFixCommands,
            specifications = specification
          )
        }
      }
    }
  }
  
  # Analyze all tables in each results schema
  # to improve performance
  distictAnalyses <- unique(dfResultsFolders$analysis)
  for (analysis in distictAnalyses) {
    resultsDatabaseSchema <- paste0("howoften_", analysis)
    message("Analyzing all tables in results schema: ", resultsDatabaseSchema)
    sql <- "ANALYZE @schema.@table_name;"
    tableList <- DatabaseConnector::getTableNames(
      connection = connection,
      databaseSchema = resultsDatabaseSchema
    )
    for (i in 1:length(tableList)) {
      DatabaseConnector::renderTranslateExecuteSql(
        connection = connection,
        sql = sql,
        schema = resultsDatabaseSchema,
        table_name = tableList[i],
        progressBar = FALSE,
        reportOverallTime = FALSE
      )
    }
  }
},
finally = {
  # Disconnect from the database -------------------------------------------------
  DatabaseConnector::disconnect(connection)
  
  # Unregister loggers -----------------------------------------------------------
  ParallelLogger::unregisterLogger("RESULTS_FILE_LOGGER")
  ParallelLogger::unregisterLogger("RESULTS_ERROR_LOGGER")
})

