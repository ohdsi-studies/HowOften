# Code for creating the result tables in the OHDSI HowOften results schemas
# NOTE: HowOften results schemas are established outside of this script.

# Set the resultsTableFolderRoot to the location that holds one database's
# full results. Since we only need to create tables 1 time per results schema,
# we can use the same folder to create all of the tables.
resultsTableFolderRoot <- "E:/HowOften/2023_10_15_HowOften_Results/Results/andreas/truven_mdcd/strategusOutput"
resultsDatabaseSchemaCreationLogFolder <- "E:/HowOften"
resultsDatabaseSchemaSuffixList <- c(
  "andreas",
  "azza",
  "evan",
  "george",
  "gowza",
  "joel",
  "overall"
)

# Setup logging ----------------------------------------------------------------
ParallelLogger::clearLoggers()
ParallelLogger::addDefaultFileLogger(
  fileName = file.path(resultsDatabaseSchemaCreationLogFolder, "results-schema-setup-log.txt"),
  name = "RESULTS_SCHEMA_SETUP_FILE_LOGGER"
)
ParallelLogger::addDefaultErrorReportLogger(
  fileName = file.path(resultsDatabaseSchemaCreationLogFolder, 'results-schema-setup-errorReport.txt'),
  name = "RESULTS_SCHEMA_SETUP_ERROR_LOGGER"
)

# Connect to the database ------------------------------------------------------
resultsDatabaseConnectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = "postgresql",
  server = Sys.getenv("OHDSI_RESULTS_DB"),
  user = Sys.getenv("OHDSI_HO_USER"),
  password = Sys.getenv("OHDSI_HO_PASSWORD")
)
connection <- DatabaseConnector::connect(connectionDetails = resultsDatabaseConnectionDetails)

# Create the tables ------------------------
moduleFolders <- list.dirs(path = resultsTableFolderRoot, recursive = FALSE)
isModuleComplete <- function(moduleFolder) {
  doneFileFound <- (length(list.files(path = moduleFolder, pattern = "done")) > 0)
  isDatabaseMetaDataFolder <- basename(moduleFolder) == "DatabaseMetaData"
  return(doneFileFound || isDatabaseMetaDataFolder)
}

tryCatch({
  # Iterate over the results schema suffixes listed in resultsDatabaseSchemaSuffixList
  # and create the tables for each results schema
  message("Creating result tables based on definitions found in ", resultsTableFolderRoot)
  for (schemaSuffix in resultsDatabaseSchemaSuffixList) {
    resultsDatabaseSchema <- paste0("howoften_", schemaSuffix)
    # Skip over table creation if there are already tables created in
    # the resultsDatabaseSchema
    tables <- DatabaseConnector::getTableNames(
      connection = connection,
      databaseSchema = resultsDatabaseSchema
    )
    if (length(tables) == 0) {
      message("Creating results tables in schema: ", resultsDatabaseSchema)
      for (moduleFolder in moduleFolders) {
        moduleName <- basename(moduleFolder)
        if (!isModuleComplete(moduleFolder)) {
          warning("Module ", moduleName, " did not complete. Skipping table creation")
        } else {
          message("- Creating results for module ", moduleName)
          rdmsFile <- file.path(moduleFolder, "resultsDataModelSpecification.csv")
          if (!file.exists(rdmsFile)) {
            stop("resultsDataModelSpecification.csv not found in ", resumoduleFolderltsFolder)
          } else {
            specification <- CohortGenerator::readCsv(file = rdmsFile)
            sql <- ResultModelManager::generateSqlSchema(csvFilepath = rdmsFile)
            sql <- SqlRender::render(
              sql = sql,
              database_schema = resultsDatabaseSchema
            )
            DatabaseConnector::executeSql(connection = connection, sql = sql)
          }
        }
      }
      message("Creating empty characterization tables in schema: ", resultsDatabaseSchema)
      rdmsFile <- "./ResultsUpload/cc_resultsDataModelSpecification.csv"
      specification <- CohortGenerator::readCsv(file = rdmsFile)
      sql <- ResultModelManager::generateSqlSchema(csvFilepath = rdmsFile)
      sql <- SqlRender::render(
        sql = sql,
        database_schema = resultsDatabaseSchema
      )
      DatabaseConnector::executeSql(connection = connection, sql = sql)
    } else {
      message("SKIPPING - Results tables already exist")
    }
  }
},
finally = {
  # Disconnect from the database -------------------------------------------------
  DatabaseConnector::disconnect(connection)
  
  # Unregister loggers -----------------------------------------------------------
  ParallelLogger::unregisterLogger("RESULTS_SCHEMA_SETUP_FILE_LOGGER")
  ParallelLogger::unregisterLogger("RESULTS_SCHEMA_SETUP_ERROR_LOGGER")
})

