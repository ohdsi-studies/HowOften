# Install keyring - one time operation ---------
#install.packages(c("keyring", "cli", "getPass"))

appendToRenviron <- function(varName, value, environFile = "~/.Renviron") {
  if (file.exists(environFile))
    lines <- readLines(environFile)
  else
    lines <- c()

  if (any(grepl(varName, lines))) {
    cli::cli_alert_info("Found existing environment variable {varName}, last value set will be taken by system")
  }

  renviron <- c(lines, glue::glue("{varName}='{value}'"))
  writeLines(renviron, environFile)
}


createStrategusKeyring <- function(keyringName = "HowOften",
                                   connectionDetailsReference = 'mYDatasourceKey') {
  if (!interactive()) {
    cli::cli_abort("Requires interactive session")
  }

  cli::cli_text("Creating keyring {keyringName}")
  cli::cli_text("This script is designed to set up your variables for your strategus keyring.")
  cli::cli_text("You will be prompted for a number of inputs that will be validated.")

  if (Sys.getenv("STRATEGUS_KEYRING_PASSWORD") == "") {
    cli::cli_par("Please enter a password for your keyring")
    passVar <- getPass::getPass("Keyring password", noblank = TRUE)

    if (passVar == "" || is.null(passVar)) {
      cli::cli_abort("Must enter a password")
    }
    Sys.setenv("STRATEGUS_KEYRING_PASSWORD" = passVar)
    appendToRenviron("STRATEGUS_KEYRING_PASSWORD", passVar)
  }

  cli::cli_alert_success("STRATEGUS_KEYRING_PASSWORD environment var set")

  if (Sys.getenv("INSTANTIATED_MODULES_FOLDER") == "") {
    strategusModuleFolder <- ""
    appendToRenviron("INSTANTIATED_MODULES_FOLDER", strategusModuleFolder)
  }

  cli::cli_alert_success("INSTANTIATED_MODULES_FOLDER environment var set")

  tempconnectionFile <- tempfile(fileext = ".R")

  cli::cli_alert_info("Creating file {tempconnectionFile} for Database credentials - this file will be automaticaly removed when completed and the settings saved securly")
  on.exit(unlink(tempconnectionFile, force = TRUE))

  rscript <- "# Enter your database connection values here and press save
# the connection will be tested
connectionDetails <-
  DatabaseConnector::createConnectionDetails(
    dbms = '',
    server = '',
    password = '',
    user = '',
    port = 0,
    extraSettings = NULL,
    connectionString = NULL,
    pathToDriver = Sys.getenv('DATABASECONNECTOR_JAR_FOLDER')
  )

# short abbreviation that describes these connection details

"

  writeLines(rscript, con = tempconnectionFile)
  connectionValid <- FALSE
  while (!connectionValid) {
    resp <- utils::menu(c("yes", "no"), title = "Secure creation of connection details required, continue?")
    if (resp != 1) {
      cli::cli_abort("Secure database credentials cannot be aquired")
    }
    utils::file.edit(tempconnectionFile)
    # test the connection
    tryCatch(
    {
      source(tempconnectionFile)
      conn <- DatabaseConnector::connect(connectionDetails)
      DatabaseConnector::disconnect(conn)
      connectionValid <- TRUE
      cli::cli_alert_success("Database Connection Works")
    },
      error = function(message) {
        cli::cli_alert_warning("Database Connection Failed, retrying...")
        cli::cli_alert(message)
      }
    )
  }
  unlink(tempconnectionFile, force = TRUE)

  keyringPassword <- Sys.getenv("STRATEGUS_KEYRING_PASSWORD") # This password is simply to avoid a prompt when creating the keyring
  # Create the keyring if it does not exist.
  # If it exists, clear it out so we can re-load the keys
  allKeyrings <- keyring::keyring_list()
  if (keyringName %in% allKeyrings$keyring) {
    if (keyring::keyring_is_locked(keyring = keyringName)) {
      keyring::keyring_unlock(keyring = keyringName, password = keyringPassword)
    }
    # Delete all keys from the keyring so we can delete it
    message(paste0("Delete existing keyring: ", keyringName))
    keys <- keyring::key_list(keyring = keyringName)
    if (nrow(keys) > 0) {
      for (i in 1:nrow(keys)) {
        keyring::key_delete(keys$service[i], keyring = keyringName)
      }
    }
    keyring::keyring_delete(keyring = keyringName)
  }

  keyring::keyring_create(keyring = keyringName, password = keyringPassword)

  # excecute this for each connectionDetails/ConnectionDetailsReference you are going to use
  Strategus::storeConnectionDetails(
    connectionDetails = connectionDetails,
    connectionDetailsReference = connectionDetailsReference,
    keyringName = keyringName
  )

  cli::cli_alert_success("Secure strategus keyring reference {keyringName} created")
}

createStrategusKeyringCli()