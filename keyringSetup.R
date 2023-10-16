# Install keyring - one time operation ---------
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

# Provide your environment specific values ------
connectionDetails <- NULL # fetch/create your own connection details here
connectionDetailsReference <- "mYDatasourceKey" # short abbreviation that describes these connection details

# test the connection
conn <- DatabaseConnector::connect(connectionDetails)
DatabaseConnector::disconnect(conn)


# Run the rest to setup keyring ----------
##################################
# DO NOT MODIFY BELOW THIS POINT
##################################
keyringName <- "HowOften"

# Create the keyring if it does not exist.
allKeyrings <- keyring::keyring_list()
if (!(keyringName %in% allKeyrings$keyring)) {
  keyring::keyring_create(keyring = keyringName, password = Sys.getenv("STRATEGUS_KEYRING_PASSWORD"))
} else {
  stop("Keyring already exists. You do not need to create it again.")
}

# excecute this for each connectionDetails/ConnectionDetailsReference you are going to use
Strategus::storeConnectionDetails(
  connectionDetails = connectionDetails,
  connectionDetailsReference = connectionDetailsReference,
  keyringName = keyringName
)



