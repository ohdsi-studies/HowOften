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

# Create the keyring using the password from Sys.getenv("STRATEGUS_KEYRING_PASSWORD")
keyring::keyring_create(keyring = keyringName, password = Sys.getenv("STRATEGUS_KEYRING_PASSWORD"))

# excecute this for each connectionDetails/ConnectionDetailsReference you are going to use
Strategus::storeConnectionDetails(
  connectionDetails = connectionDetails,
  connectionDetailsReference = connectionDetailsReference,
  keyringName = keyringName
)



