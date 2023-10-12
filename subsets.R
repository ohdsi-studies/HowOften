# Subset Operators
subsetOperators <- list (
  ageGender = CohortGenerator::createDemographicSubset(
    name = "",
    ageMin = 0,
    ageMax = 99,
    gender = c("male","female")
  ),
  priorObsAndYears = CohortGenerator::createLimitSubset(
    name = "",
    priorTime = 365,
    followUpTime = 0,
    calendarStartDate = "2012-01-01",
    calendarEndDate = "2022-12-31"
  )
)

subsetDefs <- list(
  targetSubset = CohortGenerator::createCohortSubsetDefinition(
      name = "",
      definitionId = 1,
      subsetOperators = list(subsetOperators$ageGender, subsetOperators$priorObsAndYears),
      subsetCohortNameTemplate = "@baseCohortName"
  )
)










