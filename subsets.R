# Subset Operators
subsetOperators <- list (
  adultSubset = CohortGenerator::createDemographicSubset(
    name = "",
    ageMin = 18
  ),
  priorT2Subset = CohortGenerator::createCohortSubset(
    name = "",
    cohortIds = 13308,
    startWindow = CohortGenerator::createSubsetCohortWindow(
      startDay = -9999,
      endDay = 0,
      targetAnchor = "cohortStart"
    ),
    endWindow = CohortGenerator::createSubsetCohortWindow(
      startDay = 0,
      endDay = 9999,
      targetAnchor = "cohortStart"
    ),
    cohortCombinationOperator = "all",
    negate = F
  ),
  priorCanaSubset  = CohortGenerator::createCohortSubset(
    name = "",
    cohortIds = 7792,
    startWindow = CohortGenerator::createSubsetCohortWindow(
      startDay = -9999,
      endDay = -30,
      targetAnchor = "cohortStart"
    ),
    endWindow = CohortGenerator::createSubsetCohortWindow(
      startDay = 0,
      endDay = 9999,
      targetAnchor = "cohortStart"
    ),
    cohortCombinationOperator = "all",
    negate = F
  ),
  priorMetforminSubset = CohortGenerator::createCohortSubset(
    name = "",
    cohortIds = 7371,
    startWindow = CohortGenerator::createSubsetCohortWindow(
      startDay = -9999,
      endDay =-30,
      targetAnchor = "cohortStart"
    ),
    endWindow = CohortGenerator::createSubsetCohortWindow(
      startDay = 0,
      endDay = 9999,
      targetAnchor = "cohortStart"
    ),
    cohortCombinationOperator = "all",
    negate = F
  ),
  noPriorMetforminSubset = CohortGenerator::createCohortSubset(
    name = "",
    cohortIds = 7371,
    startWindow = CohortGenerator::createSubsetCohortWindow(
      startDay = -9999,
      endDay =-30,
      targetAnchor = "cohortStart"
    ),
    endWindow = CohortGenerator::createSubsetCohortWindow(
      startDay = 0,
      endDay = 9999,
      targetAnchor = "cohortStart"
    ),
    cohortCombinationOperator = "all",
    negate = T
  ),
  priorInsulinSubset = CohortGenerator::createCohortSubset(
    name = "",
    cohortIds = 7277,
    startWindow = CohortGenerator::createSubsetCohortWindow(
      startDay = -9999,
      endDay =0,
      targetAnchor = "cohortStart"
    ),
    endWindow = CohortGenerator::createSubsetCohortWindow(
      startDay = -9999,
      endDay = 9999,
      targetAnchor = "cohortStart"
    ),
    cohortCombinationOperator = "all",
    negate = F
  ),
  noPriorInsulinSubset = CohortGenerator::createCohortSubset(
    name = "",
    cohortIds = 7277,
    startWindow = CohortGenerator::createSubsetCohortWindow(
      startDay = -9999,
      endDay =0,
      targetAnchor = "cohortStart"
    ),
    endWindow = CohortGenerator::createSubsetCohortWindow(
      startDay = -9999,
      endDay = 9999,
      targetAnchor = "cohortStart"
    ),
    cohortCombinationOperator = "all",
    negate = T
  )
)

subsetDefs <- list(
  t2dm_CanaSubset = CohortGenerator::createCohortSubsetDefinition(
      name = "[Prior T2DM + Cana]",
      definitionId = 1,
      subsetOperators = list(subsetOperators$adultSubset, subsetOperators$priorT2Subset, subsetOperators$priorCanaSubset)
  ),
  t2dm_Cana_InsulinSubset = CohortGenerator::createCohortSubsetDefinition(
      name = "[Prior T2DM + Cana + Insulin]",
      definitionId = 3,
      subsetOperators = list(subsetOperators$adultSubset, subsetOperators$priorT2Subset, subsetOperators$priorCanaSubset, subsetOperators$priorInsulinSubset)
  ),
  t2dm_Cana_NoInsulinSubset = CohortGenerator::createCohortSubsetDefinition(
      name = "[Prior T2DM + Cana + No Insulin]",
      definitionId = 6,
      subsetOperators = list(subsetOperators$adultSubset, subsetOperators$priorT2Subset, subsetOperators$priorCanaSubset, subsetOperators$noPriorInsulinSubset)
  ),
  t2dm_Cana_MetSubset = CohortGenerator::createCohortSubsetDefinition(
      name = "[Prior T2DM + Cana + Metformin]",
      definitionId = 4,
      subsetOperators = list(subsetOperators$adultSubset, subsetOperators$priorT2Subset, subsetOperators$priorCanaSubset, subsetOperators$priorMetforminSubset)
  ),
  t2dm_Cana_NoMetSubset = CohortGenerator::createCohortSubsetDefinition(
      name = "[Prior T2DM + Cana + No Metformin]",
      definitionId = 5,
      subsetOperators = list(subsetOperators$adultSubset, subsetOperators$priorT2Subset, subsetOperators$priorCanaSubset, subsetOperators$noPriorMetforminSubset)
  )
)










