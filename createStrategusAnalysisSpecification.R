library(readr)
library(dplyr)
library(readxl)
library(PhenotypeLibrary)

source("functions.R")

genPopTargetDef <- CohortIncidence::createCohortRef(id=1071, name="persons at risk at start of year 2012-2022 with 365d prior observation")



azzaFileList <- c(file.path("analysis_specifications","analysis2_azza_1.xlsx"))
azzaFileName <- "howoften_azza.json"

andreasFileList <- c(file.path("analysis_specifications","analysis2_andreas_1.xlsx"),
                     file.path("analysis_specifications","analysis2_andreas_2.xlsx"),
                     file.path("analysis_specifications","analysis2_andreas_3.xlsx"),
                     file.path("analysis_specifications","analysis2_andreas_4.xlsx"))
andreasFileName <- "howoften_andreas.json"

joelFileList <- c(file.path("analysis_specifications","analysis2_joel_1.xlsx"),
                     file.path("analysis_specifications","analysis2_joel_2.xlsx"),
                     file.path("analysis_specifications","analysis2_joel_3.xlsx"),
                     file.path("analysis_specifications","analysis2_joel_4.xlsx"))
joelFileName <- "howoften_joel.json"

evanFileList <- c(file.path("analysis_specifications","analysis2_evan_1.xlsx"),
                     file.path("analysis_specifications","analysis2_evan_2.xlsx"))
evanFileName <- "howoften_evan.json"

gowzaFileList <- c(file.path("analysis_specifications","analysis2_gowtham_1.xlsx"))
gowzaFileName <- "howoften_gowza.json"

overallFileList <- c(file.path("analysis_specifications","analysis1.xlsx"))
overallFileName <- "howoften_overall.json"

georgeFileList <- c(file.path("analysis_specifications","analysis3.xlsx"))
georgeFileName <- "howoften_george.json"

allFileList <- c(azzaFileList, andreasFileList, joelFileList, evanFileList, gowzaFileList, overallFileList)

# we want outcomeIds to be the same across analyses (by cohortId, clean window)
# so we will process all outcomes together and assign a unique ID.
outcomeMap <- data.frame()
for (file in allFileList) {
  # the general population analysis is derived by matching the general population cohort to all T's and O's in the analysis.
  # By default, the clean window of T will be 0.
  targets <- readxl::read_xlsx(file, sheet="targets")
  targets$clean_window <- 0
  outcomeMap <- rbind(outcomeMap, readxl::read_xlsx(file, sheet="outcomes"))
  outcomeMap <- rbind(outcomeMap, targets)
};
outcomeMap <-outcomeMap %>% distinct() %>% dplyr::mutate(id=dplyr::row_number()) # need unique outcome id across all analyses

# source module SettingsFunctions.R
# CohortGeneratorModule --------------------------------------------------------
source("https://raw.githubusercontent.com/OHDSI/CohortGeneratorModule/v0.2.0/SettingsFunctions.R")
# CohortIncidenceModule --------------------------------------------------------
source("https://raw.githubusercontent.com/OHDSI/CohortIncidenceModule/v0.2.0/SettingsFunctions.R")

# define unique set of TARS
tars <- list(CohortIncidence::createTimeAtRiskDef(id=1, startWith="start", startOffset = 1, endWith="start", endOffset = 30),
             CohortIncidence::createTimeAtRiskDef(id=2, startWith="start", startOffset = 1, endWith="start", endOffset = 365),
             CohortIncidence::createTimeAtRiskDef(id=3, startWith="start", startOffset = 1, endWith="start", endOffset = 9999), # Intent to Treat
             CohortIncidence::createTimeAtRiskDef(id=4, startWith="start", startOffset = 1, endWith="end", endOffset = 0), # On Treatment
             CohortIncidence::createTimeAtRiskDef(id=5, startWith="start", startOffset = 1, endWith="start", endOffset = 14), # evan tar
             CohortIncidence::createTimeAtRiskDef(id=6, startWith="start", startOffset = 1, endWith="start", endOffset = 60), # evan tar
             CohortIncidence::createTimeAtRiskDef(id=7, startWith="start", startOffset = 31, endWith="start", endOffset = 365), # evan tar
             CohortIncidence::createTimeAtRiskDef(id=8, startWith="start", startOffset = 366, endWith="start", endOffset = 730)) # evan tar

# CohortGeneratorModuleSpecs is common for all
cohortGeneratorModuleSpecifications <- createCohortGeneratorModuleSpecifications(
  incremental = TRUE,
  generateStats = TRUE
)

# Create the 7 Strategist designs
# fileList <- azzaFileList
# strategusFileName <- azzaFileName


# azza analysis IIFE
(function(fileList, strategusFileName) {
  cohortIds <- c(genPopTargetDef$id, .getUniqueCohortIds(fileList))
  cohortDefinitionSet <- PhenotypeLibrary::getPlCohortDefinitionSet(cohortIds)

  cohortDefinitionShared <- createCohortSharedResourceSpecifications(cohortDefinitionSet)

  targets <-data.frame()
  outcomes<- data.frame()
  analysisList <- list()

  for (file in fileList) {
    targetXls <- readxl::read_xlsx(file, sheet="targets")
    targets <- rbind(targets, targetXls)

    outcomeXls <- readxl::read_xlsx(file, sheet="outcomes") %>%
      dplyr::inner_join(outcomeMap %>% select("id", "cohort_definition_id", "clean_window"), by=c('cohort_definition_id', 'clean_window'))
    outcomes<-rbind(outcomes, outcomeXls)

    analysisList <- append(analysisList,
                           CohortIncidence::createIncidenceAnalysis(targets = targetXls$cohort_definition_id,
                                                                    outcomes = outcomeXls$id,
                                                                    tars = c(2,3)))
  };

  # to save time, we'll attach a clean_window = 0 and lookup the outcome ID here, and use it for the background analysis
  targets <- targets %>% distinct() %>% mutate(clean_window = 0) %>%
    dplyr::inner_join(outcomeMap %>% select("id", "cohort_definition_id", "clean_window"), by=c('cohort_definition_id', 'clean_window'))

  outcomes <- outcomes %>% distinct()

  # make the background rate for all T and Os
  analysisList <- append(analysisList,
                         CohortIncidence::createIncidenceAnalysis(targets = genPopTargetDef$id,
                                                                  outcomes = c(targets$id, outcomes$id),
                                                                  tars = c(2)))


  targetDefs <- lapply(targets$cohort_definition_id, function(targetCohortId) {
    targetCohortDef <- targets %>% filter(cohort_definition_id == targetCohortId)
    return (CohortIncidence::createCohortRef(id=targetCohortDef$cohort_definition_id, name=targetCohortDef$cohort_definition_name))
  })

  #append general pop def
  targetDefs <- append(targetDefs, genPopTargetDef)

  # the general pop analysis derives Os from T's so we need to make all outcome defs contain both T and O
  outcomeDefs <- lapply(unique(c(targets$id, outcomes$id)), function(outcomeId) {
    outcomeDef <- outcomeMap %>% filter(id == outcomeId)
    return (CohortIncidence::createOutcomeDef(id = outcomeDef$id,
                                              name = outcomeDef$cohort_definition_name,
                                              cohortId = outcomeDef$cohort_definition_id,
                                              cleanWindow = outcomeDef$clean_window))
  })


  irDesign <- CohortIncidence::createIncidenceDesign(targetDefs = targetDefs,
                                                     outcomeDefs = outcomeDefs,
                                                     tars=tars,
                                                     analysisList = analysisList,
                                                     strataSettings = CohortIncidence::createStrataSettings(byGender=T, byYear=T, byAge=T, ageBreaks = c(3,13,18,30,40,50,60,70,80,90)))

  cohortIncidenceModuleSpecifications <- createCohortIncidenceModuleSpecifications(
    irDesign = irDesign$toList()
  )

  .createAnalysisSpecification(strategusFileName, cohortDefinitionShared, cohortGeneratorModuleSpecifications, cohortIncidenceModuleSpecifications)

})(azzaFileList, azzaFileName)


# andreas analysis IIFE
(function(fileList, strategusFileName) {
  cohortIds <- c(genPopTargetDef$id, .getUniqueCohortIds(fileList))
  cohortDefinitionSet <- PhenotypeLibrary::getPlCohortDefinitionSet(cohortIds)

  cohortDefinitionShared <- createCohortSharedResourceSpecifications(cohortDefinitionSet)

  targets <-data.frame()
  outcomes<- data.frame()
  analysisList <- list()

  for (file in fileList) {
    targetXls <- readxl::read_xlsx(file, sheet="targets")
    targets <- rbind(targets, targetXls)

    outcomeXls <- readxl::read_xlsx(file, sheet="outcomes") %>%
      dplyr::inner_join(outcomeMap %>% select("id", "cohort_definition_id", "clean_window"), by=c('cohort_definition_id', 'clean_window'))
    outcomes<-rbind(outcomes, outcomeXls)

    if (any(endsWith(file,c("analysis2_andreas_1.xlsx", "analysis2_andreas_4.xlsx")))) {
      analysisList <- append(analysisList,
                             CohortIncidence::createIncidenceAnalysis(targets = targetXls$cohort_definition_id,
                                                                      outcomes = outcomeXls$id,
                                                                      tars = c(1,2,3)))
    } else {
      analysisList <- append(analysisList,
                             CohortIncidence::createIncidenceAnalysis(targets = targetXls$cohort_definition_id,
                                                                      outcomes = outcomeXls$id,
                                                                      tars = c(1,2)))
    }
  };

  # to save time, we'll attach a clean_window = 0 and lookup the outcome ID here, and use it for the background analysis
  targets <- targets %>% distinct() %>% mutate(clean_window = 0) %>%
    dplyr::inner_join(outcomeMap %>% select("id", "cohort_definition_id", "clean_window"), by=c('cohort_definition_id', 'clean_window'))

  outcomes <- outcomes %>% distinct()

  # make the background rate for all T and Os
  analysisList <- append(analysisList,
                         CohortIncidence::createIncidenceAnalysis(targets = genPopTargetDef$id,
                                                                  outcomes = c(targets$id, outcomes$id),
                                                                  tars = c(2)))


  targetDefs <- lapply(targets$cohort_definition_id, function(targetCohortId) {
    targetCohortDef <- targets %>% filter(cohort_definition_id == targetCohortId)
    return (CohortIncidence::createCohortRef(id=targetCohortDef$cohort_definition_id, name=targetCohortDef$cohort_definition_name))
  })

  #append general pop def
  targetDefs <- append(targetDefs, genPopTargetDef)

  # the general pop analysis derives Os from T's so we need to make all outcome defs contain both T and O
  outcomeDefs <- lapply(unique(c(targets$id, outcomes$id)), function(outcomeId) {
    outcomeDef <- outcomeMap %>% filter(id == outcomeId)
    return (CohortIncidence::createOutcomeDef(id = outcomeDef$id,
                                              name = outcomeDef$cohort_definition_name,
                                              cohortId = outcomeDef$cohort_definition_id,
                                              cleanWindow = outcomeDef$clean_window))
  })


  irDesign <- CohortIncidence::createIncidenceDesign(targetDefs = targetDefs,
                                                     outcomeDefs = outcomeDefs,
                                                     tars=tars,
                                                     analysisList = analysisList,
                                                     strataSettings = CohortIncidence::createStrataSettings(byGender=T, byYear=T, byAge=T, ageBreaks = c(3,13,18,30,40,50,60,70,80,90)))

  cohortIncidenceModuleSpecifications <- createCohortIncidenceModuleSpecifications(
    irDesign = irDesign$toList()
  )

  .createAnalysisSpecification(strategusFileName, cohortDefinitionShared, cohortGeneratorModuleSpecifications, cohortIncidenceModuleSpecifications)

})(andreasFileList, andreasFileName)


# Joel analysis IIFE
(function(fileList, strategusFileName) {
  cohortIds <- c(genPopTargetDef$id, .getUniqueCohortIds(fileList))
  cohortDefinitionSet <- PhenotypeLibrary::getPlCohortDefinitionSet(cohortIds)

  cohortDefinitionShared <- createCohortSharedResourceSpecifications(cohortDefinitionSet)

  targets <-data.frame()
  outcomes<- data.frame()
  analysisList <- list()

  for (file in fileList) {
    targetXls <- readxl::read_xlsx(file, sheet="targets")
    targets <- rbind(targets, targetXls)

    outcomeXls <- readxl::read_xlsx(file, sheet="outcomes") %>%
      dplyr::inner_join(outcomeMap %>% select("id", "cohort_definition_id", "clean_window"), by=c('cohort_definition_id', 'clean_window'))
    outcomes<-rbind(outcomes, outcomeXls)

    analysisList <- append(analysisList,
                           CohortIncidence::createIncidenceAnalysis(targets = targetXls$cohort_definition_id,
                                                                    outcomes = outcomeXls$id,
                                                                    tars = c(2,3)))
  };

  # to save time, we'll attach a clean_window = 0 and lookup the outcome ID here, and use it for the background analysis
  targets <- targets %>% distinct() %>% mutate(clean_window = 0) %>%
    dplyr::inner_join(outcomeMap %>% select("id", "cohort_definition_id", "clean_window"), by=c('cohort_definition_id', 'clean_window'))

  outcomes <- outcomes %>% distinct()

  # make the background rate for all T and Os
  analysisList <- append(analysisList,
                         CohortIncidence::createIncidenceAnalysis(targets = genPopTargetDef$id,
                                                                  outcomes = c(targets$id, outcomes$id),
                                                                  tars = c(2)))


  targetDefs <- lapply(targets$cohort_definition_id, function(targetCohortId) {
    targetCohortDef <- targets %>% filter(cohort_definition_id == targetCohortId)
    return (CohortIncidence::createCohortRef(id=targetCohortDef$cohort_definition_id, name=targetCohortDef$cohort_definition_name))
  })

  #append general pop def
  targetDefs <- append(targetDefs, genPopTargetDef)

  # the general pop analysis derives Os from T's so we need to make all outcome defs contain both T and O
  outcomeDefs <- lapply(unique(c(targets$id, outcomes$id)), function(outcomeId) {
    outcomeDef <- outcomeMap %>% filter(id == outcomeId)
    return (CohortIncidence::createOutcomeDef(id = outcomeDef$id,
                                              name = outcomeDef$cohort_definition_name,
                                              cohortId = outcomeDef$cohort_definition_id,
                                              cleanWindow = outcomeDef$clean_window))
  })


  irDesign <- CohortIncidence::createIncidenceDesign(targetDefs = targetDefs,
                                                     outcomeDefs = outcomeDefs,
                                                     tars=tars,
                                                     analysisList = analysisList,
                                                     strataSettings = CohortIncidence::createStrataSettings(byGender=T, byYear=T, byAge=T, ageBreaks = c(3,13,18,30,40,50,60,70,80,90)))

  cohortIncidenceModuleSpecifications <- createCohortIncidenceModuleSpecifications(
    irDesign = irDesign$toList()
  )

  .createAnalysisSpecification(strategusFileName, cohortDefinitionShared, cohortGeneratorModuleSpecifications, cohortIncidenceModuleSpecifications)

})(joelFileList, joelFileName)

# Evan analysis IIFE
(function(fileList, strategusFileName) {
  cohortIds <- c(genPopTargetDef$id, .getUniqueCohortIds(fileList))
  cohortDefinitionSet <- PhenotypeLibrary::getPlCohortDefinitionSet(cohortIds)

  cohortDefinitionShared <- createCohortSharedResourceSpecifications(cohortDefinitionSet)

  targets <-data.frame()
  outcomes<- data.frame()
  analysisList <- list()

  for (file in fileList) {
    targetXls <- readxl::read_xlsx(file, sheet="targets")
    targets <- rbind(targets, targetXls)

    outcomeXls <- readxl::read_xlsx(file, sheet="outcomes") %>%
      dplyr::inner_join(outcomeMap %>% select("id", "cohort_definition_id", "clean_window"), by=c('cohort_definition_id', 'clean_window'))
    outcomes<-rbind(outcomes, outcomeXls)

    if (any(endsWith(file,c("analysis2_evan_1.xlsx")))) {
      analysisList <- append(analysisList,
                             CohortIncidence::createIncidenceAnalysis(targets = targetXls$cohort_definition_id,
                                                                      outcomes = outcomeXls$id,
                                                                      tars = c(5,1,6,2)))
    } else {
      analysisList <- append(analysisList,
                             CohortIncidence::createIncidenceAnalysis(targets = targetXls$cohort_definition_id,
                                                                      outcomes = outcomeXls$id,
                                                                      tars = c(1,7,8)))
    }
  };

  # to save time, we'll attach a clean_window = 0 and lookup the outcome ID here, and use it for the background analysis
  targets <- targets %>% distinct() %>% mutate(clean_window = 0) %>%
    dplyr::inner_join(outcomeMap %>% select("id", "cohort_definition_id", "clean_window"), by=c('cohort_definition_id', 'clean_window'))

  outcomes <- outcomes %>% distinct()

  # make the background rate for all T and Os
  analysisList <- append(analysisList,
                         CohortIncidence::createIncidenceAnalysis(targets = genPopTargetDef$id,
                                                                  outcomes = c(targets$id, outcomes$id),
                                                                  tars = c(2)))


  targetDefs <- lapply(targets$cohort_definition_id, function(targetCohortId) {
    targetCohortDef <- targets %>% filter(cohort_definition_id == targetCohortId)
    return (CohortIncidence::createCohortRef(id=targetCohortDef$cohort_definition_id, name=targetCohortDef$cohort_definition_name))
  })

  #append general pop def
  targetDefs <- append(targetDefs, genPopTargetDef)

  # the general pop analysis derives Os from T's so we need to make all outcome defs contain both T and O
  outcomeDefs <- lapply(unique(c(targets$id, outcomes$id)), function(outcomeId) {
    outcomeDef <- outcomeMap %>% filter(id == outcomeId)
    return (CohortIncidence::createOutcomeDef(id = outcomeDef$id,
                                              name = outcomeDef$cohort_definition_name,
                                              cohortId = outcomeDef$cohort_definition_id,
                                              cleanWindow = outcomeDef$clean_window))
  })


  irDesign <- CohortIncidence::createIncidenceDesign(targetDefs = targetDefs,
                                                     outcomeDefs = outcomeDefs,
                                                     tars=tars,
                                                     analysisList = analysisList,
                                                     strataSettings = CohortIncidence::createStrataSettings(byGender=T, byYear=T, byAge=T, ageBreaks = c(3,13,18,30,40,50,60,70,80,90)))

  cohortIncidenceModuleSpecifications <- createCohortIncidenceModuleSpecifications(
    irDesign = irDesign$toList()
  )

  .createAnalysisSpecification(strategusFileName, cohortDefinitionShared, cohortGeneratorModuleSpecifications, cohortIncidenceModuleSpecifications)

})(evanFileList, evanFileName)

# Gowza analysis IIFE
(function(fileList, strategusFileName) {
  cohortIds <- c(genPopTargetDef$id, .getUniqueCohortIds(fileList))
  cohortDefinitionSet <- PhenotypeLibrary::getPlCohortDefinitionSet(cohortIds)

  cohortDefinitionShared <- createCohortSharedResourceSpecifications(cohortDefinitionSet)

  targets <-data.frame()
  outcomes<- data.frame()
  analysisList <- list()

  for (file in fileList) {
    targetXls <- readxl::read_xlsx(file, sheet="targets")
    targets <- rbind(targets, targetXls)

    outcomeXls <- readxl::read_xlsx(file, sheet="outcomes") %>%
      dplyr::inner_join(outcomeMap %>% select("id", "cohort_definition_id", "clean_window"), by=c('cohort_definition_id', 'clean_window'))
    outcomes<-rbind(outcomes, outcomeXls)

    analysisList <- append(analysisList,
                           CohortIncidence::createIncidenceAnalysis(targets = targetXls$cohort_definition_id,
                                                                    outcomes = outcomeXls$id,
                                                                    tars = c(2)))
  };

  # to save time, we'll attach a clean_window = 0 and lookup the outcome ID here, and use it for the background analysis
  targets <- targets %>% distinct() %>% mutate(clean_window = 0) %>%
    dplyr::inner_join(outcomeMap %>% select("id", "cohort_definition_id", "clean_window"), by=c('cohort_definition_id', 'clean_window'))

  outcomes <- outcomes %>% distinct()

  # make the background rate for all T and Os
  analysisList <- append(analysisList,
                         CohortIncidence::createIncidenceAnalysis(targets = genPopTargetDef$id,
                                                                  outcomes = c(targets$id, outcomes$id),
                                                                  tars = c(2)))


  targetDefs <- lapply(targets$cohort_definition_id, function(targetCohortId) {
    targetCohortDef <- targets %>% filter(cohort_definition_id == targetCohortId)
    return (CohortIncidence::createCohortRef(id=targetCohortDef$cohort_definition_id, name=targetCohortDef$cohort_definition_name))
  })

  #append general pop def
  targetDefs <- append(targetDefs, genPopTargetDef)

  # the general pop analysis derives Os from T's so we need to make all outcome defs contain both T and O
  outcomeDefs <- lapply(unique(c(targets$id, outcomes$id)), function(outcomeId) {
    outcomeDef <- outcomeMap %>% filter(id == outcomeId)
    return (CohortIncidence::createOutcomeDef(id = outcomeDef$id,
                                              name = outcomeDef$cohort_definition_name,
                                              cohortId = outcomeDef$cohort_definition_id,
                                              cleanWindow = outcomeDef$clean_window))
  })


  irDesign <- CohortIncidence::createIncidenceDesign(targetDefs = targetDefs,
                                                     outcomeDefs = outcomeDefs,
                                                     tars=tars,
                                                     analysisList = analysisList,
                                                     strataSettings = CohortIncidence::createStrataSettings(byGender=T, byYear=T, byAge=T, ageBreaks = c(3,13,18,30,40,50,60,70,80,90)))

  cohortIncidenceModuleSpecifications <- createCohortIncidenceModuleSpecifications(
    irDesign = irDesign$toList()
  )

  .createAnalysisSpecification(strategusFileName, cohortDefinitionShared, cohortGeneratorModuleSpecifications, cohortIncidenceModuleSpecifications)

})(gowzaFileList, gowzaFileName)

# General Population analysis IIFE
(function(fileList, strategusFileName) {
  cohortIds <- c(genPopTargetDef$id, .getUniqueCohortIds(fileList))
  cohortDefinitionSet <- PhenotypeLibrary::getPlCohortDefinitionSet(cohortIds)

  cohortDefinitionShared <- createCohortSharedResourceSpecifications(cohortDefinitionSet)

  targets <-data.frame()
  outcomes<- data.frame()
  analysisList <- list()

  for (file in fileList) {
    targetXls <- readxl::read_xlsx(file, sheet="targets")
    targets <- rbind(targets, targetXls)

    outcomeXls <- readxl::read_xlsx(file, sheet="outcomes") %>%
      dplyr::inner_join(outcomeMap %>% select("id", "cohort_definition_id", "clean_window"), by=c('cohort_definition_id', 'clean_window'))
    outcomes<-rbind(outcomes, outcomeXls)

    analysisList <- append(analysisList,
                           CohortIncidence::createIncidenceAnalysis(targets = targetXls$cohort_definition_id,
                                                                    outcomes = outcomeXls$id,
                                                                    tars = c(2)))
  };

  # to save time, we'll attach a clean_window = 0 and lookup the outcome ID here, and use it for the background analysis
  targets <- targets %>% distinct()

  outcomes <- outcomes %>% distinct()

  targetDefs <- lapply(targets$cohort_definition_id, function(targetCohortId) {
    targetCohortDef <- targets %>% filter(cohort_definition_id == targetCohortId)
    return (CohortIncidence::createCohortRef(id=targetCohortDef$cohort_definition_id, name=targetCohortDef$cohort_definition_name))
  })

  # the general pop analysis derives Os from T's so we need to make all outcome defs contain both T and O
  outcomeDefs <- lapply(unique(outcomes$id), function(outcomeId) {
    outcomeDef <- outcomeMap %>% filter(id == outcomeId)
    return (CohortIncidence::createOutcomeDef(id = outcomeDef$id,
                                              name = outcomeDef$cohort_definition_name,
                                              cohortId = outcomeDef$cohort_definition_id,
                                              cleanWindow = outcomeDef$clean_window))
  })


  irDesign <- CohortIncidence::createIncidenceDesign(targetDefs = targetDefs,
                                                     outcomeDefs = outcomeDefs,
                                                     tars=tars,
                                                     analysisList = analysisList,
                                                     strataSettings = CohortIncidence::createStrataSettings(byGender=T, byYear=T, byAge=T, ageBreaks = c(3,13,18,30,40,50,60,70,80,90)))

  cohortIncidenceModuleSpecifications <- createCohortIncidenceModuleSpecifications(
    irDesign = irDesign$toList()
  )

  .createAnalysisSpecification(strategusFileName, cohortDefinitionShared, cohortGeneratorModuleSpecifications, cohortIncidenceModuleSpecifications)

})(overallFileList, overallFileName)

# George analysis IIFE
(function(fileList, strategusFileName) {
  cohortIds <- c(genPopTargetDef$id, .getUniqueCohortIds(fileList))
  cohortDefinitionSet <- PhenotypeLibrary::getPlCohortDefinitionSet(cohortIds)

  cohortDefinitionShared <- createCohortSharedResourceSpecifications(cohortDefinitionSet)

  targets <-data.frame()
  outcomes<- data.frame()
  analysisList <- list()

  for (file in fileList) {
    targetXls <- readxl::read_xlsx(file, sheet="targets")
    targets <- rbind(targets, targetXls)

    outcomeXls <- readxl::read_xlsx(file, sheet="outcomes") %>%
      dplyr::inner_join(outcomeMap %>% select("id", "cohort_definition_id", "clean_window"), by=c('cohort_definition_id', 'clean_window'))
    outcomes<-rbind(outcomes, outcomeXls)

    # due to benchmarking, we will execute 1 T at a time
    for (i in 1:nrow(targetXls)){
      targetRow <- targetXls[i,]
      analysisList <- append(analysisList,
                             CohortIncidence::createIncidenceAnalysis(targets = targetRow$cohort_definition_id,
                                                                      outcomes = outcomeXls$id,
                                                                      tars = c(1,2,3,4))
      )
    }

  };

  # to save time, we'll attach a clean_window = 0 and lookup the outcome ID here, and use it for the background analysis
  targets <- targets %>% distinct() %>% mutate(clean_window = 0) %>%
    dplyr::inner_join(outcomeMap %>% select("id", "cohort_definition_id", "clean_window"), by=c('cohort_definition_id', 'clean_window'))

  outcomes <- outcomes %>% distinct()

  # make the background rate for all T and Os
  analysisList <- append(analysisList,
                         CohortIncidence::createIncidenceAnalysis(targets = genPopTargetDef$id,
                                                                  outcomes = c(targets$id, outcomes$id),
                                                                  tars = c(2)))


  targetDefs <- lapply(targets$cohort_definition_id, function(targetCohortId) {
    targetCohortDef <- targets %>% filter(cohort_definition_id == targetCohortId)
    return (CohortIncidence::createCohortRef(id=targetCohortDef$cohort_definition_id, name=targetCohortDef$cohort_definition_name))
  })

  #append general pop def
  targetDefs <- append(targetDefs, genPopTargetDef)

  # the general pop analysis derives Os from T's so we need to make all outcome defs contain both T and O
  outcomeDefs <- lapply(unique(c(targets$id, outcomes$id)), function(outcomeId) {
    outcomeDef <- outcomeMap %>% filter(id == outcomeId)
    return (CohortIncidence::createOutcomeDef(id = outcomeDef$id,
                                              name = outcomeDef$cohort_definition_name,
                                              cohortId = outcomeDef$cohort_definition_id,
                                              cleanWindow = outcomeDef$clean_window))
  })


  irDesign <- CohortIncidence::createIncidenceDesign(targetDefs = targetDefs,
                                                     outcomeDefs = outcomeDefs,
                                                     tars=tars,
                                                     analysisList = analysisList,
                                                     strataSettings = CohortIncidence::createStrataSettings(byGender=T, byYear=T, byAge=T, ageBreaks = c(3,13,18,30,40,50,60,70,80,90)))

  cohortIncidenceModuleSpecifications <- createCohortIncidenceModuleSpecifications(
    irDesign = irDesign$toList()
  )

  .createAnalysisSpecification(strategusFileName, cohortDefinitionShared, cohortGeneratorModuleSpecifications, cohortIncidenceModuleSpecifications)

})(georgeFileList, georgeFileName)

