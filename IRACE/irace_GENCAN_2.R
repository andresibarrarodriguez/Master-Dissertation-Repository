library(irace)

# ----------------------------------------------------------------------
# 
# ----------------------------------------------------------------------
evaluate_gencan <- function(problem_id, configuration) {
  trash <- tempfile()
  sink(trash)
  on.exit({ try(sink(), silent=TRUE); unlink(trash) }, add = TRUE)
  
  exe <- "/home/user/Descargas/algencan-3.1.1/minf"
  method <- as.character(configuration$method)
  pid <- as.numeric(problem_id)
  
  
  result <- system2(exe, args = c(pid, method), stdout = TRUE, stderr = trash)
  
  
  idx_fbest <- grep("^\\s*fbest\\s*=", result, ignore.case = TRUE)
  
  
  # Extract the first occurrence of fbest pattern.
  fbest_line <- result[idx_fbest[1]]

  fbest <- as.numeric(sub("(?i)^\\s*fbest\\s*=\\s*", "", fbest_line, perl = TRUE))
  
  iteration_lines <- grep("^\\s*\\d+", result)
  if (length(iteration_lines) > 0) {
    last_iter_line <- result[iteration_lines[length(iteration_lines)]]
    fcnt_match <- regmatches(last_iter_line, regexpr("\\d+\\s*$", last_iter_line))
    fcnt <- as.numeric(fcnt_match)
  } 

  fbest_int <- round(fbest * 1e8)
  cost <- fbest_int + fcnt / (fcnt + 1)
  
  return(list(cost = cost))
}
# ----------------------------------------------------------------------
# 
# ----------------------------------------------------------------------
parameters <- readParameters(text = '
method "" c (newton, tn, tr)
')
# ----------------------------------------------------------------------
# 
# ----------------------------------------------------------------------

runner <- function(experiment, scenario) {
  evaluate_gencan(experiment$instance, experiment$configuration)
}

# ----------------------------------------------------------------------
# 
# ----------------------------------------------------------------------

trainInstances <- as.character(1:18)
testInstances <- as.character(1:18)

# ----------------------------------------------------------------------
# 
# ----------------------------------------------------------------------

scenario <- list(
  targetRunner = runner,
  instances = trainInstances, 
  testInstances = testInstances,
  maxExperiments = 1000,
  logFile = "irace-GENCAN.Rdata",  
  parameters = parameters,
  debugLevel = 2,
  parallel = 1,
  deterministic = TRUE,
  testNbElites = 2, 
  elitistNewInstances = 5,
  firstTest = 10,
  elitist = 1,       
  testIterationElites = TRUE,
  minNbSurvival = 2,
  postselection = FALSE 
)

# ----------------------------------------------------------------------
# 
# ----------------------------------------------------------------------
set.seed(123456)
irace.output <- irace::irace(scenario)
