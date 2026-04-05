library(irace)
# ----------------------------------------------------------------------
# Definition of the reference values function according to the 
# More–Garbow–Hillstrom paper, for the functional values that will 
# be considered in the cost measure.
# ----------------------------------------------------------------------

f_ref <- c(0, 0, 1.127933E-8, 0, 0, 0, 2.28767E-03, 2.24997E-05, 9.376293E-06, 0.00,
           85822.2, 0, 0, 0, 0, 0, 0, 0.003516874)
f_tol <- 1e-8
# ----------------------------------------------------------------------
# Definition of the evaluation function. It sets the path to the
# executable for running the internal algorithm (GENCAN), selects
# an internal optimization method, and specifies the problem instance.
# ----------------------------------------------------------------------

evaluate_gencan <- function(problem_id, configuration) {
  trash <- tempfile()
  sink(trash)
  on.exit({ try(sink(), silent=TRUE); unlink(trash) }, add = TRUE)
  
  exe <- "/home/user/Descargas/algencan-3.1.1/CODES_DISERTATION/Experiment2/gencan"
  method <- as.character(configuration$method)
  pid <- as.numeric(problem_id) 
  result <- system2(exe, args = c(pid, method), stdout = TRUE, stderr = trash)
  
  idx_fbest <- grep("^\\s*fbest\\s*=", result, ignore.case = TRUE)
  
  fbest_line <- result[idx_fbest[1]]

  fbest <- as.numeric(sub("(?i)^\\s*fbest\\s*=\\s*", "", fbest_line, perl = TRUE))
  
  iteration_lines <- grep("^\\s*\\d+", result)
  if (length(iteration_lines) > 0) {
    last_iter_line <- result[iteration_lines[length(iteration_lines)]]
    fcnt_match <- regmatches(last_iter_line, regexpr("\\d+\\s*$", last_iter_line))
    fcnt <- as.numeric(fcnt_match)
  }
  
  
  # ----------------------------------------------------------------------
  #Definition of the Cost Measure:
  # ----------------------------------------------------------------------

  ref_value <- f_ref[pid]
  threshold <- ref_value + f_tol * max(1, abs(ref_value))
  
  if (fbest <= threshold) {
    cost <- fcnt / (fcnt + 1)
  } else {
    dist <- abs(fbest - ref_value)/(max(1, abs(ref_value)))
    cost <- 1 + dist / (dist + 1)
  }
  
  return(list(cost = cost))
}

# ----------------------------------------------------------------------
# Definition of the parameter space X, from which configurations will be 
# sampled.
# ----------------------------------------------------------------------
parameters <- readParameters(text = '
method "" c (newton, tn, tr)
')
# ----------------------------------------------------------------------
# Definition of the Runner function, which is responsible for executing
# the internal algorithm within the Irace Scenario.
# ----------------------------------------------------------------------
runner <- function(experiment, scenario) {
  evaluate_gencan(experiment$instance, experiment$configuration)
}

# ----------------------------------------------------------------------
# Here we define the problem instances considered in the training phase,
# taken from the More–Garbow–Hillstrom problem set.
# ----------------------------------------------------------------------
trainInstances <- as.character(1:18)
#testInstances <- as.character(1:18)

# ----------------------------------------------------------------------
# Here we define the Irace Scenario, which specifies the conditions
# under which the internal algorithm (GENCAN) will be executed. 
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



set.seed(123456)
irace.output <- irace::irace(scenario)
