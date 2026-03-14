library(irace)

# ============================================================================
# INITIALIZATION: Clean previous execution logs
# ============================================================================
if (file.exists("runner.log")) {
  file.remove("runner.log")
}

# ============================================================================
# REFERENCE VALUES: Benchmark solutions for success criteria
# ============================================================================
# Reference values (f_ref) represent the best known objective function values
# for each of the 18 test problems, as reported in the literature.
# The tolerance f_tol is used to establish convergence thresholds.

f_ref <- c(0, 0, 1.127933E-8, 0, 0, 0, 2.28767E-03, 2.24997E-05, 9.376293E-06, 0.00,
           85822.2, 0, 0, 0, 0, 0, 0, 0.003516874)
f_tol <- 1e-8


# ============================================================================
# EVALUATION FUNCTION: F(θ,I) 
# ============================================================================
# This function implements the evaluation mechanism F(θ,I) where θ∈Θ represents 
# a configuration and I∈I represents an instance problem. It executes the GENCAN
# algorithm through the minf executable, which integrates subroutines_GENCAN.f90, 
# conf_GENCAN.f90, and external modules src_unc.f and src_hes.f. The function
# evaluates optimization methods for all 18 unconstrained minimization problems.

evaluate_gencan <- function(problem_id, configuration) {
  trash <- tempfile()
  sink(trash)
  on.exit({ try(sink(), silent=TRUE); unlink(trash) }, add = TRUE)
  
  exe <- "/home/user/Descargas/algencan-3.1.1/minf"
  method <- as.character(configuration$method)
  pid <- as.numeric(problem_id)
  

  # ----------------------------------------------------------------------------
  # EXECUTABLE INVOCATION: System call to GENCAN solver
  # ----------------------------------------------------------------------------
  # Execute the GENCAN algorithm using system2() with the specified executable path,
  # passing problem ID and optimization method as command-line arguments.
  
  
  result <- system2(exe, args = c(pid, method), stdout = TRUE, stderr = trash)
  
  # ----------------------------------------------------------------------------
  # OBJECTIVE FUNCTION VALUE EXTRACTION: Parse fbest from GENCAN output
  # ----------------------------------------------------------------------------
  # Locate the line containing the final objective function value using pattern matching.
  # Pattern "^\\s*fbest\\s*=" matches: line start (^), optional spaces (\\s*), 
  # keyword 'fbest', optional spaces, and equals sign.
  # returns indices of matching lines in the output vector.
  
  idx_fbest <- grep("^\\s*fbest\\s*=", result, ignore.case = TRUE)
  
  
  # Extract the first occurrence of fbest pattern.
  fbest_line <- result[idx_fbest[1]]
  
  
  # Parse the numerical value from the fbest line using regular expression substitution.
  # The pattern removes the "fbest = " prefix to isolate the numerical value.

  fbest <- as.numeric(sub("(?i)^\\s*fbest\\s*=\\s*", "", fbest_line, perl = TRUE))
  
  # ----------------------------------------------------------------------------
  # FUNCTION EVALUATIONS EXTRACTION: Parse fcnt from iteration table
  # ----------------------------------------------------------------------------
  # Extract the number of function evaluations (fcnt) from GENCAN's iteration table.
  # Search for lines beginning with digits to identify iteration entries, then
  # extract the final column value (fcnt) from the last iteration row.
  
  iteration_lines <- grep("^\\s*\\d+", result)
  if (length(iteration_lines) > 0) {
    last_iter_line <- result[iteration_lines[length(iteration_lines)]]
    fcnt_match <- regmatches(last_iter_line, regexpr("\\d+\\s*$", last_iter_line))
    fcnt <- as.numeric(fcnt_match)
  } else {
    fcnt <- 1
  }
  
  # ----------------------------------------------------------------------------
  # COST METRIC COMPUTATION: d: Θ×I → ℝ
  # ----------------------------------------------------------------------------
  # Implement binary cost metric based on convergence threshold:
  # - Success: d(θ,I) = fcnt/(fcnt+1) 
  # - Failure: d(θ,I) = 1 + dist/(dist+1) 
  ref_value <- f_ref[pid]
  threshold <- ref_value + f_tol * max(1, abs(ref_value))
  
  
  # Evaluate method performance against established convergence criteria.
  # Successful configurations are further ranked by computational efficiency (FCNT).
  if (fbest <= threshold) {
    cost <- fcnt / (fcnt + 1)
  } else {
    dist <- abs(fbest - ref_value)/(max(1, abs(ref_value)))
    cost <- 1 + dist / (dist + 1)
  }
  
   # ----------------------------------------------------------------------------
  # EXECUTION LOGGING: Detailed performance metrics
  # ----------------------------------------------------------------------------
  # Log comprehensive execution details including problem ID, method, objective value,
  # function evaluations, convergence threshold, success status, and computed cost.

  cat(sprintf("Problem=%d, method=%s, fbest=%.8e, fcnt=%d, threshold=%.8e, success=%s, cost=%.8f\n",
              pid, method, fbest, fcnt, threshold, ifelse(fbest <= threshold, "YES", "NO"), cost),
      file = "runner.log", append = TRUE)
  
  
  return(list(cost = cost))
}

# ============================================================================
# PARAMETER SPACE DEFINITION: Configuration domain Θ
# ============================================================================
# Define the optimization method parameter space containing the three internal
# solvers available in GENCAN: Newton Line Search, Truncated Newton Line Search, and Newton with Trust Regions.


parameters <- readParameters(text = '
method "" c (newton, tn, tr)
')

# ============================================================================
# TARGET RUNNER: irace interface for Algorithm execution
# ============================================================================
# Implements the target runner function required by irace scenario.
# This function bridges irace's configuration sampling with our evaluation mechanism,
# executing experiments for specific (configuration, instance) pairs.

runner <- function(experiment, scenario) {
  evaluate_gencan(experiment$instance, experiment$configuration)
}

# ============================================================================
# INSTANCE DEFINITION: Training and testing problem sets
# ============================================================================
# Define the complete set of 18 unconstrained optimization problems as both
# training and testing instances.

trainInstances <- as.character(1:18)
testInstances <- as.character(1:18)

# ============================================================================
# SCENARIO CONFIGURATION: 
# ============================================================================
# Configure the complete irace scenario including target runner, instance sets,
# computational budget, parameter space, parallel execution settings, and
# elitist racing.

scenario <- list(
  targetRunner = runner,
  instances = trainInstances, 
  testInstances = testInstances,
  maxExperiments = 30000,
  logFile = "irace-GENCAN.Rdata",  
  parameters = parameters,
  debugLevel = 0,
  parallel = 4,
  deterministic = TRUE,
  testNbElites = 2, 
  elitist = 1,       
  testIterationElites = TRUE,
  minNbSurvival = 1,
  postselection = FALSE,  
  maxTime = FALSE 
)

# ============================================================================
# IRACE EXECUTION: Automatic algorithm configuration
# ============================================================================
# Execute the irace framework with deterministic seeding for reproducibility

set.seed(123456)
irace.output <- irace::irace(scenario)

# ============================================================================
# RESULTS ANALYSIS: Testing phase evaluation
# ============================================================================
if (file.exists("irace-GENCAN.Rdata")) {
  load("irace-GENCAN.Rdata")
  if (exists("iraceResults")) {
    if (!is.null(iraceResults$testing)) {
      print(iraceResults$testing)
    } else {
      testResults <- suppressMessages(suppressWarnings(
        testing_fromlog(logFile = "irace-GENCAN.Rdata",
                        testNbElites = scenario$testNbElites,
                        testIterationElites = scenario$testIterationElites)
      ))
    }
  }  
  cat("")
}
# if (file.exists("irace-GENCAN.Rdata")) {  
  # load("irace-GENCAN.Rdata")
  # if (exists("iraceResults")) {
    # if (!is.null(iraceResults$testing)) {
      # print(iraceResults$testing)
    # }
  # }
# }
