library(irace)

# ----------------------------------------------------------------------
# Definition of the function that returns the area
# according to the size of the box.
# ----------------------------------------------------------------------
get_area <- function(box_type) {
  if (box_type == 1) return(4)   # When T=2,
  if (box_type == 2) return(36)   # When T=6,
  if (box_type == 3) return(100)   # When T=10.
}

get_volume <- function(box_type) {
  if (box_type == 1) return(8)    # T=2
  if (box_type == 2) return(216)  # T=6
  if (box_type == 3) return(1000) # T=10
}

get_hypervolume <- function(box_type) {
  if (box_type == 1) return(16)     # T=2, 
  if (box_type == 2) return(1296)   # T=6, 
  if (box_type == 3) return(10000)  # T=10.
}

# ----------------------------------------------------------------------
# Definition of the evaluation function. It sets the path to the
# executable, defines the instances used for training, and selects
# the appropriate measure (area, volume, or hypervolume) according
# to the problem dimension.
# ----------------------------------------------------------------------
evaluate_montecarlo <- function(problem_id, configuration) {
  trash <- tempfile()
  sink(trash)
  on.exit({ try(sink(), silent=TRUE); unlink(trash) }, add = TRUE)
  exe <- "/home/user/Descargas/algencan-3.1.1/CODES_DISERTATION/Experiment1/montecarlo"
  densidad <- as.numeric(configuration$densidad)
  box_type <- as.numeric(configuration$box_type)
  dim2_instances <- c(4, 10, 16)
  dim3_instances <- c(1, 3, 5, 12)
  dim4_instances <- c(17, 11, 8, 9)
  pid <- as.numeric(problem_id)
  if (pid %in% dim2_instances) {
    medida <- get_area(box_type)
  } else if (pid %in% dim3_instances) {
    medida <- get_volume(box_type)
  } else if (pid %in% dim4_instances) {
    medida <- get_hypervolume(box_type)
  }  else {
    medida <- 1
  }
  
  # ----------------------------------------------------------------------
  # Definition of the number of evaluations based on the point density
  # and the selected measure (area, volume, or hypervolume).
  # It also defines how the cost value is obtained.
  # ----------------------------------------------------------------------
  numeval <- as.integer(densidad * medida)
  output <- system2(
    exe,
    args = c(problem_id, box_type, numeval),
    stdout = TRUE, stderr = trash
  )
  
  fbest_line <- output[1]
  cost <- as.numeric(sub("fbest =\\s*", "", fbest_line))
  return(list(cost = cost))
}
# ----------------------------------------------------------------------
# Definition of the parameter space X, from which configurations will be 
# sampled.
# ----------------------------------------------------------------------
parameters <- readParameters(text = '
densidad "" c (1, 10, 100, 1000)
box_type "" c (1,2,3)
')
# ----------------------------------------------------------------------
# Definition of the runner function, which is responsible for executing
# the internal algorithm within the irace scenario.
# ----------------------------------------------------------------------
runner <- function(experiment, scenario) {
  C <- experiment$configuration  
  result <- evaluate_montecarlo(experiment$instance, C)
  return(result)
}
# ----------------------------------------------------------------------
# Here we define the problem instances considered in the training phase,
# taken from the More–Garbow–Hillstrom problem set.
# ----------------------------------------------------------------------
trainInstances <- as.character(c(1, 3, 4,  5, 8, 9,10, 11, 12, 17, 16))
#testInstances  <- as.character(c(1, 3, 4,  5, 8, 9,10, 11, 12, 17, 16))
# ----------------------------------------------------------------------
# Here we define the irace scenario, which specifies the conditions
# under which the internal algorithm will be executed. 
# ----------------------------------------------------------------------
scenario <- list(
  targetRunner = runner,
  instances = trainInstances, 
  testInstances = testInstances,
  maxExperiments = 200,
  logFile = "irace-results.Rdata",  
  parameters = parameters,
  debugLevel = 0,
  deterministic = TRUE,
  testNbElites = 1, 
  elitist = 1,       
  testIterationElites = TRUE,
  minNbSurvival = 1,
  postselection = FALSE,  
  maxTime = FALSE 
)
set.seed(123456)

irace.output <- irace::irace(scenario)
