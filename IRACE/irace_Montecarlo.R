library(irace)

#  Everytime new run the of irace script We clean the log output.
if (file.exists("runner.log")) {
  file.remove("runner.log")
}

# Define the function to get the Area (Case the dimension is 2), which will be applied to the differents set \Omega_i.
get_area <- function(box_type) {
  if (box_type == 1) return(4)   # When T=2,
  if (box_type == 2) return(36)   # When T=6,
  if (box_type == 3) return(100)   # When T=10.
  stop("box_type unkown")
}

# Analogous with above, we define the function to get the Volume (Case the dimension is 3), and it will be applied to the differents set \Omega_i.
get_volume <- function(box_type) {
  if (box_type == 1) return(8)    # T=2
  if (box_type == 2) return(216)  # T=6
  if (box_type == 3) return(1000) # T=10
  stop("box_type unkown")
}

# For the highest dimension we define the tuning problem R4,  we define the function to get the Volume (Case the dimension is 3), 
#and it will be applied to the differents set \Omega_i.
get_hypervolume <- function(box_type) {
  if (box_type == 1) return(16)     # T=2, 
  if (box_type == 2) return(1296)   # T=6, 
  if (box_type == 3) return(10000)  # T=10.
  stop("box_type unkown")
}

# Now we define the evaluation function F(\theta, I), wich will take a configuration \theta\in \Theta, an a instance problem $I\in \mathcal{I}$, and 
#it will be executing our main algorithm montecarlo_search.f90. We define the rute of the executable file, and after that we define the paremeters we want to tune for the 
#algorithm, as mention in the Disertation we are interested in tuning the parameters asociated to, Density of points and to the type of Box. Then we define the instances 
#Problems we wil be using to train our target algorithm. For computational cost We used in total 11 functions which are asociated to the function wich dimension in R2, R3, R4 respectively.
#then we define an identificator to each problem to be used in the tuning process, and depending on the set that belong it will define a different measure, (area, volumen, hypervolume).
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
  
  #In this part we define, how the number of iterations will be computed for our main algorith according to the parameters defined before. Then we use the function system2, 
  #to invoke the principal program, note that this function recives as arguments exe, witch contains the rute of main program, and arg wich it will be iterating throught the 
  #tripletes, problem, box type and number of iterations.
  numeval <- as.integer(densidad * medida)
  output <- system2(
    exe,
    args = c(problem_id, box_type, numeval),
    stdout = TRUE, stderr = trash
  )
  
  # Once executed the program, we took the functional value results and we append to runner log file.
  fbest_line <- output[1]
  cost <- as.numeric(sub("fbest =\\s*", "", fbest_line))
  cat(sprintf("Resultado: instancia=%s, numeval=%s, box_type=%s, cost=%f\n",
              problem_id, numeval, box_type, cost),
      file = "runner.log", append = TRUE)
  return(list(cost = cost))
}

# Here we define the set of parameters, that is all the possible value that the two parameters can take.
parameters <- readParameters(text = '
densidad "" c (1, 10, 100, 1000)
box_type "" c (1,2,3)
')

# In this part we define the Target Runner function, which is especific concept of irace, this function is responsable to excute our main program in a particular scenario configuration
# that will be defined, and this function will execute the main algorithm using the scenario define, and it wil make an experiment with an instance using an especific configuration.
runner <- function(experiment, scenario) {
  C <- experiment$configuration  
  result <- evaluate_montecarlo(experiment$instance, C)
  return(result)
}

# In this part we define the instances that will serve as an training instance and test instances, taking in account that we have few instances we train will the whole set
# and once the tuning is done we also test with the whole set.
trainInstances <- as.character(c(1, 3, 4,  5, 8, 9,10, 11, 12, 17, 16))
testInstances  <- as.character(c(1, 3, 4,  5, 8, 9,10, 11, 12, 17, 16))

# Here we define the Scenario, and observe that this part should contains all the part have been defined before, the scenario is a vector containg, the Runner, the training a set
# and test instances, the Budget, the parameters space, if we are insterested in making the experiment in parallel for saving computational time, the type of experiment, the number
# if this experiment will implement the elisitis race, the minimun number of configuration alive to continue with the race.
scenario <- list(
  targetRunner = runner,
  instances = trainInstances, 
  testInstances = testInstances,
  maxExperiments = 200,
  logFile = "irace-results.Rdata",  
  parameters = parameters,
  debugLevel = 0,
  parallel = 4,
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

# Testing phase.
if (file.exists("irace-results.Rdata")) {
  load("irace-results.Rdata")
  if (exists("iraceResults")) {
    if (!is.null(iraceResults$testing)) {
      print(iraceResults$testing)
    } else {
      testResults <- suppressMessages(suppressWarnings(
        testing_fromlog(logFile = "irace-results.Rdata",
                        testNbElites = scenario$testNbElites,
                        testIterationElites = scenario$testIterationElites)
      ))
    }
  }  
  cat("")
}
