library(irace)

# ----------------------------------------------------------------------
# Definition of the evaluation function. It sets the path to the
# executable for running the algorithm (ALGENCAN), using a
# configuration array sampled from the parameter space.
# The version used in this experiment was ALGENCAN included in ALGENCAN 4.0.0.
# ----------------------------------------------------------------------
evaluate_algencan <- function(problem_id, configuration) {
  base <- Sys.getenv("ALGENCAN", "/home/andres/Experimet_4/algencan-4.0.0/algencan-4.0.0")
  mytests_dir <- file.path(base, "mytests")
  exe <- file.path(mytests_dir, "run-nlp")
  
  rhomult <- as.numeric(configuration$rhomult)
  rhofrac <- as.numeric(configuration$rhofrac)
  
  env_list <- list(
    RHOMULT = as.character(rhomult),
    RHOFRAC = as.character(rhofrac)
  )
  
  env_vector <- paste0(names(env_list), "=", unlist(env_list))
  
  trash <- tempfile()
  system2(exe, args = problem_id, env = env_vector, stdout = trash, stderr = trash, wait = TRUE)
  unlink(trash)
  
  f_best <- NA
  grad_norm <- NA
  fcnt <- NA
  cpu_time <- NA
  iterations <- NA
  csupn <- NA
  nlpsupn <- NA
  
  
  eps_feas <- 1e-8
  f_bar <- 1e12 
  tabline_path <- file.path(mytests_dir, "tabline.txt")
  
  # ----------------------------------------------------------------------
  #  Extracts information from the tabline or interrupted tabline files,
  # including functional values, function evaluations (fcnt), number of 
  # iterations, and norm of feasibility and Lagrangian norm. These values are then 
  # used to compute the cost measure and build the comparative tables 
  # presented in the Dissertation.
  # ----------------------------------------------------------------------
  if (file.exists(tabline_path)) {
    tl <- readLines(tabline_path, warn = FALSE)
    if (length(tl) > 0) {
      last <- tl[length(tl)]
      toks <- strsplit(trimws(last), "\\s+")[[1]]
      num_cols <- length(toks)
      
      if (num_cols >= 29) {
        f_best     <- as.numeric(gsub("[dD]", "E", toks[13]))
        csupn      <- as.numeric(gsub("[dD]", "E", toks[14]))
        nlpsupn    <- as.numeric(gsub("[dD]", "E", toks[16]))
        cpu_time   <- as.numeric(toks[3])
        iterations <- as.integer(toks[21])
        fcnt       <- as.integer(toks[25])
      }
    }
  }
  if (is.na(f_best) || is.na(fcnt)) {
    interrupted_path <- file.path(mytests_dir, "alsolver-interrupted-tabline.txt")
    if (file.exists(interrupted_path)) {
      tl <- readLines(interrupted_path, warn = FALSE)
      if (length(tl) > 0) {
        last <- tl[length(tl)]
        toks <- strsplit(trimws(last), "\\s+")[[1]]
        num_cols <- length(toks)
        
        if (num_cols >= 26) {
          f_best     <- as.numeric(gsub("[dD]", "E", toks[10]))
          csupn      <- as.numeric(gsub("[dD]", "E", toks[11]))
          nlpsupn    <- as.numeric(gsub("[dD]", "E", toks[13]))
          iterations <- as.integer(toks[18])
          fcnt       <- as.integer(toks[22])
          cpu_time   <- 30
        }
      }
    }
  }
  
  # ----------------------------------------------------------------------
  # Defintion of the Cost-Measure: 
  # ----------------------------------------------------------------------
  if (csupn <= eps_feas) {
    cost <- round(1e8 * f_best) + fcnt / (fcnt + 1)
  } else {
    cost <- round(1e8 * f_bar * (csupn / eps_feas)) + fcnt / (fcnt + 1)
  }
  
  return(list(cost = cost, f_best = f_best, nlpsupn = nlpsupn, fcnt = fcnt, cpu_time = cpu_time, iterations = iterations, csupn = csupn))
}

# ----------------------------------------------------------------------
# Definition of the Runner function, which is responsible for executing
# the internal algorithm within the Irace Scenario.
# ----------------------------------------------------------------------
runner <- function(experiment, scenario) {
  result <- evaluate_algencan(experiment$instance, experiment$configuration)
  
  return(list(cost = result$cost))
}

# ----------------------------------------------------------------------
# Definition of the parameter space X, from which configurations will be 
# sampled.
# ----------------------------------------------------------------------
parameters <- readParameters(text = '
rhomult  "" c (2, 3, 5, 10, 100)
rhofrac  "" c (0.1, 0.5, 0.9)
')

# ----------------------------------------------------------------------
# Defines the problem instances considered in the training phase,
# taken from the CUTEst benchmark, including problems with general 
# constraints of the form h(x) = 0, g(x) ≤ 0, with x ∈ Ω.
# ----------------------------------------------------------------------
#trainInstances <- c("BURKEHAN", "ALSOTAME", "BT1", "EXTRASIM", "HIMMELP2", "HS10", "HS11", "HS12", "HS13", "HS21", "HS57", "HS6", "HS7", "HS88", "HS9", "HUBFIT", "LSQFIT", "MARATOS", "S316-322", "TAME", "TRY-B", "BT10", "FLT", "HIMMELP3", "HS14", "HS15", "HS16", "HS17", "HS18", "HS19", "HS22", "SIMPLLPA", "SNAKE", "SUPERSIM", "TWOBARS", "ZECEVIC2", "ZECEVIC3", "ZECEVIC4", "HIMMELP4", "HIMMELP5", "HS20", "HS24", "HS59", "SIMPLLPB", "HIMMELP6", "HS23", "PT", "HET-Z", "SIPOW1", " SIPOW1M", "SIPOW2", "SIPOW2M", "BT2", "HS26", "HS27", "HS28", "HS29", "HS30", "HS31", "HS35", "HS35I", "HS35MOD", "HS36", "HS60", "HS62", "HS64", "HS65", "HS89", "BT4", "BT5", "BYRDSPHR", "HS32", "HS33", "HS34", "HS37", "HS61", "HS63", "HS66", "KIWCRESC", "LOOTSMA", "MAKELA1", "MIFFLIN1", "MIFFLIN2", "POLAK1", "POLAK5", "SPIRAL", "STANCMIN", "WACHBIEG", "ZY2", "CB2", "CB3", "CHACONN1", "CHACONN2", "DEMYMALO", "GIGOMEZ1", "GIGOMEZ2", "GIGOMEZ3", "MAKELA2", "WOMFLET", "MINMAXRB")

#trainInstances <- c(
#  "BURKEHAN", "ALSOTAME", "BT1", "EXTRASIM", "HIMMELP2", "HS10", "HS11",
#  "HS12", "HS13", "HS21", "HS57", "HS6", "HS7", "HS88", "HS9", "HUBFIT",
#  "LSQFIT", "MARATOS", "S316-322", "TAME", "TRY-B", "BT10", "FLT",
#  "HIMMELP3", "HS14", "HS15", "HS16", "HS17", "HS18", "HS19", "HS22",
#  "SIMPLLPA", "SNAKE", "SUPERSIM", "TWOBARS", "ZECEVIC2", "ZECEVIC3",
#  "ZECEVIC4", "HIMMELP4", "HIMMELP5", "HS20", "HS24", "HS59",
#  "SIMPLLPB", "HIMMELP6", "HS23", "PT", "HET-Z", "SIPOW1", "SIPOW1M"
#)
trainInstances <- c("BURKEHAN", "HS13", "HS8", "TAME", "GIGOMEZ1", "HS37", "MIFFLIN2", "ALLINITC", "HS71", "BT13", "HS50", "MWRIGHT", "HS95", "HS103", "HS107", "PORTSNQP", "HS117", "MAKELA3", "ACOPP14", "CORE1", "LINSPANH", "SMBANK", "HYDROELS", "ZAMB2-11", "POLYGODD", "STEENBRC", "CHAIN", "CLEUVEN4", "CMPC10", "LEUVEN2", "MODEL", "LISWET9", "CMPC1", "JJTABEL3", "HIER163A", "POWELL20", "SAROMM", "A5NSDSDM", "GRIDNETC", "LUKVLE12", "DTOC5", "LUKVLI10", "NCVXQP9", "GASOIL", "LIPPERT1", "A0ESSNDL", "A0NNDNDL", "A5NNDNDL", "RDW2D52F", "BDRY2")

#trainInstances <- c("BURKEHAN", "HS13", "HS8", "TAME", "GIGOMEZ1")
testInstances <- trainInstances
set.seed(123456)

# ----------------------------------------------------------------------
# Here we define the Irace Scenario, which specifies the conditions
# under which Algencan with using internal algorithm:(Newton with Line-Search) 
#will be executed. 
# ----------------------------------------------------------------------
scenario <- list(
  targetRunner = runner,
  testInstances = trainInstances,
  instances = trainInstances,
  maxExperiments = 1000, 
  logFile = "irace-algencan-results1.Rdata",
  debugLevel = 2,
  parallel = 1,
  deterministic = TRUE,
  elitistNewInstances = 10,
  minNbSurvival=4,
  testNbElites = 4,
  firstTest = 30, 
  parameters = parameters,
  seed = 123456,
  testIterationElites = 1,
  postselection = TRUE
  #parameters = parameters
)

scenario <- checkScenario(scenario)

irace.output <- irace::irace(scenario = scenario)
