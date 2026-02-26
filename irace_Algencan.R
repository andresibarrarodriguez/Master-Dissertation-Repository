library(irace)

# ============================================================================
# INITIALIZATION: Clean previous execution logs and results
# ============================================================================
if (file.exists("irace_evaluations.log")) {
  file.remove("irace_evaluations.log")
}

# Limpiar archivos de resultados previos en mytests_dir
base <- Sys.getenv("ALGENCAN", "/home/andres/Experimet 4/algencan-4.0.0/algencan-4.0.0")
mytests_dir <- file.path(base, "mytests")

# 
tablines_history <- file.path(mytests_dir, "tablines_history")
if (dir.exists(tablines_history)) {
  unlink(tablines_history, recursive = TRUE)
}

# ============================================================================
# EVALUATION FUNCTION: F(θ,I) 
# ============================================================================
evaluate_algencan <- function(problem_id, configuration) {
  # Directorio base donde están las librerías y donde se copian los tablines
  base <- Sys.getenv("ALGENCAN", "/home/andres/Experimet 4/algencan-4.0.0/algencan-4.0.0")
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
  
  # Parámetros de la métrica C2
  eps_feas <- 1e-8
  f_bar <- 1e12  # Cota superior para puntos no factibles
  
  # Formato del tabline.txt y alsolver-interrupted-tabline.txt:
  # Pos 1-9: dimensiones del problema
  # Pos 10: f_best, 11: csupn, 12: ssupn, 13: nlpsupn, 14: bdsvio
  # Pos 15: xtype, 16: max|x|, 17: max|lambda|
  # Pos 18: outer iterations, 19: inner iterations
  # Pos 20: nwcalls, 21: nwtotit
  # Pos 22-26: counters (evalf, evalg, evalc, evalj, evalhl)

  # tabline.txt tiene 29 columnas (incluye istop, ierr, cpu_time al inicio):
  # Pos 1: istop, 2: ierr, 3: cpu_time
  # Pos 4-12: dimensiones del problema
  # Pos 13: f_best, 14: csupn, 15: ssupn, 16: nlpsupn, 17: bdsvio
  # Pos 18: xtype, 19: max|x|, 20: max|lambda|
  # Pos 21: outer iterations, 22: inner iterations
  # Pos 23: nwcalls, 24: nwtotit
  # Pos 25-29: counters (evalf, evalg, evalc, evalj, evalhl)

  tabline_path <- file.path(mytests_dir, "tabline.txt")
  
  if (file.exists(tabline_path)) {
    tl <- readLines(tabline_path, warn = FALSE)
    if (length(tl) > 0) {
      last <- tl[length(tl)]
      toks <- strsplit(trimws(last), "\\s+")[[1]]
      num_cols <- length(toks)
      
      if (num_cols >= 29) {
        f_best     <- suppressWarnings(as.numeric(gsub("[dD]", "E", toks[13])))
        csupn      <- suppressWarnings(as.numeric(gsub("[dD]", "E", toks[14])))
        nlpsupn    <- suppressWarnings(as.numeric(gsub("[dD]", "E", toks[16])))
        cpu_time   <- suppressWarnings(as.numeric(toks[3]))
        iterations <- suppressWarnings(as.integer(toks[21]))
        fcnt       <- suppressWarnings(as.integer(toks[25]))
      }
    }
  }
  
  # SOLO si NO se obtuvo resultado del tabline normal, buscar en interrupted
  # alsolver-interrupted-tabline.txt tiene 26 columnas (NO incluye istop, ierr, cpu_time):
  # Pos 1-9: dimensiones del problema
  # Pos 10: f_best, 11: csupn, 12: ssupn, 13: nlpsupn, 14: bdsvio
  # Pos 15: xtype, 16: max|x|, 17: max|lambda|
  # Pos 18: outer iterations, 19: inner iterations
  # Pos 20: nwcalls, 21: nwtotit
  # Pos 22-26: counters (evalf, evalg, evalc, evalj, evalhl)

  if (is.na(f_best) || is.na(fcnt)) {
    interrupted_path <- file.path(mytests_dir, "alsolver-interrupted-tabline.txt")
    if (file.exists(interrupted_path)) {
      tl <- readLines(interrupted_path, warn = FALSE)
      if (length(tl) > 0) {
        last <- tl[length(tl)]
        toks <- strsplit(trimws(last), "\\s+")[[1]]
        num_cols <- length(toks)
        
        if (num_cols >= 26) {
          f_best     <- suppressWarnings(as.numeric(gsub("[dD]", "E", toks[10])))
          csupn      <- suppressWarnings(as.numeric(gsub("[dD]", "E", toks[11])))
          nlpsupn    <- suppressWarnings(as.numeric(gsub("[dD]", "E", toks[13])))
          iterations <- suppressWarnings(as.integer(toks[18]))
          fcnt       <- suppressWarnings(as.integer(toks[22]))
          cpu_time   <- 30
        }
      }
    }
  }
  
  # Métrica de costo C2 (Definition 4.2.4)
  # eps_feas = 1e-8, f_bar = 1e12
  if (csupn <= eps_feas) {
    # Caso factible: round(10^8 * f(θ,I)) + fcnt/(fcnt+1)
    cost <- round(1e8 * f_best) + fcnt / (fcnt + 1)
  } else {
    # Caso no factible: round(10^8 * f_bar * (c(θ,I)/eps_feas)) + fcnt/(fcnt+1)
    cost <- round(1e8 * f_bar * (csupn / eps_feas)) + fcnt / (fcnt + 1)
  }
  
  return(list(cost = cost, f_best = f_best, nlpsupn = nlpsupn, fcnt = fcnt, cpu_time = cpu_time, iterations = iterations, csupn = csupn))
}

runner <- function(experiment, scenario) {
  result <- evaluate_algencan(experiment$instance, experiment$configuration)
  
  log_entry <- sprintf(
    "Config #%-4d | Instance: %-12s | Cost: %.6e | f_best: %s | csupn: %s | nlpsupn: %s | iterations: %s | fcnt: %s | Time: %s | rhomult=%s, rhofrac=%s\n",
    experiment$id.configuration,
    experiment$instance,
    result$cost,
    ifelse(is.na(result$f_best), "NA", sprintf("%.8e", result$f_best)),
    ifelse(is.na(result$csupn), "NA", sprintf("%.8e", result$csupn)),
    ifelse(is.na(result$nlpsupn), "NA", sprintf("%.8e", result$nlpsupn)),
    ifelse(is.na(result$iterations), "NA", as.character(result$iterations)),
    ifelse(is.na(result$fcnt), "NA", as.character(result$fcnt)),
    ifelse(is.na(result$cpu_time), "NA", sprintf("%.6f", result$cpu_time)),
    as.character(experiment$configuration$rhomult),
    as.character(experiment$configuration$rhofrac)
  )
  
  cat(log_entry, file = "irace_evaluations.log", append = TRUE)
  
  return(list(cost = result$cost))
}

parameters <- readParameters(text = '
rhomult  "" c (2, 3, 5, 10, 100)
rhofrac  "" c (0.1, 0.5, 0.9)
')

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

testInstances <- trainInstances
set.seed(123456)

scenario <- list(
  targetRunner = runner,
  testInstances = trainInstances,
  instances = trainInstances,
  maxExperiments = 800, 
  logFile = "irace-algencan-results1.Rdata",
  debugLevel = 2,
  parallel = 1,
  deterministic = TRUE,
  elitistNewInstances = 15,
  minNbSurvival=4,
  testNbElites = 4,
  firstTest = 20, 
  postselection = TRUE
)

scenario <- checkScenario(scenario)

irace.output <- irace::irace(scenario = scenario, parameters = parameters)

# Testing
if (file.exists("irace-algencan-results1.Rdata")) {
  load("irace-algencan-results1.Rdata")
  if (exists("iraceResults")) {
    if (!is.null(iraceResults$testing)) {
      print(iraceResults$testing)
    } else {
      testResults <- testing_fromlog(
        logFile = "irace-algencan-results1.Rdata",
        testNbElites = scenario$testNbElites,
        testIterationElites = scenario$testIterationElites
      )
      print(testResults)
    }
  }
}