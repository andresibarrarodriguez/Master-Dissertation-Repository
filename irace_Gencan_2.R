library(irace)

# ============================================================================
# INITIALIZATION: Clean previous execution logs and results
# ============================================================================
if (file.exists("irace_evaluations.log")) {
  file.remove("irace_evaluations.log")
}

# Limpiar archivos de resultados previos en mytests_dir
base <- Sys.getenv("ALGENCAN", "/home/andres/CODIGOS_DISERTACION/Experiment 3/algencan-4.0.0")
mytests_dir <- file.path(base, "mytests")

# Opcional: limpiar historial de tablines si quieres empezar completamente limpio
tablines_history <- file.path(mytests_dir, "tablines_history")
if (dir.exists(tablines_history)) {
  unlink(tablines_history, recursive = TRUE)
}

# ============================================================================
# EVALUATION FUNCTION: F(θ,I) 
# ============================================================================
evaluate_gencan <- function(problem_id, configuration) {
  # Directorio base donde están las librerías y donde se copian los tablines
  base <- Sys.getenv("ALGENCAN", "/home/andres/CODIGOS_DISERTACION/Experiment 3/algencan-4.0.0")
  mytests_dir <- file.path(base, "mytests")
  exe <- file.path(mytests_dir, "run-unc-bound-constr")
  
  eta <- as.numeric(configuration$eta)
  r <- as.numeric(configuration$r)
  sgmmult <- as.numeric(configuration$sgmmult)
  nfnoprogrmax <- as.integer(configuration$nfnoprogrmax)
  
  mc234 <- strsplit(as.character(configuration$mc234), "-")[[1]]
  
  env_list <- list(
    ETA = as.character(eta),
    R = as.character(r),
    SGMMULT = as.character(sgmmult),
    NFNOPROGRMAX = as.character(nfnoprogrmax),
    MC1 = "1",
    MC2 = mc234[1],
    MC3 = mc234[2],
    MC4 = mc234[3],
    EXTALLOWED = as.character(configuration$extallowed)
  )
  
  if (configuration$extallowed == "T") {
    env_list$BETA <- as.character(configuration$beta)
  }
  
  env_vector <- paste0(names(env_list), "=", unlist(env_list))
  
  trash <- tempfile()
  system2(exe, args = problem_id, env = env_vector, stdout = trash, stderr = trash, wait = TRUE)
  unlink(trash)
  
  f_best <- NA
  grad_norm <- NA
  fcnt <- NA
  cpu_time <- NA
  iterations <- NA
  
  # BUSCAR PRIMERO EN tabline.txt 
  tabline_path <- file.path(mytests_dir, "tabline.txt")
  
  if (file.exists(tabline_path)) {
    tl <- readLines(tabline_path, warn = FALSE)
    if (length(tl) > 0) {
      last <- tl[length(tl)]
      toks <- strsplit(trimws(last), "\\s+")[[1]]
      num_cols <- length(toks)
      
      if (num_cols >= 12) {
        f_best <- suppressWarnings(as.numeric(gsub("[dD]", "E", toks[7])))
        grad_norm <- suppressWarnings(as.numeric(gsub("[dD]", "E", toks[8])))
        fcnt <- suppressWarnings(as.integer(toks[10]))
        cpu_time <- suppressWarnings(as.numeric(toks[3]))
        iterations <- suppressWarnings(as.integer(toks[9]))
      }
    }
  }
  
  # SOLO si NO se obtuvo resultado del tabline normal, buscar en interrupte
  if (is.na(f_best) || is.na(fcnt)) {
    interrupted_path <- file.path(mytests_dir, "solver-interrupted-tabline.txt")
    if (file.exists(interrupted_path)) {
      tl <- readLines(interrupted_path, warn = FALSE)
      if (length(tl) > 0) {
        last <- tl[length(tl)]
        toks <- strsplit(trimws(last), "\\s+")[[1]]
        num_cols <- length(toks)
        
        if (num_cols >= 9) {
          f_best <- suppressWarnings(as.numeric(gsub("[dD]", "E", toks[4])))
          grad_norm <- suppressWarnings(as.numeric(gsub("[dD]", "E", toks[5])))  
          iterations <- suppressWarnings(as.integer(toks[6]))  
          fcnt <- suppressWarnings(as.integer(toks[7]))
          cpu_time <- 30  
        }
      }
    }
  }
  
 

  # Definicion de la Metrica de costo
  cost <- round(1e8 * f_best) + fcnt / (fcnt + 1)
  
  return(list(cost = cost, f_best = f_best, grad_norm = grad_norm, fcnt = fcnt, cpu_time = cpu_time, iterations = iterations))
}

runner <- function(experiment, scenario) {
  #set.seed(experiment$seed)
  result <- evaluate_gencan(experiment$instance, experiment$configuration)
  
  log_entry <- sprintf(
    "Config #%-4d | Instance: %-12s | Cost: %.6e | f_best: %s | grad_norm: %s | iterations: %s| fcnt: %s | Time: %s | eta=%s, r=%s, sgm=%s, nfnp=%s, mc234=%s, ext=%s%s\n",
    experiment$id.configuration,
    experiment$instance,
    result$cost,
    ifelse(is.na(result$f_best), "NA", sprintf("%.8e", result$f_best)),
    ifelse(is.na(result$grad_norm), "NA", sprintf("%.8e", result$grad_norm)),
    ifelse(is.na(result$iterations), "NA", as.character(result$iterations)),
    ifelse(is.na(result$fcnt), "NA", as.character(result$fcnt)),
    ifelse(is.na(result$cpu_time), "NA", sprintf("%.6f", result$cpu_time)),
    as.character(experiment$configuration$eta),
    as.character(experiment$configuration$r),
    as.character(experiment$configuration$sgmmult),
    as.character(experiment$configuration$nfnoprogrmax),
    as.character(experiment$configuration$mc234),
    as.character(experiment$configuration$extallowed),
    ifelse(experiment$configuration$extallowed == "T", sprintf(", beta=%s", as.character(experiment$configuration$beta)), "")
  )
  
  cat(log_entry, file = "irace_evaluations.log", append = TRUE)
  
  return(list(cost = result$cost))
}

parameters <- readParameters(text = '
beta         "" c (0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)                | extallowed == "T"
eta          "" c (1.0e1, 1.0e2, 1.0e3, 1.0e4, 1.0e5, 1.0e6, 1.0e7, 1.0e8, 1.0e9)
r            "" c (0.01, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.75, 0.9, 0.99)
sgmmult      "" c (2, 5, 10, 15, 20, 25, 30)
nfnoprogrmax "" c (3, 5, 10, 15, 20, 25, 30)
mc234        "" c (100-500-1000, 100-500-5000, 100-500-10000, 100-1000-5000, 100-1000-10000, 100-5000-10000, 500-1000-5000, 500-1000-10000, 500-5000-10000, 1000-5000-10000)
extallowed   "" c (T, F)
')
#trainInstances <- c("BQP1VAR", "DENSCHNB", "HIMMELBCLS", "LOGROS", "S308",  "DEGDIAG", "LUKSAN15LS", "LUKSAN22LS")



#Instancias del primer experimento con 30 instancias
#trainInstances <- c("BQP1VAR", "DENSCHNB", "HIMMELBCLS", "LOGROS", "S308", "CHWIRUT1LS", "HELIX", "SSI", "MGH09LS", "DEVGLA2", "LANCZOS2LS", "PALMER1D", "PALMER4C", "COOLHANSLS", "OSBORNEB", "CHNROSNB", "DMN37142LS", "LUKSAN22LS", "LRW1A", "DIAGPQE", "WALL10", "DIXMAANF", "DRCAV3LQ", "FLETBV3M", "PENTDI", "TRIDIA", "TORSION5", "CURLY10", "POWER", "DEGDIAG")


#Instancias del segundo experimento con 59 instancias
trainInstances <- c("BQP1VAR", "BROWNBS", "DENSCHNB", "EXP2", "HIMMELBCLS", "HS4", "LOGROS", "POWELLBSLS", "S308", "WAYSEA1B", "CHWIRUT1LS", "GAUSSIAN", "HELIX", "MGH10SLS", "SSI", "DEVGLA1B", "MGH09LS", "PALMER4B", "DEVGLA2", "QINGB", "LANCZOS2LS", "PALMER7A", "PALMER1D", "MAXLIKA", "PALMER4C", "PALMER8C", "COOLHANSLS", "STRATEC", "OSBORNEB", "HATFLDC", "CHNROSNB", "BA-L1LS", "DMN37142LS", "HYDC20LS", "LUKSAN22LS", "ARGLINA", "LRW1A", "QR3DLS", "DIAGPQE", "POWELLBC", "WALL10", "EIGENALS", "DIXMAANF", "DIXMAANN", "DRCAV3LQ", "BROYDN7D", "FLETBV3M", "MCCORMCK", "PENTDI", "SINQUAD", "TRIDIA", "ODC", "TORSION5", "FMINSRF2", "CURLY10", "NCVXBQP1", "POWER", "BA-L49LS", "DEGDIAG")

#trainInstances <- c("BQP1VAR", "BEALE", "BROWNBS", "CUBE", "DENSCHNB", "EGGCRATE", "EXP2", "GBRAINLS", "HIMMELBCLS", "HS1", "HS4", "JUDGE", "LOGROS", "MISRA1ALS", "POWELLBSLS", "PRICE4", "S308", "SISSER", "WAYSEA1B", "BARD", "CHWIRUT1LS", "DGOSPEC", "GAUSSIAN", "HATFLDE", "HELIX", "LSC1LS", "MGH10SLS", "PFIT3LS", "SSI", "ALLINITU", "DEVGLA1B", "HIMMELBF", "MGH09LS", "PALMER2B", "PALMER4B", "PSPDOC", "DEVGLA2", "LEVYMONT8", "QINGB", "FBRAIN3LS", "LANCZOS2LS", "PALMER3A", "PALMER7A", "CERI651CLS", "PALMER1D", "GAUSS2LS", "MAXLIKA", "PALMER2C", "PALMER4C", "PALMER6C", "PALMER8C", "VESUVIOLS", "COOLHANSLS", "DIAGIQB", "STRATEC", "TRIGON1", "OSBORNEB", "BLEACHNG", "HATFLDC", "METHANB8LS", "CHNROSNB", "TOINTGOR", "BA-L1LS", "MINSURF", "DMN37142LS", "DIAMON3DLS", "HYDC20LS", "LUKSAN15LS", "LUKSAN22LS", "SPIN2LS", "ARGLINA", "BROWNAL", "LRW1A", "GENROSEB", "QR3DLS", "DIAGNQB", "DIAGPQE", "FLETCHCR", "POWELLBC", "EXPLIN", "WALL10","EDENSCH", "EIGENALS", "DIXMAANB", "DIXMAANF", "DIXMAANJ", "DIXMAANN", "CHAINWOO", "DRCAV3LQ", "BDEXP", "BROYDN7D", "CRAGGLVY", "FLETBV3M", "FREUROTH", "MCCORMCK", "NONCVXUN", "PENTDI", "QUDLIN", "SINQUAD", "SSBRYBND", "TRIDIA", "NCB20", "ODC", "TORSION1", "TORSION5", "TORSIONC", "FMINSRF2", "WALL20", "CURLY10", "DIXON3DQ", "NCVXBQP1", "OBSTCLAL", "POWER", "SPARSQUR", "BA-L49LS", "BA-L21LS", "OSCIGRAD", "CYCLIC3LS", "WALL100" )

#trainInstances <- c("BQP1VAR", "BEALE", "BROWNBS", "CUBE", "DENSCHNB", "EGGCRATE", "EXP2", "GBRAINLS", "HIMMELBCLS", "HS1", "HS4", "JUDGE", "LOGROS", "MISRA1ALS", "POWELLBSLS", "PRICE4", "S308", "SISSER", "WAYSEA1B", "BARD", "CHWIRUT1LS", "DGOSPEC", "GAUSSIAN", "HATFLDE", "HELIX", "LSC1LS", "MGH10SLS", "PFIT3LS", "SSI", "ALLINIT", "DEVGLA1B", "HIMMELBF", "MGH09LS", "PALMER2B", "PALMER4B", "PSPDOC", "DEVGLA2", "LEVYMONT8", "QINGB",  "FBRAIN3LS", "LANCZOS2LS", "PALMER3A", "PALMER7A", "CERI651CLS", "PALMER1D", "GAUSS2LS", "MAXLIKA", "PALMER2C", "PALMER4C", "PALMER6C", "PALMER8C", "VESUVIOLS", "COOLHANSLS", "DIAGIQB", "STRATEC", "TRIGON1", "OSBORNEB", "BLEACHNG", "HATFLDC", "METHANB8LS", "CHNROSNB", "TOINTGOR", "BA-L1LS", "MINSURF", "DMN37142LS", "DIAMON3DLS", "HYDC20LS", "LUKSAN15LS", "LUKSAN22LS", "SPIN2LS", "ARGLINA", "BROWNAL", "LRW1A", "GENROSEB", "QR3DLS", "DIAGNQB", "DIAGPQE", "FLETCHCR", "POWELLBC", "EXPLIN", "WALL10", "EDENSCH", "EIGENALS", "DIXMAANB", "DIXMAANF", "DIXMAANJ", "DIXMAANN", "CHAINWOO", "DRCAV3LQ", "BDEXP", "BROYDN7D", "CRAGGLVY", "FLETBV3M", "FREUROTH", "MCCORMCK", "NONCVXUN", "PENTDI", "QUDLIN", "SINQUAD", "SSBRYBND", "TRIDIA", "NCB20", "ODC", "TORSION1", "TORSION5", "TORSIONC", "FMINSRF2", "WALL20", "CURLY10", "DIXON3DQ", "NCVXBQP1", "OBSTCLAL", "POWER", "SPARSQUR", "BA-L49LS", "BA-L21LS", "OSCIGRAD", "CYCLIC3LS", "WALL100")


#Instancias del segundo experimento con 118 instancias
#trainInstances <- c("BQP1VAR", "BEALE", "BROWNBS", "CUBE", "DENSCHNB", "EGGCRATE", "EXP2", "GBRAINLS", "HIMMELBCLS", "HS1", "HS4", "JUDGE", "LOGROS", "MISRA1ALS", "POWELLBSLS", "PRICE4", "S308", "SISSER", "WAYSEA1B", "BARD", "CHWIRUT1LS", "DGOSPEC", "GAUSSIAN", "HATFLDE", "HELIX", "LSC1LS", "MGH10SLS", "PFIT3LS", "SSI", "ALLINIT", "DEVGLA1B", "HIMMELBF", "MGH09LS", "PALMER2B", "PALMER4B", "PSPDOC", "DEVGLA2", "LEVYMONT8", "QINGB", "FBRAIN3LS", "LANCZOS2LS", "PALMER3A", "PALMER7A", "CERI651CLS", "PALMER1D", "GAUSS2LS", "MAXLIKA", "PALMER2C", "PALMER4C", "PALMER6C", "PALMER8C", "VESUVIOLS", "COOLHANSLS", "DIAGIQB", "STRATEC", "TRIGON1", "OSBORNEB", "BLEACHNG", "HATFLDC", "METHANB8LS", "CHNROSNB", "TOINTGOR", "BA-L1LS", "MINSURF", "DMN37142LS", "DIAMON3DLS", "HYDC20LS", "LUKSAN15LS", "LUKSAN22LS", "SPIN2LS", "ARGLINA", "BROWNAL", "LRW1A", "GENROSEB", "QR3DLS", "DIAGNQB", "DIAGPQE", "FLETCHCR", "POWELLBC", "EXPLIN", "WALL10", "EDENSCH", "EIGENALS", "DIXMAANB", "DIXMAANF", "DIXMAANJ", "DIXMAANN", "CHAINWOO", "DRCAV3LQ", "BDEXP", "BROYDN7D", "CRAGGLVY", "FLETBV3M", "FREUROTH", "MCCORMCK", "NONCVXUN", "PENTDI", "QUDLIN", "SINQUAD", "SSBRYBND", "TRIDIA", "NCB20", "ODC", "TORSION1", "TORSION5", "TORSIONC", "FMINSRF2", "WALL20", "CURLY10", "DIXON3DQ", "NCVXBQP1", "OBSTCLAL", "POWER", "SPARSQUR", "BA-L49LS", "BA-L21LS", "DEGDIAG", "YATP1CLS")
testInstances <- trainInstances
set.seed(123456)

scenario <- list(
  targetRunner = runner,
  testInstances= trainInstances,
  instances = trainInstances,
  maxExperiments = 3000, 
  logFile = "irace2-gencan-results.Rdata",
  debugLevel = 0,
  parallel = 1,
  deterministic = TRUE,
  elitistNewInstances=10,
  testNbElites = 4,
  firstTest = 30
  #seed = 123456
)

scenario <- checkScenario(scenario)

irace.output <- irace::irace(scenario = scenario, parameters = parameters)


#Testing
 if (file.exists("irace2-gencan-results.Rdata")) {
   load("irace2-gencan-results.Rdata")
   if (exists("iraceResults")) {
     if (!is.null(iraceResults$testing)) {
       print(iraceResults$testing)
     } else {
       testResults <- testing_fromlog(
         logFile = "irace2-gencan-results.Rdata",
         testNbElites = scenario$testNbElites,
         testIterationElites = scenario$testIterationElites
       )
       print(testResults)
     }
   }
}

