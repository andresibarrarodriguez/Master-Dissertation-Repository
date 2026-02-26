
library(irace)

# Limpiamos archivos temporales
temp_files <- c("tabline.txt", "solver-interrupted-tabline.txt", 
                "table-unc-bound-constr.txt")
unlink(temp_files[file.exists(temp_files)])


if (file.exists("irace_evaluations.log")) {
  file.remove("irace_evaluations.log")
}

# Función de evaluación para GENCAN
evaluate_gencan <- function(problem_id, configuration, run_id) {
  # Validar que run_id es obligatorio (garantiza reproducibilidad)
  if (missing(run_id) || is.null(run_id) || run_id == "") {
    stop("FATAL: run_id is required for reproducibility. Always provide a deterministic run_id.")
  }
  
  # Crear directorio de trabajo único para esta ejecución
  work_dir <- sprintf("run_%s_%s", problem_id, run_id)
  dir.create(work_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Log único por ejecución
  runner_log <- file.path(work_dir, "runner.log")
  
  # Mensaje inicial
  cat(sprintf("\n========== Evaluando %s (run_id: %s) ==========\n", 
              problem_id, run_id),
      file = runner_log, append = FALSE)
   
  base <- Sys.getenv("ALGENCAN", "/home/andres/Experiment 3/algencan-4.0.0")
  exe <- file.path(base, "mytests", "run-unc-bound-constr")
  
  # Extraer parámetros de la configuración
  eta <- as.numeric(configuration$eta)
  r <- as.numeric(configuration$r)
  sgmmult <- as.numeric(configuration$sgmmult)
  nfnoprogrmax <- as.integer(configuration$nfnoprogrmax)
  
  # Parsear mc234: formato "mc2-mc3-mc4" 
  mc234 <- strsplit(as.character(configuration$mc234), "-")[[1]]
  mc1 <- 1  
  mc2 <- as.integer(mc234[1])
  mc3 <- as.integer(mc234[2])
  mc4 <- as.integer(mc234[3])
  
  extallowed <- configuration$extallowed
  
  # Construimos un vector para las variables de entorno
  env_list <- list(
    WORK_DIR = work_dir,  # Pasar el directorio de trabajo al bash
    ETA = as.character(eta),
    R = as.character(r),
    SGMMULT = as.character(sgmmult),
    NFNOPROGRMAX = as.character(nfnoprogrmax),
    MC1 = as.character(mc1),
    MC2 = as.character(mc2),
    MC3 = as.character(mc3),
    MC4 = as.character(mc4),
    EXTALLOWED = as.character(extallowed)
  )
  
  # Lista de parámetros usados para el log
  params_used <- list(
    eta = eta,
    r = r,
    sgmmult = sgmmult,
    nfnoprogrmax = nfnoprogrmax,
    mc1 = mc1,
    mc2 = mc2,
    mc3 = mc3,
    mc4 = mc4,
    extallowed = extallowed
  )
  
  if (extallowed == "T") {
    beta <- as.numeric(configuration$beta)
    env_list$BETA <- as.character(beta)
    params_used$beta <- beta
  }
  
  # Construir vector de entorno para system2 
  env_vector <- paste0(names(env_list), "=", unlist(env_list))
  # Log de inicio
  cat(sprintf("\n=== Ejecutando: %s ===\n", problem_id),
      file = runner_log, append = TRUE)
  cat(sprintf("Comando: %s %s\n", exe, problem_id),
      file = runner_log, append = TRUE)
  cat(sprintf("Variables de entorno: %s\n", paste(env_vector, collapse = " ")),
      file = runner_log, append = TRUE)
  
  # Ejecutamos GENCAN con la instancia problema
  stdout_file <- file.path(work_dir, "stdout.txt")
  stderr_file <- file.path(work_dir, "stderr.txt")
  
  # Definir nombre de log fijo ANTES de ejecutar 
  expected_log <- sprintf("gencan-%s-current.log", problem_id)
  
  # Ejecucion del Runner
  exit_code <- system2(
    exe,
    args = problem_id,
    env = env_vector,
    stdout = stdout_file,
    stderr = stderr_file,
    wait = TRUE
  )
  
  # Esperar un poco para que el archivo se escriba completamente
  Sys.sleep(0.5)
  
  # ===== USAR NOMBRE FIJO EN LUGAR DE BUSCAR =====
  log_file <- file.path(work_dir, expected_log)
  
  if (!file.exists(log_file)) {
    cat(sprintf("ERROR: Log esperado no fue creado: %s\n", log_file),
        file = runner_log, append = TRUE)
    
    # Debugging: listar todos los archivos en el directorio de trabajo
    if (dir.exists(work_dir)) {
      all_files <- list.files(work_dir, full.names = TRUE, recursive = TRUE)
      cat(sprintf("Archivos en %s:\n%s\n", work_dir, 
                  paste(basename(all_files), collapse = ", ")),
          file = runner_log, append = TRUE)
    }
  } else {
    cat(sprintf("Log detectado correctamente: %s\n", log_file),
        file = runner_log, append = TRUE)
  }
  
  # Mostrar stdout y stderr (siempre para debugging)
  if (file.exists(stdout_file)) {
    stdout_content <- readLines(stdout_file, warn = FALSE)
    if (length(stdout_content) > 0) {
      cat(sprintf("STDOUT:\n%s\n", paste(stdout_content, collapse = "\n")),
          file = runner_log, append = TRUE)
    }
  }
  if (file.exists(stderr_file)) {
    stderr_content <- readLines(stderr_file, warn = FALSE)
    if (length(stderr_content) > 0) {
      cat(sprintf("STDERR:\n%s\n", paste(stderr_content, collapse = "\n")),
          file = runner_log, append = TRUE)
    }
  }
  
  # Limpiar archivos temporales
  unlink(c(stdout_file, stderr_file))
  
  # Extraer el valor de la función objetivo y el número de evaluaciones del log
  f_best <- NA
  fcnt <- NA
  
  if (!is.na(log_file) && file.exists(log_file)) {
    log_content <- readLines(log_file, warn = FALSE)
    cat(sprintf("Líneas en log: %d\n", length(log_content)),
        file = runner_log, append = TRUE)
    
    # Mostrar primeras 20 líneas para debugging
    cat(sprintf("Primeras 20 líneas del log:\n%s\n", 
                paste(head(log_content, 20), collapse = "\n")),
        file = runner_log, append = TRUE)
    
    # En esta parte estamos buscando el valor de  f_best 
    f_line <- grep("REPORTED BY SOLVER.*f.*=", log_content, value = TRUE, ignore.case = TRUE)
    if (length(f_line) == 0) {
      f_line <- grep("COMPUTED BY CALLER.*f.*=", log_content, value = TRUE, ignore.case = TRUE)
    }
    if (length(f_line) > 0) {
      cat(sprintf("Línea de f encontrada: %s\n", f_line[1]),
          file = runner_log, append = TRUE)
      raw <- sub(".*=\\s*", "", f_line[1])
      raw <- gsub("[dD]", "E", raw)
      raw <- trimws(raw)
      f_best <- suppressWarnings(as.numeric(raw))
      cat(sprintf("f_best extraído: %s (raw='%s')\n", f_best, raw),
          file = runner_log, append = TRUE)
    } else {
      cat("NO se encontró línea con f_best\n",
          file = runner_log, append = TRUE)
    }

    # En esta parte estamos buscando el valor de fcnt
    fcnt_line <- grep("Number of function evaluations.*=", log_content, value = TRUE, ignore.case = TRUE)
    if (length(fcnt_line) > 0) {
      cat(sprintf("Línea de fcnt encontrada: %s\n", fcnt_line[1]),
          file = runner_log, append = TRUE)
      raw <- trimws(sub(".*=\\s*", "", fcnt_line[1]))
      fcnt <- suppressWarnings(as.integer(raw))
      cat(sprintf("fcnt extraído: %s (raw='%s')\n", fcnt, raw),
          file = runner_log, append = TRUE)
    } else {
      cat("NO se encontró línea con fcnt\n",
          file = runner_log, append = TRUE)
    }
  } else {
    cat(sprintf("ERROR: Log file not found: %s\n", ifelse(is.na(log_file), "<NA>", log_file)),
        file = runner_log, append = TRUE)
  }
  
  # 
  tabline_file <- file.path(work_dir, "tabline.txt")
  if ((is.na(f_best) || is.na(fcnt)) && file.exists(tabline_file)) {
    tl <- readLines(tabline_file, warn = FALSE)
    if (length(tl) > 0) {
      last <- tl[length(tl)]
      toks <- strsplit(trimws(last), "\\s+")[[1]]
      # Extraer primeros tokens con formato D/E (f y gpsupn). 
      idxD <- grep("^[+-]?[0-9]*\\.?[0-9]+[DEde][+-]?[0-9]+$", toks)
      if (length(idxD) >= 1 && is.na(f_best)) {
        raw <- gsub("[dD]", "E", toks[idxD[1]])
        f_best <- suppressWarnings(as.numeric(raw))
        cat(sprintf("f_best (tabline) extraído: %s\n", f_best), file = runner_log, append = TRUE)
      }
      # Últimos 3 enteros son counters(1:3); 
      if (length(toks) >= 3 && is.na(fcnt)) {
        fcnt <- suppressWarnings(as.integer(toks[length(toks) - 2]))
        if (!is.na(fcnt)) cat(sprintf("fcnt (tabline) extraído: %s\n", fcnt), file = runner_log, append = TRUE)
      }
    }
  }
  
  
  # Extraemos el tiempo de CPU del log
  cpu_time <- NA
  if (!is.na(log_file) && file.exists(log_file)) {
    cpu_line <- grep("CPU time in seconds", log_content, value = TRUE, ignore.case = TRUE)
    if (length(cpu_line) > 0) {
      raw <- sub(".*=\\s*", "", cpu_line[1])
      raw <- trimws(raw)
      cpu_time <- suppressWarnings(as.numeric(raw))
      cat(sprintf("cpu_time extraído: %s\n", cpu_time), file = runner_log, append = TRUE)
    }
  }
  
  # Calcular costo con valores reales
  cost <- round(1e8 * f_best) + fcnt / (fcnt + 1)
  
  # Construir mensaje con parámetros usados
  params_str <- paste(names(params_used), "=", sapply(params_used, function(x) {
    if (is.numeric(x)) sprintf("%.6g", x) else as.character(x)
  }), sep="", collapse=", ")
  
  cat(sprintf("Resultado: %s, f_best=%s, fcnt=%s, cost=%s\n",
              params_str, 
              sprintf("%.6e", f_best),
              as.character(fcnt),
              sprintf("%.6e", cost)),
      file = runner_log, append = TRUE)
  
  # Limpiar directorio de trabajo después de extraer resultados
  # (dejamos el runner.log para debugging, pero borramos archivos temporales grandes)
  tryCatch({
    files_to_remove <- list.files(work_dir, 
                                   pattern = "\\.(o|f|SIF|d)$|^(ELFUN|EXTER|GROUP|RANGE|gencanma-forcutest)$",
                                   full.names = TRUE)
    unlink(files_to_remove)
  }, error = function(e) {
    cat(sprintf("Warning: No se pudieron limpiar archivos temporales: %s\n", e$message),
        file = runner_log, append = TRUE)
  })
  
  # Guardar cpu_time en archivo para que el runner lo pueda leer
  cpu_time_file <- file.path(work_dir, "cpu_time.txt")
  if (!is.na(cpu_time)) {
    writeLines(as.character(cpu_time), cpu_time_file)
  }
  
  return(cost)
}


# Target Runner
runner <- function(experiment, scenario) {
  C <- experiment$configuration
  
  run_id <- sprintf("exp%06d-%d", experiment$id.configuration, experiment$seed)
  
  # Obtener nombre de la instancia correctamente
  # experiment$instance es un STRING (el nombre), NO un índice
  instance_name <- experiment$instance
  
  # Ejecutar evaluación CON EL NOMBRE de la instancia
  result <- evaluate_gencan(instance_name, C, run_id)
  
  # ===== LOG DETALLADO DE CADA EVALUACIÓN =====
  # Crear/actualizar log global de todas las evaluaciones
  global_log <- "irace_evaluations.log"
  
  # Leer cpu_time del archivo guardado por evaluate_gencan
  work_dir <- sprintf("run_%s_%s", instance_name, run_id)
  cpu_time_file <- file.path(work_dir, "cpu_time.txt")
  cpu_time <- 0
  if (file.exists(cpu_time_file)) {
    cpu_time <- as.numeric(readLines(cpu_time_file, warn = FALSE)[1])
    if (is.na(cpu_time)) cpu_time <- 0
  }
  
  # Construir línea de log con toda la información
log_entry <- sprintf(
  "Config #%-4d | Instance: %-12s | Cost: %s | Time: %.6f | eta=%s, r=%s, sgm=%s, nfnp=%s, mc234=%s, ext=%s%s\n",
  experiment$id.configuration,
  instance_name,
  sprintf("%.6e", result),
  cpu_time,
  as.character(C$eta),
  as.character(C$r),
  as.character(C$sgmmult),
  as.character(C$nfnoprogrmax),
  as.character(C$mc234),
  as.character(C$extallowed),
  ifelse(C$extallowed == "T", sprintf(", beta=%s", as.character(C$beta)), "")
)


  lock_file <- paste0(global_log, ".lock")
  max_attempts <- 100
  attempt <- 0
  
  while (attempt < max_attempts) {
    if (!file.exists(lock_file)) {
      # Crear lock
      writeLines("locked", lock_file)
      
      # Escribir log
      cat(log_entry, file = global_log, append = TRUE)
      
      # Liberar lock
      unlink(lock_file)
      break
    }
    
    Sys.sleep(0.01)
    attempt <- attempt + 1
  }
  
  return(list(cost = result))
}

# Instancias: problemas de CUTEst
trainInstances <- c("10FOLDTRLS", "3PK", "AIRCRFTB", "AKIVA", "ALLINIT", "ALLINITU", "ARGLINA", "ARGLINB", "ARGLINC", "ARGTRIGLS", "ARWHEAD", "BA-L1LS", "BA-L1LS", "BA-L1SPLS", "BA-L21LS", "BA-L21LS")
testInstances <- c("10FOLDTRLS", "3PK", "AIRCRFTB", "AKIVA", "ALLINIT", "ALLINITU", "ARGLINA", "ARGLINB", "ARGLINC", "ARGTRIGLS", "ARWHEAD", "BA-L16LS", "BA-L1LS", "BA-L1SPLS", "BA-L21LS")

# Parámetros a ajustar
parameters <- readParameters(text = '
beta         "" c (0.1, 0.5, 0.9)                | extallowed == "T"
eta          "" c (1.0e2, 1.0e3, 1.0e4, 1.0e5, 1.0e6)
r            "" c (0.01, 0.1, 0.25, 0.5, 0.75, 0.9, 0.99)
sgmmult      "" c (2, 5, 10, 20)
nfnoprogrmax "" c (3, 5, 10)
mc234        "" c (100-500-1000, 100-500-5000, 100-500-10000, 100-1000-5000, 100-1000-10000, 100-5000-10000, 500-1000-5000, 500-1000-10000, 500-5000-10000, 1000-5000-10000)
extallowed   "" c (T, F)
')

# Configuraciones iniciales que incluyen extallowed = T
initial_configs <- data.frame(
  beta = c(0.5, 0.1, 0.9, NA, NA, NA),
  eta = c("1.0e4", "1.0e3", "1.0e5", "1.0e4", "1.0e3", "1.0e5"),
  r = c(0.9, 0.5, 0.75, 0.9, 0.5, 0.75),
  sgmmult = c(5, 10, 5, 5, 10, 20),
  nfnoprogrmax = c(5, 3, 5, 5, 3, 10),
  mc234 = c("100-500-1000", "100-500-5000", "500-1000-5000", "100-500-1000", "100-1000-5000", "500-1000-10000"),
  extallowed = c("T", "T", "T", "F", "F", "F"),
  stringsAsFactors = FALSE
)

# Guardar configuraciones iniciales en un archivo temporal
initial_configs_file <- "initial_configs.txt"
write.table(initial_configs, initial_configs_file, 
            row.names = FALSE, col.names = TRUE, quote = FALSE, sep = " ")

# Escenario
set.seed(123456)  

scenario <- defaultScenario()
scenario$targetRunner <- runner
scenario$instances <- trainInstances
scenario$testInstances <- testInstances
scenario$maxExperiments <- 180
scenario$logFile <- "irace-gencan-results.Rdata"
scenario$debugLevel <- 0
scenario$parallel <- 4  
scenario$deterministic <- TRUE
scenario$seed <- 123456
scenario$firstTest <- 5
scenario$configurationsFile <- initial_configs_file  
# Ejecutar irace
irace.output <- irace::irace(scenario, parameters = parameters)  

# <- 5

scenario <- checkScenario(scenario)

# Ejecutar irace
irace.output <- irace::irace(scenario, parameters = parameters)

# Testing
if (file.exists("irace-gencan-results.Rdata")) {
  load("irace-gencan-results.Rdata")
  if (exists("iraceResults")) {
    if (!is.null(iraceResults$testing)) {
      print(iraceResults$testing)
    } else {
      testResults <- testing_fromlog(
        logFile = "irace-gencan-results.Rdata",
        testNbElites = scenario$testNbElites,
        testIterationElites = scenario$testIterationElites
      )
      print(testResults)
    }
  }
}