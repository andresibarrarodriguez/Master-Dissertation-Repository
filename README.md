
*****************************************************************
**Author: Andres Mauricio Ibarra Rodriguez**

**Advisor: Prof. Dr. Ernesto Julian Golberg Birgin**
*****************************************************************


This is the Master Repository from my Master Disertation in Applied Mathematics at Sao Paulo State University, 

Accordign to the contents of the dissertation the objetive of this is to use sequentential statistical learning for tuning the augmented Lagrangian algorithm ALGENCAN using IRACE. For this purpose in this repository contains five codes in \texttt{R} which are responsible to execute the main algorithm which are written in FORTRAN. There was implemented four experiments, we gave a brief description below and the corresponding codes in the repository and how it should be used.

----------------------------------------------------------
**EXPERIMENT 1: I-RACE for tuning Monte Carlo Algorithm**
----------------------------------------------------------

According to what is proposed in the dissertation, the following files are required to carry out this experiment.

1.Montecarlo_Search.f90
2.src_unc.f
3.irace_Montecarlo.R


(*)To run the Monte_Carlo.f90 search program, we use the file src_unc.f, which contains the OBJ.f90 subroutines used for function evaluation. Compile with: gfortran -O2 -o montecarlo_search Montecarlo_Search.f90 src_unc.f y corremos como: 
./montecarlo_search nprob boxtype itrialmax

(**) To run Irace and find a parameter configuration $\theta=(\rho, \Omega)$, use the file irace_Montecarlo.R. In that file, the line  exe <- "/home/user/Documentos/montecarlo_search", the path where the compiled evaluation program Montecarlo_Seach.f90 is located. 


To run the script, execute:

(***) Rscript irace_Montecarlo.R.




---------------------------------------------------------
**EXPERIMENT 2: I-RACE for tuning GENCAN: I**
---------------------------------------------------------

De acuerdo con lo propuesto en la disertacion, el objetivo de este experimento es el ajuste del metodo de optimizacion interno dentro de la face, implementeado por GENCAN, en la version de ALGENCAN 3.0.0, esta contempla. En este usamos la libreria HSL, la MA-57, para resolver sistemas lineales.

1. Newton con globalizacion en regiones de confianza;
2. Newton con busqueda lineal;
3. Newton Truncado.

para la ejecucion de este experimento sera necesario los siguientes archivos:

1. subroutines_GENCAN.f
2. conf_GENCAN.f90
3. src_hes.f
4. src_unc.f
5.1. irace_GENCAN.R
5.2. irace_GENCAN_2.R

(**Obs**: (5.1) y (5.2) la diferencia principal entre estos Scripts, es la definicion de la medida de costo C1 y C2.)

(*) Para este compilamos de la forma: gfortran -c src_unc.f
gfortran -c src_hes.f
gfortran -c subroutines_GENCAN.f90
gfortran -c conf_GENCAN.f90

(**) Enlazamos con las rutas y librerias HSL:

gfortran -o gencan \
  conf_GENCAN.o \
  subroutines_GENCAN.o \
  src_unc.o \
  src_hes.o \
  /home/user/Descargas/algencan-3.1.1/lib/libalgencan.a \
  /home/user/Descargas/algencan-3.1.1/lib/libhsl.a \
  -fopenmp
  
Posteriormente, correremos el ejecutable de la forma: ./gencan nprob method, con metodo\in {newton,tr,tn}

(***) Para ejecutar el Script en R (5.1) y (5.2) y correr IRACE, en la consola de comando ejecutaremos, Rscript irace_GENCAN.R o bien irace_GENCAN_2.R, donde el runner en este caso correra el programa alojado en la ruta, donde se encuentra algencan-3.1.1: /home/user/Descargas/algencan-3.1.1/CODES_DISERTATION/Experiment2/gencan


---------------------------------------------------------
**EXPERIMENT 3: I-RACE for tuning GENCAN: II**
---------------------------------------------------------

Para este experimento, se utilizo el GENCAN, de la version ALGENCAN 4.0.0, y se busco implementar IRACE, para ajustar los parametros definidos de interes definidos en la disertacion, las instancias de entrenamiento empleadas fueron las definidas en el CUTEst, para problemas irestringidos y con restricciones de caja.

Para la ejecucion de este experimento fueron necesarios los siguientes archivos:

1. gencan.f90
2. gencanma-forcutest.f90
3. run-unc-bound-contr
4. irace_Gencan.R

El algoritmo GENCAN, en este caso lo ejecutaremos mendiante el script run-unc-bound-contr, en este compilaremos las librerias ls, hsl y la interfaz del CUTEst, mediante gencanma-forcutest.f90, para esto ejecutamos: chmod +x run-unc-bound-constr, ese bash compila todas las librerias y seguidamente hacemos: Φ1 Φ2 Φ3 Φ4 Φ5 Φ6 Φ7 Φ8 Φ9 Φ10 ./run-unc-bound-constr problem, donde cada Φi, es un parametro distinto del Algoritmo GENCAN. Con la compilacion realizada procedemos a ejecutar el script en R con el comando Rscript irace_Gencan.R.


---------------------------------------------------------
**EXPERIMENT 4: I-RACE for tuning ALGENCAN**
---------------------------------------------------------

Para este experimento, se utilizo el ALGENCAN, de la version ALGENCAN 4.0.0, y se busco implementar IRACE, para ajustar los parametros definidos de interes definidos en la disertacion, las instancias de entrenamiento empleadas fueron las definidas en el CUTEst, para problemas no lineales h(x)=0, g(x)\leq 0, x\in \Omega.

Para la ejecucion de este experimento fueron necesarios los siguientes archivos:

1. algencan.f90
2. algencanma-forcutest.f90
3. run-nlp
4. irace_Algencan.R

El algoritmo ALGENCAN, en este caso lo ejecutaremos mendiante el script run-nlp, en este compilaremos las librerias ls, hsl y la interfaz del CUTEst, mediante algencanma-forcutest.f90, para esto ejecutamos: chmod +x run-nlp, ese bash compila todas las librerias y seguidamente hacemos: $\zeta_1$ ζ1 ζ2 ./run-nlp problem, donde cada Φi, es un parametro distinto del Algoritmo GENCAN. Con la compilacion realizada procedemos a ejecutar el script en R con el comando Rscript irace_Gencan.R.







