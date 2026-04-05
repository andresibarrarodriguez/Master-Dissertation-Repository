
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

3.Irace_Montecarlo.R


(*)To run the Monte_Carlo.f90 search program, we use the file src_unc.f, which contains the OBJ.f90 subroutines used for function evaluation. Compile with: gfortran -O2 -o montecarlo_search Montecarlo_Search.f90 src_unc.f y corremos como: 
./montecarlo_search nprob boxtype itrialmax

(**) To run Irace and find a parameter configuration $\theta=(\rho, \Omega)$, use the file Irace_Montecarlo.R. In that file, the line  exe <- "/home/user/Documentos/montecarlo_search", set the path where the compiled evaluation program Montecarlo_Seach.f90 is located. 


To run the script, execute:

(***) Rscript Irace_Montecarlo.R.




---------------------------------------------------------
**EXPERIMENT 2: I-RACE for tuning GENCAN: I**
---------------------------------------------------------

According to the methodology proposed in the dissertation, the objective of this experiment is to tune the internal optimization method implemented within Phase $\mathscr{F}_{I}$ by GENCAN in version 3.1.1 of ALGENCAN. In this version, the HSL library, specifically MA57, is used to solve the linear systems.

1. Newton's method with trust-region globalization;
2. Newton's method with line search;
3. Truncated Newton method.

The following files are required for the execution of this experiment:

1. subroutines_GENCAN.f90
2. conf_GENCAN.f90
3. src_hes.f
4. src_unc.f
5.1.Irace_GENCAN-3.1.1.R
5.2.Irace_GENCAN-3.1.1-(2).R

(**Obs**: (5.1) y (5.2) the main difference between these scripts lies in the definition of the cost measure. $\mathcal{C}_1$ y $\mathcal{C}_2$.)

(*) For this experiment, we compile as follows: gfortran -c src_unc.f
gfortran -c src_hes.f
gfortran -c subroutines_GENCAN.f90
gfortran -c conf_GENCAN.f90

(**) We link the code with the HSL library and the corresponding paths.:

gfortran -o gencan \
  conf_GENCAN.o \
  subroutines_GENCAN.o \
  src_unc.o \
  src_hes.o \
  /home/user/Descargas/algencan-3.1.1/lib/libalgencan.a \
  /home/user/Descargas/algencan-3.1.1/lib/libhsl.a \
  -fopenmp
  
Subsequently, we run the executable as follows::

./gencan nprob method, con metodo$\in {newton,tr,tn}$

(***) To execute the $R$ scripts in (5.1) and (5.2) and run IRACE, we use the following command in the terminal:, Rscript irace_GENCAN.R or alternatively irace_GENCAN_2.R. In this case, the runner will execute the program located at the specified path(our case). algencan-3.1.1: /home/user/Descargas/algencan-3.1.1/CODES_DISERTATION/Experiment2/gencan


---------------------------------------------------------
**EXPERIMENT 3: I-RACE for tuning GENCAN: II**
---------------------------------------------------------

For this experiment, the algorithm GENCAN from ALGENCAN version 4.0.0 was used, and IRACE was implemented to tune the parameters of interest defined in the dissertation. The training instances employed were taken from CUTEst for unconstrained problems and box-constrained problems.

The following files were required for the execution of this experiment:

1. gencan.f90
2. gencanma-forcutest.f90
3. run-unc-bound-contr
4. Irace_Gencan-4.0.0.R

In this case, the GENCAN algorithm is executed through the *run-unc-bound-constr* script. This script compiles the *ls* and *hsl* libraries, as well as the CUTEst interface, using gencanma-forcutest.f90. To do so, we first execute: chmod +x run-unc-bound-constr, this bash script compiles all the required libraries. Then, the algorithm is executed as follows: $\phi_1 \ \phi_2 \ \phi_3 \ \phi_4 \ \phi_5 \ \phi_6 \ \phi_7 \ \phi_8 \ \phi_9 \ \phi_{10}$ ./run-unc-bound-constr, where each $\phi_i$ represents a different parameter of the GENCAN algorithm, and following the order defined in gencanma-forcutest.f90. After compilation, we proceed to execute the $R$ script using the command:Rscript Irace_Gencan-4.0.0.R.

---------------------------------------------------------
**EXPERIMENT 4: I-RACE for tuning ALGENCAN**
---------------------------------------------------------

For this experiment, ALGENCAN version 4.0.0 was used, and IRACE was employed to tune the parameters of interest related to the penalty paremeter, defined in the dissertation. The training instances were selected from CUTEst and consisted of nonlinear optimization problems of the form $h(x)=0$, $g(x)\leq 0$, $x\in \Omega$.

The following files were required for the execution of this experiment.

1. algencan.f90
2. algencanma-forcutest.f90
3. run-nlp
4. Irace_Algencan-4.0.0.R

In this case, the ALGENCAN algorithm is executed through the run-nlp script. This script compiles the *ls* and *hsl* libraries, as well as the CUTEst interface, using algencanma-forcutest.f90. To do so, we first execute: chmod +x run-nlp. This bash compiles all the required libraries. The algorithm is then executed as follows: $\zeta_1$ $\zeta_2$ ./run-nlp problem, donde cada $\zeta_i$, represents a different parameter of the ALGENCAN algorithm. Once the compilation step is completed, we proceed to execute the R script using the command: Rscript Irace_Algencan-4.0.0.R.







