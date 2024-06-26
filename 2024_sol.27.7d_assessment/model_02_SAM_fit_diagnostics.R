

## Before: read SAM inputs
## After: fit model and create diagnostics

library(stockassessment)
load(file = "model/SAM_dat_conf_par.RData")

SAM_fit_sol_7d    <- sam.fit(dat.sol7d, conf, par)


# Convergence checks
SAM_fit_sol_7d$opt$convergence          # 0 = convergence
SAM_fit_sol_7d$opt$message              # 4 = ok

# AIC en log likelihood
AIC(SAM_fit_sol_7d)  #960.1803
logLik(SAM_fit_sol_7d) # -454.0901 (df=26)

# Save SAM fit object to the model directory
save(SAM_fit_sol_7d, file= file.path("model","SAM_fit.RData"))

sim_study <- F
jit_fit   <- F

# compute OSA residuals
OSA.res        <- residuals(SAM_fit_sol_7d)

# compute joint sample residuals or process residuals 
process.res    <- procres(SAM_fit_sol_7d)

# run retrospecitive analysis with 5 peels
retro.fits     <- retro(SAM_fit_sol_7d ,year=5)

# run leave-one-out fits
leaveout.fits  <- leaveout(SAM_fit_sol_7d)

# eventually run the simulation and jitter analysis
if(sim_study){
  sim.fits <- simstudy(SAM_fit_sol_7d, nsim = 100)
} else {
  sim.fits <- NULL
}

if(jit_fit){
  jit.fits <- jit(SAM_fit_sol_7d, nojit = 100)
} else {
  jit.fits <- NULL
}

diagnostics <- list(
  "OSA_residuals" = OSA.res,
  "process_residuals" = process.res,
  "retrospective_analysis" = retro.fits,
  "leave_one_out_fits" = leaveout.fits
)

if(!is.null(sim.fits)) diagnostics <- append(diagnostics, list("simulation_study" = sim.fits))
if(!is.null(jit.fits)) diagnostics <- append(diagnostics, list("simulation_study" = jit.fits))

save(diagnostics, file = "model/SAM_diagnostics.RData")