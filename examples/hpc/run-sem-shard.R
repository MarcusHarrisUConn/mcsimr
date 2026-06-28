library(mcsimr)

shard_id <- as.integer(Sys.getenv("MCSIMR_SHARD_ID", "1"))
shards <- as.integer(Sys.getenv("MCSIMR_SHARDS", "1"))
workers <- as.integer(Sys.getenv("MCSIMR_WORKERS", "1"))

preset <- sem_model_preset("latent_mediation")

spec <- sem_sim_spec(
  population_model = preset$population_model,
  fitted_model = preset$fitted_model,
  n = c(100, 250, 500),
  reps = 1000,
  estimator = "ML",
  parameter_conditions = preset$parameter_conditions,
  missing = "fiml",
  missing_rate = c(0, 0.10),
  missing_mechanism = c("mcar", "mar", "mnar"),
  missing_targets = c("m1", "m2", "m3", "y1", "y2", "y3"),
  missing_driver = "x1",
  skewness = c(0, 1),
  kurtosis = c(0, 2),
  seed = 20260608
)

message("Running mcsimr shard ", shard_id, " of ", shards, " with ", workers, " worker(s).")
print(condition_shards(spec, shards = shards))

study <- run_simulation_shard(
  spec,
  shard_id = shard_id,
  shards = shards,
  workers = workers,
  checkpoint_dir = "results/checkpoints/sem-shards",
  resume = TRUE,
  output_dir = "results/sem-shards"
)

print(study$failure_summary)
print(study$runtime_estimate)
