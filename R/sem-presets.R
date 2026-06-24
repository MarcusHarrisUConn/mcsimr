sem_model_presets <- function() {
  data.frame(
    name = c(
      "one_factor_cfa",
      "two_factor_cfa",
      "structural_regression",
      "latent_mediation",
      "latent_growth",
      "bollen_political_democracy"
    ),
    title = c(
      "One-factor CFA",
      "Two-factor CFA",
      "Latent structural regression",
      "Latent mediation",
      "Latent growth model",
      "Bollen political democracy"
    ),
    description = c(
      "A single latent factor measured by four indicators with standardized residual variances.",
      "Two correlated latent factors, each measured by three indicators.",
      "Two latent variables with a structural regression path.",
      "Three latent variables with indirect and direct structural paths.",
      "A four-wave linear growth model with intercept and slope factors.",
      "The classic Political Democracy SEM example from lavaan."
    ),
    stringsAsFactors = FALSE
  )
}

sem_model_preset <- function(name = "one_factor_cfa") {
  presets <- sem_model_presets()
  name <- match.arg(name, presets$name)

  out <- switch(
    name,
    one_factor_cfa = list(
      name = name,
      title = "One-factor CFA",
      description = presets$description[presets$name == name],
      population_model = paste(
        "f =~ 0.70*y1 + 0.80*y2 + 0.90*y3 + 0.50*y4",
        "f ~~ 1*f",
        "y1 ~~ 0.51*y1",
        "y2 ~~ 0.36*y2",
        "y3 ~~ 0.19*y3",
        "y4 ~~ 0.75*y4",
        sep = "\n"
      ),
      fitted_model = "f =~ y1 + y2 + y3 + y4",
      parameter_conditions = sem_parameter_conditions(
        lhs = c("f", "f"),
        op = c("=~", "~~"),
        rhs = c("y2", "f"),
        values = list(c(0.60, 0.80), c(0.80, 1.00))
      ),
      builder = list(
        factor_names = "f",
        indicator_map = "f: y1, y2, y3, y4",
        loading_map = "f: 0.70, 0.80, 0.90, 0.50",
        factor_covariances = "",
        structural_paths = ""
      )
    ),
    two_factor_cfa = list(
      name = name,
      title = "Two-factor CFA",
      description = presets$description[presets$name == name],
      population_model = paste(
        "f1 =~ 0.70*y1 + 0.80*y2 + 0.75*y3",
        "f2 =~ 0.65*y4 + 0.85*y5 + 0.70*y6",
        "f1 ~~ 1*f1",
        "f2 ~~ 1*f2",
        "f1 ~~ 0.30*f2",
        "y1 ~~ 0.51*y1",
        "y2 ~~ 0.36*y2",
        "y3 ~~ 0.4375*y3",
        "y4 ~~ 0.5775*y4",
        "y5 ~~ 0.2775*y5",
        "y6 ~~ 0.51*y6",
        sep = "\n"
      ),
      fitted_model = paste(
        "f1 =~ y1 + y2 + y3",
        "f2 =~ y4 + y5 + y6",
        "f1 ~~ f2",
        sep = "\n"
      ),
      parameter_conditions = sem_parameter_conditions(
        lhs = c("f2", "f1"),
        op = c("=~", "~~"),
        rhs = c("y5", "f2"),
        values = list(c(0.70, 0.85), c(0.10, 0.30, 0.50))
      ),
      builder = list(
        factor_names = "f1, f2",
        indicator_map = "f1: y1, y2, y3\nf2: y4, y5, y6",
        loading_map = "f1: 0.70, 0.80, 0.75\nf2: 0.65, 0.85, 0.70",
        factor_covariances = "f1 ~~ 0.30*f2",
        structural_paths = ""
      )
    ),
    structural_regression = list(
      name = name,
      title = "Latent structural regression",
      description = presets$description[presets$name == name],
      population_model = paste(
        "x =~ 0.80*x1 + 0.75*x2 + 0.70*x3",
        "y =~ 0.70*y1 + 0.80*y2 + 0.75*y3",
        "y ~ 0.45*x",
        "x ~~ 1*x",
        "y ~~ 0.7975*y",
        "x1 ~~ 0.36*x1",
        "x2 ~~ 0.4375*x2",
        "x3 ~~ 0.51*x3",
        "y1 ~~ 0.51*y1",
        "y2 ~~ 0.36*y2",
        "y3 ~~ 0.4375*y3",
        sep = "\n"
      ),
      fitted_model = paste(
        "x =~ x1 + x2 + x3",
        "y =~ y1 + y2 + y3",
        "y ~ x",
        sep = "\n"
      ),
      parameter_conditions = sem_parameter_conditions(
        lhs = c("y", "y"),
        op = c("~", "~~"),
        rhs = c("x", "y"),
        values = list(c(0.25, 0.45, 0.65), c(0.60, 0.80))
      ),
      builder = list(
        factor_names = "x, y",
        indicator_map = "x: x1, x2, x3\ny: y1, y2, y3",
        loading_map = "x: 0.80, 0.75, 0.70\ny: 0.70, 0.80, 0.75",
        factor_covariances = "",
        structural_paths = "y ~ 0.45*x"
      )
    ),
    latent_mediation = list(
      name = name,
      title = "Latent mediation",
      description = presets$description[presets$name == name],
      population_model = paste(
        "x =~ 0.80*x1 + 0.75*x2 + 0.70*x3",
        "m =~ 0.75*m1 + 0.80*m2 + 0.70*m3",
        "y =~ 0.70*y1 + 0.80*y2 + 0.75*y3",
        "m ~ 0.40*x",
        "y ~ 0.35*m + 0.20*x",
        "x ~~ 1*x",
        "m ~~ 0.84*m",
        "y ~~ 0.78*y",
        "x1 ~~ 0.36*x1",
        "x2 ~~ 0.4375*x2",
        "x3 ~~ 0.51*x3",
        "m1 ~~ 0.4375*m1",
        "m2 ~~ 0.36*m2",
        "m3 ~~ 0.51*m3",
        "y1 ~~ 0.51*y1",
        "y2 ~~ 0.36*y2",
        "y3 ~~ 0.4375*y3",
        sep = "\n"
      ),
      fitted_model = paste(
        "x =~ x1 + x2 + x3",
        "m =~ m1 + m2 + m3",
        "y =~ y1 + y2 + y3",
        "m ~ x",
        "y ~ m + x",
        sep = "\n"
      ),
      parameter_conditions = sem_parameter_conditions(
        lhs = c("m", "y", "y"),
        op = c("~", "~", "~"),
        rhs = c("x", "m", "x"),
        values = list(c(0.25, 0.40), c(0.20, 0.35, 0.50), c(0, 0.20))
      ),
      builder = list(
        factor_names = "x, m, y",
        indicator_map = "x: x1, x2, x3\nm: m1, m2, m3\ny: y1, y2, y3",
        loading_map = "x: 0.80, 0.75, 0.70\nm: 0.75, 0.80, 0.70\ny: 0.70, 0.80, 0.75",
        factor_covariances = "",
        structural_paths = "m ~ 0.40*x\ny ~ 0.35*m + 0.20*x"
      )
    ),
    latent_growth = list(
      name = name,
      title = "Latent growth model",
      description = presets$description[presets$name == name],
      population_model = paste(
        "i =~ 1*y1 + 1*y2 + 1*y3 + 1*y4",
        "s =~ 0*y1 + 1*y2 + 2*y3 + 3*y4",
        "i ~~ 1*i",
        "s ~~ 0.25*s",
        "i ~~ 0.20*s",
        "y1 ~~ 0.50*y1",
        "y2 ~~ 0.50*y2",
        "y3 ~~ 0.50*y3",
        "y4 ~~ 0.50*y4",
        sep = "\n"
      ),
      fitted_model = paste(
        "i =~ 1*y1 + 1*y2 + 1*y3 + 1*y4",
        "s =~ 0*y1 + 1*y2 + 2*y3 + 3*y4",
        "i ~~ s",
        sep = "\n"
      ),
      parameter_conditions = sem_parameter_conditions(
        lhs = c("s", "i"),
        op = c("~~", "~~"),
        rhs = c("s", "s"),
        values = list(c(0.10, 0.25, 0.50), c(0, 0.20))
      ),
      builder = list(
        factor_names = "i, s",
        indicator_map = "i: y1, y2, y3, y4\ns: y1, y2, y3, y4",
        loading_map = "i: 1, 1, 1, 1\ns: 0, 1, 2, 3",
        factor_covariances = "i ~~ 0.20*s",
        structural_paths = ""
      )
    ),
    bollen_political_democracy = list(
      name = name,
      title = "Bollen political democracy",
      description = presets$description[presets$name == name],
      population_model = paste(
        "ind60 =~ 1*x1 + 2.180*x2 + 1.819*x3",
        "dem60 =~ 1*y1 + 1.257*y2 + 1.058*y3 + 1.265*y4",
        "dem65 =~ 1*y5 + 1.186*y6 + 1.280*y7 + 1.266*y8",
        "dem60 ~ 1.483*ind60",
        "dem65 ~ 0.572*ind60 + 0.837*dem60",
        "ind60 ~~ 0.448*ind60",
        "dem60 ~~ 3.956*dem60",
        "dem65 ~~ 0.172*dem65",
        "y1 ~~ 1.892*y5",
        "y2 ~~ 7.373*y4",
        "y2 ~~ 2.488*y6",
        "y3 ~~ 5.067*y7",
        "y4 ~~ 1.706*y8",
        "x1 ~~ 0.082*x1",
        "x2 ~~ 0.120*x2",
        "x3 ~~ 0.467*x3",
        "y1 ~~ 1.891*y1",
        "y2 ~~ 7.373*y2",
        "y3 ~~ 5.067*y3",
        "y4 ~~ 3.148*y4",
        "y5 ~~ 2.351*y5",
        "y6 ~~ 4.954*y6",
        "y7 ~~ 3.431*y7",
        "y8 ~~ 3.254*y8",
        sep = "\n"
      ),
      fitted_model = paste(
        "ind60 =~ x1 + x2 + x3",
        "dem60 =~ y1 + y2 + y3 + y4",
        "dem65 =~ y5 + y6 + y7 + y8",
        "dem60 ~ ind60",
        "dem65 ~ ind60 + dem60",
        "y1 ~~ y5",
        "y2 ~~ y4 + y6",
        "y3 ~~ y7",
        "y4 ~~ y8",
        sep = "\n"
      ),
      parameter_conditions = sem_parameter_conditions(
        lhs = c("dem65", "ind60", "dem60"),
        op = c("~", "=~", "~~"),
        rhs = c("dem60", "x2", "dem60"),
        values = list(c(0.65, 0.85), c(1.80, 2.20), c(3.00, 4.00))
      ),
      builder = list(
        factor_names = "ind60, dem60, dem65",
        indicator_map = "ind60: x1, x2, x3\ndem60: y1, y2, y3, y4\ndem65: y5, y6, y7, y8",
        loading_map = "ind60: 1, 2.180, 1.819\ndem60: 1, 1.257, 1.058, 1.265\ndem65: 1, 1.186, 1.280, 1.266",
        factor_covariances = "y1 ~~ 1.892*y5\ny2 ~~ 7.373*y4\ny2 ~~ 2.488*y6\ny3 ~~ 5.067*y7\ny4 ~~ 1.706*y8",
        structural_paths = "dem60 ~ 1.483*ind60\ndem65 ~ 0.572*ind60 + 0.837*dem60"
      )
    )
  )

  class(out) <- c("mcsimr_sem_preset", "list")
  out
}
