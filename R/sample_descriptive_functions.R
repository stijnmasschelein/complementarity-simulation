#' # Calculate variances

var1 = function(interval = c(-5, 5), b12 = 0, d = c(1, 1), g = c(.5, .5),
                s = c(.5, .5)){
  varx2 = (interval[2] - interval[1])^2 / 12;
  varz2 = (d[2] * g[1] + b12 * g[2])/(d[1] * d[2] - b12^2);
  return(varx2 + varz2)
}

var2 = function(interval = c(-5, 5), b12 = 0, d = c(1, 1), g = c(.5, .5),
                s = c(s1, s2)){
  vare = (d[2] * s[1] + b12 * s[2])/(d[1] * d[2] - b12^2)
  return(vare)
}

#' # Calculate the optimal level

calc_opt = function(b1 = c(0, 0), b12 = 0, d = c(1, 1), g = c(.5, .5), z = 0){
  x_star = (d[2] * b1[1] + b12 * b1[2] +  (d[2] * g[1] + b12 * g[2]) * z) /
           (d[1] * d[2] - b12^2)
  return(x_star)
}

#' # A single simulation

sim_vars = function(interval = c(-5, 5), obs = 200, rep = 1, rate = 0.5, 
                    sd = 1,
                    s = c(0, 0), b1 = c(0, 0, 0), b12 = .5,
                    d = c(1, 1), g = c(0, 0)){
  par_vars = list(interval = interval, b12 = b12, d = d, g = g, s = s)
  par_sims = list(obs = obs, rep = rep, rate = rate, sd = sd, sd_eps = c(s, 0),
                  b1 = c(b1, 0), b2 = c(b12, 0, 0), d = c(d, 0), g1 = c(g, 0))
  sample = do.call(simcompl2::create_sample, par_sims)
  sample$opt = calc_opt(b1 = b1[1:2], b12 = b12, d = d, g = g, sample$z)
  var_sample = mean((sample$x1 - sample$z)^2)
  var_random = do.call(var1, par_vars)
  var_optim  = do.call(var2, par_vars)
  r2 = (var_random - var_sample)/(var_random - var_optim)
  corxx = cor(sample$x1, sample$x2)
  corxz = cor(sample$x1, sample$z)
  return(list(var_sample = var_sample, var_random = var_random,
              var_optim = var_optim, r2 = r2, corxx = corxx,
              corxz = corxz))
}
