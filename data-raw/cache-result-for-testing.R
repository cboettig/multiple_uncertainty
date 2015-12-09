escapement_cache <- multipleuncertainty::multiple_uncertainty(x_grid = seq(0, 150, length = 151), 
                                   h_grid = seq(0, 150, length = 152), 
                                   y_grid = seq(0, 150, length = 153), 
                                   q_grid = seq(0, 150, length = 154),
                                   sigma_g = 0.5, 
                                   sigma_m = 0.1, 
                                   sigma_i = 0.1)

devtools::use_data(escapement_cache, internal=TRUE)