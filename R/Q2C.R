# Load necessary libraries
library(UWBiost561)

# Define the simulation function
run_simulation <- function(n_values, clique_fractions, edge_densities, alpha, trials, time_limit = 30) {
  results <- list()
  
  for (n in n_values) {
    for (clique_fraction in clique_fractions) {
      for (edge_density in edge_densities) {
        for (trial in 1:trials) {
          # Set seed for reproducibility
          set.seed(trial)
          
          # Generate the random adjacency matrix
          data <- generate_partial_clique(n = n, clique_fraction = clique_fraction, clique_edge_density = edge_density)
          
          # Initialize a list to store results for this trial
          trial_results <- list()
          
          for (method in 1:25) {
            # Run the method with a time limit
            result <- tryCatch(
              compute_maximal_partial_clique_master(
                adj_mat = data$adj_mat, 
                alpha = alpha, 
                number = method, 
                time_limit = time_limit
              ),
              error = function(e) list(clique_idx = NULL, time_out = TRUE, error = TRUE)
            )
            
            # Validate the result
            if (!is.null(result$clique_idx)) {
              correct_density <- compute_correct_density(data$adj_mat, result$clique_idx, alpha)
              if (!correct_density$valid) {
                result$valid = FALSE
              } else {
                result$valid = TRUE
              }
            } else {
              result$valid = FALSE
            }
            
            # Store the result
            trial_results[[method]] <- list(
              time_out = ifelse(is.null(result$time_out), FALSE, result$time_out),
              error = ifelse(is.null(result$error), FALSE, result$error),
              valid = result$valid,
              size = ifelse(result$valid, length(result$clique_idx), NA),
              clique_idx = result$clique_idx
            )
          }
          
          # Store the results for this trial
          results[[paste0("n=", n, "_cf=", clique_fraction, "_ed=", edge_density, "_trial=", trial)]] <- trial_results
        }
      }
    }
  }
  
  return(results)
}

# Example usage of the function
n_values <- c(30, 50)
clique_fractions <- c(0.5)
edge_densities <- c(0.85, 0.95)
alpha <- 0.95
trials <- 2  # Use 2 trials for simplicity in testing

simulation_results <- run_simulation(n_values, clique_fractions, edge_densities, alpha, trials, time_limit = 10)

# Print a summary of the results
print(simulation_results)
