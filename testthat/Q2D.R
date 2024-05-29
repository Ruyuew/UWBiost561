# Load necessary packages
library(testthat)
library(UWBiost561)

# Source the simulation function file if needed
# source("path/to/your/run_simulation_function.R")

# Define the unit tests
test_that("run_simulation runs without errors for small input", {
  n_values <- c(10)
  clique_fractions <- c(0.5)
  edge_densities <- c(0.85)
  alpha <- 0.95
  trials <- 1
  time_limit <- 5
  
  result <- run_simulation(n_values, clique_fractions, edge_densities, alpha, trials, time_limit)
  
  expect_type(result, "list")
  expect_length(result, 1)
})

test_that("run_simulation output has correct structure", {
  n_values <- c(10)
  clique_fractions <- c(0.5)
  edge_densities <- c(0.85)
  alpha <- 0.95
  trials <- 1
  time_limit <- 5
  
  result <- run_simulation(n_values, clique_fractions, edge_densities, alpha, trials, time_limit)
  trial_result <- result[[1]][[1]]
  
  expect_true("time_out" %in% names(trial_result))
  expect_true("error" %in% names(trial_result))
  expect_true("valid" %in% names(trial_result))
  expect_true("size" %in% names(trial_result))
  expect_true("clique_idx" %in% names(trial_result))
})

test_that("run_simulation handles timeouts and errors correctly", {
  n_values <- c(10)
  clique_fractions <- c(0.5)
  edge_densities <- c(0.85)
  alpha <- 0.95
  trials <- 1
  time_limit <- 1 # Intentionally small to induce timeout
  
  result <- run_simulation(n_values, clique_fractions, edge_densities, alpha, trials, time_limit)
  trial_result <- result[[1]][[1]]
  
  expect_true(trial_result$time_out || trial_result$error)
})

test_that("run_simulation correctly validates partial cliques", {
  n_values <- c(10)
  clique_fractions <- c(0.5)
  edge_densities <- c(0.85)
  alpha <- 0.95
  trials <- 1
  time_limit <- 5
  
  result <- run_simulation(n_values, clique_fractions, edge_densities, alpha, trials, time_limit)
  trial_result <- result[[1]][[1]]
  
  if (!is.null(trial_result$clique_idx)) {
    adj_mat <- generate_partial_clique(n = 10, clique_fraction = 0.5, clique_edge_density = 0.85)$adj_mat
    correct_density <- compute_correct_density(adj_mat, trial_result$clique_idx, alpha)
    expect_equal(trial_result$valid, correct_density$valid)
  }
})
