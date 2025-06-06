#' Backtesting Portfolio Weights (with Preloaded Data)
#'
#' This version pre-slices feature matrices and windows once, so that each parallel job avoids repeated data-frame slicing.
#' @export
backtesting_weights_new <- function(
    data,
    return_label,
    benchmark_label = NULL,
    mask_label = NULL,
    features,
    pf_config,
    portfolio_object = NULL,
    rolling = TRUE,
    window_size = "5 years",
    step_size = "1 month",
    offset = "1 month",
    in_sample = TRUE,
    num_cores = NULL,
    verbose = FALSE
) {
  # 1) Input validation
  checkmate::assert_data_frame(data)
  checkmate::assert_string(return_label)
  checkmate::assert_character(features)
  checkmate::assert_list(pf_config)
  if (!is.null(benchmark_label)) checkmate::assert_string(benchmark_label)
  if (!is.null(mask_label))      checkmate::assert_string(mask_label)
  if (!is.null(num_cores))       checkmate::assert_integerish(num_cores, lower = 1)

  # 2) Initialize portfolio object
  if (is.null(portfolio_object)) {
    portfolio_object <- if (!is.null(benchmark_label)) {
      create_portfolioReturns(data, return_label, benchmark_label)
    } else {
      create_portfolioReturns(data, return_label)
    }
    if (verbose) cli::cli_alert_info("Created portfolioReturns object.")
  }

  # 3) Prepare data_post
  data2 <- data
  # mask
  if (is.null(mask_label)) {
    data2 <- dplyr::mutate(data2, mask = 1)
  } else {
    data2 <- dplyr::rename(data2, mask = !!rlang::sym(mask_label))
  }
  # benchmark
  if (is.null(benchmark_label)) {
    data2 <- dplyr::mutate(data2, benchmark = 0)
  } else {
    data2 <- dplyr::rename(data2, benchmark = !!rlang::sym(benchmark_label))
  }
  # returns
  data2 <- dplyr::rename(data2, ret = !!rlang::sym(return_label))
  # subset columns
  data_post <- dplyr::select(data2,
                             stock_id, date, benchmark, ret, dplyr::all_of(features), mask
  )

  # 4) Convert to matrices/vectors
  X_mat     <- as.matrix(data_post[, features, drop = FALSE])
  y_vec     <- data_post$ret
  dates_vec <- data_post$date

  # 5) Compute rolling-window indices
  indices <- select_dates_by_offset(dates_vec, window_size, step_size, offset, rolling)
  if (in_sample) {
    is_idx <- tibble::tibble(
      training_start   = indices$training_start[1],
      training_end     = indices$training_end[1],
      prediction_start = indices$training_start[1],
      prediction_end   = indices$prediction_start[1]
    )
    indices <- dplyr::bind_rows(
      dplyr::mutate(is_idx, prediction_phase = "IS"),
      indices
    )
  }
# store the constituents to compare




  # 6) Pre-slice all windows into list
  windows_list <- lapply(seq_len(nrow(indices)), function(i) {
    idx        <- indices[i, ]
    train_rows <- which(dates_vec >= idx$training_start & dates_vec <= idx$training_end)
    test_rows  <- which(dates_vec >= idx$prediction_start & dates_vec <= idx$prediction_end)
    list(
      train_x    = X_mat[train_rows, , drop = FALSE],
      train_y    = y_vec[train_rows],
      train_meta = data_post[train_rows, c("stock_id", "date", "benchmark")],
      test_x     = X_mat[test_rows, , drop = FALSE],
      test_meta  = data_post[test_rows, c("stock_id", "date")]
    )
  })

  # 7) Parallel plan
  if (!is.null(num_cores)) {
    future::plan(multicore, workers = num_cores) # originally mulicore instead of multisession
  } else {
    future::plan(sequential)
  }
  if (verbose) cli::cli_alert_info("Parallel plan set to {ifelse(is.null(num_cores), 'sequential', paste(num_cores, 'cores'))}.")

  # 8) Loop models/configs
  for (model_name in names(pf_config)) {
    specs       <- pf_config[[model_name]]
    weight_func <- specs$weight_func
    for (cfg_name in setdiff(names(specs), 'weight_func')) {
      model_config <- specs[[cfg_name]]
      if (verbose) cli::cli_alert_info("Running {model_name}:{cfg_name} over {length(windows_list)} windows...")

      # 9) Compute predictions in parallel
      weight_predictions <- furrr::future_map_dfr(
        seq_along(windows_list),
        function(i) {
          w <- windows_list[[i]]
          preds <- do.call(
            weight_func,
            list(
              train_x     = w$train_x,
              train_y     = w$train_y,
              train_meta  = w$train_meta,
              test_x      = w$test_x,
              test_meta   = w$test_meta,
              model_config= model_config
            )
          )
          preds
        },
        .options = furrr::furrr_options(seed = TRUE)
      )

      # 10) Add to portfolio object
      portfolio_object <- add_weight_model(
        portfolio_object,
        model_name,
        weight_predictions,
        c(model_config, list(indices = indices))
      )
      if (verbose) cli::cli_alert_success("Added weights for {model_name}:{cfg_name}.")
    }
  }

  # 11) Reset plan
  future::plan(sequential)
  if (verbose) cli::cli_alert_info("Parallel plan reset to sequential.")

  return(portfolio_object)
}





keras_weights_new <- function(
    train_x,
    train_y,
    train_meta,
    test_x,
    test_meta,
    model_config = list()
) {
  #--- Input validation -------------------------------------------------------
  checkmate::assert_matrix(train_x, mode = "numeric")
  checkmate::assert_numeric(train_y, min.len = nrow(train_x), any.missing = FALSE)
  checkmate::assert_data_frame(train_meta, min.rows = nrow(train_x))
  checkmate::assert_matrix(test_x, mode = "numeric")
  checkmate::assert_data_frame(test_meta, min.rows = nrow(test_x))
  checkmate::assert_list(model_config)

  #--- Extract shapes and metadata --------------------------------------------
  input_shape <- ncol(train_x)
  seeds       <- model_config$seeds %||% 0L

  #--- Build and train for one seed ------------------------------------------
  train_model <- function(seed) {
    keras3::set_random_seed(seed)

    # Construct the sequential model
    model <- keras_model_sequential(input_shape = list(as.integer(input_shape)))
    for (layer_cfg in model_config$layers) {
      if (layer_cfg$type == "dense") {
        model %>%
          layer_dense(
            units       = layer_cfg$units,
            activation  = layer_cfg$activation,
            kernel_initializer = layer_cfg$kernel_initializer %||% NULL,
            kernel_constraint  = layer_cfg$kernel_constraint  %||% NULL,
            bias_initializer   = layer_cfg$bias_initializer   %||% NULL,
            kernel_regularizer = layer_cfg$kernel_regularizer %||% NULL
          )
        if (!is.null(layer_cfg$dropout)) {
          model %>% layer_dropout(rate = layer_cfg$dropout)
        }
      }
    }

    # Optimizer
    opt_name <- model_config$optimizer$name
    opt      <- do.call(match.fun(opt_name), model_config$optimizer[-1])

    # Loss
    loss_name <- model_config$loss$name
    loss_fn   <- do.call(match.fun(loss_name), model_config$loss[-1])

    # Compile
    model %>% compile(
      optimizer = opt,
      loss      = loss_fn,
      metrics   = model_config$metrics %||% list()
    )

    # Fit
    history <- model %>% fit(
      x        = train_x,
      y        = as.matrix(cbind(
        stock_id = train_meta$stock_id,
        date     = as.numeric(train_meta$date),
        benchmark= train_meta$benchmark,
        ret      = train_y
      )),
      epochs        = model_config$epochs,
      batch_size    = model_config$batch_size %||% nrow(train_x),
      verbose       = model_config$verbose %||% 0,
      callbacks     = model_config$callbacks %||% list()
    )
    list(model = model, history = history)
  }

  #--- Generate predictions across seeds --------------------------------------
  pred_lists <- lapply(seeds, function(s) {
    res <- train_model(s)
    preds <- predict(res$model, test_x, batch_size = nrow(test_x), verbose = 0)
    as.vector(preds)
  })

  # Average if multiple seeds
  if (length(pred_lists) > 1) {
    pred_vec <- Reduce(`+`, pred_lists) / length(pred_lists)
  } else {
    pred_vec <- pred_lists[[1]]
  }

  #--- Return tibble ----------------------------------------------------------
  tibble::tibble(
    stock_id   = test_meta$stock_id,
    date       = test_meta$date,
    pred_weight= pred_vec
  )
}
