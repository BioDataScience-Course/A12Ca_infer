# Functions to calculate scores for inference analyses
# Copyright (c) 2024 Ph. Grosjean (phgrosjean@sciviews.org) & Guyliann Engels

repository <- sub("\\.git$", "", basename(usethis::git_remotes()$origin))
svMisc::assign_temp("scores", numeric(10L), replace.existing = TRUE)
reference_dir <- fs::path("figures")
output_dir <- fs::path("challenge_infer_files", "figure-html")
results_dir <- fs::path("results")
unlink(dir(output_dir, pattern = "\\.png$"))

#' Calculate the score for one chart or table
#'
#' @param infer The number from 1 to 10 of the inference analysis to score
#' @param ref_dir The directory containing the reference image
#' @param out_dir The directory containing the produced chart
#'
#' @return A score from 0 to 1
#' @export
#'
#' @examples
#' score_infer(1) # Get score for first inference analysis
score_infer <- function(infer, ref_dir = reference_dir, out_dir = output_dir) {
  infer <- as.integer(infer)[1]
  if (infer < 1 | infer > 10)
    stop("argument infer = must be an integer between 1 and 10")
  # Get vector of scores
  scores <- svMisc::get_temp("scores", default = NULL)
  if (is.null(scores))
    stop("scores not found")
  # Filename
  inferfile <- sprintf("infer%02.0f-1.png", infer)
  # Read reference infer
  reffile <- file.path(ref_dir, inferfile)
  if (!file.exists(reffile)) {
    warning("The reference image for infer ", infer, " is not found")
    score <- NA
  } else {
    ref <- png::readPNG(reffile)[, , 1:3] # Do not use alpha channel
    # Is an inference analysis produced?
    outfile <- file.path(out_dir, inferfile)
    if (file.exists(outfile)) {
      out <- png::readPNG(outfile)[, , 1:3] # Do not use alpha channel
      score <- try(cor(ref, out), silent = TRUE)
      if (inherits(score, "try-error")) {
        score <- 0
      } else {# If score is too low, we consider it is a different item
        if (score < 0.8)
          score <- 0
      }
    } else {
      score <- 0
    }
  }
  # Record the score and return it
  scores[infer] <- score
  svMisc::assign_temp("scores", scores, replace.existing = TRUE)
  score
}


#' Calculate the global score for all inference analyses
#'
#' @param res_dir The directory where to place the results
#' @param repos The current GitHub repository
#'
#' @return
#' @export
#'
#' @examples
score_all_infer <- function(res_dir = results_dir, repos = repository) {
  scores <- svMisc::get_temp("scores", default = NULL)
  if (is.null(scores))
    stop("scores not found")
  # Get an id and file name, according to current files in results
  all_res <- dir(res_dir, full.names = FALSE,
    pattern = paste0("^", repos, "__[0-9]{3}\\.rds$"))
  if (!length(all_res)) {
    id <- "001"
  } else {
    last_res <- sort(all_res, decreasing = TRUE)[1]
    last_id <-
      sub("^.+__([0-9]{3})\\.rds$", "\\1", last_res)
    id <- sprintf("%03.0f", as.integer(last_id) + 1)
  }
  file <- glue::glue("{repos}__{id}.rds")
  attr(scores, "id") <- id
  attr(scores, "file") <- file
  # Save the results file (only if score > 0)
  if (!any(is.na(scores)) && sum(scores) > 0) {
    resfile <- file.path(res_dir, file)
    write$rds(scores, file = resfile)
  }
  scores
}


# A hook to save the result of evaluations in chunks as .Last.chunk (if printed
# because things returned invisibly are not recorded)
knitr::opts_chunk$set(render = function(x, ...){
  svMisc::assign_temp(".Last.chunk", x, replace.existing = TRUE)
  knitr::knit_print(x, ...)
})

# A hook to save results after a code chunk is evaluated
knitr::knit_hooks$set(record_table = function(before, options, envir) {
  if (!before) {
    if (isTRUE(options$record_table)) { # Record a flextable object
      png_name <- paste0("challenge_infer_files/figure-html/", options$label, "-1.png")
      if (!fs::dir_exists("challenge_infer_files/figure-html"))
        fs::dir_create("challenge_infer_files/figure-html")
      object <- get0(".Last.chunk")
      if (inherits(object, "flextable")) {
        message("Recording ", basename(png_name))
        flextable::save_as_image(object, path = png_name)
      }
    }
    # Make sure we do not recall an old .Last.chunk flextable object next time
    svMisc::assign_temp(".Last.chunk", NULL, replace.existing = TRUE)
    NULL
  }
})
