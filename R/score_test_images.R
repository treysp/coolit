#' Score training test-set images
#'
#' @param img_dir Directory containing images to score
#'
#' @param model Keras model to use for scoring
#'
#' @export
#' @importFrom keras image_to_array image_load array_reshape predict_proba
#'             keras_model
#' @importFrom pbapply pblapply
#' @importFrom abind abind
#' @importFrom utils write.csv
score_test_images <- function(img_dir, model) {
before <- Sys.time()

valid_files <- list.files(params$valid_dir, full.names = TRUE, recursive = TRUE)

img_dims <- dim(
  keras::image_to_array(
    keras::image_load(valid_files[1])
    )
  )

img_to_score <- pblapply(valid_files, function(img) {
  out <- image_load(img)
  out <- image_to_array(out)
  out <- keras::array_reshape(out, c(1, img_dims))
  out <- out / 255
  out
})

img_to_score <- abind::abind(img_to_score, along = 1)

predicted_probs <- data.frame(pred_prob = keras::predict_proba(model, img_to_score))
predicted_probs[["img_name"]] <- valid_files
predicted_probs[["truth"]] <- as.numeric(!stringr::str_detect(valid_files, "notower"))

utils::write.csv(predicted_probs,
                 file = file.path(params$curr_model_dir, "predicted-probs.csv"),
                 row.names = FALSE)

params$scoring_time <- Sys.time() - before
message("Image scoring took ",
        round(params$scoring_time, 3),
        " ",
        attr(params$scoring_time, "units"),
        " to train.")

# # score final layer activations if small layer present
# if (params$add_small_final_layer) {
#   layer_output <- model$layers[[7]]$output
#
#   activation_model <- keras::keras_model(inputs = model$input, outputs = layer_output)
#
#   my_activations <- activation_model %>%
#     predict(img_to_score)
#
#   img_activations <- as.data.frame(my_activations)
#   names(img_activations) <- paste0("neuron", seq_len(length(img_activations)))
#   img_activations[["img_name"]] <- valid_files
#   img_activations[["truth"]] <- as.numeric(!stringr::str_detect(valid_files, "notower"))
#
#   utils::write.csv(img_activations,
#                    file = file.path(params$curr_model_dir, "final-layer-activations.csv"),
#                    row.names = FALSE)
# }

# #### examine scored test images ----------------------------
# predicted_probs <- read.csv(file.path(params$curr_model_dir, "predicted-probs.csv"),
#                             stringsAsFactors = FALSE)
#
# pred <- prediction(predicted_probs$pred_prob, predicted_probs$truth)
#
# message("\nFinal model performance measures:\n",
#         "   ROC AUC = ",
#         round(performance(pred, "auc")@y.values[[1]], 3), "\n",
#         "   Max possible accuracy = ",
#         round(max(performance(pred, "acc")@y.values[[1]]), 3)
# )
#
# ggplot(predicted_probs) +
#   geom_histogram(aes(x = pred_prob, y = ..count../sum(..count..),
#                      fill = factor(truth)),
#                  binwidth = .01, color = NA, alpha = .4) +
#   scale_fill_brewer(palette = "Dark2",
#                     guide = guide_legend(title = "Truth")) +
#   ggtitle("Model predicted probabilities for test set, by actual tower presence") +
#   xlab("Predicted probability") +
#   ylab("Proportion of all images") +
#   theme_minimal()
}