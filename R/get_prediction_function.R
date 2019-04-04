get_prediction_function = function(model) {
  UseMethod("get_prediction_function", model)
}

get_prediction_function.default = function(model) {
  function(object, newdata) predict(object, newdata = newdata)
}

get_prediction_function.WrappedModel = function(model) {
  task_type = mlr::getTaskType(model)
  if (task_type == "regr") {
    predict_fun = function(object, newdata)
      mlr::getPredictionResponse(predict(object, newdata = newdata))
  } else if (task_type == "classif") {
    predict_fun = function(object, newdata)
      mlr::getPredictionProbabilities(predict(object, newdata = newdata))
  } else stop("Task type not supported.")
  predict_fun
}

get_prediction_function.train = function(model) {
  task_type = model$modelType
  if (task_type == "Regression") {
    predict_fun = function(object, newdata)
      predict(object, newdata = newdata, type = "raw")
  } else if (task_type == "Classification") {
    base_level = model$finalModel$obsLevels[1]
    predict_fun = function(object, newdata)
      predict(object, newdata = newdata, type = "prob")[[base_level]]
  }
}

get_prediction_function.randomForest = function(model) {
  task_type = model$type
  if (task_type == "regression") {
    predict_fun = function(object, newdata)
      predict(object, newdata = newdata)
  } else if (task_type == "classification") {
    base_level = model$classes[1]
    predict_fun = function(object, newdata)
      predict(object, newdata = newdata, type = "prob")[, base_level]
  }
}

get_prediction_function.gbm = function(model) {
  task_type = model$distribution$name
  n.trees = model$n.trees
  if (task_type == "gaussian") {
    predict_fun = function(object, newdata)
      predict(object, newdata = newdata, n.trees = n.trees)
  } else if (task_type == "bernoulli") {
    predict_fun = function(object, newdata)
      predict(model, newdata = newdata, type = "response", n.trees = n.trees)
  } else {
    stop("Can't infer task type of this model.
      Please provide predict_fun argument.")
  }
}

get_prediction_function.svm = function(model) {
  task_type = model$type
  if (task_type %in% 3:4) {
    predict_fun = function(object, newdata)
      predict(object, newdata = newdata)
  } else if (task_type %in% 0:2) {
    base_level = model$levels[1]
    if (!model$compprob) stop("Need probabilities. Retrain svm() with probability=TRUE.")
    predict_fun = function(object, newdata) {
      pred_obj = predict(model, newdata = newdata, probability = TRUE)
      attributes(pred_obj)$probabilities[, base_level]
    }
  } else {
    stop("Can't infer task type of this model.
      Please provide predict_fun argument.")
  }
}
get_prediction_function = function(model) {
  UseMethod("get_prediction_function", model)
}

get_prediction_function.default = function(model) {
  function(object, newdata) predict(object, newdata = newdata)
}

get_prediction_function.WrappedModel = function(model) {
  task_type = mlr::getTaskType(model)
  if (task_type == "regr") {
    predict_fun = function(object, newdata)
      mlr::getPredictionResponse(predict(object, newdata = newdata))
  } else if (task_type == "classif") {
    predict_fun = function(object, newdata)
      mlr::getPredictionProbabilities(predict(object, newdata = newdata))
  } else stop("Task type not supported.")
  predict_fun
}

get_prediction_function.train = function(model) {
  task_type = model$modelType
  if (task_type == "Regression") {
    predict_fun = function(object, newdata)
      predict(object, newdata = newdata, type = "raw")
  } else if (task_type == "Classification") {
    base_level = model$finalModel$obsLevels[1]
    predict_fun = function(object, newdata)
      predict(object, newdata = newdata, type = "prob")[[base_level]]
  }
}

get_prediction_function.randomForest = function(model) {
  task_type = model$type
  if (task_type == "regression") {
    predict_fun = function(object, newdata)
      predict(object, newdata = newdata)
  } else if (task_type == "classification") {
    base_level = model$classes[1]
    predict_fun = function(object, newdata)
      predict(object, newdata = newdata, type = "prob")[, base_level]
  }
}

get_prediction_function.gbm = function(model) {
  task_type = model$distribution$name
  n.trees = model$n.trees
  if (task_type == "gaussian") {
    predict_fun = function(object, newdata)
      predict(object, newdata = newdata, n.trees = n.trees)
  } else if (task_type == "bernoulli") {
    predict_fun = function(object, newdata)
      predict(model, newdata = newdata, type = "response", n.trees = n.trees)
  } else {
    stop("Can't infer task type of this model.
      Please provide predict_fun argument.")
  }
  }

get_prediction_function.svm = function(model) {
  task_type = model$type
  if (task_type %in% 3:4) {
    predict_fun = function(object, newdata)
      predict(object, newdata = newdata)
  } else if (task_type %in% 0:2) {
    base_level = model$levels[1]
    if (!model$compprob) stop("Need probabilities. Retrain svm() with probability=TRUE.")
    predict_fun = function(object, newdata) {
      pred_obj = predict(model, newdata = newdata, probability = TRUE)
      attributes(pred_obj)$probabilities[, base_level]
    }
  } else {
    stop("Can't infer task type of this model.
      Please provide predict_fun argument.")
  }
  }
get_prediction_function = function(model) {
  UseMethod("get_prediction_function", model)
}

get_prediction_function.default = function(model) {
  function(object, newdata) predict(object, newdata = newdata)
}

get_prediction_function.glm = function(model) {
  function(object, newdata) predict(object, newdata = newdata, type = "response")
}

get_prediction_function.WrappedModel = function(model) {
  task_type = mlr::getTaskType(model)
  if (task_type == "regr") {
    predict_fun = function(object, newdata)
      mlr::getPredictionResponse(predict(object, newdata = newdata))
  } else if (task_type == "classif") {
    predict_fun = function(object, newdata)
      mlr::getPredictionProbabilities(predict(object, newdata = newdata))
  } else stop("Task type not supported.")
  predict_fun
}

get_prediction_function.train = function(model) {
  task_type = model$modelType
  if (task_type == "Regression") {
    predict_fun = function(object, newdata)
      predict(object, newdata = newdata, type = "raw")
  } else if (task_type == "Classification") {
    base_level = model$finalModel$obsLevels[1]
    predict_fun = function(object, newdata)
      predict(object, newdata = newdata, type = "prob")[[base_level]]
  }
}

get_prediction_function.randomForest = function(model) {
  task_type = model$type
  if (task_type == "regression") {
    predict_fun = function(object, newdata)
      predict(object, newdata = newdata)
  } else if (task_type == "classification") {
    base_level = model$classes[1]
    predict_fun = function(object, newdata)
      predict(object, newdata = newdata, type = "prob")[, base_level]
  }
}

get_prediction_function.gbm = function(model) {
  task_type = model$distribution$name
  n.trees = model$n.trees
  if (task_type == "gaussian") {
    predict_fun = function(object, newdata)
      predict(object, newdata = newdata, n.trees = n.trees)
  } else if (task_type == "bernoulli") {
    predict_fun = function(object, newdata)
      predict(model, newdata = newdata, type = "response", n.trees = n.trees)
  } else {
    stop("Can't infer task type of this model.
      Please provide predict_fun argument.")
  }
}

get_prediction_function.svm = function(model) {
  task_type = model$type
  if (task_type %in% 3:4) {
    predict_fun = function(object, newdata)
      predict(object, newdata = newdata)
  } else if (task_type %in% 0:2) {
    base_level = model$levels[1]
    if (!model$compprob) stop("Need probabilities. Retrain svm() with probability=TRUE.")
    predict_fun = function(object, newdata) {
      pred_obj = predict(model, newdata = newdata, probability = TRUE)
      attributes(pred_obj)$probabilities[, base_level]
    }
  } else {
    stop("Can't infer task type of this model.
      Please provide predict_fun argument.")
  }
}

get_prediction_function.nnet = function(nnet) {
  function(object, newdata) predict(object, newdata = newdata, type = "raw")[, 1]
}
