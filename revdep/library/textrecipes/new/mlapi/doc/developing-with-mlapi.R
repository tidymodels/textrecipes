## ---- eval=FALSE---------------------------------------------------------
#  # transformer:
#  # scaler just divide each column by std_dev
#  scaler = Scaler$new()
#  
#  # decomposition:
#  # fits truncated SVD: X = U * S * V
#  # or rephrasing X = P * Q where P = U * sqrt(S); Q = sqrt(S) * V
#  # as a result trunc_svd$fit_transform(train) returns matrix P and learns matrix Q (stores inside model)
#  # when trunc_svd$transform(test) is called, model use matrix Q in order to find matrix P for `test` data
#  trunc_svd = SVD$new(rank = 16)
#  
#  # estimator:
#  # fit L1/L2 regularized logistic regression
#  logreg = LogisticRegression(L1 = 0.1, L2 = 10)

## ---- eval=FALSE---------------------------------------------------------
#  train %>%
#    fit_transform(scaler) %>%
#    fit_transform(trunc_svd) %>%
#    fit(logreg)

## ---- eval=FALSE---------------------------------------------------------
#  predictions = test %>%
#    transform(scaler) %>%
#    transform(trunc_svd) %>%
#    predict(logreg)
#  

## ------------------------------------------------------------------------
SimpleLinearModel = R6::R6Class(
  classname = "mlapiSimpleLinearModel", 
  inherit = mlapi::mlapiEstimation, 
  public = list(
    initialize = function(tol = 1e-7) {
      private$tol = tol
      super$set_internal_matrix_formats(dense = "matrix", sparse = NULL)
    },
    fit = function(x, y, ...) {
      x = super$check_convert_input(x)
      stopifnot(is.vector(y))
      stopifnot(is.numeric(y))
      stopifnot(nrow(x) == length(y))
      
      private$n_features = ncol(x)
      private$coefficients = .lm.fit(x, y, tol = private$tol)[["coefficients"]]
    },
    predict = function(x) {
      stopifnot(ncol(x) == private$n_features)
      x %*% matrix(private$coefficients, ncol = 1)
    }
  ),
  private = list(
    tol = NULL,
    coefficients = NULL,
    n_features = NULL
  )
)

## ------------------------------------------------------------------------
set.seed(1)
model = SimpleLinearModel$new()
x = matrix(sample(100 * 10, replace = T), ncol = 10)
y = sample(c(0, 1), 100, replace = T)
model$fit(as.data.frame(x), y)
res1 = model$predict(x)
# check pipe-compatible S3 interface
res2 = predict(x, model)
identical(res1, res2)

## ------------------------------------------------------------------------
TruncatedSVD = R6::R6Class(
  classname = "TruncatedSVD", 
  inherit = mlapi::mlapiDecomposition, 
  public = list(
    initialize = function(rank = 10) {
      private$rank = rank
      super$set_internal_matrix_formats(dense = "matrix", sparse = NULL)
    },
    fit_transform = function(x, ...) {
      x = super$check_convert_input(x)
      private$n_features = ncol(x)
      svd_fit = svd(x, nu = private$rank, nv = private$rank, ...)
      sing_values = svd_fit$d[seq_len(private$rank)]
      result = svd_fit$u %*% diag(x = sqrt(sing_values))
      private$components_ = t(svd_fit$v %*% diag(x = sqrt(sing_values)))
      rm(svd_fit)
      rownames(result) = rownames(x)
      colnames(private$components_) = colnames(x)
      private$fitted = TRUE
      invisible(result)
    },
    transform = function(x, ...) {
      if (private$fitted) {
        stopifnot(ncol(x) == ncol(private$components_))
        lhs = tcrossprod(private$components_)
        rhs = as.matrix(tcrossprod(private$components_, x))
        t(solve(lhs, rhs))
      }
      else
        stop("Fit the model first woth model$fit_transform()!")
    }
  ),
  private = list(
    rank = NULL, 
    n_features = NULL, 
    fitted = NULL
  )
)

## ------------------------------------------------------------------------
set.seed(1)
model = TruncatedSVD$new(2)
x = matrix(sample(100 * 10, replace = T), ncol = 10)
x_trunc = model$fit_transform(x)
dim(x_trunc)

x_trunc_2 = model$transform(x)
sum(x_trunc_2 - x_trunc)

# check pipe-compatible S3 interface
x_trunc_2_s3 = transform(x, model)
identical(x_trunc_2, x_trunc_2_s3)

