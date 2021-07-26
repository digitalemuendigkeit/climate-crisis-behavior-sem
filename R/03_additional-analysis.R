## Function by https://paulvanderlaken.com/2020/07/28/publication-ready-correlation-matrix-significance-r/
#' correlation_matrix
#' Creates a publication-ready / formatted correlation matrix, using `Hmisc::rcorr` in the backend.
#'
#' @param df dataframe; containing numeric and/or logical columns to calculate correlations for
#' @param type character; specifies the type of correlations to compute; gets passed to `Hmisc::rcorr`; options are `"pearson"` or `"spearman"`; defaults to `"pearson"`
#' @param digits integer/double; number of decimals to show in the correlation matrix; gets passed to `formatC`; defaults to `3`
#' @param decimal.mark character; which decimal.mark to use; gets passed to `formatC`; defaults to `.`
#' @param use character; which part of the correlation matrix to display; options are `"all"`, `"upper"`, `"lower"`; defaults to `"all"`
#' @param show_significance boolean; whether to add `*` to represent the significance levels for the correlations; defaults to `TRUE`
#' @param replace_diagonal boolean; whether to replace the correlations on the diagonal; defaults to `FALSE`
#' @param replacement character; what to replace the diagonal and/or upper/lower triangles with; defaults to `""` (empty string)
#'
#' @return a correlation matrix
#' @export
#'
#' @examples
#' `correlation_matrix(iris)`
#' `correlation_matrix(mtcars)`
correlation_matrix <- function(df,
                               type = "pearson",
                               digits = 3,
                               decimal.mark = ".",
                               use = "all",
                               show_significance = TRUE,
                               replace_diagonal = FALSE,
                               replacement = ""){

  # check arguments
  stopifnot({
    is.numeric(digits)
    digits >= 0
    use %in% c("all", "upper", "lower")
    is.logical(replace_diagonal)
    is.logical(show_significance)
    is.character(replacement)
  })
  # we need the Hmisc package for this
  require(Hmisc)

  # retain only numeric and boolean columns
  isNumericOrBoolean = vapply(df, function(x) is.numeric(x) | is.logical(x), logical(1))
  if (sum(!isNumericOrBoolean) > 0) {
    cat('Dropping non-numeric/-boolean column(s):', paste(names(isNumericOrBoolean)[!isNumericOrBoolean], collapse = ', '), '\n\n')
  }
  df = df[isNumericOrBoolean]

  # transform input data frame to matrix
  x <- as.matrix(df)

  # run correlation analysis using Hmisc package
  correlation_matrix <- Hmisc::rcorr(x, type = )
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value

  # transform correlations to specific character format
  Rformatted = formatC(R, format = 'f', digits = digits, decimal.mark = decimal.mark)

  # if there are any negative numbers, we want to put a space before the positives to align all
  if (sum(R < 0) > 0) {
    Rformatted = ifelse(R > 0, paste0(' ', Rformatted), Rformatted)
  }

  # add significance levels if desired
  if (show_significance) {
    # define notions for significance levels; spacing is important.
    stars <- ifelse(is.na(p), "   ", ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "*  ", "   "))))
    Rformatted = paste0(Rformatted, stars)
  }
  # build a new matrix that includes the formatted correlations and their significance stars
  Rnew <- matrix(Rformatted, ncol = ncol(x))
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep =" ")

  # replace undesired values
  if (use == 'upper') {
    Rnew[lower.tri(Rnew, diag = replace_diagonal)] <- replacement
  } else if (use == 'lower') {
    Rnew[upper.tri(Rnew, diag = replace_diagonal)] <- replacement
  } else if (replace_diagonal) {
    diag(Rnew) <- replacement
  }

  return(Rnew)
}

cor.matrix.plot <- function (data, conf.level = 0.95)
{
  rwthcolors <- rwth.colorpalette()
  p <- corrplot::cor.mtest(data, conf.level = conf.level)
  col <- grDevices::colorRampPalette(c(rwthcolors$red, "#FFFFFF",
                                       rwthcolors$blue))
  stats::cor(data, use = "pairwise.complete.obs") %>% corrplot::corrplot(method = "color",
                                                                         col = col(200), type = "upper", order = "hclust", number.cex = 0.7,
                                                                         addCoef.col = "black", tl.col = "black", tl.srt = 90,
                                                                         p.mat = p$p, sig.level = c(0.001, 0.01, 0.05), insig = "n",
                                                                         diag = TRUE, tl.pos = "lt")
  stats::cor(data, use = "pairwise.complete.obs") %>% corrplot::corrplot(method = "color",
                                                                         col = col(200), type = "lower", order = "hclust", number.cex = 0.7,
                                                                         p.mat = p$p, sig.level = c(0.001, 0.01, 0.05), insig = "label_sig",
                                                                         pch.cex = 0.8, diag = TRUE, add = TRUE, tl.pos = "n")
}



## Based on function by https://paulvanderlaken.com/2020/07/28/publication-ready-correlation-matrix-significance-r/
#' correlation_matrix
#' Creates a publication-ready / formatted correlation matrix with different variables for columns on rows, using `Hmisc::rcorr` in the backend.
#'
#' @param df1 dataframe 1; containing numeric and/or logical columns to calculate correlations for; passed on to the rows
#' @param df2 dataframe 2; containing numeric and/or logical columns to calculate correlations for; passed on to the cols
#' @param type character; specifies the type of correlations to compute; gets passed to `Hmisc::rcorr`; options are `"pearson"` or `"spearman"`; defaults to `"pearson"`
#' @param digits integer/double; number of decimals to show in the correlation matrix; gets passed to `formatC`; defaults to `3`
#' @param decimal.mark character; which decimal.mark to use; gets passed to `formatC`; defaults to `.`
#' @param show_significance boolean; whether to add `*` to represent the significance levels for the correlations; defaults to `TRUE`
#'
#' @return a correlation matrix
#' @export
#'
#' @examples
#' `correlation_matrix_2(iris %>% select(starts_with("Sepal")), iris %>% select(starts_with("Petal")))`
correlation_matrix_2 <- function(df1,
                                 df2,
                                 type = "pearson",
                                 digits = 3,
                                 decimal.mark = ".",
                                 show_significance = TRUE){

  # check arguments
  stopifnot({
    is.numeric(digits)
    digits >= 0
    is.logical(show_significance)
  })
  # we need the Hmisc package for this
  require(Hmisc)
  require(tidyverse)

  # retain only numeric and boolean columns
  isNumericOrBoolean1 = vapply(df1, function(x) is.numeric(x) | is.logical(x), logical(1))
  if (sum(!isNumericOrBoolean1) > 0) {
    cat('Dropping non-numeric/-boolean column(s):', paste(names(isNumericOrBoolean1)[!isNumericOrBoolean1], collapse = ', '), '\n\n')
  }
  isNumericOrBoolean2 = vapply(df2, function(x) is.numeric(x) | is.logical(x), logical(1))
  if (sum(!isNumericOrBoolean2) > 0) {
    cat('Dropping non-numeric/-boolean column(s):', paste(names(isNumericOrBoolean2)[!isNumericOrBoolean2], collapse = ', '), '\n\n')
  }
  df1 = df1[isNumericOrBoolean1]
  df2 = df2[isNumericOrBoolean2]

  # transform input data frame to matrix
  x1 <- as.matrix(df1)
  x2 <- as.matrix(df2)

  # run correlation analysis using Hmisc package
  correlation_matrix <- Hmisc::rcorr(x = x1, y = x2, type = )
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value

  # transform correlations to specific character format
  Rformatted = formatC(R, format = 'f', digits = digits, decimal.mark = decimal.mark)

  # if there are any negative numbers, we want to put a space before the positives to align all
  if (sum(R < 0) > 0) {
    Rformatted = ifelse(R > 0, paste0(' ', Rformatted), Rformatted)
  }

  # add significance levels if desired
  if (show_significance) {
    # define notions for significance levels; spacing is important.
    stars <- ifelse(is.na(p), "   ", ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "*  ", "   "))))
    Rformatted = paste0(Rformatted, stars)
  }

  # build a new matrix that includes the formatted correlations and their significance stars
  Rnew <- matrix(Rformatted, ncol = ncol(x1)+ncol(x2))
  rownames(Rnew) <- c(colnames(x1), colnames(x2))
  Rnew <- data.frame(Rnew)
  colnames(Rnew) <- c(colnames(x1), colnames(x2))
  Rnew <- Rnew %>%
    select(colnames(x2))  %>%
    filter(rownames(Rnew) %in% colnames(x1))

  return(Rnew)
}


cor.matrix.plot.new <- function (data, conf.level = 0.95, method = "pearson")
{
  # use colorblind safe colors: red and blue
  colors <- c("#EE6677", "#4477AA")
  p <- corrplot::cor.mtest(data, conf.level = conf.level, method = method)
  col <- grDevices::colorRampPalette(c(colors[1], "#FFFFFF",
                                       colors[2]))
  stats::cor(data,
             use = "pairwise.complete.obs",
             method = method) %>% corrplot::corrplot(
               method = "color",
               col = col(200),
               type = "upper",
               order = "hclust",
               addCoef.col = "black",
               tl.col = "black",
               tl.srt = 90,
               p.mat = p$p,
               sig.level = c(0.001, 0.01, 0.05),
               insig = "n",
               diag = TRUE,
               tl.pos = "lt"
             )
  stats::cor(data,
             use = "pairwise.complete.obs",
             method = method) %>% corrplot::corrplot(
               method = "color",
               col = col(200),
               type = "lower",
               order = "hclust",
               p.mat = p$p,
               sig.level = c(0.001, 0.01, 0.05),
               insig = "label_sig",
               pch.cex = 0.8,
               diag = TRUE,
               add = TRUE,
               tl.pos = "n"
             )
}

## Function by https://paulvanderlaken.com/2020/07/28/publication-ready-correlation-matrix-significance-r/
#' correlation_matrix
#' Creates a publication-ready / formatted correlation matrix, using `Hmisc::rcorr` in the backend.
#'
#' @param df dataframe; containing numeric and/or logical columns to calculate correlations for
#' @param type character; specifies the type of correlations to compute; gets passed to `Hmisc::rcorr`; options are `"pearson"` or `"spearman"`; defaults to `"pearson"`
#' @param digits integer/double; number of decimals to show in the correlation matrix; gets passed to `formatC`; defaults to `3`
#' @param replace_diagonal boolean; whether to replace the correlations on the diagonal; defaults to `FALSE`
#' @param replacement character; what to replace the diagonal and/or upper/lower triangles with; defaults to `""` (empty string)
#'
#' @return a correlation matrix
#' @export
#'
#' @examples
#' `correlation_matrix(iris)`
#' `correlation_matrix(mtcars)`
correlation_matrix_full <- function(df,
                                    type = "pearson",
                                    digits = 3,
                                    replace_diagonal = FALSE,
                                    replacement = ""){

  # check arguments
  stopifnot({
    is.numeric(digits)
    digits >= 0
    is.logical(replace_diagonal)
    is.character(replacement)
  })
  # we need the Hmisc package for this
  require(Hmisc)

  # retain only numeric and boolean columns
  isNumericOrBoolean = vapply(df, function(x) is.numeric(x) | is.logical(x), logical(1))
  if (sum(!isNumericOrBoolean) > 0) {
    cat('Dropping non-numeric/-boolean column(s):', paste(names(isNumericOrBoolean)[!isNumericOrBoolean], collapse = ', '), '\n\n')
  }
  df = df[isNumericOrBoolean]

  # transform input data frame to matrix
  x <- as.matrix(df)

  # run correlation analysis using Hmisc package
  correlation_matrix <- Hmisc::rcorr(x, type = )
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value

  # transform correlations to specific character format
  Rformatted <- round(R, digits = digits)

  # make stars matrix
  stars <- ifelse(is.na(p), "", ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "*", ""))))

  # build a new matrix that includes the formatted correlations and their significance stars
  Rnew <- matrix(Rformatted, ncol = ncol(x))
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep =" ")
  Rnew[lower.tri(Rnew)] <- stars[lower.tri(stars)]

  # replace undesired values
  if (replace_diagonal) {
    diag(Rnew) <- replacement
  }

  return(Rnew)
}

# More helpers ----
closest <- function(value, vector){
  vector[which(abs(vector-value)==min(abs(vector-value)))]
}

closestpos <- function(value, vector){
  which(abs(vector-value)==min(abs(vector-value)))
}

kruskaldt <- function(data,
                      var1,
                      var2) {
  k <- kruskal.test(data[, {{var1}}] ~ data[, {{var2}}])
  data.frame(
    'Chi-squared' = k[[1]],
    'Degrees of freedom' = k[[2]],
    p = k[[3]]
  ) %>%
    datatable(rownames = FALSE,
              caption = paste0("Kruskal-Wallis rank sum test for differences in ", {{ var1 }}, " between ", {{ var2 }}, " groups"),
              autoHideNavigation = TRUE,
              options = list(pageLength = 1,
                             ordering = FALSE)) %>%
    formatRound(c(1,3),
                digits = 3)
}

