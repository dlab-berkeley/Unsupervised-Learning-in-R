#' @param outcome vector of outcome values.
lca_covariate_table =
  function(object, var_labels = NULL, outcome = NULL,
           data = NULL,
           format = "latex",
           outcome_name =
             paste0("Leukemia rate (",
                    ifelse(format == "latex", "\\", ""), "%)")) {

  num_vars = length(object$probs)

  num_classes = length(object$P)

  # Our results table should have one row for each variable, plus 1 for the header.
  # It should have 1 column per class plus 1 for the variable name.
  table = data.frame(do.call(rbind, lapply(object$probs, function(mat) {
    # mat[, "Pr(2)"]
    mat[, 2]
  })))


  # Add row of outcome means for each class.
  if (!is.null(outcome)) {

    # If outcome is {1, 2} convert to {0, 1}
    if (all(unique(outcome) %in% c(1, 2))) {
      outcome = outcome - 1
    }

    # Add row to table with outcome distribution.
    # This should work with a binary or continuous outcome.
    outcome_means = tapply(outcome, object$predclass, mean)

    # For binary variables convert to a percentage.
    if (FALSE && length(unique(outcome)) == 2) {
      outcome_means = round(outcome_means * 100, 1)
    }

    # Add as a final row in the table.
    table = rbind(table, outcome_means)
    rownames(table)[nrow(table)] = outcome_name
    table = table * 100
  }

  # Sort by ascending order of leukemia rate:
  # table columns, object$P, object$predclass.

  # Final row contains the outcome means, so sort in ascending order.
  tab_order = order(table[nrow(table), ])

  table = table[, tab_order]
  proportions = object$P[tab_order]

  # Create custom header line which shows the overall class distrbution.
  colnames(table) = paste0("Class ", 1:num_classes,
                           ifelse(format == "latex", "\n", "<br>"),
                           "(", round(proportions * 100, 1),
                           ifelse(format == "latex", "\\", ""), "%)")

  # Ideally have custom strings for each variable name.
  if (!is.null(var_labels)) {
    #lapply(names(var_labels), function(var_name) {
    for (var_name in names(var_labels)) {
      if (var_name %in% rownames(table)) {
        rownames(table)[rownames(table) == var_name] = var_labels[var_name]
      }
    }#)
  }

  # Turn this off for now.
  if (FALSE) {

  # Create a blank column to hold the ANOVA p-value.
  table$p_value = NA

  # Add ANOVA test for each variable?
  for (var_i in seq(nrow(table))) {

    if (var_i <= length(var_labels)) {
      var_data = data[, names(var_labels)[var_i]]
    } else {
      var_data = outcome
      # We don't actually have an outcome variable.
      if (!is.null(outcome)) {
        next
      }
    }

    # Create a dataframe just for this anova analysis.
    df = data.frame(var_data, class = object$predclass)

    # Restrict to non-missing values.
    df = na.omit(df)

    # Run anova.
    anova_result = stats::aov(class ~ ., data = df)

    table[var_i, "p_value"] = summary(anova_result)[[1]][1, 5]
  }

  # Add FDR-adjusted p-value column.
  table$p_value_adj = stats::p.adjust(table$p_value, method = "BH")

  # Update last two column names for better printing.
  colnames(table)[c(ncol(table) - 1, ncol(table))] =
    c(paste0("ANOVA", ifelse(format == "latex", "\n", "<br />"), "p-value"),
      paste0("FDR adj.", ifelse(format == "latex", "\n", "<br />"), "p-value"))

  }

  return(table)

}
