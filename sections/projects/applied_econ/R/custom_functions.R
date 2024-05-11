makestars <- function(pvalues) {
    return(
        dplyr::case_when(
            pvalues < 0.001 ~ "$***$",
            pvalues < 0.05 ~ "$**$",
            pvalues < 0.1 ~ "$*$",
            .default = ""
        )
    )
}


gtgazer <- function(model, n_coef = 4, coefnames, description, title, subtitle, bg_color) {
    if (class(model) %in% c("translogEst")) {
        coefficients <- summary(model)$coefTable[1:n_coef, 1]
        std_values <- summary(model)$coefTable[1:n_coef, 2]
        pvalues <- summary(model)$coefTable[1:n_coef, 4]
        signif <- makestars(pvalues)
        r2 <- round(summary(model)$r2, 3)
        adj_r2 <- round(summary(model)$r2bar, 3)
        n <- summary(model)$nObs
    } else if (class(model) %in% c("quadFuncEst", "translogCostEst")) {
        coefficients <- summary(model$est)$coefficients[, 1]
        std_values <- summary(model$est)$coefficients[, 2]
        pvalues <- summary(model$est)$coefficients[, 4]
        signif <- makestars(pvalues)
        r2 <- round(model$r2, 3)
        adj_r2 <- round(model$r2bar, 3)
        n <- model$nObs
    } else if (class(model) == "lm") {
        coefficients <- summary(model)$coefficients[, 1]
        std_values <- summary(model)$coefficients[, 2]
        pvalues <- summary(model)$coefficients[, 4]
        signif <- makestars(pvalues)
        r2 <- round(summary(model)$r.squared, 3)
        adj_r2 <- round(summary(model)$adj.r.squared, 3)
        n <- nobs(model)
    }

    coefnames <- coefnames
    description <- description
    reg_results <- data.frame(cbind(coefnames, description, coefficients, std_values, pvalues, signif)) |>
        tibble::tibble() |>
        dplyr::mutate(dplyr::across(c(coefficients, std_values, pvalues), as.numeric))

    table <- reg_results |>
        gt::gt(rowname_col = "coefnames") |>
        gt::cols_label(
            description = gt::md("**Description**"),
            coefficients = gt::md("**Coefficients**"),
            std_values = gt::md("**Ecart Type**"),
            pvalues = gt::md("**Pvalues**"),
            signif = gt::md("**Significativité**")
        ) |>
        gt::fmt_markdown(columns = c(coefnames, signif, description)) |>
        gt::fmt_number(
            columns = c(coefficients, pvalues),
            decimals = 3,
            drop_trailing_zeros = TRUE
        ) |>
        gt::fmt(columns = std_values, fns = function(std) {
            paste("+/-", round(std, 3))
        }) |>
        gt::tab_footnote(footnote = gt::md(sprintf("*Observations* : %s", n))) |>
        gt::tab_footnote(footnote = gt::md("***")) |>
        gt::tab_footnote(footnote = gt::md(glue::glue("$R^2=$ {r2}"))) |>
        gt::tab_footnote(footnote = gt::md(glue::glue("$R^2_{{adj}}=$ {adj_r2}"))) |>
        gt::tab_header(
            title = gt::md(title),
            subtitle = gt::md(subtitle)
        ) |>
        gt::tab_options(
            table.background.color = bg_color
        )

    return(table)
}
