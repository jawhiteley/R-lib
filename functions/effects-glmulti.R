effect.glmulti <- function (term, mod, xlevels = list(), default.levels = 10, given.values, 
    se = TRUE, confidence.level = 0.95, transformation = list(link = family(mod)$linkfun, 
        inverse = family(mod)$linkinv), typical = mean, ...) 
{
    if (missing(given.values)) 
        given.values <- NULL
    else if (!all(which <- names(given.values) %in% names(coef(mod)))) 
        stop("given.values (", names(given.values[!which]), ") not in the model")
    model.components <- analyze.model(term, mod, xlevels, default.levels)
    predict.data <- model.components$predict.data
    factor.levels <- model.components$factor.levels
    factor.cols <- model.components$factor.cols
    mod.aug <- model.components$mod.aug
    term <- model.components$term
    n.basic <- model.components$n.basic
    x <- model.components$x
    X.mod <- model.components$X.mod
    cnames <- model.components$cnames
    X <- model.components$X
    formula.rhs <- formula(mod)[c(1, 3)]
    nrow.X <- nrow(X)
    mf <- model.frame(formula.rhs, data = rbind(X[, names(predict.data), 
        drop = FALSE], predict.data), xlev = factor.levels)
    mod.matrix.all <- model.matrix(formula.rhs, data = mf, contrasts.arg = mod$contrasts)
    mod.matrix <- mod.matrix.all[-(1:nrow.X), ]
    fit.1 <- na.omit(predict(mod))
    wts <- mod$weights
    if (is.null(wts)) 
        wts <- rep(1, length(fit.1))
    mod.2 <- lm.wfit(mod.matrix.all[1:nrow.X, ], fit.1, wts)
    class(mod.2) <- "lm"
    y <- if (inherits(mod, "glm")) 
        mod$y
    else na.omit(model.response(model.frame(mod)))
    discrepancy <- 100 * mean(abs(fitted(mod.2) - fit.1)/(1e-10 + 
        mean(abs(fit.1))))
    if (discrepancy > 0.001) 
        warning(paste("There is a discrepancy of", round(discrepancy, 
            3), "percent \n     in the 'safe' predictions used to generate effect", 
            term))
    mod.matrix <- fixup.model.matrix(mod, mod.matrix, mod.matrix.all, 
        X.mod, mod.aug, factor.cols, cnames, term, typical, given.values)
    effect <- mod.matrix %*% mod.2$coefficients
    result <- list(term = term, formula = formula(mod), response = response.name(mod), 
        variables = x, fit = effect, x = predict.data[, 1:n.basic, 
            drop = FALSE], model.matrix = mod.matrix, data = X, 
        discrepancy = discrepancy)
    if (se) {
        if (any(family(mod)$family == c("binomial", "poisson"))) {
            dispersion <- 1
            z <- qnorm(1 - (1 - confidence.level)/2)
        }
        else {
            dispersion <- sum(wts * mod$residuals^2)/mod$df.residual
            z <- qt(1 - (1 - confidence.level)/2, df = mod$df.residual)
        }
        mod.2$terms <- mod$terms
        V <- dispersion * summary.lm(mod.2)$cov
        vcov <- mod.matrix %*% V %*% t(mod.matrix)
        rownames(vcov) <- colnames(vcov) <- NULL
        var <- diag(vcov)
        result$vcov <- vcov
        result$se <- sqrt(var)
        result$lower <- effect - z * result$se
        result$upper <- effect + z * result$se
        result$confidence.level <- confidence.level
    }
    if (is.null(transformation$link) && is.null(transformation$inverse)) {
        transformation$link <- I
        transformation$inverse <- I
    }
    result$transformation <- transformation
    class(result) <- "eff"
    result
}




analyze.model <- function (term, mod, xlevels, default.levels) 
{
    if ((!is.null(na.action(mod))) && class(na.action(mod)) == 
        "exclude") 
        class(mod$na.action) <- "omit"
    term <- gsub(" ", "", gsub("\\*", ":", term))
    intercept <- has.intercept(mod)
    terms <- term.names(mod)
    if (intercept) 
        terms <- terms[-1]
    which.term <- which(term == terms)
    mod.aug <- list()
    if (length(which.term) == 0) {
        warning(paste(term, "does not appear in the model"))
        mod.aug <- update(formula(mod), eval(parse(text = paste(". ~ . +", 
            term))))
    }
    if (!is.high.order.term(term, mod, mod.aug)) 
        warning(paste(term, "is not a high-order term in the model"))
    basic.vars <- first.order.ancestors(term, mod, mod.aug)
    all.vars <- (1:nrow(attr(terms(mod), "factors")))[0 != apply(attr(terms(mod), 
        "factors"), 1, sum)]
    if (intercept) 
        all.vars <- all.vars - 1
    if (inherits(mod, "multinom")) 
        all.vars <- all.vars - 1
    if (inherits(mod, "polr")) 
        all.vars <- all.vars - 1
    excluded.vars <- setdiff(all.vars, basic.vars)
    if (length(terms) == 1) {
        all.vars <- basic.vars <- all.vars(formula(mod))[2]
        excluded.vars <- numeric()
    }
    else {
        all.vars <- all.vars(as.formula(paste("~", paste(terms[all.vars], 
            collapse = "+"))))
        basic.vars <- all.vars(as.formula(paste("~", paste(terms[basic.vars], 
            collapse = "+"))))
    }
    excluded.vars <- if (length(excluded.vars) > 0) 
        all.vars(as.formula(paste("~", paste(terms[excluded.vars], 
            collapse = "+"))))
    else NULL
    X.mod <- model.matrix(mod)
    cnames <- colnames(X.mod)
    factor.cols <- rep(FALSE, length(cnames))
    names(factor.cols) <- cnames
    X <- model.frame(mod)
    for (name in all.vars) {
        if (is.factor(X[[name]])) 
            factor.cols[grep(paste("^", name, sep = ""), cnames)] <- TRUE
    }
    factor.cols[grep(":", cnames)] <- FALSE
    X <- na.omit(expand.model.frame(mod, all.vars))
    x <- list()
    factor.levels <- list()
    for (name in basic.vars) {
        levels <- mod$xlevels[[name]]
        fac <- !is.null(levels)
        if (!fac) {
            levels <- if (is.null(xlevels[[name]])) 
                seq(min(X[, name]), max(X[, name]), length = default.levels)
            else xlevels[[name]]
        }
        else factor.levels[[name]] <- levels
        x[[name]] <- list(name = name, is.factor = fac, levels = levels)
    }
    x.excluded <- list()
    for (name in excluded.vars) {
        levels <- mod$xlevels[[name]]
        fac <- !is.null(levels)
        level <- if (fac) 
            levels[1]
        else mean(X[, name])
        if (fac) 
            factor.levels[[name]] <- levels
        x.excluded[[name]] <- list(name = name, is.factor = fac, 
            level = level)
    }
    dims <- sapply(x, function(x) length(x$levels))
    len <- prod(dims)
    n.basic <- length(basic.vars)
    n.excluded <- length(excluded.vars)
    n.vars <- n.basic + n.excluded
    predict.data <- matrix("", len, n.vars)
    excluded <- sapply(x.excluded, function(x) x$level)
    for (i in 1:len) {
        subs <- subscripts(i, dims)
        for (j in 1:n.basic) {
            predict.data[i, j] <- x[[j]]$levels[subs[j]]
        }
        if (n.excluded > 0) 
            predict.data[i, (n.basic + 1):n.vars] <- excluded
    }
    colnames(predict.data) <- c(sapply(x, function(x) x$name), 
        sapply(x.excluded, function(x) x$name))
    predict.data <- matrix.to.df(predict.data)
    list(predict.data = predict.data, factor.levels = factor.levels, 
        factor.cols = factor.cols, mod.aug = mod.aug, term = term, 
        n.basic = n.basic, x = x, X.mod = X.mod, cnames = cnames, 
        X = X)
}




has.intercept <- function (model, ...) any(names(coefficients(model)) == "(Intercept)")

is.high.order.term <- function (term, mod, ...) 
{
    0 == length(descendants(term, mod, ...))
}

term.names <- function (model, ...) 
{
    term.names <- gsub(" ", "", labels(terms(model)))
    if (has.intercept(model)) 
        c("(Intercept)", term.names)
    else term.names
}

term.names <- function (model, ...) 
{
    term.names <- gsub(" ", "", labels(terms(model)))
    if (has.intercept(model)) 
        c("(Intercept)", term.names)
    else term.names
}

first.order.ancestors <- function (term, mod, ...) 
{
    ancestors <- ancestors(term, mod, ...)
    ancestors[attr(terms(mod), "order")[ancestors] == 1]
}
 
ancestors <- function (term, mod, ...) 
{
    names <- term.names(mod)
    if (has.intercept(mod)) 
        names <- names[-1]
    if (length(names) == 1) 
        return(NULL)
    which.term <- which(term == names)
    if (length(which.term) == 0) {
        factors <- attr(terms(...), "factors")
        rownames(factors) <- gsub(" ", "", rownames(factors))
        colnames(factors) <- gsub(" ", "", colnames(factors))
        result <- (1:length(names))[sapply(names, function(term2) is.relative(term2, 
            term, factors))]
        if (0 == length(result)) 
            which.term
        else result
    }
    else {
        factors <- attr(terms(mod), "factors")
        rownames(factors) <- gsub(" ", "", rownames(factors))
        colnames(factors) <- gsub(" ", "", colnames(factors))
        result <- (1:length(names))[-which.term][sapply(names[-which.term], 
            function(term2) is.relative(term2, term, factors))]
        if (0 == length(result)) 
            which.term
        else result
    }
}

matrix.to.df <- function (matrix) 
{
    on.exit(options(warn = opt[[1]]))
    opt <- options(warn = -1)
    ncol <- ncol(matrix)
    colnames <- colnames(matrix)
    result <- list()
    for (j in 1:ncol) {
        numbers <- as.numeric(matrix[, j])
        result[[colnames[j]]] <- if (all(is.na(numbers))) 
            matrix[, j]
        else numbers
    }
    as.data.frame(result)
}


fixup.model.matrix <- function (mod, mod.matrix, mod.matrix.all, X.mod, mod.aug, factor.cols, 
                                cnames, term, typical, given.values) 
{
    attr(mod.matrix, "assign") <- attr(mod.matrix.all, "assign")
    stranger.cols <- apply(outer(strangers(term, mod, mod.aug), 
        attr(mod.matrix, "assign"), "=="), 2, any)
    if (has.intercept(mod)) 
        stranger.cols[1] <- TRUE
    if (any(stranger.cols)) {
        facs <- factor.cols & stranger.cols
        covs <- (!factor.cols) & stranger.cols
        if (any(facs)) 
            mod.matrix[, facs] <- matrix(apply(as.matrix(X.mod[, 
                facs]), 2, mean), nrow = nrow(mod.matrix), ncol = sum(facs), 
                byrow = TRUE)
        if (any(covs)) 
            mod.matrix[, covs] <- matrix(apply(as.matrix(X.mod[, 
                covs]), 2, typical), nrow = nrow(mod.matrix), 
                ncol = sum(covs), byrow = TRUE)
        if (!is.null(given.values)) {
            stranger.names <- cnames[stranger.cols]
            given <- stranger.names %in% names(given.values)
            if (any(given)) 
                mod.matrix[, stranger.names[given]] <- matrix(given.values[stranger.names[given]], 
                  nrow = nrow(mod.matrix), ncol = length(stranger.names[given]), 
                  byrow = TRUE)
        }
    }
    for (name in cnames) {
        components <- unlist(strsplit(name, ":"))
        if (length(components) > 1) 
            mod.matrix[, name] <- apply(mod.matrix[, components], 
                1, prod)
    }
    mod.matrix
}
