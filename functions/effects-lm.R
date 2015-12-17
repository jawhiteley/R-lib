effect.lm <- function (term, mod, xlevels = list(), default.levels = 10, given.values, 
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
    if ((!is.null(mod$na.action)) && class(mod$na.action) == 
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




plot.eff <- function (x, x.var = which.max(levels), z.var = which.min(levels), 
    multiline = is.null(x$se), rug = TRUE, xlab, ylab, main = paste(effect, 
        "effect plot"), colors = palette(), symbols = 1:10, lines = 1:10, 
    cex = 1.5, ylim, factor.names = TRUE, type = c("response", 
        "link"), ticks = list(at = NULL, n = 5), alternating = TRUE, 
    layout, rescale.axis = TRUE, key.args = NULL, row = 1, col = 1, 
    nrow = 1, ncol = 1, more = FALSE, ...) 
{
    make.ticks <- function(range, link, inverse, at, n) {
        link <- if (is.null(link)) 
            function(x) nlm(function(y) (inverse(y) - x)^2, mean(range))$estimate
        else link
        if (is.null(n)) 
            n <- 5
        labels <- if (is.null(at)) {
            labels <- pretty(sapply(range, inverse), n = n + 
                1)
        }
        else at
        ticks <- sapply(labels, link)
        list(at = ticks, labels = as.character(labels))
    }
    thresholds <- x$thresholds
    has.thresholds <- !is.null(thresholds)
    if (missing(ylab)) {
        ylab <- if (has.thresholds) 
            paste(x$response, ": ", paste(x$y.levels, collapse = ", "), 
                sep = "")
        else x$response
    }
    if (has.thresholds) {
        threshold.labels <- abbreviate(x$y.levels, minlength = 1)
        threshold.labels <- paste(" ", paste(threshold.labels[-length(threshold.labels)], 
            threshold.labels[-1], sep = " - "), " ", sep = "")
    }
    trans.link <- x$transformation$link
    trans.inverse <- x$transformation$inverse
    if (!rescale.axis) {
        x$lower <- trans.inverse(x$lower)
        x$upper <- trans.inverse(x$upper)
        x$fit <- trans.inverse(x$fit)
        trans.link <- trans.inverse <- I
    }
    require(lattice)
    split <- c(col, row, ncol, nrow)
    ylab
    x.data <- x$data
    effect <- paste(sapply(x$variables, "[[", "name"), collapse = "*")
    vars <- x$variables
    x <- as.data.frame(x)
    for (i in 1:length(vars)) {
        if (!(vars[[i]]$is.factor)) 
            next
        x[, i] <- factor(x[, i], levels = vars[[i]]$levels)
    }
    has.se <- !is.null(x$se)
    n.predictors <- ncol(x) - 1 - 3 * has.se
    if (n.predictors == 1) {
        range <- if (has.se) 
            range(c(x$lower, x$upper))
        else range(x$fit)
        ylim <- if (!missing(ylim)) 
            ylim
        else c(range[1] - 0.025 * (range[2] - range[1]), range[2] + 
            0.025 * (range[2] - range[1]))
        tickmarks <- make.ticks(ylim, link = trans.link, inverse = trans.inverse, 
            at = ticks$at, n = ticks$n)
        if (is.factor(x[, 1])) {
            levs <- levels(x[, 1])
            plot <- xyplot(eval(parse(text = paste("fit ~ as.numeric(", 
                names(x)[1], ")"))), strip = function(...) strip.default(..., 
                strip.names = c(factor.names, TRUE)), panel = function(x, 
                y, lower, upper, has.se, ...) {
                llines(x, y, lwd = 2, col = colors[1], type = "b", 
                  pch = 19, cex = cex, ...)
                if (has.se) {
                  llines(x, lower, lty = 2, col = colors[2])
                  llines(x, upper, lty = 2, col = colors[2])
                }
                if (has.thresholds) {
                  panel.abline(h = thresholds, lty = 3)
                  panel.text(rep(current.panel.limits()$xlim[1], 
                    length(thresholds)), thresholds, threshold.labels, 
                    adj = c(0, 0), cex = 0.75)
                  panel.text(rep(current.panel.limits()$xlim[2], 
                    length(thresholds)), thresholds, threshold.labels, 
                    adj = c(1, 0), cex = 0.75)
                }
            }, ylim = ylim, ylab = ylab, xlab = if (missing(xlab)) 
                names(x)[1]
            else xlab, scales = list(x = list(at = 1:length(levs), 
                labels = levs), y = list(at = tickmarks$at, labels = tickmarks$labels), 
                alternating = alternating), main = main, lower = x$lower, 
                upper = x$upper, has.se = has.se, data = x, ...)
            result <- update(plot, layout = if (missing(layout)) 
                c(0, prod(dim(plot)))
            else layout)
            result$split <- split
            result$more <- more
            class(result) <- c("plot.eff", class(result))
        }
        else {
            x.vals <- x.data[, names(x)[1]]
            plot <- xyplot(eval(parse(text = paste("fit ~", names(x)[1]))), 
                strip = function(...) strip.default(..., strip.names = c(factor.names, 
                  TRUE)), panel = function(x, y, x.vals, rug, 
                  lower, upper, has.se, ...) {
                  llines(x, y, lwd = 2, col = colors[1], ...)
                  if (rug) 
                    lrug(x.vals)
                  if (has.se) {
                    llines(x, lower, lty = 2, col = colors[2])
                    llines(x, upper, lty = 2, col = colors[2])
                  }
                  if (has.thresholds) {
                    panel.abline(h = thresholds, lty = 3)
                    panel.text(rep(current.panel.limits()$xlim[1], 
                      length(thresholds)), thresholds, threshold.labels, 
                      adj = c(0, 0), cex = 0.75)
                    panel.text(rep(current.panel.limits()$xlim[2], 
                      length(thresholds)), thresholds, threshold.labels, 
                      adj = c(1, 0), cex = 0.75)
                  }
                }, ylim = ylim, ylab = ylab, xlab = if (missing(xlab)) 
                  names(x)[1]
                else xlab, x.vals = x.vals, rug = rug, main = main, 
                lower = x$lower, upper = x$upper, has.se = has.se, 
                data = x, scales = list(y = list(at = tickmarks$at, 
                  labels = tickmarks$labels), alternating = alternating), 
                ...)
            result <- update(plot, layout = if (missing(layout)) 
                c(0, prod(dim(plot)))
            else layout)
            result$split <- split
            result$more <- more
            class(result) <- c("plot.eff", class(result))
        }
        return(result)
    }
    predictors <- names(x)[1:n.predictors]
    levels <- sapply(apply(x[, predictors], 2, unique), length)
    if (is.character(x.var)) {
        which.x <- which(x.var == predictors)
        if (length(which.x) == 0) 
            stop(paste("x.var = '", x.var, "' is not in the model.", 
                sep = ""))
        x.var <- which.x
    }
    if (is.character(z.var)) {
        which.z <- which(z.var == predictors)
        if (length(which.z) == 0) 
            stop(paste("z.var = '", z.var, "' is not in the model.", 
                sep = ""))
        z.var <- which.z
    }
    if (x.var == z.var) 
        z.var <- z.var + 1
    range <- if (has.se && (!multiline)) 
        range(c(x$lower, x$upper))
    else range(x$fit)
    ylim <- if (!missing(ylim)) 
        ylim
    else c(range[1] - 0.025 * (range[2] - range[1]), range[2] + 
        0.025 * (range[2] - range[1]))
    tickmarks <- make.ticks(ylim, link = trans.link, inverse = trans.inverse, 
        at = ticks$at, n = ticks$n)
    if (multiline) {
        zvals <- unique(x[, z.var])
        if (length(zvals) > min(c(length(colors), length(lines), 
            length(symbols)))) 
            stop(paste("Not enough colors, lines, or symbols to plot", 
                length(zvals), "lines"))
        if (is.factor(x[, x.var])) {
            levs <- levels(x[, x.var])
            key <- list(title = predictors[z.var], cex.title = 1, 
                border = TRUE, text = list(as.character(zvals)), 
                lines = list(col = colors[1:length(zvals)], lty = lines[1:length(zvals)], 
                  lwd = 2), points = list(pch = 1:length(zvals)))
            key <- c(key, key.args)
            plot <- xyplot(eval(parse(text = paste("fit ~ as.numeric(", 
                predictors[x.var], ")", if (n.predictors > 2) 
                  paste(" |", paste(predictors[-c(x.var, z.var)])), 
                collapse = "*"))), strip = function(...) strip.default(..., 
                strip.names = c(factor.names, TRUE)), panel = function(x, 
                y, subscripts, z, ...) {
                for (i in 1:length(zvals)) {
                  sub <- z[subscripts] == zvals[i]
                  llines(x[sub], y[sub], lwd = 2, type = "b", 
                    col = colors[i], pch = symbols[i], lty = lines[i], 
                    cex = cex, ...)
                }
                if (has.thresholds) {
                  panel.abline(h = thresholds, lty = 3)
                  panel.text(rep(current.panel.limits()$xlim[1], 
                    length(thresholds)), thresholds, threshold.labels, 
                    adj = c(0, 0), cex = 0.75)
                  panel.text(rep(current.panel.limits()$xlim[2], 
                    length(thresholds)), thresholds, threshold.labels, 
                    adj = c(1, 0), cex = 0.75)
                }
            }, ylim = ylim, ylab = ylab, xlab = if (missing(xlab)) 
                predictors[x.var]
            else xlab, z = x[, z.var], scales = list(x = list(at = 1:length(levs), 
                labels = levs), y = list(at = tickmarks$at, labels = tickmarks$labels), 
                alternating = alternating), zvals = zvals, main = main, 
                key = key, data = x, ...)
            result <- update(plot, layout = if (missing(layout)) 
                c(0, prod(dim(plot)))
            else layout)
            result$split <- split
            result$more <- more
            class(result) <- c("plot.eff", class(result))
        }
        else {
            x.vals <- x.data[, names(x)[x.var]]
            key <- list(title = predictors[z.var], cex.title = 1, 
                border = TRUE, text = list(as.character(zvals)), 
                lines = list(col = colors[1:length(zvals)], lty = lines[1:length(zvals)], 
                  lwd = 2))
            key <- c(key, key.args)
            plot <- xyplot(eval(parse(text = paste("fit ~", predictors[x.var], 
                if (n.predictors > 2) 
                  paste(" |", paste(predictors[-c(x.var, z.var)])), 
                collapse = "*"))), strip = function(...) strip.default(..., 
                strip.names = c(factor.names, TRUE)), panel = function(x, 
                y, subscripts, x.vals, rug, z, ...) {
                if (rug) 
                  lrug(x.vals)
                for (i in 1:length(zvals)) {
                  sub <- z[subscripts] == zvals[i]
                  llines(x[sub], y[sub], lwd = 2, type = "l", 
                    col = colors[i], lty = lines[i], cex = cex, 
                    ...)
                }
                if (has.thresholds) {
                  panel.abline(h = thresholds, lty = 3)
                  panel.text(rep(current.panel.limits()$xlim[1], 
                    length(thresholds)), thresholds, threshold.labels, 
                    adj = c(0, 0), cex = 0.75)
                  panel.text(rep(current.panel.limits()$xlim[2], 
                    length(thresholds)), thresholds, threshold.labels, 
                    adj = c(1, 0), cex = 0.75)
                }
            }, ylim = ylim, ylab = ylab, xlab = if (missing(xlab)) 
                predictors[x.var]
            else xlab, x.vals = x.vals, rug = rug, z = x[, z.var], 
                zvals = zvals, main = main, key = key, data = x, 
                scales = list(y = list(at = tickmarks$at, labels = tickmarks$labels), 
                  alternating = alternating), ...)
            result <- update(plot, layout = if (missing(layout)) 
                c(0, prod(dim(plot)))
            else layout)
            result$split <- split
            result$more <- more
            class(result) <- c("plot.eff", class(result))
        }
        return(result)
    }
    if (is.factor(x[, x.var])) {
        levs <- levels(x[, x.var])
        plot <- xyplot(eval(parse(text = paste("fit ~ as.numeric(", 
            predictors[x.var], ") |", paste(predictors[-x.var], 
                collapse = "*")))), strip = function(...) strip.default(..., 
            strip.names = c(factor.names, TRUE)), panel = function(x, 
            y, subscripts, lower, upper, has.se, ...) {
            llines(x, y, lwd = 2, type = "b", col = colors[1], 
                pch = 19, cex = cex, ...)
            if (has.se) {
                llines(x, lower[subscripts], lty = 2, col = colors[2])
                llines(x, upper[subscripts], lty = 2, col = colors[2])
            }
            if (has.thresholds) {
                panel.abline(h = thresholds, lty = 3)
                panel.text(rep(current.panel.limits()$xlim[1], 
                  length(thresholds)), thresholds, threshold.labels, 
                  adj = c(0, 0), cex = 0.75)
                panel.text(rep(current.panel.limits()$xlim[2], 
                  length(thresholds)), thresholds, threshold.labels, 
                  adj = c(1, 0), cex = 0.75)
            }
        }, ylim = ylim, ylab = ylab, xlab = if (missing(xlab)) 
            predictors[x.var]
        else xlab, scales = list(x = list(at = 1:length(levs), 
            labels = levs), y = list(at = tickmarks$at, labels = tickmarks$labels), 
            alternating = alternating), main = main, lower = x$lower, 
            upper = x$upper, has.se = has.se, data = x, ...)
        result <- update(plot, layout = if (missing(layout)) 
            c(0, prod(dim(plot)))
        else layout)
        result$split <- split
        result$more <- more
        class(result) <- c("plot.eff", class(result))
    }
    else {
        x.vals <- x.data[, names(x)[x.var]]
        plot <- xyplot(eval(parse(text = paste("fit ~", predictors[x.var], 
            "|", paste(predictors[-x.var], collapse = "*")))), 
            strip = function(...) strip.default(..., strip.names = c(factor.names, 
                TRUE)), panel = function(x, y, subscripts, x.vals, 
                rug, lower, upper, has.se, ...) {
                llines(x, y, lwd = 2, col = colors[1], ...)
                if (rug) 
                  lrug(x.vals)
                if (has.se) {
                  llines(x, lower[subscripts], lty = 2, col = colors[2])
                  llines(x, upper[subscripts], lty = 2, col = colors[2])
                }
                if (has.thresholds) {
                  panel.abline(h = thresholds, lty = 3)
                  panel.text(rep(current.panel.limits()$xlim[1], 
                    length(thresholds)), thresholds, threshold.labels, 
                    adj = c(0, 0), cex = 0.75)
                  panel.text(rep(current.panel.limits()$xlim[2], 
                    length(thresholds)), thresholds, threshold.labels, 
                    adj = c(1, 0), cex = 0.75)
                }
            }, ylim = ylim, ylab = ylab, xlab = if (missing(xlab)) 
                predictors[x.var]
            else xlab, x.vals = x.vals, rug = rug, main = main, 
            lower = x$lower, upper = x$upper, has.se = has.se, 
            data = x, scales = list(y = list(at = tickmarks$at, 
                labels = tickmarks$labels), alternating = alternating), 
            ...)
        result <- update(plot, layout = if (missing(layout)) 
            c(0, prod(dim(plot)))
        else layout)
        result$split <- split
        result$more <- more
        class(result) <- c("plot.eff", class(result))
    }
    return(result)
}

