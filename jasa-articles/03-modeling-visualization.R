## Should run 02-create-variables.R first

library(glmnet)
library(ggplot2)

X = cbind(article_dat$page_order, article_dat$issue, sect_mat, title_mat, author_mat)
Y = log(article_dat$views)
var_names = c("PageOrder [Other]", "IssueNO. [Other]",
              paste(sect_dict, "[Section]"),
              paste(dict, "[Term]"),
              paste(author_dict, "[Author]"))


## Used to calculate standardized coefficients
xbar = colMeans(X)
xss  = colSums(X^2)
xsd  = sqrt((xss - nrow(X) * xbar^2) / (nrow(X) - 1))

## Lasso regression
mod = glmnet(X, Y)
## Select a penalty parameter that selects 30 variables
sum(coef(mod, s = 0.048, exact = TRUE) != 0)
## Exclude intercept
cf = coef(mod, s = 0.048, exact = TRUE)[-1]
## Non-zero coefficients
ind = abs(cf) > 1e-6
## Coefficients
vals = cf[ind] * xsd[ind]
## Variable names
vars = var_names[ind]
names(vals) = vars

## Visualize result
gd = data.frame(var = vars, coef = vals)
gd$var = reorder(gd$var, gd$coef)
gd$sign = factor(-sign(gd$coef))
ggplot(gd, aes(x = var, y = coef)) +
    geom_bar(aes(fill = sign), stat = "identity") +
    scale_fill_hue(guide = FALSE) +
    theme_grey(base_size = 20) +
    xlab("Variables") + ylab("Coefficients") +
    coord_flip(ylim = c(-0.25, 0.15)) +
    annotate("text", length(vals), 0.14,
             label = paste(round(max(vals), 3), "->"),
             color = "white", size = 5)

