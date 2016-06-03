## Should run 02-create-variables.R first

library(glmnet)
library(ggplot2)

X = cbind(article_dat$page_order, article_dat$issue, sect_mat, title_mat, author_mat)
Y = log(article_dat$views)

xbar = colMeans(X)
xss  = colSums(X^2)
xsd  = sqrt((xss - nrow(X) * xbar^2) / (nrow(X) - 1))

mod = glmnet(X, Y)
sum(coef(mod, s = 0.0475, exact = TRUE) != 0)

var_names = c("PageOrder [Other]", "IssueNO. [Other]",
              paste(sect_dict, "[Section]"),
              paste(dict, "[Term]"),
              paste(author_dict, "[Author]"))
cf = coef(mod, s = 0.0475, exact = TRUE)[-1]
# ind = (abs(cf) > 1e-2)
ind = abs(cf) > 1e-6
vals = cf[ind] * xsd[ind]
vars = var_names[ind]
names(vals) = vars



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

