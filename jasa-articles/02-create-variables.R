library(text2vec)
library(Matrix)
library(glmnet)
library(ggplot2)

article_dat = read.csv("data/jasa.csv", stringsAsFactors = FALSE)

## Articles with large number of downloads
article_dat[article_dat$views >= 2000, ]
## One outlier
article_dat = article_dat[article_dat$views < 10000, ]


## Document-term matrix
tokens = article_dat$title %>% tolower() %>% word_tokenizer()
sw = c("of", "for", "and", "in", "with", "a", "the", "to", "on", "an", "by", "its")
## 3-gram, mininum term count = 2
it = itoken(tokens)
title_voc = create_vocabulary(it, ngram = c(1, 3), stopwords = sw) %>%
    prune_vocabulary(term_count_min = 2)
dict = title_voc$vocab$terms
vectorizer = vocab_vectorizer(title_voc)
it = itoken(tokens)
title_mat  = create_dtm(it, vectorizer)


## Article-author matrix
authors = ifelse(article_dat$authors == "", "#", article_dat$authors)
authors = strsplit(authors, "#")
author_tab = table(unlist(authors))
## At least two papers to be included
author_dict = names(author_tab)[author_tab > 1]
author_match = lapply(authors, function(x) na.omit(match(x, author_dict)))
author_i = rep(seq_along(author_match), sapply(author_match, length))
author_j = unlist(author_match)
author_mat = sparseMatrix(i = author_i, j = author_j, x = 1)


## Category of articles (application section, method section, etc.)
sects = as.character(article_dat$sect)
## Combine similar categories
sects = gsub(".*Presidential Address.*", "ASA Presidential Address", sects)
sects = gsub(".*Case Studies.*", "Application and Case Studies", sects)
sects = gsub(".*Original Articles.*", "Primary Article", sects)
sects = gsub(".*Fisher Lecture.*", "Fisher Lecture", sects)
sects = gsub(".*Review.*", "Review", sects)
sects = gsub(".*Correction.*|.*Errata.*|.*Corrigendum.*", "Correction", sects)
sects = gsub(".*Index.*", "Index", sects)
sects = gsub(".*Letters to the Editor.*", "Letters to the Editors", sects)
sects = gsub(".*Editorial.*", "Editorial", sects)
sects = reorder(sects, rep(1, length(sects)), sum)
sect_mat = model.matrix(~ sects + 0)[, -1]
sect_dict = gsub("^sects", "", colnames(sect_mat))




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

library(ggplot2)
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

