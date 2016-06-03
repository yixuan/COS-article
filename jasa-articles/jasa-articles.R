library(rvest)

## From: http://www.tandfonline.com/loi/uasa20
vol_list   = read_html("vol_list.xml")
issue_info = vol_list %>% html_nodes("div .issueInfo")
issue_link = issue_info %>% html_children() %>% html_attr("href")
issue_text = gsub("[[:space:]]+", " ", issue_info %>% html_text(trim = TRUE))
issue_text  = strsplit(issue_text, " ")

## Data for issues
base_link  = "http://www.tandfonline.com"
issue_dat  = data.frame(link = paste(base_link, issue_link, sep = ""),
                        stringsAsFactors = FALSE)
issue_dat$no    = sapply(issue_text, function(x) as.integer(x[2]))
issue_dat$year  = sapply(issue_text, function(x) as.integer(x[3]))
issue_dat$pages = sapply(issue_text, function(x) x[5])

## List of issue pages
issue_content = vector(mode = "list", nrow(issue_dat))
for(i in seq_along(issue_content))
{
    print(i)
    issue_content[[i]] = read_html(issue_dat$link[i])
}

article_info = function(article)
{
    children = article %>% html_children() %>% html_name
    sect = if("h2" %in% children) article %>% html_node("h2") %>% html_text else ""
    sect = gsub("^[[:space:]]|[[:space:]]$", "", sect)
    title = article %>% html_node("a.entryTitle") %>% html_text()
    authors = article %>% html_nodes("span.hlFld-ContribAuthor") %>% html_text()
    views = (article %>% html_nodes("div.articleUsage"))[[1]] %>%
        html_text()
    views = as.integer(gsub("Article Views: ([[:digit:]]+).*", "\\1", views))
    data.frame(sect = sect, title = title,
               authors = paste(authors, collapse = "#"),
               views = views,
               stringsAsFactors = FALSE)
}

articles = vector("list", length(issue_content))
for(i in seq_along(issue_content))
{
    print(i)
    article_list = issue_content[[i]] %>% html_nodes("div.article.original.as2")
    article_dat  = do.call(rbind, lapply(article_list, article_info))
    for(j in 2:nrow(article_dat))
        if(article_dat$sect[j] == "")  article_dat$sect[j] = article_dat$sect[j - 1]
    article_dat$issue = issue_dat$no[i]
    article_dat$year  = issue_dat$year[i]
    article_dat$page_order = seq_along(article_dat$sect)
    articles[[i]] = article_dat
}

article_dat = do.call(rbind, articles)
# write.csv(article_dat, "jasa.csv", row.names = FALSE)
# article_dat = read.csv("jasa.csv", stringsAsFactors = FALSE)



library(text2vec)
library(Matrix)
# article_dat[article_dat$views >= 2000, ]
# One outlier
article_dat = article_dat[article_dat$views < 10000, ]

## Title words
# titles = gsub("[[:punct:][:digit:]]+", " ", article_dat$title)
# titles = gsub("^[[:space:]]+|[[:space:]]+&", "", titles)
# titles = gsub("[[:space:]]+", " ", titles)
# titles = tolower(titles)
# words  = strsplit(titles, " ")
# 
# dict   = unique(unlist(words))
# title_match = lapply(words, function(x) unique(match(x, dict)))
# word_i = rep(seq_along(title_match), sapply(title_match, length))
# word_j = unlist(title_match)
# 
# title_mat = sparseMatrix(i = word_i, j = word_j, x = 1)
tokens = article_dat$title %>% tolower() %>% word_tokenizer()
sw = c("of", "for", "and", "in", "with", "a", "the", "to", "on", "an", "by", "its")
it = itoken(tokens)
title_voc = create_vocabulary(it, ngram = c(1, 3), stopwords = sw) %>%
    prune_vocabulary(term_count_min = 2)
dict = title_voc$vocab$terms
vectorizer = vocab_vectorizer(title_voc)
it = itoken(tokens)
title_mat  = create_dtm(it, vectorizer)

## Authors
authors = ifelse(article_dat$authors == "", "#", article_dat$authors)
authors = strsplit(authors, "#")

author_tab = table(unlist(authors))
author_dict = names(author_tab)[author_tab > 1]
author_match = lapply(authors, function(x) na.omit(match(x, author_dict)))
author_i = rep(seq_along(author_match), sapply(author_match, length))
author_j = unlist(author_match)
author_mat = sparseMatrix(i = author_i, j = author_j, x = 1)

## Section of articles (application, theory, etc.)
sects = as.character(article_dat$sect)
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



library(glmnet)
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

