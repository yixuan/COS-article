library(rvest)

## From: http://www.tandfonline.com/loi/uasa20
vol_list = read_html("data/vol_list.html")

## Issue information is contained in <div class="issueInfo">
# <div class="issueInfo">
#     <a class="toclink" href="/toc/uasa20/111/513">
#     Issue 513</a>
#     2016
#     pages
#     1-445
# </div>
issue_info = vol_list %>% html_nodes("div.issueInfo")
issue_link = issue_info %>% html_children() %>% html_attr("href")
issue_text = gsub("[[:space:]]+", " ", issue_info %>% html_text(trim = TRUE))
issue_text  = strsplit(issue_text, " ")

## Data for journal issues
base_link  = "http://www.tandfonline.com"
issue_dat  = data.frame(link = paste(base_link, issue_link, sep = ""),
                        stringsAsFactors = FALSE)
## Issue / 513 / 2016 / pages / 1-445
issue_dat$no    = sapply(issue_text, function(x) as.integer(x[2]))
issue_dat$year  = sapply(issue_text, function(x) as.integer(x[3]))
issue_dat$pages = sapply(issue_text, function(x) x[5])

## List of issue pages
issue_content = vector(mode = "list", nrow(issue_dat))
for(i in seq_along(issue_content))
{
    print(i)
    ## Download page content
    issue_content[[i]] = read_html(issue_dat$link[i])
}

## Get information from an article node
article_info = function(article)
{
    children = article %>% html_children() %>% html_name()
    sect = if("h2" %in% children) article %>% html_node("h2") %>% html_text() else ""
    sect = gsub("^[[:space:]]|[[:space:]]$", "", sect)
    title = article %>% html_node("a.entryTitle") %>% html_text()
    authors = article %>% html_nodes("span.hlFld-ContribAuthor") %>% html_text()
    views = (article %>% html_nodes("div.articleUsage"))[[1]] %>% html_text()
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
write.csv(article_dat, "data/jasa.csv", row.names = FALSE)
