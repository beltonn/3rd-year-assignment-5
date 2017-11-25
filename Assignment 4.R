#install.packages("jsonlite")
library(jsonlite)
#install.packages("httpuv")
library(httpuv)
#install.packages("httr")
library(httr)
install.packages("xml2")
library(xml2)


oauth_endpoints("github")

myapp <- oauth_app(appname = "Assignment4",
                   key = "7f6a7eb252448b36acfd",
                   secret = "58f5d9be6a4da805d9440deb74852c88ce6078fa")

github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

gtoken <- config(token = github_token)


#shows who they are following
listFollowing <- function(username)
{
  getFollowing <- GET(paste0("https://api.github.com/users/", username, "/following"), gtoken)
  json1 = content(getFollowing)
  json1
  githubDF = jsonlite::fromJSON(jsonlite::toJSON(json1))
  following <- githubDF$login
  return (following);
}

#listFollowing("beltonn");

#lists followers
listFollowers <- function(username)
{
  getFollowers <- GET(paste0("https://api.github.com/users/", username, "/followers"), gtoken)
  json1 = content(getFollowers)
  json1
  githubDF = jsonlite::fromJSON(jsonlite::toJSON(json1))
  following <- githubDF$login
  return (following);
}

#listFollowers("beltonn")

