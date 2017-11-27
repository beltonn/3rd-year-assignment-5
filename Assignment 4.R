install.packages("jsonlite")
library(jsonlite)
install.packages("httpuv")
library(httpuv)
install.packages("httr")
library(httr)
install.packages("xml2")
library(xml2)


oauth_endpoints("github")

myapp <- oauth_app(appname = "Assignment4",
                   key = "7f6a7eb252448b36acfd",
                   secret = "58f5d9be6a4da805d9440deb74852c88ce6078fa")

github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

gtoken <- config(token = github_token)

cat("GITHUB_PAT=58f5d9be6a4da805d9440deb74852c88ce6078fa\n",
    file = file.path(normalizePath("~/"), ".Renviron"), append = TRUE)

#FUNCTIONS TO DO WITH FOLLOWERS
#shows who username is following
listFollowing1 <- function(username)
{
  getFollowing <- GET(paste0("https://api.github.com/users", "aoifetiernan", "/following"), gtoken)
  json1 = content(getFollowing)
  json1
  githubDF = jsonlite::fromJSON(jsonlite::toJSON(json1))
  following <- githubDF$login
  return (following);
}
listFollowing1("aoifetiernan");



#shows who the logged in developer is following
listFollowing2 <- function()
{
  getFollowing <- GET("https://api.github.com/user/following", gtoken)
  json1 = content(getFollowing)
  json1
  githubDF = jsonlite::fromJSON(jsonlite::toJSON(json1))
  following <- githubDF$login
  return (following);
}
listFollowing2()


#shows who the first 80 users of github is following
listFirst30UsersFollowing <- function(){
  users <- GET("https://api.github.com/users?per_page=80", gtoken)
  json1 = content(users)
  githubDF = jsonlite::fromJSON(jsonlite::toJSON(json1))
  usernames <- c(githubDF$login)
  usernames
  allFollowing = lapply(usernames, listFollowing1)
  allFollowing
  return (allFollowing);
}
listFirst30UsersFollowing()


#lists followers of username
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



#lists followers of logged in developer
listFollowers <- function()
{
  getFollowers <- GET(paste0("https://api.github.com/user/followers"), gtoken)
  json1 = content(getFollowers)
  json1
  githubDF = jsonlite::fromJSON(jsonlite::toJSON(json1))
  following <- githubDF$login
  return (following);
}
listFollowers()



#check if one user followers another user
user1FollowUser2 <- function(username, targetUser)
{
  user1FollowsUser2 <- GET(paste0("https://api.github.com/users/",username, "/following/",targetUser),gtoken)
  if(user1FollowsUser2$status == 404){
       return(FALSE);
  }
  return(TRUE);
}
#print(user1FollowUser2("aoifetiernan", "beltonn"))



#check if a user is following the logged in developer
user1FollowUser2 <- function(targetUser)
{
  user1FollowsUser2 <- GET(paste0("https://api.github.com/user/following/",targetUser),gtoken)
  if(user1FollowsUser2$status == 404){
    return(FALSE);
  }
  return(TRUE);
}
print(user1FollowUser2("aoifetiernan"))



#REPOSITORIES
#List logged in developer's repositores
listRepositories <- function()
{
  repos <- GET("https://api.github.com/user/repos",gtoken)
  json1 = content(repos)
  json1
  githubDF = jsonlite::fromJSON(jsonlite::toJSON(json1))
  listOfRepos <- githubDF$name
  return(listOfRepos);
}

#list repostitories of username
listReposOfUser <- function(username)
{
  repos <- GET(paste0("https://api.github.com/users/", username, "/repos"),gtoken)
  json1 = content(repos)
  json1
  githubDF = jsonlite::fromJSON(jsonlite::toJSON(json1))
  listOfRepos <- githubDF$name
  return(listOfRepos);
}
listReposOfUser("aoifetiernan")

#list branches of logged in developer's branches of a given repository
listOfBranches <- function(repository)
{
  branches <- GET(paste0("https://api.github.com/repos/aoifetiernan/", repository, "/branches"),gtoken)
  json1 = content(branches)
  json1
  githubDF = jsonlite::fromJSON(jsonlite::toJSON(json1))
  listOfBranches <- githubDF$name
  return(listOfBranches);
}
#listOfBranches("final-assignment-1-3rd-year")
listOfBranches("lowestCommonAncestor")


#list branches of username branches of a given repository
listOfBranches <- function(owner, repository)
{
  branches <- GET(paste0("https://api.github.com/repos/", owner, "/", repository, "/branches"),gtoken)
  json1 = content(branches)
  json1
  githubDF = jsonlite::fromJSON(jsonlite::toJSON(json1))
  listOfBranches <- githubDF$name
  return(listOfBranches);
}
listOfBranches("aoifetiernan", "lowestCommonAncestor")


#list commits of a certain repository
listOfCommits <- function(owner, repository)
{
  commits <- GET(paste0("https://api.github.com/repos/", owner,"/", repository, "/commits"),gtoken)
  json1 = content(commits)
  json1
  githubDF = jsonlite::fromJSON(jsonlite::toJSON(json1))
  listOfCommits <- githubDF$commit$message
  return(listOfCommits);
}
listOfCommits("beltonn", "final-assignment-1-3rd-year")


#ACTIVITIES
#get list of first 100 events with usersnames
listFirst100Events <- function()
{
  events <- GET("https://api.github.com/events?per_page=100",gtoken)
  json1 = content(events)
  json1
  githubDF = jsonlite::fromJSON(jsonlite::toJSON(json1))
  listOfEvents <- cbind(githubDF$actor$login, githubDF$type)
  return (listOfEvents);
}  
listFirst100Events()


#list repository events 
list80RepositoryEvents <- function(owner, repository)
{
  events <- GET(paste0("https://api.github.com/repos/", owner, "/", repository,"/","events?per_page=100"),gtoken)
  json1 = content(events)
  json1
  githubDF = jsonlite::fromJSON(jsonlite::toJSON(json1))
  listOfEvents = githubDF$type
  return (listOfEvents);
}  
list80RepositoryEvents("beltonn", "final-assignment-1-3rd-year")


#list repositories being watched 
listRepositoriesWatched <- function(username)
{
  subs <- GET(paste0("https://api.github.com/users/", username, "/subscriptions"),gtoken)
  json1 = content(subs)
  json1
  githubDF = jsonlite::fromJSON(jsonlite::toJSON(json1))
  listOfEvents = githubDF$owner$login
  listOfEvents
  return (listOfEvents);
}  
listRepositoriesWatched("mojombo")


