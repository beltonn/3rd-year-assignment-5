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
listFollowers1 <- function(username)
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
listFollowers2 <- function()
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

#list first 10 repostitories of username
list10ReposOfUser <- function(username)
{
  repos <- GET(paste0("https://api.github.com/users/", username, "/repos?per_page=10"),gtoken)
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
#get list of first 100 events 
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


write.csv(listRepositoriesWatched("mojombo"), file="/Users/niamhbelton/Documents/3rd year/Software Engineering/myData.csv", append = TRUE)
?write.csv

#ASSIGNMENT 6
#taking the first 10 members from the first 10 organizations created on github
first10membersOfFirst10Orgs <- function()
{
  organizations <- GET("https://api.github.com/organizations?per_page=10",gtoken)
  json1 = content(organizations)
  json1
  githubDF = jsonlite::fromJSON(jsonlite::toJSON(json1))
  listOfOrgs = githubDF$login
  listOfOrgs
  data <- lapply(listOfOrgs, getMembers)
  l <- list()
  for(i in 1:length(data)){
    l <- c(l, data[[i]])
  }
  return (l);
}  

getMembers <- function(organization)
{
  members <- GET(paste0("https://api.github.com/orgs/",organization,"/members?per_page=10"),gtoken)
  json1 = content(members)
  json1
  githubDF = jsonlite::fromJSON(jsonlite::toJSON(json1))
  listOfMembers = githubDF$login
  listOfMembers
  return(listOfMembers);
}
first10membersOfFirst10Orgs()

write.csv(first10membersOfFirst10Orgs(), file="/Users/niamhbelton/Documents/3rd year/Software Engineering/myData.csv")


#Looking at each member's productivity - Number of commits and LOC

#for each person, get their first 10 repositories, get the total number of commits
#from all their repositories
inter <- function()
{
  members <- first10membersOfFirst10Orgs()
  DF <- lapply(members, list10ReposOfUser)
  number <- c()
  for(i in 1:length(members)){
    blah <- totalNumberCommits(members[i], DF[[i]][])
    print(blah)
    if(length(blah)==0){
      number[i] = 0
    }
    else{
      number[i] = blah
    }
     # n <- cbind(n, totalNumberCommits(members[i], DF[[i]][]))
    print(number[i])
    print(paste0("on = ", i))
    print(length(number))
    print(number)
  }
  return(number);
}
totalNumberCommits <- function(owner, repositories)
{
  
  total = 0
  for(i in 1:length(repositories)){
    total = total + numberOfCommits(owner, repositories[[i]])
  }
  return(total);
}


numberOfCommits <- function(owner, repository)
{
 

  commits <- GET(paste0("https://api.github.com/repos/",owner,"/",repository,"/stats/participation"),gtoken)
  json1 = content(commits)
  json1
  githubDF = jsonlite::fromJSON(jsonlite::toJSON(json1))
  commitsTotal = 0
  for(i in 1:52){
    commitsTotal = commitsTotal + githubDF$owner[[i]]
  }
  commitsTotal
  return(commitsTotal);
}



#returns total additions for each user in their first 10 repositories
inter2 <- function()
{
  members <- first100membersOfFirst50Orgs()
  DF <- lapply(members, listReposOfUser)
  n <- c()
  for(i in 1:length(members)){
    n <- cbind(n, totalLOC(members[i], DF[[i]][]))
    print(n[i])
    print(paste0("on = ", i))
    print(length(n))
    print(n)
  }
  return(number);
}
totalLOC <- function(owner, repositories)
{
  total = 0
  for(i in 1:length(repositories)){
    total = total + LOC(owner, repositories[[i]])
  }
  return(total);
}
LOC <- function(owner, repository)
{
  #owner = members[1]
  #repository = DF[[1]][[1]]
  additions <- GET(paste0("https://api.github.com/repos/",owner,"/",repository,"/stats/contributors"),gtoken)
  
  json1 = content(additions)
  json1
  githubDF = jsonlite::fromJSON(jsonlite::toJSON(json1))
  
  #githubDF[[2]] #lists of for each week from each contributor
  #githubDF[[1]] #gives you total commits to a repository
  #githubDF[[3]]$login #list of contributors
  
  #length(githubDF[[3]])
  index <- 0
  for(i in 1:length(githubDF[[3]]$login)){
    if(githubDF[[3]]$login[[i]] == owner){
      index = i
    }
  }
  if(index == 0){
    return(0);
  }
  additionsTotal = 0
  for(i in 1:length(githubDF[[2]][[1]]$a)){
    additionsTotal = additionsTotal + githubDF[[2]][[index]]$a[[i]]
  }
  additionsTotal
  return(additionsTotal);
}


  
  


