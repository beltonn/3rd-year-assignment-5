install.packages("jsonlite")
library(jsonlite)
install.packages("httpuv")
library(httpuv)
install.packages("httr")
library(httr)
install.packages("xml2")
library(xml2)
install.packages("fmsb")
library("fmsb")
require(devtools)
install.packages("ggplot2")
library("ggplot2")

library(devtools)
devtools::install_github('ramnathv/rCharts', auth_token = "2586ed928db8d8c37bb24ac966013602fc01cbfc")
library(rCharts)

oauth_endpoints("github")

myapp <- oauth_app(appname = "VisualisationAssignment",
                   key = "2fd518b1da61c4a20bff",
                   secret = "14020e2776f7b12ba465c21c8ed053e065a6c4a5")

github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

gtoken <- httr::config(token = github_token)

#cat("GITHUB_PAT=myapp\n",
#    file = file.path(normalizePath("~/"), ".Renviron"), append = TRUE)
#cat("GITHUB_PAT=14020e2776f7b12ba465c21c8ed053e065a6c4a5\n",
#    file = file.path(normalizePath("~/"), ".Renviron"), append = TRUE)

#FUNCTIONS TO DO WITH FOLLOWERS
#shows who username is following
listFollowing1 <- function(username)
{
  getFollowing <- GET(paste0("https://api.github.com/users", username, "/following"), gtoken)
  json1 = content(getFollowing)
  json1
  githubDF = jsonlite::fromJSON(jsonlite::toJSON(json1))
  following <- githubDF$login
  return (following);
}


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
  getFollowers <- GET(paste0("https://api.github.com/users/", username, "/followers?per_page=200"), gtoken)
  json1 =content(getFollowers)
  json1
  githubDF = jsonlite::fromJSON(jsonlite::toJSON(json1))
  following <- githubDF$login
  return (following);
}
listFollowers1("aoifetiernan")



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
listFollowers2()



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
#list 50 repos of user
list50ReposOfUser <- function(username)
{
  repos <- GET(paste0("https://api.github.com/users/", username, "/repos?per_page=50"),gtoken)
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



#ASSIGNMENT 6
#taking the first 10 members from the first 10 organizations created on github
getOrganizations <- function() #returns a list of 30 organizations
{
  organizations <- GET("https://api.github.com/organizations",gtoken)
  json1 = content(organizations)
  json1
  githubDF = jsonlite::fromJSON(jsonlite::toJSON(json1))
  listOfOrgs = githubDF$login
  return(listOfOrgs);
}
getOrgsWithMembers <- function() #returns a list of orgs with at least 3 members
{
  listOfOrgs <- getOrganizations()
  orgs <- list() 
  j <- 1
  i <- 1
  l <- list()
  while(length(orgs) < 10) {
    mem <- getMembers(listOfOrgs[i])
    if(length(mem)>2){
      orgs[j] = listOfOrgs[i]
      j = j+1
    }
    i = i +1;
  }
  return(orgs);
}
first10membersOf10OrgsVector <- function() #returns a vector of max first 10 members of 10 orgs
{
  listOfOrgs <- getOrgsWithMembers()
  l <- list()
  for(i in 1:length(listOfOrgs)){
    mem <- getMembers(listOfOrgs[i])
    l <- c(l, mem)
  }
  l
  return (l);
}  
membersAndOrgs <- function() #returns a data frame of Members of each organization
{
  listOfOrgs <- getOrgsWithMembers()
  DF <- lapply(listOfOrgs, getMembers)
  return (DF);
}  
getMembers <- function(organization) #returns max 10 members
{
  members <- GET(paste0("https://api.github.com/orgs/",organization,"/members?per_page=10"),gtoken)
  json1 = content(members)
  json1
  githubDF = jsonlite::fromJSON(jsonlite::toJSON(json1))
  listOfMembers = githubDF$login
  listOfMembers
  return(listOfMembers);
}
orgsVector <- function()
{
  DF <- membersAndOrgs()
  orgs <- getOrgsWithMembers()
  c <- c()
  count <- 0
  for(i in 1:length(DF)){
    for(j in 1:length(DF[[i]][])){
      count = count +1
      c[count] = orgs[i]
    }
  }
  return(c);
}

BranchesOrg <- function() #returns number of branches for max 10 repos per organization
{
  organizations <- getOrgsWithMembers()
  DF <- lapply(organizations, list10ReposOfOrg)
  c <- c()
  for(i in 1:length(organizations)){
    c[i] = 0
    for(j in 1:length(DF[[i]][])){
      b <- listOfBranches(organizations[i], DF[[i]][[j]])
      c[i] = c[i] + length(b)
    }
    
  }
  return(c);
}
#list 200 repos of org
list200ReposOfOrg <- function(organization)
{
  repos <- GET(paste0("https://api.github.com/orgs/", organization, "/repos?per_page=200"),gtoken)
  json1 = content(repos)
  json1
  githubDF = jsonlite::fromJSON(jsonlite::toJSON(json1))
  listOfRepos <- githubDF$name
  return(listOfRepos);
}
numberOfOrgRepositories <- function() #returns number of repos of each organization (max 200)
{
  organizations <- getOrgsWithMembers()
  DF <- lapply(organizations, list200ReposOfOrg)
  repos <- c()
  for(i in 1:length(organizations)){
    repos[i] = length(DF[[i]][])
  }
  return(repos);
}
commOrganizations <- function() #returns a list of total commits per organization
{
  organizations <- getOrgsWithMembers()
  DF <- lapply(organizations, list10ReposOfOrg)
  numberOfComm <- c()
  for(i in 1:length(organizations)){
    print(i)
    b <- totalNumberCommitsOrg(organizations[i], DF[[i]][])
    if(length(b)==0){
      numberOfComm[i] = 0
    }
    else{
      numberOfComm[i] = b
    }
  }
  return(numberOfComm);
}
totalNumberCommitsOrg <- function(owner, repositories) 
{
  
  total = 0
  for(i in 1:length(repositories)){
    print(i)
    total = total + numberOfCommitsOrg(owner, repositories[[i]])
    print(total)
  }
  return(total);
}
numberOfCommitsOrg <- function(owner, repository)
{
  commits <- GET(paste0("https://api.github.com/repos/",owner,"/",repository,"/stats/participation"),gtoken)
  json1 = content(commits)
  json1
  githubDF = jsonlite::fromJSON(jsonlite::toJSON(json1))
  commitsTotal = 0
  if(length(githubDF$all) == 0){
    return(commitsTotal)
  }
  for(i in 1:52){
    commitsTotal = commitsTotal + githubDF$all[[i]]
  }
  return(commitsTotal);
}

spiderVisualisation <- function()
{
  numBranches <- BranchesOrg()
  Repositories <- numberOfOrgRepositories()
  numberOfComm <- commOrganizations()
  numberOfComm[[10]] =50
  spiderDF1 <- cbind(numberOfComm, numBranches, Repositories)
  write.csv(spiderDF1, file="/Users/niamhbelton/Documents/3rd year/Software Engineering/spider.csv")
  
  library(fmsb)
  set.seed(99)
  data=as.data.frame(spiderDF1 , ncol=4)
  colnames(data)=c("Commits" , "Branches", "Repositories")
  rownames(data)= listOfOrgs
  data=rbind(rep(36,3) , rep(0,3) , data)
  R1 <- radarchart(data)
  
  colors_border=c( colours()[617], colours()[657], colours()[57], colours()[73], colours()[145], colours()[217], colours()[389], colours()[453], colours()[537], colours()[553])
  colors_in=c( colours()[617], colours()[33], colours()[657], colours()[73], colours()[145], colours()[217], colours()[389], colours()[453], colours()[537], colours()[553])
  radarchart( data  , axistype=1 , 
              #custom polygon
              pcol=colors_border, plwd=1.5 , plty=1,
              #custom the grid
              cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,36,3), cglwd=0.8,
              title = "Activity of 10 Organizations", centerzero = FALSE,
              #custom labels
              vlcex=0.8 
  )
  legend(x=0.9, y=1.4, legend = rownames(data[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)
  
  ?legend
  ?radarchart
}


Commits <- function() #returns the number of Commits on each user's first 10 repos
{ 
  members <- first10membersOf10OrgsVector()
  DF <- lapply(members, list10ReposOfUser)
  number <- c()
  for(i in 1:length(members)){
    print(i)
    b <- totalNumberCommits(members[i], DF[[i]][])
    if(length(blah)==0){
      number[i] = 0
    }
    else{
      number[i] = b
    }
    print(number)
  }
  
  return(number);
}
totalNumberCommits <- function(owner, repositories)
{
  repositories <- DF[[60]][]
  owner <- members[60]
  total = 0
  for(i in 1:length(repositories)){
    print(i)
    total = total + numberOfCommits(owner, repositories[[i]])
    print(total)
  }
  return(total);
}
numberOfCommits <- function(owner, repository)
{
  owner = organizations[1]
  repository =DF[[1]][1]
  commits <- GET(paste0("https://api.github.com/repos/",owner,"/",repository,"/stats/participation"),gtoken)
  json1 = content(commits)
  json1
  githubDF = jsonlite::fromJSON(jsonlite::toJSON(json1))
  commitsTotal = 0
  if(length(githubDF$owner) == 0){
    print("here")
    return(commitsTotal)
  }
  for(i in 1:52){
    commitsTotal = commitsTotal + githubDF$owner[[i]]
  }
  commitsTotal
  return(commitsTotal);
}
Branches <- function() #gives the number of branches per user in their first 10 repos
{
  members <- first10membersOf10OrgsVector()
  DF <- lapply(members, list10ReposOfUser)
  c <- c()
  for(i in 1:length(members)){
    c[i] = 0
    for(j in 1:length(DF[[i]][])){
      blah <- listOfBranches(members[i], DF[[i]][[j]])
      c[i] = c[i] + length(blah)
    }
    
  }
  return(c);
}
list200ReposOfUser <- function(username)
{
  repos <- GET(paste0("https://api.github.com/users/", username, "/repos?per_page=200"),gtoken)
  json1 = content(repos)
  json1
  githubDF = jsonlite::fromJSON(jsonlite::toJSON(json1))
  listOfRepos <- githubDF$name
  return(listOfRepos);
}
reposOfUser <- function()
{
  members <- first10membersOf10OrgsVector()
  DF <- lapply(members, list200ReposOfUser)
  repos <- c()
  for(i in 1:length(members)){
    c[i] <- length(DF[[i]][])
  }
  return(c);
}
#lists followers of username
listFollowers1 <- function(username)
{
  getFollowers <- GET(paste0("https://api.github.com/users/", username, "/followers?per_page=200"), gtoken)
  json1 =content(getFollowers)
  json1
  githubDF = jsonlite::fromJSON(jsonlite::toJSON(json1))
  following <- githubDF$login
  return (following);
}
getF <- function()
{
  members <- first10membersOf10OrgsVector()
  num <- c()
  for(i in 1:length(members)){
    num[i] <- length(listFollowers1(members[i]))
  }
  return(num);
}
visualiseMemberActivity <- function() #member activity vs performance
{
  library(devtools)
  devtools::install_github('ramnathv/rCharts', auth_token = "2586ed928db8d8c37bb24ac966013602fc01cbfc")
  library(rCharts)
  Commits <- Commits()
  activity <- Commits
  Branches <- Branches()
  activity <- append(activity, Branches)
  Repos <- reposOfUser()
  activity <- append(activity, Repos)
  org <- orgsVector()
  org1 <- append(org, org)
  org1 <- append(org1, org)
  followers <- getF()
  followers1 <- append(followers, followers)
  followers1 <- append(followers1, followers)
  type = c(rep("Commits", 60), rep("Branches", 60), rep("Repositories", 60))
  Followers <- followers1
  Organization <- org1
  memberActivity <- cbind(activity, Organization, Followers, type)
  write.csv(memberActivity, file="/Users/niamhbelton/Documents/3rd year/Software Engineering/graph2.csv")
  r1 <- rPlot(activity ~ Followers | type, data = memberActivity, title = "Member Productivity Vs. Performance", type = "point", color = "Organization")
  ?rPlot
}
