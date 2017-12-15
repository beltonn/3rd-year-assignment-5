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
library("fmsb")
library(devtools)
devtools::install_github('ramnathv/rCharts', auth_token = "2586ed928db8d8c37bb24ac966013602fc01cbfc")
library(rCharts)

oauth_endpoints("github")

myapp <- oauth_app(appname = "VisualisationAssignment",
                   key = "2fd518b1da61c4a20bff",
                   secret = "14020e2776f7b12ba465c21c8ed053e065a6c4a5")

github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

gtoken <- httr::config(token = github_token)

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



#check if one user followers another user
user1FollowUser2 <- function(username, targetUser)
{
  user1FollowsUser2 <- GET(paste0("https://api.github.com/users/",username, "/following/",targetUser),gtoken)
  if(user1FollowsUser2$status == 404){
    return(FALSE);
  }
  return(TRUE);
}


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

list10ReposOfOrg <- function(organization)
{
  repos <- GET(paste0("https://api.github.com/orgs/", organization, "/repos?per_page=10"),gtoken)
  json1 = content(repos)
  json1
  githubDF = jsonlite::fromJSON(jsonlite::toJSON(json1))
  listOfRepos <- githubDF$name
  listOfRepos
  return(listOfRepos);
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
Commits <- function() #returns the number of Commits on each user's first 10 repos
{ 
  members <- first10membersOf10OrgsVector()
  DF <- lapply(members, list10ReposOfUser)
  number <- c()
  for(i in 1:length(members)){
    bb <- totalNumberCommits(members[i], DF[[i]][])
    if(length(bb)==0){
      number[i] = 0
    }
    else{
      number[i] = bb
    }
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
  if(length(githubDF$owner) == 0){
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
#THIS FUNCTION VISUALISES GRAPH 2
visualiseMemberActivity <- function() #member activity vs performance
{
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
  return(r1);
}

location <- function()
{
  mem <- membersAndOrgs()
  orgs <- getOrgsWithMembers()
  members <- first10membersOf10OrgsVector()
  place <- c()
  for(i in 1:length(orgs)){
    location <- GET(paste0("https://api.github.com/orgs/",orgs[i]),gtoken)
    json1 = content(location)
    json1
    githubDF = jsonlite::fromJSON(jsonlite::toJSON(json1))
    lo = githubDF$location
    place <- c(place, lo)
    
    print(place)
  }
  return(place);
}
happinessRates <- function()
{
  rates <- read.table("/Users/niamhbelton/Documents/3rd year/Software Engineering/d3-world happiness.csv", header = TRUE, sep=",")
  place <- location()
  scores <- c()
  countries <- c(rep("United States", 6), "Germany", "United States", "Poland")
  for(i in 1:length(countries)){
    for(j in 1:length(rates[[1]])){
      if(rates[[3]][j] == countries[i]){
        scores[i] = rates[[1]][j] 
      }
    }
  }
  return(scores);
}
#THIS FUNCTION VISUALISES GRAPH 4
VisualiseHappiness <- function()
{
  scores <- happinessRates()
  members <- membersAndOrgs()
  s <- c()
  count <- 0
  for(i in 1:5){
    for(j in 1:length(members[[i]][])){
      count = count + 1
      s[count] = scores[i]
      
    }
  }
  for(i in 7:length(members)){
    for(j in 1:length(members[[i]][])){
      count = count + 1
      s[count] = scores[i-1]
      
    }
  }
  scores1 <- append(s, s)
  scores1 <- append(scores1, s)
  Commits <- Commits()
  activity1 <- c()
  activity1 <- Commits[c(1:30, 35:60)]
  Branches <- Branches()
  activity1 <- append(activity1, Branches[c(1:30, 35:60)])
  Repos <- reposOfUser()
  activity1 <- append(activity1, Repos[c(1:30, 35:60)])
  org <- orgsVector()
  org1 <- append(org[c(1:30, 35:60)], org[c(1:30, 35:60)])
  org1 <- append(org1, org[c(1:30, 35:60)])
  
  type = c(rep("Commits", 56), rep("Branches", 56), rep("Repositories", 56))
  Happiness <- scores1
  happy <- cbind(activity1, org1, Happiness, type)
  write.csv(happy, file="/Users/niamhbelton/Documents/3rd year/Software Engineering/happyproductivity.csv")
  r2 <- rPlot(activity1 ~ Happiness | type, data = happy, type = "point", color = "org1")
  return(r2)
}

choroplethHappinessMap <- function()
{
  install.packages("plotly")
  library("plotly")
  df1 <- rates <- read.table("/Users/niamhbelton/Documents/3rd year/Software Engineering/d3-world happiness.csv", header = TRUE, sep=",")
  
  l <- list(color = toRGB("grey"), width = 0.5)
  g <- list(
    showframe = FALSE,
    showcoastlines = FALSE,
    projection = list(type = 'Mercator')
  )
  
  p <- plot_geo(df1) %>%
    add_trace(
      z = ~z, color = ~z, colors = 'Blues',
      text = ~text, locations = ~locations, marker = list(line = l)
    ) %>%
    colorbar(title = 'World Happiness Rates') %>%
    layout(
      title = 'World Happiness Rates<br>',
      geo = g
    )
  
}

#THIS FUNCTION VISUALISES GRAPH 5
all_variables <- function()
{
  scores <- happinessRates()
  members <- membersAndOrgs()
  s <- c()
  count <- 0
  for(i in 1:5){
    for(j in 1:length(members[[i]][])){
      count = count + 1
      s[count] = scores[i]
      
    }
  }
  for(i in 7:length(members)){
    for(j in 1:length(members[[i]][])){
      count = count + 1
      s[count] = scores[i-1]
      
    }
  }
  scores1 <- append(s, s)
  scores1 <- append(scores1, s)
  Happiness <- scores1
  Branches <- Branches()
  followers <- getF()
  Repos <- reposOfUser()
  f <- followers[c(1:30, 35:60)]
  bran <- Branches[c(1:30, 35:60)]
  r <- Repos[c(1:30, 35:60)]
  df3 <-data.frame(cbind(f, bran, Happiness, r))
  b <- plot_ly(data = df3, x = ~r, y = ~Happiness, z = ~bran,
               marker = list(color = ~f, colorscale = c('#FFE1A1', '#683531'), showscale = TRUE)) %>%
    add_markers() %>%
    layout(scene = list(xaxis = list(title = 'Repositories'),
                        yaxis = list(title = 'Happiness'),
                        zaxis = list(title = 'Branches')),
           annotations = list(
             x = 1.13,
             y = 1.05,
             text = 'Followers',
             xref = 'paper',
             yref = 'paper',
             showarrow = FALSE
           ))
  return(b)
}
#upload to plotly
p <- choroplethHappinessMap()
b <- all_variables()
Sys.setenv("plotly_username"="beltonn")
Sys.setenv("plotly_api_key"="R34kDbvZuTSBIDUaP5qK")
chart_link = api_create(p, filename="worldHappiness")
chart_link
chart_link2 = api_create(b, filename="allvariables3D_plot")
chart_link2



