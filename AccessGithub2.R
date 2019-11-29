if(!require("httpuv"))install.packages("httpuv")
if(!require("plotly"))install.packages("plotly")
if(!require("httr"))install.packages("httr")
if(!require("ggplot2"))install.packages("ggplot2")
if(!require("devtools"))install.packages("devtools")
if(!require("jsonlite"))install.packages("jsonlite")

oauth_endpoints("github")

application <- oauth_app(appname = "Software_Engineering_API",
                   key = "8120827a9ad62249f04c",
                   secret = "d66df236b58867925fa3c669b418011c1d7841cd")

github_token <- oauth2.0_token(oauth_endpoints("github"), application)

githubToken <- config(token = github_token)
req <- GET("https://api.github.com/users/dicof/repos", githubToken)

stop_for_status(req)

json1 = content(req)

gitDF = jsonlite::fromJSON(jsonlite::toJSON(json1))

gitDF[gitDF$full_name == "dicof/datasharing", "created_at"] 



MyGithubData = fromJSON("https://api.github.com/users/dicof")

MyGithubData$followers

followers = fromJSON("https://api.github.com/users/dicof/followers")
followers$login
MyGithubData$following

following = fromJSON("https://api.github.com/users/dicof/following")
following$login

MyGithubData$public_repos

repos = fromJSON("https://api.github.com/users/dicof/repos")
repos$name 
repos$created_at 
repos$full_name

#Used account of Andrew Nesbitt to produce plots, 2nd most popular developer on Github.
#Username is andrew.


andrewGithubData = GET("https://api.github.com/users/andrew/followers?per_page=100;", githubToken)
stop_for_status(andrewGithubData)
extract = content(andrewGithubData)

githubDB = jsonlite::fromJSON(jsonlite::toJSON(extract))
githubDB$login


id = githubDB$login
user_ids = c(id)


users = c()
usersDB = data.frame(
  username = integer(),
  following = integer(),
  followers = integer(),
  repos = integer(),
  dateCreated = integer()
)


for(i in 1:length(user_ids))
{
  
  followingURL = paste("https://api.github.com/users/", user_ids[i], "/following", sep = "")
  followingRequest = GET(followingURL, githubToken)
  followingContent = content(followingRequest)
  
  if(length(followingContent) == 0)
  {
    next
  }
  
  followingDF = jsonlite::fromJSON(jsonlite::toJSON(followingContent))
  followingLogin = followingDF$login
  

  for (j in 1:length(followingLogin))
  {
  
    if (is.element(followingLogin[j], users) == FALSE)
    {
      
      users[length(users) + 1] = followingLogin[j]
      

      followingUrl2 = paste("https://api.github.com/users/", followingLogin[j], sep = "")
      following2 = GET(followingUrl2, githubToken)
      followingContent2 = content(following2)
      followingDF2 = jsonlite::fromJSON(jsonlite::toJSON(followingContent2))
      
      
      followingNumber = followingDF2$following
      
      
      followersNumber = followingDF2$followers
      
       
      reposNumber = followingDF2$public_repos
      
      
      yearCreated = substr(followingDF2$created_at, start = 1, stop = 4)
      
      
      usersDB[nrow(usersDB) + 1, ] = c(followingLogin[j], followingNumber, followersNumber, reposNumber, yearCreated)
      
    }
    next
  }
  
  if(length(users) > 150)
  {
    break
  }
  next
}


Sys.setenv("plotly_username"="dicof")
Sys.setenv("plotly_api_key"="ZQ7tg7BBT6AiGI301qgf")


plot1 = plot_ly(data = usersDB, x = ~repos, y = ~followers, text = ~paste("Followers: ", followers, "<br>Repositories: ", repos, "<br>Date Created:", dateCreated), color = ~dateCreated)
plot1

api_create(plot1, filename = "Repositories vs Followers")
#https://plot.ly/~dicof/0/#/


plot2 = plot_ly(data = usersDB, x = ~following, y = ~followers, text = ~paste("Followers: ", followers, "<br>Following: ", following), color = ~dateCreated)
plot2

api_create(plot2, filename = "Following vs Followers")
#https://plot.ly/~dicof/1/#/


languages = c()

for (i in 1:length(users))
{
  RepositoriesUrl = paste("https://api.github.com/users/", users[i], "/repos", sep = "")
  Repositories = GET(RepositoriesUrl, githubToken)
  RepositoriesContent = content(Repositories)
  RepositoriesDF = jsonlite::fromJSON(jsonlite::toJSON(RepositoriesContent))
  RepositoriesNames = RepositoriesDF$name
  
 
  for (j in 1: length(RepositoriesNames))
  {
   
    RepositoriesUrl2 = paste("https://api.github.com/repos/", users[i], "/", RepositoriesNames[j], sep = "")
    Repositories2 = GET(RepositoriesUrl2, githubToken)
    RepositoriesContent2 = content(Repositories2)
    RepositoriesDF2 = jsonlite::fromJSON(jsonlite::toJSON(RepositoriesContent2))
    language = RepositoriesDF2$language
    
   
    if (length(language) != 0 && language != "<NA>")
    {
      languages[length(languages)+1] = language
    }
    next
  }
  next
}


allLanguages = sort(table(languages), increasing=TRUE)
top10Languages = allLanguages[(length(allLanguages)-9):length(allLanguages)]


languageDF = as.data.frame(top10Languages)


plot3 = plot_ly(data = languageDF, x = languageDF$languages, y = languageDF$Freq, type = "bar")
plot3

Sys.setenv("plotly_username"="dicof")
Sys.setenv("plotly_api_key"="ZQ7tg7BBT6AiGI301qgf")

api_create(plot3, filename = "10 Most Popular Languages")
https://plot.ly/~dicof/2/#/
