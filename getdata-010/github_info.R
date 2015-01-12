# github_info.R
# Codi per obtenir informació de repositoris GitHub usant la seva API.
#   Trobar la data de creació del repositori
#   "https://api.github.com/users/jtleek/repos"

# Càrrega de la llibreria de gestió de connexions http
library(httr)
source("secret_id.R")
# 1. Find OAuth settings for github:
#    http://developer.github.com/v3/oauth/
oauth_endpoints("github")

# 2. Register an application at https://github.com/settings/applications;
#    Use any URL you would like for the homepage URL (http://github.com is fine)
#    and http://localhost:1410 as the callback url
#   És imprescindible d'instal·lar el mòdul httpuv que creara el servidor al port 1410 per fer l'autenticació
#   El mòdul Rcurl també s'ha d'instal·lar per l'autenticació funcioni.
#
#    Insert your client ID and secret below - if secret is omitted, it will
#    look it up in the GITHUB_CONSUMER_SECRET environmental variable.
#    GITHUB_CONSUMER_SECRET <- "d2dfe562aade48f8b5631e8c975846cdca136d59"
myapp <- oauth_app("Coursera Data Science 1", "87d19e88a7f6d1009c66",secret=secret.id)

# 3. Get OAuth credentials
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

# 4. Use API
gtoken <- config(token = github_token)
req <- GET("https://api.github.com/rate_limit", gtoken)
stop_for_status(req)
content(req)

repo.date <- GET("https://api.github.com/users/jtleek/repos", gtoken)
stop_for_status(repo.date)
content(repo.date)
contingut <- content(repo.date)


for ( nom in 1:length(contingut)) 
{ 
    if ( contingut[[nom]]$name == "datasharing")
    {
        print (c (contingut[[nom]]$name, contingut[[nom]]$created_at)) 
    }
}


# OR:
#req <- with_config(gtoken, GET("https://api.github.com/rate_limit"))
#stop_for_status(req)
#content(req)
