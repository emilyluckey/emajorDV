# .cred <- jsonlite::fromJSON("emajor-calendar-service.json")
#
#
# httr::oauth2.0_token (
#   endpoint=httr::oauth_endpoints("google"),
#   app=httr::oauth_app(
#     "emajor calendar service",
#     key="633036986036-bskr5ispb6t9lnriuvl5j9fl4cfi01cb.apps.googleusercontent.com",
#     secret = "CLpqR9LxD5FcXAK9Y98QeSMa"
#   ),
#   scope="https://www.googleapis.com/auth/calendar.events"
# ) -> google.token

# library(gargle)
# token <- gargle::token_fetch()
#
# req <- request_build(
#   method = "GET",
#   path = "calendar/v3/users/me/calendarList",
#   token = token,
#   base_url = "https://www.googleapis.com"
# )
# resp <- request_make(req)
# out <- response_process(resp)

url <- "https://calendar.google.com/event?action=TEMPLATE&tmeid=NjQ4dGxkN2phZnNjMmM1cThzdWdyZnI1NXMgZS5tYWpvci50YWl3YW5AbQ&tmsrc=e.major.taiwan%40gmail.com"

httr::GET(url) -> eventResponse
httr::content(eventResponse) -> eventContent

library(rvest)
xml2::read_html(url) -> eventContent
rvest::html_node(eventContent,"#xTiIn")
