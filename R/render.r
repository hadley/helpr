# Render methods to eventually be moved into sinartra

render_json <- function(object) {
  json <- rjson::toJSON(object)
  list(payload = json)
}
