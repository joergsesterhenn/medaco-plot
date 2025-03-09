#' @import here
here::i_am("R/global.R")

i18n <- shiny.i18n::Translator$new(
  translation_json_path = here::here("inst", "translations.json")
)
i18n$set_translation_language("de")

clean_fun <- function(html_string) {
  gsub("<.*?>", "", html_string)
}
