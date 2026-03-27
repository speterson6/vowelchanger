#####################################
#R function to change vowels in a word
#Created by Skyler Peterson
#2/26/2026
#####################################
library(stringr)
#' Title
#'
#' @param character_string a character string that will be changed
#' @param type_change      options are "remove" or "replace" to remove all vowels or replace all vowels
#' @param capitalization   options are TRUE/FALSE. False keeps as is, true capitalizes the result
#'
#' @return                 A string that came in will be changed slightly and printed
#'
#' @examples vowel_changer(character_string = "The quick red fox jumped over the lazy brown dog", type_change = "remove", capitalization = FALSE)
#'
vowel_changer <-
  function(character_string,
           type_change = "remove",
           capitalization = FALSE) {

    if (class(character_string) != "character") {
      stop("Input character_string needs to be a character")
    }
    if (type_change == "remove") {
      changed_string <- str_replace_all(character_string, "[aeiou]", "")

    } else if (type_change == "replace") {
      changed_string <- str_replace_all(
        character_string,
        c(
          "a" = "4",
          "e" = "3",
          "i" = "1",
          "o" = "0",
          "u" = "6",
          "A" = "4",
          "E" = "3",
          "I" = "1",
          "O" = "0",
          "U" = "6"
        )
      )

    } else{
      stop("Input type_change needs to be 'remove' or 'replace'")
    }

    if (capitalization == TRUE) {
      changed_string <- toupper(changed_string)
    } else if (capitalization != FALSE) {
      stop("Input capitalization needs to be 'TRUE' or 'FALSE'")
    }

    return(changed_string)

  }
