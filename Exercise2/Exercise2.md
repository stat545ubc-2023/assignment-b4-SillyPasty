# Introduction

-   In this exercise, I design my alex pig latin rules and implemented
    them in a R function.
-   Also add the document and tests for the function.
-   Rules
    -   1.  If the first letter is not vowel, move first letter to last
            letter

    -   1.  If the first letter is vowel, add a l\*l at the front of the
            word, \* is the first vowel letter

    -   1.  Replace all ‘a’ to ‘u’

    -   1.  Replace all ‘u’ to ‘a’

    -   1.  Add ‘oy’ to the end of the new word.

# Include

    library("testthat")
    library("dplyr")

    ## 
    ## Attaching package: 'dplyr'

    ## The following object is masked from 'package:testthat':
    ## 
    ##     matches

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    library("palmerpenguins")
    library("usethis")
    library("devtools")

    ## 
    ## Attaching package: 'devtools'

    ## The following object is masked from 'package:testthat':
    ## 
    ##     test_file

# My function

    #' @title alex_pig_latin
    #' @details
    #' reveice a word and transfer it into Alex's self modified Pig Latin!
    #' The rules:
    #' 1. If the first letter is not vowel, move first letter to last letter
    #' 2. If the first letter is vowel, add a l*l at the front of the word, \* is the first vowel letter
    #' 3. Replace all 'a' to 'u'
    #' 4. Replace all 'u' to 'a'
    #' 5. Add 'oy' to the end of the new word.
    #' @param data Input data, a word, in string format
    #'
    #' @return A new word performed the Alex's Pig Latin transformation!
    #' @md
    #' @export
    #'
    #' @examples
    #' alex_pig_latin("happy") -> uppyhoy
    #' alex_pig_latin("apple") -> luluppleoy
    #' alex_pig_latin("humor") -> amorhoy
    alex_pig_latin <- function(word) {
      if (!is.character(word) || !grepl("^[A-Za-z]+$", word)) {
        stop("Input must be a word string without number and symbols.")
      }
      
      if (!grepl("^[aeiouAEIOU]", word)) {
        word1 <- substring(word, 2, nchar(word))
        word <- paste0(word1, substr(word, 1, 1))
      } else {
        first_vowel <- substr(word, 1, 1)
        prefix <- paste0("l", first_vowel, "l")
        word <- paste0(prefix, word)
      }
      temp_char <- "\001"
      word <- gsub("a", temp_char, word)
      word <- gsub("u", "a", word)
      word <- gsub(temp_char, "u", word)
      word <- paste0(word, "oy")
      return(word)
    }

# Tests

-   Test the input validation and functions

<!-- -->

    test_that("alex pig latin correctly check input word", {
      expect_error(alex_pig_latin(1), "Input must be a word string without number and symbols.")
      expect_error(alex_pig_latin(0.567), "Input must be a word string without number and symbols.")
      expect_error(alex_pig_latin("Hello!"), "Input must be a word string without number and symbols.")
      expect_error(alex_pig_latin("IronMan001"), "Input must be a word string without number and symbols.")
    })

    ## Test passed 🎉

    test_that("alex pig latin correctly move first letter", {
      rword <- alex_pig_latin("letter")
      expect_equal(rword, "etterloy")
      rword <- alex_pig_latin("eclipse")
      expect_equal(rword, "leleclipseoy")
    })

    ## Test passed 😸

    test_that("alex pig latin correctly add letter for vowel", {
      rword <- alex_pig_latin("eclipse")
      expect_equal(rword, "leleclipseoy")
      rword <- alex_pig_latin("intrest")
      expect_equal(rword, "lilintrestoy")
    })

    ## Test passed 😸

    test_that("alex pig latin correctly replace characters", {
      rword <- alex_pig_latin("apple")
      expect_equal(rword, "luluppleoy")
      rword <- alex_pig_latin("maple")
      expect_equal(rword, "uplemoy")
      rword <- alex_pig_latin("unacceptable")
      expect_equal(rword, "lalanucceptubleoy")
    })

    ## Test passed 🎉

    test_that("alex pig latin correctly add following 'oy'", {
      rword <- alex_pig_latin("apple")
      expect_equal(rword, "luluppleoy")
      rword <- alex_pig_latin("maple")
      expect_equal(rword, "uplemoy")
      rword <- alex_pig_latin("unacceptable")
      expect_equal(rword, "lalanucceptubleoy")
    })

    ## Test passed 😸
