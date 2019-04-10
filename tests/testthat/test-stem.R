context("test-stem")

library(recipes)
library(textrecipes)

test_data <- tibble(text = c("I would not eat them here or there.",
                             "I would not eat them anywhere.",
                             "I would not eat green eggs and ham.",
                             "I do not like them, Sam-I-am.")
)

# test text in portuguese-BR
test_ptbr_data <- tibble(text = c("coma frutas pois elas fazem bem para a saúde.",
                                "não coma doces, eles fazem mal para os dentes.",
                                "Em morfologia linguística e recuperação de informação a stemização (do inglês, stemming) é
                                 o processo de reduzir palavras flexionadas (ou às vezes derivadas) ao seu tronco (stem), base ou
                                 raiz, geralmente uma forma da palavra escrita. O tronco não precisa ser idêntico à raiz morfológica
                                 da palavra; ele geralmente é suficiente que palavras relacionadas sejam mapeadas para o mesmo
                                 tronco, mesmo se este tronco não for ele próprio uma raiz válida. O estudo de algoritmos para
                                 stemização tem sido realizado em ciência da computação desde a década de 60. Vários motores de
                                 buscas tratam palavras com o mesmo tronco como sinônimos como um tipo de expansão de consulta, em
                                 um processo de combinação.")
)


rec <- recipe(~ ., data = test_data)
rec_ptbr <- recipe(~., data = test_ptbr_data)

test_that("stemming is done correctly", {
  rec <- rec %>%
    step_tokenize(text) %>%
    step_stem(text) 
  
  obj <- rec %>%
    prep(training = test_data, retain = TRUE)
  
  expect_equal(
    tokenizers::tokenize_words(test_data$text[1])[[1]] %>%
      SnowballC::wordStem(),
    juice(obj) %>% 
      slice(1) %>% 
      pull(text) %>%
      unlist()
  )
  
  expect_equal(dim(tidy(rec, 2)), c(1, 3))
  expect_equal(dim(tidy(obj, 2)), c(1, 3))
})

test_that("printing", {
  rec <- rec %>%
    step_tokenize(text) %>%
    step_stem(text)
  expect_output(print(rec))
  expect_output(prep(rec, training = test_data, verbose = TRUE))
})

test_that("stemming with rslp method is done correctly", {
  rec_ptbr <- rec_ptbr %>%
    step_tokenize(text) %>%
    step_stem(text, stemmer = "rslp") 
  
  obj_ptbr <- rec_ptbr %>%
    prep(training = test_ptbr_data, retain = TRUE)
  
  expect_equal(
    tokenizers::tokenize_words(test_ptbr_data$text[1])[[1]] %>%
      rslp::rslp(),
    juice(obj_ptbr) %>% 
      slice(1) %>% 
      pull(text) %>%
      unlist()
  )
  
  expect_equal(dim(tidy(rec_ptbr, 2)), c(1, 3))
  expect_equal(dim(tidy(obj_ptbr, 2)), c(1, 3))
})