# Show token output of recipe

Returns the tokens as a list of character vectors of a recipe. This
function can be useful for diagnostics during recipe construction but
should not be used in final recipe steps. Note that this function will
both prep() and bake() the recipe it is used on.

## Usage

``` r
show_tokens(rec, var, n = 6L)
```

## Arguments

- rec:

  A recipe object

- var:

  name of variable

- n:

  Number of elements to return.

## Value

A list of character vectors

## Examples

``` r
text_tibble <- tibble(text = c("This is words", "They are nice!"))

recipe(~text, data = text_tibble) |>
  step_tokenize(text) |>
  show_tokens(text)
#> [[1]]
#> [1] "this"  "is"    "words"
#> 
#> [[2]]
#> [1] "they" "are"  "nice"
#> 

library(modeldata)
#> 
#> Attaching package: ‘modeldata’
#> The following object is masked from ‘package:datasets’:
#> 
#>     penguins
data(tate_text)

recipe(~., data = tate_text) |>
  step_tokenize(medium) |>
  show_tokens(medium)
#> [[1]]
#> [1] "video"      "monitor"    "or"         "projection" "colour"    
#> [6] "and"        "sound"      "stereo"    
#> 
#> [[2]]
#> [1] "etching" "on"      "paper"  
#> 
#> [[3]]
#> [1] "etching" "on"      "paper"  
#> 
#> [[4]]
#> [1] "etching" "on"      "paper"  
#> 
#> [[5]]
#> [1] "oil"    "paint"  "on"     "canvas"
#> 
#> [[6]]
#> [1] "oil"    "paint"  "on"     "canvas"
#> 
```
