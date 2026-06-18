# List of all feature counting functions

List of all feature counting functions

## Usage

``` r
count_functions
```

## Format

Named list of all ferature counting functions

- `n_words`:

  Number of words.

- `n_uq_words`:

  Number of unique words.

- `n_charS`:

  Number of characters. Not counting urls, hashtags, mentions or white
  spaces.

- `n_uq_charS`:

  Number of unique characters. Not counting urls, hashtags, mentions or
  white spaces.

- `n_digits`:

  Number of digits.

- `n_hashtags`:

  Number of hashtags, word preceded by a '#'.

- `n_uq_hashtags`:

  Number of unique hashtags, word preceded by a '#'.

- `n_mentions`:

  Number of mentions, word preceded by a '@'.

- `n_uq_mentions`:

  Number of unique mentions, word preceded by a '@'.

- `n_commas`:

  Number of commas.

- `n_periods`:

  Number of periods.

- `n_exclaims`:

  Number of exclamation points.

- `n_extraspaces`:

  Number of times more then 1 consecutive space have been used.

- `n_caps`:

  Number of upper case characters.

- `n_lowers`:

  Number of lower case characters.

- `n_urls`:

  Number of urls.

- `n_uq_urls`:

  Number of unique urls.

- `n_nonasciis`:

  Number of non ascii characters.

- `n_puncts`:

  Number of punctuations characters, not including exclamation points,
  periods and commas.

- `first_person`:

  Number of "first person" words.

- `first_personp`:

  Number of "first person plural" words.

- `second_person`:

  Number of "second person" words.

- `second_personp`:

  Number of "second person plural" words.

- `third_person`:

  Number of "third person" words.

- `to_be`:

  Number of "to be" words.

- `prepositions`:

  Number of preposition words.

## Details

In this function we refer to "first person", "first person plural" and
so on. This list describes what words are contained in each group.

- first person:

  I, me, myself, my, mine, this.

- first person plural:

  we, us, our, ours, these.

- second person:

  you, yours, your, yourself.

- second person plural:

  he, she, it, its, his, hers.

- third person:

  they, them, theirs, their, they're, their's, those, that.

- to be:

  am, is, are, was, were, being, been, be, were, be.

- prepositions:

  about, below, excepting, off, toward, above, beneath, on, under,
  across, from, onto, underneath, after, between, in, out, until,
  against, beyond, outside, up, along, but, inside, over, upon, among,
  by, past, around, concerning, regarding, with, at, despite, into,
  since, within, down, like, through, without, before, during, near,
  throughout, behind, except, of, to, for.
