# The following code can be used to read vocabulary lists from
# the data directory of svn://svn.tartarus.org/snowball/
# Manual fixes are needed to replace empty values by ""
for(lang in getStemLanguages()) {
    voc <- scan(file.path("words", lang, "diffs.txt"),
                list("character", "character"), quote="\"", quiet=TRUE)

    voc <- data.frame(word=voc[[1]], stem=voc[[2]], stringsAsFactors=FALSE)

    stopifnot(all(wordStem(voc[[1]], lang) == voc[[2]]))
}

# Save individual files in the most compressed form
for(lang in getStemLanguages()) {
    voc <- scan(file.path("words", lang, "diffs.txt"),
                list("character", "character"), quote="\"", quiet=TRUE)

    voc <- data.frame(word=voc[[1]], stem=voc[[2]], stringsAsFactors=FALSE)

    save(voc, file=file.path("words", paste0(lang, ".RData")), compress="xz")
}

# Only keep a subsample of words to reduce space needed for CRAN releases
for(lang in getStemLanguages()) {
    load(file.path("words", paste0(lang, ".RData")))

    voc <- voc[seq(1, nrow(voc), by=80),]

    save(voc, file=file.path("words", paste0(lang, ".RData")), compress="xz")
}

