#!/usr/bin/env Rscript

generate <- function(n) {
    data.frame(
        id = sample(10000000:99999999, n, replace = FALSE),
        code = stringi::stri_rand_strings(n, 32, "[A-Z0-9]"),
        temp = round(runif(n, min = -20, max = 46), 2),
        prec = round(runif(n, min = 0, max = 1), 2)
    )
}

generate_and_write <- function(n, path) {
    write.csv(generate(n), file = path, row.names = FALSE)
}


for (i in 2^seq(0, 20)) {
    generate_and_write(i, sprintf("data/%07d.csv", i))
}
