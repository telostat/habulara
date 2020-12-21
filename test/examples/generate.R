generate <- function (n) {
  data.frame(
    id=stringi::stri_rand_strings(n, 10, '[0-Z]'),
    name=stringi::stri_rand_strings(n, 32, '[0-Z]'),
    temperature=runif(n, min=0, max=46),
    precipitation=runif(n, min=0, max=1)
  )
}


generate_and_write <- function (n, path) {
  write.csv(generate(n), file=path, row.names=FALSE)
}


for (i in 10^seq(0, 6)) {
  generate_and_write(i, sprintf("simple_%07d.csv", i))
}
