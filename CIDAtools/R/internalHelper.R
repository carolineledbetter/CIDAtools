getHome <- function(){
  f <- paste("index <- 1",
             "while(!identical(parent.frame(n = index), globalenv())){",
             "  index = index + 1",
             "}",
             "index - 1",
             sep = ';')
  parse(text = f)
}

getName <- function(){
  f <- paste("i <- eval(substitute(i), parent.frame(home_env - 1))",
            "eval(substitute(names(rows)), parent.frame(home_env))[i]",
            sep = ';')
  parse(text = f)
}
