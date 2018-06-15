getName <- function(){
  f <- paste("i <- eval(substitute(i), parent.frame(home_env + 1))",
            "eval(substitute(names(rows)), parent.frame(home_env + 2))[i]",
            sep = ';')
  parse(text = f)
}

getHome <- function(){
  index <- 1
  while(!identical(parent.frame(n = index), globalenv())){
    index = index + 1
    }
  return(index - 2)
}
