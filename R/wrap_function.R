# Wrap an expression in a function
#
# This function takes the "body" part of a function and wraps it in
# a function. The return value depends on whether the function is created
# for its side effect with the tee operator. If the operator is \code{\%$\%}
# then the expression will be evaluated in a \code{with(., )} statement.
#
# @param body an expression which will serve as function body in single-argument
#    function with an argument names \code{.} (a dot)
# @param pipe a quoted magrittr pipe, which determines how the function is made.
# @param env The environment in which to contruct the function.

# @details Currently, the only distinction made is whether the pipe is a tee
#   or not.
#
# @return a function of a single argument, named \code{.}.
wrap_function <- function(body, pipe, is_monad, unit, env)
{

  if (is_tee(pipe)) {
    body <- call("{", body, quote(.))
  } else if (is_dollar(pipe)) {
    body <- substitute(with(., b), list(b = body))
  }


  # If we deal with a monad, call bind() generic
  if (is_monad) {
    fun <- body[[1]]
    args <- as.list(body[-c(1:2)])

    # Here we call unit() on the input to lift it to a monadic value if
    # needed. Not sure this is a good idea, this is not how it's done in
    # Haskell
    input <-
      if (!is.null(unit)) {
        ## body[[2]] <- as.call(as.expression(list(unit, body[[2]])))
        as.call(as.expression(list(unit, quote(.))))
      } else {
        quote(.)
      }
    body <- as.call(as.expression(c(list(bind, input, fun), args)))
  }

  eval(call("function", as.pairlist(alist(.=)), body), env, env)
}
