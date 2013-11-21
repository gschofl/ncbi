#' @import methods
#' @importFrom assertthat assert_that is.readable has_extension is.string
#' @importFrom assertthat has_attr "on_failure<-"
NULL


is.empty <- function(x) {
  is.null(x) || length(x) == 0L || (length(x) == 1L && !nzchar(x))
}
on_failure(is.empty) <- function(call, env) {
  paste0(deparse(call$x), " is not empty.")
}

"%||%" <- function(a, b) {
  if (is.empty(a)) force(b) else a
}

"%|na|%" <- function(a, b) {
  if (is.null(a) || all(is.na(a))) force(b) else a
}

## Vectorized default operators
"%|%" <- function(a, b) ifelse(nzchar(a), a, b)

"%|NA|%" <- function(a, b) ifelse(is.na(a), b, a)

"%ni%" <- Negate(`%in%`)

compact <- function(x) {
  x[!vapply(x, is.empty, FALSE, USE.NAMES=FALSE)]
}

compactChar <- function(x) {
  x[vapply(x, nzchar, FALSE, USE.NAMES=FALSE)]
}

compactNA <- function(x) {
  x[!is.na(x)]
}

Call <- function(fn, ...) {
  fn <- match.fun(fn)
  fn(...)
}

Partial <- function(fn, ..., .env = parent.frame()) {
  assert_that(is.function(fn))
  fcall <- substitute(fn(...))
  if (!is.primitive(fn))
    fcall <- match.call(fn, fcall)  
  fcall[[length(fcall) + 1]] <- quote(...)
  args <- list("..." = quote(expr = ))
  eval(call("function", as.pairlist(args), fcall), .env)
}

Compose <- function (...) {
  fns <- lapply(compact(list(...)), match.fun)
  len <- length(fns)
  function (...) {
    res <- Call(fns[[len]], ...)
    for (fn in rev(fns[-len]))
      res <- fn(res)
    res
  }
}

usp <- Compose("unlist", "strsplit")

dup <- function (x, n) {
  assert_that(is.string(x))
  if (any(n < 0)) n[n < 0] <- 0
  vapply(.mapply(rep.int, list(rep.int(x, length(n)), n), NULL), paste0, collapse="", "")
}

blanks <- Partial(dup, x = " ")

trim <- function(x, trim = '\\s+') {
  assert_that(is.vector(x))
  gsub(paste0("^", trim, "|", trim, "$"), '', x)
}

count_re <- function(x, re) {
  vapply(gregexpr(re, x), function (x) sum(x > 0L), 1, USE.NAMES=FALSE)
}

ellipsize <- function(obj, width=getOption("width"), ellipsis="...") {
  str <- encodeString(obj)
  ifelse(nchar(str) > width - 1,
         paste0(substring(str, 1, width - nchar(ellipsis) - 1), ellipsis),
         str)
}

#' Test if an external executable is available
#' 
#' Uses \code{\link{Sys.which}} internally, so it should work
#' on Windows and Unix.alikes.
#' 
#' @param cmd The exececutable to test for.
#' @param msg Additional message if the test fails.
#' @keywords internal
has_command <- function (cmd, msg = "") {
  assert_that(is.string(cmd))
  unname(Sys.which(cmd) != "")
}
on_failure(has_command) <- function(call, env) {
  paste0("Dependency ", sQuote(eval(call$cmd, env)), " is not installed\n",
         eval(call$msg, env))
}

#' Format paragraphs
#' 
#' Similar to \code{\link{strwrap}} but returns a single string with
#' linefeeds inserted
#' 
#' @param s a character vector or a list of character vectors
#' @param width a positive integer giving the column for inserting
#' linefeeds
#' @param indent an integer giving the indentation of the first line of
#' the paragraph; negative values of \code{indent} are allowed and reduce
#' the width for the first line by that value.
#' @param offset a non-negative integer giving the indentation of all
#' but the first line
#' @param split regular expression used for splitting. Defaults to
#' a whitespace character.
#' @param FORCE Words are force split if the available width is too small.
#' @param FULL_FORCE Lines are split exactly at the specified width
#' irrespective of whether there is whitespace or not.
#' 
#' @return a character vector
#' @keywords internal
linebreak <- function(s, width = getOption("width") - 2,
                      indent = 0, offset = 0, split = " ",
                      FORCE = FALSE, FULL_FORCE = FALSE)
{
  if (!is.character(s)) 
    s <- as.character(s)
  
  if (length(s) == 0L)
    return("")
  
  .first_iteration <- TRUE
  # set indent string to "" if a negative value is given
  # this lets us shrink the available width for the first line by that value
  indent_string <- blanks(indent)
  offset_string <- paste0("\n", blanks(offset))
  
  ans <- Map(function(s, width, offset, indent,
                      indent_string, split, FORCE,
                      FULL_FORCE)
  {
    # remove leading and trailing blanks
    # convert newlines, tabs, spaces to " "
    # find first position where 'split' applies
    if (!FULL_FORCE) {
      s <- gsub("\\s+", " ", trim(s), perl=TRUE)
    }
    fws <- regexpr(split, s, perl=TRUE)
    
    if (.first_iteration)
      string_width <- indent + nchar(s)
    else
      string_width <- offset + nchar(s)
    
    if (string_width > width)
    {
      # if not everything fits on one line
      .first_iteration <- FALSE
      if(FULL_FORCE ||
           ((fws == -1 || fws >= (width - string_width)) && FORCE))
      {
        # if no whitespace or first word too long and force break
        # cut through the middle of a word
        pat1 <- paste0("^.{", width - offset - indent, "}(?=.+)")
        pat2 <- paste0("(?<=^.{", width - offset - indent, "}).+")
        leading_string <- regmatches(s, regexpr(pat1, s, perl=TRUE))
        trailing_string <- regmatches(s, regexpr(pat2, s, perl=TRUE)) 
        s <- paste0(indent_string, leading_string, offset_string,
                    linebreak(s=trailing_string, width=width, indent=0,
                              offset=offset, split=split, FORCE=FORCE, 
                              FULL_FORCE=FULL_FORCE))
      }
      else if ((fws == -1 || fws >= (width - offset + indent)) && !FORCE)
      {
        # if no whitespace or first word too long and NO force break
        # stop right here
        stop("Can't break in the middle of a word. Use the force!")
      }
      else
      {
        # break the line
        s_split <- unlist(strsplit(s, split))
        s_cum <- cumsum(nchar(s_split) + 1)
        leading_string <- 
          paste0(s_split[s_cum < width - offset - indent],
                 ifelse(split == " ", "", split), collapse=split)
        trailing_string <- 
          paste0(s_split[s_cum >= width - offset - indent], collapse=split)
        s <- paste0(indent_string, leading_string, offset_string,
                    linebreak(s=trailing_string, width=width, indent=0,
                              offset=offset, split=split, FORCE=FORCE, FULL_FORCE=FULL_FORCE))
      }
    } else {
      # if everything fits on one line go with the string + indent
      paste0(indent_string, s)
    }
  }, s, width, offset, abs(indent), indent_string, split,
             FORCE, FULL_FORCE, USE.NAMES=FALSE)
  unlist(ans)
}

#' Strip file extensions
#'
#' Strips the extension (or an arbitrary tag) from a file name. 
#' 
#' @param file The file name(s).
#' @param sep specifies the seperator character (default ".").
#' @param level How many extensions should be stripped.
#' The default (0) strips all, 1 strips the last one, 2 strips the last two,
#' and so on.
#' @keywords internal
strip_ext <- function (file, sep="\\.", level=0) {
  assert_that(!missing(file), is.character(file))
  if (level == 0L) {
    # level 0 ditches everything that comes after a dot
    vapply(file, function(x) usp(x, sep)[1L], "", USE.NAMES = FALSE)
  } else if (level > 0L) {
    # level 1 removes the very last extension: file.xyz.abc > file.xyz
    # level 2: file.xyz.abc > file
    # and so on
    count <- count_re(file, sep) + 1L - level
    # to always grab at least the first element after the split
    # reset zero counts to 1
    count <- ifelse(count < 1, 1, count)
    unlist(Map(function(x, lvl) {
      paste0(usp(x, sep)[ seq_len(lvl) ], collapse = gsub('\\', '', sep, fixed=TRUE))
    }, x=file, lvl=count, USE.NAMES=FALSE))
  } else {
    stop(sprintf("Level %s is invalid. Must be 0, 1, 2, ...", sQuote(level)))
  }
}


