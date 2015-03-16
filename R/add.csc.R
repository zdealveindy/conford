#' Adds circle of significant contribution onto ordination diagram
#' @examples
#' library (weimea)
#' library (vegan)
#' data (vltava)
#' env <- vltava$env$pH
#' #add randomly generated variables
#'  set.seed (1234)
#'  nmds <- metaMDS (vltava$spe)
#' env <- matrix (rnorm (8*97), ncol = 8, dimnames = list (NULL, names (vltava$env)[c(1:6, 11:12)]))
#' ordiplot (nmds, display = 'si')
#' for (gr in 1:4) ordiellipse (nmds, groups = vltava$group$GROUP, show.groups = gr, col = gr)
#' ef <- envfit (nmds, env)
#' add.csc.metaMDS (ord = nmds, x = ef, fg = 'grey', lwd = 2, ci = 0.95)
#' plot (ef)
#' 
#' @export
add.csc.metaMDS <- function (ord, x, arrow.mul, at = c(0,0), add = T, choices = c(1,2), ci = 0.95, ...)
{
  require (vegan)
  m <- 2 # number of explanatory variables (ordination axes)
  n <- ord$nobj# number of samples in analysis
  df1 <- m
  df2 <- n-m-1
  F.exp <- qf (ci, df1, df2)
  k <- F.exp * (df1/df2)
  r2.exp <-  (k/(1+k))
  
  if (!is.null(x$vectors)) 
  {
    vect <- sqrt(x$vectors$r) * x$vectors$arrows[, choices, drop = FALSE]
    if (missing(arrow.mul)) 
    {
      if (!add)
        arrow.mul <- 1 else
        arrow.mul <- vegan:::ordiArrowMul(vect, at = at)
    }
  }
 radius <- sqrt (r2.exp) * arrow.mul
 symbols (x = at[1], y = at[2], circles = radius, add = T, inches = F, ...)
 return (list (r2.exp = r2.exp, radius = radius, arrow.mul = arrow.mul))
}

