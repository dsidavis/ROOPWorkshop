setMethod("[", c("Pix", "missing", "missing"),
          function(x, i, j, ...)  {
              pixGetPixels(x)
          })

setMethod("[", c("Pix", "numeric", "missing"),
          function(x, i, j, ...) {
              pixNumericSubset(x, i, seq(1, ncol(x)), ...)              
          })

    
setMethod("[", c("Pix", "missing", "numeric"),
          function(x, i, j, ...) {
              pixNumericSubset(x, seq(1, nrow(x)), j, ...)
          })


setMethod("[", c("Pix", "numeric", "numeric"), pixNumericSubset)

setMethod("[", c("Pix", "logical", "logical"),
          function(x, i, j, ...) {
              ix = which(i)
              if(length(ix) == 0)
                  return(matrix(0, nrow(x), 0))
              
              jx = which(j)
              if(length(jx) == 0)
                  return(matrix(0, 0, ncol(x)))
              
              pixNumericSubset(x, ix, jx, ...)
        })
setMethod("[", c("Pix", "logical", "missing"),
          function(x, i, j, ...) {
              ix = which(i)
              if(length(ix) == 0)
                  return(matrix(0, nrow(x), 0))
              
              pixNumericSubset(x, ix, integer(), ...)              
        })

setMethod("[", c("Pix", "missing", "logical"),
          function(x, i, j, ...) {
              ij = which(j)
              if(length(ij) == 0)
                  return(matrix(0, 0, ncol(x)))
              
              pixNumericSubset(x, integer(), ij, ...)                            
        })

# Should do the cross over combinations, e.g., numeric, integer; numeric, logical
# no methods for character indexing.

setMethod("[", c("Pix", "matrix"),
          function(x, i, j, ...) {
            pixGetPixels(x)[ i, ... ]
          })
