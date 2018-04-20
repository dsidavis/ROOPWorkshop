# Initial implementation of plot method for OCRResults
plot.OCRResults =
function(x, y, cex = 1, ...)
{
    plot(0, type = "n", xlim = range(c(x$left, x$right)) * c(.95, 1.05),
           ylim = range(c(x$top, x$bottom))*c(.95, 1.05), xlab = "", ylab = "")

    h = max(x$top)
    text(x$left, h - x$bottom, x$text, adj = c(0, 0), cex = cex)
}

## Version 3 to handle spell checked results.
# Bad approach

plot.OCRResults =
function(x, y, cex = 1, col = "black", ...)
{
    plot(0, type = "n", xlim = range(c(x$left, x$right)) * c(.95, 1.05),
           ylim = range(c(x$top, x$bottom))*c(.95, 1.05), xlab = "", ylab = "")

    h = max(x$top)
    if(is(x, "SpellCheckedOCRResults"))
        col = c("red", "black")[x$SpelledCorrectly + 1L]

    # OR check for column/element named SpelledCorrectly.
    if("SpelledCorrectly" %in% names(x))
       col = c("red", "black")[x$SpelledCorrectly + 1L]
    
    text(x$left, h - x$bottom, x$text, adj = c(0, 0), cex = cex, col = col)
}

# Version for new class SpellCheckedOCRResults
# Bad: Copied the entire function
# If we make any improvents to plot.OCRResults, need to make changes to this also.
# The only difference is the col
plot.SpellCheckedOCRResults =
function(x, y, cex = 1, col = c("red", "black")[x$SpelledCorrectly + 1L], ...)
{
    plot(0, type = "n", xlim = range(c(x$left, x$right)) * c(.95, 1.05),
           ylim = range(c(x$top, x$bottom))*c(.95, 1.05), xlab = "", ylab = "")

    h = max(x$top)
    text(x$left, h - x$bottom, x$text, adj = c(0, 0), cex = cex, col = col)
}


#####################

# Adaptation of version 1.
# Add the col parameter and use it in text().
# We add this so that plot.SpellCheckedOCRResults can override the colors.

plot.OCRResults =
function(x, y, cex = 1, col = "black", ...)
{
    plot(0, type = "n", xlim = range(c(x$left, x$right)) * c(.95, 1.05),
           ylim = range(c(x$top, x$bottom))*c(.95, 1.05), xlab = "", ylab = "")

    h = max(x$top)
    text(x$left, h - x$bottom, x$text, adj = c(0, 0), cex = cex, col = col)
}

# Now define version for SpellCheckedOCRResults that calls the method for
# OCRResults but specifies different values for the col  parameter.
# We could do this in an ugly way:
#  change the class of x to remove the SpellCheckedOCRResults
#  and then call plot() again
plot.SpellCheckedOCRResults =
function(x, y, cex = 1, col = c("red", "black")[x$SpelledCorrectly + 1L], ...)
{
    class(x) = class(x)[-1]
    plot(x, cex = cex, col = col)
}


# A better way to do this is to call NextMethod() for the inherited method.
# This behaves like the above, i.e. finding the method ignoring this one
# but smarter.
# We need to pass the cex and col from this call, not the defaults for the inherited method.
plot.SpellCheckedOCRResults =
function(x, y, cex = 1, col = c("red", "black")[x$SpelledCorrectly + 1L], ...)
{
   NextMethod("plot", cex = cex, col = col)
}



##################
# Do the spell checking.

# We could do the spell checking in the plot() method with a parameter indicating
# whether to do the spell checking.  But then this is repeated unnecessarily each time.
# Alternatively, we could have the caller be responsible for specifying the colors explicitly
#
if(FALSE) {
plot.OCRResults =
function(x, y, spell = TRUE, cex = 1, col = "black",  ...)
{
    if(spell) {
        ok = Aspell::aspell(x$text) # run this each time.
        col = c("red", "black")[ok + 1L]
        #XX Ignores any user specified value of col
    }
    
    plot(0, type = "n", xlim = range(c(x$left, x$right)) * c(.95, 1.05),
           ylim = range(c(x$top, x$bottom))*c(.95, 1.05), xlab = "", ylab = "")

    h = max(x$top)
    text(x$left, h - x$bottom, x$text, adj = c(0, 0), cex = cex, col = col)
}


# Or
plot(bb, col = c("red", "black")[Aspell::aspell(bb$text) + 1L]

}

# Or we could define a getColors()
if(FALSE) {
plot.OCRResults =
function(x, y, spell = TRUE, cex = 1, col = getColors(x),  ...)
{
    if(spell) {
        ok = Aspell::aspell(x$text) # run this each time.
        col = c("red", "black")[ok + 1L]
        #XX Ignores any user specified value of col
    }
    
    plot(0, type = "n", xlim = range(c(x$left, x$right)) * c(.95, 1.05),
           ylim = range(c(x$top, x$bottom))*c(.95, 1.05), xlab = "", ylab = "")

    h = max(x$top)
    text(x$left, h - x$bottom, x$text, adj = c(0, 0), cex = cex, col = col)
}

getColors.default =
function(x, ...)
        "black"

getColors.SpellCheckedOCRResults =
function(x, ...)
{
    ok = Aspell::aspell(x$text)
    col = c("red", "black")[ok + 1L]
}

}    



# There is a function named aspell in the utils package and one in the Aspell package.
# But we'll define a generic one here.
aspell =
function(x, ...)
  UseMethod("aspell")



aspell.OCRResults =
function(x, ...)    
{
    x$SpelledCorrectly = Aspell::aspell(x$text)
      # Make certain to keep x's original class so that this is still a data.frame.
    class(x) = c("SpellCheckedOCRResults", class(x))  # Should this be c("SpellChecked", "OCRResults")
    x
}


aspell.OCRResults =
function(x, punct = TRUE, ...)    
{
    #XXX This version changes x itself and returns x. So we lose the punctuation if punct is true.
    # See below.
    if(punct)
       x$text = gsub("[[:punct:]]+$", "", x$text)
    x$SpelledCorrectly = Aspell::aspell(x$text)
      # Make certain to keep x's original class so that this is still a data.frame.
    class(x) = c("SpellCheckedOCRResults", class(x))  # Should this be c("SpellChecked", "OCRResults")
    x
}


aspell.OCRResults =
function(x, punct = TRUE, ...)    
{
    text = x$text
    if(punct)
       text = gsub("[[:punct:]]+$", "", text)
    x$SpelledCorrectly = Aspell::aspell(text)
      # Make certain to keep x's original class so that this is still a data.frame.
    class(x) = c("SpellCheckedOCRResults", class(x))  # Should this be c("SpellChecked", "OCRResults")
    x
}
