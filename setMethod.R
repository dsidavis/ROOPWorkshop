setGeneric("GetBoxes",
           function(obj, level = 3L, keepConfidence = TRUE, asMatrix = FALSE, ...)
             standardGeneric("GetBoxes"))

setMethod("GetBoxes",
          "TesseractBaseAPI",
          function(obj, level = 3L, keepConfidence = TRUE, asMatrix = FALSE, ...) {
              if(!hasRecognized(obj))
                 Recognize(obj)
              
              ans = .Call("R_TesseractBaseAPI_getBoundingBoxes", obj, as(level, "PageIteratorLevel"))
              m = do.call(rbind, ans)
              colnames(m) = c("confidence", "left", "bottom", "right", "top") #XXXX
              if(asMatrix) {
                  rownames(m) = names(ans)
                  cols = 2:5
              } else {
                  m = as.data.frame(m)
#                  class(m) = c("OCRPositionResults", class(m))
                  m$text = names(ans)
                  rownames(m) = NULL
                  cols = 2:6
              }

              m[, c(cols, if(keepConfidence) 1)]  # still numeric! Change to integer.  Or leave the confidence in.
          })

setMethod("GetBoxes",
           "character",
          function(obj, level = 3L, keepConfidence = TRUE, asMatrix = FALSE, ...) {
              ts = tesseract(obj, ...)
              Recognize(ts) # Want to avoid doing this twice if possible
              GetBoxes(ts, level, keepConfidence, asMatrix)
          })


setMethod("GetBoxes",
          "Pix",
          function(obj, level = 3L, keepConfidence = TRUE, asMatrix = FALSE, ...) {
              api = tesseract(...)
              SetImage(api, obj)
              GetBoxes(api, level, keepConfidence, asMatrix)
          })

