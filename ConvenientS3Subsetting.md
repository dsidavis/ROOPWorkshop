# Example of Convenient S3 Method

I am working on a project that works on 4000 inividual files.
Each of these is an observation in some sense.
All the files are in a single directory
and the names are of the form
UCD_Lehmann_1234.jpg
The bit that changes is 1234. We always have a 4 digit number.


As each file is an observation, we often apply the same function to each file
and get a list as a result. We put the names of the file on the list so we can easily
identify them.
So these operations are something like
```
files = list.files(dir, pattern = "jpg", full = TRUE)
bbox = lapply(files, GetBoxes)
names(bbox) = files
```

To look at an individual element, I would use
```
bbox[[ "UCD_Lehmann_1234.jpg" ]]
```
It is inconvenient to type the UCD_Lehmann prefix and the .jpg suffix. 
Instead, we could us a function, say f(), that creates the string from just the number,
e.g.
```
bbox[[ f("1234") ]]
```

We could go one step further and  allow
```
bbox[[ "1234" ]]
```
Note that we don't want to have 
`bbox[[1234]]` as we want that to work as expected
and get  the corresponding element by position, not file name.


One way to go about allowing `bbox[["1234"]]`
is to
1. put a class on our list (`bbox`)
1. provide a method for the [[ operator for that class.


We do this first of these with
```
class(bbox) = "Results"
```


We do the second with
```
`[[.Results` =
function(x, i, ...)
{
    if(is.character(i) && grepl("^[0-9]+$", i))
        i = sprintf("UCD_Lehmann_%s.jpg", i)

    NextMethod()
}
```
This checks that i is indeed a character vector (should be of length 1)
and that it is just numbers.
If this is the case, it adds the prefix and suffix and assigns this back to the variable
i. 
We are then ready to invoke the regular, inherited method for the [[ operator.


We should check the string i has 4 digits and if not, prepend 0s to it,
i.e., convert 123 to 0123 and 23 to 0023. 


Rather than putting the computations directly into the [[ method,
I create a separate function, say, mapToFilename() and that
does the work. Then I can use this in other methods as well as the [[ method.


# An Extension

In another project, we are also dealing with many files.
In this case, they are organized in many subdirectories.
We want to be able to specify part of the name of the file, and not the subdirectory.

Again, we can provide a class for the list and a method for the [[ operator.
We assume that the names() vector of the list is the full name of
the directory and file for each file.
So our method
```
`[[.FResults` =
function(x, i, ...)
{
  if(is.character(i)) {
     if(!(i %in% names(x)))
	   j = grep(i, basename(names(x)), fixed = TRUE)
	   if(length(j) == 0)
	     stop("cannot find an element identified by ", i)
	   if(length(j) > 1)
	     warning(i, " matches ", length(j), " elements. Returning the first.")
	   i = names(x)[j[1]]
  }
  NextMethod()
}
```



