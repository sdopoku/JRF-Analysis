#Ensure Package Script
# Ensures that speficied package is loaded. 
# Downloads package and loads into memory if not in library

EnsurePackage <- function(x)
{
  x <- as.character(x)
  
  if (!require(x, character.only= TRUE))
    {
    install.packages(pkgs=x)
    require(x,character.only=TRUE)
    
  }
}

