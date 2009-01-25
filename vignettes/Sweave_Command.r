sweavePath <- file.path("C:","Documents and Settings","carnellr","My Documents",
                        "R Packages","lhs","vignettes")
                        
Sweave(file.path(sweavePath, "augmentLHS_Example.Stex"),
       syntax=SweaveSyntaxLatex,
       output=file.path(sweavePath, "augmentLHS_Example.tex"))

