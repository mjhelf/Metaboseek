library(testthat)


##some simple data frames
df1 <- data.frame(mz = c(101:105),
                  rt = c(201:205),
                  file1__XIC = c(1000,1000,1000,1000,1000),
                  file2__XIC = c(0,1000, 1000,2000,3000),
                  file3__XIC = c(1000,0,1500,500,0),
                  file4__XIC = c(1000,0,500,100,1000)
                  )

df2 <- data.frame(mz = c(301:305),
                  results = c(1:5)
)



test_check("Mosaic")


