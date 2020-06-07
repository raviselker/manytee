
context('manyttestIS')

test_that('manyttestIS works', {
    set.seed(1337)
    N <- 100
    
    df <- data.frame(
        y = rnorm(N),
        x1 = sample(LETTERS[1:2], size = N, replace = TRUE),
        x2 = sample(letters[1:3], size = N, replace = TRUE)
    )
    
    result <- manyttestIS(df, vars(y), vars(x1, x2))
    
    table <- result$tests$asDF
    
    group1 <- df[df$x1 == 'A' & df$x2 == 'a',]$y
    group2 <- df[df$x1 == 'B' & df$x2 == 'b',]$y
    
    t <- t.test(group1, group2, var.equal = TRUE)
    
    row <- table[table$x11=='A' & table$x21=='a' & table$x12=='B' & table$x22=='b',]
    
    expect_equivalent(row$t, t$statistic, tolerance = 1e-3)
    expect_equivalent(row$df, t$parameter, tolerance = 1e-3)
    expect_equivalent(row$p, t$p.value, tolerance = 1e-3)

})
