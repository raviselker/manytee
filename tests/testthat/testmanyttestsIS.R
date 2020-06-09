
context('manyttestsIS')

test_that('manyttestsIS sunny works', {
    set.seed(1337)
    N <- 100
    
    df <- data.frame(
        y = rnorm(N),
        x1 = sample(LETTERS[1:2], size = N, replace = TRUE),
        x2 = sample(letters[1:3], size = N, replace = TRUE)
    )
    
    
    result <- manyttestsIS(df, vars(y), vars(x1, x2), 
                          corMethod = 'none', effectSize = TRUE,
                          ciES = TRUE)
    
    table <- result$tests$asDF
    
    # Select one test
    row <- table[table$x11=='A' & table$x21=='a' & table$x12=='B' & table$x22=='b',]
    
    # Perform t-test + effect size using R packages for these groups
    group1 <- df[df$x1 == 'A' & df$x2 == 'a',]$y
    group2 <- df[df$x1 == 'B' & df$x2 == 'b',]$y
    
    t <- t.test(group1, group2, var.equal = TRUE)
    es <- effsize::cohen.d(group1, group2)
    
    # Test t-statistics
    expect_equivalent(row$t, t$statistic, tolerance = 1e-3)
    expect_equivalent(row$df, t$parameter, tolerance = 1e-3)
    expect_equivalent(row$p, t$p.value, tolerance = 1e-3)
    
    # Test Effect Size + CI
    expect_equivalent(row$es, es$estimate, tolerance = 1e-3)
    expect_equivalent(row$ciles, es$conf.int[1], tolerance = 1e-3)
    expect_equivalent(row$ciues, es$conf.int[2], tolerance = 1e-3)
})
