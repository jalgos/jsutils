context("Util S4")

## Base traits:

setClass("CHO", slots = c("u8" = "integer"))
setClass("ZZZ", slots = c("rox" = "numeric"))
setClass("UU", slots = c("doux" = "logical"))

test_that("adding traits",
{
    uu <- new("UU", doux = TRUE)
    uuz <- add.trait(uu, "ZZZ", rox = 3)
    expect_equal(class(uuz), "UU.ZZZ", check.attributes = FALSE)
    expect_equal(uuz@rox, 3)
    expect_equal(uuz@doux, TRUE)
})

test_that("adding two different traits",
{
    uu <- new("UU", doux = TRUE)
    uuz <- add.trait(uu, "ZZZ", rox = 3)
    uuzc <- add.trait(uuz, "CHO", u8 = 1000L)
    expect_equal(class(uuzc), "CHO.UU.ZZZ", check.attributes = FALSE)
    expect_equal(uuzc@rox, 3)
    expect_equal(uuzc@doux, TRUE)
    expect_equal(uuzc@u8, 1000L)
})

test_that("adding a trait that already exists",
{
    uu <- new("UU", doux = TRUE)
    uuz <- add.trait(uu, "ZZZ", rox = 3)
    uuzc <- add.trait(uuz, "CHO", u8 = 1000L)
    uuzc2 <- add.trait(uuzc, "CHO", u8 = 1000L)
    expect_equal(uuzc, uuzc2)
})

test_that("removing a trait",
{
    uu <- new("UU", doux = TRUE)
    uuz <- add.trait(uu, "ZZZ", rox = 3)
    uuzc <- add.trait(uuz, "CHO", u8 = 1000L)
    zc <- remove.trait(uuzc, "UU")
    expect_equal(class(zc), "CHO.ZZZ", check.attributes = FALSE)
    expect_equal(zc@rox, 3)
    expect_equal(zc@u8, 1000L)
})
