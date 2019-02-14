# LOGAN

An R package for log file analysis. It can be used as a standalone R package or as a backend for the accompanying Shiny application.

# Planned features

- Improved documentation (e.g. Wiki)
- Integration with the modules package
- Speed improvements (change loops into calls to "*apply")

# Style guide

The following is an adaptation of Google's R Style Guide, and is written here to motivate good writing practices for all package contributors. Following the guidelines below will help make LOGAN's source code easier to read, share and verify.

It is important to notice the correct letter case used in the guidelines below. The Windows Operating System may not differentiate between lowecase and uppercase letters, but other OSs such as GNU/Linux and MacOS (not to mention the R language itself) do, so a file called "A.R" is different from another called "a.r".

## File and object naming 

1. Files containing R code should end in `.R`
2. Function names should have Pascal case, e.g. `FunctionName`
3. Other object names (function arguments, variables, constants, etc.) should have lowercase names with dots for spaces, e.g. `variable.name`

## Source code formatting

1. Lines of code should have no more than **80 characters** per column. If one line of your code is taking longer than that, it would probably me more readable if it is broken into multiple lines
2. Indentation should be made with **two spaces**, no tabs
3. The opening brace should be on the same line; the closing brace should be in its own line

For example, instead of this:

```
if (x == 1)
{
    print(x)
}
```

write this:

```
if (x == 1) {
    print(x)
}
```

The exception is the `else` statement, which should occupy the same line as the closing brace of the previous `if` statement, like this:

```
if (x == 1) {
    print(x)
} else {
    print("not one")       
}
```

4. Assignments should be made with `<-`, not `=`
5. Don't use `;` or `attach`
6. All binary operators (`=`, `==`, `+`, `!=`, `<-`, etc.) must have spaces before and after. There is no space after an opening parenthesis or before a closing parenthesis.
7. Never place a space before a comma
8. Always place a space after a comma

## Commenting
1. Inline comments need at least one space before the `#`. All comments need one space after `#`
2. Any persisting `# TODO:` comment should be written in the `README.md` file under Section `Planned features`
3. When commenting, remember to favor explaqining the "why", not the "how"

## References
- Google's R Style Guide: https://google.github.io/styleguide/Rguide.xml
- R Style. An Rchaeological Commentary: https://cran.r-project.org/web/packages/rockchalk/vignettes/Rstyle.pdf
- R packages style guide: http://r-pkgs.had.co.nz/style.html