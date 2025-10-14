# Contributing to distionary

Thank you for considering contributing to `distionary`! This package
and the probaverse more generally have ambitious goals, so your help
is needed. This document 
provides guidelines and instructions for contributing to the package.

## Code of Conduct

Please note that this project is released with a 
[Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in 
this project you agree to abide by its terms.

## How Can I Contribute?

### Reporting Bugs

If you find a bug, please open an issue on 
[GitHub](https://github.com/probaverse/distionary/issues) with:

- A clear, descriptive title
- Steps to reproduce the issue
- Expected behavior
- Actual behavior
- Your R session info (output of `sessionInfo()`)
- A minimal reproducible example (reprex)

You can create a reprex using the 
[reprex package](https://reprex.tidyverse.org/), for example.

### Suggesting Enhancements

Enhancement suggestions are tracked as GitHub issues. When creating an 
enhancement suggestion, please include:

- A clear, descriptive title
- A detailed description of the proposed enhancement
- Explanation of why this enhancement would be useful
- Examples of how the enhancement would be used

If your suggestion is about adding a new distribution family, please
indicate why you think the family is significant enough to be included.

### Pull Requests

We welcome pull requests! Here's the process:

1. **Fork the repository** and clone it locally.
2. **Create a branch** for your changes.
3. **Make your changes** following the coding style guidelines.
4. **Write or update tests** to cover your changes.
5. **Run checks** locally.
6. **Update documentation** including roxygen2 comments and vignettes 
   if needed.
7. **Commit your changes** with clear, descriptive commit messages.
8. **Push to your fork** and submit a pull request to the 
   `development` branch.


## Coding Style

`distionary` follows the 
[tidyverse style guide](https://style.tidyverse.org/). Key points:

- Use `<-` for assignment, not `=`
- Use `snake_case` for function and variable names
- Keep lines to a maximum of 80 characters
- Add spaces around operators (`x + y`, not `x+y`)
- Add spaces after commas (`f(x, y)`, not `f(x,y)`)


### Documentation Style

- All exported functions must have complete roxygen2 documentation
- Include `@param` for all parameters with clear descriptions
- Include `@return` describing what the function returns
- Include `@examples` with working examples
- Use proper markdown formatting in documentation


## Questions?

If you have questions about contributing, please
contact the maintainer: Vincenzo Coia (vincenzo.coia@gmail.com).
