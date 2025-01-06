# Contributing to tasa-shiny-reporting

## Workflow

1. **Choose an Issue**
    - Check the **Issues** tab to select an issue which you can contribute to.
    - Once you have chosen an issue, assign yourself to the issue.
2. **Create a Branch**:
   - Create a branch which will contribute towards solving the chosen issue
   - Use concise descriptive names in **kebab-case** and present tense, such as `create-coral-bleaching-plot`
3. **Write your Code**
    - Work within your branch to address the issue.
    - Follow the [Styling](#styling) guidelines for the appropriate coding language.
    - Commit significant changes frequently, using descriptive commit messages:
        - Titles should be short, in present tense, and capitalized: e.g., `Add theming to coral bleaching plot`.
        - Add a description for extra details if needed.
4. **Make a Pull Request**:
   - When you are satisfied with your contribution, make a pull request.
   - Pull request titles should be short and descriptive, written in present tense with only the first letter capitalized and no end period, such as `Create coral bleaching plot`. You may choose to use the pull request description to add any extra details deemed important.
   - Base your pull request on the `main` branch unless otherwise specified.
   
5. **Get a Review**:
   - Request at least one review from a supervisor.
   - After receiving a review, make the requested changes or dispute the requested changes as needed. If changes are made, make and push a new commit. The commit will automatically update the pull request and the supervisor will be notified.
   - Once the supervisor is satisfied with the pull request, the supervisor will merge the branch. Thank you for your contributions!

## Styling

### R

When performing statistical analyses, creating visualizations for outputs or internal reports, or creating a Shiny app, R should be preferably used. R code should be formatted in scripts (.R) according to the [tidyverse style guide](https://style.tidyverse.org/index.html), established by Hadley Wickham. To make this process easier, the packages [styler](https://styler.r-lib.org/) and [lintr](https://github.com/r-lib/lintr) may be used to automatically style according to this standard. Please read the tidyverse style guide to familiarize with conventions for file names, variable and function names, syntax, formatting, and more. 

### Python

Python code should be formatted according to [PEP 8 style guide standards](https://peps.python.org/pep-0008/). To make this process easier, the extension [Pylint](https://www.pylint.org/) may be used to automatically style according to this standard.

### Git

Git branches should be titled all lowercase, with words separated by dashes. Where possible, standard sub-directory and file names should be used, for example .server and .ui R files for creating a shiny app, instead of project-specific names. Title commits, issues, and pull requests in present tense. Refer to [GitHub's best practices](https://docs.github.com/en/contributing/writing-for-github-docs/best-practices-for-github-docs) for more details.


## Team Practices

Please test all code changes locally before commiting. Do not create a pull request with code that breaks other app features. If you are unable to solve an issue, add the label `Help Wanted` onto the issue, explain the problem in a comment on the issue page, and clearly state the branch name. A supervisor or other  collaborator will follow up.

If you notice a bug or otherwise unsatisfactory feature on the app, feel free to open an issue. Clearly explain the issue in the description and with a descriptive issue title, and tag the issue appropriately.

This project is part of a **private repository**. As a contributor, you are expected to maintain the confidentiality and integrity of the code. Do not share any part of the codebase, including files or screenshots, outside of team members. Do not grant repository access to others without supervisor approval. By contributing to this repository, you agree to respect and uphold these privacy guidelines. If you have any questions or concerns about confidentiality, please reach out to the project supervisor.
