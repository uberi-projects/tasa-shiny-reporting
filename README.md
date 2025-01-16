# TASA Shiny Reporting

![status](https://img.shields.io/badge/status-in%20planning-blue)
![version](https://img.shields.io/badge/version-v0.0.0-blue)
![workflow: pull request only](https://img.shields.io/badge/workflow-pull_request_only-blue)

Repository (tasa-shiny-reporting) containing codebase to build a marine monitoring data reporting tool for Turneffe Atoll Sustainability Association (TASA) internal use, built through contract for consulting services with the University of Belize Environmental Research Institute (UB-ERI). The associated operational manual (in progress) will provide instructions for launching, maintaining, and updating the app as needed, to ensure long-term sustainability.  

The software owner is TASA, and the software developer is Jess Boles, Data Scientist, UB-ERI. Please direct questions about the codebase to Jess Boles, jessica.boles@ub.edu.bz.

# Contributing to the Project

By following the contributor guidelines outlined in this section, we can ensure everyone is on the same page regarding [workflow](#-workflow), [styling](#-styling), project [management structure](#-management-structure), and appropriate [team practices](#-team-practices) for developers on this project. Central to this document are the principles of consistency (following set standards across the team to ensure a high-quality finished product), transparency (making all project code accessible to team members, and making the source of code apparent), and respect (ensuring all team members are treated with respect, and treat the project itself with respect).

## 🔄 Workflow

Please follow the described workflow below when working on the project. 

1. **Choose an Issue**:
    - Check the **Issues** tab on the GitHub repository to select an issue which you can contribute to.
    - Once you have chosen an issue, assign yourself to the issue.
2. **Create a Branch**:
   - Create a branch which will contribute towards solving the chosen issue
   - Use concise descriptive names in **kebab-case** and present tense, such as `create-coral-bleaching-plot`
3. **Write your Code**:
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

Occasionally, contributors will receive tasks from their supervisor outside of the GitHub **Issues** tab. This will occur purely for logistical, training, or organizational purposes, and should never involve code development. All code development should occur transparently on GitHub. 

## 🎨 Styling

### R

When performing statistical analyses, creating visualizations for outputs or internal reports, or creating a Shiny app, R should be preferably used. R code should be formatted in scripts (.R) according to the [tidyverse style guide](https://style.tidyverse.org/index.html), established by Hadley Wickham. To make this process easier, the packages [styler](https://styler.r-lib.org/) and [lintr](https://github.com/r-lib/lintr) may be used to automatically style according to this standard. Please read the tidyverse style guide to familiarize with conventions for file names, variable and function names, syntax, formatting, and more. 

### Python

Python code should be formatted according to [PEP 8 style guide standards](https://peps.python.org/pep-0008/). To make this process easier, the extension [Pylint](https://www.pylint.org/) may be used to automatically style according to this standard.

### Git

Git branches should be titled all lowercase, with words separated by dashes. Where possible, standard sub-directory and file names should be used, for example .server and .ui R files for creating a shiny app, instead of project-specific names. Title commits, issues, and pull requests in present tense. Refer to [GitHub's best practices](https://docs.github.com/en/contributing/writing-for-github-docs/best-practices-for-github-docs) for more details.


## 🌟 Management Structure

This project is being funded by Turneffe Atoll Sustainability Association (TASA). TASA staff are collectively considered the client, while the University of Belize Environmental Research Institute (UB-ERI) represents the contractor. The project's scope, terms of agreement, and payment structure are explicitly outlined in the contract between the client and the contractor. By contributing to this project, you acknowledge your understanding of the conditions under which you are contributing.

The project supervisor, assigned by the contractor, is considered the lead code developer as a member of the contractor body, UB-ERI. The supervisor is responsible for overseeing all contributor activities, ensuring they adhere to the guidelines outlined in this document and the client agreement. The supervisor reports to the client directly as well as the head of the contractor organizer. 

Contributors are hired on as interns by the contractor. Contributors report directly to the supervisor. 


## 💖 Team Practices

### Project Quality

Please test all code changes locally before commiting. Do not create a pull request with code that breaks other app features. If you are unable to solve an issue, add the label `Help Wanted` onto the issue, explain the problem in a comment on the issue page, and clearly state the branch name. A supervisor or other  collaborator will follow up.

If you notice a bug or otherwise unsatisfactory feature on the app, feel free to open an issue. Clearly explain the issue in the description and with a descriptive issue title, and tag the issue appropriately. Please include enough detail so that others are able to reproduce the issue, which will be necessary to resolve the problem. 

All contributors are expected to do their best to ensure any code submitted through a pull request is of high quality, and follows the standards of this document.

### AI Use

While contributors are encouraged to write original code or incorporate code snippets from reliable sources online, the use of AI tools to assist with coding and development in this project is acceptable, provided that any use of AI tools for coding is clearly acknowledged with inline comments. Inline acknowledgement is not required for proofreading, such as finding missing commas, correcting typos, or identifying styling mismatches. By contributing to this project, you take responsibility for ensuring the quality and appropriateness of any AI-generated code.  Example acknowledgement comments include:

      ```
      # The contributors acknowledge the use of ChatGPT to solve a bug with knitting this PDF.
      # The contributors acknowledge the use of Copilot to generate the base code for this plot, which was adapted for use in this project.
      ```

### Code Confidentiality

This project is part of a private repository. As a contributor, you are expected to maintain the confidentiality and integrity of the code. Do not share any part of the codebase, including files or screenshots, to anyone outside of team members or the client. Do not grant repository access to others without supervisor approval. By contributing to this repository, you agree to respect and uphold these privacy guidelines. If you have any questions or concerns about confidentiality, please reach out to the project supervisor.

### Kindness and Respect

All team members are expected to do their best to treat other team members and clients with respect, using polite and professional language in inline code comments, in GitHub discussions, and in office where applicable.  Team members are also expected to treat the project itself with respect, and write code with the intention of bettering the project, and not harming its quality.