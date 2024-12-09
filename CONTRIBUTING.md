# Contributing to AVR Emulator

🎉 Welcome, and thank you for considering contributing to the AVR Emulator project! This project aims to provide a robust emulator for the AVR instruction set, and your input—whether through code, documentation, or discussions—is invaluable.

## How to Contribute

We welcome contributions in the form of bug fixes, new features, tests, or improvements to the documentation. If you're unsure where to start, take a look at the open [issues](https://github.com/Flu/avr-emulator/issues)—some are tagged as "good first issue" for newcomers.

### Step 1: Fork the Repository

If you're new to forking, here's a guide from GitHub: [Fork a repo](https://docs.github.com/en/get-started/quickstart/fork-a-repo).

1. Click the "Fork" button at the top-right corner of the repository page.
2. Clone the fork to your local machine:
```bash
git clone https://github.com/<your-username>/avr-emulator.git
```
3. Navigate to the project directory:
```bash
cd avr-emulator
```

### Step 1: Create a new branch

Always create a new branch for your changes. This keeps your work isolated and makes it easier to manage. If you're adding something, like a new feature, your branch name should be something like `feature/your-feature-branch`. If it fixes a bug, it should be something like `fix/some-bug`.

```bash
git checkout -b feature/your-feature-branch
```

```bash
git checkout -b fix/some-bug
```

### Step 3: Make your changes
- Address an issue or implement a feature from the issues page. Be sure to leave a comment on the issue to let others know you're working on it.
- Mention the issue number in your commits if possible, so there's a clear link between your commits and the issue it solves.
- If your changes add significant functionality, consider writing tests for them (see "Running and Adding Tests" below).
- Ensure your code follows the style and conventions of the project.

### Step 4: Run Tests
Before submitting your changes, make sure all tests pass:

```bash
cabal test all
```

If you’ve added new functionality, add corresponding tests to cover the changes. Tests ensure that the project remains reliable as it grows. You can find existing tests in the app/tests/ directory.

### Step 5: Push and Create a pull request
Push your branch to your forked repository:

```bash
git push origin your-feature-branch
```

Go to the original repository's page and click the "Pull Request" button.
Fill in a clear title and description for your pull request, detailing:
 - The problem you're solving or feature you're adding, mentioning the issue number (e.g. Fixing issue #344).
 - How you’ve tested your changes.

### Step 6: Wait for review

One of the maintainers will review your pull request. Please be patient, as it may take some time. Be prepared to make further changes if requested.

## Guidelines

### Code Style
 - Follow the conventions used in the existing codebase.
 - Keep your code clean and well-documented.
 - Write descriptive commit messages that explain your changes.

### Bug Reports and Feature Requests
If you're not ready to contribute code but have identified a bug or have a feature idea, feel free to open an issue on the issues page. Make sure to provide as much detail as possible, including steps to reproduce the bug or a clear description of the feature.

Thank you for helping make this project better!