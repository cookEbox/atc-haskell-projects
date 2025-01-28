# Project 1: Todo List Manager

## How to Run the Application

Install Nix if you haven't already. Follow the instructions at https://nixos.org/download.html.

You'll need to ensure your Github is properly set up for SSH. Follow the instructions at https://docs.github.com/en/authentication/connecting-to-github-with-ssh/generating-a-new-ssh-key-and-adding-it-to-the-ssh-agent 

Load the Nix environment by navigating to the project folder and running in your terminal:

```bash
# If you haven't already 
git clone git@github.com:Ace-Interview-Prep/atc-haskell-projects.git
cd atc-haskell-projects
git switch 1-todo-manager

nix-shell
```

This will load your development environment with all the necessary Haskell dependencies.

Build the project using cabal:

```bash
cabal build
```

Run the application:

```bash
cabal run
```

Test the CLI by typing in commands and interacting with the recursive prompt.

## Software Requirements

### Basic Functionality:
- Users can add a new task to the to-do list.
- Users can view all tasks in the to-do list.
- Users can mark a task as completed.
- Users can delete a task from the list.
- Users can edit a task’s description.
- Tasks should be stored persistently (e.g., in a text file).

### Advanced Features (Optional):
- Allow users to prioritize tasks (e.g., High, Medium, Low).
- Implement due dates for tasks and sort tasks by due date.
- Filter tasks by their completion status (e.g., show only incomplete tasks).
- Provide command-line options to manage tasks without entering an interactive mode (e.g., `todo add "Buy groceries"`).

### User Interface:
- A simple and intuitive command-line interface.
- Display a help menu when requested (`--help` or `-h`), listing all available commands.

### Error Handling:
- The application should handle errors gracefully, providing user-friendly messages (e.g., when trying to mark a non-existent task as complete).

### Code Structure:
- The code should be modular, separating concerns (e.g., task management, file handling, user interaction).
- Follow best practices for Haskell, including proper use of types, functions, and purity where applicable.

## Acceptance Criteria:
- **Task Management:**
  - The user can successfully add a new task, view it, mark it as complete, edit it, and delete it.
  - Tasks persist between sessions (i.e., closing and reopening the application should not lose data).

- **Advanced Features (if implemented):**
  - Tasks can be prioritized, and the list can be filtered or sorted as per user input.
  - Tasks with due dates are sorted correctly when the user requests it.

- **User Interface:**
  - The help menu is clear and correctly displays all available commands.
  - Commands are intuitive and easy to use.

- **Error Handling:**
  - The application should not crash or behave unexpectedly when given invalid input (e.g., marking a non-existent task as complete).

- **Code Quality:**
  - The code should be clean, well-documented, and follow Haskell best practices.
  - Modular design should be evident, with distinct functions and types handling different aspects of the application.

## Rubric:

# Task Manager Grading Rubric

| **Category**          | **Criteria**                                  | **Points** | **Score** |
|----------------------|----------------------------------------------|------------|----------|
| **Basic Functionality (40 points)** | | | |
|                      | Adding a task                               | 10         | 8 (Missing input validation on priority)        |
|                      | Viewing tasks                              | 5          | 5         |
|                      | Marking a task as complete                 | 10         | 8 (Priority is missing, extra space in string)         |
|                      | Deleting a task                            | 10         | 8 (Should show the task list before deleting, and should show details of task before deleting (ask for confirmation)        |
|                      | Editing a task                             | 5          | 5         |
| **Advanced Features (Optional - 20 points)** | | | |
|                      | Task prioritization                        | 5          | 5         |
|                      | Due dates and sorting                      | 5          | 0         |
|                      | Filtering tasks by status                  | 5          | 5         |
|                      | Command-line options for non-interactive mode | 5      | 0         |
| **User Interface (20 points)** | | | |
|                      | Help menu                                  | 10         | 10         |
|                      | Overall usability and intuitiveness        | 10         | 10         |
| **Error Handling (10 points)** | | | |
|                      | Graceful handling of invalid input         | 5          | 4         |
|                      | User-friendly error messages               | 5          | 5         |
| **Code Quality (30 points)** | | | |
|                      | Modular design                             | 10         | 9 (some reusability, but overall great work here)         |
|                      | Code cleanliness and readability           | 10         | 10         |
|                      | Use of Haskell best practices              | 10         | 8 (ensure state is valid at all time)         |
| **Total**            | *(100 points if advanced features are not implemented, 120 if they are)* | **100** | **100** |
