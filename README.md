# Project 2: Tic Tac Toe CLI

## How to Run the Application

Install Nix if you haven't already. Follow the instructions at https://nixos.org/download.html.

Load the Nix environment by navigating to the project folder and running in your terminal:

```bash
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
- Users can play a game of Tic Tac Toe against another player (two-player mode).
- The game board should be displayed in the command line.
- Players should be able to place their mark (X or O) on the board by specifying the row and column.
- The game should detect and announce when a player has won or when the game ends in a draw.
- The game should allow players to restart the game after it ends.

### Advanced Features (Optional):
- Implement an AI opponent that players can play against.
- Allow players to choose their mark (X or O) before the game starts.
- Implement a scoring system that tracks wins, losses, and draws.
- Provide a command-line option to view the help menu during the game.

### User Interface:
- The game board should be clearly displayed after each move, with current player information.
- Display a help menu that explains how to make moves and restart the game.

### Error Handling:
- The game should prevent invalid moves (e.g., placing a mark in an already occupied cell or outside the grid).
- The application should handle input errors gracefully and prompt the user to try again.

### Code Structure:
- The code should be modular, separating concerns (e.g., game logic, user input, display).
- Follow best practices for Haskell, ensuring functions are pure and data types are well-defined.

## Acceptance Criteria:
- **Game Mechanics:**
  - The game must accurately detect win conditions and draws.
  - Players should be able to make moves without errors, and the board should update correctly.

- **Advanced Features (if implemented):**
  - The AI should make reasonable moves and provide a challenging opponent.
  - The scoring system should correctly track results and reset when requested.

- **User Interface:**
  - The game should display the board and instructions clearly.
  - The help menu should be accessible and informative.

- **Error Handling:**
  - Invalid moves should be prevented, and the user should be prompted to enter a valid move.
  - The game should not crash due to invalid input.

- **Code Quality:**
  - The code should be clean, modular, and well-documented.
  - Haskell best practices should be evident throughout the project.

## Rubric:

| **Category**          | **Criteria**                                  | **Points** | **Score** |
|----------------------|----------------------------------------------|------------|----------|
| **Basic Functionality (40 points)** | | | |
|                      | Two-player mode                             | 15         | 15         |
|                      | Board display and updating                  | 10         | 10         |
|                      | Win/draw detection                          | 10         | 10         |
|                      | Game restart option                         | 5          | 5         |
| **Advanced Features (Optional - 20 points)** | | | |
|                      | AI opponent                                 | 10         | 10         |
|                      | Customizable player marks                   | 5          | 5         |
|                      | Scoring system                              | 5          | 5         |
| **User Interface (20 points)** | | | |
|                      | Board clarity and player information        | 10         | 10         |
|                      | Help menu                                   | 10         | 10         |
| **Error Handling (10 points)** | | | |
|                      | Prevention of invalid moves                 | 5          | 5         |
|                      | Graceful handling of input errors           | 5          | 5         |
| **Code Quality (30 points)** | | | |
|                      | Modular design                              | 10         | 10         |
|                      | Code cleanliness and readability            | 10         | 10         |
|                      | Use of Haskell best practices               | 10         | 9.5 (try not to use show instances for writing to the user)        |
| **Total**            | *(100 points if advanced features are not implemented, 120 if they are)* | **100** | **119.5** |
