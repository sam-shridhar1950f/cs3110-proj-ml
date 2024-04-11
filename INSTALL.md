## Setup & Installation

Before running the project, there are a few prerequisites and setup steps to ensure everything works smoothly, especially regarding the graphics components.

### For Mac Users: Install XQuartz
- **XQuartz** is required for the graphics to function properly on macOS.
- Download and install XQuartz from [here](https://www.xquartz.org/).
- Remember to **open XQuartz** before running the program.

### For Windows Users: Install XMing
- **XMing** is required for the graphics to function properly on macOS.
- Download and install XQuartz from [here](https://sourceforge.net/projects/xming/).
- Remember to **open your XServer** before running the program.

### Install Necessary Packages
- You'll need to install the `graphics` package using OPAM. Run the following command in your terminal:
  ```
  opam install graphics
  ```

### Running the Program
To compile and execute the program, follow these steps:
1. **Build the Program:**
   ```
   dune build
   ```
2. **Execute the Program:**
   ```
   OCAMLRUNPARAM=b dune exec bin/main.exe
   ```

## Playing the Terminal Game
To play the terminal-based game included in our project:
1. Navigate to the `bin` directory:
   ```
   cd bin
   ```
2. Run the game executable:
   ```
   ./game
   ```
   If the game does not start, try running:
   ```
   ocamlrun game
   ```
