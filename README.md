# CS3110 Project

Welcome to our Five Nights at Freddy's project for CS3110! This document serves as a guide for setting up and using the tools and programs associated with our project.

## Team Members
- **Samarth (Sam) Shridhar**
  - NetID: shs96
  - Email: sam.shridhar1950f@gmail.com or shs96@cornell.edu
- **Larry Tao**
  - NetID: ltt28
  - Email: ltt28@cornell.edu or larrytao100@gmail.com
- **Jacob Huang**
  - NetID: jzh23
  - Email: jzh23@cornell.edu or jacobzghuang@gmail.com
- **Rohan Mahajan**
  - NetID: rm939
  - Email: rm939@cornell.edu or rohan.mahajan@icloud.com

## Setup & Installation

Before running the project, there are a few prerequisites and setup steps to ensure everything works smoothly, especially regarding the graphics components.

### For Mac Users: Install XQuartz
- **XQuartz** is required for the graphics to function properly on macOS.
- Download and install XQuartz from [here](https://www.xquartz.org/).
- Remember to **open XQuartz** before running the program.

### Install Necessary Packages
- You'll need to install the `graphics` package using OPAM. Run the following command in your terminal:
  ```
  opam install graphics
  ```

### Running the Program
To compile and execute the program, follow these steps:
1. **Build the Program:**
   ```
   OCAMLRUNPARAM=b dune build
   ```
2. **Execute the Program:**
   ```
   OCAMLRUNPARAM=b dune exec bin/main.exe
   ```

## Playing the Terminal Game
To play the terminal-based game included in our project:
1. Navigate to the `/bin` directory:
   ```
   cd /bin
   ```
2. Run the game executable:
   ```
   ./game
   ```
