# AoC2019
[Advent of Code 2019](https://adventofcode.com/2019) challenges. Using F# to solve, for fun, and Github Actions to provide a workflow on commit, also for fun.

# Setup
Used the dotnet .Core CLI to set up the original project after dabbling with FAKE, Paket and directly within VS Code via Ionide.

Eventually slimmed back down to just using the CLI to add project and solution resources and build, test and run the application.

(Github Actions did not at this time have a FAKE action built in, and the dotnet CLI gave me everything I wanted, including Actions support.)

# Notes
The Nunit test project shouldn't really need a Program.fs but if this file is not included, the build generates a warning as documented [here](https://github.com/dotnet/fsharp/issues/2669).
