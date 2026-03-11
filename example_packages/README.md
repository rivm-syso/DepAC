# Example Projects Using DepAC

This repository contains example projects demonstrating how to use DepAC. More examples will be added in the future.

## Current Examples

-- [basic_depac](./basic_depac):
	This is a minimal working example demonstrating how to use the DepAC model in a Fortran application. The example initializes a set of input parameters for a single chemical component (NH3), a land use type (grass), and meteorological conditions. It then configures the DepAC model, sets compensation point parameters, and calls the main DepAC routine.

	The program prints out the calculated total canopy resistance, total canopy conductance, compensation point, and effective canopy resistance. It also demonstrates error handling by checking for model errors after the calculation. This example serves as a template for integrating DepAC into other scientific workflows or models, showing the required input structure and typical output.
-- [performance_depac](./performance_depac):
	This example is designed to test the performance of the DepAC model when run multiple times with random input parameters. It initializes a large number of runs (10,000,000) and allocates an array to store random values for the input parameters. The program then iteratively calls the DepAC model for each set of random inputs, allowing users to assess the computational efficiency and scalability of the model under varying conditions. This example is useful for benchmarking and optimizing the performance of DepAC in larger simulations or when integrated into more complex models.