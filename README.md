# Molecular Dynamics in Vulkan

A molecular dynamics simulation implement in Haskell + Vulkan, using [FIR](https://gitlab.com/sheaf/fir).

Example: (Unrealistically) supercooled gas condensing. Clusters of larger atoms form, and lose heat by emitting small atoms.

https://github.com/JakobBruenker/vulkan-molecules/assets/10101851/b3c655c5-5cc9-4977-9fbb-a5b57666a154


## Controls:

Space: Pause/Run simulation
Escape: Close

## Planned:
- use (implemented) automatic differentiation to make compute shaders simpler and easier to experiment with different potentials
- implement ReaxFF to facilitate creating and breaking covalent bonds
- Implement more elements, with realistic properties (currently existing are essentially Hydrogen+Carbon)
- Implement neighbor list to improve performance with large number of atoms
- Make it possible to add/remove/drag atoms at runtime
