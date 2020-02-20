# aoo-paper

Code, data and figures for onepager describing the methodology of making occurrence cubes.

## Repo structure

The repository structure is based on [Cookiecutter Data Science](http://drivendata.github.io/cookiecutter-data-science/). Files and directories indicated with `GENERATED` should not be edited manually.

```
├── README.md            : Description of this repository
├── LICENSE              : Repository license
├── aoo-paper.Rproj : RStudio project file
├── .gitignore           : Files and directories to be ignored by git
│
├── data
│   ├── raw              : Occurrence data as downloaded from GBIF GENERATED
│   └── processed        : occurrence data cube and related taxonomic compendium GENERATED
│   └── external
        ├── utm1_bel     : EEA reference grid at 1km scale GENERATED
└── src
    ├── make_cube_reynoutria.R     : Script to generate occurrence cube
    ├── add_uncertainty_1000m.R    : Script to show occurrences with default uncertainty of 1000m
    ├── table_taxa_in_occurrences.R : Script to generate data.frame with all taxa in occurrence cube
    ├── code_occurrence-as-circles-and-random-pts-on-grid.R   : Script to show the method of assigning an occurrence to a grid cell
    ├── project_cube_to_2d.R      : Script to generate the subfigures (B) (C) and (D) about the projection of the cube on the 2d planes
```

## Contributors

[List of contributors](https://github.com/trias-project/occ-cube-alien/contributors)

## License

[MIT License](https://github.com/trias-project/unified-checklist/blob/master/LICENSE) for the code and documentation in this repository.
