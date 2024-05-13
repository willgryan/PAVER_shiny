# PAVER

## Description

PAVER is a bioinformatics application designed to create summary interpretations and visualizations of pathway analyses.

## Installation

### Requirements

- R

### Instructions

Clone the repository, then run the following commands to run the application:

```bash
cd PAVER_shiny
R
```

```R
renv::update()
renv::hydrate()
renv::restore()
shiny::runApp()
```

## Contributing

Contributions are welcome. If you want to contribute to PAVER, please follow these steps:

1. Fork the repository
2. Create a new branch (`git checkout -b feature/improvement`)
3. Make your changes
4. Commit your changes (`git commit -am 'Add new feature'`)
5. Push to the branch (`git push origin feature/improvement`)
6. Create a new Pull Request
