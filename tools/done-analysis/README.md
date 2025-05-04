# Done analysis

This directory is a space to do data analysis on this What's Done app.

## Setup

1. Install [mise](https://mise.jdx.dev/) if you haven't already.
2. In this directory, run:

```sh
mise install
```

This will:
- Install the correct Python and uv versions
- Create and activate the `.venv` virtual environment
- Install all dependencies from `pyproject.toml` using `uv sync`

## Usage

- To install or update dependencies after editing `pyproject.toml`:

```sh
uv sync
```

- To start Jupyter Notebook:

```sh
jupyter notebook
```

## Notebooks

Place your notebooks in the `notebooks/` directory (create it if it doesn't exist).
