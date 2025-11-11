# Rule: Setting up a python project

Whenever setting up a python project, carefully follow the below instruction:

- Use `mise` to install python interpreter
- Use `mise` (`mise.toml`) to install `uv`
- Use `uv` to manage dependencies
- Use `uv` to create a python virtual environment
- Use `mise` to automatically activate the python virtual environment
- Use `pyproject.toml` to describe the project and its dependencies
- Do not use requirements.txt as we don't use `pip`
- Use `uv sync` command to lock dependency versions

The `mise.toml` should look like this

```toml
[tools]
python = "3.13"
uv = "0.6.14"

[env]
_.python.venv = { path = ".venv", create = true }
```
