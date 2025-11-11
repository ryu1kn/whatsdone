# General guidelines

## Project context

- This "What's Done" system is written in TypeScript + React (Node.js).
- Use `yarn` as a package manager for Node.js. Do not use `npm`.
- Use `uv` as a package manager for Python. Do not use `pip`.

## Coding guidelines

- Make the code self-explanatory so that you won't need comments. Comments should be to clarify "Why" instead of "What".

  Bad example: "Multiply by 24"

  Good example: "Multiplying by 24 as we need to count for whole day"

- For non-customer facing part, don't use try-catch to just provide custom error messages. It makes the code unnecessarily long
  and harder to read.

  Bad example:

  ```python
  try:
      response = self.comprehend.list_document_classifiers()
      return response['DocumentClassifierPropertiesList']
  except Exception as e:
      print(f"Error listing classifiers: {str(e)}")
      raise
  ```

  Good example:

  ```python
  response = self.comprehend.list_document_classifiers()
  return response['DocumentClassifierPropertiesList']
  ```

- When referring to a file / directory within a project, always use a relative path and do not hardcode an absolute path.
  This is to ensure the code can run in other environments (deployed environments, other developers' laptops or CI)

  DON'T:

  ```python
  with open('/Users/foo/projects/whatsdone/some-file') as f:
      # Do something with `w`
  ```

  DO:

  ```python
  from pathlib import Path

  script_dir = Path(__file__).resolve().parent
  project_dir = script_dir / path / to / project_root

  with open(project_dir / 'some-file') as f:
      # Do something with `w`
  ```

## Git commits

- Do not add "conventional commit" style message prefix like "feat:".

  DON'T:

  ```sh
  git commit -m 'feat: Added feature A"
  ```

  DO:

  ```sh
  git commit -m "Added feature A"
  ```

## Jupyter notebook

- When asked to create a notebook, create a Jupyter notebook under tools/done-analysis/notebooks.
