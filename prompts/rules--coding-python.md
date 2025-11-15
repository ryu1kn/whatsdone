# Rules: Python Coding

Follow all the rules provided in @prompts/rules--coding.md .

Additionally, follow the below rules:

- When reading command line arguments, parse them
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
