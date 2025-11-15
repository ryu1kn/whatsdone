# Rules: Coding

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

- When referring to a file / directory within a project, always use a relative path and do NOT hardcode an absolute path.
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

  with open(script_dir / '..' / 'some-file') as f:
      # Do something with `w`
  ```

