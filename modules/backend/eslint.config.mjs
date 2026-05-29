import tseslint from 'typescript-eslint';
import stylistic from '@stylistic/eslint-plugin';

// Flat config replacing the former tslint.json. Each rule below maps to the
// equivalent tslint rule that used to be enforced (defaultSeverity: error).
export default tseslint.config(
  {
    ignores: ['built/**', 'dist/**', 'coverage/**', 'node_modules/**', 'ops/**', 'types/**'],
  },
  tseslint.configs.base,
  {
    files: ['src/**/*.ts'],
    plugins: {
      '@stylistic': stylistic,
    },
    rules: {
      // tslint: no-string-throw
      'no-throw-literal': 'error',
      // tslint: no-unused-expression
      'no-unused-expressions': 'error',
      // tslint: no-duplicate-variable
      'no-redeclare': 'error',
      // tslint: curly [true, "ignore-same-line"]
      'curly': ['error', 'multi-line'],
      // tslint: class-name
      '@typescript-eslint/naming-convention': [
        'error',
        {selector: 'class', format: ['PascalCase']},
      ],
      // tslint: semicolon [true, "always"]
      '@stylistic/semi': ['error', 'always'],
      // tslint: quotemark [true, "single", "avoid-escape"]
      '@stylistic/quotes': ['error', 'single', {avoidEscape: true}],
      // tslint: triple-equals
      'eqeqeq': ['error', 'always'],
    },
  },
);
