import tseslint from 'typescript-eslint';
import stylistic from '@stylistic/eslint-plugin';

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
      'no-throw-literal': 'error',
      'no-unused-expressions': 'error',
      'no-redeclare': 'error',
      'curly': ['error', 'multi-line'],
      '@typescript-eslint/naming-convention': [
        'error',
        {selector: 'class', format: ['PascalCase']},
      ],
      '@stylistic/semi': ['error', 'always'],
      '@stylistic/quotes': ['error', 'single', {avoidEscape: true}],
      'eqeqeq': ['error', 'always'],
    },
  },
);
