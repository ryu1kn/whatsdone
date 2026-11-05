import {defineConfig} from 'vitest/config';

process.env.TZ = 'Australia/Melbourne';

export default defineConfig({
  test: {
    globals: true,
    include: ['test/**/*.test.{ts,tsx}'],
    environment: 'node'
  }
});
