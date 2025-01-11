import { defineConfig } from '@playwright/test';

export default defineConfig({
  webServer: {
    command: 'npm run start',
    url: 'http://localhost:48080',
    reuseExistingServer: false,
    stdout: 'pipe',
    stderr: 'pipe',
    timeout: 30 * 1000,
  },
  use: {
    baseURL: 'http://localhost:48080',
  },
});
