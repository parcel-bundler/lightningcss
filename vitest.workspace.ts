import {defineWorkspace} from 'vitest/config';

export default defineWorkspace([
  {
    test: {
      name: 'Node.js',
      environment: 'node',
      restoreMocks: true,
    },
  },

  // TODO: enable for browser tests
  // {
  //   test: {
  //     name: 'Browser',
  //     restoreMocks: true,
  //     env: {
  //       TEST_WASM: 'browser',
  //     },
  //     browser: {
  //       provider: 'playwright',
  //       enabled: true,
  //       instances: [{browser: 'chromium'}],
  //     },
  //   },
  // },
]);
