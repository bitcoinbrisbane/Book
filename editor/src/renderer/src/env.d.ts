/// <reference types="vite/client" />

import type { api } from '../../preload'

declare global {
  interface Window {
    api: typeof api
  }
}

export {}
