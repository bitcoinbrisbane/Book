import { contextBridge, ipcRenderer } from 'electron'

export type TreeNode =
  | { type: 'dir'; name: string; path: string; children: TreeNode[] }
  | { type: 'file'; name: string; path: string }

export type GitStatus = {
  modified: string[]
  not_added: string[]
  staged: string[]
  ahead: number
  behind: number
  current: string | null
}

export const api = {
  listTree: (): Promise<TreeNode[]> => ipcRenderer.invoke('book:listTree'),
  readFile: (path: string): Promise<string> => ipcRenderer.invoke('book:readFile', path),
  writeFile: (path: string, content: string): Promise<void> =>
    ipcRenderer.invoke('book:writeFile', path, content),
  gitStatus: (): Promise<GitStatus> => ipcRenderer.invoke('book:gitStatus'),
  gitCommitPush: (message: string): Promise<{ ok: boolean; error?: string }> =>
    ipcRenderer.invoke('book:gitCommitPush', message),
  root: (): Promise<string> => ipcRenderer.invoke('book:root')
}

contextBridge.exposeInMainWorld('api', api)
