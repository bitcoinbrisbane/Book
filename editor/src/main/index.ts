import { app, BrowserWindow, ipcMain, shell } from 'electron'
import { join, resolve, relative, sep, dirname } from 'node:path'
import { fileURLToPath } from 'node:url'
import { readFile, writeFile, readdir, stat } from 'node:fs/promises'
import simpleGit from 'simple-git'

const __dirname = dirname(fileURLToPath(import.meta.url))

// editor/out/main/index.js -> editor/ -> Book/
const BOOK_ROOT = resolve(app.getAppPath(), '..')
const git = simpleGit(BOOK_ROOT)

type TreeNode =
  | { type: 'dir'; name: string; path: string; children: TreeNode[] }
  | { type: 'file'; name: string; path: string }

function safeResolve(relPath: string): string {
  const abs = resolve(BOOK_ROOT, relPath)
  const rel = relative(BOOK_ROOT, abs)
  if (rel.startsWith('..') || rel.startsWith(sep)) {
    throw new Error(`path escapes book root: ${relPath}`)
  }
  return abs
}

async function listTree(): Promise<TreeNode[]> {
  const entries = await readdir(BOOK_ROOT, { withFileTypes: true })
  const nodes: TreeNode[] = []
  for (const e of entries) {
    if (e.name.startsWith('.') || e.name === 'node_modules' || e.name === 'editor') continue
    const abs = join(BOOK_ROOT, e.name)
    if (e.isDirectory()) {
      const children = await listMarkdownIn(abs, e.name)
      if (children.length > 0) {
        nodes.push({ type: 'dir', name: e.name, path: e.name, children })
      }
    } else if (e.isFile() && e.name.endsWith('.md')) {
      nodes.push({ type: 'file', name: e.name, path: e.name })
    }
  }
  nodes.sort((a, b) => a.name.localeCompare(b.name))
  return nodes
}

async function listMarkdownIn(absDir: string, relDir: string): Promise<TreeNode[]> {
  const entries = await readdir(absDir, { withFileTypes: true })
  const out: TreeNode[] = []
  for (const e of entries) {
    if (!e.isFile() || !e.name.endsWith('.md')) continue
    out.push({ type: 'file', name: e.name, path: `${relDir}/${e.name}` })
  }
  out.sort((a, b) => a.name.localeCompare(b.name))
  return out
}

function registerHandlers() {
  ipcMain.handle('book:listTree', () => listTree())

  ipcMain.handle('book:readFile', async (_e, relPath: string) => {
    const abs = safeResolve(relPath)
    return readFile(abs, 'utf8')
  })

  ipcMain.handle('book:writeFile', async (_e, relPath: string, content: string) => {
    const abs = safeResolve(relPath)
    await writeFile(abs, content, 'utf8')
  })

  ipcMain.handle('book:gitStatus', async () => {
    const s = await git.status()
    return {
      modified: s.modified,
      not_added: s.not_added,
      staged: s.staged,
      ahead: s.ahead,
      behind: s.behind,
      current: s.current
    }
  })

  ipcMain.handle('book:gitCommitPush', async (_e, message: string) => {
    try {
      await git.add('.')
      const diff = await git.diff(['--cached', '--name-only'])
      if (!diff.trim()) return { ok: false, error: 'nothing staged' }
      await git.commit(message)
      await git.push()
      return { ok: true }
    } catch (err) {
      return { ok: false, error: err instanceof Error ? err.message : String(err) }
    }
  })

  ipcMain.handle('book:root', () => BOOK_ROOT)
}

function createWindow() {
  const win = new BrowserWindow({
    width: 1400,
    height: 900,
    title: 'Book Editor',
    webPreferences: {
      preload: join(__dirname, '../preload/index.mjs'),
      contextIsolation: true,
      nodeIntegration: false,
      sandbox: false
    }
  })

  win.webContents.setWindowOpenHandler((details) => {
    shell.openExternal(details.url)
    return { action: 'deny' }
  })

  if (process.env['ELECTRON_RENDERER_URL']) {
    win.loadURL(process.env['ELECTRON_RENDERER_URL'])
  } else {
    win.loadFile(join(__dirname, '../renderer/index.html'))
  }
}

app.whenReady().then(() => {
  registerHandlers()
  createWindow()
  app.on('activate', () => {
    if (BrowserWindow.getAllWindows().length === 0) createWindow()
  })
})

app.on('window-all-closed', () => {
  if (process.platform !== 'darwin') app.quit()
})
