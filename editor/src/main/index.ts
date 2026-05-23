import { app, BrowserWindow, ipcMain, shell } from 'electron'
import { join, resolve, relative, sep, dirname } from 'node:path'
import { fileURLToPath } from 'node:url'
import { readFile, writeFile, readdir, stat } from 'node:fs/promises'
import { spawn } from 'node:child_process'
import simpleGit from 'simple-git'

const __dirname = dirname(fileURLToPath(import.meta.url))

// editor/out/main/index.js -> editor/ -> Book/
const BOOK_ROOT = resolve(app.getAppPath(), '..')
const git = simpleGit(BOOK_ROOT)

type TreeNode =
  | { type: 'dir'; name: string; path: string; children: TreeNode[] }
  | { type: 'file'; name: string; path: string }

function countWords(text: string): number {
  const stripped = text
    .replace(/```[\s\S]*?```/g, ' ')
    .replace(/`[^`]*`/g, ' ')
    .replace(/!\[[^\]]*\]\([^)]*\)/g, ' ')
    .replace(/\[([^\]]*)\]\([^)]*\)/g, '$1')
    .replace(/[#>*_~\-]/g, ' ')
  const matches = stripped.match(/\S+/g)
  return matches ? matches.length : 0
}

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

const COMMIT_PROMPT = `You are writing a git commit message for a Markdown book repository.

A unified git diff will be provided on stdin. Read it and output ONE conventional-commit message:
- First line: imperative subject, <= 72 chars, prefixed with one of: chore:, feat:, fix:, docs:, refactor:.
- Optionally one blank line then 1-3 short bullets starting with "- ".
- Output ONLY the message. No code fences. No preamble. No trailing commentary.`

const MAX_DIFF_BYTES = 200_000

function runClaudeForCommitMessage(diff: string): Promise<string> {
  const payload = diff.length > MAX_DIFF_BYTES ? diff.slice(0, MAX_DIFF_BYTES) : diff
  return new Promise((resolveP, rejectP) => {
    const proc = spawn('claude', ['-p', COMMIT_PROMPT], {
      stdio: ['pipe', 'pipe', 'pipe'],
      cwd: BOOK_ROOT
    })
    let stdout = ''
    let stderr = ''
    proc.stdout.on('data', (d) => (stdout += d.toString()))
    proc.stderr.on('data', (d) => (stderr += d.toString()))
    proc.on('error', rejectP)
    proc.on('close', (code) => {
      if (code !== 0) {
        return rejectP(new Error(stderr.trim() || `claude exited ${code}`))
      }
      const msg = stdout.trim().replace(/^```[a-z]*\n?|\n?```$/g, '').trim()
      if (!msg) return rejectP(new Error('claude returned empty output'))
      resolveP(msg)
    })
    proc.stdin.write(payload)
    proc.stdin.end()
  })
}

function runGhCreatePr(opts: {
  baseBranch: string
  branch: string
  title: string
  body: string
}): Promise<string> {
  return new Promise((resolveP, rejectP) => {
    const proc = spawn(
      'gh',
      [
        'pr',
        'create',
        '--base',
        opts.baseBranch,
        '--head',
        opts.branch,
        '--title',
        opts.title,
        '--body',
        opts.body
      ],
      { cwd: BOOK_ROOT, stdio: ['ignore', 'pipe', 'pipe'] }
    )
    let stdout = ''
    let stderr = ''
    proc.stdout.on('data', (d) => (stdout += d.toString()))
    proc.stderr.on('data', (d) => (stderr += d.toString()))
    proc.on('error', (err) => {
      const msg =
        (err as NodeJS.ErrnoException).code === 'ENOENT'
          ? 'gh CLI not found — install it from https://cli.github.com/'
          : err.message
      rejectP(new Error(msg))
    })
    proc.on('close', (code) => {
      if (code !== 0) return rejectP(new Error(stderr.trim() || `gh exited ${code}`))
      const url = stdout
        .trim()
        .split('\n')
        .find((l) => l.startsWith('http'))
      resolveP(url || stdout.trim())
    })
  })
}

function slugForBranch(s: string): string {
  return (
    s
      .toLowerCase()
      .replace(/[^a-z0-9]+/g, '-')
      .replace(/^-+|-+$/g, '')
      .slice(0, 40) || 'edit'
  )
}

async function runSuggestChange(args: {
  filePath: string
  before: string
  after: string
  comment: string
}): Promise<{ ok: true; prUrl: string; branch: string } | { ok: false; error: string }> {
  const { filePath, before, after, comment } = args

  if (!filePath) return { ok: false, error: 'no file path' }
  if (!before) return { ok: false, error: 'no selection' }
  if (before === after) return { ok: false, error: 'replacement matches the original' }
  if (!comment.trim()) return { ok: false, error: 'comment is required' }

  let abs: string
  try {
    abs = safeResolve(filePath)
  } catch (err) {
    return { ok: false, error: err instanceof Error ? err.message : String(err) }
  }

  let original: string
  try {
    original = await readFile(abs, 'utf8')
  } catch (err) {
    return { ok: false, error: `cannot read ${filePath}: ${(err as Error).message}` }
  }

  const occurrences = original.split(before).length - 1
  if (occurrences === 0)
    return { ok: false, error: 'selected text not found in file on disk — save first?' }
  if (occurrences > 1)
    return {
      ok: false,
      error: 'selected text appears multiple times in the file — widen your selection'
    }

  const status = await git.status()
  const baseBranch = status.current ?? 'master'
  const dirty =
    status.modified.length + status.not_added.length + status.staged.length > 0

  let stashed = false
  let branchCreated = false
  const firstLine = comment.split('\n')[0].trim()
  const branch = `suggest/${new Date().toISOString().replace(/[-:T]/g, '').slice(0, 12)}-${slugForBranch(firstLine)}`

  try {
    if (dirty) {
      await git.stash(['push', '--include-untracked', '-m', `book-editor-suggest-${branch}`])
      stashed = true
    }

    await git.checkoutBranch(branch, baseBranch)
    branchCreated = true

    const updated = original.replace(before, after)
    await writeFile(abs, updated, 'utf8')
    await git.add(filePath)
    await git.commit(firstLine || 'suggestion from book editor')
    await git.push('origin', branch, ['--set-upstream'])

    const prUrl = await runGhCreatePr({
      baseBranch,
      branch,
      title: firstLine || 'Suggestion from book editor',
      body: comment
    })

    await git.checkout(baseBranch)
    if (stashed) {
      await git.stash(['pop'])
      stashed = false
    }

    return { ok: true, prUrl, branch }
  } catch (err) {
    // Best-effort recovery
    try {
      const cur = (await git.status()).current
      if (cur && cur !== baseBranch) await git.checkout(baseBranch)
    } catch {
      // ignore
    }
    if (stashed) {
      try {
        await git.stash(['pop'])
      } catch {
        // ignore
      }
    }
    if (branchCreated) {
      try {
        await git.deleteLocalBranch(branch, true)
      } catch {
        // ignore
      }
    }
    return { ok: false, error: err instanceof Error ? err.message : String(err) }
  }
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

  ipcMain.handle('book:summary', async () => {
    const tree = await listTree()
    const files: string[] = []
    const collect = (nodes: TreeNode[]) => {
      for (const n of nodes) {
        if (n.type === 'file') files.push(n.path)
        else collect(n.children)
      }
    }
    collect(tree)
    let words = 0
    for (const rel of files) {
      try {
        const text = await readFile(safeResolve(rel), 'utf8')
        words += countWords(text)
      } catch {
        // ignore unreadable files
      }
    }
    let branch: string | null = null
    let modified = 0
    let ahead = 0
    let behind = 0
    try {
      const s = await git.status()
      branch = s.current
      modified = s.modified.length + s.not_added.length + s.staged.length
      ahead = s.ahead
      behind = s.behind
    } catch {
      // ignore — repo may not be initialised
    }
    return { chapters: files.length, words, branch, modified, ahead, behind }
  })

  ipcMain.handle('book:suggestCommitMessage', async () => {
    try {
      await git.add('.')
      const diff = await git.diff(['--cached'])
      if (!diff.trim()) return { ok: false, error: 'nothing staged' }
      const message = await runClaudeForCommitMessage(diff)
      return { ok: true, message }
    } catch (err) {
      return { ok: false, error: err instanceof Error ? err.message : String(err) }
    }
  })

  ipcMain.handle('book:suggestChange', async (_e, args: {
    filePath: string
    before: string
    after: string
    comment: string
  }) => {
    return runSuggestChange(args)
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
