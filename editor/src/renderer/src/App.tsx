import { useCallback, useEffect, useState } from 'react'
import type { TreeNode } from '../../preload'
import Sidebar from './components/Sidebar'
import Editor from './components/Editor'
import StatusBar from './components/StatusBar'

export default function App() {
  const [tree, setTree] = useState<TreeNode[]>([])
  const [currentPath, setCurrentPath] = useState<string | null>(null)
  const [content, setContent] = useState('')
  const [savedContent, setSavedContent] = useState('')
  const [bookRoot, setBookRoot] = useState('')
  const [status, setStatus] = useState('')

  const refreshTree = useCallback(async () => {
    const t = await window.api.listTree()
    setTree(t)
  }, [])

  useEffect(() => {
    refreshTree()
    window.api.root().then(setBookRoot)
  }, [refreshTree])

  const openFile = useCallback(async (path: string) => {
    const text = await window.api.readFile(path)
    setCurrentPath(path)
    setContent(text)
    setSavedContent(text)
    setStatus(`opened ${path}`)
  }, [])

  const save = useCallback(async () => {
    if (!currentPath) return
    await window.api.writeFile(currentPath, content)
    setSavedContent(content)
    setStatus(`saved ${currentPath}`)
  }, [currentPath, content])

  const commitPush = useCallback(async () => {
    if (content !== savedContent && currentPath) {
      await window.api.writeFile(currentPath, content)
      setSavedContent(content)
    }
    const msg = window.prompt('Commit message:', 'chore: edits from book editor')
    if (!msg) return
    setStatus('committing…')
    const res = await window.api.gitCommitPush(msg)
    setStatus(res.ok ? 'pushed ✓' : `push failed: ${res.error}`)
  }, [content, savedContent, currentPath])

  useEffect(() => {
    const onKey = (e: KeyboardEvent) => {
      if ((e.ctrlKey || e.metaKey) && e.key === 's') {
        e.preventDefault()
        save()
      }
    }
    window.addEventListener('keydown', onKey)
    return () => window.removeEventListener('keydown', onKey)
  }, [save])

  const dirty = content !== savedContent

  return (
    <div className="app">
      <Sidebar
        tree={tree}
        currentPath={currentPath}
        onOpen={openFile}
        onRefresh={refreshTree}
        bookRoot={bookRoot}
      />
      <div className="main">
        <div className="toolbar">
          <span className="filepath">
            {currentPath ?? 'no file'}
            {dirty && <span className="dirty"> •</span>}
          </span>
          <div className="actions">
            <button onClick={save} disabled={!currentPath || !dirty}>
              Save
            </button>
            <button onClick={commitPush} disabled={!currentPath}>
              Commit &amp; push
            </button>
          </div>
        </div>
        <Editor value={content} onChange={setContent} />
        <StatusBar content={content} status={status} dirty={dirty} />
      </div>
    </div>
  )
}
