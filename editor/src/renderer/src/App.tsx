import { useCallback, useEffect, useState } from 'react'
import type { BookSummary, TreeNode } from '../../preload'
import Sidebar from './components/Sidebar'
import Editor from './components/Editor'
import Preview from './components/Preview'
import StatusBar from './components/StatusBar'
import SuggestModal from './components/SuggestModal'

export default function App() {
  const [tree, setTree] = useState<TreeNode[]>([])
  const [currentPath, setCurrentPath] = useState<string | null>(null)
  const [content, setContent] = useState('')
  const [savedContent, setSavedContent] = useState('')
  const [bookRoot, setBookRoot] = useState('')
  const [status, setStatus] = useState('')
  const [showPreview, setShowPreview] = useState(true)
  const [summary, setSummary] = useState<BookSummary | null>(null)
  const [selection, setSelection] = useState('')
  const [showSuggest, setShowSuggest] = useState(false)

  const refreshSummary = useCallback(async () => {
    const s = await window.api.summary()
    setSummary(s)
  }, [])

  const refreshTree = useCallback(async () => {
    const t = await window.api.listTree()
    setTree(t)
    refreshSummary()
  }, [refreshSummary])

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
    refreshSummary()
  }, [currentPath, content, refreshSummary])

  const commitPush = useCallback(async () => {
    if (content !== savedContent && currentPath) {
      await window.api.writeFile(currentPath, content)
      setSavedContent(content)
    }
    setStatus('asking claude for a commit message…')
    const suggestion = await window.api.suggestCommitMessage()
    const fallback = 'chore: edits from book editor'
    const defaultMsg = suggestion.ok ? suggestion.message : fallback
    if (!suggestion.ok) {
      setStatus(`claude unavailable (${suggestion.error}) — using fallback`)
    }
    const msg = window.prompt('Commit message:', defaultMsg)
    if (!msg) {
      setStatus('commit cancelled')
      return
    }
    setStatus('committing…')
    const res = await window.api.gitCommitPush(msg)
    setStatus(res.ok ? 'pushed ✓' : `push failed: ${res.error}`)
    refreshSummary()
  }, [content, savedContent, currentPath, refreshSummary])

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
  const canSuggest = !!currentPath && !dirty && selection.trim().length > 0

  return (
    <div className="app">
      <Sidebar
        tree={tree}
        currentPath={currentPath}
        onOpen={openFile}
        onRefresh={refreshTree}
        bookRoot={bookRoot}
        summary={summary}
      />
      <div className="main">
        <div className="toolbar">
          <span className="filepath">
            {currentPath ?? 'no file'}
            {dirty && <span className="dirty"> •</span>}
          </span>
          <div className="actions">
            <button onClick={() => setShowPreview((v) => !v)}>
              {showPreview ? 'Hide preview' : 'Show preview'}
            </button>
            <button onClick={save} disabled={!currentPath || !dirty}>
              Save
            </button>
            <button
              onClick={() => setShowSuggest(true)}
              disabled={!canSuggest}
              title={
                !currentPath
                  ? 'Open a file first'
                  : dirty
                    ? 'Save your changes first'
                    : !selection.trim()
                      ? 'Select some text in the editor first'
                      : 'Open a PR with this change suggested'
              }
            >
              Suggest change
            </button>
            <button onClick={commitPush} disabled={!currentPath}>
              Commit &amp; push
            </button>
          </div>
        </div>
        <div className={showPreview ? 'split split-2' : 'split split-1'}>
          <Editor value={content} onChange={setContent} onSelectionChange={setSelection} />
          {showPreview && <Preview source={content} />}
        </div>
        <StatusBar content={content} status={status} dirty={dirty} />
      </div>
      {showSuggest && currentPath && (
        <SuggestModal
          filePath={currentPath}
          selection={selection}
          onClose={() => {
            setShowSuggest(false)
            refreshSummary()
          }}
        />
      )}
    </div>
  )
}
