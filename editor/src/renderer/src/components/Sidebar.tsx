import { useState } from 'react'
import type { BookSummary, TreeNode } from '../../../preload'

type Props = {
  tree: TreeNode[]
  currentPath: string | null
  onOpen: (path: string) => void
  onRefresh: () => void
  bookRoot: string
  summary: BookSummary | null
}

export default function Sidebar({
  tree,
  currentPath,
  onOpen,
  onRefresh,
  bookRoot,
  summary
}: Props) {
  return (
    <aside className="sidebar">
      <div className="sidebar-header">
        <div className="title">Book</div>
        <button className="icon-btn" title="Refresh" onClick={onRefresh}>
          ↻
        </button>
      </div>
      <div className="root-path" title={bookRoot}>
        {bookRoot}
      </div>
      <SummaryPanel summary={summary} />
      <div className="tree">
        {tree.map((node) => (
          <TreeItem key={node.path} node={node} currentPath={currentPath} onOpen={onOpen} />
        ))}
      </div>
    </aside>
  )
}

function SummaryPanel({ summary }: { summary: BookSummary | null }) {
  if (!summary) {
    return <div className="summary loading">loading…</div>
  }
  const { chapters, words, branch, modified, ahead, behind } = summary
  const gitBits: string[] = []
  if (branch) gitBits.push(branch)
  if (modified > 0) gitBits.push(`${modified} modified`)
  if (ahead > 0) gitBits.push(`↑${ahead}`)
  if (behind > 0) gitBits.push(`↓${behind}`)
  return (
    <div className="summary">
      <div className="summary-row">
        <span className="summary-label">chapters</span>
        <span className="summary-value">{chapters}</span>
      </div>
      <div className="summary-row">
        <span className="summary-label">words</span>
        <span className="summary-value">{words.toLocaleString()}</span>
      </div>
      {gitBits.length > 0 && (
        <div className="summary-row summary-git" title="git status">
          <span className="summary-label">git</span>
          <span className="summary-value">{gitBits.join(' · ')}</span>
        </div>
      )}
    </div>
  )
}

function TreeItem({
  node,
  currentPath,
  onOpen
}: {
  node: TreeNode
  currentPath: string | null
  onOpen: (path: string) => void
}) {
  const [open, setOpen] = useState(true)

  if (node.type === 'file') {
    const selected = currentPath === node.path
    return (
      <div
        className={`file ${selected ? 'selected' : ''}`}
        onClick={() => onOpen(node.path)}
      >
        {node.name}
      </div>
    )
  }

  return (
    <div className="dir">
      <div className="dir-label" onClick={() => setOpen(!open)}>
        <span className="caret">{open ? '▾' : '▸'}</span> {node.name}
      </div>
      {open && (
        <div className="dir-children">
          {node.children.map((child) => (
            <TreeItem
              key={child.path}
              node={child}
              currentPath={currentPath}
              onOpen={onOpen}
            />
          ))}
        </div>
      )}
    </div>
  )
}
