import { useState } from 'react'
import type { TreeNode } from '../../../preload'

type Props = {
  tree: TreeNode[]
  currentPath: string | null
  onOpen: (path: string) => void
  onRefresh: () => void
  bookRoot: string
}

export default function Sidebar({ tree, currentPath, onOpen, onRefresh, bookRoot }: Props) {
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
      <div className="tree">
        {tree.map((node) => (
          <TreeItem key={node.path} node={node} currentPath={currentPath} onOpen={onOpen} />
        ))}
      </div>
    </aside>
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
