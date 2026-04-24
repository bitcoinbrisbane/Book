import { useMemo } from 'react'

type Props = {
  content: string
  status: string
  dirty: boolean
}

function countWords(text: string): number {
  const trimmed = text.trim()
  if (!trimmed) return 0
  return trimmed.split(/\s+/).length
}

export default function StatusBar({ content, status, dirty }: Props) {
  const words = useMemo(() => countWords(content), [content])
  const chars = content.length
  const lines = content ? content.split('\n').length : 0

  return (
    <div className="statusbar">
      <span>{words.toLocaleString()} words</span>
      <span>{chars.toLocaleString()} chars</span>
      <span>{lines} lines</span>
      <span className="spacer" />
      {dirty && <span className="badge">unsaved</span>}
      <span className="status-msg">{status}</span>
    </div>
  )
}
