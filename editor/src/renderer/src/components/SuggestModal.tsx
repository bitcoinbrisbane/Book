import { useEffect, useState } from 'react'

type Props = {
  filePath: string
  selection: string
  onClose: () => void
}

type Status = { kind: 'idle' } | { kind: 'busy'; message: string } | { kind: 'done'; prUrl: string } | { kind: 'error'; message: string }

export default function SuggestModal({ filePath, selection, onClose }: Props) {
  const [after, setAfter] = useState(selection)
  const [comment, setComment] = useState('')
  const [status, setStatus] = useState<Status>({ kind: 'idle' })

  useEffect(() => {
    const onKey = (e: KeyboardEvent) => {
      if (e.key === 'Escape' && status.kind !== 'busy') onClose()
    }
    window.addEventListener('keydown', onKey)
    return () => window.removeEventListener('keydown', onKey)
  }, [onClose, status.kind])

  const submit = async () => {
    if (!comment.trim()) {
      setStatus({ kind: 'error', message: 'comment is required' })
      return
    }
    if (after === selection) {
      setStatus({ kind: 'error', message: 'replacement matches the original' })
      return
    }
    setStatus({ kind: 'busy', message: 'creating branch + committing + pushing + opening PR…' })
    const res = await window.api.suggestChange({
      filePath,
      before: selection,
      after,
      comment
    })
    if (res.ok) {
      setStatus({ kind: 'done', prUrl: res.prUrl })
    } else {
      setStatus({ kind: 'error', message: res.error })
    }
  }

  const busy = status.kind === 'busy'

  return (
    <div className="modal-overlay" onClick={busy ? undefined : onClose}>
      <div className="modal" onClick={(e) => e.stopPropagation()}>
        <div className="modal-header">
          <span>Suggest a change</span>
          <button className="icon-btn" onClick={onClose} disabled={busy}>×</button>
        </div>

        <div className="modal-body">
          <div className="modal-row">
            <label>File</label>
            <div className="filepath modal-filepath">{filePath}</div>
          </div>

          <div className="modal-row">
            <label>Original (selected)</label>
            <textarea className="suggest-before" value={selection} readOnly rows={6} />
          </div>

          <div className="modal-row">
            <label>Replacement</label>
            <textarea
              className="suggest-after"
              value={after}
              onChange={(e) => setAfter(e.target.value)}
              rows={6}
              disabled={busy}
              spellCheck
            />
          </div>

          <div className="modal-row">
            <label>Comment (becomes the PR body — first line is the title)</label>
            <textarea
              className="suggest-comment"
              value={comment}
              onChange={(e) => setComment(e.target.value)}
              rows={5}
              placeholder="Cross-reference Chapter 6&#10;&#10;Adds a link to the SPDX section so readers don't have to flip back."
              disabled={busy}
              spellCheck
            />
          </div>

          {status.kind === 'busy' && (
            <div className="modal-status busy">{status.message}</div>
          )}
          {status.kind === 'error' && (
            <div className="modal-status error">error: {status.message}</div>
          )}
          {status.kind === 'done' && (
            <div className="modal-status done">
              PR opened →{' '}
              <a href={status.prUrl} target="_blank" rel="noreferrer">
                {status.prUrl}
              </a>
            </div>
          )}
        </div>

        <div className="modal-footer">
          {status.kind === 'done' ? (
            <button onClick={onClose}>Close</button>
          ) : (
            <>
              <button onClick={onClose} disabled={busy} className="secondary">
                Cancel
              </button>
              <button onClick={submit} disabled={busy || !comment.trim() || after === selection}>
                Open suggestion PR
              </button>
            </>
          )}
        </div>
      </div>
    </div>
  )
}
