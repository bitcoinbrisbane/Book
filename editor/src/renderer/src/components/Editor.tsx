import { useEffect, useRef } from 'react'
import { EditorState } from '@codemirror/state'
import { EditorView, keymap, lineNumbers, highlightActiveLine } from '@codemirror/view'
import { defaultKeymap, history, historyKeymap } from '@codemirror/commands'
import { markdown } from '@codemirror/lang-markdown'
import { syntaxHighlighting, defaultHighlightStyle } from '@codemirror/language'

type Props = {
  value: string
  onChange: (v: string) => void
  onSelectionChange?: (text: string) => void
}

export default function Editor({ value, onChange, onSelectionChange }: Props) {
  const host = useRef<HTMLDivElement>(null)
  const view = useRef<EditorView | null>(null)
  const onChangeRef = useRef(onChange)
  onChangeRef.current = onChange
  const onSelectionRef = useRef(onSelectionChange)
  onSelectionRef.current = onSelectionChange

  useEffect(() => {
    if (!host.current) return

    const state = EditorState.create({
      doc: value,
      extensions: [
        lineNumbers(),
        highlightActiveLine(),
        history(),
        keymap.of([...defaultKeymap, ...historyKeymap]),
        markdown(),
        syntaxHighlighting(defaultHighlightStyle, { fallback: true }),
        EditorView.lineWrapping,
        EditorView.updateListener.of((u) => {
          if (u.docChanged) onChangeRef.current(u.state.doc.toString())
          if (u.selectionSet || u.docChanged) {
            const sel = u.state.selection.main
            const text = u.state.doc.sliceString(sel.from, sel.to)
            onSelectionRef.current?.(text)
          }
        }),
        EditorView.theme({
          '&': { height: '100%', fontSize: '14px' },
          '.cm-scroller': { fontFamily: 'ui-monospace, SFMono-Regular, Menlo, monospace' },
          '.cm-content': { padding: '12px 16px' }
        })
      ]
    })

    view.current = new EditorView({ state, parent: host.current })
    return () => {
      view.current?.destroy()
      view.current = null
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [])

  // Sync external value changes (e.g. opening a different file) without
  // echoing our own updates back through setState.
  useEffect(() => {
    const v = view.current
    if (!v) return
    if (v.state.doc.toString() === value) return
    v.dispatch({
      changes: { from: 0, to: v.state.doc.length, insert: value }
    })
  }, [value])

  return <div ref={host} className="editor" />
}
