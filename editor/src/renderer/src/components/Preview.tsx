import { useMemo } from 'react'
import { marked } from 'marked'

marked.setOptions({ gfm: true, breaks: false })

type Props = { source: string }

export default function Preview({ source }: Props) {
  const html = useMemo(() => marked.parse(source) as string, [source])
  return (
    <div className="preview">
      <div className="preview-body" dangerouslySetInnerHTML={{ __html: html }} />
    </div>
  )
}
